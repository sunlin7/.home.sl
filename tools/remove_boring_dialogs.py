'''
Tools for removing the boring dialog.
globals().update("MANUAL_START_THREAD=True") will suppress auto start the thread.
'''
import cv2
import numpy as np
import re
import sys
import time
import ctypes
import logging
import threading
import win32api
import win32con
import win32gui
import winreg
import copy
from ctypes import windll, wintypes
from PIL import ImageGrab as igrab
from tesserocr import PyTessBaseAPI


def get_rectangles(img, mode=cv2.RETR_EXTERNAL):
    imgCv = cv2.cvtColor(np.array(img), cv2.COLOR_RGB2BGR)
    imgBlur = cv2.GaussianBlur(imgCv, (3, 3), 0)
    imgGray = cv2.cvtColor(imgBlur, cv2.COLOR_BGR2GRAY)
    imgBin = cv2.Canny(imgGray, 30, 100, apertureSize=3)
    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (6,3))
    dilate = cv2.dilate(imgBin, kernel, iterations=4)
    contours, _hierarchy = cv2.findContours(dilate, mode, cv2.CHAIN_APPROX_SIMPLE)
    return [cv2.boundingRect(x) for x in contours]  # ordered in buttom to up

def mouse_click(pos, evt=win32con.MOUSEEVENTF_LEFTDOWN):
    logging.debug(f"click at {pos}")
    cInfo = win32gui.GetCursorInfo()  # save the Cursor pos
    win32api.SetCursor(None)          # hide the Cursor
    win32api.SetCursorPos(pos)
    win32api.mouse_event(evt, 0, 0, 0, 0)
    win32api.mouse_event(evt<<1, 0, 0, 0, 0)
    win32api.SetCursorPos(cInfo[2])  # restore the Cursor
    win32api.SetCursor(cInfo[1])


def apply_1st_auto_fill(pos):
    logging.debug(f"auto fill at {pos}")
    cInfo = win32gui.GetCursorInfo()  # save the Cursor pos
    win32api.SetCursor(None)          # hide the Cursor
    win32api.SetCursorPos(pos)
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
    win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
    time.sleep(0.5)
    win32api.keybd_event(win32con.VK_DOWN, 0, 0, 0)
    win32api.keybd_event(win32con.VK_DOWN, 0, win32con.KEYEVENTF_KEYUP, 0)
    time.sleep(0.15)
    win32api.keybd_event(win32con.VK_RETURN, 0, 0, 0)
    win32api.keybd_event(win32con.VK_RETURN, 0, win32con.KEYEVENTF_KEYUP, 0)
    time.sleep(0.2)
    win32api.keybd_event(win32con.VK_RETURN, 0, 0, 0)
    win32api.keybd_event(win32con.VK_RETURN, 0, win32con.KEYEVENTF_KEYUP, 0)
    win32api.SetCursorPos(cInfo[2])  # restore the Cursor
    win32api.SetCursor(cInfo[1])


class ITimerExec(object):
    '''Interface for executable object'''
    def run():
        '''Run the action. Return True for done, False for need another try'''
        return True


class TimerExecSecurityDlg(ITimerExec):
    hwnd = None
    def __init__(self, hwnd):
        self.hwnd = hwnd

    def run(self):
        if (win32gui.GetForegroundWindow() != self.hwnd  # foreground switched
            or not win32gui.IsWindowVisible(self.hwnd)):  # invisible
            logging.debug(f"SecrityDlg:foreground changed or invisible window {self.hwnd}")
            return True

        tess = PyTessBaseAPI()
        titleH = win32api.GetSystemMetrics(win32con.SM_CYCAPTION)
        wRect = win32gui.GetWindowRect(self.hwnd)
        pad = titleH//4         # remove window pad, 1/4 of caption height
        img = igrab.grab(tuple(np.add(wRect, [pad, pad, -pad, -pad])))

        # tesserocr has problem on "OK" with background color
        # re-split "OK" button into exactly rectangle and search text
        okBtnVector = (img.width//8, img.height - 3*titleH, img.width//2, img.height - titleH)
        imgBtnOK = img.crop(okBtnVector)
        rectList = get_rectangles(imgBtnOK)
        tess.SetImage(imgBtnOK)
        for x in rectList:
            tess.SetRectangle(*x)
            txt = tess.GetUTF8Text()
            if txt == "OK\n":
                pos = np.add(okBtnVector[:2], x[:2])  # btn pos to window pos
                logging.debug(f"Click the OK button: {pos} : {okBtnVector}")
                mouse_click(win32gui.ClientToScreen(self.hwnd, tuple(pos)))
                return True  # discontinue

        tess.SetImage(img)
        rectList = get_rectangles(img)
        for x in rectList:
            tess.SetRectangle(*x)
            txt = tess.GetUTF8Text()
            # try search the "Face" option first
            if txt == "Face\n":
                r0, r1 = x[1] - x[3], x[1] + x[3]  # range in [-h, +h]
                res = [x for x in rectList if r0 < x[1] < r1]
                logging.debug(f"'Face' column has rectangles count={len(res)}")
                if len(res) < 3:  # not click on "Face"
                    mouse_click(win32gui.ClientToScreen(self.hwnd, x[:2]))

                return False     # to the next round

            # try click the "More choices"
            if txt == "More choices\n":
                logging.debug("Click the 'More choices' and retry immediately")
                mouse_click(win32gui.ClientToScreen(self.hwnd, x[:2]))
                time.sleep(0.2)
                return self.run()  # retry after expanded choices

        return False            # will check the window on next cycle


class TimerExecGlobalProDlg(ITimerExec):
    hwnd = None
    lastTime = None
    def __init__(self, hwnd):
        self.hwnd = hwnd
        self.lastTime = time.time()

    def run(self):
        lastTime = self.lastTime
        self.lastTime = 0       # reset lastTime for failure branches defaultly
        if not win32gui.IsWindow(self.hwnd):
            return True         # invalid window, discontinue

        if not win32gui.IsWindowVisible(self.hwnd):
            self.lastTime = time.time()
            return False        # invisible, try next round

        fhwnd = win32gui.GetForegroundWindow()
        if fhwnd and (fhwnd == self.hwnd  # still foreground
                      or '#32770' == win32gui.GetClassName(fhwnd)):  # list popup
            logging.debug("The GP is still foreground or its list activated")
            return False

        def childEnumHandler(hwnd, res):
            if win32gui.GetWindowText(hwnd) == "Connected":
                res.append(hwnd)
                return False    # discontinue enum child windows

        res = []
        win32gui.EnumChildWindows(self.hwnd, childEnumHandler, res)
        if len(res) > 0:
            ntime = time.time()
            if ntime - lastTime <= 3.0:  # will let the UI show 3+ seconds
                self.lastTime = lastTime  # restore to the saved value
                return False

            self.lastTime = 0
            win32gui.ShowWindow(self.hwnd, win32con.SW_HIDE)
            logging.debug(f'Stop displaying the GlobalProcect window {self.hwnd}')
            return False

        return False


class TimerExecLoginPage(ITimerExec):
    ''' Login Page automation. It rely on the follow packages:
    tesserocr: https://github.com/simonflueckiger/tesserocr-windows_build/,
      install by pipenv install <RELEASE_URL/package.whl>
    tesserocr-data: https://github.com/tesseract-ocr/tessdata/blob/main/eng.traineddata
      download and put on the work directoy.
    '''
    def __init__(self, hwnd):
        self.hwnd = hwnd
        self.lastTime = time.time()

    def run(self):
        if (win32gui.GetForegroundWindow() != self.hwnd  # foreground switched
            or not win32gui.IsWindowVisible(self.hwnd)):  # invisible
            logging.debug("discontinue for foreground or invisible changed")
            return True

        title = win32gui.GetWindowText(self.hwnd)
        logging.debug(f"title: {title}")
        if title == "GlobalProtect - Google Chrome":
            ntime = time.time()
            dtime = ntime - self.lastTime
            if dtime <= 1.0:    # will let the UI show 1+ seconds
                return False
            elif dtime >= 3.0:  # page switched, reset the timer
                self.lastTime = ntime
                return False

            win32api.keybd_event(win32con.VK_LCONTROL, 0, 0, 0)
            win32api.keybd_event(win32con.VK_F4, 0, win32con.KEYEVENTF_KEYUP, 0)
            win32api.keybd_event(win32con.VK_F4, 0, 0, 0)
            win32api.keybd_event(win32con.VK_LCONTROL, 0, win32con.KEYEVENTF_KEYUP, 0)
            logging.debug(f'Close the GlobalProcect page {self.hwnd}')
            self.lastTime = ntime  # next GlobalProcect page
            return False        # continue to other pages


        pageIters = pageIterFactor(self.hwnd, title)
        if len(pageIters) <= 0:
            logging.debug(f"not page iters for: {title}")
            return False        # retry on next round

        titleH = win32api.GetSystemMetrics(win32con.SM_CYCAPTION)
        wRect = win32gui.GetWindowRect(self.hwnd)
        img = igrab.grab(wRect)
        rectList = get_rectangles(img, cv2.RETR_CCOMP)
        rectList.sort(key=lambda x:(x[1], x[0]))  # sort by (top, left)
        rectList = [x for x in rectList if titleH//2 <= x[3] <= titleH*2]  # drop small or large rectangles
        tess = PyTessBaseAPI()
        tess.SetImage(img)
        for x in rectList:
            Y = x[1]+x[3]
            if (Y < titleH*4):  # skip browser tab head and address line
                continue

            tess.SetRectangle(*x)
            txt = tess.GetUTF8Text()

            for pg in pageIters:
                res, cont = pg.send((txt, x))
                if not cont:     # page matched and action done
                    return False  # still watch the browser window

        logging.debug(f"Nothing to do, will try next round.")
        return False


def PageAutoFillNamePass(hwnd):
    txt, rect = "", (0,0,1,1)
    while not any([re.search('Verification timed out.', txt),
                   re.search('Refresh or return', txt),
                   txt in ["Username\n", "Password\n"]]):
        txt, rect = yield(0, True)

    if txt in ["Username\n", "Password\n"]:
        pos0 = (rect[0], rect[1]+int(rect[3]*2))
        pos = win32gui.ClientToScreen(hwnd, pos0)
        logging.debug(f'try auto fill on: {pos}')
        apply_1st_auto_fill(pos)
        yield (1, False)

    if re.search('Refresh or return', txt):
        logging.debug(f'try refresh page for: {txt}')
        win32api.keybd_event(win32con.VK_F5, 0, 0, 0)
        win32api.keybd_event(win32con.VK_F5, 0, win32con.KEYEVENTF_KEYUP, 0)
        yield(1, False)

    while txt != "Back to sign in\n":
        txt, rect = yield(0, True)

    # Try to click "Back to sign in"
    logging.debug(f'try to click on: {rect[:2]}')
    mouse_click(win32gui.ClientToScreen(hwnd, rect[:2]))
    yield(1, False)


def PageGAccounts(hwnd):
    txt, rect = "", (0,0,1,1)
    while not re.search('Use your Google Account\n', txt):
        txt, rect = yield(0, True)
    while not re.search('Forgot email', txt):
        txt, rect = yield(0, True)
    # reach here mean got previous conditions are satisfied
    pos = (rect[0], rect[1]-rect[3]-rect[3]//2)  # on its Top
    logging.debug("try auto fill on page [%s], pos (%s)", txt, pos)
    apply_1st_auto_fill(win32gui.ClientToScreen(hwnd, pos))
    yield(1, False)


def PageGAccountRelogin(hwnd):
    txt, rect = "", (0,0,1,1)
    while not re.search('Choose an account', txt):
        txt, rect = yield(0, True)
    while not re.search('@[a-z.]+\n', txt):
        txt, rect = yield(0, True)

    mouse_click( win32gui.ClientToScreen(hwnd, rect[:2]))
    logging.debug(f"try relogin: {txt}")
    yield(1, False)


def PageTeleportLogin(hwnd):
    txt, rect = "", (0,0,1,1)
    while not re.search('Teleport\n', txt):
        txt, rect = yield(0, True)
    while not re.search('Successful\n', txt):
        txt, rect = yield(0, True)

    logging.debug(f"try close succeed Teleport page: {txt}")
    win32api.keybd_event(win32con.VK_LCONTROL, 0, 0, 0)
    win32api.keybd_event(win32con.VK_F4, 0, win32con.KEYEVENTF_KEYUP, 0)
    win32api.keybd_event(win32con.VK_F4, 0, 0, 0)
    win32api.keybd_event(win32con.VK_LCONTROL, 0, win32con.KEYEVENTF_KEYUP, 0)
    yield(1, False)


def pageIterFactor(hwnd, title):
    res = []
    if any([re.search(x, title) for x in ["^Gmail$", "Communications - Sign In"]]):
        res.append(PageAutoFillNamePass(hwnd))
        res.append(PageGAccountRelogin(hwnd))
    elif re.search("Sign in - Google Accounts", title):
        res.append(PageGAccounts(hwnd))
        res.append(PageGAccountRelogin(hwnd))
    elif re.search("^Success -", title):
        res.append(PageTeleportLogin(hwnd))

    [next(x) for x in res]      # reach the first yield in iterators
    return res


def listen_foreground(cb=lambda *args:None):
    WinEventProcType = ctypes.WINFUNCTYPE(
        None,
        wintypes.HANDLE,
        wintypes.DWORD,
        wintypes.HWND,
        wintypes.LONG,
        wintypes.LONG,
        wintypes.DWORD,
        wintypes.DWORD
    )

    WinEventProc = WinEventProcType(
        lambda hWinEventHook, event, hwnd, idObject, idChild, dwEventThread, dwmsEventTime:\
        cb(event, hwnd, win32gui.GetWindowText(hwnd), win32gui.GetClassName(hwnd)))

    hook = windll.user32.SetWinEventHook(
        win32con.EVENT_SYSTEM_FOREGROUND,
        win32con.EVENT_SYSTEM_FOREGROUND,
        0, WinEventProc, 0, 0,
        win32con.WINEVENT_OUTOFCONTEXT | win32con.WINEVENT_SKIPOWNPROCESS
    )
    if hook == 0:
        logging.error('SetWinEventHook failed', file=sys.stderr)
        exit(1)

    msg = wintypes.MSG()
    while windll.user32.GetMessageW(ctypes.byref(msg), 0, 0, 0) != 0:
        windll.user32.TranslateMessageW(msg)
        windll.user32.DispatchMessageW(msg)

    # Stopped receiving events, so clear up the winevent hook and uninitialise.
    logging.error('Stopped receiving new window change events. Exiting...')
    windll.user32.UnhookWinEvent(hook)


class RepeatTimer(threading.Timer):
    def run(self):
        while not self.finished.wait(self.interval):
            self.function(*self.args, **self.kwargs)

_timer_thread = None
_listen_thread = None

def stop():
    '''Stop the plugin'''
    if _timer_thread:
        _timer_thread.cancel()
    if _listen_thread:
        win32api.PostThreadMessage(_listen_thread.ident, win32con.WM_QUIT, 0, 0)

def join(rgs):
    _listen_thread.join()

if not globals().get('MANUAL_START_THREAD'):
    '''The main function when running as a plugin'''
    windll.user32.SetProcessDpiAwarenessContext(wintypes.HANDLE(-2))  # Toggle ON

    eObjs = dict()
    def evtCB(e, hwnd, title, clsName):
        if 'cygwin/x X rl' == clsName:
            regKey = winreg.OpenKey(winreg.HKEY_CURRENT_USER, "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize", 0, winreg.KEY_QUERY_VALUE)
            light, _ = winreg.QueryValueEx(regKey, "AppsUseLightTheme")
            winreg.CloseKey(regKey)
            DWMWA_USE_IMMERSIVE_DARK_MODE = 20
            rendering_policy = DWMWA_USE_IMMERSIVE_DARK_MODE
            dark = ctypes.c_int(not light)
            ctypes.windll.dwmapi.DwmSetWindowAttribute(hwnd, rendering_policy, ctypes.byref(dark), ctypes.sizeof(dark))

        # overwrite the eObjs if the title changed
        eobj = None
        if title == "Windows Security":
            dlg = TimerExecSecurityDlg(hwnd)
            eobj = None if dlg.run() else dlg
        elif title == "GlobalProtect" and '#32770' == clsName:
            eobj = TimerExecGlobalProDlg(hwnd)
        elif any([re.search(x, title) for x in [" - Google Chrome", "^Gmail$"]]):
            page = TimerExecLoginPage(hwnd)
            eobj = None if page.run() else page

        if eobj:
            eObjs[hwnd] = eobj
            return True         # True for intresting on the window

    _listen_thread = threading.Thread(target=listen_foreground, args=(evtCB,), daemon=True)
    _listen_thread.start()

    def timerCB(eObjs):
        rkeys = []
        objs = copy.copy(eObjs)
        for key,obj in objs.items():
            if obj.run():
                rkeys.append(key)

        for key in rkeys:
            del eObjs[key]

    _timer_thread = RepeatTimer(1, timerCB, (eObjs,))
    _timer_thread.start()

    # initialize the list at startup
    win32gui.EnumWindows(
        # always return True to continue enum window
        lambda hwnd,_: [True, evtCB(win32con.EVENT_SYSTEM_FOREGROUND, hwnd, win32gui.GetWindowText(hwnd), win32gui.GetClassName(hwnd))][0],
        None)

    win32api.SetConsoleCtrlHandler(
        # always return False to continue signal chain
        lambda ct: [False, ct == win32con.CTRL_C_EVENT and stop()][0],
        True)

if __name__ == '__main__':
    logging.basicConfig(format='%(asctime)s %(filename)s:%(lineno)d %(message)s', level=logging.DEBUG)
    win32api.SetConsoleCtrlHandler(
        # return True to avoid backtrace, discontinue the signal handler chain
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _listen_thread.join()
