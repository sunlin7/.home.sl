''' -*- code: utf-8 -*-
Tools for removing the boring dialog.
globals().update("MANUAL_START_THREAD=True") will suppress auto start the thread.
'''
import sys
import time
import ctypes
import threading
import win32api
import win32con
import win32gui
from ctypes import windll, wintypes
from PIL import ImageGrab as igrab

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
        if not win32gui.IsWindowVisible(self.hwnd):  # not visible
            return True
        windll.user32.SetThreadDpiAwarenessContext(wintypes.HANDLE(-2))  # Toggle ON
        wRect = win32gui.GetWindowRect(self.hwnd)
        X = int(wRect[0] + (wRect[2] - wRect[0]) / 4)  # left + width*1/4
        titleH = win32api.GetSystemMetrics(win32con.SM_CYCAPTION)
        Y = int(wRect[3] - titleH * 2)         # above bottom with TitleHeight*2
        win32gui.SetForegroundWindow(self.hwnd)  # bring the window to forge
        color = igrab.grab((X-1, Y-1, X, Y)).getpixel((0,0))
        if color == (76, 194, 255):  # "OK" button highlighted with Blue color
            cInfo = win32gui.GetCursorInfo()  # save the Cursor pos
            win32api.SetCursor(None)          # hide the Cursor
            win32api.SetCursorPos((X, Y))
            win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
            win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
            win32api.SetCursorPos(cInfo[2])  # restore the Cursor
            win32api.SetCursor(cInfo[1])

        return False            # will check the window on next cycle


class TimerExecGlobalProDlg(ITimerExec):
    hwnd = None
    def __init__(self, hwnd):
        self.hwnd = hwnd

    def sWinEnumHandler(self, hwnd, res):
        if win32gui.GetWindowText(hwnd) == "Connected":
            res.append(hwnd)
            return False        # discontinue the loop

    def run(self):
        if not win32gui.IsWindowVisible(self.hwnd):
            return True

        res = []
        win32gui.EnumChildWindows(self.hwnd, self.sWinEnumHandler, res)
        if len(res) > 0:
            win32gui.ShowWindow(self.hwnd, win32con.SW_HIDE)
            return True

        return False


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
        cb(event, hwnd, win32gui.GetWindowText(hwnd)))

    hook = windll.user32.SetWinEventHook(
        win32con.EVENT_SYSTEM_FOREGROUND,
        win32con.EVENT_SYSTEM_FOREGROUND,
        0, WinEventProc, 0, 0,
        win32con.WINEVENT_OUTOFCONTEXT | win32con.WINEVENT_SKIPOWNPROCESS
    )
    if hook == 0:
        print('SetWinEventHook failed', file=sys.stderr)
        exit(1)

    msg = wintypes.MSG()
    while windll.user32.GetMessageW(ctypes.byref(msg), 0, 0, 0) != 0:
        windll.user32.TranslateMessageW(msg)
        windll.user32.DispatchMessageW(msg)

    # Stopped receiving events, so clear up the winevent hook and uninitialise.
    print('Stopped receiving new window change events. Exiting...')
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

if not globals().get('MANUAL_START_THREAD'):
    '''The main function when running as a plugin'''
    eObjs = dict()
    def evtCB(e,hwnd,title):
        if hwnd in eObjs:
            pass
        elif title == "Windows Security":
            eObjs[hwnd] = TimerExecSecurityDlg(hwnd)
            eObjs[hwnd].run()
        elif title == "GlobalProtect" and '#32770' == win32gui.GetClassName(hwnd):
            eObjs[hwnd] = TimerExecGlobalProDlg(hwnd)
            eObjs[hwnd].run()

    _listen_thread = threading.Thread(target=listen_foreground, args=(evtCB,), daemon=True)
    _listen_thread.start()

    def timerCB(eObjs):
        rkeys = []
        for key,obj in eObjs.items():
            if obj.run():
                rkeys.append(key)

        for key in rkeys:
            del eObjs[key]

    _timer_thread = RepeatTimer(1.5, timerCB, (eObjs,))
    _timer_thread.start()

    win32api.SetConsoleCtrlHandler(
        # always return False to continue signal chain
        lambda ct: [False, ct == win32con.CTRL_C_EVENT and stop()][0],
        True)

if __name__ == '__main__':
    win32api.SetConsoleCtrlHandler(
        # return True to avoid backtrace, discontinue the signal hander chain
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _listen_thread.join()
