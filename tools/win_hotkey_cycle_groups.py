'''
hotkey for windows to cycle taskbar groups.
This tools depends on pywin32, pillow, numpy, python-opencv
$ pythonw.exe win_hotkey_cycle_groups.py
Use the global variable MANUAL_START_THREAD to suppress the thread auto start.
globals().update("MANUAL_START_THREAD=1")
'''
import ctypes
import win32api
import win32gui
import win32con

import logging
import threading
import win32process
import numpy as np
import cv2
import time
import sys
import pywintypes

from ctypes import windll, wintypes
from win32com.propsys import propsys, pscon

from PIL import ImageGrab as igrab


HOTKEY_CYCLE_GRO_NEXT = 3
HOTKEY_CYCLE_GRO_PREV = 4
HOTKEY_CYCLE_GRO_LAST = 5
MOD_NOREPEAT = 0x4000
# https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
VK_OEM_4 = 0xDB                 # the [{ key
VK_OEM_6 = 0xDD                 # the ]} key
VK_OEM_7 = 0xDE                 # the '" key

class APPBARDATA(ctypes.Structure):
    _fields_ = [("cbSize", ctypes.wintypes.DWORD),
                ("hWnd", ctypes.wintypes.HANDLE),
                ("uCallbackMessage", ctypes.wintypes.UINT),
                ("uEdge", ctypes.wintypes.UINT),
                ("rc", ctypes.wintypes.RECT),
                ("lParam", ctypes.wintypes.LPARAM)]


def hotkey_main():
    '''The main function to for hotkey procedural.'''
    # DPI Awareness
    windll.user32.SetThreadDpiAwarenessContext(wintypes.HANDLE(-2))  # Toggle ON

    # create and initialize window class
    wndClass                = win32gui.WNDCLASS()
    wndClass.lpfnWndProc    = wndProc
    wndClass.hInstance      = win32api.GetModuleHandle()
    wndClass.lpszClassName  = 'CycleWindowGroup'

    # register window class
    wndClassAtom = None
    try:
        wndClassAtom = win32gui.RegisterClass(wndClass)
    except Exception as e:
        print(e)
        raise e

    hWindow = win32gui.CreateWindowEx(
        win32con.WS_EX_TOPMOST | win32con.WS_EX_TOOLWINDOW,
        wndClassAtom,  # NOTE: an atom indicates to use a wndProc, otherwise pywin32 will treat as msg map
        'Python Win32 Window',
        win32con.WS_POPUP,
        0, 0, 1, 1, None, None, wndClass.hInstance, None)

    win32gui.RegisterHotKey(hWindow, HOTKEY_CYCLE_GRO_PREV, MOD_NOREPEAT | win32con.MOD_WIN, VK_OEM_7)
    win32gui.RegisterHotKey(hWindow, HOTKEY_CYCLE_GRO_NEXT, MOD_NOREPEAT | win32con.MOD_WIN | win32con.MOD_SHIFT, VK_OEM_7)
    # win32gui.RegisterHotKey(None, HOTKEY_CYCLE_GRO_LAST, MOD_NOREPEAT | win32con.MOD_WIN, VK_OEM_4)

    # Dispatch messages
    win32gui.PumpMessages()
    windll.user32.UnregisterHotKey(hWindow, HOTKEY_CYCLE_GRO_NEXT)
    windll.user32.UnregisterHotKey(hWindow, HOTKEY_CYCLE_GRO_PREV)


ts0 = time.time()           # last hotkey happened
def wndProc(hWindow, message, wParam, lParam):
    """Windows messasge procedual"""
    if message == win32con.WM_DESTROY:
        win32gui.PostQuitMessage(0)
        return 0

    elif message == win32con.WM_HOTKEY \
           and wParam in [HOTKEY_CYCLE_GRO_PREV, HOTKEY_CYCLE_GRO_NEXT]:
        hwndDesktop = win32gui.GetDesktopWindow()
        hwndTaskbar = win32gui.FindWindow("Shell_TrayWnd", None)
        if not all([hwndDesktop, hwndTaskbar]):  # non-exists Windows Explore
            logging.error(f"missed desktop: {hwndDesktop}, or taskbar: {hwndTaskbar}")
            return 0

        rectDesktop = win32gui.GetWindowRect(hwndDesktop)
        rectTaskbar = win32gui.GetWindowRect(hwndTaskbar)
        logging.debug(f"rectDesktop: {rectDesktop}, rectTaskbar: {rectTaskbar}")
        if rectTaskbar[3] > rectDesktop[3]:  # taskbar hided
            logging.error(f"taskbar hided")
            return 0

        H = int((rectTaskbar[3] - rectTaskbar[1]) / 6)
        img = igrab.grab((rectTaskbar[0], rectTaskbar[3] - H, rectTaskbar[2], rectTaskbar[3]))
        imgCv = np.array(img)
        imgBlur = cv2.GaussianBlur(imgCv, (3, 3), 0)
        imgGray = cv2.cvtColor(imgBlur, cv2.COLOR_BGR2GRAY)
        imgBin = cv2.Canny(imgGray, 30, 100, apertureSize=3)
        contours, _hierarchy = cv2.findContours(imgBin, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if len(contours) < 2:  # less than two window groups on taskbar
            logging.warning(f"less than 2 windows groups")
            return 0

        rectList = [cv2.boundingRect(x) for x in contours]
        logging.debug(f"H: {H}, rectList: {rectList}")
        rectList.sort()     # the contours maybe in reverse order
        # for a flashing group indicator, the Contours are near by, connect them to a large one
        for idx in range (len(rectList) - 1, 0, -1):  # reverse
            ra,rb = rectList[idx-1], rectList[idx]
            if ra[0] + ra[2] >= rb[0]:  # near by
                ra = list(ra)           # conver tuple to list first
                ra[2] = rb[0]+rb[2]-ra[0]
                rectList[idx-1] = ra
                del rectList[idx]

        activeIdx = next((idx for idx,val in enumerate(rectList) if val[2]>H and val[2]<2.5*H), -1)
        global ts0              # use the global one
        ts1 = time.time()
        if ts1 - ts0 >= 1.0 and activeIdx != len(rectList) - 1:
            off = -activeIdx-1  # non-continily operation, jump to tail
        else:
            off = 1 if wParam == HOTKEY_CYCLE_GRO_NEXT else -1

        nextIdx = (activeIdx + off) % len(rectList)
        logging.debug(f"activeIdx: {activeIdx}, off: {off}, nextIdx: {nextIdx}, hotkey: {wParam}")
        tagRect = rectList[nextIdx]
        pos1 = win32gui.ClientToScreen(hwndTaskbar, (tagRect[0], H*3))
        logging.debug(f"pos1: {pos1}")

        hwnd = win32gui.WindowFromPoint(pos1)
        if hwndTaskbar != win32gui.GetAncestor(hwnd, win32con.GA_ROOTOWNER):
            logging.error(f"taskbar covered by: {hwnd}")
            return 0

        # Show the window on top to grab the forcus, otherwise it can NOT move the cursor for an elevated window
        win32gui.ShowWindow(hWindow, win32con.SW_SHOW)
        win32gui.SetForegroundWindow(hWindow)

        ksSHIFT = win32api.GetAsyncKeyState(win32con.VK_SHIFT)
        if ksSHIFT & 0x8000:
            win32api.keybd_event(win32con.VK_SHIFT, 0, win32con.KEYEVENTF_KEYUP, 0)
        cInfo = win32gui.GetCursorInfo()  # save the Cursor pos

        win32api.SetCursor(None)
        win32api.SetCursorPos(pos1)
        win32api.keybd_event(win32con.VK_CONTROL, 0, 0, 0)
        win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
        win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
        win32api.keybd_event(win32con.VK_CONTROL, 0, win32con.KEYEVENTF_KEYUP, 0)
        win32api.SetCursorPos(cInfo[2])
        win32api.SetCursor(cInfo[1])
        if ksSHIFT & 0x8000:
            win32api.keybd_event(win32con.VK_SHIFT, 0, 0, 0)

        win32gui.ShowWindow(hWindow, win32con.SW_HIDE)
        time.sleep(0.1)                 # wait for the animation
        ts0 = ts1

        return 0

    return win32gui.DefWindowProc(hWindow, message, wParam, lParam)


def stop():
    '''Stop the plugin'''
    win32api.PostThreadMessage(_thread.ident, win32con.WM_QUIT, 0, 0)

if not globals().get('MANUAL_START_THREAD'):
    win32api.SetConsoleCtrlHandler(
        # always return False to continue signal chain
        lambda ct: [False, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread = threading.Thread(target=hotkey_main, daemon=True)
    _thread.start()

if __name__ == "__main__":
    logging.basicConfig(format='%(asctime)s %(filename)s:%(lineno)d %(message)s', level=logging.DEBUG)
    win32api.SetConsoleCtrlHandler(
        # return True to discontinue the signal
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread.join()
