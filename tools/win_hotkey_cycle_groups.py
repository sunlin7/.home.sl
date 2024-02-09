''' -*- encode:utf-8 -*-
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

import threading
import win32process
import numpy as np
import cv2
import time
import sys

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
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_GRO_NEXT, MOD_NOREPEAT | win32con.MOD_WIN, VK_OEM_7)
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_GRO_PREV, MOD_NOREPEAT | win32con.MOD_WIN | win32con.MOD_SHIFT, VK_OEM_7)
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_GRO_LAST, MOD_NOREPEAT | win32con.MOD_WIN, VK_OEM_4)
    windll.user32.SetThreadDpiAwarenessContext(wintypes.HANDLE(-2))  # Toggle ON
    msg = ctypes.wintypes.MSG()
    while windll.user32.GetMessageW(ctypes.byref(msg), 0, 0, 0) != 0:
        if msg.message == win32con.WM_HOTKEY \
           and msg.wParam in [HOTKEY_CYCLE_GRO_PREV, HOTKEY_CYCLE_GRO_NEXT, HOTKEY_CYCLE_GRO_LAST]:
            hwndDesktop = win32gui.GetDesktopWindow()
            hwndTaskbar = win32gui.FindWindow("Shell_TrayWnd", None)
            if not all([hwndDesktop, hwndTaskbar]):  # non-exists Windows Explore
                print (f"missed desktop: {hwndDesktop}, or taskbar: {hwndTaskbar}", file=sys.stderr)
                continue

            rectDesktop = win32gui.GetWindowRect(hwndDesktop)
            rectTaskbar = win32gui.GetWindowRect(hwndTaskbar)
            if rectTaskbar[3] > rectDesktop[3]:  # taskbar hided
                print (f"taskbar hided", file=sys.stderr)
                continue

            H = int((rectTaskbar[3] - rectTaskbar[1]) / 6)
            img = igrab.grab((rectTaskbar[0], rectTaskbar[3] - H, rectTaskbar[2], rectTaskbar[3]))
            imgCv = np.array(img)
            imgBlur = cv2.GaussianBlur(imgCv, (3, 3), 0)
            imgGray = cv2.cvtColor(imgBlur, cv2.COLOR_BGR2GRAY)
            imgBin = cv2.Canny(imgGray, 30, 100, apertureSize=3)
            contours, _hierarchy = cv2.findContours(imgBin, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            if (len(contours) < 2):  # less than two window groups on taskbar
                print (f"less than 2 windows groups", file=sys.stderr)
                continue

            rectList = [cv2.boundingRect(x) for x in contours]
            print(f"rectList: {rectList}")
            rectList.sort()     # the contours maybe in reverse order
            activeIdx = next((idx for idx,val in enumerate(rectList) if val[2]>H), -1)
            off = 1 if msg.wParam == HOTKEY_CYCLE_GRO_NEXT else -1 if msg.wParam == HOTKEY_CYCLE_GRO_PREV else -activeIdx-1
            nextIdx = (activeIdx + off) % len(rectList)
            print(f"activeIdx: {activeIdx}, off: {off}, nextIdx: {nextIdx}, hotkey: {msg.wParam}")
            tagRect = rectList[nextIdx]
            cInfo = win32gui.GetCursorInfo()  # save the Cursor pos
            pos1 = win32gui.ClientToScreen(hwndTaskbar, (tagRect[0] + int(tagRect[2]/2), int(H*3)))
            print (f"pos1: {pos1}")
            if hwndTaskbar != win32gui.WindowFromPoint((0, 2398)):
                print (f"taskbar covered by: {hwndTaskbar}", file=sys.stderr)
                continue
            ksSHIFT = win32api.GetAsyncKeyState(win32con.VK_SHIFT)
            if ksSHIFT & 0x8000:
                win32api.keybd_event(win32con.VK_SHIFT, 0, win32con.KEYEVENTF_KEYUP, 0)
            win32api.SetCursor(None)
            win32api.SetCursorPos(pos1)
            win32api.keybd_event(win32con.VK_CONTROL, 0, 0, 0)
            win32api.mouse_event(win32con.MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0)
            win32api.mouse_event(win32con.MOUSEEVENTF_LEFTUP, 0, 0, 0, 0)
            win32api.keybd_event(win32con.VK_CONTROL, 0, win32con.KEYEVENTF_KEYUP, 0)
            win32api.SetCursorPos(cInfo[2])  # restore the Cursor
            win32api.SetCursor(cInfo[1])
            if ksSHIFT & 0x8000:
                win32api.keybd_event(win32con.VK_SHIFT, 0, 0, 0)
            time.sleep(0.1)                 # wait for the animation

        else:
            win32gui.TranslateMessage(ctypes.byref(msg))
            win32gui.DispatchMessage(ctypes.byref(msg))

    # unregister the keys
    windll.user32.UnregisterHotKey(None, HOTKEY_CYCLE_GRO_NEXT)
    windll.user32.UnregisterHotKey(None, HOTKEY_CYCLE_GRO_PREV)

    return 0

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
    win32api.SetConsoleCtrlHandler(
        # return True to discontinue the signal
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread.join()
