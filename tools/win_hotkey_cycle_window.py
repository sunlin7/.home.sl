'''
Cycle the windows in their group.
This tools depends on pywin32.
Run with pythonw.exe on background.
$ pythonw.exe win_hotkey_cycle_window.py
'''
import ctypes
import win32api
import win32gui
import win32con

import threading
import win32process

from ctypes import windll, wintypes
from win32com.propsys import propsys, pscon

HOTKEY_CYCLE_WIN_NEXT = 1
HOTKEY_CYCLE_WIN_PREV = 2
VK_GRAVE = 0xC0                 # VK_OEM_3
MOD_NOREPEAT = 0x4000


def hotkeyWinEnumerationProc(hwnd, karg):
    if win32gui.GetClassLong(hwnd, win32con.GCW_ATOM) == karg['classAtom'] \
       and propsys.SHGetPropertyStoreForWindow(hwnd).GetValue(pscon.PKEY_AppUserModel_ID).ToString() == karg['appUserID'] \
       and win32con.WS_VISIBLE & win32gui.GetWindowLong(hwnd, win32con.GWL_STYLE) \
       and not win32gui.IsIconic(hwnd):
        threadId, processId = win32process.GetWindowThreadProcessId(hwnd)
        createTime = 0
        try:
            threadHdl = win32api.OpenThread(win32con.THREAD_QUERY_INFORMATION,
                                        False, threadId)
            info = win32process.GetThreadTimes(threadHdl)
            createTime = info['CreationTime']
        except Exception as e:
            print (f"Exception: {e}")
            pass

        karg['wList'].append((hwnd, threadId, createTime))


def hotkey_main():
    '''The main function to for hotkey procedural.'''
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_WIN_NEXT, MOD_NOREPEAT | win32con.MOD_WIN, VK_GRAVE)
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_WIN_PREV, MOD_NOREPEAT | win32con.MOD_WIN | win32con.MOD_SHIFT, VK_GRAVE)
    msg = wintypes.MSG()
    while windll.user32.GetMessageW(ctypes.byref(msg), 0, 0, 0) != 0:
        if msg.message == win32con.WM_HOTKEY \
           and msg.wParam in [HOTKEY_CYCLE_WIN_PREV, HOTKEY_CYCLE_WIN_NEXT]:
            hwnd = win32gui.GetForegroundWindow()
            if not hwnd:        # not a foreground window
                continue
            classAtom = win32gui.GetClassLong(hwnd, win32con.GCW_ATOM)
            if classAtom == 0:  # failed
                print (f"Error on GetWindowLong()")
                continue

            appUserID = propsys.SHGetPropertyStoreForWindow(hwnd).GetValue(pscon.PKEY_AppUserModel_ID).ToString()
            karg = {"classAtom": classAtom, "appUserID": appUserID, "wList": []}
            win32gui.EnumWindows(hotkeyWinEnumerationProc, karg)
            if len(karg['wList']) < 2:  # less than 2 windows
                continue

            karg['wList'].sort(key=lambda x: (x[2], x[0]))  # sort by CreationTime,whdl
            for idx, val in enumerate(karg['wList']):
                if val[0] == hwnd:
                    off = 1 if msg.wParam == HOTKEY_CYCLE_WIN_NEXT else -1
                    idx = (idx + off) % len(karg['wList'])
                    win32gui.SetForegroundWindow(karg['wList'][idx][0])
                    break

        else:
            win32gui.TranslateMessage(ctypes.byref(msg))
            win32gui.DispatchMessage(ctypes.byref(msg))

    # unregister
    ctypes.windll.user32.UnregisterHotKey(None, HOTKEY_CYCLE_WIN_NEXT)
    ctypes.windll.user32.UnregisterHotKey(None, HOTKEY_CYCLE_WIN_PREV)
    return 0

def stop():
    '''Stop the plugin'''
    win32api.PostThreadMessage(_thread.ident, win32con.WM_QUIT, 0, 0)

if not globals().get('MANUAL_START_THREAD'):
    '''The main function when running as a plugin'''
    win32api.SetConsoleCtrlHandler(
        # always return False to continue signal chain
        lambda ct: [False, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread = threading.Thread(target=hotkey_main, daemon=True)
    _thread.start()

if __name__ == "__main__":
    # return True to discontinue the signal
    win32api.SetConsoleCtrlHandler(
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread.join()
