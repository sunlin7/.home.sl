'''
Cycle the windows in their group.
This tools depends on pywin32.
Run with pythonw.exe on background.
$ pythonw.exe win_hotkey_cycle_window.py
'''
import ctypes
import logging
import pythoncom
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

GUID = ctypes.c_ubyte * 16

class IAppResolver8Vtbl(ctypes.Structure):
    _fields_ = [
        ('QueryInterface', ctypes.CFUNCTYPE(ctypes.HRESULT, ctypes.c_void_p, ctypes.POINTER(GUID), ctypes.POINTER(ctypes.c_void_p))),
        ('AddRef', ctypes.CFUNCTYPE(ctypes.c_ulong, ctypes.c_void_p)),
        ('Release', ctypes.CFUNCTYPE(ctypes.c_ulong, ctypes.c_void_p)),
        ('GetAppIDForShortcut', ctypes.CFUNCTYPE(ctypes.HRESULT, ctypes.c_void_p)),
        ('GetAppIDForShortcutObject', ctypes.CFUNCTYPE(ctypes.c_ulong, ctypes.c_void_p)),
        ('GetAppIDForWindow', ctypes.CFUNCTYPE(ctypes.HRESULT, ctypes.c_void_p, wintypes.HWND, ctypes.POINTER(wintypes.LPWSTR), ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p)),
        ('GetAppIDForProcess', ctypes.CFUNCTYPE(ctypes.HRESULT, ctypes.c_void_p, wintypes.DWORD, ctypes.POINTER(wintypes.LPWSTR), ctypes.c_void_p, ctypes.c_void_p, ctypes.c_void_p))
    ]

class IAppResolver8(ctypes.Structure):
    _fields_ = [('lpVtbl', ctypes.POINTER(IAppResolver8Vtbl))]

# '{660b90c8-73a9-4b58-8cae-355b7f55341b}'
CLSID_StartMenuCacheAndAppResolver = (GUID)(*bytearray.fromhex("c8900b66a973584b8cae355b7f55341b"))
# {de25675a-72de-44b4-9373-05170450c140}
IID_IAppResolver_8 = (GUID)(*bytearray.fromhex("5a6725dede72b444937305170450c140"))

ctypes.windll.ole32.CoInitialize.restype = ctypes.HRESULT
ctypes.windll.ole32.CoInitialize.argtypes = [ctypes.c_void_p]
ctypes.windll.ole32.CoUninitialize.restype = None
ctypes.windll.ole32.CoUninitialize.argtypes = None


def getAppUserId(hwnd, karg):
    '''Return the Application User ID from a HWND'''
    appResolver = karg['appResolver']
    functions = appResolver.contents.lpVtbl.contents
    pszAppId = ctypes.wintypes.LPWSTR()
    appUserID = ""
    try:
        res = functions.GetAppIDForWindow(appResolver, hwnd, ctypes.pointer(pszAppId), None, None, None)
        appUserID = pszAppId.value if res == 0 else ""
    except:
        pass
    ctypes.windll.ole32.CoTaskMemFree(pszAppId)
    return appUserID


def hotkeyWinEnumerationProc(hwnd, karg):
    appUserId = getAppUserId(hwnd, karg)
    if win32gui.IsWindowVisible(hwnd) and not win32gui.IsIconic(hwnd) \
       and (win32gui.GetClassLong(hwnd, win32con.GCW_ATOM) == karg['classAtom']
            or getAppUserId(hwnd, karg) == karg['appUserID']):
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
    ctypes.windll.ole32.CoInitialize(None)
    appResolver = ctypes.POINTER(IAppResolver8)()
    res = ctypes.windll.ole32.CoCreateInstance(CLSID_StartMenuCacheAndAppResolver, None, pythoncom.CLSCTX_INPROC_SERVER | pythoncom.CLSCTX_INPROC_HANDLER, IID_IAppResolver_8, ctypes.pointer(appResolver))
    if res != 0:
        logging.error(f"CoCreateInstance failed with {res}")
        return -1

    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_WIN_NEXT, MOD_NOREPEAT | win32con.MOD_WIN, VK_GRAVE)
    win32gui.RegisterHotKey(None, HOTKEY_CYCLE_WIN_PREV, MOD_NOREPEAT | win32con.MOD_WIN | win32con.MOD_SHIFT, VK_GRAVE)
    msg = wintypes.MSG()
    while windll.user32.GetMessageW(ctypes.byref(msg), 0, 0, 0) != 0:
        if msg.message == win32con.WM_HOTKEY \
           and msg.wParam in [HOTKEY_CYCLE_WIN_PREV, HOTKEY_CYCLE_WIN_NEXT]:
            hwnd = win32gui.GetForegroundWindow()
            if not hwnd:        # no foreground window
                continue
            classAtom = win32gui.GetClassLong(hwnd, win32con.GCW_ATOM)
            if classAtom == 0:  # failed
                print (f"Error on GetWindowLong()")
                continue

            appUserID = getAppUserId(hwnd, {"appResolver": appResolver})

            # appUserID = propsys.SHGetPropertyStoreForWindow(hwnd).GetValue(pscon.PKEY_AppUserModel_ID).ToString()
            karg = {"classAtom": classAtom, "appUserID": appUserID, "wList": [], "appResolver": appResolver}
            win32gui.EnumWindows(hotkeyWinEnumerationProc, karg)
            if len(karg['wList']) < 2:  # less than 2 windows
                logging.debug(f"One window in Group, {karg}")
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
    appResolver.contents.lpVtbl.contents.Release(appResolver)
    ctypes.windll.ole32.CoUninitialize()
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
