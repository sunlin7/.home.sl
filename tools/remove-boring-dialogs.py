'''
Hide the VPN (GlobalProtect) windows after seconds idle
This tools depends on pywin32.
Run with pythonw.exe on background.
$ pythonw.exe remove-boring-dialogs.py
'''
import time
import sys
import win32gui
import win32con


def SubwinEnumerationHandler(hwnd, winList):
    if win32gui.GetWindowText(hwnd) == "Connected":
        winList.append(hwnd)
        return False            # discontinue the loop

def windowEnumerationHandler(hwnd, winList):
    '''The procedual for enumerating windows to filter out the VPN windows'''
    winStyle = win32gui.GetWindowLong(hwnd, win32con.GWL_STYLE)
    if winStyle & win32con.WS_VISIBLE:
        winTitle = win32gui.GetWindowText(hwnd)
        if "GlobalProtect Notification - " in winTitle:
            winList.append(hwnd)
        elif winTitle == "GlobalProtect":
            winClsName = win32gui.GetClassName(hwnd)
            if winClsName == '#32770':
                subList = []
                win32gui.EnumChildWindows(hwnd, SubwinEnumerationHandler, subList)
                if len(subList) > 0:
                    winList.append(hwnd)


def hide_vpn_windows_main(argv=None):
    '''The main function to hide the VPN windows.'''
    lastWins = []
    while True:
        winList = []
        win32gui.EnumWindows(windowEnumerationHandler, winList)
        for x in set(winList).intersection(lastWins):  # exists VPN windows
            win32gui.ShowWindow(x, win32con.SW_HIDE)
            print(f"Hide Window: 0x{x}")

        lastWins = set(winList).difference(lastWins)  # keep the new windows
        time.sleep(3)

    return 0


if __name__ == "__main__":
    sys.exit(hide_vpn_windows_main())
