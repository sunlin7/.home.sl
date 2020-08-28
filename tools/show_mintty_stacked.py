'''
Show the mintty as two columns and two rows in the screen.
This tools depends on pywin32.
'''
import sys
import win32api
import win32gui
import win32con
import win32process
import ctypes


def getWorkArea():
    '''The SystemParametersInfo not support SPI_GETWORKAREA in pywin32, so use
    ctypes instead.'''
    SPI = ctypes.windll.user32.SystemParametersInfoW

    class RECT(ctypes.Structure):
        _fields_ = [
            ('left', ctypes.c_long),
            ('top', ctypes.c_long),
            ('right', ctypes.c_long),
            ('bottom', ctypes.c_long)
        ]

    SPI.restype = ctypes.c_bool
    SPI.argtypes = [
        ctypes.c_uint,
        ctypes.c_uint,
        ctypes.POINTER(RECT),
        ctypes.c_uint
    ]

    rect = RECT()

    result = SPI(
        win32con.SPI_GETWORKAREA,
        0,
        ctypes.byref(rect),
        0
    )
    if result:
        return (rect.left, rect.top, rect.right, rect.bottom)

    return (0, 0, 640, 480)


def windowEnumerationHandler(hwnd, top_windows):
    clsName = win32gui.GetClassName(hwnd)
    winStyle = win32gui.GetWindowLong(hwnd, win32con.GWL_STYLE)
    if clsName == 'mintty' and winStyle & win32con.WS_VISIBLE:
        threadId, processId = win32process.GetWindowThreadProcessId(hwnd)
        procHdl = win32api.OpenProcess(win32con.PROCESS_QUERY_INFORMATION,
                                       False, processId)
        info = win32process.GetProcessTimes(procHdl)
        top_windows.append((hwnd, info['CreationTime'], clsName,
                            win32gui.GetWindowText(hwnd)))


def show_mintty_stacked_main(argv=None):
    '''The main function to show mintty windows statcked'''
    top_windows = []
    win32gui.EnumWindows(windowEnumerationHandler, top_windows)
    if len(top_windows) == 0:
        print('No Mintty window be found!')
        sys.exit(1)

    top_windows.sort(key=lambda x: x[1])

    x, y, w, h = getWorkArea()
    nw, nh = int(w/2), int(h/2)
    posPre = [           # split workarea(x,y, w,h) into 2 columns and two rows
        (x-6,       y, nw+16, nh+8),
        (x-6,    y+nh, nw+16, nh+8),
        (x+nw-6,    y, nw+16, nh+8),
        (x+nw-6, y+nh, nw+16, nh+8),
    ]
    # rect = win32gui.GetWindowRect(top_windows[0][0])
    # if rect[:2] == posPre[0][:2]:
    #     top_windows.reverse()

    for idx, item in enumerate(top_windows[:4]):
        win32gui.ShowWindow(item[0], win32con.SW_RESTORE)
        win32gui.SetWindowPos(item[0], win32con.HWND_TOP,
                              *posPre[idx],  # it has four params
                              win32con.SWP_SHOWWINDOW)

    return 0


if __name__ == "__main__":
    sys.exit(show_mintty_stacked_main())
