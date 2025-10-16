import winreg
import ctypes
import win32con

def set_dark_mode():
    """
    Sets the Windows color setting to "Dark" by modifying the registry.
    """
    # Define the registry key path
    key_path = r"Software\Microsoft\Windows\CurrentVersion\Themes\Personalize"

    try:
        # Open the registry key for the current user
        reg_key = winreg.OpenKey(winreg.HKEY_CURRENT_USER, key_path, 0, winreg.KEY_READ | winreg.KEY_SET_VALUE)
        light, _ = winreg.QueryValueEx(reg_key, "AppsUseLightTheme")
        # inverse the color settings, light == 0 means dark, otherwise light
        ncolor = 1 if light == 0 else 0
        print(f"current light value: {light}, new value {ncolor}")
        # Set the 'AppsUseLightTheme' value to 0 for dark mode
        winreg.SetValueEx(reg_key, "AppsUseLightTheme", 0, winreg.REG_DWORD, ncolor)
        winreg.SetValueEx(reg_key, "SystemUsesLightTheme", 0, winreg.REG_DWORD, ncolor)
        winreg.CloseKey(reg_key)

        # Notify Windows of the change to update the UI
        # NOTE the Windows taskbar only works with SendMessageA, NOT working with SendMessageW or PostMessageA/W
        # res = ctypes.windll.user32.SendMessageW(
        #     # win32con.HWND_BROADCAST, win32con.WM_SETTINGCHANGE, 0,
        #     ctypes.c_wchar_p("ImmersiveColorSet")
        # )
        res, ret = ctypes.windll.user32.SendMessageTimeoutA(
            win32con.HWND_BROADCAST, win32con.WM_SETTINGCHANGE, 0,
            ctypes.c_char_p(b"ImmersiveColorSet"),
            win32con.SMTO_ABORTIFHUNG, 1)
        print(f"Windows color setting changes return : {res}")

    except FileNotFoundError:
        print(f"Registry key not found: {key_path}")
    except PermissionError:
        print("Permission denied. Run the script as administrator.")
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    set_dark_mode()
