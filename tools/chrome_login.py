import logging
import json
import os
import subprocess
import threading
import time
import win32api

# Configuration
PREF_PATH = os.path.join(os.environ['USERPROFILE'], r'AppData\Local\Google\Chrome\User Data\Default\Preferences')
LOGIN_URL = "https://accounts.google.com/AddSession?Email=lin.sun%40zoom.us&continue=https%3A%2F%2Fgoogle.com"
CHROME_PATH = r"C:\Program Files\Google\Chrome\Application\chrome.exe"

def is_user_signed_in():
    """Checks the Default Preferences file for any signed-in profile."""
    try:
        with open(PREF_PATH, 'r', encoding='utf-8') as f:
            data = json.load(f)
            login = data.get('sync', {}).get('data_type_status_for_sync_to_signin',{}).get('account_setting')
            return login == True
    except Exception:
        pass
    return False

def chrome_login_main():
    """Check the Chrome login status and popup login page if expired."""
    while True:
       logging.debug("try check status")
       if not is_user_signed_in():
           logging.debug("No login detected. Launching Chrome...")
           subprocess.Popen([CHROME_PATH, LOGIN_URL])
           while not is_user_signed_in():
               logging.debug("Wainting for Chrome user login!")
               time.sleep(5)

       time.sleep(5) # next status

if not globals().get('MANUAL_START_THREAD'):
    '''The main function when running as a plugin'''
    win32api.SetConsoleCtrlHandler(
        # always return False to continue signal chain
        lambda ct: [False, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread = threading.Thread(target=chrome_login_main, daemon=True)
    _thread.start()

if __name__ == "__main__":
    # return True to discontinue the signal
    win32api.SetConsoleCtrlHandler(
        lambda ct: [True, ct == win32con.CTRL_C_EVENT and stop()][0], True)
    _thread.join()
