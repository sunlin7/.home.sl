import ctypes
from ctypes import wintypes

user32 = ctypes.WinDLL("user32", use_last_error=True)

# Constants
QDC_DATABASE_CURRENT = 0x00000004

# --- Minimal required structures ---

class LUID(ctypes.Structure):
    _fields_ = [("LowPart", wintypes.DWORD), ("HighPart", wintypes.LONG)]

class DISPLAYCONFIG_RATIONAL(ctypes.Structure):
    _fields_ = [("Numerator", wintypes.UINT), ("Denominator", wintypes.UINT)]

class DISPLAYCONFIG_2DREGION(ctypes.Structure):
    _fields_ = [("cx", wintypes.UINT), ("cy", wintypes.UINT)]

class DISPLAYCONFIG_VIDEO_SIGNAL_INFO(ctypes.Structure):
    _fields_ = [
        ("pixelRate",       ctypes.c_uint64),
        ("hSyncFreq",       DISPLAYCONFIG_RATIONAL),
        ("vSyncFreq",       DISPLAYCONFIG_RATIONAL),
        ("activeSize",      DISPLAYCONFIG_2DREGION),
        ("totalSize",       DISPLAYCONFIG_2DREGION),
        ("videoStandard",   wintypes.UINT),
        ("scanLineOrdering",wintypes.UINT),
    ]

class DISPLAYCONFIG_TARGET_MODE(ctypes.Structure):
    _fields_ = [("targetVideoSignalInfo", DISPLAYCONFIG_VIDEO_SIGNAL_INFO)]

class POINTL(ctypes.Structure):
    _fields_ = [("x", wintypes.LONG), ("y", wintypes.LONG)]

class DISPLAYCONFIG_SOURCE_MODE(ctypes.Structure):
    _fields_ = [
        ("width",       wintypes.UINT),
        ("height",      wintypes.UINT),
        ("pixelFormat", wintypes.UINT),
        ("position",    POINTL),
    ]

class DISPLAYCONFIG_MODE_INFO_UNION(ctypes.Union):
    _fields_ = [
        ("targetMode", DISPLAYCONFIG_TARGET_MODE),
        ("sourceMode",  DISPLAYCONFIG_SOURCE_MODE),
    ]

class DISPLAYCONFIG_MODE_INFO(ctypes.Structure):
    _anonymous_ = ("u",)
    _fields_ = [
        ("infoType",    wintypes.UINT),
        ("id",          wintypes.UINT),
        ("adapterId",   LUID),
        ("u",           DISPLAYCONFIG_MODE_INFO_UNION),
    ]

class DISPLAYCONFIG_PATH_SOURCE_INFO(ctypes.Structure):
    _fields_ = [
        ("adapterId",   LUID),
        ("id",          wintypes.UINT),
        ("modeInfoIdx", wintypes.UINT),
        ("statusFlags", wintypes.UINT),
    ]

class DISPLAYCONFIG_PATH_TARGET_INFO(ctypes.Structure):
    _fields_ = [
        ("adapterId",       LUID),
        ("id",              wintypes.UINT),
        ("modeInfoIdx",     wintypes.UINT),
        ("outputTechnology",wintypes.UINT),
        ("rotation",        wintypes.UINT),
        ("scaling",         wintypes.UINT),
        ("refreshRate",     DISPLAYCONFIG_RATIONAL),
        ("scanLineOrdering",wintypes.UINT),
        ("targetAvailable", wintypes.BOOL),
        ("statusFlags",     wintypes.UINT),
    ]

class DISPLAYCONFIG_PATH_INFO(ctypes.Structure):
    _fields_ = [
        ("sourceInfo", DISPLAYCONFIG_PATH_SOURCE_INFO),
        ("targetInfo", DISPLAYCONFIG_PATH_TARGET_INFO),
        ("flags",      wintypes.UINT),
    ]

# --- Function prototypes ---

user32.GetDisplayConfigBufferSizes.restype  = wintypes.LONG
user32.GetDisplayConfigBufferSizes.argtypes = [
    wintypes.UINT,
    ctypes.POINTER(wintypes.UINT),
    ctypes.POINTER(wintypes.UINT),
]

user32.QueryDisplayConfig.restype  = wintypes.LONG
user32.QueryDisplayConfig.argtypes = [
    wintypes.UINT,
    ctypes.POINTER(wintypes.UINT),
    ctypes.POINTER(DISPLAYCONFIG_PATH_INFO),
    ctypes.POINTER(wintypes.UINT),
    ctypes.POINTER(DISPLAYCONFIG_MODE_INFO),
    ctypes.POINTER(wintypes.UINT),   # pCurrentTopologyId
]

# --- Step 1: Get buffer sizes ---

num_paths = wintypes.UINT()
num_modes = wintypes.UINT()

ret = user32.GetDisplayConfigBufferSizes(
    QDC_DATABASE_CURRENT,
    ctypes.byref(num_paths),
    ctypes.byref(num_modes),
)
if ret != 0:
    raise ctypes.WinError(ret)

# --- Step 2: Query topology ---

paths    = (DISPLAYCONFIG_PATH_INFO * num_paths.value)()
modes    = (DISPLAYCONFIG_MODE_INFO * num_modes.value)()
topology = wintypes.UINT()

ret = user32.QueryDisplayConfig(
    QDC_DATABASE_CURRENT,
    ctypes.byref(num_paths),
    paths,
    ctypes.byref(num_modes),
    modes,
    ctypes.byref(topology),   # <-- receives topology ID
)
if ret != 0:
    raise ctypes.WinError(ret)

# --- Step 3: Decode and print ---

# TOPOLOGY_MAP = {
#     0x1: "Internal only  (/internal)",
#     0x2: "Clone / Duplicate  (/clone)",
#     0x4: "Extended desktop  (/extend)",
#     0x8: "External only  (/external)",
# }

# print("Current display topology:", TOPOLOGY_MAP.get(topology.value, f"Unknown (0x{topology.value:X})"))
import subprocess
subprocess.Popen(["DisplaySwitch", "/extend" if topology.value == 0x1 else "/internal"])
