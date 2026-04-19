#!/usr/bin/env python3
"""
xunlei_magnet.py - Daemon for magnet link playback via Xunlei SDK

Runs as a persistent daemon that keeps the Xunlei SDK (and its xlairplay
HTTP proxy) alive so mpv can stream magnet downloads in real-time.

Workflow:
  1. Create Magnet task → downloads .torrent metadata
  2. Parse torrent → find video file index
  3. Create BT task with video file index → actual download
  4. Set player mode → enable streaming
  5. Get xlairplay HTTP URL → mpv can stream while downloading

Usage:
    xunlei_magnet.py daemon              # Start daemon (forks to background)
    xunlei_magnet.py play <magnet_url>   # Create magnet task, get local URL
    xunlei_magnet.py stop <task_id>      # Stop a download task
    xunlei_magnet.py shutdown            # Stop daemon
    xunlei_magnet.py status              # Check daemon status

The daemon listens on a Unix socket at /tmp/xunlei_magnet.sock.
Emacs calls 'play' which returns a local HTTP URL for mpv streaming.

Environment:
    LD_LIBRARY_PATH must include /opt/apps/com.xunlei.download/files
"""

import ctypes
import hashlib
import json
import os
import signal
import socket
import struct
import sys
import threading
import time

try:
    import bencodepy
    HAS_BENCODE = True
except ImportError:
    HAS_BENCODE = False

SDK_LIB_PATH = "/opt/apps/com.xunlei.download/files/libxl_thunder_sdk.so"
DEFAULT_SAVE_BASE = "/tmp/xunlei_magnet"
# Must use the GUI's data dir for CidStore.DB and DHT nodes
SDK_DATA_DIR = "/home/lynx/ThunderNetwork/dk"
SOCKET_PATH = "/tmp/xunlei_magnet.sock"
PID_FILE = "/tmp/xunlei_magnet.pid"


# --- C struct layouts (reverse-engineered from libxl_thunder_sdk.so) ---

class XLDownloadLibInitParam(ctypes.Structure):
    """TAG_XL_DOWNLOAD_LIB_INIT_PARAM - (char* ptr, int32 len, int32 pad) per string"""
    _fields_ = [
        ("mAppKey_ptr", ctypes.c_char_p), ("mAppKey_len", ctypes.c_int), ("_pad0", ctypes.c_int),
        ("mPackageName_ptr", ctypes.c_char_p), ("mPackageName_len", ctypes.c_int), ("_pad1", ctypes.c_int),
        ("mAppVersion_ptr", ctypes.c_char_p), ("mAppVersion_len", ctypes.c_int), ("_pad2", ctypes.c_int),
        ("mPeerId_ptr", ctypes.c_char_p), ("mPeerId_len", ctypes.c_int), ("_pad3", ctypes.c_int),
        ("mGuid_ptr", ctypes.c_char_p), ("mGuid_len", ctypes.c_int), ("_pad4", ctypes.c_int),
        ("mReserved_ptr", ctypes.c_char_p), ("mReserved_len", ctypes.c_int), ("_pad5", ctypes.c_int),
        ("mStatSavePath_ptr", ctypes.c_char_p), ("mStatSavePath_len", ctypes.c_int), ("_pad6", ctypes.c_int),
        ("mStatCfgSavePath_ptr", ctypes.c_char_p), ("mStatCfgSavePath_len", ctypes.c_int), ("_pad7", ctypes.c_int),
        ("mPermissionLevel", ctypes.c_int),
    ]


class TagTaskParamMagnet(ctypes.Structure):
    """TAG_TASK_PARAM_MAGNET - (char* ptr, int32 len, int32 pad) per string"""
    _fields_ = [
        ("mUrl_ptr", ctypes.c_char_p), ("mUrl_len", ctypes.c_int), ("_pad0", ctypes.c_int),
        ("mFilePath_ptr", ctypes.c_char_p), ("mFilePath_len", ctypes.c_int), ("_pad1", ctypes.c_int),
        ("mFileName_ptr", ctypes.c_char_p), ("mFileName_len", ctypes.c_int), ("_pad2", ctypes.c_int),
    ]


class TagTaskParamBt(ctypes.Structure):
    """TAG_TASK_PARAM_BT - (int type, int pad, int flag, int pad,
       then (char* ptr, int32 len, int32 pad) per string + file indices)
    Verified by disassembly: XL_CreateBTTask sets mTaskType=1, mFlag=5."""
    _fields_ = [
        ("mTaskType", ctypes.c_int), ("_pad0", ctypes.c_int),
        ("mFlag", ctypes.c_int), ("_pad1", ctypes.c_int),
        ("mTorrentPath_ptr", ctypes.c_char_p), ("mTorrentPath_len", ctypes.c_int), ("_pad2", ctypes.c_int),
        ("mSavePath_ptr", ctypes.c_char_p), ("mSavePath_len", ctypes.c_int), ("_pad3", ctypes.c_int),
        ("mFileIndices_ptr", ctypes.c_void_p), ("mFileIndices_len", ctypes.c_int), ("_pad4", ctypes.c_int),
    ]


# --- SDK wrapper ---

class XunleiSDK:
    def __init__(self):
        self._sdk = None
        self._bufs = []
        self._tasks = {}  # bt_task_id -> {url, save_path, magnet_url, ...}
        self._aliases = {}  # placeholder_id -> real_bt_task_id
        self._lock = threading.RLock()

    def _make_str(self, s):
        """Create a persistent string buffer for ctypes structs."""
        buf = ctypes.create_string_buffer(s, max(len(s), 1))
        self._bufs.append(buf)
        return ctypes.cast(buf, ctypes.c_char_p), len(s)

    def init(self):
        self._sdk = ctypes.CDLL(SDK_LIB_PATH)
        param = XLDownloadLibInitParam()
        for s, name in [
            (b"linux_thunder", "mAppKey"),
            (b"bGludXhfdGh1bmRlcgClFwE=", "mPackageName"),
            (b"1.0.0.1", "mAppVersion"),
            (b"", "mPeerId"),
            (b"", "mGuid"),
            (b"", "mReserved"),
            (SDK_DATA_DIR.encode(), "mStatSavePath"),
            (SDK_DATA_DIR.encode(), "mStatCfgSavePath"),
        ]:
            ptr, length = self._make_str(s)
            setattr(param, f"{name}_ptr", ptr)
            setattr(param, f"{name}_len", length)
        param.mPermissionLevel = 1
        result = self._sdk.XLInit(ctypes.byref(param))
        if result not in (9000, 9101):
            return False
        self._sdk.XLSetStatReportSwitch(False)
        self._sdk.XLSetSpeedLimit(ctypes.c_longlong(-1), ctypes.c_longlong(-1))
        return True

    def create_magnet_task(self, magnet_url, save_path):
        """Create a magnet task to download torrent metadata."""
        param = TagTaskParamMagnet()
        ptr, length = self._make_str(magnet_url.encode())
        param.mUrl_ptr = ptr; param.mUrl_len = length
        ptr, length = self._make_str(save_path.encode())
        param.mFilePath_ptr = ptr; param.mFilePath_len = length
        ptr, length = self._make_str(b"download")
        param.mFileName_ptr = ptr; param.mFileName_len = length

        task_id = ctypes.c_ulong(0)
        self._sdk.XLCreateBtMagnetTask.argtypes = [
            ctypes.POINTER(TagTaskParamMagnet), ctypes.POINTER(ctypes.c_ulong)
        ]
        self._sdk.XLCreateBtMagnetTask.restype = ctypes.c_int
        result = self._sdk.XLCreateBtMagnetTask(ctypes.byref(param), ctypes.byref(task_id))
        return task_id.value, result

    def create_bt_task(self, torrent_path, save_path, file_indices):
        """Create a BT task from a torrent file with selected file indices.
        Returns (bt_task_id, result_code)."""
        param = TagTaskParamBt()
        param.mTaskType = 1
        param.mFlag = 5

        ptr, length = self._make_str(torrent_path.encode())
        param.mTorrentPath_ptr = ptr; param.mTorrentPath_len = length
        ptr, length = self._make_str(save_path.encode())
        param.mSavePath_ptr = ptr; param.mSavePath_len = length

        n = len(file_indices)
        arr = (ctypes.c_uint * n)(*file_indices)
        self._bufs.append(arr)
        param.mFileIndices_ptr = ctypes.cast(arr, ctypes.c_void_p)
        param.mFileIndices_len = n

        task_id = ctypes.c_ulong(0)
        self._sdk.XLCreateBtTask.argtypes = [
            ctypes.c_void_p, ctypes.POINTER(ctypes.c_ulong)
        ]
        self._sdk.XLCreateBtTask.restype = ctypes.c_int
        result = self._sdk.XLCreateBtTask(ctypes.byref(param), ctypes.byref(task_id))
        return task_id.value, result

    def start_task(self, task_id):
        self._sdk.XLStartTask.argtypes = [ctypes.c_ulong]
        self._sdk.XLStartTask.restype = ctypes.c_int
        return self._sdk.XLStartTask(ctypes.c_ulong(task_id))

    def set_player_mode(self, task_id, mode=1):
        self._sdk.XLSetPlayerMode.argtypes = [ctypes.c_ulong, ctypes.c_int]
        self._sdk.XLSetPlayerMode.restype = ctypes.c_int
        return self._sdk.XLSetPlayerMode(ctypes.c_ulong(task_id), ctypes.c_int(mode))

    def get_local_url(self, file_path):
        """Get xlairplay HTTP proxy URL for a file being downloaded."""
        self._sdk.XLGetLocalUrl.argtypes = [
            ctypes.c_char_p, ctypes.c_int, ctypes.c_char_p, ctypes.c_int
        ]
        self._sdk.XLGetLocalUrl.restype = ctypes.c_int
        url_buf = ctypes.create_string_buffer(4096)
        result = self._sdk.XLGetLocalUrl(
            file_path.encode('utf-8'), len(file_path.encode('utf-8')), url_buf, 4096
        )
        url = url_buf.value.decode() if url_buf.value else ""
        return result, url

    def stop_task(self, task_id):
        self._sdk.XLStopTask.argtypes = [ctypes.c_ulong]
        self._sdk.XLStopTask.restype = ctypes.c_int
        return self._sdk.XLStopTask(ctypes.c_ulong(task_id))

    def release_task(self, task_id):
        self._sdk.XLReleaseTask.argtypes = [ctypes.c_ulong]
        self._sdk.XLReleaseTask.restype = ctypes.c_int
        return self._sdk.XLReleaseTask(ctypes.c_ulong(task_id))

    VIDEO_EXTS = {'.mp4', '.mkv', '.avi', '.wmv', '.flv', '.mov', '.ts', '.m2ts'}

    def _parse_torrent_files(self, torrent_path):
        """Parse a .torrent file to list all files with video extensions.
        Returns list of (index, filename, size) sorted by size desc.
        """
        if not HAS_BENCODE:
            return []
        try:
            with open(torrent_path, 'rb') as f:
                data = bencodepy.decode(f.read())
            info = data[b'info']
            files = info[b'files']
            result = []
            for i, f in enumerate(files):
                length = f[b'length']
                path = f[b'path']
                filename = path[-1].decode('utf-8', errors='replace') if path else 'unknown'
                ext = os.path.splitext(filename)[1].lower()
                if ext in self.VIDEO_EXTS:
                    result.append((i, filename, length))
            result.sort(key=lambda x: x[2], reverse=True)
            return result
        except Exception:
            return []

    def _find_video_file_in_dir(self, save_path, video_name=None):
        """Find video files in the download directory.
        If video_name is given, find that specific file.
        Otherwise returns list of (filename, size) sorted by size desc.
        """
        if not os.path.isdir(save_path):
            return [] if not video_name else None
        if video_name:
            full = os.path.join(save_path, video_name)
            if os.path.isfile(full):
                return full, os.path.getsize(full)
            # Search subdirectories
            for root, dirs, files in os.walk(save_path):
                for name in files:
                    if name == video_name:
                        full = os.path.join(root, name)
                        return full, os.path.getsize(full)
            return None
        results = []
        for name in os.listdir(save_path):
            ext = os.path.splitext(name)[1].lower()
            if ext in self.VIDEO_EXTS:
                full = os.path.join(save_path, name)
                size = os.path.getsize(full) if os.path.isfile(full) else 0
                results.append((name, size))
        results.sort(key=lambda x: x[1], reverse=True)
        return results

    def _format_size(self, n):
        """Format byte count as human-readable string."""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if n < 1024:
                return f"{n:.1f}{unit}"
            n /= 1024
        return f"{n:.1f}TB"

    def _is_valid_torrent(self, path):
        """Check if a file is a valid torrent by trying to parse it."""
        if not HAS_BENCODE:
            # Fallback: just check it's a bencode dict starting with 'd'
            try:
                with open(path, 'rb') as f:
                    return f.read(1) == b'd' and os.path.getsize(path) > 100
            except Exception:
                return False
        try:
            with open(path, 'rb') as f:
                data = bencodepy.decode(f.read())
            return b'info' in data
        except Exception:
            return False

    @staticmethod
    def _extract_info_hash(magnet_url):
        """Extract btih info hash from magnet URL for dedup."""
        import re
        m = re.search(r'xt=urn:btih:([a-fA-F0-9]{40})', magnet_url)
        return m.group(1).lower() if m else magnet_url.lower()

    def play_magnet(self, magnet_url, max_wait=180):
        """Start magnet playback. Returns immediately with task info.
        Download continues in a background thread; poll 'progress' for status.
        If the same magnet is already being downloaded, returns its progress."""
        info_hash = self._extract_info_hash(magnet_url)
        # Check if we already have a task for this magnet (by info hash)
        with self._lock:
            for tid, info in self._tasks.items():
                existing_hash = self._extract_info_hash(info.get("magnet_url", ""))
                if existing_hash == info_hash:
                    # Multi-file torrent: reset to needs_selection so user can re-select
                    if len(info.get("video_files", [])) > 1:
                        info["phase"] = "needs_selection"
                    return self._task_progress(tid)

        h = info_hash[:12]
        save_path = os.path.join(DEFAULT_SAVE_BASE, h)
        os.makedirs(save_path, exist_ok=True)

        # Register a placeholder task so we can return task_id immediately
        placeholder_id = hash(magnet_url) & 0xFFFFFFFF
        with self._lock:
            self._tasks[placeholder_id] = {
                "url": "",
                "save_path": save_path,
                "magnet_url": magnet_url,
                "local_file": "",
                "video_index": 0,
                "video_name": "",
                "magnet_task_id": 0,
                "phase": "fetching_metadata",
                "downloaded": 0,
                "total_size": 0,
                "torrent_file": "",
                "video_files": [],
            }

        # Launch background thread for the actual download
        t = threading.Thread(
            target=self._play_magnet_bg,
            args=(magnet_url, save_path, placeholder_id, max_wait),
            daemon=True
        )
        t.start()

        return self._task_progress(placeholder_id)

    def _play_magnet_bg(self, magnet_url, save_path, task_id, max_wait):
        """Background thread: magnet → torrent → BT task → wait for data."""
        torrent_file = os.path.join(save_path, "download")

        # Check if we already have a valid torrent file from a previous attempt
        torrent_ready = os.path.exists(torrent_file) and self._is_valid_torrent(torrent_file)

        if not torrent_ready:
            # Step 1: Create magnet task (downloads .torrent metadata)
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "fetching_metadata"

            magnet_task_id, code = self.create_magnet_task(magnet_url, save_path)
            if code != 9000 or magnet_task_id == 0:
                with self._lock:
                    if task_id in self._tasks:
                        self._tasks[task_id]["phase"] = "error"
                        self._tasks[task_id]["error"] = f"create_magnet_task_failed: {code}"
                return

            start_code = self.start_task(magnet_task_id)
            if start_code != 9000:
                with self._lock:
                    if task_id in self._tasks:
                        self._tasks[task_id]["phase"] = "error"
                        self._tasks[task_id]["error"] = f"start_magnet_task_failed: {start_code}"
                return

            # Step 2: Wait for torrent metadata (up to 120s)
            for i in range(60):
                time.sleep(2)
                if os.path.exists(torrent_file) and self._is_valid_torrent(torrent_file):
                    torrent_ready = True
                    break

            if not torrent_ready:
                with self._lock:
                    if task_id in self._tasks:
                        self._tasks[task_id]["phase"] = "error"
                        self._tasks[task_id]["error"] = "torrent_metadata_timeout"
                return

        # Step 3: Parse torrent to list video files
        video_files = self._parse_torrent_files(torrent_file)
        if not video_files:
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "error"
                    self._tasks[task_id]["error"] = "torrent_parse_failed"
            return

        # Store torrent path and video file list
        with self._lock:
            if task_id in self._tasks:
                self._tasks[task_id]["torrent_file"] = torrent_file
                self._tasks[task_id]["video_files"] = [
                    {"index": idx, "name": name, "size": size,
                     "size_h": self._format_size(size)}
                    for idx, name, size in video_files
                ]

        # If only one video file, auto-select it
        if len(video_files) == 1:
            video_idx, video_name = video_files[0][0], video_files[0][1]
        else:
            # Multiple video files — pause and wait for user selection
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "needs_selection"
                    self._tasks[task_id]["video_name"] = video_files[0][1]
            return

        # Auto-selected single file — continue to create BT task
        self._create_bt_and_wait(magnet_url, save_path, task_id, max_wait,
                                 torrent_file, video_idx, video_name)

    def _create_bt_and_wait(self, magnet_url, save_path, task_id, max_wait,
                            torrent_file, video_idx, video_name):
        """Create BT task for a selected video file and wait for streaming URL."""
        with self._lock:
            if task_id in self._tasks:
                self._tasks[task_id]["video_name"] = video_name
                self._tasks[task_id]["video_index"] = video_idx
                self._tasks[task_id]["phase"] = "creating_bt_task"

        # Create BT task with selected video file index
        bt_task_id, bt_code = self.create_bt_task(
            torrent_file, save_path, [video_idx])
        if bt_code != 9000 or bt_task_id == 0:
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "error"
                    self._tasks[task_id]["error"] = f"create_bt_task_failed: {bt_code}"
            return

        # Replace placeholder with real bt_task_id, add alias
        with self._lock:
            info = self._tasks.pop(task_id, None)
            if info:
                self._tasks[bt_task_id] = info
                self._aliases[task_id] = bt_task_id
                task_id = bt_task_id

        # Start BT download + enable streaming mode
        start_bt = self.start_task(bt_task_id)
        if start_bt != 9000:
            with self._lock:
                if bt_task_id in self._tasks:
                    self._tasks[bt_task_id]["phase"] = "error"
                    self._tasks[bt_task_id]["error"] = f"start_bt_task_failed: {start_bt}"
            return

        self.set_player_mode(bt_task_id, 1)

        with self._lock:
            if bt_task_id in self._tasks:
                self._tasks[bt_task_id]["phase"] = "downloading"

        # Wait for video data to appear (at least 2MB for streaming)
        video_file_path = None
        for i in range(max_wait // 2):
            time.sleep(2)
            found = self._find_video_file_in_dir(save_path, video_name)
            if found:
                video_file_path, downloaded = found
                with self._lock:
                    if bt_task_id in self._tasks:
                        self._tasks[bt_task_id]["downloaded"] = downloaded
                        self._tasks[bt_task_id]["total_size"] = downloaded
                        self._tasks[bt_task_id]["local_file"] = video_file_path
                if downloaded > 2 * 1024 * 1024:
                    break
            else:
                # Fallback: check any video file
                videos = self._find_video_file_in_dir(save_path)
                if videos:
                    video_file_path = os.path.join(save_path, videos[0][0])
                    downloaded = os.path.getsize(video_file_path) if os.path.isfile(video_file_path) else 0
                    with self._lock:
                        if bt_task_id in self._tasks:
                            self._tasks[bt_task_id]["downloaded"] = downloaded
                            self._tasks[bt_task_id]["total_size"] = videos[0][1]
                            self._tasks[bt_task_id]["local_file"] = video_file_path
                    if downloaded > 2 * 1024 * 1024:
                        break

        if not video_file_path:
            with self._lock:
                if bt_task_id in self._tasks:
                    self._tasks[bt_task_id]["phase"] = "error"
                    self._tasks[bt_task_id]["error"] = "video_download_timeout"
            return

        # Get xlairplay HTTP URL for streaming
        result, url = self.get_local_url(video_file_path)
        with self._lock:
            if bt_task_id in self._tasks:
                self._tasks[bt_task_id]["url"] = url
                self._tasks[bt_task_id]["phase"] = "ready"

    def select_file(self, task_id, file_index):
        """User selected a file from the list. Continue with BT task creation.
        If the same file was previously selected and is ready, return it directly."""
        task_id = int(task_id)
        with self._lock:
            # Resolve alias
            real_id = self._aliases.get(task_id, task_id)
            if real_id not in self._tasks:
                return {"error": "task_not_found"}
            info = self._tasks[real_id]
            task_id = real_id

        if info.get("phase") != "needs_selection":
            return {"error": "task_not_awaiting_selection", "phase": info.get("phase")}

        video_files = info.get("video_files", [])
        selected = None
        for vf in video_files:
            if vf["index"] == file_index:
                selected = vf
                break
        if not selected:
            return {"error": "invalid_file_index", "available": [vf["index"] for vf in video_files]}

        # If same file was previously downloaded and is ready, reuse it
        if info.get("video_index") == file_index and info.get("phase") == "needs_selection" and info.get("url"):
            with self._lock:
                info["phase"] = "ready"
                info["video_name"] = selected["name"]
            return self._task_progress(task_id)

        # Launch background thread to continue with selected file
        torrent_file = info.get("torrent_file", "")
        save_path = info.get("save_path", "")
        magnet_url = info.get("magnet_url", "")
        max_wait = 180
        video_name = selected["name"]

        t = threading.Thread(
            target=self._create_bt_and_wait,
            args=(magnet_url, save_path, task_id, max_wait,
                  torrent_file, file_index, video_name),
            daemon=True
        )
        t.start()
        return {"status": "creating_bt_task", "video_name": video_name}

    def _task_progress(self, task_id):
        """Get progress info for a task. Supports both real and placeholder IDs."""
        with self._lock:
            # Resolve alias (placeholder -> real task_id)
            real_id = self._aliases.get(task_id, task_id)
            if real_id not in self._tasks:
                return {"error": "task_not_found"}
            info = self._tasks[real_id]
            task_id = real_id
        # Refresh downloaded size
        local_file = info.get("local_file", "")
        downloaded = info.get("downloaded", 0)
        if local_file and os.path.isfile(local_file):
            downloaded = os.path.getsize(local_file)
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["downloaded"] = downloaded
        result = {
            "status": info.get("phase", "unknown"),
            "url": info.get("url", ""),
            "local_file": local_file,
            "task_id": task_id,
            "save_path": info.get("save_path", ""),
            "video_name": info.get("video_name", ""),
            "downloaded": downloaded,
            "downloaded_h": self._format_size(downloaded),
            "total_size": info.get("total_size", 0),
        }
        # Include video file list when awaiting selection
        if info.get("phase") == "needs_selection":
            result["video_files"] = info.get("video_files", [])
        return result

    def stop_magnet(self, task_id):
        task_id = int(task_id)
        with self._lock:
            # Resolve alias
            real_id = self._aliases.pop(task_id, task_id)
            if real_id in self._tasks:
                self.stop_task(real_id)
                self.release_task(real_id)
                del self._tasks[real_id]
                return {"status": "ok"}
        return {"error": "task_not_found"}

    def get_status(self):
        with self._lock:
            tasks = {}
            for tid, info in self._tasks.items():
                task_data = dict(info)
                # Refresh download size
                lf = task_data.get("local_file", "")
                if lf and os.path.isfile(lf):
                    task_data["downloaded"] = os.path.getsize(lf)
                    task_data["downloaded_h"] = self._format_size(task_data["downloaded"])
                tasks[str(tid)] = task_data
            return {
                "status": "running",
                "tasks": tasks,
                "task_count": len(tasks),
            }

    def shutdown(self):
        with self._lock:
            for task_id in list(self._tasks.keys()):
                self.stop_task(task_id)
                self.release_task(task_id)
            self._tasks.clear()
        self._sdk.XLUnInit.argtypes = []
        self._sdk.XLUnInit.restype = ctypes.c_int
        self._sdk.XLUnInit()


# --- Socket protocol ---

def _send_msg(sock, data):
    """Send a length-prefixed JSON message."""
    msg = json.dumps(data).encode()
    sock.sendall(struct.pack('!I', len(msg)) + msg)


def _recv_msg(sock):
    """Receive a length-prefixed JSON message."""
    raw_len = _recvall(sock, 4)
    if not raw_len:
        return None
    msg_len = struct.unpack('!I', raw_len)[0]
    data = _recvall(sock, msg_len)
    if not data:
        return None
    return json.loads(data.decode())


def _recvall(sock, n):
    data = b''
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data += packet
    return data


# --- Daemon ---

def _run_daemon(sdk):
    """Main daemon loop: listen on Unix socket and handle commands.
    Each client connection is handled in a separate thread so that
    long-running 'play' commands don't block 'status' queries."""
    if os.path.exists(SOCKET_PATH):
        os.unlink(SOCKET_PATH)

    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(SOCKET_PATH)
    os.chmod(SOCKET_PATH, 0o600)
    server.listen(5)

    running = True

    def handle_client(conn):
        nonlocal running
        try:
            msg = _recv_msg(conn)
            if not msg:
                return
            cmd = msg.get("cmd", "")
            if cmd == "play":
                result = sdk.play_magnet(
                    msg.get("url", ""),
                    msg.get("max_wait", 180)
                )
                _send_msg(conn, result)
            elif cmd == "stop":
                result = sdk.stop_magnet(msg.get("task_id", 0))
                _send_msg(conn, result)
            elif cmd == "status":
                _send_msg(conn, sdk.get_status())
            elif cmd == "progress":
                task_id = msg.get("task_id", 0)
                if task_id:
                    _send_msg(conn, sdk._task_progress(int(task_id)))
                else:
                    _send_msg(conn, {"error": "task_id required"})
            elif cmd == "select":
                task_id = msg.get("task_id", 0)
                file_index = msg.get("file_index", 0)
                _send_msg(conn, sdk.select_file(int(task_id), int(file_index)))
            elif cmd == "shutdown":
                _send_msg(conn, {"status": "shutting_down"})
                running = False
            else:
                _send_msg(conn, {"error": f"unknown_command: {cmd}"})
        except Exception as e:
            try:
                _send_msg(conn, {"error": str(e)})
            except:
                pass

    server.settimeout(1.0)
    while running:
        try:
            conn, _ = server.accept()
            # Handle each client in a thread so 'play' doesn't block 'status'
            t = threading.Thread(target=handle_client, args=(conn,), daemon=True)
            t.start()
        except socket.timeout:
            continue
        except OSError:
            break

    sdk.shutdown()
    server.close()
    if os.path.exists(SOCKET_PATH):
        os.unlink(SOCKET_PATH)
    if os.path.exists(PID_FILE):
        os.unlink(PID_FILE)


def start_daemon():
    """Start the daemon process."""
    if os.path.exists(PID_FILE):
        try:
            with open(PID_FILE) as f:
                pid = int(f.read().strip())
            os.kill(pid, 0)
            print(json.dumps({"status": "already_running", "pid": pid}))
            return
        except (ProcessLookupError, ValueError):
            if os.path.exists(PID_FILE):
                os.unlink(PID_FILE)
            if os.path.exists(SOCKET_PATH):
                os.unlink(SOCKET_PATH)

    pid = os.fork()
    if pid > 0:
        time.sleep(0.5)
        if os.path.exists(PID_FILE):
            with open(PID_FILE) as f:
                daemon_pid = f.read().strip()
            print(json.dumps({"status": "started", "pid": int(daemon_pid)}))
        else:
            print(json.dumps({"status": "start_failed"}))
        return

    os.setsid()
    pid2 = os.fork()
    if pid2 > 0:
        os._exit(0)

    with open(PID_FILE, 'w') as f:
        f.write(str(os.getpid()))

    devnull = os.open(os.devnull, os.O_RDWR)
    os.dup2(devnull, 0)
    os.dup2(devnull, 1)
    os.dup2(devnull, 2)
    os.close(devnull)

    sdk = XunleiSDK()
    if not sdk.init():
        os.unlink(PID_FILE)
        os._exit(1)

    _run_daemon(sdk)
    os._exit(0)


def send_command(cmd_dict):
    """Send a command to the running daemon and return the response."""
    if not os.path.exists(SOCKET_PATH):
        return {"error": "daemon_not_running"}

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(SOCKET_PATH)
        _send_msg(sock, cmd_dict)
        return _recv_msg(sock)
    except Exception as e:
        return {"error": str(e)}
    finally:
        sock.close()


def main():
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <daemon|play|stop|shutdown|status> [args...]",
              file=sys.stderr)
        sys.exit(1)

    command = sys.argv[1]

    if command == "daemon":
        start_daemon()
    elif command == "play":
        if len(sys.argv) < 3:
            print("Usage: xunlei_magnet.py play <magnet_url> [max_wait]", file=sys.stderr)
            sys.exit(1)
        if not os.path.exists(SOCKET_PATH):
            start_daemon()
            time.sleep(1)
        magnet_url = sys.argv[2]
        max_wait = int(sys.argv[3]) if len(sys.argv) > 3 else 180
        result = send_command({"cmd": "play", "url": magnet_url, "max_wait": max_wait})
        print(json.dumps(result))
    elif command == "stop":
        if len(sys.argv) < 3:
            print("Usage: xunlei_magnet.py stop <task_id>", file=sys.stderr)
            sys.exit(1)
        result = send_command({"cmd": "stop", "task_id": int(sys.argv[2])})
        print(json.dumps(result))
    elif command == "shutdown":
        result = send_command({"cmd": "shutdown"})
        print(json.dumps(result))
    elif command == "status":
        result = send_command({"cmd": "status"})
        print(json.dumps(result))
    else:
        print(f"Unknown command: {command}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
