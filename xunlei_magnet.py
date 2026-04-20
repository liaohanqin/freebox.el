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

    def bt_select_sub_task(self, task_id, file_indices):
        """Select specific sub-files for download in a BT task.
        task_id: BT task ID
        file_indices: list of unsigned int file indices to select
        Returns SDK result code (9000 = success)."""
        n = len(file_indices)
        arr = (ctypes.c_uint * n)(*file_indices)
        self._bufs.append(arr)
        self._sdk.XLBtSelectSubTask.argtypes = [ctypes.c_ulong, ctypes.c_void_p, ctypes.c_uint]
        self._sdk.XLBtSelectSubTask.restype = ctypes.c_int
        return self._sdk.XLBtSelectSubTask(ctypes.c_ulong(task_id), ctypes.cast(arr, ctypes.c_void_p), n)

    def bt_deselect_sub_task(self, task_id, file_indices):
        """Deselect specific sub-files in a BT task (stop downloading them).
        task_id: BT task ID
        file_indices: list of unsigned int file indices to deselect
        Returns SDK result code (9000 = success)."""
        n = len(file_indices)
        arr = (ctypes.c_uint * n)(*file_indices)
        self._bufs.append(arr)
        self._sdk.XLBtDeselectSubTask.argtypes = [ctypes.c_ulong, ctypes.c_void_p, ctypes.c_uint]
        self._sdk.XLBtDeselectSubTask.restype = ctypes.c_int
        return self._sdk.XLBtDeselectSubTask(ctypes.c_ulong(task_id), ctypes.cast(arr, ctypes.c_void_p), n)

    VIDEO_EXTS = {'.mp4', '.mkv', '.avi', '.wmv', '.flv', '.mov', '.ts', '.m2ts'}
    SUBTITLE_EXTS = {'.srt', '.ass', '.ssa', '.sub', '.sup', '.idx', '.vtt', '.lrc'}
    IMAGE_EXTS = {'.jpg', '.jpeg', '.png', '.gif', '.bmp', '.webp'}

    def _parse_torrent_files(self, torrent_path):
        """Parse a .torrent file to list all files with type classification.
        Returns list of (index, filename, size, file_type) sorted by size desc.
        file_type is 'video', 'subtitle', 'image', or 'other'.
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
                    ftype = 'video'
                elif ext in self.SUBTITLE_EXTS:
                    ftype = 'subtitle'
                elif ext in self.IMAGE_EXTS:
                    ftype = 'image'
                else:
                    ftype = 'other'
                result.append((i, filename, length, ftype))
            result.sort(key=lambda x: x[2], reverse=True)
            return result
        except Exception:
            return []

    def _get_torrent_file_count(self, torrent_path):
        """Return the total number of files in a .torrent (including non-video).
        Returns 0 on error or if bencodepy is unavailable."""
        if not HAS_BENCODE:
            return 0
        try:
            with open(torrent_path, 'rb') as f:
                data = bencodepy.decode(f.read())
            info = data[b'info']
            files = info.get(b'files', [])
            return len(files)
        except Exception:
            return 0

    def _get_torrent_all_filenames(self, torrent_path):
        """Return list of all filenames in a .torrent (including non-video).
        Returns list of (index, filename) tuples.
        Returns empty list on error or if bencodepy is unavailable."""
        if not HAS_BENCODE:
            return []
        try:
            with open(torrent_path, 'rb') as f:
                data = bencodepy.decode(f.read())
            info = data[b'info']
            files = info.get(b'files', [])
            result = []
            for i, f in enumerate(files):
                path = f.get(b'path', [])
                filename = path[-1].decode('utf-8', errors='replace') if path else 'unknown'
                result.append((i, filename))
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
                    # If paused, resume it
                    if info.get("phase") == "paused":
                        return self.resume_task(tid)
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
                "downloaded_h": "0.0B",
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

        # Step 3: Parse torrent to list video and subtitle files
        parsed_files = self._parse_torrent_files(torrent_file)
        if not parsed_files:
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "error"
                    self._tasks[task_id]["error"] = "torrent_parse_failed"
            return

        # Store torrent path and file list
        with self._lock:
            if task_id in self._tasks:
                self._tasks[task_id]["torrent_file"] = torrent_file
                self._tasks[task_id]["video_files"] = [
                    {"index": idx, "name": name, "size": size,
                     "size_h": self._format_size(size), "type": ftype}
                    for idx, name, size, ftype in parsed_files
                ]

        # Determine if user selection is needed
        video_only = [f for f in parsed_files if f[3] == 'video']

        # Auto-select only when there's exactly 1 file total and it's a video.
        # Otherwise (multiple files, or non-video files present), let user choose.
        if len(parsed_files) == 1 and video_only:
            video_idx, video_name = video_only[0][0], video_only[0][1]
        else:
            # Multiple files or subtitles present — let user choose
            with self._lock:
                if task_id in self._tasks:
                    self._tasks[task_id]["phase"] = "needs_selection"
                    self._tasks[task_id]["video_name"] = video_only[0][1] if video_only else ""
            return

        # Auto-selected single video file — continue to create BT task
        self._create_bt_and_wait(magnet_url, save_path, task_id, max_wait,
                                 torrent_file, [video_idx], video_name)

    def _create_bt_and_wait(self, magnet_url, save_path, task_id, max_wait,
                            torrent_file, file_indices, video_name):
        """Create BT task for selected files and get streaming URL.

        file_indices: list of file indices to download (video + optional subtitles).
        The first video file in file_indices is used for xlairplay streaming.
        With XLSetPlayerMode enabled, the xlairplay proxy can stream immediately
        from the SDK's download buffer.
        """
        with self._lock:
            if task_id in self._tasks:
                self._tasks[task_id]["video_name"] = video_name
                self._tasks[task_id]["video_index"] = file_indices[0]
                self._tasks[task_id]["file_indices"] = file_indices
                self._tasks[task_id]["phase"] = "creating_bt_task"

        # Create BT task with selected file indices
        bt_task_id, bt_code = self.create_bt_task(
            torrent_file, save_path, file_indices)
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

        # Select only the target files BEFORE starting the task.
        # XLCreateBtTask creates tasks with all files selected by default.
        # Must deselect all files then select only the targets.
        if self._get_torrent_file_count(torrent_file) > 1:
            total_files = self._get_torrent_file_count(torrent_file)
            all_indices = list(range(total_files))
            self.bt_deselect_sub_task(bt_task_id, all_indices)
            self.bt_select_sub_task(bt_task_id, file_indices)

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

        # Wait for the video file to appear on disk (up to 60s)
        # Xunlei SDK creates 0-byte sparse files; we just need the path,
        # not actual data — xlairplay streams from the SDK buffer.
        video_file_path = None
        for i in range(30):
            time.sleep(2)
            found = self._find_video_file_in_dir(save_path, video_name)
            if found:
                video_file_path, _ = found
                break
            # Fallback: check any video file
            videos = self._find_video_file_in_dir(save_path)
            if videos:
                video_file_path = os.path.join(save_path, videos[0][0])
                break

        # If file not found yet, try the expected path — xlairplay may still work
        if not video_file_path:
            video_file_path = os.path.join(save_path, video_name)

        with self._lock:
            if bt_task_id in self._tasks:
                self._tasks[bt_task_id]["local_file"] = video_file_path
                # Set total_size from torrent info if available
                if not self._tasks[bt_task_id].get("total_size"):
                    for vf in self._tasks[bt_task_id].get("video_files", []):
                        if vf["index"] == file_indices[0]:
                            self._tasks[bt_task_id]["total_size"] = vf["size"]
                            break

        # Get xlairplay HTTP URL — works immediately with player mode
        # (streams from SDK buffer, not from disk file)
        result, url = self.get_local_url(video_file_path)
        with self._lock:
            if bt_task_id in self._tasks:
                self._tasks[bt_task_id]["url"] = url
                self._tasks[bt_task_id]["phase"] = "ready"

        # Start background thread to track actual download progress
        t = threading.Thread(
            target=self._track_download,
            args=(bt_task_id, video_file_path),
            daemon=True
        )
        t.start()

    def _track_download(self, bt_task_id, video_file_path):
        """Background thread: track actual download progress and detect stalls.

        Xunlei SDK creates 0-byte sparse files during download, so
        os.path.getsize() returns the apparent (logical) size which may be
        inaccurate. We use st_blocks*512 for actual disk usage instead,
        and only update the stored 'downloaded' value when it increases.
        """
        STALL_LIMIT = 45  # 45 checks × 2s = 90s of zero progress after data appears
        stall_count = 0
        last_downloaded = 0

        for i in range(450):  # 450 × 2s = 15 minutes max tracking
            time.sleep(2)
            with self._lock:
                if bt_task_id not in self._tasks:
                    return  # Task was removed

            downloaded = 0
            if os.path.isfile(video_file_path):
                try:
                    st = os.stat(video_file_path)
                    # Use st_blocks*512 for actual disk usage (works for sparse files)
                    downloaded = st.st_blocks * 512
                    # Cap at the logical file size
                    if downloaded > st.st_size:
                        downloaded = st.st_size
                except OSError:
                    downloaded = 0

            # Only update if file size increased (never go backwards)
            with self._lock:
                if bt_task_id in self._tasks:
                    current = self._tasks[bt_task_id].get("downloaded", 0)
                    if downloaded > current:
                        self._tasks[bt_task_id]["downloaded"] = downloaded
                        self._tasks[bt_task_id]["downloaded_h"] = self._format_size(downloaded)

            # Stall detection (only after file has data)
            if downloaded > 0:
                if downloaded > last_downloaded:
                    stall_count = 0
                    last_downloaded = downloaded
                else:
                    stall_count += 1

            if stall_count >= STALL_LIMIT:
                break

            # If download appears complete, stop tracking
            with self._lock:
                if bt_task_id in self._tasks:
                    total = self._tasks[bt_task_id].get("total_size", 0)
                    if total > 0 and downloaded >= total:
                        self._tasks[bt_task_id]["downloaded"] = downloaded
                        self._tasks[bt_task_id]["downloaded_h"] = self._format_size(downloaded)
                        return

    def select_file(self, task_id, file_indices):
        """User selected files from the list. Continue with BT task creation.
        file_indices: list of file indices to download (video + optional subtitles)."""
        task_id = int(task_id)
        if isinstance(file_indices, int):
            file_indices = [file_indices]

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
        # Validate all selected indices and find the primary video
        selected_names = []
        primary_video_name = None
        for fi in file_indices:
            found = None
            for vf in video_files:
                if vf["index"] == fi:
                    found = vf
                    break
            if not found:
                return {"error": "invalid_file_index", "available": [vf["index"] for vf in video_files]}
            selected_names.append(found["name"])
            if found.get("type") == "video" and primary_video_name is None:
                primary_video_name = found["name"]

        video_name = primary_video_name or selected_names[0]

        # Stop existing BT task before creating a new one
        # (SDK rejects XLCreateBtTask if a BT task already owns the save_path)
        with self._lock:
            old_url = info.get("url", "")
            # Clear the old URL so mpv won't use it
            info["url"] = ""
            info["phase"] = "creating_bt_task"
            info["video_name"] = video_name
            info["video_index"] = file_indices[0]
            info["file_indices"] = file_indices

        # Stop and release the old BT task (safe to call even if none exists)
        try:
            self.stop_task(task_id)
            self.release_task(task_id)
        except Exception:
            pass

        # Launch background thread to continue with selected files
        torrent_file = info.get("torrent_file", "")
        save_path = info.get("save_path", "")
        magnet_url = info.get("magnet_url", "")
        max_wait = 180

        t = threading.Thread(
            target=self._create_bt_and_wait,
            args=(magnet_url, save_path, task_id, max_wait,
                  torrent_file, file_indices, video_name),
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
        # Refresh downloaded size — use st_blocks*512 for actual disk usage
        # (handles sparse files where os.path.getsize returns logical size)
        local_file = info.get("local_file", "")
        downloaded = info.get("downloaded", 0)
        if local_file and os.path.isfile(local_file):
            try:
                st = os.stat(local_file)
                disk_size = st.st_blocks * 512
                if disk_size > st.st_size:
                    disk_size = st.st_size
                if disk_size > downloaded:
                    downloaded = disk_size
                    with self._lock:
                        if task_id in self._tasks:
                            self._tasks[task_id]["downloaded"] = downloaded
                            self._tasks[task_id]["downloaded_h"] = self._format_size(downloaded)
            except OSError:
                pass
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
        # Only include error when there is one
        err = info.get("error", "")
        if err:
            result["error"] = err
        # Include video file list when awaiting selection
        if info.get("video_files"):
            result["video_files"] = info.get("video_files", [])
        return result

    def pause_task(self, task_id):
        """Pause a download task by stopping and releasing the SDK handle.

        Since XLStopTask invalidates the task handle (XLStartTask returns 9105),
        we must fully stop and release the task. Resume is done by recreating
        the task from the cached torrent file via play_magnet.
        """
        task_id = int(task_id)
        with self._lock:
            real_id = self._aliases.get(task_id, task_id)
            if real_id not in self._tasks:
                return {"error": "task_not_found"}
            info = self._tasks[real_id]
            if info.get("phase") == "paused":
                return self._task_progress(real_id)
        # Fully stop and release the SDK task
        try:
            self.stop_task(real_id)
            self.release_task(real_id)
        except Exception as e:
            return {"error": f"stop_failed: {e}"}
        with self._lock:
            self._tasks[real_id]["phase"] = "paused"
            self._tasks[real_id]["url"] = ""  # xlairplay URL invalid after stop
        return self._task_progress(real_id)

    def resume_task(self, task_id):
        """Resume a paused download task by recreating it from cached torrent.

        Since XLStopTask invalidates the task handle (XLStartTask returns 9105),
        we cannot simply restart the task. Instead, we recreate the BT task
        from the cached torrent file, re-selecting the previously chosen video file.
        This preserves the video_index so the user doesn't need to re-select.
        """
        task_id = int(task_id)
        with self._lock:
            real_id = self._aliases.get(task_id, task_id)
            if real_id not in self._tasks:
                return {"error": "task_not_found"}
            info = self._tasks[real_id]
            if info.get("phase") != "paused":
                return self._task_progress(real_id)
            magnet_url = info.get("magnet_url", "")
            save_path = info.get("save_path", "")
            torrent_file = info.get("torrent_file", "")
            video_index = info.get("video_index", 0)
            video_name = info.get("video_name", "")
            file_indices = info.get("file_indices", [video_index])
            old_downloaded = info.get("downloaded", 0)
            old_downloaded_h = info.get("downloaded_h", "0.0B")
            old_total_size = info.get("total_size", 0)
            old_video_files = info.get("video_files", [])

        if not magnet_url:
            return {"error": "no_magnet_url"}
        if not torrent_file or not os.path.isfile(torrent_file):
            # No cached torrent — fall back to full play_magnet
            return self._resume_via_play_magnet(
                task_id, real_id, magnet_url, old_downloaded, old_downloaded_h)

        # Remove the paused task so play_magnet won't find it as duplicate
        with self._lock:
            del self._tasks[real_id]
            aliases_to_remove = [k for k, v in self._aliases.items() if v == real_id]
            for k in aliases_to_remove:
                del self._aliases[k]

        # Create a new placeholder task
        placeholder_id = hash(magnet_url) & 0xFFFFFFFF
        with self._lock:
            self._tasks[placeholder_id] = {
                "url": "",
                "save_path": save_path,
                "magnet_url": magnet_url,
                "local_file": "",
                "video_index": video_index,
                "file_indices": file_indices,
                "video_name": video_name,
                "magnet_task_id": 0,
                "phase": "creating_bt_task",
                "downloaded": old_downloaded,
                "downloaded_h": old_downloaded_h,
                "total_size": old_total_size,
                "torrent_file": torrent_file,
                "video_files": old_video_files,
            }

        # Launch background thread to create BT task with previously selected files
        t = threading.Thread(
            target=self._create_bt_and_wait,
            args=(magnet_url, save_path, placeholder_id, 180,
                  torrent_file, file_indices, video_name),
            daemon=True
        )
        t.start()

        return self._task_progress(placeholder_id)

    def _resume_via_play_magnet(self, task_id, real_id, magnet_url,
                                 old_downloaded, old_downloaded_h):
        """Fallback: resume by calling play_magnet (full re-creation).
        Used when cached torrent file is not available."""
        with self._lock:
            del self._tasks[real_id]
            aliases_to_remove = [k for k, v in self._aliases.items() if v == real_id]
            for k in aliases_to_remove:
                del self._aliases[k]
        result = self.play_magnet(magnet_url)
        if old_downloaded > 0 and not result.get("error"):
            new_task_id = result.get("task_id", 0)
            with self._lock:
                if new_task_id in self._tasks:
                    if old_downloaded > self._tasks[new_task_id].get("downloaded", 0):
                        self._tasks[new_task_id]["downloaded"] = old_downloaded
                        self._tasks[new_task_id]["downloaded_h"] = old_downloaded_h
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
                task_data["task_id"] = tid
                # Refresh download size — use st_blocks*512 for actual disk usage
                lf = task_data.get("local_file", "")
                current_dl = task_data.get("downloaded", 0)
                if lf and os.path.isfile(lf):
                    try:
                        st = os.stat(lf)
                        disk_size = st.st_blocks * 512
                        if disk_size > st.st_size:
                            disk_size = st.st_size
                        if disk_size > current_dl:
                            task_data["downloaded"] = disk_size
                            task_data["downloaded_h"] = self._format_size(disk_size)
                    except OSError:
                        pass
                # Ensure downloaded_h is always present
                if "downloaded_h" not in task_data or not task_data["downloaded_h"]:
                    task_data["downloaded_h"] = self._format_size(task_data.get("downloaded", 0))
                # Add per-file download progress for multi-file torrents
                video_files = task_data.get("video_files", [])
                if video_files:
                    save_path = task_data.get("save_path", "")
                    updated_vf = []
                    for vf in video_files:
                        vf = dict(vf)
                        vf_path = os.path.join(save_path, vf.get("name", ""))
                        if os.path.isfile(vf_path):
                            st = os.stat(vf_path)
                            # Use st_blocks*512 for actual disk usage (handles sparse files)
                            disk_size = st.st_blocks * 512
                            # Cap at file's declared size
                            declared_size = vf.get("size", 0)
                            if declared_size > 0 and disk_size > declared_size:
                                disk_size = declared_size
                            vf["downloaded"] = disk_size
                            vf["downloaded_h"] = self._format_size(disk_size)
                        else:
                            vf["downloaded"] = 0
                            vf["downloaded_h"] = "0.0B"
                        updated_vf.append(vf)
                    task_data["video_files"] = updated_vf
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
                file_indices = msg.get("file_indices", msg.get("file_index", 0))
                if isinstance(file_indices, list):
                    file_indices = [int(x) for x in file_indices]
                else:
                    file_indices = int(file_indices)
                _send_msg(conn, sdk.select_file(int(task_id), file_indices))
            elif cmd == "pause":
                _send_msg(conn, sdk.pause_task(int(msg.get("task_id", 0))))
            elif cmd == "resume":
                _send_msg(conn, sdk.resume_task(int(msg.get("task_id", 0))))
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
