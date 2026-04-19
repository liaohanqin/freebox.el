;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.
;; Magnet links are played via Xunlei SDK daemon (xunlei_magnet.py)
;; which provides an xlairplay HTTP proxy URL for mpv streaming.
;; During magnet download, progress is shown via message updates.

;;; Code:

(require 'empv nil t)
(require 'json)
(require 'cl-lib)

;; ── Xunlei SDK daemon configuration ──

(defcustom freebox-empv-xunlei-script
  (expand-file-name "xunlei_magnet.py"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "Path to the xunlei_magnet.py daemon script."
  :type 'string
  :group 'freebox)

(defcustom freebox-empv-xunlei-ld-path
  "/opt/apps/com.xunlei.download/files"
  "Directory containing libxl_thunder_sdk.so and libxl_stat.so.
Added to LD_LIBRARY_PATH when starting the daemon."
  :type 'string
  :group 'freebox)

(defvar freebox-empv--xunlei-sock "/tmp/xunlei_magnet.sock"
  "Unix socket path for communicating with xunlei_magnet.py daemon.")

(defvar freebox-empv--xunlei-poll-timer nil
  "Timer for polling magnet download progress.")

(defvar freebox-empv--xunlei-poll-task-id nil
  "Current task ID being polled for progress.")

(defvar freebox-empv--xunlei-poll-title nil
  "Title of the current magnet being polled.")

(defvar freebox-empv--xunlei-active-task-id nil
  "Task ID of the magnet currently being played via mpv.
Used to auto-pause the download when mpv exits.
Nil when no magnet playback is active.")

(defcustom freebox-empv-download-dir "/tmp/xunlei_magnet"
  "Directory where magnet downloads are stored."
  :type 'string
  :group 'freebox)

(defcustom freebox-empv-download-max-age 7
  "Maximum age in days for magnet download directories.
Directories older than this are automatically cleaned up when the daemon starts."
  :type 'integer
  :group 'freebox)

;; ── Python socket helper (used by all daemon communication) ──

(defun freebox-empv--xunlei-python-script ()
  "Return the inline Python script for Unix socket communication.
The script takes socket path as sys.argv[1], JSON command as sys.argv[2].
It prints the daemon's JSON response to stdout."
  (concat
   "import socket,struct,json,sys\n"
   "sock=socket.socket(socket.AF_UNIX,socket.SOCK_STREAM)\n"
   "sock.connect(sys.argv[1])\n"
   "msg=json.dumps(json.loads(sys.argv[2])).encode()\n"
   "sock.sendall(struct.pack('!I',len(msg))+msg)\n"
   "raw=sock.recv(4)\n"
   "rl=struct.unpack('!I',raw)[0]\n"
   "data=b''\n"
   "while len(data)<rl:\n"
   "  chunk=sock.recv(rl-len(data))\n"
   "  if not chunk: break\n"
   "  data+=chunk\n"
   "sock.close()\n"
   "print(data.decode())"))

;; ── Xunlei daemon management ──

(defun freebox-empv-xunlei-running-p ()
  "Return non-nil if the xunlei daemon is running."
  (and (file-exists-p freebox-empv--xunlei-sock)
       (condition-case nil
           (freebox-empv--xunlei-send-raw '((cmd . "status")))
         (error nil))))

(defun freebox-empv-xunlei-start ()
  "Start the xunlei_magnet.py daemon if not already running.
Returns t on success, nil on failure."
  (if (freebox-empv-xunlei-running-p)
      t
    (message "FreeBox: starting Xunlei SDK daemon...")
    (let* ((default-directory "/tmp")
           (process-environment
            (cons (format "LD_LIBRARY_PATH=%s" freebox-empv-xunlei-ld-path)
                  process-environment))
           (proc (make-process
                  :name "xunlei-magnet-daemon"
                  :buffer nil
                  :command (list "python3"
                                 freebox-empv-xunlei-script
                                 "daemon")
                  :sentinel
                  (lambda (_proc event)
                    (when (string-match-p "exited\\|failed" event)
                      (message "FreeBox: Xunlei daemon exited"))))))
      (when proc
        (sleep-for 1)
        (when (freebox-empv-xunlei-running-p)
          (freebox-empv-cleanup-downloads)
          t)))))

(defun freebox-empv-xunlei-stop ()
  "Stop the xunlei_magnet.py daemon."
  (when (file-exists-p freebox-empv--xunlei-sock)
    (condition-case nil
        (freebox-empv--xunlei-send-raw '((cmd . "shutdown")))
      (error nil))))

(defun freebox-empv--xunlei-send-raw (data)
  "Send DATA (alist) to the xunlei daemon synchronously, return parsed response.
Uses `call-process' with an inline Python script for reliable Unix socket I/O."
  (let* ((json-str (json-encode data))
         (output (with-output-to-string
                   (call-process "python3" nil standard-output nil
                                 "-c" (freebox-empv--xunlei-python-script)
                                 freebox-empv--xunlei-sock json-str))))
    (when (and output (not (string-empty-p (string-trim output))))
      (json-read-from-string (string-trim output)))))

(defun freebox-empv-xunlei-stop-task (task-id)
  "Stop a download task by TASK-ID."
  (freebox-empv--xunlei-send-raw
   `(("cmd" . "stop")
     ("task_id" . ,task-id))))

;; ── Progress polling ──

(defun freebox-empv--xunlei-poll-progress ()
  "Poll daemon for current task progress and display in message area."
  (when (and freebox-empv--xunlei-poll-task-id
             (file-exists-p freebox-empv--xunlei-sock))
    (condition-case nil
        (let* ((result (freebox-empv--xunlei-send-raw
                        `(("cmd" . "progress")
                          ("task_id" . ,freebox-empv--xunlei-poll-task-id))))
               (phase (alist-get 'status result))
               (downloaded (alist-get 'downloaded_h result))
               (url (alist-get 'url result))
               (video-name (alist-get 'video_name result))
               (error-msg (alist-get 'error result)))
          (cond
           ;; Ready to play
           ((and (string= phase "ready") url (not (string-empty-p url)))
            (let ((current-task-id freebox-empv--xunlei-poll-task-id))
              (freebox-empv--xunlei-cancel-poll)
              ;; Register auto-pause hook for when mpv exits
              (when current-task-id
                (freebox-empv--xunlei-register-exit-hook current-task-id)))
            (let ((size-display
                   (if (or (not downloaded)
                           (string= downloaded "0.0B")
                           (string= downloaded "0B"))
                       "streaming" downloaded)))
              (message "FreeBox: streaming %s via xlairplay (%s) — S to save"
                       (or video-name "video") size-display))
            (freebox-empv--play-mpv url (or freebox-empv--xunlei-poll-title video-name)))
           ;; Multiple video files — user must select
           ((string= phase "needs_selection")
            (freebox-empv--xunlei-cancel-poll)
            (freebox-empv--xunlei-select-file result))
           ;; Fetching metadata
           ((string= phase "fetching_metadata")
            (message "FreeBox: fetching torrent metadata for %s..."
                     (or video-name "video")))
           ;; Creating BT task
           ((string= phase "creating_bt_task")
            (message "FreeBox: creating download task for %s..."
                     (or video-name "video")))
           ;; Downloading
           ((string= phase "downloading")
            (let ((size-display
                   (if (or (not downloaded)
                           (string= downloaded "0.0B")
                           (string= downloaded "0B"))
                       "" (format " %s" downloaded))))
              (message "FreeBox: downloading %s...%s"
                       (or video-name "video") size-display)))
           ;; Error
           ((or error-msg (string= phase "error"))
            (freebox-empv--xunlei-cancel-poll)
            (message "FreeBox: %s"
                     (cond
                      ((string= error-msg "download_stalled")
                       "下载无速度，已自动取消")
                      ((string= error-msg "torrent_metadata_timeout")
                       "获取种子元数据超时，请检查网络")
                      ((string= error-msg "video_download_timeout")
                       "视频下载超时")
                      (t (format "magnet playback failed — %s"
                                 (or error-msg "unknown error"))))))
           ;; Unknown state — keep polling
           (t
            (message "FreeBox: magnet status %s..." (or phase "unknown")))))
      (error
       (freebox-empv--xunlei-cancel-poll)
       (message "FreeBox: lost connection to Xunlei daemon")))))

(defun freebox-empv--xunlei-select-file (progress-result)
  "Present video file selection from PROGRESS-RESULT via completing-read.
Sends the selected file index to the daemon and resumes progress polling."
  (let* ((video-files (append (alist-get 'video_files progress-result) nil))
         (task-id (alist-get 'task_id progress-result))
         (candidates
          (mapcar (lambda (vf)
                    (let* ((name (alist-get 'name vf))
                           (size-h (alist-get 'size_h vf))
                           (idx (alist-get 'index vf))
                           (display (format "%s  (%s)" name size-h)))
                      (cons display idx)))
                  video-files))
         (choice (completing-read
                  "Select video file: "
                  (mapcar #'car candidates)
                  nil t)))
    (when-let* ((selected (cl-find choice candidates :key #'car :test #'string=)))
      (let* ((file-index (cdr selected))
             (sel-result (freebox-empv--xunlei-send-raw
                          `(("cmd" . "select")
                            ("task_id" . ,task-id)
                            ("file_index" . ,file-index)))))
        (if (let ((e (alist-get 'error sel-result))) (and e (not (string-empty-p e))))
            (message "FreeBox: file selection failed — %s" (alist-get 'error sel-result))
          (freebox-empv--xunlei-start-poll
           (number-to-string task-id)
           (or freebox-empv--xunlei-poll-title
               (alist-get 'video_name sel-result))))))))

(defun freebox-empv--xunlei-start-poll (task-id title)
  "Start polling for TASK-ID progress every 3 seconds.
TITLE is the video title for display."
  (freebox-empv--xunlei-cancel-poll)
  (setq freebox-empv--xunlei-poll-task-id task-id
        freebox-empv--xunlei-poll-title title)
  (setq freebox-empv--xunlei-poll-timer
        (run-at-time 3 3 #'freebox-empv--xunlei-poll-progress))
  ;; First poll immediately
  (freebox-empv--xunlei-poll-progress))

(defun freebox-empv--xunlei-cancel-poll ()
  "Cancel progress polling timer."
  (when (timerp freebox-empv--xunlei-poll-timer)
    (cancel-timer freebox-empv--xunlei-poll-timer))
  (setq freebox-empv--xunlei-poll-timer nil
        freebox-empv--xunlei-poll-task-id nil
        freebox-empv--xunlei-poll-title nil))

(defun freebox-empv--xunlei-on-player-stopped (state)
  "Callback for `empv-player-state-changed-hook'.
When mpv stops and a magnet task is active, pause the download.
Uses a short delay to distinguish between track changes (transient stopped)
and actual mpv exit (persistent stopped)."
  (when (and (eq state 'stopped)
             freebox-empv--xunlei-active-task-id)
    (let ((task-id freebox-empv--xunlei-active-task-id))
      ;; Delay check — if mpv restarts within 1s, this is a track change, not exit
      (run-at-time 1.0 nil
                   (lambda (saved-task-id)
                     (when (and freebox-empv--xunlei-active-task-id
                                (string= freebox-empv--xunlei-active-task-id saved-task-id)
                                (not (and (fboundp 'empv--running?) (empv--running?))))
                       ;; mpv is truly not running — pause the download
                       (setq freebox-empv--xunlei-active-task-id nil)
                       (remove-hook 'empv-player-state-changed-hook
                                    #'freebox-empv--xunlei-on-player-stopped)
                       (condition-case nil
                           (let* ((result (freebox-empv--xunlei-send-raw
                                          `(("cmd" . "pause")
                                            ("task_id" . ,saved-task-id))))
                                  (err (alist-get 'error result)))
                             (if (and err (not (string-empty-p err)))
                                 (message "FreeBox: auto-pause failed — %s" err)
                               (message "FreeBox: magnet download paused (mpv exited)")))
                         (error
                          (message "FreeBox: auto-pause failed — daemon not reachable")))))
                   task-id))))

(defun freebox-empv--xunlei-register-exit-hook (task-id)
  "Register hook to auto-pause TASK-ID when mpv exits."
  (setq freebox-empv--xunlei-active-task-id task-id)
  (add-hook 'empv-player-state-changed-hook
            #'freebox-empv--xunlei-on-player-stopped)
  (message "FreeBox: registered mpv exit hook for task %s" task-id))

(defun freebox-empv--xunlei-unregister-exit-hook ()
  "Remove the mpv exit hook if still registered."
  (setq freebox-empv--xunlei-active-task-id nil)
  (remove-hook 'empv-player-state-changed-hook
               #'freebox-empv--xunlei-on-player-stopped))

;; ── Playback ──

(defun freebox-empv-play-url (url &optional title)
  "Play URL in mpv via empv, replacing current item and keeping old in playlist.
URL is the media URL to play.  TITLE is optional media title.
Magnet links are played via Xunlei SDK daemon + xlairplay HTTP proxy."
  (cond
   ((string-prefix-p "magnet:" url)
    (freebox-empv-play-magnet url title))
   ((not (fboundp 'empv-play))
    (error "empv package not found or empv-play is not bound"))
   ((empv--running?)
    (empv--send-command
     '(get_property path)
     (lambda (cur-path)
       (let ((old-path (when (and cur-path (stringp cur-path)
                                  (not (string-empty-p cur-path)))
                         cur-path)))
         (empv--send-command `(loadfile ,url replace) nil)
         (when title
           (empv--send-command `(set_property media-title ,title) nil))
         (when old-path
           (run-at-time 0.5 nil
                        (lambda (p)
                          (empv--send-command `(loadfile ,p append) nil))
                        old-path))))))
   (t
    (empv-start url))))

(defun freebox-empv-play-magnet (url &optional title)
  "Play magnet URL via Xunlei SDK daemon + xlairplay HTTP proxy.
Sends the play command to the daemon which returns immediately with a task ID.
A timer then polls progress every 3 seconds and shows status updates.
When enough data is buffered, mpv starts streaming automatically.
URL is the magnet link.  TITLE is optional display name."
  (message "FreeBox: requesting magnet playback via Xunlei SDK%s..."
           (if title (format " — %s" title) ""))
  (freebox-empv-xunlei-start)
  ;; Send play command synchronously — daemon returns immediately with task_id
  (condition-case err
      (let* ((result (freebox-empv--xunlei-send-raw
                      `(("cmd" . "play")
                        ("url" . ,url)
                        ("max_wait" . 180))))
             (status (alist-get 'status result))
             (task-id (alist-get 'task_id result))
             (error-msg (alist-get 'error result))
             (video-name (alist-get 'video_name result)))
        (cond
         ;; Already ready (cached)
         ((and (string= status "ready")
               (alist-get 'url result)
               (not (string-empty-p (alist-get 'url result))))
          (when task-id
            (freebox-empv--xunlei-register-exit-hook
             (number-to-string task-id)))
          (freebox-empv--play-mpv (alist-get 'url result)
                                  (or title video-name)))
         ;; Got a task_id — start progress polling (covers fetching/creating/downloading)
         ((and task-id
               (member status '("fetching_metadata" "creating_bt_task"
                                "downloading" "ready")))
          (message "FreeBox: %s %s..."
                   (pcase status
                     ("fetching_metadata" "fetching torrent metadata")
                     ("creating_bt_task" "creating download task")
                     ("downloading" "downloading")
                     ("ready" "ready to play")
                     (_ status))
                   (or video-name ""))
          (freebox-empv--xunlei-start-poll
           (number-to-string task-id) (or title video-name)))
         ;; Multiple video files — user must select (can happen on dedup hit)
         ((string= status "needs_selection")
          (setq freebox-empv--xunlei-poll-title (or title video-name))
          (freebox-empv--xunlei-select-file result))
         ;; Error from daemon
         (t
          (message "FreeBox: magnet playback failed — %s"
                   (or error-msg "unknown error")))))
    (error
     (message "FreeBox: failed to send play command — %s" err))))

(defun freebox-empv--play-mpv (url &optional title)
  "Play URL in mpv via empv, with optional TITLE.
Handles both running and not-running mpv cases, with playlist preservation."
  (cond
   ((not (fboundp 'empv-play))
    (error "empv package not found"))
   ((empv--running?)
    (empv--send-command
     '(get_property path)
     (lambda (cur-path)
       (let ((old-path (when (and cur-path (stringp cur-path)
                                  (not (string-empty-p cur-path)))
                         cur-path)))
         (empv--send-command `(loadfile ,url replace) nil)
         (when title
           (empv--send-command `(set_property media-title ,title) nil))
         (when old-path
           (run-at-time 0.5 nil
                        (lambda (p)
                          (empv--send-command `(loadfile ,p append) nil))
                        old-path))))))
   (t
    (empv-start url))))

;; ── Auto cleanup old downloads ──

(defun freebox-empv-cleanup-downloads ()
  "Remove magnet download directories older than `freebox-empv-download-max-age' days.
Skips directories that are actively used by current daemon tasks."
  (when (file-directory-p freebox-empv-download-dir)
    (let* ((active-paths (freebox-empv--active-save-paths))
           (now (float-time))
           (max-age-secs (* freebox-empv-download-max-age 86400))
           (deleted 0))
      (dolist (entry (directory-files freebox-empv-download-dir t))
        (let ((name (file-name-nondirectory entry)))
          (unless (member name '("." ".."))
            (when (and (file-directory-p entry)
                       (not (member (file-name-as-directory entry) active-paths))
                       (> (- now (float-time (nth 5 (file-attributes entry))))
                          max-age-secs))
              (delete-directory entry t)
              (setq deleted (1+ deleted))))))
      (when (> deleted 0)
        (message "FreeBox: cleaned up %d old download directory(ies)" deleted)))))

(defun freebox-empv--active-save-paths ()
  "Return list of save_path values from active daemon tasks."
  (condition-case nil
      (let* ((status (freebox-empv--xunlei-send-raw '((cmd . "status"))))
             (tasks (alist-get 'tasks status)))
        (delq nil
              (mapcar (lambda (pair)
                        (file-name-as-directory
                         (alist-get 'save_path (cdr pair))))
                      (if (listp tasks) tasks (append tasks nil)))))
    (error nil)))

;; ── Save magnet file ──

(defun freebox-empv--scan-downloads ()
  "Scan download directories and daemon tasks for downloadable files.
Return a list of alists: ((name . \"filename\") (path . \"/full/path\") (size . 1234) (size_h . \"1.2GB\") (source . \"daemon\"|\"disk\"))."
  (let (results)
    ;; 1. Active daemon tasks
    (condition-case nil
        (let* ((status (freebox-empv--xunlei-send-raw '((cmd . "status"))))
               (tasks (alist-get 'tasks status))
               (task-list (if (listp tasks) tasks (append tasks nil)))
               daemon-items)
          (dolist (pair task-list)
            (let* ((info (cdr pair))
                   (phase (alist-get 'phase info))
                   (magnet-url (alist-get 'magnet_url info))
                   (save-path (alist-get 'save_path info))
                   (current-index (alist-get 'video_index info))
                   (downloaded (alist-get 'downloaded info))
                   (total-size (alist-get 'total_size info))
                   (downloaded-h (or (alist-get 'downloaded_h info) ""))
                   (complete-p (and total-size
                                    (> total-size 0)
                                    (>= (or downloaded 0) total-size)))
                   (video-files (append (alist-get 'video_files info) nil)))
              (if video-files
                  ;; Multi-file torrent: show each video file as a separate entry
                  (dolist (vf video-files)
                    (let* ((vf-name (alist-get 'name vf))
                           (vf-index (alist-get 'index vf))
                           (vf-size (alist-get 'size vf))
                           (is-current (eq vf-index current-index))
                           (vf-local (if is-current
                                         (alist-get 'local_file info)
                                       (concat (file-name-as-directory save-path) vf-name)))
                           ;; Use daemon's per-file downloaded (st_blocks-based) when available
                           (vf-downloaded (if is-current
                                              (or downloaded 0)
                                            (or (alist-get 'downloaded vf) 0)))
                           (vf-downloaded-h (if is-current
                                                downloaded-h
                                              (or (alist-get 'downloaded_h vf) "0.0B")))
                           (vf-complete (if is-current complete-p
                                          (and vf-size (> vf-size 0)
                                               (> vf-downloaded 0)
                                               (>= vf-downloaded vf-size)))))
                      (push `((name . ,vf-name)
                              (path . ,vf-local)
                              (size . ,vf-downloaded)
                              (size_h . ,vf-downloaded-h)
                              (total_size . ,vf-size)
                              (complete . ,vf-complete)
                              (phase . ,(if is-current phase "available"))
                              (magnet . ,(or magnet-url ""))
                              (source . "daemon")
                              (task_id . ,(alist-get 'task_id info))
                              (file_index . ,vf-index))
                            daemon-items)))
                ;; Single-file or no video_files list
                (let ((local-file (alist-get 'local_file info))
                      (video-name (or (alist-get 'video_name info) "")))
                  (when local-file
                    (push `((name . ,(if (string-empty-p video-name)
                                         (file-name-nondirectory local-file)
                                       video-name))
                            (path . ,local-file)
                            (size . ,(or downloaded 0))
                            (size_h . ,downloaded-h)
                            (total_size . ,total-size)
                            (complete . ,complete-p)
                            (phase . ,phase)
                            (magnet . ,(or magnet-url ""))
                            (source . "daemon")
                            (task_id . ,(alist-get 'task_id info)))
                          daemon-items))))))
          (setq results (nconc daemon-items results)))
      (error nil))
    ;; 2. Disk-only downloads (not in daemon tasks)
    (when (file-directory-p freebox-empv-download-dir)
      (let ((daemon-files (mapcar (lambda (r) (alist-get 'path r)) results)))
        (dolist (dir (directory-files freebox-empv-download-dir t))
          (let ((dirname (file-name-nondirectory dir)))
            (unless (member dirname '("." ".."))
              (when (file-directory-p dir)
                (dolist (f (directory-files dir t))
                  (let ((fname (file-name-nondirectory f)))
                    (unless (member fname '("." ".." "download"))
                      ;; Skip SDK metadata files
                      (unless (string-match-p "\\.js$" fname)
                        (unless (member f daemon-files)
                          (when (and (file-regular-p f)
                                     ;; Only show files with actual data on disk
                                     ;; (skip 0-byte sparse files from active downloads)
                                     (> (file-attribute-size (file-attributes f)) 0))
                            (let* ((size (file-attribute-size (file-attributes f)))
                                   (size-h (freebox-empv--format-size size)))
                              (push `((name . ,fname)
                                      (path . ,f)
                                      (size . ,size)
                                      (size_h . ,size-h)
                                      (total_size . ,size)
                                      (complete . t)
                                      (phase . "disk")
                                      (magnet . "")
                                      (source . "disk"))
                                    results))))))))))))))
    results))

(defun freebox-empv--format-size (bytes)
  "Format BYTES as human-readable string."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes (* 1024.0 1024.0))))
   (t (format "%.1fGB" (/ bytes (* 1024.0 1024.0 1024.0))))))

(defun freebox-empv-save-magnet-file ()
  "List all downloaded magnet files and save selected one to another directory.
Shows files from both active daemon tasks and disk download directory.
Files still downloading are marked with their progress."
  (interactive)
  (let* ((all-files (freebox-empv--scan-downloads)))
    (if (not all-files)
        (message "FreeBox: 没有找到已下载的资源")
      (let* ((candidates
              (mapcar (lambda (f)
                        (let* ((name (alist-get 'name f))
                               (size-h (alist-get 'size_h f))
                               (complete (alist-get 'complete f))
                               (total-size (alist-get 'total_size f))
                               (phase (alist-get 'phase f))
                               (tag (cond
                                     (complete " [已完成]")
                                     ((string= phase "paused") " [已暂停]")
                                     ((member phase '("downloading"
                                                      "fetching_metadata"
                                                      "creating_bt_task"
                                                      "ready"))
                                      " [下载中]")
                                     ((string= phase "available")
                                      " [未下载]")
                                     ((string= phase "disk") "")
                                     (t (format " [%s]" phase)))))
                          (cons (format "%s  (%s/%s)%s"
                                         name size-h
                                         (if total-size
                                             (freebox-empv--format-size total-size)
                                           "?")
                                         tag)
                                f)))
                      all-files))
             (choice (completing-read
                      "FreeBox 下载资源: "
                      (mapcar #'car candidates) nil t)))
        (when-let* ((selected (cl-find choice candidates
                                         :key #'car :test #'string=)))
          (let* ((info (cdr selected))
                 (phase (alist-get 'phase info))
                 (local-file (alist-get 'path info))
                 (name (alist-get 'name info))
                 (task-id (alist-get 'task_id info))
                 (file-index (alist-get 'file_index info)))
            (cond
             ;; Paused — resume download and play
             ((string= phase "paused")
              (if task-id
                  (let* ((result (freebox-empv--xunlei-send-raw
                                  `(("cmd" . "resume")
                                    ("task_id" . ,task-id))))
                         (err (let ((e (alist-get 'error result)))
                                (and e (not (string-empty-p e))))))
                    (if err
                        (message "FreeBox: 恢复下载失败 — %s" (alist-get 'error result))
                      ;; resume_task recreates the task with a new task_id
                      ;; via play_magnet — must use the new task_id from result
                      (let* ((new-task-id (let ((tid (alist-get 'task_id result)))
                                            (if tid (number-to-string tid) task-id)))
                             (url (alist-get 'url result))
                             (new-phase (alist-get 'status result)))
                        (message "FreeBox: 已恢复下载 %s" name)
                        (when (and (string= new-phase "ready")
                                   url (not (string-empty-p url)))
                          (freebox-empv--xunlei-register-exit-hook new-task-id)
                          (freebox-empv--play-mpv url name))
                        (when (member new-phase '("downloading" "creating_bt_task"
                                                   "fetching_metadata"))
                          (freebox-empv--xunlei-start-poll new-task-id name)))))
                (message "FreeBox: 无法恢复 — 无 task_id")))
             ;; Available but not yet downloaded — trigger select_file
             ((string= phase "available")
              (if (and task-id file-index
                       (freebox-empv--xunlei-send-raw
                        `(("cmd" . "select")
                          ("task_id" . ,task-id)
                          ("file_index" . ,file-index))))
                  (message "FreeBox: 开始下载 %s" name)
                (message "FreeBox: 无法切换到 %s" name)))
             ((member phase '("fetching_metadata" "creating_bt_task"))
              (message "FreeBox: 资源尚未开始下载 — %s" name))
             ((not (alist-get 'complete info))
              (message "FreeBox: 资源尚未下载完成 (%s/%s) — %s"
                       (alist-get 'size_h info)
                       (let ((ts (alist-get 'total_size info)))
                         (if ts (freebox-empv--format-size ts) "?"))
                       name))
             ((not (file-exists-p local-file))
              (message "FreeBox: 文件不存在 — %s" local-file))
             (t
              (let* ((dest-dir (read-directory-name
                                (format "保存 %s 到: " name)))
                     (filename (file-name-nondirectory local-file))
                     (dest-path (expand-file-name filename dest-dir)))
                (copy-file local-file dest-path t)
                (message "FreeBox: 已保存到 %s" dest-path))))))))))

(provide 'freebox-empv)
;;; freebox-empv.el ends here
