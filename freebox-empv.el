;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.
;; Magnet links are played via Xunlei SDK daemon (xunlei_magnet.py)
;; which provides an xlairplay HTTP proxy URL for mpv streaming.
;; During magnet download, progress is shown via message updates.

;;; Code:

(require 'empv nil t)
(require 'json)

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
        (freebox-empv-xunlei-running-p)))))

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
            (freebox-empv--xunlei-cancel-poll)
            (message "FreeBox: streaming %s via xlairplay (%s)"
                     (or video-name "video") (or downloaded ""))
            (freebox-empv--play-mpv url (or freebox-empv--xunlei-poll-title video-name)))
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
            (message "FreeBox: downloading %s... %s"
                     (or video-name "video") (or downloaded "?")))
           ;; Error
           ((or error-msg (string= phase "error"))
            (freebox-empv--xunlei-cancel-poll)
            (message "FreeBox: magnet playback failed — %s"
                     (or error-msg "unknown error")))
           ;; Unknown state — keep polling
           (t
            (message "FreeBox: magnet status %s..." (or phase "unknown")))))
      (error
       (freebox-empv--xunlei-cancel-poll)
       (message "FreeBox: lost connection to Xunlei daemon")))))

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
         ;; Got a task_id — start progress polling
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
         ;; Already ready (cached)
         ((and (string= status "ready")
               (alist-get 'url result)
               (not (string-empty-p (alist-get 'url result))))
          (freebox-empv--play-mpv (alist-get 'url result)
                                  (or title video-name)))
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

(provide 'freebox-empv)
;;; freebox-empv.el ends here
