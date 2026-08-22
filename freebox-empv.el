;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.
;; Magnet links are handled by the vbox backend (Xunlei SDK daemon +
;; xlairplay HTTP proxy) via the /api/magnet/* HTTP API.
;; During magnet download, progress is shown via message updates.

;;; Code:

(require 'empv nil t)
(require 'json)
(require 'cl-lib)
(require 'freebox-http)

;; ── Polling state ──

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

(defvar freebox-empv--last-save-dir nil
  "Last directory used for saving magnet downloads.
Used as default for subsequent save operations.")

;; ── vbox magnet HTTP API ──

(defun freebox-empv--magnet-request (cmd params callback)
  "Send CMD to vbox /api/magnet/* with PARAMS (alist).
CALLBACK is called with (ERROR DATA).  DATA is the daemon JSON alist,
same shape as the old socket protocol: status/error/task_id/video_name/url/..."
  (freebox-http--request (format "magnet/%s" cmd) params callback))

(defun freebox-empv-ensure-vbox ()
  "Ensure the vbox backend daemon is running, starting it if needed.
Magnet tasks run inside the vbox-managed xunlei daemon."
  (unless (freebox-http--server-running-p)
    (freebox-http-start-server)))

;; ── Progress polling ──

(defun freebox-empv--xunlei-poll-progress ()
  "Poll vbox for current task progress and display in message area."
  (when freebox-empv--xunlei-poll-task-id
    (freebox-empv--magnet-request
     "progress" `((task_id . ,freebox-empv--xunlei-poll-task-id))
     (lambda (err result)
       (if err
           (progn
             (freebox-empv--xunlei-cancel-poll)
             (message "FreeBox: lost connection to vbox — %s" err))
         (let* ((phase (alist-get 'status result))
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
            ;; Download only (no video file selected)
            ((string= phase "download_only")
             (freebox-empv--xunlei-cancel-poll)
             (message "FreeBox: 已开始下载 %s — S 保存" (or video-name "文件")))
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
             (message "FreeBox: magnet status %s..." (or phase "unknown"))))))))))

(defun freebox-empv--xunlei-select-file (progress-result)
  "Present file selection from PROGRESS-RESULT via `completing-read-multiple'.
Uses Vertico UI if available.  Select one or more files — use TAB to complete
each item, RET to confirm selection, comma to separate multiple items."
  (let* ((video-files (append (alist-get 'video_files progress-result) nil))
         (task-id (alist-get 'task_id progress-result))
         ;; Build candidates: (display . index)
         (candidates
          (mapcar (lambda (vf)
                    (let* ((name (alist-get 'name vf))
                           (size-h (alist-get 'size_h vf))
                           (ftype (alist-get 'type vf))
                           (idx (alist-get 'index vf))
                           (tag (cond ((string= ftype "video") "视频")
                                      ((string= ftype "subtitle") "字幕")
                                      ((string= ftype "image") "图片")
                                      (t "其他")))
                           (display (format "%s %s (%s)" tag name size-h)))
                      (cons display idx)))
                  video-files))
         (candidate-names (mapcar #'car candidates)))
    (let ((chosen (completing-read-multiple
                   "FreeBox 选择文件 (, 分隔多选): "
                   candidate-names nil t)))
      (if (not chosen)
          (message "FreeBox: 没有选中任何文件")
        ;; Map chosen display strings back to torrent file indices
        (let ((selected-indices
               (delq nil
                     (mapcar (lambda (c)
                               (cdr (assoc c candidates #'string=)))
                             chosen))))
          (if (not selected-indices)
              (message "FreeBox: 没有选中任何文件")
            (freebox-empv--magnet-request
             "select"
             `((task_id . ,task-id)
               (indices . ,(mapconcat #'number-to-string selected-indices ",")))
             (lambda (err sel-result)
               (if err
                   (message "FreeBox: 文件选择失败 — %s" err)
                 (if (let ((e (alist-get 'error sel-result)))
                       (and e (not (string-empty-p e))))
                     (message "FreeBox: 文件选择失败 — %s" (alist-get 'error sel-result))
                   (freebox-empv--xunlei-start-poll
                    (number-to-string task-id)
                    (or freebox-empv--xunlei-poll-title
                        (alist-get 'video_name sel-result)))))))))))))

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

;; ── mpv exit auto-pause ──

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
                       (freebox-empv--magnet-request
                        "pause" `((task_id . ,saved-task-id))
                        (lambda (err result)
                          (if (or err
                                  (let ((e (alist-get 'error result)))
                                    (and e (not (string-empty-p e)))))
                              (message "FreeBox: auto-pause failed — %s"
                                       (or err (alist-get 'error result)))
                            (message "FreeBox: magnet download paused (mpv exited)"))))))
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
  "Play URL in mpv via empv, appending to the playlist and switching to it.
URL is the media URL to play.  TITLE is optional media title.
Magnet links are played via the vbox backend (Xunlei SDK + xlairplay HTTP proxy)."
  (cond
   ((string-prefix-p "magnet:" url)
    (freebox-empv-play-magnet url title))
   ((not (fboundp 'empv-play))
    (error "empv package not found or empv-play is not bound"))
   (t
    (freebox-empv--play-mpv url title))))

(defun freebox-empv-play-magnet (url &optional title)
  "Play magnet URL via vbox Xunlei SDK daemon + xlairplay HTTP proxy.
Sends play to vbox which returns immediately with a task ID.
A timer then polls progress every 3 seconds and shows status updates.
When enough data is buffered, mpv starts streaming automatically.
URL is the magnet link.  TITLE is optional display name."
  (message "FreeBox: requesting magnet playback via vbox%s..."
           (if title (format " — %s" title) ""))
  (freebox-empv-ensure-vbox)
  (freebox-empv--magnet-request
   "play" `((url . ,url) (max_wait . 180))
   (lambda (err result)
     (if err
         (message "FreeBox: %s" err)
       (let* ((status (alist-get 'status result))
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
                    (or error-msg "unknown error")))))))))

(defun freebox-empv--play-mpv (url &optional title)
  "Play URL in mpv via empv, with optional TITLE.
Insert the new item right after the currently playing one and switch
to it, preserving the rest of the playlist.

Example: playlist is [A, B, C] with B currently playing.  Playing D
yields [A, B, D, C] and switches to D.

Previous attempts and why they failed:
- `empv-play' (loadfile append + get_property playlist-count +
  playlist-play-index (1- count)): races — playlist-count can be
  reported before the append lands, so (1- count) points at the OLD
  last item (C), not the new one (D).
- `append-play' flag: mpv 0.37 only starts the new item when NOTHING
  is currently playing; silently no-ops while a video is running.
- `playlist-clear' + append + playlist-next: clobbers the existing
  playlist, losing items the user wants to keep.

This implementation mirrors `empv-enqueue-next' but also switches
playback: read playlist-pos (idx) and playlist-count (len) BEFORE
appending, append the new item (it lands at index `len'), move it to
`idx+1' via playlist-move, then playlist-play-index `idx+1'.  All
property reads happen up front via `empv--let-properties', so there is
no race between append and the count query."
  (cond
   ((not (fboundp 'empv-play))
    (error "empv package not found"))
   ((empv--running?)
    (empv--let-properties '(playlist-pos playlist-count)
      (let* ((idx .playlist-pos)
             (len .playlist-count)
             (target (if (numberp idx) (1+ idx) 0))
             ;; New item appends at the end, i.e. index `len'.
             (new-idx (if (numberp len) len 0)))
        (empv--cmd 'loadfile (list url 'append)
          (empv--cmd 'playlist-move (list new-idx target)
            (empv--cmd 'playlist-play-index target
              (empv--cmd 'set_property '(pause :json-false)
                (when title
                  (empv--send-command `(set_property media-title ,title) nil)))))))))
   (t
    (empv-start url)
    (when title
      (empv--send-command `(set_property media-title ,title) nil)))))

;; ── Save magnet file ──

(defun freebox-empv--format-size (bytes)
  "Format BYTES as human-readable string."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes (* 1024.0 1024.0))))
   (t (format "%.1fGB" (/ bytes (* 1024.0 1024.0 1024.0))))))

(defun freebox-empv-save-magnet-file ()
  "List all downloaded magnet files and save selected one to another directory.
Shows files from both vbox daemon tasks and disk download directory.
Files still downloading are marked with their progress."
  (interactive)
  (freebox-empv--magnet-request
   "files" nil
   (lambda (err all-files)
     (if err
         (message "FreeBox: %s" err)
       (let ((all-files (append all-files nil)))
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
                       (freebox-empv--magnet-request
                        "resume" `((task_id . ,task-id))
                        (lambda (err result)
                          (if err
                              (message "FreeBox: 恢复下载失败 — %s" err)
                            (let ((e (alist-get 'error result)))
                              (if (and e (not (string-empty-p e)))
                                  (message "FreeBox: 恢复下载失败 — %s" e)
                                ;; resume_task recreates the task with a new task_id
                                ;; via play_magnet — must use the new task_id from result
                                (let* ((new-task-id
                                        (let ((tid (alist-get 'task_id result)))
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
                                    (freebox-empv--xunlei-start-poll new-task-id name)))))))
                     (message "FreeBox: 无法恢复 — 无 task_id")))
                  ;; Available but not yet downloaded — trigger select_file
                  ((string= phase "available")
                   (if (and task-id file-index)
                       (freebox-empv--magnet-request
                        "select" `((task_id . ,task-id) (indices . ,file-index))
                        (lambda (err result)
                          (if (or err
                                  (let ((e (alist-get 'error result)))
                                    (and e (not (string-empty-p e)))))
                              (message "FreeBox: 无法切换到 %s" name)
                            (message "FreeBox: 开始下载 %s" name))))
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
                                     (format "保存 %s 到: " name)
                                     (or freebox-empv--last-save-dir "~/")))
                          (dest-path (expand-file-name
                                      (file-name-nondirectory local-file) dest-dir)))
                     (setq freebox-empv--last-save-dir dest-dir)
                     (freebox-empv--magnet-request
                      "save" `((name . ,name) (dest . ,dest-dir))
                      (lambda (err result)
                        (if err
                            (message "FreeBox: 保存失败 — %s" err)
                          (message "FreeBox: 已保存到 %s"
                                   (or (alist-get 'path result) dest-path))))))))))))))))))

(provide 'freebox-empv)
;;; freebox-empv.el ends here
