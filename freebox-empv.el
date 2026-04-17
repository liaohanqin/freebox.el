;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.
;; Magnet links are handled via `webtorrent --mpv'.

;;; Code:

(require 'empv nil t)

(defcustom freebox-empv-webtorrent-executable "webtorrent"
  "Path to webtorrent executable for magnet link playback."
  :type 'string
  :group 'freebox)

(defcustom freebox-empv-tracker-list-url
  "https://cdn.jsdelivr.net/gh/ngosang/trackerslist@master/trackers_best.txt"
  "URL to fetch popular BitTorrent tracker list.
Trackers are appended to magnet links to improve peer discovery."
  :type 'string
  :group 'freebox)

(defvar freebox-empv--trackers nil
  "Cached list of tracker URLs.")

(defvar freebox-empv--trackers-fetched nil
  "Non-nil if trackers have been fetched at least once.")

(defvar freebox-empv--magnet-timer nil
  "Timer for polling webtorrent log file.")

(defun freebox-empv--fetch-trackers ()
  "Fetch tracker list synchronously and cache it.
Uses curl for synchronous download; falls back to async url-retrieve."
  (if (executable-find "curl")
      (with-temp-buffer
        (call-process "curl" nil t nil "-sL" "--connect-timeout" "5"
                      freebox-empv-tracker-list-url)
        (setq freebox-empv--trackers
              (seq-filter (lambda (s) (not (string-empty-p s)))
                          (split-string (buffer-string) "\n")))
        (setq freebox-empv--trackers-fetched t)
        (message "FreeBox: loaded %d trackers" (length freebox-empv--trackers)))
    ;; 无 curl 时异步获取
    (let ((url freebox-empv-tracker-list-url))
      (url-retrieve url
        (lambda (status)
          (if (plist-get status :error)
              (message "FreeBox: failed to fetch tracker list")
            (goto-char url-http-end-of-headers)
            (setq freebox-empv--trackers
                  (seq-filter (lambda (s) (not (string-empty-p s)))
                              (split-string (buffer-string) "\n")))
            (setq freebox-empv--trackers-fetched t)
            (message "FreeBox: loaded %d trackers" (length freebox-empv--trackers))))))))

(defun freebox-empv--enrich-magnet (url)
  "Append popular trackers to magnet URL as &tr= parameters.
Only adds trackers not already present in URL."
  (unless freebox-empv--trackers-fetched
    (freebox-empv--fetch-trackers))
  (if (not freebox-empv--trackers)
      url
    (concat url
            (apply #'concat
                   (seq-map (lambda (tr)
                              (if (string-match-p
                                   (regexp-quote (url-hexify-string tr))
                                   url)
                                  ""
                                (concat "&tr=" (url-hexify-string tr))))
                            freebox-empv--trackers)))))

(defun freebox-empv-play-url (url &optional title)
  "Play URL in mpv via empv, replacing current item and keeping old in playlist.
URL is the media URL to play.  TITLE is optional media title.
Magnet links are played via `webtorrent --mpv'."
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

(defun freebox-empv--update-magnet-buffer (buf log-file final)
  "Update BUF with contents of LOG-FILE.
If FINAL is non-nil, this is the last update (process exited)."
  (when (buffer-live-p buf)
    (when (file-exists-p log-file)
      (let* ((raw (with-temp-buffer
                    (insert-file-contents log-file)
                    (buffer-string)))
             (clean (replace-regexp-in-string
                     "\r" ""
                     (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" raw))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert clean)
            (goto-char (point-max)))
          (when final
            (goto-char (point-max))
            (insert "\nFreeBox: webtorrent exited")))))))

(defun freebox-empv-play-magnet (url &optional title)
  "Play magnet URL via webtorrent --mpv.
Trackers are appended to URL to improve peer discovery.
Uses `script' to provide PTY so webtorrent outputs progress.
URL is the magnet link.  TITLE is optional display name."
  (let* ((executable (or (executable-find freebox-empv-webtorrent-executable)
                         (and (file-executable-p freebox-empv-webtorrent-executable)
                              freebox-empv-webtorrent-executable)))
         (enriched-url (freebox-empv--enrich-magnet url))
         (buf-name "*freebox-webtorrent*")
         (log-file (make-temp-file "freebox-wt-" nil ".log"))
         buf)
    (unless executable
      (error "FreeBox: webtorrent not found (set `freebox-empv-webtorrent-executable')"))
    ;; 取消上一次的 timer
    (when freebox-empv--magnet-timer
      (cancel-timer freebox-empv--magnet-timer)
      (setq freebox-empv--magnet-timer nil))
    ;; 准备 buffer
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (setq buf (get-buffer-create buf-name))
    (with-current-buffer buf
      (insert (format "FreeBox: magnet%s\nWaiting for webtorrent...\n\n"
                      (if title (format " — %s" title) ""))))
    (display-buffer buf '(nil (window-height . 10)))
    ;; 用 shell-command 异步启动 webtorrent，重定向输出到日志文件
    ;; webtorrent 在非 TTY 时输出文本模式（速度、peers 等信息）
    (let* ((shell-cmd (format "%s '%s' --mpv --keep-seeding >> %s 2>&1"
                              executable
                              (replace-regexp-in-string "'" "'\\''" enriched-url)
                              log-file))
           (proc (start-process-shell-command "freebox-webtorrent" nil shell-cmd)))
      (set-process-sentinel
       proc
       (lambda (_proc event)
         (when (memq (process-status _proc) '(exit signal))
           (when freebox-empv--magnet-timer
             (cancel-timer freebox-empv--magnet-timer)
             (setq freebox-empv--magnet-timer nil))
           (freebox-empv--update-magnet-buffer buf log-file t)
           (delete-file log-file t)))))
    ;; 定期读取日志文件更新 buffer
    (setq freebox-empv--magnet-timer
          (run-at-time 1 1 #'freebox-empv--update-magnet-buffer buf log-file nil))))

(provide 'freebox-empv)
;;; freebox-empv.el ends here
