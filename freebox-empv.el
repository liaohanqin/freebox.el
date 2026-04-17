;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.

;;; Code:

(require 'empv nil t)

(defun freebox-empv-play-url (url &optional title)
  "Play URL in mpv via empv, replacing current item and keeping old in playlist.
URL is the media URL to play.  TITLE is optional media title.

Strategy: get current path → replace with new URL → delay → append old path."
  (cond
   ((not (fboundp 'empv-play))
    (error "empv package not found or empv-play is not bound"))
   ((empv--running?)
    (empv--send-command
     '(get_property path)
     (lambda (cur-path)
       (let ((old-path (when (and cur-path (stringp cur-path)
                                  (not (string-empty-p cur-path)))
                         cur-path)))
         ;; 1. 替换当前项为新 URL
         (empv--send-command `(loadfile ,url replace) nil)
         (when title
           (empv--send-command `(set_property media-title ,title) nil))
         ;; 2. 延时将旧项追加回列表末尾，避免干扰新视频加载
         (when old-path
           (run-at-time 0.5 nil
                        (lambda (p)
                          (empv--send-command `(loadfile ,p append) nil))
                        old-path))))))
   (t
    ;; mpv 未运行：启动新进程
    (empv-start url))))

(provide 'freebox-empv)
;;; freebox-empv.el ends here
