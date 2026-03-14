;;; freebox-empv.el --- empv integration for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handlers to pass FreeBox media URLs to empv for playback.

;;; Code:

(require 'empv nil t)

(defun freebox-empv-play-url (url &optional title)
  "Play URL using empv.
TITLE is optional and can be used to set the media title."
  (if (fboundp 'empv--play-url)
      (empv--play-url url)
    (if (fboundp 'empv-play)
        (empv-play url)
      (error "empv package not found or empv-play is not bound"))))

(provide 'freebox-empv)
;;; freebox-empv.el ends here
