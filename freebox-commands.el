;;; freebox-commands.el --- Interactive commands for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing M-x commands and the main transient menu.
;;
;; Quick start:
;;   1. Start FreeBox backend:  ./FreeBox_*.AppImage --headless
;;   2. M-x freebox            Open main menu
;;      or  C-c v v            (if setup-freebox.el is loaded)
;;
;; Main menu actions:
;;   c  freebox-select-client  Change client config (video source JSON)
;;   S  freebox-select-source  Change current source within client
;;   s  freebox-search         Search videos
;;   b  freebox-browse-category  Browse by category

;;; Code:

(require 'freebox-ui)
(require 'transient)

;;; ─── Status helpers ──────────────────────────────────────────────────────────

(defun freebox--client-status ()
  "Return a string describing the current client config."
  (if freebox-ui-current-client-name
      (propertize
       (format "[%s]"
               (truncate-string-to-width freebox-ui-current-client-name 50 nil nil "…"))
       'face 'font-lock-string-face)
    (propertize "(none — press c)" 'face 'shadow)))

(defun freebox--source-status ()
  "Return a string describing the current source."
  (if freebox-ui-current-source
      (propertize (format "[%s]" freebox-ui-current-source-name)
                  'face 'font-lock-constant-face)
    (propertize "(none — press S)" 'face 'shadow)))

;;; ─── Transient main menu ──────────────────────────────────────────────────────

(transient-define-prefix freebox ()
  "FreeBox — Emacs video client."
  [:description
   (lambda ()
     (format "FreeBox\n  client: %s\n  source: %s"
             (freebox--client-status)
             (freebox--source-status)))]
  [["Configure"
    ("c" "Select client config"   freebox-select-client)
    ("S" "Select source"          freebox-select-source)]
   ["Browse"
    ("s" "Search videos"          freebox-search)
    ("b" "Browse by category"     freebox-browse-category)]])

;;; ─── Interactive commands ─────────────────────────────────────────────────────

;;;###autoload
(defun freebox-select-client ()
  "Select a FreeBox client configuration (video source JSON URL)."
  (interactive)
  (freebox-ui-select-client))

;;;###autoload
(defun freebox-search ()
  "Search FreeBox for videos."
  (interactive)
  (freebox-ui-search))

;;;###autoload
(defun freebox-select-source ()
  "Select or change the FreeBox source."
  (interactive)
  (freebox-ui-select-source))

;;;###autoload
(defun freebox-browse-category ()
  "Browse FreeBox videos by category."
  (interactive)
  (freebox-ui-browse-category))

(provide 'freebox-commands)
;;; freebox-commands.el ends here
