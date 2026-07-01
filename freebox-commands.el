;;; freebox-commands.el --- Interactive commands for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing M-x commands and the main pretty-hydra menu.
;;
;; Quick start:
;;   1. Start FreeBox backend:  ./FreeBox_*.AppImage --headless
;;   2. M-x freebox            Open main hydra menu
;;      or  C-c v v            (if setup-freebox.el is loaded)
;;
;; Main menu actions:
;;   x  Select client    -- Change client config (video source JSON)
;;   y  Select source    -- Change current source within client
;;   z  Select category  -- Select a category within current source
;;   l  Select live src  -- Select live TV source (SINGLE_LIVE)
;;   s  Search videos    -- Full-text search
;;   v  Resume last pos  -- Resume from last remembered navigation node
;;   o  Open URL         -- Play a URL directly (supports magnet links)
;;   L  Live TV          -- Browse and play live TV channels
;;   S  Save magnet file -- Save downloaded magnet file to another directory
;;   r  Start server     -- Start FreeBox backend
;;   k  Stop server      -- Stop managed backend
;;   q  Quit             -- Close menu

;;; Code:

(require 'freebox-ui)
(require 'freebox-http)

;;; --- Hydra title helpers -----------------------------------------------------

(defun freebox--client-status-short ()
  "Return short client status string for hydra display."
  (if freebox-ui-current-client-name
      (truncate-string-to-width freebox-ui-current-client-name 30 nil nil "...")
    "(none)"))

(defun freebox--source-status-short ()
  "Return short source status string for hydra display."
  (if freebox-ui-current-source-name
      freebox-ui-current-source-name
    "(none)"))

(defun freebox--category-status-short ()
  "Return short category status string for hydra display."
  (if freebox-ui-current-category-name
      freebox-ui-current-category-name
    "(none)"))

(defun freebox--server-status-string ()
  "Return server status string for hydra display.
Checks managed process first (non-blocking), then falls back to HTTP ping."
  (cond
   ((and freebox-http--server-process
         (process-live-p freebox-http--server-process))
    "Running (managed)")
   ((freebox-http--server-running-p)
    "Running (external)")
   (t "Stopped")))

(defun freebox--format-menu-title ()
  "Format the main menu title with current state."
  (format "FreeBox - Emacs Video Client\nClient: %s | Source: %s\nServer: %s"
          (freebox--client-status-short)
          (freebox--source-status-short)
          (freebox--server-status-string)))

;;; --- Static hydra definition (loaded once) -----------------------------------

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define freebox-menu
    (:title (format "%s" (freebox--format-menu-title))
     :color red
     :quit-key "q")
    ("Configure"
     (("x" freebox-select-client   "Select client")
      ("y" freebox-select-source   "Select source")
      ("z" freebox-select-category "Select category")
      ("l" freebox-select-live-client "Select live source"))
     "Browse"
     (("s" freebox-search  "Search videos")
      ("v" freebox-resume  "Resume last pos")
      ("o" freebox-open-url "Open URL")
      ("L" freebox-live "Live TV")
      ("S" freebox-save-magnet-file "Save magnet file"))
     "Server"
     (("r" freebox-http-start-server "Start server")
      ("k" freebox-http-stop-server  "Stop server"))
     "Other"
     (("?" freebox-help "Help"))
     "Login"
     (("Q" freebox-qr-login-quark "Quark扫码")
      ("U" freebox-qr-login-uc    "UC扫码")
      ("B" freebox-qr-login-bd    "百度扫码")))))

;;; --- Main entry point --------------------------------------------------------

;;;###autoload
(defun freebox ()
  "Open FreeBox main menu (hydra).
Restores previous menu state and displays current selections in title."
  (interactive)
  (freebox-ui-restore-state)
  ;; Re-define the hydra with fresh title (state was just restored above)
  (pretty-hydra-define freebox-menu
    (:title (format "%s" (freebox--format-menu-title))
     :color red
     :quit-key "q")
    ("Configure"
     (("x" freebox-select-client   "Select client")
      ("y" freebox-select-source   "Select source")
      ("z" freebox-select-category "Select category")
      ("l" freebox-select-live-client "Select live source"))
     "Browse"
     (("s" freebox-search  "Search videos")
      ("v" freebox-resume  "Resume last pos")
      ("o" freebox-open-url "Open URL")
      ("L" freebox-live "Live TV")
      ("S" freebox-save-magnet-file "Save magnet file"))
     "Server"
     (("r" freebox-http-start-server "Start server")
      ("k" freebox-http-stop-server  "Stop server"))
     "Other"
     (("?" freebox-help "Help"))
     "Login"
     (("Q" freebox-qr-login-quark "Quark扫码")
      ("U" freebox-qr-login-uc    "UC扫码")
      ("B" freebox-qr-login-bd    "百度扫码"))))
  (freebox-menu/body))

;;; --- Interactive commands ----------------------------------------------------

;;;###autoload
(defun freebox-select-client ()
  "Select a FreeBox client configuration (video source JSON URL)."
  (interactive)
  (freebox-ui-select-client))

;;;###autoload
(defun freebox-select-live-client ()
  "Select a FreeBox live TV client (SINGLE_LIVE source URL)."
  (interactive)
  (freebox-ui-select-live-client))

;;;###autoload
(defun freebox-select-source ()
  "Select or change the FreeBox source."
  (interactive)
  (freebox-ui-select-source))

;;;###autoload
(defun freebox-select-category ()
  "Select a FreeBox category within the current source."
  (interactive)
  (freebox-ui-select-category))

;;;###autoload
(defun freebox-search ()
  "Search FreeBox for videos."
  (interactive)
  (freebox-ui-search))

;;;###autoload
(defun freebox-browse-category ()
  "Browse FreeBox videos by (remembered) category."
  (interactive)
  (freebox-ui-browse-category))

;;;###autoload
(defun freebox-resume ()
  "Resume browsing from the last remembered navigation position.
Restores to the deepest saved node: vod-list page, category, or source selection."
  (interactive)
  (freebox-ui-resume))

;;;###autoload
(defun freebox-open-url ()
  "Open a URL for playback via empv.  Supports magnet links and direct URLs."
  (interactive)
  (let ((hydra-curr-on-exit nil)
        (url (read-string "FreeBox URL (or magnet): ")))
    (when (and url (not (string-empty-p url)))
      (freebox-empv-play-url url))))

;;;###autoload
(defun freebox-live ()
  "Browse and play FreeBox live TV channels."
  (interactive)
  (freebox-ui-live))

;;;###autoload
(defun freebox-save-magnet-file ()
  "Save the current magnet download to another directory."
  (interactive)
  (freebox-empv-save-magnet-file))

;;;###autoload
(defun freebox-help ()
  "Show FreeBox keybinding help."
  (interactive)
  (message
   "FreeBox: x=client  y=source  z=category  l=live-src  s=search  v=resume  o=open-url  L=live  S=save  r=start  k=stop  Q=Quark  U=UC  B=百度  q=quit"))

;;;###autoload
(defun freebox-qr-login-quark ()
  "Start QR code login for Quark cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "quark" nil nil
      (lambda ()
        (message "FreeBox: Quark 扫码完成！请重新播放视频"))))))

;;;###autoload
(defun freebox-qr-login-uc ()
  "Start QR code login for UC cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "uc" nil nil
      (lambda ()
        (message "FreeBox: UC 扫码完成！请重新播放视频"))))))

;;;###autoload
(defun freebox-qr-login-bd ()
  "Start QR code login for Baidu cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "bd" nil nil
      (lambda ()
        (message "FreeBox: 百度网盘 扫码完成！请重新播放视频"))))))

(provide 'freebox-commands)
;;; freebox-commands.el ends here
