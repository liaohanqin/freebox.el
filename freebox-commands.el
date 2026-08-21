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
;;   b  Browse category  -- Open the VOD tree browser
;;   s  Search videos    -- Full-text search
;;   v  Resume last pos  -- Resume from last remembered navigation node
;;   o  Open URL         -- Play a URL directly (supports magnet links)
;;   L  Live TV          -- Browse and play live TV channels
;;   S  Save magnet file -- Save downloaded magnet file to another directory
;;   r  Start server     -- Start FreeBox backend
;;   K  Stop server      -- Stop managed backend (shifted: plain `k' is too
;;                          easy to hit by accident during minibuffer
;;                          interaction — hydra intercepts head keys there,
;;                          and stopping kills the whole backend)
;;   q  Quit             -- Close menu

;;; Code:

(require 'freebox-ui)
(require 'freebox-http)
(require 'freebox-live)
(require 'freebox-vod)

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
     (("b" freebox-browse-category "Browse category")
      ("s" freebox-search  "Search videos")
      ("v" freebox-resume  "Resume last pos")
      ("o" freebox-open-url "Open URL")
      ("L" freebox-live "Live TV")
      ("S" freebox-save-magnet-file "Save magnet file"))
     "Server"
     (("r" freebox-http-start-server "Start server")
      ("K" freebox-http-stop-server  "Stop server"))
     "Mode"
     (("M" freebox-http-toggle-cloud-mode "Cloud"
       :toggle freebox-http-cloud-mode))
     "Other"
     (("?" freebox-help "Help"))
     "Login"
     (("Q" freebox-qr-login-quark "Quark")
      ("U" freebox-qr-login-uc    "UC")
      ("B" freebox-qr-login-bd    "Baidu")))))

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
     (("b" freebox-browse-category "Browse category")
      ("s" freebox-search  "Search videos")
      ("v" freebox-resume  "Resume last pos")
      ("o" freebox-open-url "Open URL")
      ("L" freebox-live "Live TV")
      ("S" freebox-save-magnet-file "Save magnet file"))
     "Server"
     (("r" freebox-http-start-server "Start server")
      ("K" freebox-http-stop-server  "Stop server"))
     "Mode"
     (("M" freebox-http-toggle-cloud-mode "Cloud"
       :toggle freebox-http-cloud-mode))
     "Other"
     (("?" freebox-help "Help"))
     "Login"
     (("Q" freebox-qr-login-quark "Quark")
      ("U" freebox-qr-login-uc    "UC")
      ("B" freebox-qr-login-bd    "Baidu"))))
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
  (freebox-live-select-client))

;;;###autoload
(defun freebox-select-source ()
  "Select or change the FreeBox source."
  (interactive)
  (freebox-ui-select-source))

;;;###autoload
(defun freebox-select-category ()
  "Select a FreeBox category within the current source (in the VOD tree)."
  (interactive)
  (freebox-vod-select-category))

;;;###autoload
(defun freebox-search ()
  "Search FreeBox for videos (results as a group in the VOD tree)."
  (interactive)
  (freebox-vod-search))

;;;###autoload
(defun freebox-browse-category ()
  "Browse FreeBox videos in the VOD tree buffer."
  (interactive)
  (freebox-vod-open))

;;;###autoload
(defun freebox-resume ()
  "Resume browsing from the last remembered navigation position.
Restores the VOD tree to the deepest saved node: category page,
vod detail, or episode line."
  (interactive)
  (freebox-vod-resume))

;;;###autoload
(defun freebox-open-url ()
  "Open a URL for playback via empv.  Supports magnet links and direct URLs."
  (interactive)
  (let ((was-hydra-active (and (boundp 'hydra-curr-map) hydra-curr-map)))
    (setq hydra-curr-on-exit nil)
    (let ((url (read-string "FreeBox URL (or magnet): ")))
      (when (and was-hydra-active (not hydra-curr-map)
                 (fboundp 'freebox-menu/body))
        (freebox-menu/body))
      (when (and url (not (string-empty-p url)))
        (freebox-empv-play-url url)))))

;;;###autoload
(defun freebox-live ()
  "Browse and play FreeBox live TV channels in a tree buffer."
  (interactive)
  (freebox-live-open))

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
   "FreeBox: x=client  y=source  z=category  l=live-src  b=browse  s=search  v=resume  o=open-url  L=live  S=save  r=start  K=stop  Q=Quark  U=UC  B=Baidu  q=quit"))

;;;###autoload
(defun freebox-qr-login-quark ()
  "Start QR code login for Quark cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "quark" nil nil
      (lambda ()
        (message "FreeBox: Quark login done! Please replay the video"))))))

;;;###autoload
(defun freebox-qr-login-uc ()
  "Start QR code login for UC cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "uc" nil nil
      (lambda ()
        (message "FreeBox: UC login done! Please replay the video"))))))

;;;###autoload
(defun freebox-qr-login-bd ()
  "Start QR code login for Baidu cloud drive."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--start-qr-login
      "bd" nil nil
      (lambda ()
        (message "FreeBox: Baidu cloud login done! Please replay the video"))))))

(provide 'freebox-commands)
;;; freebox-commands.el ends here
