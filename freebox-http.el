;;; freebox-http.el --- HTTP API client for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP API wrappers for FreeBox backend.
;; All functions are async with callback pattern.

;;; Code:

(require 'request)

(defgroup freebox-http nil
  "FreeBox HTTP API client."
  :group 'freebox)

(defcustom freebox-http-url "http://127.0.0.1:9978/api"
  "Base URL for FreeBox REST API."
  :type 'string
  :group 'freebox-http)

(defcustom freebox-http-timeout 30
  "HTTP request timeout in seconds."
  :type 'integer
  :group 'freebox-http)

(defcustom freebox-http-cloud-url
  "https://vbox-300334-11-1471098780.sh.run.tcloudbase.com/api"
  "Base URL for the cloud-hosted FreeBox backend (used when cloud mode is on)."
  :type 'string
  :group 'freebox-http)

(defcustom freebox-http-local-url "http://127.0.0.1:9978/api"
  "Base URL for the local FreeBox backend (used when cloud mode is off)."
  :type 'string
  :group 'freebox-http)

(defcustom freebox-http-cloud-mode nil
  "When non-nil, `freebox-http-url' points at the cloud backend.
Toggle with `freebox-http-toggle-cloud-mode'."
  :type 'boolean
  :group 'freebox-http)

(defun freebox-http-cloud-mode ()
  "Return t if cloud mode is active (local URL is nil or cloud flag set)."
  (if freebox-http-cloud-mode
      t
    (not (string-match-p "127\\.0\\.0\\.1\\|localhost" freebox-http-url))))

(defun freebox-http-toggle-cloud-mode ()
  "Toggle between local and cloud FreeBox backend.
Cloud mode uses `freebox-http-cloud-url'; local mode uses
`freebox-http-local-url' (defaults to http://127.0.0.1:9978/api)."
  (interactive)
  (let ((cloud (not (freebox-http-cloud-mode))))
    (setq freebox-http-cloud-mode cloud)
    (setq freebox-http-url
          (if cloud
              freebox-http-cloud-url
            (or freebox-http-local-url "http://127.0.0.1:9978/api")))
    (message "FreeBox: %s backend (%s)" (if cloud "云端" "本地") freebox-http-url)))

;;; ─── Helper functions ─────────────────────────────────────────────────────

(defun freebox-http--jget (obj key)
  "Get KEY (string or symbol) from OBJ returned by json-read (alist with symbol keys)."
  (alist-get (if (symbolp key) key (intern key)) obj))

(defun freebox-http--request (endpoint params callback &optional timeout)
  "Make HTTP GET request to ENDPOINT with PARAMS.
CALLBACK is called with (ERROR DATA).
Optional TIMEOUT overrides `freebox-http-timeout' (useful for long-poll)."
  (request (format "%s/%s" freebox-http-url endpoint)
    :type "GET"
    :params params
    :parser 'json-read
    :timeout (or timeout freebox-http-timeout)
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((code   (freebox-http--jget data 'code))
                       (result (freebox-http--jget data 'data)))
                  (if (equal code 200)
                      (funcall callback nil result)
                    (funcall callback
                             (format "API error %s: %s"
                                     code
                                     (freebox-http--jget data 'message))
                             nil)))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (funcall callback (format "HTTP error: %s" error-thrown) nil)))))

;;; ─── API functions ───────────────────────────────────────────────────────

(defun freebox-http-get-clients (callback)
  "Get list of saved CATVOD_SPIDER client configurations from FreeBox.
CALLBACK is called with (ERROR CLIENTS)."
  (freebox-http--request "clients" nil callback))

(defun freebox-http-get-live-clients (callback)
  "Get list of SINGLE_LIVE (live TV) client configurations from FreeBox.
CALLBACK is called with (ERROR CLIENTS)."
  (freebox-http--request "live/clients" nil callback))

(defun freebox-http-get-live-channels (client-id callback)
  "Get live TV channel groups for CLIENT-ID from FreeBox.
CALLBACK is called with (ERROR GROUPS).
Uses 45s timeout — live sources can be large."
  (freebox-http--request "live/channels"
    `((clientId . ,client-id))
    callback 45))

(defun freebox-http-get-sources (&optional client-id callback)
  "Get list of video sources from FreeBox.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR SOURCES)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "sources"
    (when client-id `((clientId . ,client-id)))
    callback))

(defun freebox-http-search (source-key keyword &optional client-id callback)
  "Search for videos in SOURCE-KEY by KEYWORD.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "search"
    `((sourceKey . ,source-key)
      (keyword . ,keyword)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-get-categories (source-key &optional client-id callback)
  "Get top-level categories from SOURCE-KEY (home/首页 content).
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "categories"
    `((sourceKey . ,source-key)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-get-category (source-key tid &optional page client-id callback)
  "Get category content from SOURCE-KEY with category id TID.
PAGE defaults to 1.  Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT)."
  ;; Handle optional args: (source-key tid callback) or (source-key tid page callback)
  ;; or (source-key tid page client-id callback)
  (cond
   ((functionp page)
    (setq callback page page 1 client-id nil))
   ((functionp client-id)
    (setq callback client-id client-id nil)))
  (freebox-http--request "category"
    `((sourceKey . ,source-key)
      (tid . ,tid)
      (page . ,(number-to-string (or page 1)))
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-get-detail (source-key vod-id &optional client-id callback)
  "Get VOD details for VOD-ID from SOURCE-KEY.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR DETAIL)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "detail"
    `((sourceKey . ,source-key)
      (vodId . ,vod-id)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-get-play-url (source-key play-flag vod-id &optional client-id callback)
  "Get playback URL for VOD.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR PLAYINFO)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "play"
    `((sourceKey . ,source-key)
      (playFlag . ,play-flag)
      (vodId . ,vod-id)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-resolve-share (source-key flag share-link &optional client-id callback)
  "Resolve SHARE-LINK for FLAG under SOURCE-KEY.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "resolve-share"
    `((sourceKey . ,source-key)
      (flag . ,flag)
      (shareLink . ,share-link)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-get-qr-login (drive-type &optional client-id callback)
  "Get QR code login URL for DRIVE-TYPE (quark/uc/bd).
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT)."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (freebox-http--request "qr-login"
    `((type . ,drive-type)
      ,@(when client-id `((clientId . ,client-id))))
    callback))

(defun freebox-http-poll-qr-status (drive-type token &optional client-id callback)
  "Poll QR code login status for DRIVE-TYPE (quark/uc/bd) with TOKEN.
Optional CLIENT-ID selects which client config to use.
CALLBACK is called with (ERROR RESULT).
Result data has: (status . \"pending\"|\"success\"|\"failed\"|\"expired\")
BD uses 45s timeout for baidu unicast long-poll, others use default."
  (when (functionp client-id)
    (setq callback client-id)
    (setq client-id nil))
  (let ((timeout (if (string= drive-type "bd") 45 nil)))
    (freebox-http--request "qr-status"
      `((type . ,drive-type)
        (token . ,token)
        ,@(when client-id `((clientId . ,client-id))))
      callback
      timeout)))

;;; ─── Server management ────────────────────────────────────────────────────

(defcustom freebox-http-server-script nil
  "Path to the FreeBox jlink launcher script.
Example: \"/home/USER/git/FreeBox/build/image/bin/FreeBox\"
When nil, auto-start is disabled and the server must be started manually."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'freebox-http)

(defcustom freebox-http-server-args '("run" "--args=--headless")
  "Command-line args passed to `freebox-http-server-script' when starting.
The default starts the Java FreeBox backend via gradlew.
For the vbox Node.js backend, set to (\"daemon\" \"start\")."
  :type '(repeat string)
  :group 'freebox-http)

(defcustom freebox-http-server-stop-args nil
  "When non-nil, args passed to `freebox-http-server-script' to stop the daemon.
For the vbox backend, set to (\"daemon\" \"stop\").
When nil, the managed process is stopped by deleting the process object."
  :type '(choice (const :tag "Managed process" nil) (repeat string))
  :group 'freebox-http)

(defcustom freebox-http-server-start-timeout 30
  "Seconds to wait for FreeBox server to become ready after starting."
  :type 'integer
  :group 'freebox-http)

(defvar freebox-http--server-process nil
  "The FreeBox server process managed by Emacs, or nil.")

(defvar freebox-http--proxy-process nil
  "The lu-proxy-server process managed by Emacs, or nil.")

(defcustom freebox-http-proxy-server-path
  (expand-file-name "lu-proxy-server-linux-amd64" "~/TV/")
  "Path to the lu-proxy-server binary.
When non-nil, this proxy server is started automatically before FreeBox."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'freebox-http)

(defun freebox-http--server-running-p ()
  "Return t if FreeBox server responds with HTTP 200 at `freebox-http-url'/clients."
  (condition-case nil
      (let ((buf (url-retrieve-synchronously
                  (concat freebox-http-url "/clients") t nil 2)))
        (when buf
          (with-current-buffer buf
            (goto-char (point-min))
            (prog1
                (looking-at "HTTP/[0-9.]+ 200")
              (kill-buffer buf)))))
    (error nil)))

(defun freebox-http-start-server ()
  "Start the FreeBox backend in headless mode.
Also starts the lu-proxy-server on port 12345 for video streaming.
Requires `freebox-http-server-script' to be set (path to gradlew).
The process is started in the gradlew directory with `run --args=--headless'.
Output is collected in the *freebox-server* buffer."
  (interactive)
  (unless freebox-http-server-script
    (user-error "FreeBox: freebox-http-server-script is not configured"))
  ;; Start proxy server if configured and not already running
  (when (and freebox-http-proxy-server-path
             (file-executable-p freebox-http-proxy-server-path)
             (not (and freebox-http--proxy-process
                       (process-live-p freebox-http--proxy-process))))
    (message "FreeBox: starting proxy server (port 12345)...")
    (setq freebox-http--proxy-process
          (make-process
           :name    "freebox-proxy"
           :buffer  "*freebox-proxy*"
           :command (list freebox-http-proxy-server-path)
           :noquery t)))
  (let* ((script  (expand-file-name freebox-http-server-script))
         ;; gradlew must be run from the project root directory
         (default-directory (file-name-directory script)))
    (unless (file-executable-p script)
      (user-error "FreeBox: server script not executable: %s" script))
    (message "FreeBox: starting server (output in *freebox-server*)...")
    (setq freebox-http--server-process
          (make-process
           :name    "freebox-server"
           :buffer  "*freebox-server*"
           :command (cons script freebox-http-server-args)
           :noquery t
           :sentinel (lambda (_proc event)
                       (message "FreeBox server: %s" (string-trim event)))))
    freebox-http--server-process))

(defun freebox-http-stop-server ()
  "Stop the FreeBox backend and proxy server managed by Emacs."
  (interactive)
  ;; Stop proxy server
  (when (and freebox-http--proxy-process
             (process-live-p freebox-http--proxy-process))
    (delete-process freebox-http--proxy-process)
    (setq freebox-http--proxy-process nil))
  ;; Stop backend server
  (cond
   ;; vbox 等 daemon 型后端：调用脚本的 stop 子命令
   (freebox-http-server-stop-args
    (message "FreeBox: stopping server via %s %s..."
             freebox-http-server-script (mapconcat #'identity freebox-http-server-stop-args " "))
    (let ((default-directory (file-name-directory
                              (expand-file-name freebox-http-server-script))))
      (make-process
       :name    "freebox-server-stop"
       :buffer  "*freebox-server-stop*"
       :command (cons (expand-file-name freebox-http-server-script)
                      freebox-http-server-stop-args)
       :noquery t)))
   ((and freebox-http--server-process
         (process-live-p freebox-http--server-process))
    (delete-process freebox-http--server-process)
    (setq freebox-http--server-process nil)
    (message "FreeBox: server stopped."))
   (t
    (message "FreeBox: no managed server process running."))))

(defconst freebox-http--server-script-default
  "~/git/FreeBox/gradlew"
  "Default path shown when prompting the user to locate the FreeBox launcher script.\nExpected to be the gradlew wrapper; the server is started with --args=\"--headless\".")

(defun freebox-http--prompt-server-script ()
  "Interactively ask the user for the FreeBox launcher script path.
Saves the chosen path to `freebox-http-server-script' persistently via
`customize-save-variable' so the prompt only appears once."
  (let* ((default (expand-file-name freebox-http--server-script-default))
         (chosen  (read-file-name
                   "FreeBox launcher script: "
                   (file-name-directory default)
                   default
                   t
                   (file-name-nondirectory default))))
    (when (and chosen (not (string-empty-p chosen)))
      (customize-save-variable 'freebox-http-server-script chosen)
      chosen)))

(defun freebox-http-ensure-server (callback)
  "Ensure FreeBox server is running, then invoke CALLBACK with no args.
If the server already responds at `freebox-http-url', CALLBACK is called
immediately (synchronous fast path).
If `freebox-http-server-script' is nil, prompt the user once to select
the launcher script path (default: `freebox-http--server-script-default'),
save the selection persistently, then start the server.
If the user cancels the prompt, the operation is aborted.
Otherwise, starts the server process and polls every 2 seconds until the
server is ready or `freebox-http-server-start-timeout' is exceeded."
  (if (freebox-http--server-running-p)
      (funcall callback)
    ;; Resolve script path: prompt if not yet configured
    (let ((script (or freebox-http-server-script
                      (freebox-http--prompt-server-script))))
      (if (not script)
          (message "FreeBox: server script not selected, aborting.")
        ;; Persist chosen path for future sessions
        (setq freebox-http-server-script script)
        (freebox-http-start-server)
        (let* ((deadline (+ (float-time) freebox-http-server-start-timeout))
               (poll nil))
          (setq poll
                (run-with-timer
                 2 2
                 (lambda ()
                   (cond
                    ((freebox-http--server-running-p)
                     (cancel-timer poll)
                     (message "FreeBox: server ready.")
                     (funcall callback))
                    ((> (float-time) deadline)
                     (cancel-timer poll)
                     (message
                      "FreeBox: server failed to start within %ds. Check *freebox-server* buffer."
                      freebox-http-server-start-timeout)))))))))))

(provide 'freebox-http)
;;; freebox-http.el ends here
