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

;;; ─── Helper functions ─────────────────────────────────────────────────────

(defun freebox-http--jget (obj key)
  "Get KEY (string or symbol) from OBJ returned by json-read (alist with symbol keys)."
  (alist-get (if (symbolp key) key (intern key)) obj))

(defun freebox-http--request (endpoint params callback)
  "Make HTTP GET request to ENDPOINT with PARAMS.
CALLBACK is called with (ERROR DATA)."
  (request (format "%s/%s" freebox-http-url endpoint)
    :type "GET"
    :params params
    :parser 'json-read
    :timeout freebox-http-timeout
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

(provide 'freebox-http)
;;; freebox-http.el ends here
