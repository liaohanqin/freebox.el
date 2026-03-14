;;; freebox-api.el --- API wrappers for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; High-level asynchronous API functions utilizing the WebSocket client.

;;; Code:

(require 'freebox-ws-client)

;; Message codes from MessageCodes.java
(defconst freebox-api-code-get-sources 201)
(defconst freebox-api-code-get-search 213)
(defconst freebox-api-code-get-detail 207)
(defconst freebox-api-code-get-player 209)
(defconst freebox-api-code-get-category 205)

(defun freebox-api-get-sources (callback)
  "Get the list of sources from FreeBox.
CALLBACK is called with (CODE DATA) where DATA is a list of SourceBean alists."
  (freebox-ws-send freebox-api-code-get-sources nil callback))

(defun freebox-api-search (source-key keyword callback)
  "Search for KEYWORD in SOURCE-KEY.
CALLBACK is called with (CODE DATA) where DATA is the search result."
  (freebox-ws-send freebox-api-code-get-search
                   `((sourceKey . ,source-key)
                     (keyword . ,keyword))
                   callback))

(defun freebox-api-get-detail (source-key vod-id callback)
  "Get details for a VOD.
CALLBACK is called with (CODE DATA) where DATA is a list containing VodInfo."
  (freebox-ws-send freebox-api-code-get-detail
                   `((sourceKey . ,source-key)
                     (vodId . ,vod-id))
                   callback))

(defun freebox-api-get-player-url (source-key play-flag vod-id callback)
  "Get playback URL for a VOD.
CALLBACK is called with (CODE DATA) where DATA contains the URL."
  (freebox-ws-send freebox-api-code-get-player
                   `((sourceKey . ,source-key)
                     (playFlag . ,play-flag)
                     (vodId . ,vod-id)
                     (vipParseFlags . []))
                   callback))

(defun freebox-api-get-category (source-key tid page callback &optional filter extend)
  "Get category content.
CALLBACK is called with (CODE DATA)."
  (freebox-ws-send freebox-api-code-get-category
                   `((sourceKey . ,source-key)
                     (tid . ,tid)
                     (page . ,(number-to-string page))
                     (filter . ,(if filter t :json-false))
                     (extend . ,(or extend (make-hash-table))))
                   callback))

(provide 'freebox-api)
;;; freebox-api.el ends here
