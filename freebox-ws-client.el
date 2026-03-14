;;; freebox-ws-client.el --- WebSocket client for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Handles the WebSocket connection and message routing with the FreeBox backend.
;; Implements the topic-based request-response protocol.

;;; Code:

(require 'websocket)
(require 'json)

(defgroup freebox-ws nil
  "WebSocket client for FreeBox."
  :group 'freebox)

(defcustom freebox-ws-url "ws://127.0.0.1:9977/api/ws/keb"
  "The WebSocket URL for the FreeBox backend."
  :type 'string
  :group 'freebox-ws)

(defvar freebox-ws--connection nil
  "The current WebSocket connection.")

(defvar freebox-ws--topic-callbacks (make-hash-table :test 'equal)
  "A hash table storing callbacks for pending topics.
Key is the topicId (string), value is a function taking the response data.")

(defun freebox-ws--generate-uuid ()
  "Generate a random UUID for topicId."
  (org-id-uuid)) ;; Alternatively we can use a simpler generator if org-id is not preferred

(require 'org-id)

(defun freebox-ws--handle-message (_websocket frame)
  "Handle an incoming WebSocket message FRAME."
  (let* ((text (websocket-frame-text frame))
         (msg-alist (condition-case err
                        (json-read-from-string text)
                      (error
                       (message "FreeBox WS JSON parse error: %s" err)
                       nil))))
    (when msg-alist
      (let ((code (alist-get 'code msg-alist))
            (data (alist-get 'data msg-alist))
            (topic-flag (alist-get 'topicFlag msg-alist))
            (topic-id (alist-get 'topicId msg-alist)))

        ;; If it's a topic response (it has topicId and might not have topicFlag true)
        ;; Actually according to Java model: topicFlag=true means sender wants a response.
        ;; So if we sent topicFlag=true, the server replies with topicFlag=false but same topicId.
        (when (and topic-id (hash-table-p freebox-ws--topic-callbacks))
          (let ((callback (gethash topic-id freebox-ws--topic-callbacks)))
            (when callback
              (remhash topic-id freebox-ws--topic-callbacks)
              (funcall callback code data))))

        ;; We can also handle specific pushed codes here if needed
        ))))

(defun freebox-ws--on-open (_websocket)
  "Callback when WebSocket connection opens."
  (message "FreeBox WebSocket connected."))

(defun freebox-ws--on-close (_websocket)
  "Callback when WebSocket connection closes."
  (message "FreeBox WebSocket disconnected.")
  (setq freebox-ws--connection nil)
  (clrhash freebox-ws--topic-callbacks))

(defun freebox-ws--on-error (_websocket &rest args)
  "Callback when WebSocket connection error occurs."
  (message "FreeBox WebSocket error: %S" args))

(defun freebox-ws-connect ()
  "Establish a WebSocket connection to FreeBox."
  (interactive)
  (when (and freebox-ws--connection (websocket-openp freebox-ws--connection))
    (websocket-close freebox-ws--connection))
  (clrhash freebox-ws--topic-callbacks)
  (setq freebox-ws--connection
        (websocket-open freebox-ws-url
                        :on-message #'freebox-ws--handle-message
                        :on-open #'freebox-ws--on-open
                        :on-close #'freebox-ws--on-close
                        :on-error #'freebox-ws--on-error)))

(defun freebox-ws-disconnect ()
  "Disconnect the WebSocket connection."
  (interactive)
  (when freebox-ws--connection
    (websocket-close freebox-ws--connection)
    (setq freebox-ws--connection nil)))

(defun freebox-ws-send (code data &optional callback)
  "Send a message to FreeBox.
CODE is the message code (see MessageCodes.java).
DATA is an alist or plist to be serialized as JSON.
If CALLBACK is provided, it is a function taking (RESPONSE-CODE RESPONSE-DATA).
It will be called when the server replies to this topic."
  (unless (and freebox-ws--connection (websocket-openp freebox-ws--connection))
    (error "FreeBox WebSocket is not connected"))

  (let* ((topic-id (when callback (freebox-ws--generate-uuid)))
         (topic-flag (if callback t :json-false))
         (msg `((code . ,code)
                (data . ,data)
                (topicFlag . ,topic-flag))))

    (when topic-id
      (push `(topicId . ,topic-id) msg)
      (puthash topic-id callback freebox-ws--topic-callbacks))

    (websocket-send-text freebox-ws--connection
                         (json-encode msg))))

(provide 'freebox-ws-client)
;;; freebox-ws-client.el ends here
