;;; freebox-commands.el --- Interactive commands for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; User-facing M-x commands.

;;; Code:

(require 'freebox-ws-client)
(require 'freebox-ui)

;;;###autoload
(defun freebox-connect ()
  "Connect to the FreeBox backend WebSocket."
  (interactive)
  (freebox-ws-connect))

;;;###autoload
(defun freebox-disconnect ()
  "Disconnect from the FreeBox backend WebSocket."
  (interactive)
  (freebox-ws-disconnect))

;;;###autoload
(defun freebox-search ()
  "Search FreeBox for videos."
  (interactive)
  (freebox-ui-search))

;;;###autoload
(defun freebox-select-source ()
  "Select the current FreeBox source."
  (interactive)
  (freebox-ui-select-source))

(provide 'freebox-commands)
;;; freebox-commands.el ends here
