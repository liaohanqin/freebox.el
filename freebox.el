;;; freebox.el --- Emacs client for FreeBox backend -*- lexical-binding: t; -*-

;; Copyright (C) 2024 lynx

;; Author: lynx
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (websocket "1.14") (request "0.3.2"))
;; Keywords: multimedia, video

;;; Commentary:

;; An Emacs frontend for FreeBox, a TVBox-compatible video streaming backend.
;; This package manages the FreeBox local server process and provides a
;; transient/hydra based UI for searching, browsing, and playing videos via empv.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'websocket nil t)
(require 'request nil t)

(defgroup freebox nil
  "Emacs client for FreeBox."
  :group 'multimedia
  :prefix "freebox-")

(defcustom freebox-jar-path (expand-file-name "~/git/FreeBox/build/libs/FreeBox.jar") ; TODO: update path
  "Path to the FreeBox backend JAR file."
  :type 'file
  :group 'freebox)

(defcustom freebox-ws-port 8081
  "Port for the FreeBox WebSocket server."
  :type 'integer
  :group 'freebox)

(defcustom freebox-http-port 8080
  "Port for the FreeBox HTTP server."
  :type 'integer
  :group 'freebox)

(defcustom freebox-java-executable "java"
  "Path to the Java executable (requires Java 17+)."
  :type 'string
  :group 'freebox)

(provide 'freebox)
;;; freebox.el ends here
