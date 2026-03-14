;;; freebox.el --- Emacs frontend for FreeBox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 lynx
;; Author: lynx <lynx@localhost>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (request "0.3.3") (dash "2.19.1") (transient "0.3.0"))
;; Keywords: multimedia, video, tools
;; URL: https://github.com/lynx/freebox.el

;;; Commentary:
;;
;; FreeBox Emacs Client - A video streaming client for Emacs
;;
;; Connects to a FreeBox backend via HTTP REST API to browse, search,
;; and play video contents using empv/mpv.
;;
;; Quick start:
;;   1. Start FreeBox:  ./FreeBox_*.AppImage --headless
;;   2. In Emacs:       M-x freebox
;;
;; Main entry point:
;;   M-x freebox          — Open main transient menu
;;   M-x freebox-search   — Search for videos
;;   M-x freebox-browse-category — Browse by category
;;   M-x freebox-select-source   — Choose a source
;;

;;; Code:

(require 'freebox-model)
(require 'freebox-http)
(require 'freebox-ui)
(require 'freebox-empv)
(require 'freebox-commands)

(provide 'freebox)
;;; freebox.el ends here
