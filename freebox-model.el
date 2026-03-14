;;; freebox-model.el --- Data models for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Data structures and parsing helpers representing FreeBox backend entities.

;;; Code:

(require 'json)

;; Basically we can represent models as alists or plists in Emacs Lisp.
;; For convenience, we define some accessors.

;;; SourceBean
(defun freebox-model-source-key (source)
  (alist-get 'key source))

(defun freebox-model-source-name (source)
  (alist-get 'name source))

(defun freebox-model-source-searchable-p (source)
  (let ((searchable (alist-get 'searchable source)))
    (and searchable (= searchable 1))))

;;; VodInfo
(defun freebox-model-vod-id (vod)
  (alist-get 'id vod))

(defun freebox-model-vod-name (vod)
  (alist-get 'name vod))

(defun freebox-model-vod-pic (vod)
  (alist-get 'pic vod))

(defun freebox-model-vod-note (vod)
  (alist-get 'note vod))

(defun freebox-model-vod-actor (vod)
  (alist-get 'actor vod))

(defun freebox-model-vod-director (vod)
  (alist-get 'director vod))

(defun freebox-model-vod-des (vod)
  (alist-get 'des vod))

(defun freebox-model-vod-series-flags (vod)
  (let ((flags (alist-get 'seriesFlags vod)))
    (mapcar (lambda (f) (alist-get 'name f)) flags)))

(defun freebox-model-vod-series-map (vod)
  "Return an alist mapping flag (string) to a list of VodSeries alists."
  (alist-get 'seriesMap vod))

;;; VodSeries
(defun freebox-model-series-name (series)
  (alist-get 'name series))

(defun freebox-model-series-url (series)
  (alist-get 'url series))


(provide 'freebox-model)
;;; freebox-model.el ends here
