;;; freebox-ui.el --- UI components for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Completing-read interfaces for selecting sources, searching and playing.

;;; Code:

(require 'freebox-api)
(require 'freebox-model)
(require 'freebox-empv)

(defvar freebox-ui-current-source nil
  "The currently selected source key.")

(defun freebox-ui-select-source ()
  "Interactively select a FreeBox source."
  (interactive)
  (freebox-api-get-sources
   (lambda (_code data)
     (let* ((sources (append data nil))
            (candidates (mapcar (lambda (s)
                                  (cons (freebox-model-source-name s)
                                        (freebox-model-source-key s)))
                                sources)))
       (when candidates
         (let* ((selected-name (completing-read "Select FreeBox Source: " candidates nil t))
                (selected-key (cdr (assoc selected-name candidates))))
           (setq freebox-ui-current-source selected-key)
           (message "Selected source: %s" selected-name)))))))

(defun freebox-ui-search ()
  "Interactively search using the current source."
  (interactive)
  (unless freebox-ui-current-source
    (call-interactively 'freebox-ui-select-source))
  (when freebox-ui-current-source
    (let ((keyword (read-string "Search FreeBox: ")))
      (freebox-api-search
       freebox-ui-current-source
       keyword
       (lambda (_code data)
         (let* ((list (alist-get 'list data))
                (candidates (mapcar (lambda (v)
                                      (cons (freebox-model-vod-name v)
                                            (freebox-model-vod-id v)))
                                    (append list nil))))
           (if (not candidates)
               (message "No results found.")
             (let* ((selected-name (completing-read "Select Video: " candidates nil t))
                    (selected-id (cdr (assoc selected-name candidates))))
               (freebox-ui-show-detail selected-id)))))))))

(defun freebox-ui-show-detail (vod-id)
  "Show details and select episode for VOD-ID."
  (freebox-api-get-detail
   freebox-ui-current-source
   vod-id
   (lambda (_code data)
     (let* ((list (alist-get 'list data))
            (vod (when (> (length list) 0) (aref list 0))))
       (when vod
         (let* ((series-map (freebox-model-vod-series-map vod))
                (flags (mapcar 'car series-map))
                (selected-flag (if (> (length flags) 1)
                                   (completing-read "Select Play Flag: " flags nil t)
                                 (car flags)))
                (episodes (alist-get (intern selected-flag) series-map))
                (candidates (mapcar (lambda (ep)
                                      (cons (freebox-model-series-name ep)
                                            (freebox-model-series-url ep)))
                                    (append episodes nil))))
           (if (not candidates)
               (message "No episodes found.")
             (let* ((selected-ep (completing-read "Select Episode: " candidates nil t))
                    (play-url (cdr (assoc selected-ep candidates))))
               ;; Sometimes the URL returned is a direct URL, but often it requires passing back to get_player
               ;; It depends on the FreeBox implementation.
               ;; We can try direct play or call get_player API based on URL structure.
               (freebox-api-get-player-url
                freebox-ui-current-source
                selected-flag
                vod-id
                (lambda (_pcode pdata)
                  (let ((final-url (alist-get 'url pdata)))
                    (if final-url
                        (freebox-empv-play-url final-url selected-ep)
                      (freebox-empv-play-url play-url selected-ep)))))))))))))

(provide 'freebox-ui)
;;; freebox-ui.el ends here
