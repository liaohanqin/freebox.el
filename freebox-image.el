;;; freebox-image.el --- Image cache and poster preview for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Async image download, local caching, and poster preview buffer.
;; Uses `url-retrieve' for binary downloads (not `request.el' which assumes JSON).
;; Cache: ~/.freebox/cache/posters/{md5(url)}.img
;; Preview buffer: *freebox-poster* with special-mode keybindings.

;;; Code:

(require 'url)

;;; --- Customization -----------------------------------------------------------

(defgroup freebox-image nil
  "FreeBox image cache and poster preview."
  :group 'freebox)

(defcustom freebox-image-cache-dir
  (expand-file-name "cache/posters/" "~/.freebox/")
  "Directory for cached poster images."
  :type 'directory
  :group 'freebox-image)

(defcustom freebox-image-cache-max-age-days 30
  "Maximum age in days before a cached image is considered expired."
  :type 'integer
  :group 'freebox-image)

(defcustom freebox-image-download-timeout 10
  "Timeout in seconds for image downloads."
  :type 'integer
  :group 'freebox-image)

;;; --- Cache management --------------------------------------------------------

(defun freebox-image--ensure-cache-dir ()
  "Ensure the poster cache directory exists."
  (make-directory freebox-image-cache-dir t))

(defun freebox-image--url-to-hash (url)
  "Return MD5 hash of URL as a hex string."
  (md5 url))

(defun freebox-image--cache-path (url)
  "Return the cache file path for URL (may not exist yet)."
  (expand-file-name
   (concat (freebox-image--url-to-hash url) ".img")
   freebox-image-cache-dir))

(defun freebox-image--cache-fresh-p (path)
  "Return non-nil if cached file at PATH exists and is not expired."
  (when (file-exists-p path)
    (let* ((attrs (file-attributes path))
           (mtime (file-attribute-modification-time attrs))
           (age-secs (float-time (time-subtract nil mtime)))
           (max-age-secs (* freebox-image-cache-max-age-days 24 3600)))
      (< age-secs max-age-secs))))

(defun freebox-image-clear-cache ()
  "Delete all cached poster images."
  (interactive)
  (when (file-directory-p freebox-image-cache-dir)
    (delete-directory freebox-image-cache-dir t)
    (freebox-image--ensure-cache-dir)
    (message "FreeBox: poster cache cleared.")))

(defun freebox-image-cleanup-expired ()
  "Delete cached poster images older than `freebox-image-cache-max-age-days'."
  (interactive)
  (let ((count 0))
    (when (file-directory-p freebox-image-cache-dir)
      (dolist (file (directory-files freebox-image-cache-dir t "\\.img\\'"))
        (unless (freebox-image--cache-fresh-p file)
          (delete-file file)
          (cl-incf count))))
    (message "FreeBox: removed %d expired poster(s)." count)))

;;; --- Async image download ----------------------------------------------------

(defun freebox-image-get (url callback)
  "Asynchronously fetch the image at URL, cache it, call CALLBACK with path.
CALLBACK is called with (PATH-OR-NIL).  PATH is the local cache file on
success, nil on failure.  If a fresh cache exists, CALLBACK is called
immediately with the cached path."
  (freebox-image--ensure-cache-dir)
  (let ((cache-path (freebox-image--cache-path url)))
    (if (freebox-image--cache-fresh-p cache-path)
        (funcall callback cache-path)
      ;; Async download
      (let ((url-request-extra-headers
             '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) Emacs"))))
        (condition-case _err
            (url-retrieve
             url
             (lambda (status _cb-url cb-path cb-callback)
               (if (or (plist-get status :error)
                       (not (buffer-live-p (current-buffer))))
                   (funcall cb-callback nil)
                 (condition-case nil
                     (progn
                       ;; Skip HTTP headers
                       (goto-char (point-min))
                       (when (re-search-forward "\r?\n\r?\n" nil t)
                         (let ((data (buffer-substring-no-properties
                                      (point) (point-max))))
                           (if (< (length data) 100)
                               ;; Too small, likely an error page
                               (funcall cb-callback nil)
                             (with-temp-file cb-path
                               (set-buffer-multibyte nil)
                               (insert data))
                             (funcall cb-callback cb-path)))))
                   (error (funcall cb-callback nil))))
               (when (buffer-live-p (current-buffer))
                 (kill-buffer (current-buffer))))
             (list url cache-path callback)
             t  ; silent
             t) ; inhibit cookies
          (error (funcall callback nil)))))))

;;; --- Poster preview buffer ---------------------------------------------------

(defconst freebox-image-buffer-name "*freebox-poster*"
  "Name of the poster preview buffer.")

(defvar-local freebox-image--vod nil
  "VOD alist for the currently previewed item.")

(defvar-local freebox-image--vod-id nil
  "VOD ID for the currently previewed item.")

(defvar-local freebox-image--poster-marker nil
  "Marker pointing to the poster insertion position.")

(declare-function freebox-ui--select-episode "freebox-ui")
(declare-function freebox-ui--jget "freebox-ui")

(defvar freebox-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'freebox-image-select-episode)
    (define-key map (kbd "p")   #'freebox-image-select-episode)
    map)
  "Keymap for `freebox-image-mode'.")

(define-derived-mode freebox-image-mode special-mode "FreeBox-Poster"
  "Major mode for FreeBox poster preview.
\\<freebox-image-mode-map>
\\[freebox-image-select-episode] - Select episode and play
\\[quit-window] - Close preview"
  :group 'freebox-image
  (setq-local cursor-type nil)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun freebox-image-select-episode ()
  "Close poster preview and enter episode selection."
  (interactive)
  (let ((vod freebox-image--vod)
        (vod-id freebox-image--vod-id))
    (quit-window t)
    (when (and vod vod-id)
      (freebox-ui--select-episode vod vod-id))))

(defun freebox-image-show-poster (vod vod-id pic-url)
  "Show poster preview buffer for VOD with VOD-ID.
PIC-URL is the poster image URL.  Text metadata is shown immediately;
the image is loaded asynchronously and inserted when ready."
  (let ((buf (get-buffer-create freebox-image-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (freebox-image-mode)
        ;; Save state
        (setq freebox-image--vod vod
              freebox-image--vod-id vod-id)
        ;; Insert metadata
        (freebox-image--insert-metadata vod)
        ;; Poster placeholder
        (insert "\n")
        (setq freebox-image--poster-marker (point-marker))
        (insert "[loading poster...]\n")
        ;; Description
        (let ((des (freebox-ui--jget vod 'des)))
          (when (and des (stringp des) (not (string-empty-p des)))
            (insert "\n" des "\n")))
        ;; Footer
        (insert "\n"
                (propertize (make-string 40 ?─) 'face 'shadow)
                "\n"
                (propertize "[RET/p] 选择剧集   [q] 返回"
                            'face 'font-lock-comment-face)
                "\n")
        (goto-char (point-min))))
    ;; Display buffer
    (pop-to-buffer buf '((display-buffer-same-window)))
    ;; Async fetch poster image
    (freebox-image-get
     pic-url
     (lambda (path)
       (when (buffer-live-p buf)
         (freebox-image--insert-poster buf path))))))

(defun freebox-image--insert-metadata (vod)
  "Insert text metadata for VOD into current buffer."
  (let ((name  (freebox-ui--jget vod 'name))
        (note  (freebox-ui--jget vod 'note))
        (actor (freebox-ui--jget vod 'actor)))
    ;; Title
    (insert (propertize (or name "Unknown")
                        'face '(:height 1.3 :weight bold))
            "\n")
    ;; Note (year/rating)
    (when (and note (stringp note) (not (string-empty-p note)))
      (insert (propertize (concat "*" note) 'face 'font-lock-type-face)))
    ;; Actor
    (when (and actor (stringp actor) (not (string-empty-p actor)))
      (insert "  "
              (propertize (truncate-string-to-width actor 60 nil nil "...")
                          'face 'font-lock-function-name-face)))
    (insert "\n")))

(defun freebox-image--insert-poster (buf path)
  "Replace the poster placeholder in BUF with the image at PATH."
  (when (and (buffer-live-p buf) path (file-exists-p path))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (marker freebox-image--poster-marker))
        (when (and marker (marker-position marker))
          (save-excursion
            (goto-char marker)
            ;; Delete placeholder text
            (let ((end (line-end-position)))
              (delete-region marker end))
            ;; Insert image
            (condition-case nil
                (let* ((win (get-buffer-window buf))
                       (max-w (if win
                                  (- (window-body-width win t) 20)
                                600))
                       (img (create-image path nil nil
                                          :max-width max-w
                                          :max-height 500
                                          :ascent 'center)))
                  (insert-image img "[poster]"))
              (error
               (insert (propertize "[image load failed]"
                                   'face 'font-lock-warning-face))))))))))

;;; --- Poster gallery buffer ----------------------------------------------------

(defconst freebox-image-gallery-buffer-name "*freebox-gallery*"
  "Name of the poster gallery buffer.")

(defcustom freebox-image-thumbnail-width 120
  "Width in pixels for gallery thumbnails."
  :type 'integer
  :group 'freebox-image)

(defcustom freebox-image-thumbnail-height 160
  "Max height in pixels for gallery thumbnails."
  :type 'integer
  :group 'freebox-image)

(defvar-local freebox-image--gallery-source-key nil)
(defvar-local freebox-image--gallery-tid nil)
(defvar-local freebox-image--gallery-cat-name nil)
(defvar-local freebox-image--gallery-page nil)

(declare-function freebox-ui-show-detail "freebox-ui")

(defvar freebox-gallery-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET")       #'freebox-gallery-open-detail)
    (define-key map (kbd "n")         #'freebox-gallery-next)
    (define-key map (kbd "p")         #'freebox-gallery-prev)
    (define-key map (kbd "TAB")       #'freebox-gallery-next)
    (define-key map (kbd "<backtab>") #'freebox-gallery-prev)
    map)
  "Keymap for `freebox-gallery-mode'.")

(define-derived-mode freebox-gallery-mode special-mode "FreeBox-Gallery"
  "Major mode for FreeBox poster gallery.
\\<freebox-gallery-mode-map>
\\[freebox-gallery-open-detail] - Open VOD detail
\\[freebox-gallery-next] - Next poster
\\[freebox-gallery-prev] - Previous poster
\\[quit-window] - Close gallery"
  :group 'freebox-image
  (setq-local cursor-type 'box)
  (setq-local truncate-lines t))

(defun freebox-gallery-next ()
  "Move to the next poster in the gallery."
  (interactive)
  (let ((pos (point)))
    ;; If currently on a vod-id region, move past it
    (when (get-text-property pos 'freebox-vod-id)
      (setq pos (or (next-single-property-change pos 'freebox-vod-id) pos)))
    ;; Now find the next region that has vod-id
    (while (and pos (< pos (point-max))
                (not (get-text-property pos 'freebox-vod-id)))
      (setq pos (next-single-property-change pos 'freebox-vod-id)))
    (when (and pos (get-text-property pos 'freebox-vod-id))
      (goto-char pos))))

(defun freebox-gallery-prev ()
  "Move to the previous poster in the gallery."
  (interactive)
  (let ((pos (point)))
    ;; If currently on a vod-id region, move before it
    (when (and (> pos (point-min))
               (get-text-property pos 'freebox-vod-id)
               (get-text-property (1- pos) 'freebox-vod-id))
      ;; Still in the same region, find its start
      (setq pos (previous-single-property-change pos 'freebox-vod-id))
      (setq pos (or pos (point-min))))
    ;; Move before current position
    (when (> pos (point-min))
      (setq pos (previous-single-property-change pos 'freebox-vod-id))
      (when pos
        ;; pos is now end of previous region or start of gap
        ;; Find the start of the vod-id region containing or before pos
        (if (get-text-property (max (1- pos) (point-min)) 'freebox-vod-id)
            (let ((start (previous-single-property-change pos 'freebox-vod-id)))
              (goto-char (or start (point-min))))
          (goto-char pos))))))

(defun freebox-gallery-open-detail ()
  "Open detail page for the poster at point."
  (interactive)
  (let ((vod-id (get-text-property (point) 'freebox-vod-id)))
    (if (not vod-id)
        (message "FreeBox: no poster at point.")
      (quit-window t)
      (freebox-ui-show-detail vod-id))))

(defun freebox-image-show-gallery (items cat-name page pagecount source-key tid)
  "Show a gallery buffer with poster thumbnails for ITEMS.
CAT-NAME, PAGE, PAGECOUNT describe the current category page.
SOURCE-KEY and TID are saved for context.
Thumbnails are arranged in a grid, with multiple posters per row."
  (let ((buf (get-buffer-create freebox-image-gallery-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (freebox-gallery-mode)
        (setq freebox-image--gallery-source-key source-key
              freebox-image--gallery-tid tid
              freebox-image--gallery-cat-name cat-name
              freebox-image--gallery-page page)
        ;; Title
        (insert (propertize (format "%s p.%d/%d (%d items)\n\n"
                                    cat-name page pagecount (length items))
                            'face '(:weight bold :height 1.2)))
        ;; Calculate columns per row
        (let* ((cell-w (+ freebox-image-thumbnail-width 16))
               (win-w (or (and (get-buffer-window buf)
                               (window-body-width (get-buffer-window buf) t))
                          800))
               (cols (max 1 (floor (/ (float win-w) cell-w))))
               (name-chars (max 8 (/ freebox-image-thumbnail-width 8)))
               (col-idx 0))
          ;; Render items in grid: image row then name row per grid-row
          (let ((row-items nil))
            (dolist (v items)
              (push v row-items)
              (cl-incf col-idx)
              (when (= col-idx cols)
                (freebox-image--gallery-insert-row buf (nreverse row-items) name-chars)
                (setq row-items nil col-idx 0)))
            ;; Remaining items in last partial row
            (when row-items
              (freebox-image--gallery-insert-row buf (nreverse row-items) name-chars))))
        ;; Footer
        (insert "\n"
                (propertize (make-string 50 ?─) 'face 'shadow)
                "\n"
                (propertize "[RET] 查看详情  [n/p] 下/上一项  [q] 返回列表"
                            'face 'font-lock-comment-face)
                "\n")
        ;; Move to first poster
        (goto-char (point-min))
        (let ((first (next-single-property-change (point) 'freebox-vod-id)))
          (when first (goto-char first)))))
    (pop-to-buffer buf '((display-buffer-same-window)))))

(defun freebox-image--gallery-insert-row (buf row-items name-chars)
  "Insert one grid row into BUF: a line of thumbnails then a line of names.
ROW-ITEMS is a list of VOD alists.  NAME-CHARS is the max name width in chars."
  (let ((sep "  "))
    ;; Image line: placeholders for each item
    (dolist (v row-items)
      (let* ((vod-id (freebox-ui--jget v 'id))
             (pic    (freebox-ui--jget v 'pic))
             (marker (point-marker)))
        (let ((start (point)))
          (insert (propertize "[loading...]"
                              'face 'shadow
                              'freebox-vod-id vod-id))
          (put-text-property start (point) 'freebox-vod-id vod-id))
        (insert sep)
        ;; Async load thumbnail
        (when (and pic (stringp pic) (not (string-empty-p pic)))
          (let ((m marker) (b buf) (vid vod-id))
            (freebox-image-get
             pic
             (lambda (path)
               (when (and path (buffer-live-p b))
                 (freebox-image--gallery-replace-placeholder b m path vid))))))))
    (insert "\n")
    ;; Name line: truncated names aligned under each thumbnail
    (dolist (v row-items)
      (let* ((name (or (freebox-ui--jget v 'name) "?"))
             (label (truncate-string-to-width name name-chars nil nil "..")))
        (insert (propertize (format (format "%%-%ds" (+ name-chars 2)) label)
                            'face 'font-lock-keyword-face))))
    (insert "\n\n")))

(defun freebox-image--gallery-replace-placeholder (buf marker path vod-id)
  "In BUF, replace the placeholder at MARKER with a thumbnail image from PATH."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char marker)
        (when (get-text-property marker 'freebox-vod-id)
          ;; Delete only the placeholder region, not the whole line
          (let ((end (next-single-property-change marker 'freebox-vod-id
                                                  nil (line-end-position))))
            (delete-region marker end))
          (condition-case nil
              (let ((img (create-image path nil nil
                                       :max-width freebox-image-thumbnail-width
                                       :max-height freebox-image-thumbnail-height
                                       :ascent 'center)))
                (let ((start (point)))
                  (insert-image img "[poster]")
                  (put-text-property start (point) 'freebox-vod-id vod-id)))
            (error
             (let ((start (point)))
               (insert (propertize "[no image]" 'face 'font-lock-warning-face))
               (put-text-property start (point) 'freebox-vod-id vod-id)))))))))
(provide 'freebox-image)
;;; freebox-image.el ends here
