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

(defvar-local freebox-image--gallery-context nil
  "Gallery context for returning from poster detail: (source-key tid cat-name page).")

(declare-function freebox-ui--select-episode "freebox-ui")
(declare-function freebox-ui--jget "freebox-ui")
(declare-function freebox-ui--category-page "freebox-ui")
(declare-function hydra-keyboard-quit "hydra")

(defvar freebox-image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'freebox-image-select-episode)
    (define-key map (kbd "p")   #'freebox-image-select-episode)
    (define-key map (kbd "b")   #'freebox-image-return-to-gallery)
    (define-key map (kbd "v")   #'freebox-image-goto-vod-list)
    map)
  "Keymap for `freebox-image-mode'.")

(define-derived-mode freebox-image-mode special-mode "FreeBox-Poster"
  "Major mode for FreeBox poster preview.
\\<freebox-image-mode-map>
\\[freebox-image-select-episode] - Select episode and play
\\[freebox-image-return-to-gallery] - Return to gallery (if available)
\\[quit-window] - Close preview"
  :group 'freebox-image
  (setq-local cursor-type nil)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun freebox-image-select-episode ()
  "Enter episode selection, keeping the poster buffer visible."
  (interactive)
  (let ((vod freebox-image--vod)
        (vod-id freebox-image--vod-id))
    (when (and vod vod-id)
      (freebox-ui--select-episode vod vod-id))))

(defun freebox-image-return-to-gallery ()
  "Return to the gallery buffer if this poster was opened from a gallery."
  (interactive)
  (let ((ctx freebox-image--gallery-context))
    (if (not ctx)
        (message "FreeBox: gallery context not available. Use [q] to close.")
      (pcase-let ((`(,source-key ,tid ,cat-name ,page) ctx))
        (quit-window t)
        (freebox-ui--category-page-gallery source-key tid cat-name page)))))

(defun freebox-image-goto-vod-list ()
  "Go back to the vod-list (category page) from poster detail."
  (interactive)
  (let ((ctx freebox-image--gallery-context))
    (if (not ctx)
        (message "FreeBox: no navigation context available.")
      (pcase-let ((`(,source-key ,tid ,cat-name ,page) ctx))
        (quit-window t)
        (freebox-ui--category-page source-key tid cat-name page)))))

(defun freebox-image-show-poster (vod vod-id pic-url &optional gallery-context)
  "Show poster preview buffer for VOD with VOD-ID.
PIC-URL is the poster image URL.  Text metadata is shown immediately;
the image is loaded asynchronously and inserted when ready.

GALLERY-CONTEXT, if provided, is a list (SOURCE-KEY TID CAT-NAME PAGE)
saved to allow returning to the gallery."
  ;; Dismiss hydra if active
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (let ((buf (get-buffer-create freebox-image-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (freebox-image-mode)
        ;; Save state
        (setq freebox-image--vod vod
              freebox-image--vod-id vod-id
              freebox-image--gallery-context gallery-context)
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
                (propertize (if gallery-context
                               "[RET/p] 选择剧集   [b] 返回画廊   [v] 列表   [q] 返回"
                             "[RET/p] 选择剧集   [q] 返回")
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
(defvar-local freebox-image--gallery-pagecount nil)

(declare-function freebox-ui-show-detail "freebox-ui")
(declare-function freebox-ui--category-page "freebox-ui")
(declare-function freebox-ui--category-page-gallery "freebox-ui")

(defvar freebox-gallery-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET")       #'freebox-gallery-open-detail)
    (define-key map (kbd "j")         #'freebox-gallery-next)
    (define-key map (kbd "k")         #'freebox-gallery-prev)
    (define-key map (kbd "TAB")       #'freebox-gallery-next)
    (define-key map (kbd "<backtab>") #'freebox-gallery-prev)
    (define-key map (kbd "n")         #'freebox-gallery-next-page)
    (define-key map (kbd "p")         #'freebox-gallery-prev-page)
    (define-key map (kbd "v")         #'freebox-gallery-goto-vod-list)
    map)
  "Keymap for `freebox-gallery-mode'.")

(define-derived-mode freebox-gallery-mode special-mode "FreeBox-Gallery"
  "Major mode for FreeBox poster gallery.
\\<freebox-gallery-mode-map>
\\[freebox-gallery-open-detail] - Open VOD detail
\\[freebox-gallery-next] - Next poster
\\[freebox-gallery-prev] - Previous poster
\\[freebox-gallery-next-page] - Next page
\\[freebox-gallery-prev-page] - Previous page
\\[quit-window] - Close gallery"
  :group 'freebox-image
  (setq-local cursor-type 'box)
  (setq-local truncate-lines t))

(defun freebox-gallery-next ()
  "Move to the next poster in the gallery."
  (interactive)
  (let ((pos (point)))
    (when (get-text-property pos 'freebox-vod-id)
      (setq pos (or (next-single-property-change pos 'freebox-vod-id) pos)))
    (while (and pos (< pos (point-max))
                (not (get-text-property pos 'freebox-vod-id)))
      (setq pos (next-single-property-change pos 'freebox-vod-id)))
    (when (and pos (get-text-property pos 'freebox-vod-id))
      (goto-char pos))))

(defun freebox-gallery-prev ()
  "Move to the previous poster in the gallery."
  (interactive)
  (let ((pos (point)))
    (when (and (> pos (point-min))
               (get-text-property pos 'freebox-vod-id)
               (get-text-property (1- pos) 'freebox-vod-id))
      (setq pos (previous-single-property-change pos 'freebox-vod-id))
      (setq pos (or pos (point-min))))
    (when (> pos (point-min))
      (setq pos (previous-single-property-change pos 'freebox-vod-id))
      (when pos
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
      ;; Save gallery buffer instead of closing it, so we can return to it
      (bury-buffer (current-buffer))
      ;; Open detail with gallery context
      (freebox-ui-show-detail vod-id
                              (list freebox-image--gallery-source-key
                                    freebox-image--gallery-tid
                                    freebox-image--gallery-cat-name
                                    freebox-image--gallery-page)))))

(defun freebox-gallery-next-page ()
  "Load the next page in gallery view."
  (interactive)
  (let ((page freebox-image--gallery-page)
        (pagecount freebox-image--gallery-pagecount)
        (source-key freebox-image--gallery-source-key)
        (tid freebox-image--gallery-tid)
        (cat-name freebox-image--gallery-cat-name))
    (if (and page pagecount (< page pagecount))
        (progn
          (quit-window t)
          (freebox-ui--category-page-gallery
           source-key tid cat-name (1+ page)))
      (message "FreeBox: already at last page."))))

(defun freebox-gallery-prev-page ()
  "Load the previous page in gallery view."
  (interactive)
  (let ((page freebox-image--gallery-page)
        (source-key freebox-image--gallery-source-key)
        (tid freebox-image--gallery-tid)
        (cat-name freebox-image--gallery-cat-name))
    (if (and page (> page 1))
        (progn
          (quit-window t)
          (freebox-ui--category-page-gallery
           source-key tid cat-name (1- page)))
      (message "FreeBox: already at first page."))))

(defun freebox-gallery-goto-vod-list ()
  "Go back to the vod-list (category page) from gallery."
  (interactive)
  (let ((source-key freebox-image--gallery-source-key)
        (tid freebox-image--gallery-tid)
        (cat-name freebox-image--gallery-cat-name)
        (page freebox-image--gallery-page))
    (quit-window t)
    (freebox-ui--category-page source-key tid cat-name page)))

(defun freebox-image-show-gallery (items cat-name page pagecount source-key tid)
  "Show a gallery buffer with poster thumbnails for ITEMS.
CAT-NAME, PAGE, PAGECOUNT describe the current category page.
SOURCE-KEY and TID are saved for context.
Thumbnails are arranged in a grid, with multiple posters per row.
Each cell shows the poster above its truncated title."
  ;; Dismiss hydra if active, so its keybindings don't shadow gallery keys
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (let ((buf (get-buffer-create freebox-image-gallery-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (freebox-gallery-mode)
        (setq freebox-image--gallery-source-key source-key
              freebox-image--gallery-tid tid
              freebox-image--gallery-cat-name cat-name
              freebox-image--gallery-page page
              freebox-image--gallery-pagecount pagecount)
        ;; Title
        (insert (propertize (format "%s p.%d/%d (%d items)\n\n"
                                    cat-name page pagecount (length items))
                            'face '(:weight bold :height 1.2)))
        ;; Calculate columns per row
        ;; Build grid rows and collect pending (uncached) items
        (let* ((cell-px (+ freebox-image-thumbnail-width 16))
               (win-px (or (and (get-buffer-window buf)
                                (window-body-width (get-buffer-window buf) t))
                           800))
               (cols (max 1 (floor (/ (float win-px) cell-px))))
               (col-idx 0)
               (row-items nil)
               (all-pending nil))
          (dolist (v items)
            (push (cons col-idx v) row-items)
            (cl-incf col-idx)
            (when (= col-idx cols)
              (push (freebox-image--gallery-insert-row buf (nreverse row-items) cell-px)
                    all-pending)
              (setq row-items nil col-idx 0)))
          (when row-items
            (push (freebox-image--gallery-insert-row buf (nreverse row-items) cell-px)
                  all-pending))
          ;; Async refresh: download uncached posters and replace placeholders
          (dolist (p (apply #'nconc (nreverse all-pending)))
            (pcase-let ((`(,pic-url ,marker ,vod-id) p))
              (freebox-image-get
               pic-url
               (lambda (path)
                 (when (and path (buffer-live-p buf)
                            (marker-position marker))
                   (freebox-image--gallery-replace-placeholder
                    buf marker path vod-id)))))))
        ;; Footer
        (insert "\n"
                (propertize (make-string 50 ?─) 'face 'shadow)
                "\n"
                (propertize "[RET] 查看详情  [j/k] 下/上一项  [n/p] 下/上一页  [v] 列表  [q] 返回"
                            'face 'font-lock-comment-face)
                "\n")
        ;; Move to first poster
        (goto-char (point-min))
        (let ((first (next-single-property-change (point) 'freebox-vod-id)))
          (when first (goto-char first)))))
    (pop-to-buffer buf '((display-buffer-same-window)))))

(defun freebox-image--gallery-replace-placeholder (buf marker path vod-id)
  "Replace the [no image] placeholder at MARKER in BUF with thumbnail at PATH.
VOD-ID is set as the `freebox-vod-id' text property on the inserted image."
  (when (and (buffer-live-p buf) (marker-position marker)
             path (file-exists-p path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          ;; Delete placeholder text up to the next vod-id property or line end
          (let ((end (or (next-single-property-change (point) 'freebox-vod-id)
                         (line-end-position))))
            (delete-region (point) end))
          ;; Insert thumbnail
          (condition-case nil
              (let ((img (create-image path nil nil
                                       :max-width freebox-image-thumbnail-width
                                       :max-height freebox-image-thumbnail-height
                                       :ascent 'center)))
                (insert-image img "[poster]")
                (put-text-property marker (point) 'freebox-vod-id vod-id))
            (error nil)))
        ;; Force window redisplay
        (let ((win (get-buffer-window buf)))
          (when win
            (with-selected-window win
              (redisplay t))))))))

(defun freebox-image--gallery-insert-row (_buf indexed-items cell-px)
  "Insert one grid row: poster line then name line, pixel-aligned.
INDEXED-ITEMS is a list of (IDX . VOD-ALIST) conses.
CELL-PX is the cell width in pixels.
Images are inserted synchronously from cache; uncached items show text.
Returns a list of (PIC-URL MARKER VOD-ID) for uncached items."
  (let ((name-chars (max 6 (/ (- cell-px 16) 8)))
        (col 0)
        (pending nil))
    ;; Poster line
    (dolist (entry indexed-items)
      (let* ((v      (cdr entry))
             (vod-id (freebox-ui--jget v 'id))
             (pic    (freebox-ui--jget v 'pic))
             (px-offset (* col cell-px))
             (cache-path (and pic (stringp pic) (not (string-empty-p pic))
                              (freebox-image--cache-path pic)))
             (cached (and cache-path (freebox-image--cache-fresh-p cache-path))))
        ;; Pixel-align to column
        (when (> col 0)
          (insert (propertize " " 'display `(space :align-to (,px-offset)))))
        (let ((start (point)))
          (if cached
              ;; Insert image directly from cache
              (condition-case nil
                  (let ((img (create-image cache-path nil nil
                                           :max-width freebox-image-thumbnail-width
                                           :max-height freebox-image-thumbnail-height
                                           :ascent 'center)))
                    (insert-image img "[poster]"))
                (error (insert "[no image]")))
            ;; No cache: show text placeholder, record for async refresh
            (insert (propertize "[no image]" 'face 'shadow))
            (when pic
              (push (list pic (copy-marker start) vod-id) pending)))
          (put-text-property start (point) 'freebox-vod-id vod-id)))
      (cl-incf col))
    (insert "\n")
    ;; Name line (pixel-aligned under each poster)
    (setq col 0)
    (dolist (entry indexed-items)
      (let* ((v (cdr entry))
             (name (or (freebox-ui--jget v 'name) "?"))
             (label (truncate-string-to-width name name-chars nil nil ".."))
             (px-offset (* col cell-px)))
        (when (> col 0)
          (insert (propertize " " 'display `(space :align-to (,px-offset)))))
        (insert (propertize label 'face 'font-lock-keyword-face)))
      (cl-incf col))
    (insert "\n\n")
    pending))

(provide 'freebox-image)
;;; freebox-image.el ends here
