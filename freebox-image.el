;;; freebox-image.el --- Image cache and poster preview for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Async image download, local caching, and poster preview buffer.
;; Uses `url-retrieve' for binary downloads (not `request.el' which assumes JSON).
;; Cache: ~/.freebox/cache/posters/{md5(url)}.img
;; Preview buffer: *freebox-poster* with special-mode keybindings.

;;; Code:

(require 'url)
(require 'cl-lib)

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

(defvar freebox-image--inflight nil
  "Alist of (URL . CALLBACKS) for downloads currently in progress.
Prevents re-rendering the gallery from starting a second download of a
URL that is already being fetched.")

(defun freebox-image-get (url callback)
  "Asynchronously fetch the image at URL, cache it, call CALLBACK with path.
CALLBACK is called with (PATH-OR-NIL).  PATH is the local cache file on
success, nil on failure.  If a fresh cache exists, CALLBACK is called
immediately with the cached path.  If the same URL is already being
downloaded, CALLBACK is queued and run when that download finishes."
  (freebox-image--ensure-cache-dir)
  (let ((cache-path (freebox-image--cache-path url)))
    (cond
     ((freebox-image--cache-fresh-p cache-path)
      (funcall callback cache-path))
     ((assoc url freebox-image--inflight)
      (setf (alist-get url freebox-image--inflight)
            (append (alist-get url freebox-image--inflight) (list callback))))
     (t
      (push (cons url (list callback)) freebox-image--inflight)
      (let ((url-request-extra-headers
             '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) Emacs"))))
        (cl-labels
            ((finish (path)
               (let ((cbs (alist-get url freebox-image--inflight)))
                 (setq freebox-image--inflight
                       (assq-delete-all url freebox-image--inflight))
                 (dolist (cb cbs) (funcall cb path)))))
          (condition-case _err
              (url-retrieve
               url
               (lambda (status _cb-url cb-path _cb-callback)
                 (let ((path
                        (if (or (plist-get status :error)
                                (not (buffer-live-p (current-buffer))))
                            nil
                          (condition-case nil
                              (progn
                                ;; Skip HTTP headers
                                (goto-char (point-min))
                                (when (re-search-forward "\r?\n\r?\n" nil t)
                                  (let ((data (buffer-substring-no-properties
                                               (point) (point-max))))
                                    (if (< (length data) 100)
                                        ;; Too small, likely an error page
                                        nil
                                      (with-temp-file cb-path
                                        (set-buffer-multibyte nil)
                                        (insert data))
                                      cb-path))))
                            (error nil)))))
                   (when (buffer-live-p (current-buffer))
                     (kill-buffer (current-buffer)))
                   (finish path)))
               (list url cache-path nil)
               t  ; silent
               t) ; inhibit cookies
            (error (finish nil)))))))))

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

(defvar-local freebox-image--cached-path nil
  "Cached file path for the current poster image, used for refit on resize.")

(defvar-local freebox-image--last-width nil
  "Last known window pixel width for poster buffer, to avoid redundant refit.")

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
      ;; Save path for refit on window resize
      (setq freebox-image--cached-path path)
      (freebox-image--insert-poster-at-point path buf)
      ;; Record width for refit comparison
      (let ((win (get-buffer-window buf)))
        (setq freebox-image--last-width
              (and win (window-body-width win t)))))))

(defun freebox-image--insert-poster-at-point (path buf)
  "Insert poster image at `freebox-image--poster-marker' in BUF using PATH.
Replaces any existing content on that line (placeholder or old image)."
  (let ((inhibit-read-only t)
        (marker freebox-image--poster-marker))
    (when (and marker (marker-position marker))
      (save-excursion
        (goto-char marker)
        (let ((end (line-end-position)))
          (delete-region marker end))
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
                               'face 'font-lock-warning-face))))))))

(defun freebox-image--poster-refit (frame)
  "Re-insert poster image when window width changes for FRAME.
Scans all windows on FRAME for a displayed *freebox-poster* buffer
and refits its image if the width changed.
This is a global hook, so it works even when the poster buffer
is not the current buffer (e.g. after C-x 4 b)."
  (dolist (win (window-list frame))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (eq (buffer-local-value 'major-mode buf) 'freebox-image-mode)
                 (buffer-local-value 'freebox-image--cached-path buf))
        (let ((win-px (window-body-width win t))
              (last-w (buffer-local-value 'freebox-image--last-width buf)))
          (unless (equal win-px last-w)
            (with-current-buffer buf
              (setq freebox-image--last-width win-px)
              (freebox-image--insert-poster-at-point
               freebox-image--cached-path buf))))))))

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

(defcustom freebox-gallery-selection-scale 1.45
  "Scale factor applied to the poster of the selected gallery cell.
The selected poster is drawn this much larger than the other
thumbnails, so the current cell stands out like a dock icon."
  :type 'number
  :group 'freebox-image)

(defface freebox-gallery-selection
  '((((background dark)) :box (:line-width 3 :color "#79b8ff"))
    (t :box (:line-width 3 :color "#2f6fdd")))
  "Face for the poster frame of the selected gallery cell."
  :group 'freebox-image)

(defface freebox-gallery-selection-label
  '((((background dark)) :background "#79b8ff" :foreground "black" :weight bold)
    (t :background "#2f6fdd" :foreground "white" :weight bold))
  "Face for the name of the selected gallery cell."
  :group 'freebox-image)

(defface freebox-gallery-missing-poster
  '((((background dark)) :background "#3a3a3a" :foreground "#8a8a8a")
    (t :background "#e8e8e8" :foreground "#7a7a7a"))
  "Face for the placeholder shown when a poster cannot be loaded."
  :group 'freebox-image)

(defvar-local freebox-image--gallery-source-key nil)
(defvar-local freebox-image--gallery-tid nil)
(defvar-local freebox-image--gallery-cat-name nil)
(defvar-local freebox-image--gallery-page nil)
(defvar-local freebox-image--gallery-pagecount nil)
(defvar-local freebox-image--gallery-items nil
  "Items data for current gallery page, used for refit on window resize.")
(defvar-local freebox-image--gallery-last-width nil
  "Last known window pixel width, to avoid redundant refit.")
(defvar-local freebox-image--gallery-selected nil
  "VOD id of the poster currently selected in the gallery.")

(declare-function freebox-ui-show-detail "freebox-ui")
(declare-function freebox-ui--category-page "freebox-ui")
(declare-function freebox-ui--category-page-gallery "freebox-ui")

(defvar freebox-gallery-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET")       #'freebox-gallery-open-detail)
    (define-key map (kbd "h")         #'freebox-gallery-left)
    (define-key map (kbd "j")         #'freebox-gallery-down)
    (define-key map (kbd "k")         #'freebox-gallery-up)
    (define-key map (kbd "l")         #'freebox-gallery-right)
    (define-key map (kbd "TAB")       #'freebox-gallery-right)
    (define-key map (kbd "<backtab>") #'freebox-gallery-left)
    (define-key map (kbd "n")         #'freebox-gallery-next-page)
    (define-key map (kbd "p")         #'freebox-gallery-prev-page)
    (define-key map (kbd "r")         #'freebox-gallery-reload-poster)
    (define-key map (kbd "v")         #'freebox-gallery-goto-vod-list)
    map)
  "Keymap for `freebox-gallery-mode'.")

(define-derived-mode freebox-gallery-mode special-mode "FreeBox-Gallery"
  "Major mode for FreeBox poster gallery.
\\<freebox-gallery-mode-map>
\\[freebox-gallery-open-detail] - Open VOD detail
\\[freebox-gallery-left] - Previous poster (left)
\\[freebox-gallery-right] - Next poster (right)
\\[freebox-gallery-up] - Previous row
\\[freebox-gallery-down] - Next row
\\[freebox-gallery-next-page] - Next page
\\[freebox-gallery-prev-page] - Previous page
\\[freebox-gallery-reload-poster] - Reload the poster at point
\\[quit-window] - Close gallery"
  :group 'freebox-image
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (add-hook 'post-command-hook #'freebox-gallery--sync-selection nil t))

(defun freebox-image--gallery-layout (items win-px selected)
  "Lay ITEMS out into rows for a text area WIN-PX pixels wide.
SELECTED is the VOD id drawn enlarged.  Returns a list of rows,
each a list of (OFFSET ITEM SELECTED-P CELL-PX) in display order."
  (let* ((base-w freebox-image-thumbnail-width)
         (sel-w (round (* base-w freebox-gallery-selection-scale)))
         (base-cell (+ base-w 16))
         (sel-cell (+ sel-w 16))
         (rows nil)
         (row nil)
         (used 0)
         (off 0))
    (dolist (v items)
      (let* ((id (freebox-ui--jget v 'id))
             (selp (and selected (equal id selected)))
             (cell (if selp sel-cell base-cell)))
        (when (and row (> (+ used cell) win-px))
          (push (nreverse row) rows)
          (setq row nil used 0 off 0))
        (push (list off v selp cell) row)
        (cl-incf used cell)
        (cl-incf off cell)))
    (when row (push (nreverse row) rows))
    (nreverse rows)))

(defun freebox-image--gallery-render (buf)
  "Render the gallery in BUF from its buffer-local state.
Point is left on the selected poster when there is one."
  (with-current-buffer buf
    (let ((items freebox-image--gallery-items)
          (cat-name freebox-image--gallery-cat-name)
          (page freebox-image--gallery-page)
          (pagecount freebox-image--gallery-pagecount)
          (selected freebox-image--gallery-selected)
          (inhibit-read-only t)
          (pending nil))
      (erase-buffer)
      ;; Title
      (insert (propertize (format "%s p.%d/%d (%d items)\n\n"
                                  cat-name page pagecount (length items))
                          'face '(:weight bold :height 1.2)))
      ;; Grid
      (let* ((win (get-buffer-window buf))
             (win-px (or (and win (window-body-width win t)) 800)))
        (dolist (row (freebox-image--gallery-layout items win-px selected))
          (setq pending (nconc pending
                               (freebox-image--gallery-insert-row row))))
        (setq freebox-image--gallery-last-width win-px))
      ;; Footer
      (insert "\n"
              (propertize (make-string 50 ?─) 'face 'shadow)
              "\n"
              (propertize "[RET] 查看详情  [h/j/k/l] 导航  [n/p] 翻页  [r] 重载海报  [v] 列表  [q] 返回"
                          'face 'font-lock-comment-face)
              "\n")
      ;; Put point back on the selected poster
      (let ((pos (and selected (freebox-image--gallery-pos-of selected))))
        (if pos
            (goto-char pos)
          (goto-char (point-min))
          (let ((first (next-single-property-change (point) 'freebox-vod-id)))
            (when first (goto-char first)))))
      ;; Async refresh: download uncached posters and replace placeholders
      (dolist (p pending)
        (pcase-let ((`(,pic-url ,marker ,vod-id) p))
          (freebox-image-get
           pic-url
           (lambda (path)
             (when (and path (buffer-live-p buf)
                        (marker-position marker))
               (freebox-image--gallery-replace-placeholder
                buf marker path vod-id)))))))))

(defun freebox-image--gallery-pos-of (vod-id)
  "Return the buffer position of the poster cell for VOD-ID, or nil."
  (let ((pos (point-min))
        (found nil))
    (save-excursion
      (while (and (not found)
                  (setq pos (next-single-property-change pos 'freebox-vod-id)))
        (when (equal (get-text-property pos 'freebox-vod-id) vod-id)
          (setq found pos))))
    found))

(defun freebox-image--gallery-rerender ()
  "Re-render the gallery using saved items and current window width."
  (freebox-image--gallery-render (current-buffer)))

(defun freebox-gallery--sync-selection ()
  "Re-render when point moved to a different poster."
  (let ((id (get-text-property (point) 'freebox-vod-id)))
    (when (and id (not (equal id freebox-image--gallery-selected)))
      (setq freebox-image--gallery-selected id)
      (freebox-image--gallery-render (current-buffer)))))

(defun freebox-gallery-reload-poster ()
  "Discard the cached poster at point and fetch it again.
Useful when a poster failed to download; failures are usually caused
by the upstream image host, so a reload only helps for transient errors."
  (interactive)
  (let* ((id (get-text-property (point) 'freebox-vod-id))
         (item (and id
                    (cl-find-if (lambda (v)
                                  (equal (freebox-ui--jget v 'id) id))
                                freebox-image--gallery-items)))
         (pic (and item (freebox-ui--jget item 'pic))))
    (cond
     ((not id) (message "FreeBox: no poster at point."))
     ((not (and pic (stringp pic) (not (string-empty-p pic))))
      (message "FreeBox: this item has no poster URL."))
     (t
      (let ((cache (freebox-image--cache-path pic)))
        (when (file-exists-p cache) (delete-file cache)))
      (message "FreeBox: reloading poster...")
      (freebox-image--gallery-render (current-buffer))))))


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

(defun freebox-gallery--current-vod-start ()
  "Return start position of the vod-id property region at or after point.
Returns nil if no vod-id is found."
  (let ((pos (point)))
    (cond
     ((get-text-property pos 'freebox-vod-id)
      ;; Walk backwards to the exact first character of the property region.
      (let ((start pos))
        (while (and (> start (point-min))
                    (get-text-property (1- start) 'freebox-vod-id))
          (setq start (1- start)))
        start))
     (t
      (let ((next (next-single-property-change pos 'freebox-vod-id)))
        (and next (get-text-property next 'freebox-vod-id) next))))))

(defun freebox-gallery--vod-starts-in-line (line-start)
  "Return sorted list of vod-id start positions in line starting at LINE-START."
  (let ((result nil)
        (in-prop nil))
    (save-excursion
      (goto-char line-start)
      (let ((end (line-end-position)))
        (while (< (point) end)
          (let ((has-prop (get-text-property (point) 'freebox-vod-id)))
            (when (and has-prop (not in-prop))
              (push (point) result))
            (setq in-prop has-prop))
          (forward-char 1))))
    (nreverse result)))

(defun freebox-gallery--index-of (item list)
  "Return 0-based index of ITEM in LIST, or nil."
  (let ((idx 0))
    (catch 'found
      (dolist (el list)
        (when (eql el item)
          (throw 'found idx))
        (setq idx (1+ idx)))
      nil)))

(defun freebox-gallery--col-index-at-pos (pos)
  "Return 0-based column index of vod-id at POS within its line."
  (let* ((line-start (save-excursion (goto-char pos) (line-beginning-position)))
         (starts (freebox-gallery--vod-starts-in-line line-start)))
    (freebox-gallery--index-of pos starts)))

(defun freebox-gallery--find-poster-row (direction)
  "Find the next or previous line containing vod-id properties.
DIRECTION is `next' or `prev'.
Returns the line-beginning-position of that row, or nil."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (if (eq direction 'next)
                      (zerop (forward-line 1))
                    (and (zerop (forward-line -1))
                         (>= (point) (point-min)))))
        (when (freebox-gallery--vod-starts-in-line (line-beginning-position))
          (setq found (line-beginning-position))))
      found)))

(defun freebox-gallery-left ()
  "Move to the previous poster in the same row."
  (interactive)
  (let ((start (freebox-gallery--current-vod-start)))
    (if (not start)
        (message "FreeBox: no poster here.")
      (let* ((line-start (save-excursion (goto-char start) (line-beginning-position)))
             (starts (freebox-gallery--vod-starts-in-line line-start))
             (idx (freebox-gallery--index-of start starts)))
        (if (and idx (> idx 0))
            (goto-char (nth (1- idx) starts))
          (message "FreeBox: already at first column."))))))

(defun freebox-gallery-right ()
  "Move to the next poster in the same row."
  (interactive)
  (let ((start (freebox-gallery--current-vod-start)))
    (if (not start)
        (message "FreeBox: no poster here.")
      (let* ((line-start (save-excursion (goto-char start) (line-beginning-position)))
             (starts (freebox-gallery--vod-starts-in-line line-start))
             (idx (freebox-gallery--index-of start starts)))
        (if (and idx (< (1+ idx) (length starts)))
            (goto-char (nth (1+ idx) starts))
          (message "FreeBox: already at last column."))))))

(defun freebox-gallery-up ()
  "Move to the poster in the same column of the previous row."
  (interactive)
  (let* ((start (freebox-gallery--current-vod-start))
         (idx (and start (freebox-gallery--col-index-at-pos start)))
         (prev-row (and start (freebox-gallery--find-poster-row 'prev))))
    (if (not (and idx prev-row))
        (message "FreeBox: already at first row.")
      (let ((prev-starts (freebox-gallery--vod-starts-in-line prev-row)))
        (if (< idx (length prev-starts))
            (goto-char (nth idx prev-starts))
          (message "FreeBox: no poster in that column."))))))

(defun freebox-gallery-down ()
  "Move to the poster in the same column of the next row."
  (interactive)
  (let* ((start (freebox-gallery--current-vod-start))
         (idx (and start (freebox-gallery--col-index-at-pos start)))
         (next-row (and start (freebox-gallery--find-poster-row 'next))))
    (if (not (and idx next-row))
        (message "FreeBox: already at last row.")
      (let ((next-starts (freebox-gallery--vod-starts-in-line next-row)))
        (if (< idx (length next-starts))
            (goto-char (nth idx next-starts))
          (message "FreeBox: no poster in that column."))))))

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
Each cell shows the poster above its title.  The selected cell is
enlarged and its title is shown in full, wrapping if necessary."
  ;; Dismiss hydra if active, so its keybindings don't shadow gallery keys
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (let ((buf (get-buffer-create freebox-image-gallery-buffer-name)))
    ;; Display buffer FIRST so window-body-width is accurate for column calculation
    (pop-to-buffer buf '((display-buffer-same-window)))
    (with-current-buffer buf
      (freebox-gallery-mode)
      (setq freebox-image--gallery-source-key source-key
            freebox-image--gallery-tid tid
            freebox-image--gallery-cat-name cat-name
            freebox-image--gallery-page page
            freebox-image--gallery-pagecount pagecount
            freebox-image--gallery-items items
            freebox-image--gallery-selected
            (and items (freebox-ui--jget (car items) 'id)))
      (freebox-image--gallery-render buf))))

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

(defun freebox-image--gallery-insert-row (entries)
  "Insert one grid row: poster line then name line, pixel-aligned.
ENTRIES is a list of (OFFSET ITEM SELECTED-P CELL-PX), as produced by
`freebox-image--gallery-layout'.  The selected cell is drawn enlarged,
framed, and its name is wrapped so the full title is visible.
Images are inserted synchronously from cache; uncached items show a
placeholder.  Returns a list of (PIC-URL MARKER VOD-ID) for uncached
items."
  (let ((pending nil))
    ;; Poster line
    (dolist (entry entries)
      (pcase-let ((`(,off ,v ,selp ,cell-px) entry))
        (let* ((vod-id (freebox-ui--jget v 'id))
               (pic    (freebox-ui--jget v 'pic))
               (cache-path (and pic (stringp pic) (not (string-empty-p pic))
                                (freebox-image--cache-path pic)))
               (cached (and cache-path (freebox-image--cache-fresh-p cache-path)))
               (max-w (if selp
                          (round (* freebox-image-thumbnail-width
                                    freebox-gallery-selection-scale))
                        freebox-image-thumbnail-width))
               (max-h (if selp
                          (round (* freebox-image-thumbnail-height
                                    freebox-gallery-selection-scale))
                        freebox-image-thumbnail-height)))
          (when (> off 0)
            (insert (propertize " " 'display `(space :align-to (,off)))))
          (let ((start (point)))
            (if cached
                (condition-case nil
                    (let ((img (create-image cache-path nil nil
                                             :max-width max-w
                                             :max-height max-h
                                             :ascent 'center)))
                      (insert-image img "[poster]"))
                  (error (insert (propertize "[no image]"
                                             'face 'freebox-gallery-missing-poster))))
              ;; No cache: show placeholder, record for async refresh
              (insert (propertize
                       (truncate-string-to-width
                        (or (freebox-ui--jget v 'name) "?")
                        (max 4 (/ cell-px 8)) nil nil "..")
                       'face 'freebox-gallery-missing-poster
                       'help-echo "海报加载失败，按 r 重试"))
              (when pic
                (push (list pic (copy-marker start) vod-id) pending)))
            (put-text-property start (point) 'freebox-vod-id vod-id)
            (when selp
              (let ((ov (make-overlay start (point))))
                (overlay-put ov 'face 'freebox-gallery-selection)))))))
    (insert "\n")
    ;; Name line (pixel-aligned under each poster).  The selected cell
    ;; gets the wider budget of its enlarged cell so its title usually
    ;; fits without truncation.
    (let ((overflow nil))
      (dolist (entry entries)
        (pcase-let ((`(,off ,v ,selp ,cell-px) entry))
          (let* ((name (or (freebox-ui--jget v 'name) "?"))
                 (budget (max 6 (/ (- cell-px 16) 8)))
                 (label (truncate-string-to-width name budget nil nil "..")))
            (when (> off 0)
              (insert (propertize " " 'display `(space :align-to (,off)))))
            (if selp
                (progn
                  (insert (propertize label 'face 'freebox-gallery-selection-label))
                  (when (string-match-p "\\.\\.\\'" label)
                    (setq overflow name)))
              (insert (propertize label 'face 'font-lock-keyword-face))))))
      (insert "\n")
      ;; Long selected title: show it in full on its own wrapped line.
      (when overflow
        (let ((start (point)))
          (insert (propertize overflow 'face 'freebox-gallery-selection-label))
          (fill-region start (point) nil nil t)
          (insert "\n"))))
    (insert "\n")
    pending))

;;; --- Global window-size-change hook ------------------------------------------

;; Register global refit for poster and gallery buffers.
;; This is necessary because buffer-local `window-size-change-functions'
;; only fire when the buffer is current, so after C-x 4 b the poster
;; buffer never gets notified.
(add-hook 'window-size-change-functions #'freebox-image--poster-refit)
(add-hook 'window-size-change-functions #'freebox-gallery--refit-global)

(defun freebox-gallery--refit-global (frame)
  "Global version of gallery refit for FRAME.
Scans all windows on FRAME for a *freebox-gallery* buffer
and refits if width changed."
  (dolist (win (window-list frame))
    (let ((buf (window-buffer win)))
      (when (and (buffer-live-p buf)
                 (eq (buffer-local-value 'major-mode buf) 'freebox-gallery-mode)
                 (buffer-local-value 'freebox-image--gallery-items buf))
        (let ((win-px (window-body-width win t))
              (last-w (buffer-local-value 'freebox-image--gallery-last-width buf)))
          (unless (equal win-px last-w)
            (with-current-buffer buf
              (setq freebox-image--gallery-last-width win-px)
              (freebox-image--gallery-rerender))))))))

(provide 'freebox-image)
;;; freebox-image.el ends here
