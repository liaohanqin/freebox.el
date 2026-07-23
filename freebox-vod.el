;;; freebox-vod.el --- VOD tree browser for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-style VOD browser (`*freebox-vod*' buffer).
;; Replaces the old completing-read chain (category -> vod -> flag -> episode)
;; with a single foldable, lazily loaded tree:
;;
;;   FreeBox 点播 [源名]
;;   ──────────────────────────────────────────
;;   ▾ 搜索: 关键词 (3)              <- search pseudo-group (via / or s)
;;     ▾ 影片X
;;         ▸ 夸克线路
;;   ▾ 国产剧 (12)
;;     ▾ 影片A (2024·更新至12集)
;;         ▾ 夸克线路 (12集)
;;             第01集
;;             第02集
;;         ▸ UC线路 ⇢               <- RESOLVE type, resolve-share on expand
;;     ▸ 影片B
;;     -- 加载更多 (p.1/5) --
;;   ▸ 电影
;;
;; Keys: RET expand/play, TAB fold, j/k/n move, g refresh, s switch source,
;;       / search, p poster, V gallery, q quit.
;;
;; Lazy loading: expanding an unloaded node fires its fetch exactly once
;; (:state loading single-flight guard).  Render collects nodes to load and
;; fires them after the redraw, so even synchronous (mocked) callbacks can
;; not recurse into a half-rendered buffer.  A generation counter plus
;; buffer-liveness plus node re-lookup by key guards every async callback.
;;
;; The buffer is cached: re-entering with the same source pops it instantly;
;; use g to refresh (expanded state survives, contents reload lazily).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'freebox-ui)
(require 'freebox-image)

(declare-function hydra-keyboard-quit "hydra")

;;; --- Constants & state --------------------------------------------------------

(defconst freebox-vod-buffer-name "*freebox-vod*"
  "Name of the VOD tree buffer.")

(defvar-local freebox-vod--source-key nil
  "Source key backing the current buffer.")

(defvar-local freebox-vod--source-name nil
  "Source display name backing the current buffer.")

(defvar-local freebox-vod--generation 0
  "Generation counter; bumped on source switch / refresh to kill stale callbacks.")

(defvar-local freebox-vod--categories nil
  "List of category plists.
Each: (:tid :name :state unloaded/loading/loading-more/loaded
       :page :pagecount :items (vod-plist...))")

(defvar-local freebox-vod--cats-state 'unloaded
  "State of the category list itself: unloaded/loading/loaded.")

(defvar-local freebox-vod--search nil
  "Search pseudo-group plist, or nil.
(:keyword :state unloaded/loading/loaded :items (vod-plist...))")

(defvar-local freebox-vod--expanded nil
  "Hash table (:test equal) of expanded node keys.
Keys: \"c:<tid>\" category, \"v:<id>\" vod, \"v:<id>:f:<flag>\" flag,
\"q:\" search group, \"q:<id>\" search-result vod, \"q:<id>:f:<flag>\" its flag.")

(defvar freebox-vod--cached-source-key nil
  "Source key last rendered into the VOD buffer.
Used by `freebox-vod-open' to pop the cached buffer without refetching.")

;;; --- Key & id helpers ----------------------------------------------------------

(defun freebox-vod--id= (a b)
  "Compare ids A and B which may be strings or numbers."
  (equal (format "%s" a) (format "%s" b)))

(defun freebox-vod--cat-key (tid)
  "Expansion key for category TID."
  (format "c:%s" tid))

(defun freebox-vod--vod-key (vod)
  "Expansion key for VOD plist (prefix q: for search results, v: otherwise)."
  (pcase (plist-get vod :owner)
    (`(:search) (format "q:%s" (plist-get vod :id)))
    (_ (format "v:%s" (plist-get vod :id)))))

(defun freebox-vod--flag-key (vod flag-plist)
  "Expansion key for FLAG-PLIST under VOD."
  (format "%s:f:%s" (freebox-vod--vod-key vod) (plist-get flag-plist :flag)))

;;; --- Constructors & finders -----------------------------------------------------

(defun freebox-vod--make-vod (entry owner)
  "Build a vod plist from raw alist ENTRY.
OWNER is (:cat TID) or (:search); used to re-locate the plist in callbacks."
  (list :id (freebox-ui--jget entry 'id)
        :name (or (freebox-ui--jget entry 'name) "?")
        :note (freebox-ui--jget entry 'note)
        :pic (freebox-ui--jget entry 'pic)
        :raw entry
        :owner owner
        :state 'unloaded
        :detail nil
        :flags nil))

(defun freebox-vod--find-category (tid)
  "Return the category plist with TID, or nil."
  (cl-find tid freebox-vod--categories
           :test #'freebox-vod--id=
           :key (lambda (c) (plist-get c :tid))))

(defun freebox-vod--find-vod-in (items id)
  "Return the vod plist with ID in ITEMS, or nil."
  (cl-find id items
           :test #'freebox-vod--id=
           :key (lambda (v) (plist-get v :id))))

(defun freebox-vod--locate-vod (vod)
  "Re-locate the current plist for VOD by owner+id, or nil if it is gone.
Callbacks hold a possibly stale plist; this finds the live one."
  (pcase (plist-get vod :owner)
    (`(:cat ,tid)
     (let ((c (freebox-vod--find-category tid)))
       (and c (freebox-vod--find-vod-in (plist-get c :items)
                                        (plist-get vod :id)))))
    (`(:search)
     (and freebox-vod--search
          (freebox-vod--find-vod-in (plist-get freebox-vod--search :items)
                                    (plist-get vod :id))))))

(defun freebox-vod--find-flag (vod flag-name)
  "Return the flag plist named FLAG-NAME under VOD, or nil."
  (cl-find flag-name (plist-get vod :flags)
           :test #'equal
           :key (lambda (f) (plist-get f :flag))))

;;; --- Callback guard --------------------------------------------------------------

(defun freebox-vod--tree-buffer ()
  "Return the VOD tree buffer, or nil."
  (get-buffer freebox-vod-buffer-name))

(defun freebox-vod--valid-callback-p (buf gen)
  "Non-nil when BUF is a live `freebox-vod-mode' buffer at generation GEN."
  (and (buffer-live-p buf)
       (with-current-buffer buf
         (and (derived-mode-p 'freebox-vod-mode)
              (= gen freebox-vod--generation)))))

;;; --- Fillers (pure-ish, mutate plists) ---------------------------------------------

(defun freebox-vod--fill-category-page (cat data page)
  "Parse page PAGE of category response DATA and append to CAT's items."
  (let* ((movie (freebox-ui--jget data 'movie))
         (pagecount (or (freebox-ui--jget movie 'pagecount) 9999))
         (items (freebox-ui--vec->list (freebox-ui--jget movie 'videoList)))
         (tid (plist-get cat :tid)))
    (dolist (e items)
      (let ((pic (freebox-ui--jget e 'pic)))
        (when (and pic (stringp pic) (not (string-empty-p pic)))
          (freebox-image-get pic #'ignore))))
    (setf (plist-get cat :items)
          (append (plist-get cat :items)
                  (mapcar (lambda (e) (freebox-vod--make-vod e (list :cat tid)))
                          items))
          (plist-get cat :page) page
          (plist-get cat :pagecount) pagecount)))

(defun freebox-vod--fill-detail (vod data)
  "Parse detail response DATA into VOD: raw detail + flag plists."
  (let* ((movie (freebox-ui--jget data 'movie))
         (items (freebox-ui--vec->list (freebox-ui--jget movie 'videoList)))
         (detail (and items (car items))))
    (when detail
      (setf (plist-get vod :detail) detail
            (plist-get vod :name) (or (freebox-ui--jget detail 'name)
                                      (plist-get vod :name))
            (plist-get vod :note) (or (freebox-ui--jget detail 'note)
                                      (plist-get vod :note))
            (plist-get vod :pic) (or (freebox-ui--jget detail 'pic)
                                     (plist-get vod :pic)))
      (let* ((url-bean (freebox-ui--jget detail 'urlBean))
             (info-list (freebox-ui--vec->list
                         (freebox-ui--jget url-bean 'infoList))))
        (setf (plist-get vod :flags)
              (mapcar
               (lambda (info)
                 (let* ((fname (or (freebox-ui--jget info 'flag) "?"))
                        (urls (freebox-ui--jget info 'urls))
                        (share (and (stringp urls)
                                    (string-match "RESOLVE:\\(.+\\)" urls)
                                    (match-string 1 urls))))
                   (list :flag fname
                         :url-str (and (not share) urls)
                         :share-link share
                         :state 'unloaded
                         :episodes nil)))
               info-list))))))

;;; --- v-cursor helpers --------------------------------------------------------------

(defun freebox-vod--save-cursor-for-vod (vod &optional flag)
  "Save vod-detail (or episode when FLAG) v-cursor for VOD.
Includes category tid/name/page when VOD comes from a category, so
`freebox-vod-resume' can rebuild the exact tree position."
  (pcase (plist-get vod :owner)
    (`(:cat ,tid)
     (let ((c (freebox-vod--find-category tid)))
       (if flag
           (freebox-ui--save-v-cursor 'episode freebox-vod--source-key
                                      (plist-get vod :id) flag
                                      tid
                                      (and c (plist-get c :name))
                                      (and c (plist-get c :page)))
         (freebox-ui--save-v-cursor 'vod-detail freebox-vod--source-key
                                    (plist-get vod :id) (plist-get vod :name)
                                    tid
                                    (and c (plist-get c :name))
                                    (and c (plist-get c :page))))))
    (_
     (if flag
         (freebox-ui--save-v-cursor 'episode freebox-vod--source-key
                                    (plist-get vod :id) flag)
       (freebox-ui--save-v-cursor 'vod-detail freebox-vod--source-key
                                  (plist-get vod :id) (plist-get vod :name))))))

;;; --- Lazy loading: categories --------------------------------------------------------

(defun freebox-vod--load-categories (&optional done-fn)
  "Fetch the category list for the buffer's source.
On success call DONE-FN (if any) in the tree buffer, after rendering."
  (setf freebox-vod--cats-state 'loading)
  (freebox-vod--render)
  (let ((buf (freebox-vod--tree-buffer))
        (gen freebox-vod--generation)
        (src freebox-vod--source-key))
    (freebox-http-get-categories
     src freebox-ui-current-client-id
     (lambda (err data)
       (when (freebox-vod--valid-callback-p buf gen)
         (with-current-buffer buf
           (if err
               (progn
                 (setf freebox-vod--cats-state 'unloaded)
                 (freebox-ui--error err)
                 (freebox-vod--render))
             (let* ((classes (freebox-ui--jget data 'classes))
                    (items (freebox-ui--vec->list
                            (freebox-ui--jget classes 'sortList))))
               (setf freebox-vod--categories
                     (mapcar (lambda (c)
                               (list :tid (freebox-ui--jget c 'id)
                                     :name (or (freebox-ui--jget c 'name)
                                               (format "%s" (freebox-ui--jget c 'id)))
                                     :state 'unloaded
                                     :page 0
                                     :pagecount nil
                                     :items nil))
                             items)
                     freebox-vod--cats-state 'loaded)
               (freebox-vod--render)
               (when done-fn (funcall done-fn))))))))))

;;; --- Lazy loading: category pages ------------------------------------------------------

(defun freebox-vod--ensure-category-loaded (cat)
  "Fire the page-1 fetch for CAT when unloaded.  Single-flight via :state."
  (when (eq (plist-get cat :state) 'unloaded)
    (setf (plist-get cat :state) 'loading)
    (let* ((buf (freebox-vod--tree-buffer))
           (gen (and buf (buffer-local-value 'freebox-vod--generation buf)))
           (src (and buf (buffer-local-value 'freebox-vod--source-key buf)))
           (tid (plist-get cat :tid)))
      (freebox-http-get-category
       src tid 1 freebox-ui-current-client-id
       (lambda (err data)
         (when (freebox-vod--valid-callback-p buf gen)
           (with-current-buffer buf
             (let ((c (freebox-vod--find-category tid)))
               (when (and c (eq (plist-get c :state) 'loading))
                 (if err
                     (progn
                       (setf (plist-get c :state) 'unloaded)
                       (freebox-ui--error err))
                   (freebox-vod--fill-category-page c data 1)
                   (setf (plist-get c :state) 'loaded)
                   (freebox-ui--save-v-cursor 'vod-list src tid
                                              (plist-get c :name) 1))
                 (freebox-vod--render))))))))))

(defun freebox-vod--load-more (cat)
  "Fetch the next page of CAT and append.  No-op unless fully loaded."
  (when (and (eq (plist-get cat :state) 'loaded)
             (< (plist-get cat :page) (or (plist-get cat :pagecount) 0)))
    (let* ((buf (freebox-vod--tree-buffer))
           (gen (and buf (buffer-local-value 'freebox-vod--generation buf)))
           (src (and buf (buffer-local-value 'freebox-vod--source-key buf)))
           (tid (plist-get cat :tid))
           (next (1+ (plist-get cat :page))))
      (setf (plist-get cat :state) 'loading-more)
      (freebox-vod--render)
      (freebox-http-get-category
       src tid next freebox-ui-current-client-id
       (lambda (err data)
         (when (freebox-vod--valid-callback-p buf gen)
           (with-current-buffer buf
             (let ((c (freebox-vod--find-category tid)))
               (when (and c (eq (plist-get c :state) 'loading-more))
                 (if err
                     (progn
                       (setf (plist-get c :state) 'loaded)
                       (freebox-ui--error err))
                   (freebox-vod--fill-category-page c data next)
                   (setf (plist-get c :state) 'loaded)
                   (freebox-ui--save-v-cursor 'vod-list src tid
                                              (plist-get c :name) next))
                 (freebox-vod--render))))))))))

;;; --- Lazy loading: vod detail ------------------------------------------------------------

(defun freebox-vod--load-detail (vod done-fn)
  "Fetch detail for VOD, then call DONE-FN with non-nil on success.
DONE-FN runs in the tree buffer.  Single-flight via :state; if VOD is
already loaded, DONE-FN is called immediately."
  (cond
   ((eq (plist-get vod :state) 'loaded)
    (let ((buf (freebox-vod--tree-buffer)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (funcall done-fn t)))))
   ((eq (plist-get vod :state) 'unloaded)
    (setf (plist-get vod :state) 'loading)
    (let* ((buf (freebox-vod--tree-buffer))
           (gen (and buf (buffer-local-value 'freebox-vod--generation buf)))
           (src (and buf (buffer-local-value 'freebox-vod--source-key buf)))
           (vod-id (plist-get vod :id)))
      (freebox-http-get-detail
       src vod-id freebox-ui-current-client-id
       (lambda (err data)
         (when (freebox-vod--valid-callback-p buf gen)
           (with-current-buffer buf
             (let ((v (freebox-vod--locate-vod vod)))
               (when (and v (eq (plist-get v :state) 'loading))
                 (if err
                     (progn
                       (setf (plist-get v :state) 'unloaded)
                       (freebox-ui--error err)
                       (funcall done-fn nil))
                   (freebox-vod--fill-detail v data)
                   (setf (plist-get v :state) 'loaded)
                   (funcall done-fn t))))))))))))

(defun freebox-vod--ensure-detail-loaded (vod)
  "Fire the detail fetch for VOD when unloaded, then re-render."
  (freebox-vod--load-detail
   vod
   (lambda (ok)
     (when ok (freebox-vod--save-cursor-for-vod vod))
     (freebox-vod--render))))

;;; --- Lazy loading: flags (episodes) --------------------------------------------------------

(defun freebox-vod--login-type-name (login-type)
  "Human-readable name for LOGIN-TYPE."
  (pcase login-type
    ("quark" "夸克网盘")
    ("uc" "UC网盘")
    ("bd" "百度网盘")
    (_ login-type)))

(defun freebox-vod--retry-flag-after-login (vod flag-name)
  "Re-fire the episode load of flag FLAG-NAME under VOD (after QR login)."
  (let ((buf (freebox-vod--tree-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((v (freebox-vod--locate-vod vod)))
          (when v
            (let ((f (freebox-vod--find-flag v flag-name)))
              (when f
                (setf (plist-get f :state) 'unloaded)
                (freebox-vod--ensure-flag-loaded v f)
                (freebox-vod--render)))))))))

(defun freebox-vod--resolve-empty (vod flag-plist share-link)
  "Handle an empty/error RESOLVE result for FLAG-PLIST.
Offers QR login when the backend reports an unconfigured drive."
  (let* ((flag (plist-get flag-plist :flag))
         (urls (plist-get flag-plist :url-str))
         (err-msg (and urls (freebox-ui--error-url-p urls)
                       (freebox-ui--extract-error-message urls)))
         (login-type (freebox-ui--infer-login-type flag)))
    (if (and err-msg login-type
             (string-match-p "网盘未配置\\|未配置" err-msg)
             share-link)
        (progn
          (message "FreeBox: [%s] %s" flag err-msg)
          (when (y-or-n-p (format "是否扫码登录%s？"
                                  (freebox-vod--login-type-name login-type)))
            (freebox-ui--start-qr-login
             login-type flag share-link
             (lambda () (freebox-vod--retry-flag-after-login vod flag)))))
      (message "FreeBox: [%s] %s" flag (or err-msg "解析失败，请检查网盘配置")))))

(defun freebox-vod--resolve-flag (vod flag-plist share-link)
  "Resolve SHARE-LINK for FLAG-PLIST (RESOLVE-type line) into episodes."
  (let* ((buf (freebox-vod--tree-buffer))
         (gen (and buf (buffer-local-value 'freebox-vod--generation buf)))
         (src (and buf (buffer-local-value 'freebox-vod--source-key buf)))
         (flag-name (plist-get flag-plist :flag)))
    (freebox-http-resolve-share
     src flag-name share-link freebox-ui-current-client-id
     (lambda (err data)
       (when (freebox-vod--valid-callback-p buf gen)
         (with-current-buffer buf
           (let* ((v (freebox-vod--locate-vod vod))
                  (f (and v (freebox-vod--find-flag v flag-name))))
             (when (and f (eq (plist-get f :state) 'loading))
               (cond
                (err
                 (setf (plist-get f :state) 'unloaded)
                 (freebox-ui--error err))
                (t
                 (let ((urls (and data (alist-get 'urls data))))
                   (if (or (not urls) (string-empty-p urls))
                       (progn
                         (setf (plist-get f :state) 'unloaded)
                         (freebox-vod--resolve-empty v f share-link))
                     (setf (plist-get f :url-str) urls)
                     (let ((eps (freebox-ui-parse-episodes urls)))
                       (if eps
                           (setf (plist-get f :episodes) eps
                                 (plist-get f :state) 'loaded)
                         (setf (plist-get f :state) 'unloaded)
                         (freebox-vod--resolve-empty v f share-link)))))))
               (freebox-vod--render)))))))))

(defun freebox-vod--flag-parse-failed (flag-plist)
  "Handle a direct flag whose URL string has no playable episodes."
  (let* ((flag (plist-get flag-plist :flag))
         (url-str (plist-get flag-plist :url-str)))
    (if (freebox-ui--error-url-p url-str)
        (let ((err-msg (freebox-ui--extract-error-message url-str))
              (login-type (freebox-ui--infer-login-type flag)))
          (message "FreeBox: [%s] %s" flag err-msg)
          (when login-type
            (message "FreeBox: 该网盘可能需要扫码登录%s"
                     (freebox-vod--login-type-name login-type))))
      (message "FreeBox: [%s] 无可播放剧集" flag)))
  (freebox-vod--render))

(defun freebox-vod--ensure-flag-loaded (vod flag-plist)
  "Load episodes for FLAG-PLIST under VOD when unloaded.  Single-flight.
Direct lines parse synchronously; RESOLVE-type lines fire resolve-share."
  (when (eq (plist-get flag-plist :state) 'unloaded)
    (setf (plist-get flag-plist :state) 'loading)
    (let ((share (plist-get flag-plist :share-link)))
      (if share
          (freebox-vod--resolve-flag vod flag-plist share)
        (let ((episodes (freebox-ui-parse-episodes
                         (plist-get flag-plist :url-str))))
          (if episodes
              (progn
                (setf (plist-get flag-plist :episodes) episodes
                      (plist-get flag-plist :state) 'loaded)
                (freebox-vod--render))
            (setf (plist-get flag-plist :state) 'unloaded)
            (freebox-vod--flag-parse-failed flag-plist)))))))

;;; --- Lazy loading: search -------------------------------------------------------------------

(defun freebox-vod--fire-search ()
  "Run the search stored in `freebox-vod--search' when unloaded."
  (let ((s freebox-vod--search))
    (when (and s (eq (plist-get s :state) 'unloaded))
      (setf (plist-get s :state) 'loading)
      (let* ((buf (freebox-vod--tree-buffer))
             (gen (and buf (buffer-local-value 'freebox-vod--generation buf)))
             (src (and buf (buffer-local-value 'freebox-vod--source-key buf)))
             (kw (plist-get s :keyword)))
        (freebox-http-search
         src kw freebox-ui-current-client-id
         (lambda (err data)
           (when (freebox-vod--valid-callback-p buf gen)
             (with-current-buffer buf
               (when (and freebox-vod--search
                          (equal (plist-get freebox-vod--search :keyword) kw)
                          (eq (plist-get freebox-vod--search :state) 'loading))
                 (if err
                     (progn
                       (setf (plist-get freebox-vod--search :state) 'unloaded)
                       (freebox-ui--error err))
                   (let* ((movie (freebox-ui--jget data 'movie))
                          (items (freebox-ui--vec->list
                                  (freebox-ui--jget movie 'videoList))))
                     (dolist (e items)
                       (let ((pic (freebox-ui--jget e 'pic)))
                         (when (and pic (stringp pic)
                                    (not (string-empty-p pic)))
                           (freebox-image-get pic #'ignore))))
                     (setf (plist-get freebox-vod--search :items)
                           (mapcar (lambda (e)
                                     (freebox-vod--make-vod e '(:search)))
                                   items)
                           (plist-get freebox-vod--search :state) 'loaded)))
                 (freebox-vod--render))))))))))

;;; --- Rendering ----------------------------------------------------------------------------------

(defun freebox-vod--render ()
  "Re-render the whole tree, preserving point by node key when possible.
Nodes that are expanded but unloaded are collected during the walk and
their fetches fired after the redraw (single-flight guards make this
idempotent), so callbacks can never recurse into a half-rendered buffer."
  (let ((buf (freebox-vod--tree-buffer)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'freebox-vod-mode)
          (let ((inhibit-read-only t)
                (node-key (freebox-vod--node-key-at-point))
                (line-no (line-number-at-pos))
                (pending nil))
            (erase-buffer)
            (insert (propertize (format "FreeBox 点播 [%s]"
                                        (or freebox-vod--source-name "?"))
                                'face '(:weight bold :height 1.1))
                    "\n"
                    (propertize (make-string 64 ?─) 'face 'shadow)
                    "\n")
            (when freebox-vod--search
              (setq pending
                    (nconc pending (freebox-vod--insert-search-group))))
            (pcase freebox-vod--cats-state
              ('loading
               (insert (propertize "  加载中…\n" 'face 'shadow)))
              ('loaded
               (if (null freebox-vod--categories)
                   (insert (propertize "  (该源没有分类)\n" 'face 'shadow))
                 (dolist (c freebox-vod--categories)
                   (setq pending
                         (nconc pending (freebox-vod--insert-category c))))))
              (_
               (insert (propertize "  (按 g 加载分类)\n" 'face 'shadow))))
            (insert (propertize (make-string 64 ?─) 'face 'shadow)
                    "\n"
                    (propertize
                     "[RET] 播放/展开  [TAB] 折叠  [j/k/n] 移动  [g] 刷新  [s] 换源  [/] 搜索  [p] 海报  [V] 海报集  [q] 退出"
                     'face 'font-lock-comment-face)
                    "\n")
            (unless (and node-key (freebox-vod--goto-node-key node-key))
              (goto-char (point-min))
              (forward-line (1- line-no)))
            (dolist (thunk pending)
              (funcall thunk))))))))

(defun freebox-vod--insert-search-group ()
  "Insert the search pseudo-group.  Return a list of load thunks."
  (let* ((s freebox-vod--search)
         (key "q:")
         (expanded (gethash key freebox-vod--expanded))
         (state (plist-get s :state))
         (items (plist-get s :items))
         (pending nil))
    (insert (propertize (format "%s 搜索: %s%s\n"
                                (if expanded "▾" "▸")
                                (plist-get s :keyword)
                                (if (eq state 'loaded)
                                    (format " (%d)" (length items))
                                  ""))
                        'face 'font-lock-function-name-face
                        'freebox-vod-node
                        (list :type 'search :key key :data s)))
    (when expanded
      (cond
       ((eq state 'unloaded)
        (insert (propertize "    加载中…\n" 'face 'shadow))
        (push (lambda () (freebox-vod--fire-search)) pending))
       ((eq state 'loading)
        (insert (propertize "    加载中…\n" 'face 'shadow)))
       ((null items)
        (insert (propertize (format "    无结果: %s\n" (plist-get s :keyword))
                            'face 'shadow)))
       (t
        (dolist (v items)
          (setq pending (nconc pending (freebox-vod--insert-vod v)))))))
    (nreverse pending)))

(defun freebox-vod--insert-category (cat)
  "Insert CAT and (when expanded) its children.  Return a list of load thunks."
  (let* ((tid (plist-get cat :tid))
         (key (freebox-vod--cat-key tid))
         (expanded (gethash key freebox-vod--expanded))
         (state (plist-get cat :state))
         (items (plist-get cat :items))
         (pending nil))
    (insert (propertize (format "%s %s%s\n"
                                (if expanded "▾" "▸")
                                (plist-get cat :name)
                                (if (memq state '(loaded loading-more))
                                    (format " (%d)" (length items))
                                  ""))
                        'face 'font-lock-keyword-face
                        'freebox-vod-node
                        (list :type 'category :key key :data cat)))
    (when expanded
      (cond
       ((eq state 'unloaded)
        (insert (propertize "    加载中…\n" 'face 'shadow))
        (push (lambda () (freebox-vod--ensure-category-loaded cat)) pending))
       (t
        (dolist (v items)
          (setq pending (nconc pending (freebox-vod--insert-vod v))))
        (cond
         ((memq state '(loading loading-more))
          (insert (propertize "    加载中…\n" 'face 'shadow)))
         ((null items)
          (insert (propertize "    (空)\n" 'face 'shadow)))
         ((< (plist-get cat :page) (or (plist-get cat :pagecount) 0))
          (insert (propertize (format "  -- 加载更多 (p.%d/%d) --\n"
                                      (plist-get cat :page)
                                      (plist-get cat :pagecount))
                              'face 'font-lock-comment-face
                              'freebox-vod-node
                              (list :type 'more
                                    :key (concat key ":more")
                                    :data cat))))))))
    (nreverse pending)))

(defun freebox-vod--insert-vod (vod)
  "Insert VOD and (when expanded) its flags.  Return a list of load thunks."
  (let* ((key (freebox-vod--vod-key vod))
         (expanded (gethash key freebox-vod--expanded))
         (state (plist-get vod :state))
         (note (plist-get vod :note))
         (label (if (and note (stringp note) (not (string-empty-p note)))
                    (format "%s (%s)" (plist-get vod :name) note)
                  (format "%s" (plist-get vod :name))))
         (pending nil))
    (insert (propertize (format "  %s %s\n" (if expanded "▾" "▸") label)
                        'freebox-vod-node
                        (list :type 'vod :key key :data vod)))
    (when expanded
      (cond
       ((eq state 'unloaded)
        (insert (propertize "      加载中…\n" 'face 'shadow))
        (push (lambda () (freebox-vod--ensure-detail-loaded vod)) pending))
       ((eq state 'loading)
        (insert (propertize "      加载中…\n" 'face 'shadow)))
       (t
        (let ((flags (plist-get vod :flags)))
          (if (null flags)
              (insert (propertize "      (无可播放线路)\n" 'face 'shadow))
            (dolist (f flags)
              (setq pending
                    (nconc pending (freebox-vod--insert-flag vod f)))))))))
    (nreverse pending)))

(defun freebox-vod--insert-flag (vod flag-plist)
  "Insert FLAG-PLIST under VOD and (when expanded) its episodes.
Return a list of load thunks."
  (let* ((key (freebox-vod--flag-key vod flag-plist))
         (expanded (gethash key freebox-vod--expanded))
         (state (plist-get flag-plist :state))
         (eps (plist-get flag-plist :episodes))
         (suffix (cond ((eq state 'loaded)
                        (format " (%d集)" (length eps)))
                       ((plist-get flag-plist :share-link) " ⇢")
                       (t "")))
         (pending nil))
    (insert (propertize (format "      %s %s%s\n"
                                (if expanded "▾" "▸")
                                (plist-get flag-plist :flag)
                                suffix)
                        'freebox-vod-node
                        (list :type 'flag :key key :data flag-plist :vod vod)))
    (when expanded
      (cond
       ((eq state 'unloaded)
        (insert (propertize "          加载中…\n" 'face 'shadow))
        (push (lambda () (freebox-vod--ensure-flag-loaded vod flag-plist))
              pending))
       ((eq state 'loading)
        (insert (propertize "          加载中…\n" 'face 'shadow)))
       (t
        (dolist (ep eps)
          (insert (propertize (format "          %s\n" (car ep))
                              'face 'font-lock-comment-face
                              'freebox-vod-node
                              (list :type 'episode
                                    :key (format "%s:e:%s" key (car ep))
                                    :data ep
                                    :vod vod
                                    :flag (plist-get flag-plist :flag))))))))
    (nreverse pending)))

;;; --- Node helpers --------------------------------------------------------------------------------

(defun freebox-vod--node-at-point ()
  "Return the node plist at the beginning of the current line, or nil."
  (get-text-property (line-beginning-position) 'freebox-vod-node))

(defun freebox-vod--node-key-at-point ()
  "Return the node key at the current line, or nil."
  (plist-get (freebox-vod--node-at-point) :key))

(defun freebox-vod--goto-node-key (key)
  "Move point to the line whose node key is KEY.  Return non-nil if found."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (if (equal (freebox-vod--node-key-at-point) key)
            (setq found (line-beginning-position))
          (forward-line 1))))
    (when found
      (goto-char found)
      t)))

;;; --- Interaction ------------------------------------------------------------------------------------

(defun freebox-vod-activate ()
  "RET: play an episode, page a category, or toggle an expandable node."
  (interactive)
  (let ((node (freebox-vod--node-at-point)))
    (if (not node)
        (message "FreeBox: 当前行不是节点")
      (pcase (plist-get node :type)
        ('episode
         (freebox-vod--play-episode node))
        ('more
         (freebox-vod--load-more (plist-get node :data)))
        (_
         (puthash (plist-get node :key)
                  (not (gethash (plist-get node :key) freebox-vod--expanded))
                  freebox-vod--expanded)
         (freebox-vod--render))))))

(defun freebox-vod-toggle ()
  "TAB: fold the current node (expansion is RET's job, it may trigger loads)."
  (interactive)
  (let ((node (freebox-vod--node-at-point)))
    (cond
     ((not node)
      (message "FreeBox: 当前行不是节点"))
     ((gethash (plist-get node :key) freebox-vod--expanded)
      (puthash (plist-get node :key) nil freebox-vod--expanded)
      (freebox-vod--render))
     (t
      (message "FreeBox: 按 RET 展开")))))

(defun freebox-vod--play-episode (node)
  "Play the episode at NODE (records episode v-cursor first)."
  (let* ((ep (plist-get node :data))
         (vod (plist-get node :vod))
         (flag (plist-get node :flag))
         (name (car ep))
         (url (cdr ep)))
    (freebox-vod--save-cursor-for-vod vod flag)
    (freebox-ui--resolve-and-play
     freebox-vod--source-key flag url url name)))

(defun freebox-vod-refresh ()
  "g: refetch everything; expansion state survives, contents reload lazily."
  (interactive)
  (unless freebox-vod--source-key
    (user-error "FreeBox: 当前 buffer 没有点播源上下文"))
  (cl-incf freebox-vod--generation)
  (when freebox-vod--search
    (setf (plist-get freebox-vod--search :state) 'unloaded
          (plist-get freebox-vod--search :items) nil))
  (freebox-vod--load-categories))

(defun freebox-vod-switch-source ()
  "s: pick another source and rebuild the tree."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--with-client
      (lambda (client-id)
        (freebox-ui--loading "fetching sources")
        (freebox-http-get-sources
         client-id
         (lambda (err sources)
           (if err
               (freebox-ui--error err)
             (if (not sources)
                 (message "FreeBox: no sources available.")
               (let ((picked (freebox-ui--pick-source-from-list sources)))
                 (when picked
                   (freebox-ui--save-source (car picked) (cdr picked))
                   (message "FreeBox: source -> %s" (cdr picked))
                   (freebox-vod--open-with-source (car picked)))))))))))))

(defun freebox-vod-search-in-tree ()
  "/: prompt for a keyword and show results as the top pseudo-group."
  (interactive)
  (let ((kw (condition-case nil
                (read-string (format "FreeBox 搜索 [%s]: "
                                     (or freebox-vod--source-name "?")))
              (quit nil))))
    (when (and kw (not (string-empty-p kw)))
      (freebox-vod--do-search kw))))

(defun freebox-vod--do-search (keyword)
  "Set up KEYWORD as the search pseudo-group and fire the search lazily."
  (setq freebox-vod--search
        (list :keyword keyword :state 'unloaded :items nil))
  (puthash "q:" t freebox-vod--expanded)
  (freebox-vod--render)
  (freebox-vod--goto-node-key "q:"))

(defun freebox-vod-show-poster ()
  "p: show the poster of the vod at point (loads detail first if needed)."
  (interactive)
  (let ((node (freebox-vod--node-at-point)))
    (if (not (memq (and node (plist-get node :type)) '(vod flag episode)))
        (message "FreeBox: 在影片节点上按 p 查看海报")
      (let ((vod (if (eq (plist-get node :type) 'vod)
                     (plist-get node :data)
                   (plist-get node :vod))))
        (if (not (display-images-p))
            (message "FreeBox: 当前环境不支持图片显示")
          (freebox-vod--load-detail
           vod
           (lambda (ok)
             (when ok
               (let ((pic (plist-get vod :pic))
                     (detail (or (plist-get vod :detail)
                                 (plist-get vod :raw))))
                 (if (and pic (stringp pic) (not (string-empty-p pic)))
                     (freebox-image-show-poster detail (plist-get vod :id)
                                                pic nil)
                   (message "FreeBox: [%s] 无海报"
                            (plist-get vod :name))))))))))))

(defun freebox-vod-show-gallery ()
  "V: show a poster gallery of the loaded items of the category at point."
  (interactive)
  (let ((node (freebox-vod--node-at-point)))
    (if (not (eq (and node (plist-get node :type)) 'category))
        (message "FreeBox: 在分类节点上按 V 查看海报集")
      (let ((cat (plist-get node :data)))
        (cond
         ((not (display-images-p))
          (message "FreeBox: 当前环境不支持图片显示"))
         ((not (memq (plist-get cat :state) '(loaded loading-more)))
          (message "FreeBox: 先按 RET 展开加载该分类"))
         ((null (plist-get cat :items))
          (message "FreeBox: 该分类为空"))
         (t
          (freebox-image-show-gallery
           (mapcar (lambda (v) (plist-get v :raw)) (plist-get cat :items))
           (plist-get cat :name)
           (plist-get cat :page)
           (or (plist-get cat :pagecount) 1)
           freebox-vod--source-key
           (plist-get cat :tid))))))))

;;; --- Entry points -----------------------------------------------------------------------------------

(defun freebox-vod-open ()
  "Open the VOD tree browser.
Pops the cached buffer instantly when it exists and the source has not
changed; otherwise (re)loads the category list."
  (interactive)
  (let ((buf (get-buffer freebox-vod-buffer-name)))
    (if (and buf
             freebox-vod--cached-source-key
             freebox-ui-current-source
             (equal freebox-vod--cached-source-key freebox-ui-current-source))
        (progn
          (when (bound-and-true-p hydra-curr-map)
            (hydra-keyboard-quit))
          (pop-to-buffer buf '((display-buffer-same-window))))
      (freebox-http-ensure-server
       (lambda () (freebox-ui--with-source #'freebox-vod--open-with-source))))))

(defun freebox-vod--open-with-source (source-key)
  "Show the tree buffer for SOURCE-KEY, loading categories when needed.
Leaves the tree buffer current and displayed."
  (let ((buf (get-buffer-create freebox-vod-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'freebox-vod-mode)
        (freebox-vod-mode))
      (unless freebox-vod--expanded
        (setq freebox-vod--expanded (make-hash-table :test 'equal)))
      (let ((new-source (not (equal freebox-vod--source-key source-key))))
        (when new-source
          (cl-incf freebox-vod--generation)
          (setq freebox-vod--source-key source-key
                freebox-vod--source-name freebox-ui-current-source-name
                freebox-vod--categories nil
                freebox-vod--cats-state 'unloaded
                freebox-vod--search nil)
          (clrhash freebox-vod--expanded))
        (setq freebox-vod--cached-source-key source-key)
        (when (eq freebox-vod--cats-state 'loaded)
          (freebox-vod--render))))
    (when (bound-and-true-p hydra-curr-map)
      (hydra-keyboard-quit))
    (pop-to-buffer buf '((display-buffer-same-window)))
    (with-current-buffer buf
      (unless (eq freebox-vod--cats-state 'loaded)
        (freebox-vod--load-categories)))))

(defun freebox-vod-search ()
  "Open the VOD tree and prompt for a search keyword.
Entry point for the hydra s head."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--with-source
      (lambda (source-key)
        (freebox-vod--open-with-source source-key)
        (let ((kw (condition-case nil
                      (read-string (format "FreeBox 搜索 [%s]: "
                                           (or freebox-vod--source-name
                                               source-key)))
                    (quit nil))))
          (when (and kw (not (string-empty-p kw)))
            (freebox-vod--do-search kw))))))))

(defun freebox-vod-select-category ()
  "Open the VOD tree and jump straight to a picked category.
Entry point for the hydra z head."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--with-source
      (lambda (source-key)
        (freebox-vod--open-with-source source-key)
        (if (eq freebox-vod--cats-state 'loaded)
            (freebox-vod--pick-category-in-tree)
          (freebox-vod--load-categories
           #'freebox-vod--pick-category-in-tree)))))))

(defun freebox-vod--pick-category-in-tree ()
  "Prompt for a category, expand it and move point to it."
  (let* ((cands (mapcar (lambda (c)
                          (cons (plist-get c :name) (plist-get c :tid)))
                        freebox-vod--categories))
         (sel (and cands
                   (freebox-ui--completing-read "FreeBox -- Category: " cands))))
    (when sel
      (let ((cat (freebox-vod--find-category (cdr (assoc sel cands)))))
        (when cat
          (freebox-ui--save-category (plist-get cat :tid) (plist-get cat :name))
          (freebox-ui--save-v-cursor 'category freebox-vod--source-key
                                     (plist-get cat :tid)
                                     (plist-get cat :name))
          (puthash (freebox-vod--cat-key (plist-get cat :tid)) t
                   freebox-vod--expanded)
          (freebox-vod--render)
          (freebox-vod--goto-node-key
           (freebox-vod--cat-key (plist-get cat :tid))))))))

;;; --- Resume (v-cursor restore) -----------------------------------------------------------------------

(defun freebox-vod-resume ()
  "Resume browsing from the last remembered navigation node (v-cursor).
Entry point for the hydra v head.  Falls back to a plain tree opening
(with a message) when the recorded position no longer exists."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--with-source
      (lambda (source-key)
        (let* ((cursor (freebox-persist-get-v-cursor))
               (src-key (and cursor (alist-get 'source-key cursor))))
          (freebox-vod--open-with-source source-key)
          (cond
           ((not cursor)
            (message "FreeBox: 没有历史浏览位置"))
           ((not (equal source-key src-key))
            (message "FreeBox: 源已切换，无法恢复上次位置"))
           (t
            (freebox-vod--resume-cursor cursor)))))))))

(defun freebox-vod--resume-cursor (cursor)
  "Restore CURSOR into the tree (loading the category list first if needed)."
  (if (eq freebox-vod--cats-state 'loaded)
      (freebox-vod--resume-cursor-1 cursor)
    (freebox-vod--load-categories
     (lambda () (freebox-vod--resume-cursor-1 cursor)))))

(defun freebox-vod--resume-cursor-1 (cursor)
  "Restore CURSOR into the tree; category list must be loaded already."
  (let* ((type (alist-get 'type cursor))
         (tid (alist-get 'tid cursor))
         (page (or (alist-get 'page cursor) 1))
         (vod-id (alist-get 'vod-id cursor))
         (flag (alist-get 'flag cursor)))
    (pcase type
      ("category"
       (let ((cat (and tid (freebox-vod--find-category tid))))
         (if (not cat)
             (message "FreeBox: 分类 [%s] 已不存在" tid)
           (freebox-ui--save-category (plist-get cat :tid)
                                      (plist-get cat :name))
           (puthash (freebox-vod--cat-key tid) t freebox-vod--expanded)
           (freebox-vod--render)
           (freebox-vod--goto-node-key (freebox-vod--cat-key tid)))))
      ((or "vod-list" "vod-detail" "episode")
       (if (not tid)
           (message "FreeBox: 历史位置不含分类信息（旧格式），请用 / 搜索影片")
         (let ((cat (freebox-vod--find-category tid)))
           (if (not cat)
               (message "FreeBox: 分类 [%s] 已不存在，请重新浏览" tid)
             (puthash (freebox-vod--cat-key tid) t freebox-vod--expanded)
             (freebox-vod--render)
             (freebox-vod--resume-fetch-pages
              cat page
              (lambda ()
                (freebox-vod--render)
                (if (not vod-id)
                    (freebox-vod--goto-node-key (freebox-vod--cat-key tid))
                  (freebox-vod--resume-vod cat vod-id flag page))))))))
      (_
       (message "FreeBox: 无法识别的历史位置类型: %s" type)))))

(defun freebox-vod--resume-vod (cat vod-id flag page)
  "After CAT's pages are restored, expand VOD-ID (and FLAG) and goto it."
  (let ((vod (freebox-vod--find-vod-in (plist-get cat :items) vod-id)))
    (if (not vod)
        (progn
          (message "FreeBox: 影片 [%s] 不在该分类前 %s 页中" vod-id page)
          (freebox-vod--goto-node-key
           (freebox-vod--cat-key (plist-get cat :tid))))
      (puthash (freebox-vod--vod-key vod) t freebox-vod--expanded)
      ;; Load BEFORE rendering: the render post-pass would mark the vod
      ;; loading and this call (guarded by the single-flight check) would
      ;; then never chain DONE-FN.
      (freebox-vod--load-detail
       vod
       (lambda (ok)
         (freebox-vod--render)
         (if (not ok)
             (freebox-vod--goto-node-key (freebox-vod--vod-key vod))
           (freebox-vod--resume-goto-vod vod flag))))
      (freebox-vod--render))))

(defun freebox-vod--resume-goto-vod (vod flag)
  "Goto VOD's node; when FLAG is given, expand it (lazily) and goto it."
  (if (not flag)
      (freebox-vod--goto-node-key (freebox-vod--vod-key vod))
    (let ((f (freebox-vod--find-flag vod flag)))
      (if (not f)
          (progn
            (message "FreeBox: 线路 [%s] 已不存在" flag)
            (freebox-vod--goto-node-key (freebox-vod--vod-key vod)))
        (puthash (freebox-vod--flag-key vod f) t freebox-vod--expanded)
        (freebox-vod--render)
        (freebox-vod--goto-node-key (freebox-vod--flag-key vod f))))))

(defun freebox-vod--resume-fetch-pages (cat target done-fn)
  "Ensure CAT has pages 1..TARGET loaded (serial fetches), then call DONE-FN.
Pages already loaded are skipped; a fetch error stops the chain (the
partially loaded items stay visible)."
  (let ((cur (if (memq (plist-get cat :state) '(loaded loading-more))
                 (plist-get cat :page)
               0)))
    (if (>= cur target)
        (funcall done-fn)
      (let* ((buf (freebox-vod--tree-buffer))
             (gen freebox-vod--generation)
             (src freebox-vod--source-key)
             (tid (plist-get cat :tid)))
        (setf (plist-get cat :state) 'loading)
        (freebox-vod--render)
        (letrec
            ((step
              (lambda (p)
                (freebox-http-get-category
                 src tid p freebox-ui-current-client-id
                 (lambda (err data)
                   (when (freebox-vod--valid-callback-p buf gen)
                     (with-current-buffer buf
                       (let ((c (freebox-vod--find-category tid)))
                         (when (and c (eq (plist-get c :state) 'loading))
                           (if err
                               (progn
                                 (setf (plist-get c :state)
                                       (if (plist-get c :items)
                                           'loaded
                                         'unloaded))
                                 (freebox-ui--error err)
                                 (freebox-vod--render))
                             (freebox-vod--fill-category-page c data p)
                             (if (< p target)
                                 (funcall step (1+ p))
                               (setf (plist-get c :state) 'loaded)
                               (funcall done-fn))))))))))))
          (funcall step (1+ cur)))))))

;;; --- Mode ---------------------------------------------------------------------------------------------

(defvar freebox-vod-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'freebox-vod-activate)
    (define-key map (kbd "TAB") #'freebox-vod-toggle)
    (define-key map (kbd "j")   #'next-line)
    (define-key map (kbd "k")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "g")   #'freebox-vod-refresh)
    (define-key map (kbd "s")   #'freebox-vod-switch-source)
    (define-key map (kbd "/")   #'freebox-vod-search-in-tree)
    (define-key map (kbd "p")   #'freebox-vod-show-poster)
    (define-key map (kbd "V")   #'freebox-vod-show-gallery)
    map)
  "Keymap for `freebox-vod-mode'.")

(define-derived-mode freebox-vod-mode special-mode "FreeBox-VOD"
  "Major mode for the FreeBox VOD tree.
\\<freebox-vod-mode-map>
\\[freebox-vod-activate] - Play / expand
\\[freebox-vod-toggle] - Fold
\\[freebox-vod-refresh] - Refresh (lazy reload, expansion kept)
\\[freebox-vod-switch-source] - Switch source
\\[freebox-vod-search-in-tree] - Search
\\[freebox-vod-show-poster] - Show poster of vod at point
\\[freebox-vod-show-gallery] - Show poster gallery of category at point
\\[quit-window] - Quit"
  :group 'freebox
  (setq-local truncate-lines t))

(provide 'freebox-vod)
;;; freebox-vod.el ends here
