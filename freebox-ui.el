;;; freebox-ui.el --- UI components for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Completing-read based UI for FreeBox.
;; json-read returns alist with symbol keys, so we use alist-get throughout.
;;
;; Workflow:
;;   1. Select client (from /api/clients -- saved CATVOD_SPIDER configs)
;;   2. Select source (from /api/sources?clientId=...)
;;   3. Browse / search within that source
;;
;; C-g behavior:
;;   At ANY level, C-g cancels the current menu silently.
;;   v-cursor is NOT updated on C-g -- next v reopens the same position.
;;   Only actual user selections (not C-g, not "返回上一级") update v-cursor.

;;; Code:

(require 'freebox-http)
(require 'freebox-persist)
(require 'freebox-image)

;;; --- Constants ----------------------------------------------------------------

(defconst freebox-ui--node-levels
  '((category  . 1)
    (vod-list  . 2)
    (vod-detail . 3)
    (episode   . 4))
  "Node level numbers for v-cursor hierarchy comparison.
 Higher numbers are deeper in the tree.")

(defconst freebox-ui--back-label ".. (返回上一级)"
  "Label used for the 'go up one level' entry in menus.")

;;; --- State -------------------------------------------------------------------

(defvar freebox-ui-current-client-id nil
  "ID of the currently selected FreeBox client configuration.")

(defvar freebox-ui-current-client-name nil
  "Display name of the currently selected FreeBox client configuration.")

(defvar freebox-ui-current-source nil
  "Key of the currently selected FreeBox source.")

(defvar freebox-ui-current-source-name nil
  "Display name of the currently selected FreeBox source.")

(defvar freebox-ui-current-category-tid nil
  "TID of the currently selected FreeBox category.")

(defvar freebox-ui-current-category-name nil
  "Display name of the currently selected FreeBox category.")

(defvar freebox-ui-current-live-client-id nil
  "ID of the currently selected FreeBox live TV client (SINGLE_LIVE).")

(defvar freebox-ui-current-live-client-name nil
  "Display name of the currently selected FreeBox live TV client.")

;;; --- Initialization ----------------------------------------------------------

(defun freebox-ui-init ()
  "Initialize UI with persisted state."
  (freebox-persist-init)
  (let ((client-id   (freebox-persist-get-client-id))
        (client-name (freebox-persist-get-client-name))
        (source-key  (freebox-persist-get-source-key))
        (source-name (freebox-persist-get-source-name))
        (cat-tid     (freebox-persist-get-category-tid))
        (cat-name    (freebox-persist-get-category-name))
        (live-id     (freebox-persist-get 'live-client-id))
        (live-name   (freebox-persist-get 'live-client-name)))
    (setq freebox-ui-current-client-id    client-id
          freebox-ui-current-client-name  client-name
          freebox-ui-current-source       source-key
          freebox-ui-current-source-name  source-name
          freebox-ui-current-category-tid  cat-tid
          freebox-ui-current-category-name cat-name
          freebox-ui-current-live-client-id   live-id
          freebox-ui-current-live-client-name live-name)))

;;; --- Internal helpers --------------------------------------------------------

(defun freebox-ui--loading (msg)
  "Display MSG in the echo area as a loading indicator."
  (message "FreeBox: %s..." msg))

(defun freebox-ui--error (msg)
  "Display MSG as an error."
  (message "FreeBox error: %s" msg))

(defun freebox-ui--completing-read (prompt candidates &optional require-match)
  "Like `completing-read' but return nil silently on C-g (quit).
PROMPT and CANDIDATES are passed to `completing-read'.
REQUIRE-MATCH defaults to t.

When called from a (possibly async) hydra head, suppress the hydra
on-exit callback so the lv hint window stays visible during
minibuffer interaction -- mirroring the behavior of synchronous
hydras like `empv-hydra'."
  (condition-case nil
      (let ((hydra-curr-on-exit nil)
            (result (completing-read prompt candidates nil
                                     (if (eq require-match nil) nil t))))
        (if (string-empty-p result) nil result))
    (quit nil)))

(defun freebox-ui--jget (obj key)
  "Get KEY (symbol) from OBJ which may be an alist returned by json-read."
  (alist-get key obj))

(defun freebox-ui--vec->list (v)
  "Convert vector or list V to a list."
  (if (vectorp v) (append v nil) (or v nil)))

;;; --- Client selection --------------------------------------------------------

(defun freebox-ui--pick-client-from-list (clients)
  "Prompt user to pick from CLIENTS list.
Returns (ID . NAME) cons, or nil if cancelled."
  (let* ((items (freebox-ui--vec->list clients))
         (candidates (mapcar (lambda (c)
                               (cons (freebox-ui--jget c 'name)
                                     (freebox-ui--jget c 'id)))
                             items))
         (selected-name (freebox-ui--completing-read
                         "FreeBox -- Select client config: " candidates)))
    (when selected-name
      (cons (cdr (assoc selected-name candidates)) selected-name))))

(defun freebox-ui--save-v-cursor (type &rest args)
  "Save current navigation node as v-cursor.
TYPE is one of: category, vod-list, vod-detail, episode.
ARGS depend on TYPE:
  category:   (source-key tid name)
  vod-list:   (source-key tid cat-name page)
  vod-detail: (source-key vod-id vod-name)
  episode:    (source-key vod-id flag)"
  (let ((cursor
         (pcase type
           ('category
            (let ((source-key (nth 0 args)) (tid (nth 1 args)) (name (nth 2 args)))
              `((type . "category") (source-key . ,source-key)
                (tid . ,tid) (name . ,name))))
           ('vod-list
            (let ((source-key (nth 0 args)) (tid (nth 1 args))
                  (cat-name (nth 2 args)) (page (nth 3 args)))
              `((type . "vod-list") (source-key . ,source-key)
                (tid . ,tid) (cat-name . ,cat-name) (page . ,page))))
           ('vod-detail
            (let ((source-key (nth 0 args)) (vod-id (nth 1 args)) (vod-name (nth 2 args)))
              `((type . "vod-detail") (source-key . ,source-key)
                (vod-id . ,vod-id) (vod-name . ,vod-name))))
           ('episode
            (let ((source-key (nth 0 args)) (vod-id (nth 1 args)) (flag (nth 2 args)))
              `((type . "episode") (source-key . ,source-key)
                (vod-id . ,vod-id) (flag . ,flag)))))))
    (when cursor
      (freebox-persist-set-v-cursor cursor))))

(defun freebox-ui--node-level (type-val)
  "Return numeric level for node TYPE-VAL (string or symbol), or 0 if unknown."
  (let ((sym (cond
              ((symbolp type-val) type-val)
              ((stringp type-val) (intern type-val))
              (t nil))))
    (if sym (or (alist-get sym freebox-ui--node-levels) 0) 0)))

(defun freebox-ui--save-client (id name)
  "Save client selection (ID, NAME) to state and history.
Clears source, category, and v-cursor since client changed."
  (setq freebox-ui-current-client-id   id
        freebox-ui-current-client-name name
        freebox-ui-current-source      nil
        freebox-ui-current-source-name nil)
  (freebox-persist-set-client-id id)
  (freebox-persist-set-client-name name)
  (freebox-persist-clear-v-cursor)
  (freebox-persist-add-history 'clients (vector name id)))

(defun freebox-ui-select-client ()
  "Interactively select a FreeBox client configuration.
This determines which video source config (JSON URL) is used.
Auto-starts the backend if needed.  Saves selection to persistent state."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--loading "fetching client configs")
     (freebox-http-get-clients
      (lambda (err clients)
        (if err
            (freebox-ui--error err)
          (if (not clients)
              (message "FreeBox: no client configs found. Add a source in FreeBox app first.")
            (let ((picked (freebox-ui--pick-client-from-list clients)))
              (when picked
                (freebox-ui--save-client (car picked) (cdr picked))
                (message "FreeBox: client -> [%s]" (cdr picked)))))))))))

(defun freebox-ui--with-client (fn)
  "Ensure a client is selected, then call FN with client-id.
First tries to use persisted client selection, then prompts user."
  (if freebox-ui-current-client-id
      (funcall fn freebox-ui-current-client-id)
    (freebox-ui--loading "fetching client configs")
    (freebox-http-get-clients
     (lambda (err clients)
       (if err
           (freebox-ui--error err)
         (if (not clients)
             (message "FreeBox: no client configs found. Add a source in FreeBox app first.")
           ;; Auto-select if only one client, otherwise prompt
           (let* ((items (freebox-ui--vec->list clients))
                  (picked (if (= (length items) 1)
                              (let* ((c (car items)))
                                (cons (freebox-ui--jget c 'id)
                                      (freebox-ui--jget c 'name)))
                            (freebox-ui--pick-client-from-list clients))))
             (when picked
               (freebox-ui--save-client (car picked) (cdr picked))
               (message "FreeBox: client -> [%s]" (cdr picked))
               (funcall fn (car picked))))))))))

;;; --- Source selection --------------------------------------------------------

(defun freebox-ui--pick-source-from-list (sources)
  "Prompt user to pick from SOURCES (alist list from json-read).
Returns (KEY . NAME) cons, or nil if cancelled."
  (let* ((items (freebox-ui--vec->list sources))
         (candidates (mapcar (lambda (s)
                               (cons (freebox-ui--jget s 'name)
                                     (freebox-ui--jget s 'key)))
                             items))
         (selected-name (freebox-ui--completing-read
                         "FreeBox -- Select source: " candidates)))
    (when selected-name
      (cons (cdr (assoc selected-name candidates)) selected-name))))

(defun freebox-ui--save-source (key name)
  "Save source selection (KEY, NAME) to state and history.
Clears category and v-cursor since source changed."
  (setq freebox-ui-current-source       key
        freebox-ui-current-source-name  name
        freebox-ui-current-category-tid  nil
        freebox-ui-current-category-name nil)
  (freebox-persist-set-source-key key)
  (freebox-persist-set-source-name name)
  (freebox-persist-set-category-tid nil)
  (freebox-persist-set-category-name nil)
  (freebox-persist-clear-v-cursor)
  (freebox-persist-add-history 'sources (vector name key)))

(defun freebox-ui--save-category (tid name)
  "Save category selection (TID, NAME) to state and history.
Clears v-cursor if it points to vod-list or deeper (child of category)."
  (setq freebox-ui-current-category-tid  tid
        freebox-ui-current-category-name name)
  (freebox-persist-set-category-tid tid)
  (freebox-persist-set-category-name name)
  ;; Invalidate v-cursor if it's at vod-list level or deeper
  (let* ((cursor (freebox-persist-get-v-cursor))
         (cursor-type (and cursor (alist-get 'type cursor)))
         (cursor-level (freebox-ui--node-level cursor-type)))
    (when (>= cursor-level (freebox-ui--node-level "vod-list"))
      (freebox-persist-clear-v-cursor)))
  (freebox-persist-add-history 'categories (vector name tid)))

(defun freebox-ui-select-source ()
  "Interactively select (or change) the FreeBox source.
Auto-starts the backend if needed.  Selects a client first if none is chosen."
  (interactive)
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-client #'freebox-ui--do-select-source))))

(defun freebox-ui--do-select-source (client-id)
  "Fetch sources for CLIENT-ID and prompt user to pick one.
Saves selection to persistent state."
  (freebox-ui--loading "fetching sources")
  (freebox-http-get-sources client-id
   (lambda (err sources)
     (if err
         (freebox-ui--error err)
       (if (not sources)
           (message "FreeBox: no sources available.")
         (let ((picked (freebox-ui--pick-source-from-list sources)))
           (when picked
             (freebox-ui--save-source (car picked) (cdr picked))
             (message "FreeBox: source -> [%s]" (cdr picked)))))))))

(defun freebox-ui--with-source (fn)
  "Ensure client and source are selected, then call FN with source-key."
  (if freebox-ui-current-source
      (funcall fn freebox-ui-current-source)
    (freebox-ui--with-client
     (lambda (client-id)
       (freebox-ui--loading "fetching sources")
       (freebox-http-get-sources client-id
        (lambda (err sources)
          (if err
              (freebox-ui--error err)
            (if (not sources)
                (message "FreeBox: no sources available.")
              (let ((picked (freebox-ui--pick-source-from-list sources)))
                (when picked
                  (freebox-ui--save-source (car picked) (cdr picked))
                  (message "FreeBox: source -> %s" (cdr picked))
                  (funcall fn (car picked))))))))))))

;;; --- Search ------------------------------------------------------------------

(defun freebox-ui-search ()
  "Search FreeBox for videos. Picks a client/source first if needed.
Auto-starts the backend if `freebox-http-server-script' is configured."
  (interactive)
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-source #'freebox-ui--do-search))))

(defun freebox-ui--do-search (source-key)
  "Prompt for a keyword and search in SOURCE-KEY."
  (let ((hydra-curr-on-exit nil)
        (keyword (condition-case nil
                     (read-string
                      (format "FreeBox search [%s]: "
                              (or freebox-ui-current-source-name source-key)))
                   (quit nil))))
    (when (and keyword (not (string-empty-p keyword)))
      (freebox-ui--loading (format "searching \"%s\"" keyword))
      (freebox-http-search source-key keyword freebox-ui-current-client-id
        (lambda (err data)
          (if err
              (freebox-ui--error err)
            (let* ((movie  (freebox-ui--jget data 'movie))
                   (items  (freebox-ui--vec->list
                            (freebox-ui--jget movie 'videoList)))
                   (candidates (mapcar (lambda (v)
                                         (let* ((name (freebox-ui--jget v 'name))
                                                (pic  (freebox-ui--jget v 'pic))
                                                (label (if (and pic (stringp pic)
                                                                (not (string-empty-p pic)))
                                                           (concat name " [*]")
                                                         name)))
                                           (cons label (freebox-ui--jget v 'id))))
                                       items)))
              (if (not candidates)
                  (message "FreeBox: no results for \"%s\"." keyword)
                (let* ((selected-name
                        (freebox-ui--completing-read
                         (format "Results for \"%s\" (%d): "
                                 keyword (length candidates))
                         candidates))
                       (selected-id (and selected-name
                                         (cdr (assoc selected-name candidates)))))
                  (when selected-id
                    (freebox-ui-show-detail selected-id)))))))))))

;;; --- Category browse ---------------------------------------------------------

(defun freebox-ui-browse-category ()
  "Browse FreeBox content by category.
Auto-starts the backend if `freebox-http-server-script' is configured."
  (interactive)
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-source #'freebox-ui--pick-category))))

(defun freebox-ui-select-category ()
  "Interactively select a FreeBox category and start browsing from page 1.
Auto-starts the backend if needed. Saves category selection."
  (interactive)
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-source #'freebox-ui--pick-category))))

(defun freebox-ui--pick-category (source-key)
  "Fetch top-level categories for SOURCE-KEY and let user pick one.
Shows a `返回上一级' entry at the top; selecting it goes back to source selection.

C-g cancels silently: v-cursor is NOT updated, so next v resumes same position.
Only an actual category selection updates v-cursor."
  ;; Do NOT update v-cursor here. Only update after the user actually picks.
  ;; This ensures C-g leaves v-cursor unchanged.
  (freebox-ui--loading "fetching categories")
  (freebox-http-get-categories source-key freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        (let* ((classes    (freebox-ui--jget data 'classes))
               (items      (freebox-ui--vec->list
                            (freebox-ui--jget classes 'sortList)))
               (candidates (mapcar (lambda (c)
                                     (cons (or (freebox-ui--jget c 'name)
                                               (freebox-ui--jget c 'id))
                                           (freebox-ui--jget c 'id)))
                                   items))
               (all-cands  (cons (cons freebox-ui--back-label :back) candidates)))
          (if (not candidates)
              (message "FreeBox: no categories found.")
            (let* ((selected-name
                    (freebox-ui--completing-read "FreeBox -- Category: " all-cands))
                   (selected-val (and selected-name
                                      (cdr (assoc selected-name all-cands)))))
              (cond
               ((null selected-val)        nil) ; C-g: cancel, v-cursor unchanged
               ((eq selected-val :back)
                ;; Explicit 返回: go back to source selection
                (freebox-ui--with-client #'freebox-ui--do-select-source))
               (t
                ;; Real selection: update v-cursor now
                (freebox-ui--save-category selected-val selected-name)
                (freebox-ui--save-v-cursor 'category source-key selected-val selected-name)
                (freebox-ui--category-page source-key selected-val selected-name 1))))))))))

(defun freebox-ui--category-page (source-key tid cat-name page)
  "Fetch page PAGE of category TID in SOURCE-KEY.
Records current page as v-cursor for later resumption.
Shows a `返回上一级' entry that goes back to category selection.

C-g cancels silently: v-cursor stays at the current page."
  ;; Record position BEFORE showing the menu, so v-cursor reflects this page.
  (freebox-ui--save-v-cursor 'vod-list source-key tid cat-name page)
  (freebox-ui--loading (format "loading %s p.%d" cat-name page))
  (freebox-http-get-category source-key tid page freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        (let* ((movie      (freebox-ui--jget data 'movie))
               (pagecount  (or (freebox-ui--jget movie 'pagecount) 9999))
               (items      (freebox-ui--vec->list
                            (freebox-ui--jget movie 'videoList)))
               (candidates (mapcar (lambda (v)
                                     (let* ((name (freebox-ui--jget v 'name))
                                            (pic  (freebox-ui--jget v 'pic))
                                            (label (if (and pic (stringp pic)
                                                            (not (string-empty-p pic)))
                                                       (concat name " [*]")
                                                     name)))
                                       (cons label (freebox-ui--jget v 'id))))
                                   items)))
          ;; Preload poster images in background
          (dolist (v items)
            (let ((pic (freebox-ui--jget v 'pic)))
              (when (and pic (stringp pic) (not (string-empty-p pic)))
                (freebox-image-get pic #'ignore))))
          (if (not candidates)
              (message "FreeBox: no content in [%s] p.%d." cat-name page)
            (let* ((has-next   (< page pagecount))
                   (has-prev   (> page 1))
                   (next-label (format "-- Next page (p.%d/%d) --" (1+ page) pagecount))
                   (prev-label (format "-- Prev page (p.%d/%d) --" (1- page) pagecount))
                   (gallery-label (format "-- 查看海报集 (p.%d, %d项) --" page (length candidates)))
                   (all-cands  (append
                                (list (cons freebox-ui--back-label :back))
                                (when has-prev (list (cons prev-label :prev)))
                                (when (display-images-p)
                                  (list (cons gallery-label :gallery)))
                                candidates
                                (when has-next (list (cons next-label :next)))))
                   (selected-name
                    (freebox-ui--completing-read
                     (format "%s p.%d/%d (%d): "
                             cat-name page pagecount (length candidates))
                     all-cands))
                   (selected-val (and selected-name
                                      (cdr (assoc selected-name all-cands)))))
              (cond
               ((null selected-val)      nil) ; C-g: cancel, v-cursor stays at this page
               ((eq selected-val :back)
                (freebox-ui--pick-category source-key))
               ((eq selected-val :prev)
                (freebox-ui--category-page source-key tid cat-name (1- page)))
               ((eq selected-val :next)
                (freebox-ui--category-page source-key tid cat-name (1+ page)))
               ((eq selected-val :gallery)
                (freebox-image-show-gallery
                 items cat-name page pagecount source-key tid))
               (t
                (freebox-ui-show-detail selected-val))))))))))

(defun freebox-ui--category-page-gallery (source-key tid cat-name page)
  "Like `freebox-ui--category-page' but opens gallery view directly.
Used by gallery M-n/M-p page navigation."
  (freebox-ui--save-v-cursor 'vod-list source-key tid cat-name page)
  (freebox-ui--loading (format "loading %s p.%d (gallery)" cat-name page))
  (freebox-http-get-category source-key tid page freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        (let* ((movie     (freebox-ui--jget data 'movie))
               (pagecount (or (freebox-ui--jget movie 'pagecount) 9999))
               (items     (freebox-ui--vec->list
                           (freebox-ui--jget movie 'videoList))))
          (if (not items)
              (message "FreeBox: no content in [%s] p.%d." cat-name page)
            ;; Preload posters
            (dolist (v items)
              (let ((pic (freebox-ui--jget v 'pic)))
                (when (and pic (stringp pic) (not (string-empty-p pic)))
                  (freebox-image-get pic #'ignore))))
            (freebox-image-show-gallery
             items cat-name page pagecount source-key tid)))))))

;;; --- VOD detail & episode selection ------------------------------------------

(defun freebox-ui-show-detail (vod-id &optional gallery-context)
  "Fetch VOD details for VOD-ID and prompt user to select an episode.
GALLERY-CONTEXT, if provided, is a list (SOURCE-KEY TID CAT-NAME PAGE)
to allow returning to the gallery from the poster detail view."
  (freebox-ui--loading "loading details")
  (freebox-http-get-detail freebox-ui-current-source vod-id freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        (let* ((movie (freebox-ui--jget data 'movie))
               (items (freebox-ui--vec->list
                       (freebox-ui--jget movie 'videoList)))
               (vod (and items (car items))))
          (if (not vod)
              (message "FreeBox: could not load VOD details.")
            ;; Record vod-detail node BEFORE showing menu, so C-g leaves
            ;; v-cursor at vod-detail and next v reopens this detail page.
            (freebox-ui--save-v-cursor 'vod-detail
                                       freebox-ui-current-source
                                       vod-id
                                       (freebox-ui--jget vod 'name))
            (let ((pic (freebox-ui--jget vod 'pic)))
              (if (and pic (stringp pic) (not (string-empty-p pic))
                       (display-images-p))
                  ;; Show poster preview buffer; episode selection via RET/p
                  (freebox-image-show-poster vod vod-id pic gallery-context)
                ;; No poster or terminal: original text-only flow
                (freebox-ui--show-vod-info vod)
                (freebox-ui--select-episode vod vod-id)))))))))

(defun freebox-ui--show-vod-info (vod)
  "Display brief metadata for VOD in the echo area."
  (let ((name  (freebox-ui--jget vod 'name))
        (note  (freebox-ui--jget vod 'note))
        (actor (freebox-ui--jget vod 'actor))
        (des   (freebox-ui--jget vod 'des)))
    (message "> %s%s%s%s"
             (or name "?")
             (if note  (format "  *%s" note) "")
             (if (and actor (stringp actor) (not (string-empty-p actor)))
                 (format "  [%s]" (truncate-string-to-width actor 40 nil nil "..."))
               "")
             (if (and des (stringp des) (not (string-empty-p des)))
                 (format "\n%s" (truncate-string-to-width des 120 nil nil "..."))
               ""))))

(defun freebox-ui--resolve-and-play (source-key play-flag episode-url direct-url title)
  "Resolve the final playback URL via /api/play then call empv.
EPISODE-URL is the raw episode URL (used as vodId).
DIRECT-URL is a fallback if the API fails."
  (freebox-ui--loading (format "resolving URL for \"%s\"" title))
  (freebox-http-get-play-url source-key play-flag episode-url freebox-ui-current-client-id
    (lambda (err pdata)
      (if err
          (if direct-url
              (progn (message "FreeBox: playing \"%s\" (direct)" title)
                     (freebox-empv-play-url direct-url title))
            (freebox-ui--error err))
        (let* ((nvp       (freebox-ui--jget pdata 'nameValuePairs))
               (final-url (freebox-ui--jget nvp 'url))
               (url (if (and final-url (stringp final-url)
                             (not (string-empty-p final-url)))
                        final-url
                      direct-url)))
          (if url
              (if (freebox-ui--error-url-p url)
                  ;; 播放 URL 返回错误，检测是否需要登录
                  (let ((err-msg (freebox-ui--extract-error-message url))
                        (login-type (freebox-ui--infer-login-type play-flag)))
                    (message "FreeBox: [%s] %s" play-flag err-msg)
                    (when (and login-type
                               (y-or-n-p (format "是否扫码登录%s？"
                                                 (pcase login-type
                                                   ("quark" "夸克网盘")
                                                   ("uc" "UC网盘")
                                                   ("bd" "百度网盘")
                                                   (_ login-type)))))
                      (freebox-ui--start-qr-login
                       login-type play-flag nil
                       (lambda ()
                         (freebox-ui--resolve-and-play
                          source-key play-flag episode-url direct-url title)))))
                (progn (message "FreeBox: playing \"%s\"" title)
                       (freebox-empv-play-url url title)))
            (message "FreeBox: could not resolve URL for \"%s\"." title)))))))

(defun freebox-ui--error-url-p (url)
  "Return non-nil if URL is an error placeholder returned by the spider.
Error URLs start with \"http://error.com/\"."
  (and (stringp url) (string-prefix-p "http://error.com/" url)))

(defun freebox-ui--extract-error-message (url)
  "Extract human-readable error message from an error URL.
\"http://error.com/网盘未配置\" -> \"网盘未配置\"
\"http://error.com/解析失败: cookie为空\" -> \"解析失败: cookie为空\""
  (if (string-match "^http://error\\.com/\\(.+\\)" url)
      (match-string 1 url)
    url))

(defun freebox-ui--infer-login-type (flag)
  "Infer QR login type from play FLAG. Return nil if unsupported.
Matches quark/uc/bd identifiers in the flag string."
  (cond ((string-match-p "quark\\|夸克" flag) "quark")
        ((string-match-p "uc\\|UC" flag) "uc")
        ((string-match-p "bd\\|百度\\|BD原画" flag) "bd")
        (t nil)))

;; ── QR 码登录流程 ──

(defvar freebox-ui--qr-poll-timer nil
  "Timer for polling QR login status.")

(defun freebox-ui--cancel-qr-poll ()
  "Cancel QR login polling timer."
  (when (timerp freebox-ui--qr-poll-timer)
    (cancel-timer freebox-ui--qr-poll-timer))
  (setq freebox-ui--qr-poll-timer nil))

(defun freebox-ui--qr-login-callback (drive-type retry-fn err data)
  "Callback for QR login: handle QR URL response.
DRIVE-TYPE and RETRY-FN are partially-applied values.
DATA contains url, token, and image flag. When image is non-nil,
display the QR image inside Emacs; otherwise use qrencode to
generate a PNG from the jump URL (for quark) and display it."
  (if err
      (freebox-ui--error err)
    (let ((qr-url (and data (alist-get 'url data)))
          (qr-token (and data (alist-get 'token data)))
          (is-image (and data (alist-get 'image data))))
      (if (or (not qr-url) (not qr-token))
          (message "FreeBox: 获取 %s 二维码失败" drive-type)
        (progn
          ;; 复制 URL 到剪贴板
          (kill-new qr-url)
          (if is-image
              ;; 图片 URL：用 freebox-image-get 下载并在 Emacs 内显示
              (freebox-image-get
               qr-url
               (lambda (path)
                 (if path
                     (freebox-ui--show-qr-image-in-buffer drive-type path qr-token retry-fn)
                   ;; 下载失败回退浏览器
                   (progn
                     (condition-case nil (browse-url qr-url) (error nil))
                     (freebox-ui--show-qr-text-buffer drive-type qr-url)
                     (freebox-ui--poll-qr-status drive-type qr-token retry-fn)))))
            ;; 非图片（夸克跳转 URL）：用 qrencode 生成 PNG 在 Emacs 内显示
            (freebox-ui--show-quark-qr-image drive-type qr-url qr-token retry-fn)))))))

(defun freebox-ui--show-qr-text-buffer (drive-type qr-url)
  "Show QR URL as clickable text in *FreeBox QR Login* buffer (fallback)."
  (with-help-window "*FreeBox QR Login*"
    (with-current-buffer "*FreeBox QR Login*"
      (insert (format "请用 %s App 扫描以下二维码登录:\n\n"
                      (pcase drive-type
                        ("quark" "夸克网盘") ("uc" "UC网盘")
                        ("bd" "百度网盘") (_ (upcase drive-type)))))
      (insert-button qr-url
                     'action (lambda (_) (browse-url qr-url))
                     'follow-link t)
      (insert "\n\n二维码已自动在浏览器中打开，如果未打开请点击上方链接\n")
      (insert "\nURL 已复制到剪贴板，也可手动访问\n\n")
      (insert (format "登录后会自动继续 (轮询中: %s)..." drive-type))))
  (message "FreeBox: %s 二维码已发送到浏览器，请用手机 App 扫码 (URL已复制)"
           (pcase drive-type
             ("quark" "夸克网盘") ("uc" "UC网盘")
             ("bd" "百度网盘") (_ (upcase drive-type)))))

(defun freebox-ui--show-qr-image-in-buffer (drive-type path token retry-fn)
  "Show QR image at PATH in *FreeBox QR Login* buffer and start polling.
DRIVE-TYPE is quark/uc/bd. TOKEN is the QR session token.
RETRY-FN is called after successful login."
  (with-help-window "*FreeBox QR Login*"
    (with-current-buffer "*FreeBox QR Login*"
      (insert (format "请用 %s App 扫描二维码登录:\n\n"
                      (pcase drive-type
                        ("quark" "夸克网盘") ("uc" "UC网盘")
                        ("bd" "百度网盘") (_ (upcase drive-type)))))
      (condition-case nil
          (let ((img (create-image path nil nil :max-width 400 :max-height 400 :ascent 'center)))
            (insert-image img "[QR]"))
        (error
         (insert (format "[二维码图片: %s]\n" path))))
      (insert "\n\n扫码后自动继续，无需操作...\n")))
  (message "FreeBox: %s 二维码已显示，请用手机 App 扫码"
           (pcase drive-type
             ("quark" "夸克网盘") ("uc" "UC网盘")
             ("bd" "百度网盘") (_ (upcase drive-type))))
  (freebox-ui--poll-qr-status drive-type token retry-fn))

(defun freebox-ui--show-quark-qr-image (drive-type url token retry-fn)
  "Use qrencode to generate a PNG from URL and display in Emacs.
Falls back to browse-url if qrencode is unavailable or fails."
  (if (executable-find "qrencode")
      (let ((png-file (expand-file-name
                       (format "/tmp/freebox-%s-qr.png" drive-type))))
        (condition-case nil
            (let ((ret (call-process "qrencode" nil nil nil
                                     "-o" png-file "-s" "10" "-l" "H" url)))
              (if (eq ret 0)
                  (freebox-ui--show-qr-image-in-buffer drive-type png-file token retry-fn)
                ;; qrencode 失败，回退浏览器
                (progn
                  (condition-case nil (browse-url url) (error nil))
                  (freebox-ui--show-qr-text-buffer drive-type url)
                  (freebox-ui--poll-qr-status drive-type token retry-fn))))
          (error
           (progn
             (condition-case nil (browse-url url) (error nil))
             (freebox-ui--show-qr-text-buffer drive-type url)
             (freebox-ui--poll-qr-status drive-type token retry-fn)))))
    ;; qrencode 未安装，回退浏览器
    (progn
      (condition-case nil (browse-url url) (error nil))
      (freebox-ui--show-qr-text-buffer drive-type url)
      (freebox-ui--poll-qr-status drive-type token retry-fn))))

(defun freebox-ui--start-qr-login (drive-type flag share-link retry-fn)
  "Start QR code login flow for DRIVE-TYPE (quark/uc/bd)."
  (freebox-ui--loading (format "获取 %s 二维码" drive-type))
  (freebox-http-get-qr-login
   drive-type freebox-ui-current-client-id
   (apply-partially #'freebox-ui--qr-login-callback drive-type retry-fn)))

(defun freebox-ui--qr-timer-fn (drive-type token retry-fn)
  "Timer function for QR status polling.
Called by run-at-time timer."
  (freebox-http-poll-qr-status
   drive-type token freebox-ui-current-client-id
   (apply-partially #'freebox-ui--qr-poll-callback drive-type retry-fn)))

(defun freebox-ui--qr-poll-callback (drive-type retry-fn err data)
  "Callback for QR status polling.
If login succeeded, cancel timer and retry resolve."
  (if err
      (progn
        (freebox-ui--cancel-qr-poll)
        (freebox-ui--error err))
    (let ((status (and data (alist-get 'status data)))
          (msg (and data (alist-get 'message data))))
      (cond
       ((string= status "success")
        (freebox-ui--cancel-qr-poll)
        (message "FreeBox: %s 登录成功！正在刷新..." drive-type)
        (when (buffer-live-p (get-buffer "*FreeBox QR Login*"))
          (kill-buffer "*FreeBox QR Login*"))
        (funcall retry-fn))
       ((string= status "failed")
        (freebox-ui--cancel-qr-poll)
        (message "FreeBox: %s 登录失败 - %s" drive-type (or msg "")))
       ((string= status "expired")
        (freebox-ui--cancel-qr-poll)
        (message "FreeBox: %s 二维码已过期，请重新操作" drive-type))
       ((string= status "pending")
        (message "FreeBox: 等待 %s App 扫码..." drive-type))
       (t
        (message "FreeBox: %s 扫码状态: %s" drive-type (or status "unknown")))))))

(defun freebox-ui--poll-qr-status (drive-type token retry-fn)
  "Poll QR login status, retry resolve after success.
BD uses 30-second interval (unicast is long-poll), others 3 seconds."
  (freebox-ui--cancel-qr-poll)
  (let ((interval (if (string= drive-type "bd") 30 3)))
    (setq freebox-ui--qr-poll-timer
          (run-at-time interval interval
                       (apply-partially #'freebox-ui--qr-timer-fn
                                        drive-type token retry-fn))))
  ;; 立即轮询一次
  (freebox-http-poll-qr-status
   drive-type token freebox-ui-current-client-id
   (apply-partially #'freebox-ui--qr-poll-callback drive-type retry-fn)))

(defun freebox-ui--resolve-after-qr-login (vod vod-id selected-flag share-link)
  "Retry resolveShare for FLAG after QR login.
Resolves SHARE-LINK and shows episodes."
  (freebox-ui--loading (format "重新解析 %s" selected-flag))
  (freebox-http-resolve-share
   freebox-ui-current-source selected-flag share-link freebox-ui-current-client-id
   (lambda (err data)
     (if err
         (freebox-ui--error err)
       (let ((real-urls (and data (alist-get 'urls data))))
         (if (or (not real-urls) (string-empty-p real-urls))
             (message "FreeBox: [%s] 仍无法解析，请检查网盘配置" selected-flag)
           (freebox-ui--pick-episode vod vod-id selected-flag real-urls nil)))))))

(defun freebox-ui--pick-episode (vod vod-id flag url-str &optional share-link)
  "Let user pick an episode from URL-STR under FLAG, then play it.
VOD is the full VOD object (needed for :back recursion).
SHARE-LINK is the original share URL, used for retry after QR login."
  (let* ((ep-parts (and url-str (split-string url-str "#")))
         (candidates
          (and ep-parts
               (delq nil
                     (mapcar (lambda (part)
                               (when (string-match "^\\(.*?\\)\\$\\(.*\\)$" part)
                                 (cons (match-string 1 part)
                                       (match-string 2 part))))
                             ep-parts))))
         (cands-with-back (cons (cons freebox-ui--back-label :back) candidates)))
    (if (not candidates)
        ;; Check if url-str is an error URL (from failed resolveShare)
        (if (freebox-ui--error-url-p url-str)
            (let ((err-msg (freebox-ui--extract-error-message url-str)))
              (message "FreeBox: [%s] %s" flag err-msg)
              ;; 如果是"网盘未配置"，提供扫码登录选项
              (let ((login-type (freebox-ui--infer-login-type flag)))
                (if (and login-type
                         (string-match-p "网盘未配置\\|未配置" err-msg)
                         share-link)
                    (when (y-or-n-p (format "是否扫码登录%s？"
                                            (pcase login-type
                                              ("quark" "夸克网盘")
                                              ("uc" "UC网盘")
                                              ("bd" "百度网盘")
                                              (_ login-type))))
                      (freebox-ui--start-qr-login
                       login-type flag share-link
                       (apply-partially #'freebox-ui--resolve-after-qr-login
                                        vod vod-id flag share-link)))
                  (when login-type
                    (message "FreeBox: 可按 %s 菜单键扫码登录%s"
                             (upcase (substring login-type 0 1))
                             (pcase login-type
                               ("quark" "夸克网盘")
                               ("uc" "UC网盘")
                               ("bd" "百度网盘")
                               (_ login-type)))))))
          (message "FreeBox: [%s] 无可播放剧集" flag))
      (let* ((selected-ep
              (freebox-ui--completing-read
               (format "Episode (%d): " (length candidates))
               cands-with-back))
             (selected-val (and selected-ep
                                (cdr (assoc selected-ep cands-with-back)))))
        (cond
         ((null selected-val) nil) ; C-g: cancel, v-cursor stays at vod-detail
         ((eq selected-val :back)
          ;; Explicit 返回: back to flag selection
          (freebox-ui--select-episode vod vod-id))
         (t
          ;; Real selection: record episode node, then play
          (freebox-ui--save-v-cursor 'episode
                                     freebox-ui-current-source
                                     vod-id
                                     flag)
          (freebox-ui--resolve-and-play
           freebox-ui-current-source flag selected-val
           selected-val selected-ep)))))))

(defun freebox-ui--select-episode (vod vod-id)
  "Let user pick a play-flag and episode from VOD, then play it.
Data structure: urlBean.infoList = [{flag, urls}]
urls = 'ep_name$ep_url#ep_name$ep_url#...'

VOD-ID is the stable id passed from `freebox-ui-show-detail'.
C-g at any sub-level cancels silently; v-cursor stays at vod-detail
so next v reopens this detail page."
  (let* ((url-bean  (freebox-ui--jget vod 'urlBean))
         (info-list (freebox-ui--vec->list
                     (freebox-ui--jget url-bean 'infoList)))
         (flags     (mapcar (lambda (info) (freebox-ui--jget info 'flag))
                            info-list)))
    (if (not flags)
        (message "FreeBox: no playable episodes found.")
      ;; --- Flag selection ---
      (let* ((flags-with-back (cons freebox-ui--back-label flags))
             (selected-flag
              (freebox-ui--completing-read
               (format "Play source (%d available): " (length flags))
               flags-with-back)))
        (cond
         ((null selected-flag) nil) ; C-g: cancel, v-cursor stays at vod-detail
         ((equal selected-flag freebox-ui--back-label)
          ;; Explicit 返回: go back to vod-list
          (freebox-ui--with-source #'freebox-ui--pick-category))
         (t
          ;; --- Resolve or use cached episodes ---
          (let* ((info (cl-find selected-flag info-list
                                :test #'equal
                                :key (lambda (i) (freebox-ui--jget i 'flag))))
                 (url-str  (and info (freebox-ui--jget info 'urls))))
            (if (and url-str (string-match "RESOLVE:\\(.+\\)" url-str))
                ;; Delayed resolution: fetch real episodes from backend
                (let ((share-link (match-string 1 url-str)))
                  (freebox-ui--loading (format "resolving %s" selected-flag))
                  (freebox-http-resolve-share
                   freebox-ui-current-source selected-flag
                   share-link freebox-ui-current-client-id
                   (lambda (err data)
                     (if err
                         (freebox-ui--error err)
                       (let ((real-urls (and data (alist-get 'urls data))))
                         (if (or (not real-urls) (string-empty-p real-urls))
                             (message "FreeBox: [%s] 解析失败，请检查网盘配置" selected-flag)
                           (freebox-ui--pick-episode vod vod-id selected-flag real-urls share-link)))))))
              ;; Already resolved: proceed directly
              (freebox-ui--pick-episode vod vod-id selected-flag url-str nil)))))))))

;;; --- Menu Persistence --------------------------------------------------------

(defun freebox-ui-restore-state ()
  "Restore UI state from persistent storage.
Called on menu startup to recover previous selections."
  (freebox-ui-init))

(defun freebox-ui-show-current-state ()
  "Display current menu state as a status string.
Used by transient menus to show [client] [source] indicators."
  (let ((client (freebox-persist-get-client-name))
        (source (freebox-persist-get-source-name)))
    (format "%s%s"
            (if client (format "[%s] " client) "")
            (if source (format "[%s]" source) ""))))

;;; --- Resume (v-cursor restore) -----------------------------------------------

(defun freebox-ui-resume ()
  "Resume browsing from the last remembered navigation node (v-cursor).

Restores to the deepest valid node recorded:
  vod-list   -> directly opens the saved category page (e.g. page 3)
  category   -> if tid is valid: directly enters that category page 1
                if tid is nil (was mid-selection): re-shows category list
  vod-detail -> directly opens the vod detail page
  episode    -> re-opens the parent vod detail (nearest valid parent)
  nil        -> falls back to full select-source -> category flow

If the parent source no longer matches the current source, falls back
to the nearest valid parent (category -> source -> client)."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (let* ((cursor   (freebox-persist-get-v-cursor))
            (type     (and cursor (alist-get 'type cursor)))
            (src-key  (and cursor (alist-get 'source-key cursor)))
            (src-ok   (and freebox-ui-current-source
                           (equal freebox-ui-current-source src-key))))
       (cond
        ((equal type "vod-list")
         (let ((tid      (alist-get 'tid cursor))
               (cat-name (alist-get 'cat-name cursor))
               (page     (or (alist-get 'page cursor) 1)))
           (if src-ok
               (freebox-ui--category-page freebox-ui-current-source tid cat-name page)
             (message "FreeBox: source changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ((equal type "category")
         (let ((tid  (alist-get 'tid cursor))
               (name (alist-get 'name cursor)))
           (if src-ok
               (if (and tid (not (equal tid "nil")) (not (string-empty-p (or tid ""))))
                   (progn
                     (freebox-ui--save-category tid name)
                     (freebox-ui--category-page freebox-ui-current-source tid name 1))
                 (freebox-ui--pick-category freebox-ui-current-source))
             (message "FreeBox: source changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ((equal type "vod-detail")
         (let ((vod-id (alist-get 'vod-id cursor)))
           (if (and src-ok vod-id)
               (freebox-ui-show-detail vod-id)
             (message "FreeBox: context changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ((equal type "episode")
         (let ((vod-id (alist-get 'vod-id cursor)))
           (if (and src-ok vod-id)
               (freebox-ui-show-detail vod-id)
             (message "FreeBox: context changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        (t
         (freebox-ui--with-source #'freebox-ui--pick-category)))))))

;;; --- Live TV ------------------------------------------------------------------

(defun freebox-ui-select-live-client ()
  "Interactively select a FreeBox live TV client (SINGLE_LIVE source).
Saves selection to persistent state.  Equivalent of `freebox-select-client'
for live TV."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (freebox-ui--loading "fetching live TV sources")
     (freebox-http-get-live-clients
      (lambda (err clients)
        (if err
            (freebox-ui--error err)
          (if (not clients)
              (message "FreeBox: 没有直播源。请在 FreeBox 添加 SINGLE_LIVE 客户端。")
            (let ((picked (freebox-ui--pick-live-client-from-list clients)))
              (when picked
                (freebox-ui--save-live-client (car picked) (cdr picked))
                (message "FreeBox: 直播源 -> [%s]" (cdr picked)))))))))))

(defun freebox-ui--pick-live-client-from-list (clients)
  "Prompt user to pick from CLIENTS list (live TV).
Returns (ID . NAME) cons, or nil if cancelled."
  (let* ((items (freebox-ui--vec->list clients))
         (candidates (mapcar (lambda (c)
                               (cons (or (freebox-ui--jget c 'name)
                                         (freebox-ui--jget c 'id))
                                     (freebox-ui--jget c 'id)))
                             items))
         (selected-name (freebox-ui--completing-read
                         "FreeBox -- Select live TV source: " candidates)))
    (when selected-name
      (cons (cdr (assoc selected-name candidates)) selected-name))))

(defun freebox-ui--save-live-client (id name)
  "Save live TV client selection (ID, NAME) to state."
  (setq freebox-ui-current-live-client-id   id
        freebox-ui-current-live-client-name name)
  (freebox-persist-set 'live-client-id id)
  (freebox-persist-set 'live-client-name name))

(defun freebox-ui--with-live-client (fn)
  "Ensure a live TV client is selected, then call FN with client-id.
First tries persisted selection, then prompts user."
  (if freebox-ui-current-live-client-id
      (funcall fn freebox-ui-current-live-client-id
               freebox-ui-current-live-client-name)
    (freebox-ui--loading "fetching live TV sources")
    (freebox-http-get-live-clients
     (lambda (err clients)
       (if err
           (freebox-ui--error err)
         (if (not clients)
             (message "FreeBox: 没有直播源。请在 FreeBox 添加 SINGLE_LIVE 客户端。")
           (let* ((items (freebox-ui--vec->list clients))
                  (picked (if (= (length items) 1)
                              (let* ((c (car items)))
                                (cons (freebox-ui--jget c 'id)
                                      (freebox-ui--jget c 'name)))
                            (freebox-ui--pick-live-client-from-list clients))))
             (when picked
               (freebox-ui--save-live-client (car picked) (cdr picked))
               (message "FreeBox: 直播源 -> [%s]" (cdr picked))
               (funcall fn (car picked) (cdr picked))))))))))

(defun freebox-ui-live ()
  "Browse and play FreeBox live TV channels.
Uses the selected live TV client if set, otherwise prompts to select one."
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-live-client #'freebox-ui--live-load-channels))))

(defun freebox-ui--live-load-channels (client-id client-name)
  "Fetch channel groups for CLIENT-ID (displayed as CLIENT-NAME) and show groups."
  (freebox-ui--loading (format "加载直播源 [%s]" client-name))
  (freebox-http-get-live-channels
   client-id
   (lambda (err groups)
     (if err
         (freebox-ui--error err)
       (let* ((items (freebox-ui--vec->list groups)))
         (if (not items)
             (message "FreeBox: 直播源 [%s] 没有频道" client-name)
           (freebox-ui--live-pick-group items client-id client-name)))))))

(defun freebox-ui--live-pick-group (groups client-id client-name)
  "Let user pick a channel group from GROUPS.
GROUPS is the list of LiveChannelGroup objects from the backend.
CLIENT-ID and CLIENT-NAME are for re-entry (back to client selection)."
  (let* ((candidates (mapcar (lambda (g)
                               (let ((title (or (freebox-ui--jget g 'title) "未分组"))
                                     (size (length (freebox-ui--vec->list
                                                    (freebox-ui--jget g 'channels)))))
                                 (cons (format "%s (%d)" title size) g)))
                             groups))
         (all-cands (cons (cons freebox-ui--back-label :back) candidates))
         (selected-name (freebox-ui--completing-read
                         (format "FreeBox [%s] 分组: " client-name) all-cands)))
    (cond
     ((null selected-name) nil)            ; C-g
     ((eq (cdr (assoc selected-name all-cands)) :back)
      (freebox-ui-select-live-client))     ; back to live source selection
     (t
      (let ((group (cdr (assoc selected-name all-cands))))
        (freebox-ui--live-pick-channel group groups client-id client-name))))))

(defun freebox-ui--live-pick-channel (group groups client-id client-name)
  "Let user pick a channel from GROUP, then a line if multiple, then play.
GROUPS, CLIENT-ID, CLIENT-NAME for re-entry."
  (let* ((channels (freebox-ui--vec->list (freebox-ui--jget group 'channels)))
         (candidates (mapcar (lambda (ch)
                               (cons (or (freebox-ui--jget ch 'title) "?") ch))
                             channels))
         (all-cands (cons (cons freebox-ui--back-label :back) candidates))
         (selected-name (freebox-ui--completing-read
                         "FreeBox 频道: " all-cands)))
    (cond
     ((null selected-name) nil)            ; C-g
     ((eq (cdr (assoc selected-name all-cands)) :back)
      (freebox-ui--live-pick-group groups client-id client-name)) ; back to groups
     (t
      (let* ((channel (cdr (assoc selected-name all-cands)))
             (lines (freebox-ui--vec->list (freebox-ui--jget channel 'lines)))
             (channel-title (freebox-ui--jget channel 'title)))
        (cond
         ((not lines)
          (message "FreeBox: 频道 [%s] 没有可播放线路" channel-title))
         ((= (length lines) 1)
          (freebox-ui--live-play (car lines) channel-title))
         (t
          (freebox-ui--live-pick-line lines channel-title groups client-id client-name))))))))

(defun freebox-ui--live-pick-line (lines channel-title groups client-id client-name)
  "Let user pick a line from LINES, then play.
CHANNEL-TITLE is the channel name for display.
GROUPS, CLIENT-ID, CLIENT-NAME for re-entry via back."
  (let* ((candidates (mapcar (lambda (l)
                               (cons (or (freebox-ui--jget l 'title) "线路") l))
                             lines))
         (all-cands (append
                     (list (cons freebox-ui--back-label :back))
                     candidates))
         (selected-name (freebox-ui--completing-read
                         (format "FreeBox [%s] 线路: " channel-title) all-cands)))
    (cond
     ((null selected-name) nil)            ; C-g
     ((eq (cdr (assoc selected-name all-cands)) :back)
      ;; back: re-enter channel picker (need group context — rebuild from groups)
      ;; Simplified: go back to group selection.
      (freebox-ui--live-pick-group groups client-id client-name))
     (t
      (freebox-ui--live-play (cdr (assoc selected-name all-cands)) channel-title)))))

(defun freebox-ui--live-play (line title)
  "Play live channel LINE (with url field) under TITLE."
  (let ((url (freebox-ui--jget line 'url)))
    (if (or (not url) (string-empty-p url))
        (message "FreeBox: 频道 [%s] 线路无 URL" title)
      (message "FreeBox: 播放直播 [%s]" title)
      (freebox-empv-play-url url title))))

(provide 'freebox-ui)
;;; freebox-ui.el ends here
