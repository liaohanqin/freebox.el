;;; freebox-ui.el --- UI components for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Completing-read based UI for FreeBox.
;; json-read returns alist with symbol keys, so we use alist-get throughout.
;;
;; Workflow:
;;   1. Select client (from /api/clients -- saved CATVOD_SPIDER configs)
;;   2. Select source (from /api/sources?clientId=...)
;;   3. Browse / search within that source

;;; Code:

(require 'freebox-http)
(require 'freebox-persist)

;;; --- Constants ----------------------------------------------------------------

(defconst freebox-ui--node-levels
  '((category  . 1)
    (vod-list  . 2)
    (vod-detail . 3)
    (episode   . 4))
  "Node level numbers for v-cursor hierarchy comparison.
 Higher numbers are deeper in the tree.")

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

;;; --- Initialization ----------------------------------------------------------

(defun freebox-ui-init ()
  "Initialize UI with persisted state."
  (freebox-persist-init)
  (let ((client-id   (freebox-persist-get-client-id))
        (client-name (freebox-persist-get-client-name))
        (source-key  (freebox-persist-get-source-key))
        (source-name (freebox-persist-get-source-name))
        (cat-tid     (freebox-persist-get-category-tid))
        (cat-name    (freebox-persist-get-category-name)))
    (setq freebox-ui-current-client-id    client-id
          freebox-ui-current-client-name  client-name
          freebox-ui-current-source       source-key
          freebox-ui-current-source-name  source-name
          freebox-ui-current-category-tid  cat-tid
          freebox-ui-current-category-name cat-name)))

;;; --- Internal helpers --------------------------------------------------------

(defun freebox-ui--loading (msg)
  "Display MSG in the echo area as a loading indicator."
  (message "FreeBox: %s..." msg))

(defun freebox-ui--error (msg)
  "Display MSG as an error."
  (message "FreeBox error: %s" msg))

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
         (selected-name (completing-read "FreeBox -- Select client config: "
                                         candidates nil t)))
    (when (and selected-name (not (string-empty-p selected-name)))
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

(defun freebox-ui--node-level (type-str)
  "Return numeric level for node TYPE-STR, or 0 if unknown."
  (or (alist-get (intern (or type-str "")) freebox-ui--node-levels) 0))

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
         (selected-name (completing-read "FreeBox -- Select source: " candidates nil t)))
    (when (and selected-name (not (string-empty-p selected-name)))
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
    (when (>= cursor-level (freebox-ui--node-level 'vod-list))
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
  (let ((keyword (read-string
                  (format "FreeBox search [%s]: "
                          (or freebox-ui-current-source-name source-key)))))
    (when (not (string-empty-p keyword))
      (freebox-ui--loading (format "searching \"%s\"" keyword))
      (freebox-http-search source-key keyword freebox-ui-current-client-id
        (lambda (err data)
          (if err
              (freebox-ui--error err)
            ;; Search result path: data.movie.videoList
            (let* ((movie  (freebox-ui--jget data 'movie))
                   (items  (freebox-ui--vec->list
                            (freebox-ui--jget movie 'videoList)))
                   (candidates (mapcar (lambda (v)
                                         (cons (freebox-ui--jget v 'name)
                                               (freebox-ui--jget v 'id)))
                                       items)))
              (if (not candidates)
                  (message "FreeBox: no results for \"%s\"." keyword)
                (let* ((selected-name
                        (completing-read
                         (format "Results for \"%s\" (%d): "
                                 keyword (length candidates))
                         candidates nil t))
                       (selected-id (cdr (assoc selected-name candidates))))
                  (freebox-ui-show-detail selected-id))))))))))

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
  "Fetch top-level categories for SOURCE-KEY and let user pick one."
  ;; Record that we're at the category-selection node
  (freebox-ui--save-v-cursor 'category source-key nil "(selecting)")
  (freebox-ui--loading "fetching categories")
  (freebox-http-get-categories source-key freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; Category path: data.classes.sortList, each item's id field as tid
        (let* ((classes    (freebox-ui--jget data 'classes))
               (items      (freebox-ui--vec->list
                            (freebox-ui--jget classes 'sortList)))
               (candidates (mapcar (lambda (c)
                                     (cons (or (freebox-ui--jget c 'name)
                                               (freebox-ui--jget c 'id))
                                           (freebox-ui--jget c 'id)))
                                   items)))
          (if (not candidates)
              (message "FreeBox: no categories found.")
            (let* ((selected-name
                    (completing-read "FreeBox -- Category: " candidates nil t))
                   (selected-tid (cdr (assoc selected-name candidates))))
              ;; Save category selection
              (freebox-ui--save-category selected-tid selected-name)
              ;; Update v-cursor to category node with actual tid
              (freebox-ui--save-v-cursor 'category source-key selected-tid selected-name)
              (freebox-ui--category-page
               source-key selected-tid selected-name 1))))))))

(defun freebox-ui--category-page (source-key tid cat-name page)
  "Fetch page PAGE of category TID in SOURCE-KEY.
  Records current page as v-cursor for later resumption."
  ;; Record current position as vod-list node
  (freebox-ui--save-v-cursor 'vod-list source-key tid cat-name page)
  (freebox-ui--loading (format "loading %s p.%d" cat-name page))
  (freebox-http-get-category source-key tid page freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; Category content path: data.movie.videoList (same as search/detail)
        (let* ((movie      (freebox-ui--jget data 'movie))
               (pagecount  (or (freebox-ui--jget movie 'pagecount) 9999))
               (items      (freebox-ui--vec->list
                            (freebox-ui--jget movie 'videoList)))
               (candidates (mapcar (lambda (v)
                                     (cons (freebox-ui--jget v 'name)
                                           (freebox-ui--jget v 'id)))
                                   items)))
          (if (not candidates)
              (message "FreeBox: no content in [%s] p.%d." cat-name page)
            (let* ((has-next   (< page pagecount))
                   (has-prev   (> page 1))
                   (next-label (format "-- Next page (p.%d/%d) --" (1+ page) pagecount))
                   (prev-label (format "-- Previous page (p.%d/%d) --" (1- page) pagecount))
                   (all-cands  (append
                                (when has-prev (list (cons prev-label :prev)))
                                candidates
                                (when has-next (list (cons next-label :next)))))
                   (selected-name
                    (completing-read
                     (format "%s p.%d/%d (%d items): "
                             cat-name page pagecount (length candidates))
                     all-cands nil t))
                   (selected-val (cdr (assoc selected-name all-cands))))
              (cond
               ((eq selected-val :prev)
                (freebox-ui--category-page source-key tid cat-name (1- page)))
               ((eq selected-val :next)
                (freebox-ui--category-page source-key tid cat-name (1+ page)))
               (t
                (freebox-ui-show-detail selected-val))))))))))

;;; --- VOD detail & episode selection ------------------------------------------

(defun freebox-ui-show-detail (vod-id)
  "Fetch VOD details for VOD-ID and prompt user to select an episode."
  (freebox-ui--loading "loading details")
  (freebox-http-get-detail freebox-ui-current-source vod-id freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; Detail path: data.movie.videoList, take first item
        (let* ((movie (freebox-ui--jget data 'movie))
               (items (freebox-ui--vec->list
                       (freebox-ui--jget movie 'videoList)))
               (vod (and items (car items))))
          (if (not vod)
              (message "FreeBox: could not load VOD details.")
            (freebox-ui--show-vod-info vod)
            (freebox-ui--select-episode vod vod-id)))))))

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
          ;; Fall back to direct-url on error
          (if direct-url
              (progn (message "FreeBox: playing \"%s\" (direct)" title)
                     (freebox-empv-play-url direct-url title))
            (freebox-ui--error err))
        ;; play API returns data.nameValuePairs.url
        (let* ((nvp       (freebox-ui--jget pdata 'nameValuePairs))
               (final-url (freebox-ui--jget nvp 'url))
               (url (if (and final-url (stringp final-url)
                             (not (string-empty-p final-url)))
                        final-url
                      direct-url)))
          (if url
              (progn (message "FreeBox: playing \"%s\"" title)
                     (freebox-empv-play-url url title))
            (message "FreeBox: could not resolve URL for \"%s\"." title)))))))

(defun freebox-ui--select-episode (vod _vod-id)
  "Let user pick a play-flag and episode from VOD, then play it.
Data structure: urlBean.infoList = [{flag, urls}]
urls = 'ep_name$ep_url#ep_name$ep_url#...'"
  (let* ((url-bean  (freebox-ui--jget vod 'urlBean))
         (info-list (freebox-ui--vec->list
                     (freebox-ui--jget url-bean 'infoList)))
         (flags     (mapcar (lambda (info) (freebox-ui--jget info 'flag))
                            info-list)))
    (if (not flags)
        (message "FreeBox: no playable episodes found.")
      (let* ((selected-flag
              (if (> (length flags) 1)
                  (completing-read
                   (format "Play source (%d available): " (length flags))
                   flags nil t)
                (car flags)))
             ;; Find info item for selected flag
             (info (cl-find selected-flag info-list
                            :test #'equal
                            :key (lambda (i) (freebox-ui--jget i 'flag))))
             ;; Parse urls string: each item "ep_name$ep_url", # separated
             (url-str  (and info (freebox-ui--jget info 'urls)))
             (ep-parts (and url-str (split-string url-str "#")))
             (candidates
              (and ep-parts
                   (delq nil
                         (mapcar (lambda (part)
                                   (when (string-match "^\\(.*?\\)\\$\\(.*\\)$" part)
                                     (cons (match-string 1 part)
                                           (match-string 2 part))))
                                 ep-parts)))))
        (if (not candidates)
            (message "FreeBox: no episodes under [%s]." selected-flag)
          (let* ((selected-ep
                  (completing-read
                   (format "Episode (%d): " (length candidates))
                   candidates nil t))
                 ;; episode URL passed directly as vodId to /api/play
                 (ep-url (cdr (assoc selected-ep candidates))))
            (freebox-ui--resolve-and-play
             freebox-ui-current-source selected-flag ep-url
             ep-url selected-ep)))))))

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
  vod-list  → directly opens the saved category page (e.g. page 3)
  category  → if tid is valid: directly enters that category page 1
              if tid is nil (was mid-selection): re-shows category list
  vod-detail→ directly opens the vod detail page
  nil       → falls back to full select-source → category flow

If the parent source no longer matches the current source, falls back
to the nearest valid parent (category → source → client)."
  (interactive)
  (freebox-http-ensure-server
   (lambda ()
     (let* ((cursor   (freebox-persist-get-v-cursor))
            (type     (and cursor (alist-get 'type cursor)))
            (src-key  (and cursor (alist-get 'source-key cursor)))
            ;; Check if saved source matches current source
            (src-ok   (and freebox-ui-current-source
                           (equal freebox-ui-current-source src-key))))
       (cond
        ;; vod-list: restore to the exact page in the saved category
        ((equal type "vod-list")
         (let ((tid      (alist-get 'tid cursor))
               (cat-name (alist-get 'cat-name cursor))
               (page     (or (alist-get 'page cursor) 1)))
           (if src-ok
               (freebox-ui--category-page freebox-ui-current-source tid cat-name page)
             ;; Source mismatch → fall back to category selection
             (message "FreeBox: source changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ;; category with valid tid: directly enter that category (page 1)
        ;; category with nil tid (was mid-selection): re-show category list
        ((equal type "category")
         (let ((tid  (alist-get 'tid cursor))
               (name (alist-get 'name cursor)))
           (if src-ok
               (if (and tid (not (equal tid "nil")) (not (string-empty-p (or tid ""))))
                   ;; Valid tid saved: go directly to page 1 of that category
                   (progn
                     (freebox-ui--save-category tid name)
                     (freebox-ui--category-page freebox-ui-current-source tid name 1))
                 ;; No valid tid: re-show category selection list
                 (freebox-ui--pick-category freebox-ui-current-source))
             ;; Source mismatch → fall back to category selection
             (message "FreeBox: source changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ;; vod-detail: restore to the vod detail page
        ((equal type "vod-detail")
         (let ((vod-id (alist-get 'vod-id cursor)))
           (if (and src-ok vod-id)
               (freebox-ui-show-detail vod-id)
             ;; Source mismatch or no vod-id → fall back to category
             (message "FreeBox: context changed, resuming from category selection.")
             (freebox-ui--with-source #'freebox-ui--pick-category))))

        ;; nil or unknown: full flow from source → category
        (t
         (freebox-ui--with-source #'freebox-ui--pick-category)))))))

(provide 'freebox-ui)
;;; freebox-ui.el ends here
