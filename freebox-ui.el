;;; freebox-ui.el --- UI components for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Completing-read based UI for FreeBox.
;; json-read returns alist with symbol keys, so we use alist-get throughout.
;;
;; Workflow:
;;   1. Select client (from /api/clients — saved CATVOD_SPIDER configs)
;;   2. Select source (from /api/sources?clientId=...)
;;   3. Browse / search within that source

;;; Code:

(require 'freebox-http)

;;; ─── State ────────────────────────────────────────────────────────────────────

(defvar freebox-ui-current-client-id nil
  "ID of the currently selected FreeBox client configuration.")

(defvar freebox-ui-current-client-name nil
  "Display name of the currently selected FreeBox client configuration.")

(defvar freebox-ui-current-source nil
  "Key of the currently selected FreeBox source.")

(defvar freebox-ui-current-source-name nil
  "Display name of the currently selected FreeBox source.")

;;; ─── Internal helpers ─────────────────────────────────────────────────────────

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

;;; ─── Client selection ─────────────────────────────────────────────────────────

(defun freebox-ui--pick-client-from-list (clients)
  "Prompt user to pick from CLIENTS list.
Returns (ID . NAME) cons, or nil if cancelled."
  (let* ((items (freebox-ui--vec->list clients))
         (candidates (mapcar (lambda (c)
                               (cons (freebox-ui--jget c 'name)
                                     (freebox-ui--jget c 'id)))
                             items))
         (selected-name (completing-read "FreeBox — Select client config: "
                                         candidates nil t)))
    (when (and selected-name (not (string-empty-p selected-name)))
      (cons (cdr (assoc selected-name candidates)) selected-name))))

(defun freebox-ui-select-client ()
  "Interactively select a FreeBox client configuration.
This determines which video source config (JSON URL) is used."
  (interactive)
  (freebox-ui--loading "fetching client configs")
  (freebox-http-get-clients
   (lambda (err clients)
     (if err
         (freebox-ui--error err)
       (if (not clients)
           (message "FreeBox: no client configs found. Add a source in FreeBox app first.")
         (let ((picked (freebox-ui--pick-client-from-list clients)))
           (when picked
             ;; Reset source when client changes
             (setq freebox-ui-current-client-id   (car picked)
                   freebox-ui-current-client-name (cdr picked)
                   freebox-ui-current-source      nil
                   freebox-ui-current-source-name nil)
             (message "FreeBox: client → [%s]" (cdr picked)))))))))

(defun freebox-ui--with-client (fn)
  "Ensure a client is selected, then call FN with client-id."
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
                              (let* ((c (car items))
                                     (id   (freebox-ui--jget c 'id))
                                     (name (freebox-ui--jget c 'name)))
                                (cons id name))
                            (freebox-ui--pick-client-from-list clients))))
             (when picked
               (setq freebox-ui-current-client-id   (car picked)
                     freebox-ui-current-client-name (cdr picked)
                     freebox-ui-current-source      nil
                     freebox-ui-current-source-name nil)
               (message "FreeBox: client → [%s]" (cdr picked))
               (funcall fn (car picked))))))))))

;;; ─── Source selection ─────────────────────────────────────────────────────────

(defun freebox-ui--pick-source-from-list (sources)
  "Prompt user to pick from SOURCES (alist list from json-read).
Returns (KEY . NAME) cons, or nil if cancelled."
  (let* ((items (freebox-ui--vec->list sources))
         (candidates (mapcar (lambda (s)
                               (cons (freebox-ui--jget s 'name)
                                     (freebox-ui--jget s 'key)))
                             items))
         (selected-name (completing-read "FreeBox — Select source: " candidates nil t)))
    (when (and selected-name (not (string-empty-p selected-name)))
      (cons (cdr (assoc selected-name candidates)) selected-name))))

(defun freebox-ui-select-source ()
  "Interactively select (or change) the FreeBox source.
Selects a client first if none is chosen."
  (interactive)
  (freebox-ui--with-client #'freebox-ui--do-select-source))

(defun freebox-ui--do-select-source (client-id)
  "Fetch sources for CLIENT-ID and prompt user to pick one."
  (freebox-ui--loading "fetching sources")
  (freebox-http-get-sources client-id
   (lambda (err sources)
     (if err
         (freebox-ui--error err)
       (if (not sources)
           (message "FreeBox: no sources available.")
         (let ((picked (freebox-ui--pick-source-from-list sources)))
           (when picked
             (setq freebox-ui-current-source      (car picked)
                   freebox-ui-current-source-name (cdr picked))
             (message "FreeBox: source → [%s]" (cdr picked)))))))))

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
                  (setq freebox-ui-current-source      (car picked)
                        freebox-ui-current-source-name (cdr picked))
                  (message "FreeBox: source → %s" (cdr picked))
                  (funcall fn (car picked))))))))))))

;;; ─── Search ───────────────────────────────────────────────────────────────────

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
            ;; 搜索结果路径: data.movie.videoList
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

;;; ─── Category browse ──────────────────────────────────────────────────────────

(defun freebox-ui-browse-category ()
  "Browse FreeBox content by category.
Auto-starts the backend if `freebox-http-server-script' is configured."
  (interactive)
  (freebox-http-ensure-server
   (lambda () (freebox-ui--with-source #'freebox-ui--pick-category))))

(defun freebox-ui--pick-category (source-key)
  "Fetch top-level categories for SOURCE-KEY and let user pick one."
  (freebox-ui--loading "fetching categories")
  (freebox-http-get-categories source-key freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; 分类路径: data.classes.sortList，每项 id 字段作为 tid
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
                    (completing-read "FreeBox — Category: " candidates nil t))
                   (selected-tid (cdr (assoc selected-name candidates))))
              (freebox-ui--category-page
               source-key selected-tid selected-name 1))))))))

(defun freebox-ui--category-page (source-key tid cat-name page)
  "Fetch page PAGE of category TID in SOURCE-KEY."
  (freebox-ui--loading (format "loading %s p.%d" cat-name page))
  (freebox-http-get-category source-key tid page freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; 分类内容路径: data.movie.videoList（与 search/detail 相同）
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
                   (next-label (format "-- Next page (p.%d/%d) --" (1+ page) pagecount))
                   (all-cands  (if has-next
                                   (append candidates (list (cons next-label :next)))
                                 candidates))
                   (selected-name
                    (completing-read
                     (format "%s p.%d/%d (%d items): "
                             cat-name page pagecount (length candidates))
                     all-cands nil t))
                   (selected-val (cdr (assoc selected-name all-cands))))
              (if (eq selected-val :next)
                  (freebox-ui--category-page source-key tid cat-name (1+ page))
                (freebox-ui-show-detail selected-val)))))))))

;;; ─── VOD detail & episode selection ──────────────────────────────────────────

(defun freebox-ui-show-detail (vod-id)
  "Fetch VOD details for VOD-ID and prompt user to select an episode."
  (freebox-ui--loading "loading details")
  (freebox-http-get-detail freebox-ui-current-source vod-id freebox-ui-current-client-id
    (lambda (err data)
      (if err
          (freebox-ui--error err)
        ;; 详情路径: data.movie.videoList，取第一项
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
    (message "▶ %s%s%s%s"
             (or name "?")
             (if note  (format "  ★%s" note) "")
             (if (and actor (stringp actor) (not (string-empty-p actor)))
                 (format "  [%s]" (truncate-string-to-width actor 40 nil nil "…"))
               "")
             (if (and des (stringp des) (not (string-empty-p des)))
                 (format "\n%s" (truncate-string-to-width des 120 nil nil "…"))
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
        ;; play API 返回 data.nameValuePairs.url
        (let* ((nvp      (freebox-ui--jget pdata 'nameValuePairs))
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
             ;; 找到选中 flag 对应的 info 项
             (info (cl-find selected-flag info-list
                            :test #'equal
                            :key (lambda (i) (freebox-ui--jget i 'flag))))
             ;; 解析 urls 字符串：每项 "集名$ep_url"，# 分隔
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
                 ;; episode URL 直接作为 vodId 传给 /api/play
                 (ep-url (cdr (assoc selected-ep candidates))))
            (freebox-ui--resolve-and-play
             freebox-ui-current-source selected-flag ep-url
             ep-url selected-ep)))))))

(provide 'freebox-ui)
;;; freebox-ui.el ends here
