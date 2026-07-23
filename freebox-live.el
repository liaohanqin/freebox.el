;;; freebox-live.el --- Live TV tree browser for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-style live TV channel browser (`*freebox-live*' buffer).
;; Replaces the old completing-read chain (group -> channel -> line)
;; with a single foldable tree: group -> channel -> line(s).
;;
;; Layout:
;;   ▾ 央视 (12)
;;       ▶ CCTV-1 综合          <- last played marker
;;         CCTV-2 财经
;;       ▸ CCTV-5 体育 (2条线路)  <- multi-line, expandable
;;           线路1
;;           线路2
;;   ▸ 卫视 (30)
;;
;; Keys: RET play/expand, TAB fold/expand, j/k move, g refresh,
;;       s switch live source, q quit.
;;
;; The buffer is cached: re-entering with the same live client pops the
;; existing buffer instantly (no refetch); use g to refresh.

;;; Code:

(require 'subr-x)
(require 'freebox-ui)

(declare-function hydra-keyboard-quit "hydra")
(declare-function freebox-empv-play-url "freebox-empv")

;;; --- Constants & state --------------------------------------------------------

(defconst freebox-live-buffer-name "*freebox-live*"
  "Name of the live TV tree buffer.")

(defvar-local freebox-live--groups nil
  "LiveChannelGroup alist list for the current buffer.")

(defvar-local freebox-live--client-id nil
  "Live client ID backing the current buffer.")

(defvar-local freebox-live--client-name nil
  "Live client display name backing the current buffer.")

(defvar-local freebox-live--expanded nil
  "Hash table (:test equal) of expanded node keys.
Keys are index path strings: \"gi\" for groups, \"gi/ci\" for channels.
Index-based (not name-based) because group/channel titles may repeat.")

(defvar freebox-live--cached-client-id nil
  "Client ID last rendered into the `freebox-live' buffer.
Used by `freebox-live-open' to pop the cached buffer without refetching.")

(defvar freebox-live--last-channel-title nil
  "Title of the last played live channel (marked with ▶ in the tree).")

;;; --- Client selection (moved from freebox-ui.el) ------------------------------

(defun freebox-live-select-client ()
  "Interactively select a FreeBox live TV client (SINGLE_LIVE source).
Saves selection to persistent state.  Equivalent of
`freebox-select-client' for live TV."
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
            (let ((picked (freebox-live--pick-client-from-list clients)))
              (when picked
                (freebox-live--save-client (car picked) (cdr picked))
                (message "FreeBox: 直播源 -> [%s]" (cdr picked)))))))))))

(defun freebox-live--pick-client-from-list (clients)
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

(defun freebox-live--save-client (id name)
  "Save live TV client selection (ID, NAME) to state."
  (setq freebox-ui-current-live-client-id   id
        freebox-ui-current-live-client-name name)
  (freebox-persist-set 'live-client-id id)
  (freebox-persist-set 'live-client-name name))

(defun freebox-live--with-client (fn)
  "Ensure a live TV client is selected, then call FN with client-id and name.
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
                            (freebox-live--pick-client-from-list clients))))
             (when picked
               (freebox-live--save-client (car picked) (cdr picked))
               (message "FreeBox: 直播源 -> [%s]" (cdr picked))
               (funcall fn (car picked) (cdr picked))))))))))

;;; --- Playback -----------------------------------------------------------------

(defun freebox-live--play-line (line title)
  "Play live channel LINE (with url field) under TITLE."
  (let ((url (freebox-ui--jget line 'url)))
    (if (or (not url) (string-empty-p url))
        (message "FreeBox: 频道 [%s] 线路无 URL" title)
      (message "FreeBox: 播放直播 [%s]" title)
      (freebox-empv-play-url url title))))

;;; --- Entry points --------------------------------------------------------------

(defun freebox-live-open ()
  "Open the live TV tree browser.
Pops the cached buffer instantly when it exists and the live client has
not changed; otherwise fetches channels and renders."
  (interactive)
  (let ((buf (get-buffer freebox-live-buffer-name)))
    (if (and buf
             freebox-live--cached-client-id
             freebox-ui-current-live-client-id
             (equal freebox-live--cached-client-id
                    freebox-ui-current-live-client-id))
        (progn
          (when (bound-and-true-p hydra-curr-map)
            (hydra-keyboard-quit))
          (pop-to-buffer buf '((display-buffer-same-window))))
      (freebox-http-ensure-server
       (lambda () (freebox-live--with-client #'freebox-live--load-channels))))))

(defun freebox-live--load-channels (client-id client-name)
  "Fetch channel groups for CLIENT-ID (displayed as CLIENT-NAME), then show tree."
  (freebox-ui--loading (format "加载直播源 [%s]" client-name))
  (freebox-http-get-live-channels
   client-id
   (lambda (err groups)
     (if err
         (freebox-ui--error err)
       (let ((items (freebox-ui--vec->list groups)))
         (if (not items)
             (message "FreeBox: 直播源 [%s] 没有频道" client-name)
           (freebox-live--show-buffer items client-id client-name)))))))

(defun freebox-live--show-buffer (groups client-id client-name)
  "Render GROUPS for CLIENT-ID/CLIENT-NAME into the live tree buffer and pop it."
  ;; Dismiss hydra if active, or its transient map keeps intercepting keys
  (when (bound-and-true-p hydra-curr-map)
    (hydra-keyboard-quit))
  (let ((buf (get-buffer-create freebox-live-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'freebox-live-mode)
        (freebox-live-mode))
      (let ((new-client (not (equal freebox-live--client-id client-id))))
        (setq freebox-live--groups      groups
              freebox-live--client-id   client-id
              freebox-live--client-name client-name)
        ;; (Re)initialize expansion state: default all groups expanded
        (when (or new-client (not freebox-live--expanded))
          (setq freebox-live--expanded (make-hash-table :test 'equal))
          (dotimes (i (length groups))
            (puthash (number-to-string i) t freebox-live--expanded))))
      (freebox-live--render))
    (setq freebox-live--cached-client-id client-id)
    (pop-to-buffer buf '((display-buffer-same-window)))))

;;; --- Rendering ------------------------------------------------------------------

(defun freebox-live--render ()
  "Re-render the whole tree, preserving point by node key when possible."
  (let ((inhibit-read-only t)
        (node-key (freebox-live--node-key-at-point))
        (line-no (line-number-at-pos)))
    (erase-buffer)
    (insert (propertize (format "FreeBox 直播 [%s]"
                                (or freebox-live--client-name "?"))
                        'face '(:weight bold :height 1.1))
            "\n"
            (propertize (make-string 46 ?─) 'face 'shadow)
            "\n")
    (let ((gi 0))
      (dolist (g freebox-live--groups)
        (freebox-live--insert-group g gi)
        (setq gi (1+ gi))))
    (insert (propertize (make-string 46 ?─) 'face 'shadow)
            "\n"
            (propertize
             "[RET] 播放/展开  [TAB] 折叠/展开  [j/k] 移动  [g] 刷新  [s] 换源  [q] 退出"
             'face 'font-lock-comment-face)
            "\n")
    (unless (and node-key (freebox-live--goto-node-key node-key))
      (goto-char (point-min))
      (forward-line (1- line-no)))))

(defun freebox-live--insert-group (group gi)
  "Insert GROUP (index GI) and, when expanded, its channels."
  (let* ((gkey (number-to-string gi))
         (title (or (freebox-ui--jget group 'title) "未分组"))
         (channels (freebox-ui--vec->list (freebox-ui--jget group 'channels)))
         (expanded (gethash gkey freebox-live--expanded)))
    (insert (propertize (format "%s %s (%d)\n"
                                (if expanded "▾" "▸") title (length channels))
                        'face 'font-lock-keyword-face
                        'freebox-live-node (list :type 'group :gi gi)))
    (when expanded
      (let ((ci 0))
        (dolist (ch channels)
          (freebox-live--insert-channel ch gi ci)
          (setq ci (1+ ci)))))))

(defun freebox-live--insert-channel (ch gi ci)
  "Insert channel CH (index GI/CI) and, when expanded, its lines."
  (let* ((title (or (freebox-ui--jget ch 'title) "?"))
         (lines (freebox-ui--vec->list (freebox-ui--jget ch 'lines)))
         (n (length lines))
         (multi (> n 1))
         (ckey (format "%d/%d" gi ci))
         (expanded (and multi (gethash ckey freebox-live--expanded)))
         (last (equal title freebox-live--last-channel-title))
         (arrow (cond (last "▶")
                      (multi (if expanded "▾" "▸"))
                      (t " ")))
         (suffix (cond ((= n 0) " (无线路)")
                       (multi (format " (%d条线路)" n))
                       (t ""))))
    (insert (propertize (format "  %s %s%s\n" arrow title suffix)
                        'face (when last 'font-lock-constant-face)
                        'freebox-live-node
                        (list :type 'channel :gi gi :ci ci :title title :data ch)))
    (when expanded
      (let ((li 0))
        (dolist (line lines)
          (insert (propertize (format "      %s\n"
                                      (or (freebox-ui--jget line 'title) "线路"))
                              'face 'font-lock-comment-face
                              'freebox-live-node
                              (list :type 'line :gi gi :ci ci :li li
                                    :title title :data line)))
          (setq li (1+ li)))))))

;;; --- Node helpers ----------------------------------------------------------------

(defun freebox-live--node-at-point ()
  "Return the node plist at the beginning of the current line, or nil.
Read at line beginning because the trailing newline carries no property."
  (get-text-property (line-beginning-position) 'freebox-live-node))

(defun freebox-live--node-key (node)
  "Return the index path key string for NODE, or nil."
  (when node
    (pcase (plist-get node :type)
      ('group   (format "%d" (plist-get node :gi)))
      ('channel (format "%d/%d" (plist-get node :gi) (plist-get node :ci)))
      ('line    (format "%d/%d/%d" (plist-get node :gi)
                        (plist-get node :ci) (plist-get node :li))))))

(defun freebox-live--node-key-at-point ()
  "Return the node key at the current line, or nil."
  (freebox-live--node-key (freebox-live--node-at-point)))

(defun freebox-live--goto-node-key (key)
  "Move point to the line whose node key is KEY.  Return non-nil if found."
  (let ((found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (if (equal (freebox-live--node-key-at-point) key)
            (setq found (line-beginning-position))
          (forward-line 1))))
    (when found
      (goto-char found)
      t)))

;;; --- Interaction -------------------------------------------------------------------

(defun freebox-live-activate ()
  "RET: play a line/channel, or toggle a group/multi-line channel."
  (interactive)
  (let ((node (freebox-live--node-at-point)))
    (if (not node)
        (message "FreeBox: 当前行不是频道节点")
      (pcase (plist-get node :type)
        ('group
         (freebox-live--toggle-node node))
        ('channel
         (let ((lines (freebox-ui--vec->list
                       (freebox-ui--jget (plist-get node :data) 'lines))))
           (cond
            ((not lines)
             (message "FreeBox: 该频道没有可播放线路"))
            ((= (length lines) 1)
             (freebox-live--play-node node (car lines)))
            (t
             (freebox-live--toggle-node node)))))
        ('line
         (freebox-live--play-node node (plist-get node :data)))))))

(defun freebox-live-toggle ()
  "TAB: fold/expand a group or multi-line channel (never plays)."
  (interactive)
  (let ((node (freebox-live--node-at-point)))
    (if (not node)
        (message "FreeBox: 当前行不是频道节点")
      (freebox-live--toggle-node node))))

(defun freebox-live--toggle-node (node)
  "Flip expansion state of NODE (group or channel) and re-render."
  (pcase (plist-get node :type)
    ('group
     (let ((gkey (number-to-string (plist-get node :gi))))
       (puthash gkey (not (gethash gkey freebox-live--expanded))
                freebox-live--expanded)
       (freebox-live--render)))
    ('channel
     (let ((lines (freebox-ui--vec->list
                   (freebox-ui--jget (plist-get node :data) 'lines))))
       (if (<= (length lines) 1)
           (message "FreeBox: 单线路频道，按 RET 直接播放")
         (let ((ckey (format "%d/%d" (plist-get node :gi)
                             (plist-get node :ci))))
           (puthash ckey (not (gethash ckey freebox-live--expanded))
                    freebox-live--expanded)
           (freebox-live--render)))))
    ('line
     (message "FreeBox: 按 RET 播放该线路"))))

(defun freebox-live--play-node (node line)
  "Play LINE under NODE's channel title, update last-played marker, re-render."
  (let ((title (or (plist-get node :title) "?")))
    (setq freebox-live--last-channel-title title)
    (freebox-live--play-line line title)
    (freebox-live--render)))

(defun freebox-live-refresh ()
  "g: refetch channels for the buffer's client and re-render.
Expansion state is preserved; out-of-range keys are simply unused."
  (interactive)
  (unless freebox-live--client-id
    (user-error "FreeBox: 当前 buffer 没有直播源上下文"))
  (let ((buf (current-buffer))
        (client-id freebox-live--client-id)
        (client-name freebox-live--client-name))
    (freebox-ui--loading (format "刷新直播源 [%s]" client-name))
    (freebox-http-get-live-channels
     client-id
     (lambda (err groups)
       (if err
           (freebox-ui--error err)
         ;; Buffer may have been killed or switched to another client meanwhile
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (equal freebox-live--client-id client-id)
               (setq freebox-live--groups (freebox-ui--vec->list groups))
               (freebox-live--render)))))))))

(defun freebox-live-switch-source ()
  "s: pick another live TV client and reload the tree."
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
            (let ((picked (freebox-live--pick-client-from-list clients)))
              (when picked
                (freebox-live--save-client (car picked) (cdr picked))
                (freebox-live--load-channels (car picked) (cdr picked)))))))))))

;;; --- Mode ---------------------------------------------------------------------------

(defvar freebox-live-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'freebox-live-activate)
    (define-key map (kbd "TAB") #'freebox-live-toggle)
    (define-key map (kbd "j")   #'next-line)
    (define-key map (kbd "k")   #'previous-line)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "g")   #'freebox-live-refresh)
    (define-key map (kbd "s")   #'freebox-live-switch-source)
    map)
  "Keymap for `freebox-live-mode'.")

(define-derived-mode freebox-live-mode special-mode "FreeBox-Live"
  "Major mode for the FreeBox live TV channel tree.
\\<freebox-live-mode-map>
\\[freebox-live-activate] - Play / expand
\\[freebox-live-toggle] - Fold / expand
\\[freebox-live-refresh] - Refresh channels
\\[freebox-live-switch-source] - Switch live source
\\[quit-window] - Quit"
  :group 'freebox
  (setq-local truncate-lines t))

(provide 'freebox-live)
;;; freebox-live.el ends here
