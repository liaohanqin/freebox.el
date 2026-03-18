;;; freebox-persist.el --- Menu state persistence for FreeBox -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides persistent storage of FreeBox menu selections (client, source, category).
;; Stores state in ~/.freebox/menu-state.json for recovery on next session.
;;
;; Public API:
;;   freebox-persist-init                 ; Initialize storage
;;   freebox-persist-get-state            ; Read current state
;;   freebox-persist-set-state            ; Save state
;;   freebox-persist-get                  ; Get specific key
;;   freebox-persist-set                  ; Set specific key
;;   freebox-persist-clear                ; Clear all state
;;   freebox-persist-add-history          ; Add to history list
;;   freebox-persist-get-history          ; Get history list

;;; Code:

(require 'json)

;;; ─── Configuration ────────────────────────────────────────────────────────────

(defvar freebox-persist-dir (expand-file-name "~/.freebox")
  "Directory for FreeBox state files.")

(defvar freebox-persist-file (expand-file-name "menu-state.json" freebox-persist-dir)
  "File path for menu state persistence.")

(defvar freebox-persist-version 1
  "Version of persist file format.")

;;; ─── State Cache ──────────────────────────────────────────────────────────────

(defvar freebox-persist--state nil
  "In-memory cache of current state. Lazy-loaded from file.")

(defvar freebox-persist--dirty nil
  "Flag indicating state has been modified and should be saved.")

;;; ─── Initialization ────────────────────────────────────────────────────────────

(defun freebox-persist-init ()
  "Initialize the persist system. Creates directories if needed."
  (unless (file-exists-p freebox-persist-dir)
    (make-directory freebox-persist-dir t))
  (unless (file-exists-p freebox-persist-file)
    (with-temp-buffer
      (insert (json-encode
               `((version . ,freebox-persist-version)
                 (timestamp . ,(floor (time-to-seconds)))
                 (state . ())
                 (history . ((clients . [])
                             (sources . [])
                             (categories . []))))))
      (write-file freebox-persist-file))))

;;; ─── File I/O ─────────────────────────────────────────────────────────────────

(defun freebox-persist--read-file ()
  "Read and parse persist file. Returns nil if file doesn't exist or is invalid."
  (if (not (file-exists-p freebox-persist-file))
      nil
    (condition-case err
        (let ((content (with-temp-buffer
                        (insert-file-contents freebox-persist-file)
                        (buffer-string))))
          (json-read-from-string content))
      (error
       (message "FreeBox: error reading persist file: %s" err)
       nil))))

(defun freebox-persist--write-file (data)
  "Write DATA (alist) to persist file as JSON."
  (freebox-persist-init)
  (condition-case err
      (with-temp-buffer
        (insert (json-encode data))
        (write-file freebox-persist-file))
    (error
     (message "FreeBox: error writing persist file: %s" err))))

;;; ─── State Access ────────────────────────────────────────────────────────────

(defun freebox-persist--ensure-loaded ()
  "Ensure state is loaded into memory."
  (unless freebox-persist--state
    (let ((file-data (freebox-persist--read-file)))
      (if file-data
          (setq freebox-persist--state
                (alist-get 'state file-data))
        (freebox-persist-init)
        (setq freebox-persist--state nil))))
  freebox-persist--state)

(defun freebox-persist-get (key)
  "Get value for KEY from state. Returns nil if not set.
KEY should be a symbol like `client-id', `source-key', etc."
  (alist-get key (freebox-persist--ensure-loaded)))

(defun freebox-persist-set (key value)
  "Set KEY to VALUE in state.
KEY should be a symbol like `client-id', `source-key', etc.
Automatically marks state as dirty and saves to file."
  (freebox-persist--ensure-loaded)
  (unless freebox-persist--state
    (setq freebox-persist--state nil))

  ;; Update or add the key-value pair
  (if (assq key freebox-persist--state)
      (setcdr (assq key freebox-persist--state) value)
    (push (cons key value) freebox-persist--state))

  ;; Save to file
  (freebox-persist--save-now))

(defun freebox-persist-get-state ()
  "Get the entire current state as an alist."
  (freebox-persist--ensure-loaded))

(defun freebox-persist-set-state (state-alist)
  "Set the entire state to STATE-ALIST.
STATE-ALIST should be an alist of (key . value) pairs."
  (setq freebox-persist--state state-alist)
  (freebox-persist--save-now))

(defun freebox-persist--save-now ()
  "Immediately save current state to file."
  (let ((full-data `((version . ,freebox-persist-version)
                     (timestamp . ,(floor (time-to-seconds)))
                     (state . ,freebox-persist--state)
                     (history . ,(freebox-persist--read-history)))))
    (freebox-persist--write-file full-data)))

(defun freebox-persist-clear ()
  "Clear all persisted state."
  (setq freebox-persist--state nil)
  ;; Save empty state to file
  (let ((full-data `((version . ,freebox-persist-version)
                     (timestamp . ,(floor (time-to-seconds)))
                     (state . ())
                     (history . ((clients . [])
                                 (sources . [])
                                 (categories . []))))))
    (freebox-persist--write-file full-data)))

;;; ─── History Management ────────────────────────────────────────────────────────

(defun freebox-persist--read-history ()
  "Read history section from file."
  (let* ((file-data (freebox-persist--read-file)))
    (if file-data
        (alist-get 'history file-data)
      `((clients . [])
        (sources . [])
        (categories . [])))))

(defun freebox-persist-add-history (category item)
  "Add ITEM to CATEGORY history. CATEGORY is `clients', `sources', or `categories'.
ITEM format:
  - clients/sources: [display-name, id-or-key]
  - categories: [display-name, tid]"
  (freebox-persist-init)
  (let* ((history (freebox-persist--read-history))
         (cat-list (alist-get category history))
         (cat-vec (if (vectorp cat-list) (append cat-list nil) ())))

    ;; Remove item if it already exists (to re-add it at the front)
    (setq cat-vec (seq-filter (lambda (x)
                               (not (equal (aref x 0) (aref item 0))))
                             cat-vec))

    ;; Add item at front (most recent first)
    (setq cat-vec (cons item cat-vec))

    ;; Limit to 20 most recent items
    (when (> (length cat-vec) 20)
      (setq cat-vec (seq-subseq cat-vec 0 20)))

    ;; Update history
    (setf (alist-get category history) (vconcat cat-vec))

    ;; Save
    (let ((full-data `((version . ,freebox-persist-version)
                       (timestamp . ,(floor (time-to-seconds)))
                       (state . ,freebox-persist--state)
                       (history . ,history))))
      (freebox-persist--write-file full-data))))

(defun freebox-persist-get-history (category)
  "Get CATEGORY history as a list of items.
Returns a list of [display-name, id-or-key] pairs in most-recent-first order."
  (let* ((history (freebox-persist--read-history))
         (cat-list (alist-get category history)))
    (if (vectorp cat-list)
        (append cat-list nil)
      nil)))

;;; ─── V-Cursor (Current Navigation Node) ────────────────────────────────────────
;;
;; v-cursor 记录用户当前停留的最深导航节点，供 v 键恢复使用。
;; 格式为 alist，类型字段 `type' 可为：
;;   category  → 已进入分类选择
;;   vod-list  → 正在浏览分类的第 N 页影片列表
;;   vod-detail→ 正在查看影片详情
;;   episode   → 正在选集
;;
;; 示例：
;;   ((type . "vod-list") (source-key . "xxx") (tid . "1") (cat-name . "电影") (page . 3))

(defun freebox-persist-get-v-cursor ()
  "Get the persisted v-cursor navigation node.
Returns an alist with at least a `type' key, or nil if not set."
  (freebox-persist-get 'v-cursor))

(defun freebox-persist-set-v-cursor (cursor-alist)
  "Set the v-cursor navigation node to CURSOR-ALIST.
CURSOR-ALIST should contain at least `type' and `source-key' keys."
  (freebox-persist-set 'v-cursor cursor-alist))

(defun freebox-persist-clear-v-cursor ()
  "Clear the persisted v-cursor navigation node."
  (freebox-persist-set 'v-cursor nil))

;;; ─── Convenience Helpers ────────────────────────────────────────────────────────

(defun freebox-persist-get-client-id ()
  "Get stored client ID."
  (freebox-persist-get 'client-id))

(defun freebox-persist-set-client-id (id)
  "Set and save client ID."
  (freebox-persist-set 'client-id id))

(defun freebox-persist-get-client-name ()
  "Get stored client name."
  (freebox-persist-get 'client-name))

(defun freebox-persist-set-client-name (name)
  "Set and save client name."
  (freebox-persist-set 'client-name name))

(defun freebox-persist-get-source-key ()
  "Get stored source key."
  (freebox-persist-get 'source-key))

(defun freebox-persist-set-source-key (key)
  "Set and save source key."
  (freebox-persist-set 'source-key key))

(defun freebox-persist-get-source-name ()
  "Get stored source name."
  (freebox-persist-get 'source-name))

(defun freebox-persist-set-source-name (name)
  "Set and save source name."
  (freebox-persist-set 'source-name name))

(defun freebox-persist-get-category-tid ()
  "Get stored category TID."
  (freebox-persist-get 'category-tid))

(defun freebox-persist-set-category-tid (tid)
  "Set and save category TID."
  (freebox-persist-set 'category-tid tid))

(defun freebox-persist-get-category-name ()
  "Get stored category name."
  (freebox-persist-get 'category-name))

(defun freebox-persist-set-category-name (name)
  "Set and save category name."
  (freebox-persist-set 'category-name name))

(provide 'freebox-persist)
;;; freebox-persist.el ends here
