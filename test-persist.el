;;; test-persist.el --- Test freebox-persist functionality

;; Load required libraries
(require 'cl-lib)
(require 'freebox-persist)

;; Test 1: Initialize
(message "Test 1: Initializing persist system...")
(freebox-persist-init)
(message "✓ Init successful")

;; Test 2: Set and get client
(message "Test 2: Testing client persistence...")
(freebox-persist-set-client-id "test-client-123")
(freebox-persist-set-client-name "My Test Client")
(cl-assert (equal (freebox-persist-get-client-id) "test-client-123") nil "Client ID mismatch")
(cl-assert (equal (freebox-persist-get-client-name) "My Test Client") nil "Client name mismatch")
(message "✓ Client persistence works")

;; Test 3: Set and get source
(message "Test 3: Testing source persistence...")
(freebox-persist-set-source-key "iqiyi")
(freebox-persist-set-source-name "爱奇艺")
(cl-assert (equal (freebox-persist-get-source-key) "iqiyi") nil "Source key mismatch")
(cl-assert (equal (freebox-persist-get-source-name) "爱奇艺") nil "Source name mismatch")
(message "✓ Source persistence works")

;; Test 4: History management
(message "Test 4: Testing history management...")
(freebox-persist-add-history 'clients (vector "Client1" "id1"))
(freebox-persist-add-history 'clients (vector "Client2" "id2"))
(let ((history (freebox-persist-get-history 'clients)))
  (cl-assert history nil "History is empty")
  (message "✓ History has %d entries" (length history)))

;; Test 5: File persistence
(message "Test 5: Verifying file persistence...")
(let ((state-file (expand-file-name "~/.freebox/menu-state.json")))
  (cl-assert (file-exists-p state-file) nil "State file not created")
  (message "✓ State file exists: %s" state-file)

  ;; Read and parse the file
  (with-temp-buffer
    (insert-file-contents state-file)
    (let ((data (json-read-from-string (buffer-string))))
      (cl-assert (assq 'version data) nil "No version in state file")
      (cl-assert (assq 'state data) nil "No state in state file")
      (message "✓ State file is valid JSON with expected structure"))))

;; Test 6: State recovery
(message "Test 6: Testing state recovery after reload...")
(let ((saved-id (freebox-persist-get-client-id))
      (saved-name (freebox-persist-get-client-name)))
  ;; Clear in-memory cache
  (setq freebox-persist--state nil)
  ;; Reload from file
  (freebox-persist--ensure-loaded)
  ;; Verify recovered
  (cl-assert (equal (freebox-persist-get-client-id) saved-id) nil "Failed to recover client ID")
  (cl-assert (equal (freebox-persist-get-client-name) saved-name) nil "Failed to recover client name")
  (message "✓ State recovery successful"))

;; Test 7: Clear state
(message "Test 7: Testing state clearing...")
(freebox-persist-clear)
(cl-assert (not (freebox-persist-get-client-id)) nil "Client ID not cleared")
(cl-assert (not (freebox-persist-get-source-key)) nil "Source key not cleared")
(message "✓ State clearing works")

(message "\n=== All tests passed! ===")
