;;; test-3-callback.el --- Test callback invocation with stored data -*- lexical-binding: t -*-

;;; Commentary:
;; Test 3: Test calling a callback function with stored data after transient exits.
;; Goal: Verify callback pattern works in general.

;;; Code:

(require 'transient)

;; Buffer-local variable to store callback (simulating question-tools pattern)
(defvar-local test-3-callback nil
  "Callback function to invoke with collected data.")

;; Data storage
(defvar test-3-collected-data nil
  "Data collected during transient session.")

(transient-define-suffix test-3-collect-data ()
  "Collect some test data."
  :transient t
  (interactive)
  (push "item-1" test-3-collected-data)
  (push "item-2" test-3-collected-data)
  (push "item-3" test-3-collected-data)
  (message "Collected %d items" (length test-3-collected-data)))

(transient-define-suffix test-3-finish ()
  "Finish collection and invoke callback."
  :transient nil
  (interactive)
  (when test-3-callback
    (message "Invoking callback with %d items..." (length test-3-collected-data))
    (funcall test-3-callback test-3-collected-data)))

(transient-define-prefix test-3-menu ()
  "Callback test menu."
  ["Test Actions"
   ("c" "Collect data" test-3-collect-data)
   ("f" "Finish and invoke callback" test-3-finish)])

(defun test-3-my-callback (data)
  "Test callback that receives DATA."
  (message "")
  (message "=== Callback Invoked ===")
  (message "Received %d items:" (length data))
  (dolist (item data)
    (message "  - %s" item))

  ;; Verify data
  (let ((expected '("item-3" "item-2" "item-1")))  ; Reversed because of push
    (if (equal data expected)
        (message "✓ TEST 3 PASSED: Callback received correct data")
      (message "✗ TEST 3 FAILED: Expected %s, got %s" expected data))))

(defun test-3-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 3: Callback Invocation ===")
  (message "Testing callback pattern...")

  ;; Set up callback
  (setq test-3-callback #'test-3-my-callback)
  (setq test-3-collected-data nil)

  ;; Simulate collecting data
  (test-3-collect-data)

  ;; Simulate finishing (which invokes callback)
  (test-3-finish)

  (message "=== TEST 3 COMPLETE ==="))

(provide 'test-3-callback)
;;; test-3-callback.el ends here
