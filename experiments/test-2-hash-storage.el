;;; test-2-hash-storage.el --- Test hash table storage in transient scope -*- lexical-binding: t -*-

;;; Commentary:
;; Test 2: Test storing values in hash tables within transient scope (like question-tools does).
;; Goal: Verify hash table storage mechanics work correctly.

;;; Code:

(require 'transient)

;; Hash table to store answers (simulating question-tools pattern)
(defvar test-2-answers-hash nil
  "Hash table storing test answers.")

(transient-define-suffix test-2-store-value (key value)
  "Store VALUE under KEY in hash table."
  :transient t
  (interactive)
  (unless test-2-answers-hash
    (setq test-2-answers-hash (make-hash-table :test 'equal)))
  (puthash key value test-2-answers-hash)
  (message "Stored: %s = %s" key value))

(transient-define-suffix test-2-show-values ()
  "Show all stored values."
  :transient nil
  (interactive)
  (if (and test-2-answers-hash (> (hash-table-count test-2-answers-hash) 0))
      (progn
        (message "=== Stored Values ===")
        (maphash (lambda (k v)
                   (message "  %s = %s" k v))
                 test-2-answers-hash))
    (message "No values stored")))

(transient-define-prefix test-2-menu ()
  "Hash table storage test menu."
  ["Test Actions"
   ("1" "Store value 1" (lambda () (interactive) (test-2-store-value "q1" "answer1")))
   ("2" "Store value 2" (lambda () (interactive) (test-2-store-value "q2" "answer2")))
   ("3" "Store value 3" (lambda () (interactive) (test-2-store-value "q3" "answer3")))
   ("s" "Show values and exit" test-2-show-values)])

(defun test-2-verify ()
  "Verify hash table contents."
  (let ((expected '(("q1" . "answer1")
                    ("q2" . "answer2")
                    ("q3" . "answer3")))
        (all-correct t))
    (dolist (pair expected)
      (let* ((key (car pair))
             (expected-val (cdr pair))
             (actual-val (gethash key test-2-answers-hash)))
        (if (equal actual-val expected-val)
            (message "✓ %s: %s" key actual-val)
          (message "✗ %s: expected '%s', got '%s'" key expected-val actual-val)
          (setq all-correct nil))))
    (if all-correct
        (message "✓ TEST 2 PASSED: All hash values correct")
      (message "✗ TEST 2 FAILED: Hash values mismatch"))))

(defun test-2-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 2: Hash Table Storage ===")
  (message "Testing hash table storage mechanics...")

  ;; Initialize hash table
  (setq test-2-answers-hash (make-hash-table :test 'equal))

  ;; Simulate storing multiple values
  (test-2-store-value "q1" "answer1")
  (test-2-store-value "q2" "answer2")
  (test-2-store-value "q3" "answer3")

  ;; Verify the values
  (message "")
  (message "Verification:")
  (test-2-verify)

  (message "=== TEST 2 COMPLETE ==="))

(provide 'test-2-hash-storage)
;;; test-2-hash-storage.el ends here
