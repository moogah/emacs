;;; test-6-json-building.el --- Test JSON building from hash table with backquote -*- lexical-binding: t -*-

;;; Commentary:
;; Test 6: Test building JSON from hash table using backquote/unquote pattern.
;; Goal: Reproduce the exact alist-building and JSON-encoding pattern from question-tools.
;; This tests whether the backquote pattern with string values causes symbol errors.

;;; Code:

(require 'json)

;; Simulate question data
(defvar test-6-questions
  '((:id "q1"
     :type "multiple-choice"
     :prompt "What is your favorite season?"
     :choices ("Spring - Fresh beginnings and blooming flowers"
              "Summer - Warm days and long nights"
              "Fall - Colorful leaves and crisp air"
              "Winter - Snow and cozy evenings"))
    (:id "q2"
     :type "text"
     :prompt "Why did you choose this season?"
     :choices nil))
  "Test questions.")

(defun test-6-build-answer-list (questions answers-hash comments-hash)
  "Build answer list from QUESTIONS and hash tables.
This mirrors jf/gptel-questions--build-answer-list exactly."
  (let ((answer-list '()))
    (dolist (q questions)
      (let* ((id (plist-get q :id))
             (answer (gethash id answers-hash))
             (comment (gethash id comments-hash))
             (skipped (null answer)))
        ;; Use alist format for proper JSON encoding (EXACT pattern from question-tools)
        (push `((id . ,id)
                (answer . ,(or answer ""))
                (comment . ,(or comment ""))
                (skipped . ,skipped))
              answer-list)))

    ;; Encode as JSON string for gptel tool result serialization
    (json-encode (nreverse answer-list))))

(defun test-6-mock-callback (answer-json)
  "Mock callback that receives ANSWER-JSON string.
This simulates what gptel's tool handler would receive."
  (message "")
  (message "=== Mock Callback Received ===")
  (message "JSON string length: %d" (length answer-json))
  (message "JSON content: %s" answer-json)

  ;; Decode JSON to verify it's valid
  (condition-case err
      (let ((decoded (json-read-from-string answer-json)))
        (message "✓ JSON decoded successfully")
        (message "Decoded structure: %S" decoded)

        ;; Verify we can access the values
        (let ((first-answer (aref decoded 0)))
          (let-alist first-answer
            (message "")
            (message "First answer details:")
            (message "  ID: %s (type: %s)" .id (type-of .id))
            (message "  Answer: %s (type: %s)" .answer (type-of .answer))
            (message "  Comment: %s (type: %s)" .comment (type-of .comment))
            (message "  Skipped: %s (type: %s)" .skipped (type-of .skipped))

            ;; Check if answer is the expected string
            (if (equal .answer "Spring - Fresh beginnings and blooming flowers")
                (message "✓ Answer value matches expected string")
              (message "✗ Answer value mismatch: %s" .answer)))))
    (error
     (message "✗ ERROR decoding JSON: %s" (error-message-string err))
     (message "Error details: %S" err))))

(defun test-6-main ()
  "Entry point for batch mode execution."
  (message "=== TEST 6: JSON Building with Backquote ===")
  (message "Testing alist building and JSON encoding...")

  ;; Set up hash tables (simulating transient scope)
  (let ((answers (make-hash-table :test 'equal))
        (comments (make-hash-table :test 'equal)))

    ;; Store test answers
    (puthash "q1" "Spring - Fresh beginnings and blooming flowers" answers)
    (puthash "q2" "Because it represents renewal" answers)
    (puthash "q1" "" comments)
    (puthash "q2" "Personal preference" comments)

    (message "")
    (message "Stored answers:")
    (message "  q1: %s" (gethash "q1" answers))
    (message "  q2: %s" (gethash "q2" answers))

    ;; Build answer list using exact pattern from question-tools
    (message "")
    (message "Building answer list with backquote pattern...")
    (let ((answer-json (test-6-build-answer-list test-6-questions answers comments)))

      ;; Invoke mock callback
      (test-6-mock-callback answer-json)

      (message "")
      (message "✓ TEST 6 PASSED: JSON building completed without errors")))

  (message "=== TEST 6 COMPLETE ==="))

(provide 'test-6-json-building)
;;; test-6-json-building.el ends here
