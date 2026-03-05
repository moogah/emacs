;;; debug-failures.el --- Debug script for test failures -*- lexical-binding: t; -*-

(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))

(message "\n========================================")
(message "DEBUG: Investigating Test Failures")
(message "========================================\n")

;; Test 1: Simple find command (corpus test that's failing)
(message "=== Test 1: find . -name '*.log' -exec rm {} \\; ===")
(message "Expected: 2 operations (read-directory ., delete {})")
(message "Previous error: Got 3 operations (extra match-pattern)")
(let* ((parsed (jf/bash-parse "find . -name '*.log' -exec rm {} \\;"))
       (ops (jf/bash-extract-file-operations parsed)))
  (message "Actual operations count: %d" (length ops))
  (dolist (op ops)
    (message "  - File: '%s', Op: %s, Source: %s, Cmd: %s"
             (plist-get op :file)
             (plist-get op :operation)
             (plist-get op :source)
             (plist-get op :command))))

(message "")

;; Test 2: cat with find substitution (pattern flow test that's failing)
(message "=== Test 2: cat $(find . -name '*.log') ===")
(message "Expected: cat reads pattern '*.log' with pattern-source")
(message "Previous error: cat-op is nil (operation not found)")
(let* ((parsed (jf/bash-parse "cat $(find . -name '*.log')"))
       (ops (jf/bash-extract-file-operations parsed)))
  (message "Actual operations count: %d" (length ops))
  (dolist (op ops)
    (message "  - File: '%s', Op: %s, Cmd: %s, From-subst: %s, Pattern: %s"
             (plist-get op :file)
             (plist-get op :operation)
             (plist-get op :command)
             (plist-get op :from-substitution)
             (plist-get op :pattern)))
  (message "\nLooking for cat operation on '*.log':")
  (let ((cat-op (seq-find (lambda (op)
                           (and (string= (plist-get op :file) "*.log")
                                (eq (plist-get op :operation) :read)
                                (string= (plist-get op :command) "cat")))
                         ops)))
    (if cat-op
        (message "  FOUND: %S" cat-op)
      (message "  NOT FOUND (this is the bug)"))))

(message "")

;; Test 3: Simple chain (should be working)
(message "=== Test 3: cat a.txt > b.txt; cat c.txt >> b.txt ===")
(message "Expected: 4 operations (read a.txt, write b.txt, read c.txt, append b.txt)")
(let* ((parsed (jf/bash-parse "cat a.txt > b.txt; cat c.txt >> b.txt"))
       (ops (jf/bash-extract-file-operations parsed)))
  (message "Parse type: %s, Command count: %s"
           (plist-get parsed :type)
           (plist-get parsed :command-count))
  (message "Actual operations count: %d" (length ops))
  (dolist (op ops)
    (message "  - File: '%s', Op: %s"
             (plist-get op :file)
             (plist-get op :operation))))

(message "\n========================================")
(message "DEBUG Complete")
(message "========================================\n")
