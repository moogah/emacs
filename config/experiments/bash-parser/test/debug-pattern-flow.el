;;; debug-pattern-flow.el --- Debug pattern flow issue -*- lexical-binding: t; -*-

;; Load the bash parser
(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))

(defun debug-pattern-flow (command-string)
  "Debug pattern flow for COMMAND-STRING."
  (let* ((parsed (jf/bash-parse command-string))
         (ops (jf/bash-extract-file-operations parsed)))
    (message "\n=== Debugging: %s ===" command-string)
    (message "Parsed structure:")
    (pp parsed)
    (message "\nExtracted operations:")
    (dolist (op ops)
      (message "  %S" op))
    (message "\n")
    ops))

;; Test case from failing test
(message "\n\n=== TEST 1: cat $(find . -name '*.log') ===")
(let ((ops (debug-pattern-flow "cat $(find . -name '*.log')")))
  (message "\nLooking for cat operation on *.log...")
  (let ((cat-op (seq-find (lambda (op)
                           (and (string= (plist-get op :command) "cat")
                                (string= (plist-get op :file) "*.log")))
                         ops)))
    (if cat-op
        (message "✓ Found cat operation: %S" cat-op)
      (message "✗ MISSING: cat operation on *.log")))

  (message "\nLooking for cat operation on literal substitution...")
  (let ((cat-literal (seq-find (lambda (op)
                                (and (string= (plist-get op :command) "cat")
                                     (string-prefix-p "$(" (plist-get op :file))))
                               ops)))
    (if cat-literal
        (message "✗ PROBLEM: cat operates on literal: %S" cat-literal)
      (message "✓ No literal substitution found"))))

;; Test case 2
(message "\n\n=== TEST 2: rm $(ls *.tmp) ===")
(let ((ops (debug-pattern-flow "rm $(ls *.tmp)")))
  (message "\nLooking for rm operation on *.tmp...")
  (let ((rm-op (seq-find (lambda (op)
                          (and (string= (plist-get op :command) "rm")
                               (string= (plist-get op :file) "*.tmp")))
                        ops)))
    (if rm-op
        (message "✓ Found rm operation: %S" rm-op)
      (message "✗ MISSING: rm operation on *.tmp")))

  (message "\nLooking for rm operation on literal substitution...")
  (let ((rm-literal (seq-find (lambda (op)
                               (and (string= (plist-get op :command) "rm")
                                    (string-prefix-p "$(" (plist-get op :file))))
                              ops)))
    (if rm-literal
        (message "✗ PROBLEM: rm operates on literal: %S" rm-literal)
      (message "✓ No literal substitution found"))))

(provide 'debug-pattern-flow)
