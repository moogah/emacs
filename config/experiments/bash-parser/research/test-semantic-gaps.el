#!/usr/bin/env emacs --script
;;; test-semantic-gaps.el --- Quick test of semantic gap detection -*- lexical-binding: t; -*-

;; Load path setup
(defvar jf/emacs-dir
  (let ((script-dir (or (and load-file-name (file-name-directory load-file-name))
                       default-directory)))
    (expand-file-name "../../../" script-dir))
  "Repository root directory.")

(add-to-list 'load-path (expand-file-name "config/experiments/bash-parser" jf/emacs-dir))
(add-to-list 'load-path (expand-file-name "config/experiments/bash-parser/research" jf/emacs-dir))

;; Load modules
(require 'bash-parser)
(require 'semantic-gap-detection)

;;; Test cases

(defun test-semantic-gap (command expected-gap-type description)
  "Test that COMMAND produces a gap of EXPECTED-GAP-TYPE."
  (let* ((parsed (jf/bash-parse command))
         (gaps (jf/bash-parse-semantic--detect-gaps command parsed))
         (gap-types (mapcar (lambda (g) (plist-get g :type)) gaps))
         (found (member expected-gap-type gap-types)))
    (if found
        (progn
          (princ (format "✓ %s\n" description))
          (princ (format "  Command: %s\n" command))
          (princ (format "  Detected: %s\n\n" gap-types)))
      (progn
        (princ (format "✗ FAILED: %s\n" description))
        (princ (format "  Command: %s\n" command))
        (princ (format "  Expected gap type: %s\n" expected-gap-type))
        (princ (format "  Got gap types: %s\n\n" gap-types))))))

(defun test-no-semantic-gap (command description)
  "Test that COMMAND produces NO semantic gaps."
  (let* ((parsed (jf/bash-parse command))
         (gaps (jf/bash-parse-semantic--detect-gaps command parsed)))
    (if (null gaps)
        (progn
          (princ (format "✓ %s\n" description))
          (princ (format "  Command: %s\n" command))
          (princ "  No gaps detected (as expected)\n\n"))
      (progn
        (princ (format "✗ FAILED: %s\n" description))
        (princ (format "  Command: %s\n" command))
        (princ (format "  Expected no gaps but got: %s\n\n"
                      (mapcar (lambda (g) (plist-get g :type)) gaps)))))))

;;; Run tests

(princ "=== Semantic Gap Detection Tests ===\n\n")

;; Command substitution tests
(test-semantic-gap
 "echo $(whoami)"
 'command-substitution
 "Simple command substitution")

(test-semantic-gap
 "ls -la $(dirname $(which openspec))"
 'command-substitution
 "Nested command substitution")

(test-semantic-gap
 "--eval '(setq dir \"'$(pwd)'\")'"
 'command-substitution
 "Command substitution in quoted string")

;; Process substitution tests
(test-semantic-gap
 "diff <(ls dir1) <(ls dir2)"
 'process-substitution
 "Process substitution")

;; For loop tests
(test-semantic-gap
 "for f in *.txt; do echo $f; done"
 'for-loop
 "For loop with variables")

;; Conditional tests
(test-semantic-gap
 "if [ -f file ]; then rm file; fi"
 'conditional
 "Conditional statement")

;; Heredoc tests
(test-semantic-gap
 "cat <<EOF
Content here
EOF"
 'heredoc
 "Heredoc content")

;; No gaps expected
(test-no-semantic-gap
 "ls -la /tmp"
 "Simple command (no gaps expected)")

(test-no-semantic-gap
 "git commit -m 'message'"
 "Simple command with args (no gaps expected)")

(princ "=== Test Complete ===\n")
