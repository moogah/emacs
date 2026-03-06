;;; test-parser-extension.el --- Tests for parser extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests for bash-parser extension features including:
;; - Command injection detection
;; - Nested command parsing
;; - Indirect operation marking
;; - Backward compatibility verification
;;
;; These tests validate Wave 4 functionality from bash-parser-file-ops OpenSpec change.
;;
;; Test naming convention: test-parser-extension-<scenario-slug>
;; Each test includes spec reference in docstring.

;;; Code:

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

;; Set up tree-sitter library path for batch mode
(let ((repo-root (or (getenv "EMACS_ROOT")
                     (locate-dominating-file default-directory ".git"))))
  ;; Add runtime/tree-sitter to treesit-extra-load-path
  (when (and (boundp 'treesit-extra-load-path)
             repo-root)
    (add-to-list 'treesit-extra-load-path
                 (expand-file-name "runtime/tree-sitter" repo-root)))

  ;; Load the bash-parser implementation
  (let ((parser-path (expand-file-name
                      "config/experiments/bash-parser/bash-parser.el"
                      repo-root)))
    (load parser-path)))

;;; Backward Compatibility Tests

(ert-deftest test-parser-extension-existing-unchanged ()
  "Scenario: bash-parser § 'Existing parse function unchanged'

Test that existing jf/bash-parse API returns the same structure
without breaking changes."
  (let ((result (jf/bash-parse "cat file.txt")))
    ;; Verify basic structure preserved
    (should (eq (plist-get result :type) :simple))
    (should (string= (plist-get result :command-name) "cat"))
    (should (equal (plist-get result :positional-args) '("file.txt")))

    ;; Verify optional fields don't break structure
    (should (listp result))
    (should (plist-member result :type))
    (should (plist-member result :command-name))))

(ert-deftest test-parser-extension-new-extraction-available ()
  "Scenario: bash-parser § 'New extraction function available'

Test that new file operation extraction function exists."
  (should (fboundp 'jf/bash-extract-file-operations))
  (should (fboundp 'jf/bash-detect-command-injection))
  (should (fboundp 'jf/bash-parse-nested-command))
  (should (fboundp 'jf/bash-mark-indirect-operations)))

(ert-deftest test-parser-extension-parse-return-format ()
  "Test that parse return format is unchanged for backward compatibility.

Verify that basic parsing structure matches expected format."
  (let ((result (jf/bash-parse "rm -rf /tmp/foo")))
    (should (plist-get result :type))
    (should (plist-get result :command-name))
    (should (string= (plist-get result :command-name) "rm"))
    (should (member "-rf" (plist-get result :flags)))
    (should (member "/tmp/foo" (plist-get result :positional-args)))))

;;; Command Injection Detection Tests

(ert-deftest test-parser-extension-detect-bash-c-injection ()
  "Scenario: bash-parser § 'Detect bash -c pattern'

Test detection of bash -c command injection."
  (let* ((parsed (jf/bash-parse "bash -c 'rm file.txt'"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should injection)
    (should (plist-get injection :command-injection))
    (should (equal (plist-get injection :nested-command-string) "rm file.txt"))
    (should (eq (plist-get injection :injection-type) :flag-based))
    (should (equal (plist-get injection :trigger-flag) "-c"))))

(ert-deftest test-parser-extension-detect-sh-c-injection ()
  "Scenario: bash-parser § 'Detect sh -c pattern'

Test detection of sh -c command injection."
  (let* ((parsed (jf/bash-parse "sh -c 'cat file.txt'"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should injection)
    (should (plist-get injection :command-injection))
    (should (equal (plist-get injection :nested-command-string) "cat file.txt"))
    (should (eq (plist-get injection :injection-type) :flag-based))))

(ert-deftest test-parser-extension-detect-python-c-injection ()
  "Scenario: bash-parser § 'Python -c does NOT inject bash code'

Test that python -c is NOT detected as command injection.
Python -c executes Python code, not bash, so it should not be
detected as bash command injection."
  (let* ((parsed (jf/bash-parse "python -c 'import os; os.remove(file)'"))
         (injection (jf/bash-detect-command-injection parsed)))
    ;; Should NOT detect as injection since it's Python code, not bash
    (should-not injection)))

(ert-deftest test-parser-extension-detect-env-s-injection ()
  "Scenario: bash-parser § 'Detect env -S pattern'

Test detection of env -S command injection."
  (let* ((parsed (jf/bash-parse "env -S 'bash -c cmd'"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should injection)
    (should (plist-get injection :command-injection))
    (should (equal (plist-get injection :nested-command-string) "bash -c cmd"))
    (should (eq (plist-get injection :injection-type) :flag-based))))

(ert-deftest test-parser-extension-detect-eval-injection ()
  "Test detection of eval command injection (no flag required).

Eval is a direct injection pattern without a trigger flag."
  (let* ((parsed (jf/bash-parse "eval 'rm file.txt'"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should injection)
    (should (plist-get injection :command-injection))
    (should (equal (plist-get injection :nested-command-string) "rm file.txt"))
    (should (eq (plist-get injection :injection-type) :direct))))

(ert-deftest test-parser-extension-no-false-positives ()
  "Test that regular commands are not detected as injection.

Verify no false positives for normal commands."
  (let* ((parsed (jf/bash-parse "bash script.sh"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should-not injection))

  (let* ((parsed (jf/bash-parse "python app.py"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should-not injection))

  (let* ((parsed (jf/bash-parse "rm file.txt"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should-not injection)))

(ert-deftest test-parser-extension-injection-with-flags-before ()
  "Scenario: bash-parser § 'Handle flags before injection'

Test that injection is detected even with flags before -c."
  (let* ((parsed (jf/bash-parse "bash -x -e -c 'command'"))
         (injection (jf/bash-detect-command-injection parsed)))
    (should injection)
    (should (plist-get injection :command-injection))
    (should (equal (plist-get injection :nested-command-string) "command"))
    (should (equal (plist-get injection :trigger-flag) "-c"))))

;;; Nested Command Parsing Tests

(ert-deftest test-parser-extension-parse-nested-command ()
  "Scenario: bash-parser § 'Parse nested bash command'

Test recursive parsing of nested command from bash -c."
  (let* ((nested-cmd "rm /workspace/file.txt")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "rm"))
    (should (equal (plist-get parsed :positional-args) '("/workspace/file.txt")))
    (should (= (plist-get parsed :nested-level) 1))))

(ert-deftest test-parser-extension-parse-nested-strip-single-quotes ()
  "Scenario: bash-parser § 'Strip single quotes'

Test that outer single quotes are stripped before parsing."
  (let* ((nested-cmd "'rm file.txt'")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "rm"))
    (should (equal (plist-get parsed :positional-args) '("file.txt")))))

(ert-deftest test-parser-extension-parse-nested-strip-double-quotes ()
  "Scenario: bash-parser § 'Strip double quotes'

Test that outer double quotes are stripped before parsing."
  (let* ((nested-cmd "\"cat file.txt\"")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "cat"))
    (should (equal (plist-get parsed :positional-args) '("file.txt")))))

(ert-deftest test-parser-extension-parse-nested-preserve-inner-quotes ()
  "Scenario: bash-parser § 'Preserve inner quotes'

Test that inner quotes are preserved when outer quotes are stripped.
Note: Tree-sitter bash parser normalizes quoted strings, so the actual
parsed result will have the quotes stripped from arguments."
  (let* ((nested-cmd "'grep \"pattern\" file.txt'")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "grep"))
    ;; Tree-sitter normalizes: grep "pattern" file.txt -> grep pattern file.txt
    ;; The semantic content is preserved even if quote syntax is normalized
    (should (member "pattern" (plist-get parsed :positional-args)))
    (should (member "file.txt" (plist-get parsed :positional-args)))))

(ert-deftest test-parser-extension-parse-nested-multiple-levels ()
  "Test parsing with multiple nesting levels.

Verify that nested command within nested command is handled."
  (let* ((nested-cmd "bash -c 'rm file.txt'")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "bash"))
    ;; First level should have the bash command
    (should (= (plist-get parsed :nested-level) 1))

    ;; Should detect nested injection in the parsed result
    (let ((injection (jf/bash-detect-command-injection parsed)))
      (should injection)
      (should (equal (plist-get injection :nested-command-string) "rm file.txt")))))

(ert-deftest test-parser-extension-nested-max-depth ()
  "Test that maximum nesting depth is enforced.

Verify recursion protection at depth limit."
  (let* ((parsed (jf/bash-parse-nested-command "rm file.txt" 11)))
    (should-not (plist-get parsed :success))
    (should (string-match-p "Maximum nesting depth" (plist-get parsed :error)))
    (should (= (plist-get parsed :nested-level) 11))))

(ert-deftest test-parser-extension-nested-with-variables ()
  "Scenario: bash-parser § 'Parse nested command with variables'

Test that variable references are preserved in nested commands."
  (let* ((nested-cmd "rm $FILE")
         (parsed (jf/bash-parse-nested-command nested-cmd)))
    (should (plist-get parsed :success))
    (should (string= (plist-get parsed :command-name) "rm"))
    ;; Variable reference should be preserved
    (should (member "$FILE" (plist-get parsed :positional-args)))))

;;; Indirect Operation Marking Tests

(ert-deftest test-parser-extension-mark-exec-block-indirect ()
  "Scenario: bash-parser § 'Mark indirect operation'

Test that operations from exec blocks are marked as indirect."
  (let* ((operations '((:file "{}" :operation :delete :source :exec-block)))
         (marked (jf/bash-mark-indirect-operations operations)))
    (should (= (length marked) 1))
    (should (plist-get (car marked) :indirect))
    (should (eq (plist-get (car marked) :indirect) t))))

(ert-deftest test-parser-extension-direct-operation-unmarked ()
  "Scenario: bash-parser § 'Direct operation unmarked'

Test that direct operations are not marked as indirect."
  (let* ((operations '((:file "file.txt" :operation :delete :source :positional-arg)))
         (marked (jf/bash-mark-indirect-operations operations)))
    (should (= (length marked) 1))
    ;; Should not have :indirect t
    (should-not (eq (plist-get (car marked) :indirect) t))))

(ert-deftest test-parser-extension-mixed-operations ()
  "Test marking with mix of direct and indirect operations.

Verify that only indirect operations are marked."
  (let* ((operations '((:file "a.txt" :operation :read :source :positional-arg)
                      (:file "{}" :operation :delete :source :exec-block)
                      (:file "b.txt" :operation :write :source :redirection)))
         (marked (jf/bash-mark-indirect-operations operations)))
    (should (= (length marked) 3))
    ;; First operation (positional-arg) should not be indirect
    (should-not (eq (plist-get (nth 0 marked) :indirect) t))
    ;; Second operation (exec-block) should be indirect
    (should (eq (plist-get (nth 1 marked) :indirect) t))
    ;; Third operation (redirection) should not be indirect
    (should-not (eq (plist-get (nth 2 marked) :indirect) t))))

(ert-deftest test-parser-extension-already-marked-preserved ()
  "Test that operations already marked as indirect are preserved.

Verify that pre-existing :indirect t is not lost."
  (let* ((operations '((:file "file.txt" :operation :read :source :nested-command :indirect t)))
         (marked (jf/bash-mark-indirect-operations operations)))
    (should (= (length marked) 1))
    (should (eq (plist-get (car marked) :indirect) t))
    (should (eq (plist-get (car marked) :source) :nested-command))))

(ert-deftest test-parser-extension-single-nesting-depth ()
  "Scenario: bash-parser § 'Single level nesting depth'

Test that operations from single-level nested commands have :nesting-depth 1."
  (let* ((cmd "bash -c 'rm file.txt'")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have operations from nested command
    (should (> (length ops) 0))
    ;; Find the delete operation on file.txt
    (let ((delete-op (seq-find (lambda (op)
                                 (and (eq (plist-get op :operation) :delete)
                                      (string= (plist-get op :file) "file.txt")))
                               ops)))
      (should delete-op)
      ;; Should be marked indirect
      (should (eq (plist-get delete-op :indirect) t))
      ;; Should have nesting-depth 1
      (should (eq (plist-get delete-op :nesting-depth) 1)))))

(ert-deftest test-parser-extension-double-nesting-depth ()
  "Scenario: bash-parser § 'Nested indirect operations'

Test that operations from double-nested commands have :nesting-depth 2."
  (let* ((cmd "bash -c 'sh -c \"rm file.txt\"'")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have operations from nested command
    (should (> (length ops) 0))
    ;; Find the delete operation on file.txt
    (let ((delete-op (seq-find (lambda (op)
                                 (and (eq (plist-get op :operation) :delete)
                                      (string= (plist-get op :file) "file.txt")))
                               ops)))
      (should delete-op)
      ;; Should be marked indirect
      (should (eq (plist-get delete-op :indirect) t))
      ;; Should have nesting-depth 2
      (should (eq (plist-get delete-op :nesting-depth) 2)))))

(ert-deftest test-parser-extension-direct-no-nesting-depth ()
  "Test that direct operations do not have :nesting-depth metadata.

Verify that operations from non-nested commands don't get nesting-depth."
  (let* ((cmd "rm file.txt")
         (parsed (jf/bash-parse cmd))
         (ops (jf/bash-extract-file-operations parsed)))
    ;; Should have operations
    (should (> (length ops) 0))
    ;; Find the delete operation on file.txt
    (let ((delete-op (seq-find (lambda (op)
                                 (and (eq (plist-get op :operation) :delete)
                                      (string= (plist-get op :file) "file.txt")))
                               ops)))
      (should delete-op)
      ;; Should not be marked indirect
      (should-not (plist-get delete-op :indirect))
      ;; Should not have nesting-depth
      (should-not (plist-get delete-op :nesting-depth)))))

;;; Integration Tests

(ert-deftest test-parser-extension-end-to-end-bash-c ()
  "End-to-end test: Parse bash -c, detect injection, parse nested.

Complete workflow from parsing to nested command extraction."
  (let* ((cmd "bash -c 'rm /workspace/file.txt'")
         (parsed (jf/bash-parse cmd))
         (injection (jf/bash-detect-command-injection parsed)))
    ;; Step 1: Parse top-level command
    (should (string= (plist-get parsed :command-name) "bash"))

    ;; Step 2: Detect injection
    (should injection)
    (should (plist-get injection :command-injection))

    ;; Step 3: Parse nested command
    (let ((nested (jf/bash-parse-nested-command
                   (plist-get injection :nested-command-string))))
      (should (plist-get nested :success))
      (should (string= (plist-get nested :command-name) "rm"))
      (should (equal (plist-get nested :positional-args) '("/workspace/file.txt"))))))

(ert-deftest test-parser-extension-leverage-parsed-data ()
  "Scenario: bash-parser § 'Leverage existing parsed data'

Test that extraction uses existing parse structures (redirections, exec-blocks, positional-args)."
  (let ((parsed (jf/bash-parse "cat < input.txt > output.txt")))
    ;; Verify redirections field exists and is populated
    (should (plist-get parsed :redirections))
    (should (> (length (plist-get parsed :redirections)) 0)))

  (let ((parsed (jf/bash-parse "find . -name '*.txt' -exec rm {} \\;")))
    ;; Verify exec-blocks field exists
    (should (plist-get parsed :exec-blocks)))

  (let ((parsed (jf/bash-parse "rm file1.txt file2.txt")))
    ;; Verify positional-args field exists
    (should (plist-get parsed :positional-args))
    (should (equal (plist-get parsed :positional-args) '("file1.txt" "file2.txt")))))

(provide 'test-parser-extension)
;;; test-parser-extension.el ends here
