;;; test-pwd-directory-context.el --- PWD/directory context resolution tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Specification tests for PWD/directory parameter context in bash parser.
;;
;; CRITICAL SECURITY REQUIREMENT:
;; The bash-parser integrates with run_bash_command(command, directory) which
;; requires EXACT path resolution matching actual execution behavior. Any mismatch
;; between parser-extracted paths and execution-accessed paths is a security
;; vulnerability allowing unauthorized file access.
;;
;; INTEGRATION DESIGN:
;; 1. Caller creates var-context with ((PWD . directory))
;; 2. Parser resolves $PWD references using var-context
;; 3. Parser resolves relative paths (., ./, ../) using PWD from var-context
;; 4. Parser returns absolute paths for all file operations
;; 5. Caller validates absolute paths against scope.yml patterns
;;
;; CURRENT STATUS:
;; These tests document REQUIRED behavior for scope-shell-tools integration.
;; Many tests will initially FAIL because relative path resolution is not yet
;; implemented in the parser. The parser currently only resolves variables,
;; not relative paths.
;;
;; Test categories:
;; 1. Explicit $PWD variable references (should work with current var-context)
;; 2. Relative path resolution (., ./, ../) - NEEDS IMPLEMENTATION
;; 3. Command substitutions with pwd - NEEDS DESIGN
;; 4. Variable chains with PWD context
;; 5. Nested structures with relative paths
;;
;; Each test verifies:
;; - Parser extracts correct absolute path
;; - Extracted path matches what execution would access
;; - No security gap between static analysis and runtime behavior

;;; Code:

(require 'ert)
(require 'bash-parser)

;;; Explicit $PWD Variable References
;;
;; These tests verify that $PWD in var-context is properly resolved.
;; Should work with existing variable resolution infrastructure.

(ert-deftest test-pwd-explicit-variable-simple ()
  "Test that $PWD variable resolves to directory from var-context.

SECURITY: Ensures $PWD/file.txt resolves to correct absolute path."
  (let* ((parsed (jf/bash-parse "cat $PWD/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      (should (equal (plist-get op :file) "/base/dir/file.txt"))
      (should (eq (plist-get op :operation) :read))
      (should (eq (plist-get op :confidence) :high)))))

(ert-deftest test-pwd-explicit-variable-nested-path ()
  "Test $PWD with nested path components.

SECURITY: $PWD/subdir/file.txt must resolve correctly."
  (let* ((parsed (jf/bash-parse "cat $PWD/subdir/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/subdir/file.txt"))))

(ert-deftest test-pwd-explicit-variable-in-redirection ()
  "Test $PWD in output redirection.

SECURITY: echo > $PWD/output.txt must resolve for write validation."
  (let* ((parsed (jf/bash-parse "echo test > $PWD/output.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    (let ((write-op (cl-find-if (lambda (op)
                                   (eq (plist-get op :operation) :write))
                                 ops)))
      (should write-op)
      (should (equal (plist-get write-op :file) "/base/dir/output.txt")))))

(ert-deftest test-pwd-without-var-context ()
  "Test that $PWD remains unresolved without var-context.

SECURITY: Unresolved variables should be detectable for validation rejection."
  (let* ((parsed (jf/bash-parse "cat $PWD/file.txt"))
         (ops (jf/bash-extract-file-operations parsed nil)))
    (should (= (length ops) 1))
    (let ((op (car ops)))
      ;; Should contain unresolved $PWD
      (should (string-match-p "\\$PWD" (plist-get op :file))))))

;;; Relative Path Resolution - Current Directory (.)
;;
;; These tests specify how . (current directory) should resolve.
;; IMPLEMENTATION NEEDED: Parser currently does not resolve relative paths.

(ert-deftest test-relative-path-dot-alone ()
  "Test that . resolves to PWD from var-context.

SECURITY: find . must resolve to exact directory for scope validation.
IMPLEMENTATION: Parser needs to resolve . using PWD from var-context."
  (let* ((parsed (jf/bash-parse "find . -name '*.txt'"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    (let ((dir-op (cl-find-if (lambda (op)
                                 (eq (plist-get op :operation) :read-directory))
                               ops)))
      (should dir-op)
      (should (equal (plist-get dir-op :file) "/base/dir")))))

(ert-deftest test-relative-path-dot-ls ()
  "Test ls . resolves to PWD.

SECURITY: ls . must validate against PWD's scope, not literal '.'."
  :expected-result :failed  ; ls semantics don't produce operations for directory arguments yet
  (let* ((parsed (jf/bash-parse "ls ."))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir"))))

(ert-deftest test-relative-path-dot-in-pipeline ()
  "Test . in pipeline context.

SECURITY: grep pattern . | head must resolve . to PWD."
  (let* ((parsed (jf/bash-parse "grep pattern . | head -10"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    ;; grep should show operation on /base/dir
    (let ((grep-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :command) "grep"))
                                ops)))
      (should grep-op)
      (should (equal (plist-get grep-op :file) "/base/dir")))))

;;; Relative Path Resolution - ./ Prefix
;;
;; These tests specify how ./ paths should resolve.
;; IMPLEMENTATION NEEDED: Parser must recognize ./ and resolve against PWD.

(ert-deftest test-relative-path-dot-slash-file ()
  "Test ./file.txt resolves to PWD/file.txt.

SECURITY: cat ./file.txt must resolve to /base/dir/file.txt for validation."
  (let* ((parsed (jf/bash-parse "cat ./file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/file.txt"))))

(ert-deftest test-relative-path-dot-slash-nested ()
  "Test ./subdir/file.txt resolves correctly.

SECURITY: ./subdir/file.txt → /base/dir/subdir/file.txt"
  (let* ((parsed (jf/bash-parse "cat ./subdir/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/subdir/file.txt"))))

(ert-deftest test-relative-path-dot-slash-script-execution ()
  "Test ./script.sh resolves for execute operations.

SECURITY: ./script.sh must resolve to /base/dir/script.sh.
FUTURE: When :execute operations implemented, verify operation type."
  (let* ((parsed (jf/bash-parse "./script.sh"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    (let ((exec-op (car ops)))
      (should (equal (plist-get exec-op :file) "/base/dir/script.sh"))
      ;; Future: should be :execute operation
      (should (plist-get exec-op :self-executing)))))

(ert-deftest test-relative-path-dot-slash-with-args ()
  "Test ./script.sh with arguments.

SECURITY: ./deploy.sh staging must resolve script path, not arguments."
  (let* ((parsed (jf/bash-parse "./deploy.sh staging us-west-2"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/deploy.sh"))
    ;; Verify arguments are preserved
    (should (equal (plist-get (car ops) :script-args)
                   '("staging" "us-west-2")))))

;;; Relative Path Resolution - ../ Parent Directory
;;
;; These tests specify how ../ paths should resolve.
;; IMPLEMENTATION NEEDED: Parser must resolve ../ against PWD.

(ert-deftest test-relative-path-parent-simple ()
  "Test ../file.txt resolves to parent directory.

SECURITY: cat ../file.txt with PWD=/base/dir/sub → /base/dir/file.txt"
  (let* ((parsed (jf/bash-parse "cat ../file.txt"))
         (var-context '((PWD . "/base/dir/sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/file.txt"))))

(ert-deftest test-relative-path-parent-multiple-levels ()
  "Test ../../file.txt resolves correctly.

SECURITY: ../../file.txt with PWD=/a/b/c → /a/file.txt"
  (let* ((parsed (jf/bash-parse "cat ../../file.txt"))
         (var-context '((PWD . "/a/b/c")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/a/file.txt"))))

(ert-deftest test-relative-path-parent-with-subdir ()
  "Test ../other/file.txt navigation pattern.

SECURITY: ../other/file.txt with PWD=/base/dir/sub → /base/dir/other/file.txt"
  (let* ((parsed (jf/bash-parse "cat ../other/file.txt"))
         (var-context '((PWD . "/base/dir/sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/other/file.txt"))))

(ert-deftest test-relative-path-parent-script-execution ()
  "Test ../bin/runner script execution.

SECURITY: ../bin/runner with PWD=/base/project → /base/bin/runner"
  (let* ((parsed (jf/bash-parse "../bin/runner"))
         (var-context '((PWD . "/base/project")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/bin/runner"))))

;;; Variable Chains with PWD Context
;;
;; These tests verify variable assignments that reference PWD or relative paths.
;; IMPLEMENTATION NEEDED: Track variable values through assignments.

(ert-deftest test-variable-chain-pwd-assignment ()
  "Test BASE=$PWD assignment and usage.

SECURITY: BASE=$PWD; cat $BASE/file.txt must resolve to /base/dir/file.txt
IMPLEMENTATION: Variable chain tracking implemented (bead emacs-1op2).
Fixed by bead emacs-wpa0: semicolon-separated assignments now parsed as chains."
  (let* ((parsed (jf/bash-parse "BASE=$PWD; cat $BASE/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find the cat operation with resolved path
    (let ((cat-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :command) "cat"))
                               ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/file.txt")))))

(ert-deftest test-variable-chain-relative-path-assignment ()
  "Test DIR=./sub assignment with relative path.

SECURITY: DIR=./sub; cat $DIR/file.txt → /base/dir/sub/file.txt
IMPLEMENTATION: Variable chain tracking implemented (bead emacs-1op2).
Fixed by bead emacs-wpa0: semicolon-separated assignments now parsed as chains."
  (let* ((parsed (jf/bash-parse "DIR=./sub; cat $DIR/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :command) "cat"))
                               ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/sub/file.txt")))))

(ert-deftest test-variable-chain-parent-path-assignment ()
  "Test PATH=../other assignment with parent directory.

SECURITY: PATH=../other; cat $PATH/file.txt with PWD=/dir/sub → /dir/other/file.txt
IMPLEMENTATION: Variable chain tracking implemented (bead emacs-1op2).
Fixed by bead emacs-wpa0: semicolon-separated assignments now parsed as chains."
  (let* ((parsed (jf/bash-parse "PATH=../other; cat $PATH/file.txt"))
         (var-context '((PWD . "/dir/sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :command) "cat"))
                               ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/dir/other/file.txt")))))

;;; Nested Structures with Relative Paths
;;
;; These tests verify relative path resolution in loops and conditionals.

(ert-deftest test-nested-for-loop-relative-pattern ()
  "Test for loop with relative path pattern.

SECURITY: for f in ./src/*.txt must resolve ./src to /base/dir/src"
  (let* ((parsed (jf/bash-parse "for f in ./src/*.txt; do cat \"$f\"; done"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find pattern operation with resolved path
    (let ((pattern-op (cl-find-if (lambda (op)
                                     (plist-get op :pattern))
                                   ops)))
      (should pattern-op)
      (should (string-match-p "^/base/dir/src/.*\\.txt"
                             (plist-get pattern-op :file))))))

(ert-deftest test-nested-conditional-relative-test ()
  "Test if statement with relative path test.

SECURITY: if [ -f ./config ] must resolve ./config to /base/dir/config"
  (let* ((parsed (jf/bash-parse "if [ -f ./config ]; then cat ./config; fi"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find both test and cat operations on /base/dir/config
    (should (>= (length ops) 1))
    (dolist (op ops)
      (should (equal (plist-get op :file) "/base/dir/config")))))

(ert-deftest test-nested-find-exec-relative-paths ()
  "Test find -exec with relative paths in both positions.

SECURITY: find . -exec cat {} must resolve . to /base/dir"
  (let* ((parsed (jf/bash-parse "find . -name '*.txt' -exec cat {} \\;"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find the find operation on /base/dir
    (let ((find-op (cl-find-if (lambda (op)
                                  (equal (plist-get op :command) "find"))
                                ops)))
      (should find-op)
      (should (equal (plist-get find-op :file) "/base/dir")))))

;;; Command Substitution with pwd
;;
;; These tests specify how $(pwd) should be handled.
;; DESIGN NEEDED: Should pwd substitution use PWD from var-context?

(ert-deftest test-pwd-substitution-simple ()
  "Test cat $(pwd)/file.txt command substitution.

SECURITY: $(pwd) should resolve to PWD from var-context.
DESIGN QUESTION: Is static analysis of pwd feasible?"
  (let* ((parsed (jf/bash-parse "cat $(pwd)/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/file.txt"))))

(ert-deftest test-pwd-substitution-nested ()
  "Test nested pwd substitution: cat $(basename $(pwd))/file.txt

SECURITY: $(basename $(pwd)) with PWD=/Users/name/project → project/file.txt
DESIGN QUESTION: How deep should substitution analysis go?"
  :expected-result :failed  ; $(pwd) substitution not yet implemented
  (let* ((parsed (jf/bash-parse "cat $(basename $(pwd))/file.txt"))
         (var-context '((PWD . "/Users/name/project")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "project/file.txt"))))

(ert-deftest test-pwd-backtick-substitution ()
  "Test backtick pwd substitution: cat `pwd`/file.txt

SECURITY: Backtick and $() forms should behave identically."
  (let* ((parsed (jf/bash-parse "cat `pwd`/file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "/base/dir/file.txt"))))

;;; Edge Cases and Security Boundaries

(ert-deftest test-pwd-context-multiple-operations ()
  "Test multiple file operations with PWD context.

SECURITY: All operations in pipeline must use same PWD for resolution."
  (let* ((parsed (jf/bash-parse "cat ./input.txt | grep pattern > ./output.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (>= (length ops) 2))
    ;; Both paths should be resolved
    (let ((input-op (cl-find-if (lambda (op)
                                   (string-match-p "input\\.txt"
                                                  (plist-get op :file)))
                                 ops))
          (output-op (cl-find-if (lambda (op)
                                    (string-match-p "output\\.txt"
                                                   (plist-get op :file)))
                                  ops)))
      (should input-op)
      (should (equal (plist-get input-op :file) "/base/dir/input.txt"))
      (should output-op)
      (should (equal (plist-get output-op :file) "/base/dir/output.txt")))))

(ert-deftest test-pwd-context-mixed-absolute-relative ()
  "Test command with both absolute and relative paths.

SECURITY: cp /tmp/file.txt ./dest.txt must preserve absolute and resolve relative."
  (let* ((parsed (jf/bash-parse "cp /tmp/file.txt ./dest.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 2))
    (let ((read-op (cl-find-if (lambda (op)
                                  (eq (plist-get op :operation) :read))
                                ops))
          (write-op (cl-find-if (lambda (op)
                                   (eq (plist-get op :operation) :write))
                                 ops)))
      (should read-op)
      (should (equal (plist-get read-op :file) "/tmp/file.txt"))
      (should write-op)
      (should (equal (plist-get write-op :file) "/base/dir/dest.txt")))))

(ert-deftest test-pwd-context-without-pwd-in-context ()
  "Test that relative paths remain unresolved without PWD in var-context.

SECURITY: Parser should not guess PWD - explicit context required."
  (let* ((parsed (jf/bash-parse "cat ./file.txt"))
         (var-context nil)  ; No PWD provided
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    ;; Path should remain relative (unresolved)
    (should (equal (plist-get (car ops) :file) "./file.txt"))))

(ert-deftest test-pwd-context-empty-var-context ()
  "Test relative paths with empty var-context.

SECURITY: Empty context should leave paths unresolved."
  (let* ((parsed (jf/bash-parse "cat ../file.txt"))
         (var-context '())  ; Empty alist
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (should (= (length ops) 1))
    (should (equal (plist-get (car ops) :file) "../file.txt"))))

;;; Execution Parity Validation
;;
;; These tests verify that parser output matches actual execution behavior.
;; CRITICAL for security - any mismatch is a vulnerability.

(ert-deftest test-execution-parity-relative-path ()
  "Verify parser extraction matches actual execution for relative paths.

SECURITY: Parser must extract EXACTLY the path that execution accesses."
  (let* ((test-dir (make-temp-file "bash-parser-test-" t))
         (test-file (expand-file-name "test.txt" test-dir))
         (command "cat ./test.txt")
         (var-context `((PWD . ,test-dir))))

    (unwind-protect
        (progn
          ;; Create test file
          (write-region "test content" nil test-file)

          ;; Parse command
          (let* ((parsed (jf/bash-parse command))
                 (ops (jf/bash-extract-file-operations parsed var-context)))

            ;; Verify parser extracted the absolute path
            (should (= (length ops) 1))
            (should (equal (plist-get (car ops) :file) test-file))

            ;; Verify execution would access that path
            (let ((default-directory test-dir))
              (should (file-exists-p "./test.txt"))
              (should (equal (expand-file-name "./test.txt")
                           test-file)))))

      ;; Cleanup
      (delete-directory test-dir t))))

(provide 'test-pwd-directory-context)
;;; test-pwd-directory-context.el ends here
