;;; test-directory-changing-commands.el --- Directory context change tracking tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Specification tests for tracking directory context changes in bash commands.
;;
;; SECURITY REQUIREMENT:
;; Commands that change the working directory (cd, pushd, popd, PWD reassignment)
;; must be tracked to ensure subsequent file operations resolve paths correctly.
;; Failure to track directory changes creates security vulnerabilities where the
;; parser extracts different paths than execution actually accesses.
;;
;; SCOPE:
;; This test suite focuses on STATEFUL directory context tracking - commands
;; that change what subsequent relative paths resolve to:
;;
;; 1. cd command - changes working directory
;; 2. PWD variable reassignment - changes $PWD value
;; 3. pushd/popd - directory stack operations
;; 4. Subshells - isolated directory context
;;
;; INTEGRATION DESIGN:
;; The parser must track directory context through command sequences:
;; 1. Start with initial PWD from var-context
;; 2. Track cd/pushd/popd/PWD= commands to update context
;; 3. Use updated context for subsequent file operations
;; 4. Handle subshells as isolated contexts
;;
;; CURRENT STATUS:
;; These tests document REQUIRED behavior. Most will initially FAIL because
;; directory context tracking is not yet implemented.
;;
;; Test categories:
;; 1. cd command with absolute paths
;; 2. cd command with relative paths
;; 3. cd command with variables
;; 4. cd special forms (cd -, cd ~, cd with no args)
;; 5. Multiple cd commands in sequence
;; 6. PWD variable reassignment
;; 7. pushd/popd directory stack
;; 8. Subshell isolation
;; 9. cd with conditionals and error handling
;;
;; Each test verifies:
;; - Parser tracks directory changes correctly
;; - File operations after directory change use new context
;; - Multiple directory changes are sequenced properly
;; - Subshells don't affect parent context

;;; Code:

(require 'ert)
(require 'bash-parser)

;;; cd Command - Absolute Paths
;;
;; These tests verify that cd with absolute paths updates the directory context.

(ert-deftest test-cd-absolute-path-simple ()
  "Test cd /path && cat file.txt uses /path as directory context.

SECURITY: After 'cd /new/dir', relative paths must resolve against /new/dir.
IMPLEMENTATION: Parser must track cd command and update directory context."
  (let* ((parsed (jf/bash-parse "cd /new/dir && cat file.txt"))
         (var-context '((PWD . "/original/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find cat operation with file.txt resolved in /new/dir
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/dir/file.txt")))))

(ert-deftest test-cd-absolute-path-with-relative-file ()
  "Test cd /path && cat ./file.txt resolves ./ against new directory.

SECURITY: ./file.txt must resolve to /new/dir/file.txt after cd /new/dir."
  (let* ((parsed (jf/bash-parse "cd /new/dir && cat ./file.txt"))
         (var-context '((PWD . "/original/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/dir/file.txt")))))

(ert-deftest test-cd-absolute-path-multiple-operations ()
  "Test cd /path && cat file1.txt && grep pattern file2.txt

SECURITY: All operations after cd must use the new directory context."
  (let* ((parsed (jf/bash-parse "cd /new/dir && cat file1.txt && grep pattern file2.txt"))
         (var-context '((PWD . "/original/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Both file operations should be in /new/dir
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops))
          (grep-op (cl-find-if (lambda (op)
                                 (equal (plist-get op :command) "grep"))
                               ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/dir/file1.txt"))
      (should grep-op)
      (should (equal (plist-get grep-op :file) "/new/dir/file2.txt")))))

;;; cd Command - Relative Paths
;;
;; These tests verify cd with relative paths resolves against current PWD.

(ert-deftest test-cd-relative-path-subdir ()
  "Test cd subdir && cat file.txt resolves cd path and file.

SECURITY: cd subdir with PWD=/base → new PWD=/base/subdir
         Then file.txt → /base/subdir/file.txt"
  (let* ((parsed (jf/bash-parse "cd subdir && cat file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/subdir/file.txt")))))

(ert-deftest test-cd-relative-path-dot-slash ()
  "Test cd ./subdir && cat file.txt

SECURITY: cd ./subdir resolves against current PWD first."
  (let* ((parsed (jf/bash-parse "cd ./subdir && cat file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/subdir/file.txt")))))

(ert-deftest test-cd-relative-path-parent ()
  "Test cd ../other && cat file.txt navigates up then to sibling.

SECURITY: cd ../other with PWD=/base/dir/sub → /base/dir/other
         Then file.txt → /base/dir/other/file.txt"
  (let* ((parsed (jf/bash-parse "cd ../other && cat file.txt"))
         (var-context '((PWD . "/base/dir/sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/other/file.txt")))))

(ert-deftest test-cd-relative-path-current-dir ()
  "Test cd . && cat file.txt (stays in same directory).

SECURITY: cd . should be a no-op, PWD remains unchanged."
  (let* ((parsed (jf/bash-parse "cd . && cat file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/file.txt")))))

;;; cd Command - Variable Expansion
;;
;; These tests verify cd with variable references in the path.

(ert-deftest test-cd-with-variable-absolute ()
  "Test cd $DIR && cat file.txt where DIR is absolute path.

SECURITY: $DIR must be resolved before cd updates context."
  (let* ((parsed (jf/bash-parse "cd $DIR && cat file.txt"))
         (var-context '((PWD . "/original")
                       (DIR . "/target/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/target/dir/file.txt")))))

(ert-deftest test-cd-with-variable-relative ()
  "Test cd $SUBDIR && cat file.txt where SUBDIR is relative.

SECURITY: Relative $SUBDIR resolves against current PWD, then cd updates context."
  (let* ((parsed (jf/bash-parse "cd $SUBDIR && cat file.txt"))
         (var-context '((PWD . "/base/dir")
                       (SUBDIR . "sub")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/sub/file.txt")))))

(ert-deftest test-cd-with-pwd-variable ()
  "Test cd $PWD/sub && cat file.txt

SECURITY: cd $PWD/sub should resolve $PWD then navigate to subdirectory."
  (let* ((parsed (jf/bash-parse "cd $PWD/sub && cat file.txt"))
         (var-context '((PWD . "/base/dir")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/dir/sub/file.txt")))))

;;; cd Special Forms
;;
;; These tests verify cd special cases: cd -, cd ~, cd (no args).

(ert-deftest test-cd-with-dash ()
  "Test cd - && cat file.txt (returns to previous directory).

SECURITY: cd - uses OLDPWD. Parser must track OLDPWD changes.
DESIGN QUESTION: Should parser track OLDPWD? Mark as low-priority."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "cd /new && cd - && cat file.txt"))
         (var-context '((PWD . "/original")
                       (OLDPWD . "/previous")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; After cd /new, then cd -, should be back in /original
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/original/file.txt")))))

(ert-deftest test-cd-with-tilde ()
  "Test cd ~ && cat file.txt (changes to home directory).

SECURITY: cd ~ uses HOME from environment. Parser must resolve $HOME.
IMPLEMENTATION: Treat ~ as $HOME variable reference."
  (let* ((parsed (jf/bash-parse "cd ~ && cat file.txt"))
         (var-context '((PWD . "/some/dir")
                       (HOME . "/home/user")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/home/user/file.txt")))))

(ert-deftest test-cd-no-args ()
  "Test cd && cat file.txt (changes to home directory).

SECURITY: cd with no args behaves like cd ~, goes to $HOME."
  (let* ((parsed (jf/bash-parse "cd && cat file.txt"))
         (var-context '((PWD . "/some/dir")
                       (HOME . "/home/user")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/home/user/file.txt")))))

;;; Multiple cd Commands
;;
;; These tests verify sequential cd commands update context correctly.

(ert-deftest test-cd-sequence-two-cds ()
  "Test cd /first && cd /second && cat file.txt

SECURITY: Last cd wins - file.txt should resolve in /second."
  (let* ((parsed (jf/bash-parse "cd /first && cd /second && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/second/file.txt")))))

(ert-deftest test-cd-sequence-relative-after-absolute ()
  "Test cd /base && cd subdir && cat file.txt

SECURITY: cd subdir resolves against /base → /base/subdir."
  (let* ((parsed (jf/bash-parse "cd /base && cd subdir && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/base/subdir/file.txt")))))

(ert-deftest test-cd-with-operations-between ()
  "Test cd /dir1 && cat file1.txt && cd /dir2 && cat file2.txt

SECURITY: Each file operation uses directory context at its position in chain."
  (let* ((parsed (jf/bash-parse "cd /dir1 && cat file1.txt && cd /dir2 && cat file2.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Find both cat operations
    (let ((file1-op (cl-find-if (lambda (op)
                                   (string-match-p "file1" (plist-get op :file)))
                                 ops))
          (file2-op (cl-find-if (lambda (op)
                                   (string-match-p "file2" (plist-get op :file)))
                                 ops)))
      (should file1-op)
      (should (equal (plist-get file1-op :file) "/dir1/file1.txt"))
      (should file2-op)
      (should (equal (plist-get file2-op :file) "/dir2/file2.txt")))))

(ert-deftest test-cd-semicolon-separator ()
  "Test cd /dir; cat file.txt with semicolon separator.

SECURITY: Semicolon separation should behave like && for cd context tracking.
NOTE: Parser may have existing issues with semicolon parsing."
  (let* ((parsed (jf/bash-parse "cd /new/dir; cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/dir/file.txt")))))

;;; PWD Variable Reassignment
;;
;; These tests verify direct PWD variable assignments.

(ert-deftest test-pwd-assignment-inline ()
  "Test PWD=/new/path cat file.txt (inline environment variable).

SECURITY: Inline PWD assignment affects only the single command.
Fixed by bead emacs-6ysw: Parser now extracts inline env vars and applies them."
  (let* ((parsed (jf/bash-parse "PWD=/new/path cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; Inline assignment affects this command only
      (should (equal (plist-get cat-op :file) "/new/path/file.txt")))))

(ert-deftest test-pwd-assignment-export ()
  "Test export PWD=/new/path && cat file.txt

SECURITY: export PWD updates context for subsequent commands."
  (let* ((parsed (jf/bash-parse "export PWD=/new/path && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/path/file.txt")))))

(ert-deftest test-pwd-assignment-simple ()
  "Test PWD=/new/path; cat file.txt (simple assignment).

SECURITY: Simple assignment updates PWD for subsequent commands.
Fixed by bead emacs-wpa0: semicolon-separated assignments now parsed as chains."
  (let* ((parsed (jf/bash-parse "PWD=/new/path; cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/path/file.txt")))))

(ert-deftest test-pwd-assignment-with-variable ()
  "Test PWD=$NEWDIR && cat file.txt

SECURITY: PWD assignment with variable must resolve variable first."
  (let* ((parsed (jf/bash-parse "PWD=$NEWDIR && cat file.txt"))
         (var-context '((PWD . "/original")
                       (NEWDIR . "/target")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/target/file.txt")))))

;;; pushd/popd Directory Stack
;;
;; These tests verify pushd/popd commands (if implemented).

(ert-deftest test-pushd-absolute-path ()
  "Test pushd /new/dir && cat file.txt

SECURITY: pushd changes directory like cd but also pushes to stack.
DESIGN QUESTION: Low priority - pushd/popd less common than cd."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "pushd /new/dir && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/new/dir/file.txt")))))

(ert-deftest test-popd-returns-to-previous ()
  "Test pushd /new && pushd /other && popd && cat file.txt

SECURITY: popd should return to previous directory in stack.
DESIGN QUESTION: Low priority - requires full directory stack tracking."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "pushd /new && pushd /other && popd && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; After two pushds and one popd, should be in /new
      (should (equal (plist-get cat-op :file) "/new/file.txt")))))

;;; Subshell Isolation
;;
;; These tests verify subshells create isolated directory contexts.

(ert-deftest test-subshell-cd-isolation ()
  "Test (cd /subdir && cat sub.txt) && cat main.txt

SECURITY: cd in subshell must NOT affect parent context.
         sub.txt → /subdir/sub.txt
         main.txt → (original PWD)/main.txt"
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "(cd /subdir && cat sub.txt) && cat main.txt"))
         (var-context '((PWD . "/base")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((sub-op (cl-find-if (lambda (op)
                                 (string-match-p "sub\\.txt" (plist-get op :file)))
                               ops))
          (main-op (cl-find-if (lambda (op)
                                  (string-match-p "main\\.txt" (plist-get op :file)))
                                ops)))
      (should sub-op)
      (should (equal (plist-get sub-op :file) "/subdir/sub.txt"))
      (should main-op)
      ;; main.txt should use original PWD, not subshell's cd
      (should (equal (plist-get main-op :file) "/base/main.txt")))))

(ert-deftest test-subshell-pwd-assignment-isolation ()
  "Test (PWD=/sub && cat sub.txt) && cat main.txt

SECURITY: PWD assignment in subshell isolated from parent."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "(PWD=/sub && cat sub.txt) && cat main.txt"))
         (var-context '((PWD . "/base")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((sub-op (cl-find-if (lambda (op)
                                 (string-match-p "sub\\.txt" (plist-get op :file)))
                               ops))
          (main-op (cl-find-if (lambda (op)
                                  (string-match-p "main\\.txt" (plist-get op :file)))
                                ops)))
      (should sub-op)
      (should (equal (plist-get sub-op :file) "/sub/sub.txt"))
      (should main-op)
      (should (equal (plist-get main-op :file) "/base/main.txt")))))

(ert-deftest test-nested-subshells ()
  "Test ((cd /a && cat a.txt) && cd /b && cat b.txt) && cat c.txt

SECURITY: Nested subshells each have isolated context.
         a.txt → /a/a.txt (inner subshell)
         b.txt → /b/b.txt (outer subshell)
         c.txt → (original)/c.txt (parent)"
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "((cd /a && cat a.txt) && cd /b && cat b.txt) && cat c.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((a-op (cl-find-if (lambda (op)
                              (string-match-p "a\\.txt" (plist-get op :file)))
                            ops))
          (b-op (cl-find-if (lambda (op)
                              (string-match-p "b\\.txt" (plist-get op :file)))
                            ops))
          (c-op (cl-find-if (lambda (op)
                              (string-match-p "c\\.txt" (plist-get op :file)))
                            ops)))
      (should a-op)
      (should (equal (plist-get a-op :file) "/a/a.txt"))
      (should b-op)
      (should (equal (plist-get b-op :file) "/b/b.txt"))
      (should c-op)
      (should (equal (plist-get c-op :file) "/original/c.txt")))))

;;; cd with Conditionals and Error Handling
;;
;; These tests verify cd in conditional contexts.

(ert-deftest test-cd-with-or-fallback ()
  "Test cd /dir || exit 1; cat file.txt

SECURITY: Parser should assume cd succeeds for path resolution.
DESIGN NOTE: Static analysis assumes success path unless impossible."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "cd /dir || exit 1; cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; Assume cd succeeds
      (should (equal (plist-get cat-op :file) "/dir/file.txt")))))

(ert-deftest test-cd-in-if-statement ()
  "Test if cd /dir; then cat file.txt; fi

SECURITY: cd in if condition updates context for then block."
  (let* ((parsed (jf/bash-parse "if cd /dir; then cat file.txt; fi"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      (should (equal (plist-get cat-op :file) "/dir/file.txt")))))

(ert-deftest test-cd-conditional-both-branches ()
  "Test if [ -d /dir ]; then cd /dir; cat a.txt; else cd /other; cat b.txt; fi

SECURITY: Each branch has different directory context.
         Both possibilities should be tracked."
  (let* ((parsed (jf/bash-parse "if [ -d /dir ]; then cd /dir && cat a.txt; else cd /other && cat b.txt; fi"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Should find operations in both branches with correct contexts
    (let ((a-op (cl-find-if (lambda (op)
                              (string-match-p "a\\.txt" (plist-get op :file)))
                            ops))
          (b-op (cl-find-if (lambda (op)
                              (string-match-p "b\\.txt" (plist-get op :file)))
                            ops)))
      (should a-op)
      (should (equal (plist-get a-op :file) "/dir/a.txt"))
      (should b-op)
      (should (equal (plist-get b-op :file) "/other/b.txt")))))

;;; Edge Cases
;;
;; These tests verify edge cases and boundary conditions.

(ert-deftest test-cd-empty-string ()
  "Test cd '' && cat file.txt (cd to empty string).

SECURITY: cd '' typically fails or goes to PWD. Assume no change."
  (let* ((parsed (jf/bash-parse "cd '' && cat file.txt"))
         (var-context '((PWD . "/base")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; cd '' typically fails, so && chain breaks - but for static analysis,
      ;; if we assume success, it would be a no-op
      (should (equal (plist-get cat-op :file) "/base/file.txt")))))

(ert-deftest test-cd-to-file-not-directory ()
  "Test cd file.txt && cat other.txt (cd to non-directory).

SECURITY: cd to file would fail at runtime, but static analysis may not know.
         For scope validation, assume it could succeed (conservative)."
  (let* ((parsed (jf/bash-parse "cd file.txt && cat other.txt"))
         (var-context '((PWD . "/base")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; Static analysis can't know file.txt isn't a directory
      ;; Conservative: assume cd succeeds to /base/file.txt
      (should (equal (plist-get cat-op :file) "/base/file.txt/other.txt")))))

(ert-deftest test-multiple-cds-with-wildcards ()
  "Test cd /a*/b* && cat file.txt (cd with glob patterns).

SECURITY: cd with globs typically expands to first match.
DESIGN: Low priority - complex runtime behavior."
  (let* ((parsed (jf/bash-parse "cd /a*/b* && cat file.txt"))
         (var-context '((PWD . "/original")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; Should recognize cd path contains patterns
      ;; For now, may leave as pattern (unresolved)
      (should (plist-get cat-op :file)))))

(ert-deftest test-cd-with-command-substitution ()
  "Test cd $(dirname $FILE) && cat base.txt

SECURITY: cd with command substitution needs full substitution resolution.
IMPLEMENTATION: Fixed by emacs-n1jk - static evaluation of dirname/basename commands."
  (let* ((parsed (jf/bash-parse "cd $(dirname $FILE) && cat base.txt"))
         (var-context '((PWD . "/original")
                       (FILE . "/path/to/file.txt")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((cat-op (cl-find-if (lambda (op)
                                (equal (plist-get op :command) "cat"))
                              ops)))
      (should cat-op)
      ;; $(dirname /path/to/file.txt) → /path/to
      (should (equal (plist-get cat-op :file) "/path/to/base.txt")))))

;;; Integration Tests - Real-World Patterns
;;
;; These tests verify common real-world patterns.

(ert-deftest test-cd-and-run-script ()
  "Test cd /project && ./run-tests.sh pattern.

SECURITY: Script execution path must resolve in new directory."
  (let* ((parsed (jf/bash-parse "cd /project && ./run-tests.sh"))
         (var-context '((PWD . "/home/user")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((script-op (cl-find-if (lambda (op)
                                   (string-match-p "run-tests" (plist-get op :file)))
                                 ops)))
      (should script-op)
      (should (equal (plist-get script-op :file) "/project/run-tests.sh")))))

(ert-deftest test-cd-for-loop-files ()
  "Test cd /logs && for f in *.log; do cat $f; done

SECURITY: Both glob pattern and loop variable resolve in /logs."
  :expected-result :failed
  (let* ((parsed (jf/bash-parse "cd /logs && for f in *.log; do cat $f; done"))
         (var-context '((PWD . "/home")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    ;; Pattern should be in /logs
    (let ((pattern-op (cl-find-if (lambda (op)
                                     (and (plist-get op :pattern)
                                          (string-match-p "\\.log" (plist-get op :file))))
                                   ops)))
      (should pattern-op)
      (should (string-prefix-p "/logs/" (plist-get pattern-op :file))))))

(ert-deftest test-deploy-script-pattern ()
  "Test cd /deploy && ./build.sh && cd /target && ./install.sh

SECURITY: Common deployment pattern with multiple cd and script executions."
  (let* ((parsed (jf/bash-parse "cd /deploy && ./build.sh && cd /target && ./install.sh"))
         (var-context '((PWD . "/home")))
         (ops (jf/bash-extract-file-operations parsed var-context)))
    (let ((build-op (cl-find-if (lambda (op)
                                  (string-match-p "build" (plist-get op :file)))
                                ops))
          (install-op (cl-find-if (lambda (op)
                                    (string-match-p "install" (plist-get op :file)))
                                  ops)))
      (should build-op)
      (should (equal (plist-get build-op :file) "/deploy/build.sh"))
      (should install-op)
      (should (equal (plist-get install-op :file) "/target/install.sh")))))

(provide 'test-directory-changing-commands)
;;; test-directory-changing-commands.el ends here
