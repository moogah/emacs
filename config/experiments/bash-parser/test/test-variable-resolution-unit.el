;;; test-variable-resolution-unit.el --- Unit tests for variable resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for bash-parser variable resolution functions.
;;
;; Tests the four core variable resolution functions:
;; 1. jf/bash-detect-variable-references - Detection without resolution
;; 2. jf/bash-resolve-variables - Full and partial variable resolution
;; 3. jf/bash--resolve-assignment-value - Assignment value resolution
;; 4. jf/bash--extract-assignments-from-command - Assignment extraction
;;
;; These are pure unit tests focusing on individual function behavior.
;; For integration tests, see test-variable-chain-ampersand.el.

;;; Code:

(require 'test-helper (expand-file-name "test-helper.el"
                                        (file-name-directory load-file-name)))

;;; Variable Detection Tests (jf/bash-detect-variable-references)

(ert-deftest test-detect-no-variables ()
  "Test detection with no variables returns (nil)."
  (should (equal (jf/bash-detect-variable-references "/absolute/path.txt")
                 '(nil))))

(ert-deftest test-detect-simple-dollar-variable ()
  "Test detection of simple $VAR syntax."
  (should (equal (jf/bash-detect-variable-references "$HOME/file.txt")
                 '(t . ("HOME")))))

(ert-deftest test-detect-braced-variable ()
  "Test detection of braced ${VAR} syntax."
  (should (equal (jf/bash-detect-variable-references "${TEMP_DIR}/output.txt")
                 '(t . ("TEMP_DIR")))))

(ert-deftest test-detect-multiple-variables ()
  "Test detection of multiple variables in path."
  (should (equal (jf/bash-detect-variable-references "$SRC/$FILE.txt")
                 '(t . ("SRC" "FILE")))))

(ert-deftest test-detect-mixed-variable-syntax ()
  "Test detection of both $VAR and ${VAR} syntax."
  (should (equal (jf/bash-detect-variable-references "$A/${B}/file")
                 '(t . ("A" "B")))))

(ert-deftest test-detect-variable-order ()
  "Test that variables are returned in order of appearance."
  (should (equal (jf/bash-detect-variable-references "$FIRST/$SECOND/$THIRD")
                 '(t . ("FIRST" "SECOND" "THIRD")))))

(ert-deftest test-detect-duplicate-variables ()
  "Test that duplicate variables are all detected."
  (should (equal (jf/bash-detect-variable-references "$VAR/subdir/$VAR/file")
                 '(t . ("VAR" "VAR")))))

(ert-deftest test-detect-variable-at-end ()
  "Test detection of variable at end of path."
  (should (equal (jf/bash-detect-variable-references "/path/to/$FILE")
                 '(t . ("FILE")))))

;;; Variable Resolution Tests (jf/bash-resolve-variables)

(ert-deftest test-resolve-no-variables ()
  "Test resolution with no variables returns original path."
  (should (equal (jf/bash-resolve-variables "/absolute/path.txt" nil)
                 "/absolute/path.txt")))

(ert-deftest test-resolve-all-variables-simple ()
  "Test full resolution with all variables in context."
  (let ((context '((WORKSPACE . "/workspace"))))
    (should (equal (jf/bash-resolve-variables "$WORKSPACE/file.txt" context)
                   "/workspace/file.txt"))))

(ert-deftest test-resolve-multiple-variables-fully ()
  "Test full resolution of multiple variables."
  (let ((context '((A . "/a") (B . "b"))))
    (should (equal (jf/bash-resolve-variables "$A/${B}/file" context)
                   "/a/b/file"))))

(ert-deftest test-resolve-partial-some-unresolved ()
  "Test partial resolution returns plist with :unresolved."
  (let* ((context '((WORKSPACE . "/workspace")))
         (result (jf/bash-resolve-variables "$WORKSPACE/$FILE" context)))
    (should (listp result))
    (should (equal (plist-get result :path) "/workspace/$FILE"))
    (should (equal (plist-get result :unresolved) '("FILE")))))

(ert-deftest test-resolve-none-no-context ()
  "Test that unresolved variables are marked when no context provided."
  (let ((result (jf/bash-resolve-variables "$UNKNOWN/file.txt" nil)))
    (should (listp result))
    (should (equal (plist-get result :path) "$UNKNOWN/file.txt"))
    (should (equal (plist-get result :unresolved) '("UNKNOWN")))))

(ert-deftest test-resolve-normalizes-slashes ()
  "Test that multiple slashes are normalized to single slash."
  (let ((context '((VAR . "/value/"))))
    (should (equal (jf/bash-resolve-variables "$VAR/file" context)
                   "/value/file"))))

(ert-deftest test-resolve-word-boundary ()
  "Test word boundary prevents partial matches like $VAR matching $VARIABLE."
  (let ((context '((VAR . "/short"))))
    (let ((result (jf/bash-resolve-variables "$VARIABLE" context)))
      (should (listp result))
      (should (equal (plist-get result :unresolved) '("VARIABLE"))))))

(ert-deftest test-resolve-braced-vs-simple ()
  "Test both ${VAR} and $VAR syntax are resolved correctly."
  (let ((context '((HOME . "/home/user"))))
    (should (equal (jf/bash-resolve-variables "${HOME}/file" context)
                   "/home/user/file"))
    (should (equal (jf/bash-resolve-variables "$HOME/file" context)
                   "/home/user/file"))))

(ert-deftest test-resolve-duplicate-variable-references ()
  "Test that duplicate variable references are all resolved."
  (let ((context '((VAR . "value"))))
    (should (equal (jf/bash-resolve-variables "$VAR/$VAR/file" context)
                   "value/value/file"))))

;;; Assignment Value Resolution Tests (jf/bash--resolve-assignment-value)

(ert-deftest test-resolve-assignment-simple-variable ()
  "Test assignment value resolution with simple variable."
  (let ((context '((PWD . "/base"))))
    (should (equal (jf/bash--resolve-assignment-value "$PWD" context)
                   "/base"))))

(ert-deftest test-resolve-assignment-relative-path ()
  "Test assignment value resolution with relative path."
  (let ((context '((PWD . "/base"))))
    (should (equal (jf/bash--resolve-assignment-value "./sub" context)
                   "/base/sub"))))

(ert-deftest test-resolve-assignment-command-substitution-pwd ()
  "Test assignment value resolution with $(pwd) substitution."
  (let ((context '((PWD . "/base"))))
    (should (equal (jf/bash--resolve-assignment-value "$(pwd)" context)
                   "/base"))))

(ert-deftest test-resolve-assignment-command-substitution-dirname ()
  "Test assignment value resolution with $(dirname $VAR)."
  (let ((context '((FILE . "/a/b/c.txt"))))
    (should (equal (jf/bash--resolve-assignment-value "$(dirname $FILE)" context)
                   "/a/b"))))

(ert-deftest test-resolve-assignment-command-substitution-basename ()
  "Test assignment value resolution with $(basename $VAR)."
  (let ((context '((FILE . "/path/to/file.txt"))))
    (should (equal (jf/bash--resolve-assignment-value "$(basename $FILE)" context)
                   "file.txt"))))

(ert-deftest test-resolve-assignment-unresolved-variable ()
  "Test assignment value resolution with unresolved variable returns plist."
  (let ((result (jf/bash--resolve-assignment-value "$UNKNOWN" nil)))
    (should (listp result))
    (should (equal (plist-get result :path) "$UNKNOWN"))
    (should (equal (plist-get result :unresolved) '("UNKNOWN")))))

(ert-deftest test-resolve-assignment-unresolved-command-substitution ()
  "Test assignment value with unresolvable command substitution returns plist."
  (let ((result (jf/bash--resolve-assignment-value "$(dirname $UNKNOWN)" nil)))
    (should (listp result))
    ;; When variable cannot be resolved, returns variable name as unresolved
    (should (equal (plist-get result :unresolved) '("UNKNOWN")))))

(ert-deftest test-resolve-assignment-combined-resolution ()
  "Test assignment value with variable + relative path resolution."
  (let ((context '((BASE . "/base") (PWD . "/current"))))
    (should (equal (jf/bash--resolve-assignment-value "$BASE/sub" context)
                   "/base/sub"))))

;;; Assignment Extraction Tests (jf/bash--extract-assignments-from-command)

(ert-deftest test-extract-assignment-unified-format ()
  "Test extraction of VAR=value in command-name (unified format)."
  (let* ((command '(:command-name "BASE=/tmp" :positional-args nil))
         (context '((PWD . "/current")))
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal assignments '((BASE . "/tmp"))))))

(ert-deftest test-extract-assignment-with-variable-resolution ()
  "Test that assignment values are resolved using context."
  (let* ((command '(:command-name "DIR=$PWD/sub" :positional-args nil))
         (context '((PWD . "/base")))
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal assignments '((DIR . "/base/sub"))))))

(ert-deftest test-extract-assignment-with-relative-path ()
  "Test assignment extraction resolves relative paths."
  (let* ((command '(:command-name "DIR=./sub" :positional-args nil))
         (context '((PWD . "/base")))
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal assignments '((DIR . "/base/sub"))))))

(ert-deftest test-extract-assignment-unresolved-skipped ()
  "Test that unresolved assignments are skipped (not added to context)."
  (let* ((command '(:command-name "VAR=$UNKNOWN" :positional-args nil))
         (context nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (null assignments))))

(ert-deftest test-extract-assignment-split-format ()
  "Test extraction of split assignment (command-name=VAR, args=(value))."
  (let* ((command '(:command-name "DIR" :positional-args ("/tmp")))
         (context nil)
         ;; Mock the semantics lookup to return nil (not a known command)
         (jf/bash-command-semantics-db nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal assignments '((DIR . "/tmp"))))))

(ert-deftest test-extract-assignment-from-positional-args ()
  "Test extraction of VAR=value from positional args in env command."
  ;; env command accepts VAR=value in positional args
  (let* ((command '(:command-name "env" :positional-args ("VAR=/value" "cmd")))
         (context nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    ;; Should find VAR=value in positional args
    (should (member '(VAR . "/value") assignments))))

(ert-deftest test-extract-assignment-multiple-in-args ()
  "Test extraction of multiple assignments from positional args."
  (let* ((command '(:command-name "cmd" :positional-args ("A=/a" "B=/b")))
         (context nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal (length assignments) 2))
    (should (member '(A . "/a") assignments))
    (should (member '(B . "/b") assignments))))

(ert-deftest test-extract-assignment-no-assignments ()
  "Test extraction returns nil when no assignments found."
  (let* ((command '(:command-name "cat" :positional-args ("/file")))
         (context nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (null assignments))))

(ert-deftest test-extract-assignment-skips-flags ()
  "Test that split assignment format rejects flag-like values."
  (let* ((command '(:command-name "VAR" :positional-args ("-flag")))
         (context nil)
         (jf/bash-command-semantics-db nil)
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (null assignments))))

(ert-deftest test-extract-assignment-command-substitution-in-value ()
  "Test assignment extraction with command substitution in value."
  (let* ((command '(:command-name "DIR=$(pwd)/sub" :positional-args nil))
         (context '((PWD . "/base")))
         (assignments (jf/bash--extract-assignments-from-command command context)))
    (should (equal assignments '((DIR . "/base/sub"))))))

(provide 'test-variable-resolution-unit)
;;; test-variable-resolution-unit.el ends here
