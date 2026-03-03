;;; test-glob-matching.el --- Tests for glob pattern matching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Comprehensive ERT tests for glob pattern matching component.
;; Tests the jf/bash-glob-match-p function and supporting functions.
;;
;; These tests validate glob pattern matching WITHOUT filesystem access.
;; All tests use string paths and patterns only.
;;
;; Test naming convention: test-glob-<scenario-slug>
;; Each test includes spec reference in docstring.

;;; Code:

(require 'ert)

;; Note: Once bash-parser.el implements glob matching functions,
;; uncomment the following line:
;; (require 'bash-parser)

;;; Single-Level Wildcard (*) Tests

(ert-deftest test-glob-single-level-wildcard-match ()
  "Scenario: bash-sandbox-security § 'Single-level wildcard match'

Test that * matches any characters within a single path segment."
  (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/*.txt"))
  (should (jf/bash-glob-match-p "/workspace/foo.txt" "/workspace/*.txt"))
  (should (jf/bash-glob-match-p "/workspace/README.txt" "/workspace/*.txt")))

(ert-deftest test-glob-single-star-rejects-multiple-levels ()
  "Scenario: bash-sandbox-security § 'Single star rejects multiple levels'

Test that * does NOT cross directory boundaries (matches within segment only)."
  (should-not (jf/bash-glob-match-p "/workspace/a/b/foo.el" "/workspace/*/foo.el"))
  (should-not (jf/bash-glob-match-p "/workspace/src/foo.txt" "/workspace/*.txt")))

(ert-deftest test-glob-single-star-one-level-match ()
  "Scenario: bash-sandbox-security § 'Single star does not cross directory boundaries'

Test that * matches exactly one directory level."
  (should (jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/*/foo.el"))
  (should (jf/bash-glob-match-p "/workspace/lib/bar.el" "/workspace/*/bar.el")))

;;; Recursive Wildcard (**) Tests

(ert-deftest test-glob-recursive-wildcard-match ()
  "Scenario: bash-sandbox-security § 'Recursive wildcard match'

Test that ** matches zero or more directory levels."
  (should (jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/**/*.el"))
  (should (jf/bash-glob-match-p "/workspace/a/b/c/d/foo.el" "/workspace/**/*.el")))

(ert-deftest test-glob-double-star-zero-segments ()
  "Scenario: bash-sandbox-security § 'Match with double-star consuming zero segments'

Test that ** can match zero directory levels."
  (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/**/file.txt"))
  (should (jf/bash-glob-match-p "/workspace/README.md" "/workspace/**/*.md")))

(ert-deftest test-glob-double-star-multiple-segments ()
  "Scenario: bash-sandbox-security § 'Match with double-star consuming multiple segments'

Test that ** can match multiple directory levels."
  (should (jf/bash-glob-match-p "/workspace/a/b/c/file.txt" "/workspace/**/file.txt"))
  (should (jf/bash-glob-match-p "/workspace/src/util/helpers/foo.el" "/workspace/**/*.el")))

(ert-deftest test-glob-double-star-at-start ()
  "Test that ** works at the beginning of a pattern."
  (should (jf/bash-glob-match-p "/workspace/a/b/file.txt" "**/file.txt"))
  (should (jf/bash-glob-match-p "/a/b/c/d/e/foo.el" "**/*.el")))

(ert-deftest test-glob-double-star-in-middle ()
  "Test that ** works in the middle of a pattern."
  (should (jf/bash-glob-match-p "/workspace/src/foo/bar.el" "/workspace/**/bar.el"))
  (should (jf/bash-glob-match-p "/a/b/c/d/e.txt" "/a/**/e.txt")))

(ert-deftest test-glob-double-star-at-end ()
  "Test that ** works at the end of a pattern."
  (should (jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/**"))
  (should (jf/bash-glob-match-p "/workspace/a/b/c/d/e.txt" "/workspace/**")))

;;; Character Class Tests

(ert-deftest test-glob-character-class-match ()
  "Scenario: bash-sandbox-security § 'Character class match'

Test that character classes [abc] work correctly."
  (should (jf/bash-glob-match-p "/workspace/file1.txt" "/workspace/file[0-9].txt"))
  (should (jf/bash-glob-match-p "/workspace/file5.txt" "/workspace/file[0-9].txt"))
  (should (jf/bash-glob-match-p "/workspace/file9.txt" "/workspace/file[0-9].txt")))

(ert-deftest test-glob-character-class-no-match ()
  "Test that character classes reject non-matching characters."
  (should-not (jf/bash-glob-match-p "/workspace/filea.txt" "/workspace/file[0-9].txt"))
  (should-not (jf/bash-glob-match-p "/workspace/fileX.txt" "/workspace/file[0-9].txt")))

(ert-deftest test-glob-character-class-letters ()
  "Test character classes with letter ranges."
  (should (jf/bash-glob-match-p "/workspace/filea.txt" "/workspace/file[abc].txt"))
  (should (jf/bash-glob-match-p "/workspace/fileb.txt" "/workspace/file[abc].txt"))
  (should (jf/bash-glob-match-p "/workspace/filec.txt" "/workspace/file[abc].txt"))
  (should-not (jf/bash-glob-match-p "/workspace/filed.txt" "/workspace/file[abc].txt")))

;;; Question Mark (?) Tests

(ert-deftest test-glob-question-mark-single-char ()
  "Scenario: bash-sandbox-security § 'Question mark to regex'

Test that ? matches exactly one character."
  (should (jf/bash-glob-match-p "/workspace/file1.txt" "/workspace/file?.txt"))
  (should (jf/bash-glob-match-p "/workspace/fileA.txt" "/workspace/file?.txt"))
  (should (jf/bash-glob-match-p "/workspace/file-.txt" "/workspace/file?.txt")))

(ert-deftest test-glob-question-mark-no-match-zero-chars ()
  "Test that ? does NOT match zero characters."
  (should-not (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/file?.txt")))

(ert-deftest test-glob-question-mark-no-match-two-chars ()
  "Test that ? does NOT match two characters."
  (should-not (jf/bash-glob-match-p "/workspace/fileAB.txt" "/workspace/file?.txt")))

(ert-deftest test-glob-multiple-question-marks ()
  "Test multiple ? wildcards in a pattern."
  (should (jf/bash-glob-match-p "/workspace/file12.txt" "/workspace/file??.txt"))
  (should (jf/bash-glob-match-p "/workspace/fileAB.txt" "/workspace/file??.txt"))
  (should-not (jf/bash-glob-match-p "/workspace/file1.txt" "/workspace/file??.txt")))

;;; Path Boundary Tests

(ert-deftest test-glob-no-match-different-directory ()
  "Scenario: bash-sandbox-security § 'No match for different directory'

Test that patterns don't match paths in different directories."
  (should-not (jf/bash-glob-match-p "/etc/passwd" "/workspace/**"))
  (should-not (jf/bash-glob-match-p "/tmp/file.txt" "/workspace/*.txt"))
  (should-not (jf/bash-glob-match-p "/home/user/doc.md" "/workspace/**/*.md")))

(ert-deftest test-glob-path-must-match-from-start ()
  "Test that patterns must match from the beginning of the path."
  (should-not (jf/bash-glob-match-p "/home/workspace/file.txt" "/workspace/*.txt"))
  (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/*.txt")))

(ert-deftest test-glob-trailing-slash-handling ()
  "Test handling of trailing slashes in paths and patterns."
  (should (jf/bash-glob-match-p "/workspace/dir/" "/workspace/**/"))
  (should (jf/bash-glob-match-p "/workspace/dir" "/workspace/**")))

;;; Literal Character Tests

(ert-deftest test-glob-literal-dots ()
  "Scenario: bash-sandbox-security § 'Escape special regex characters'

Test that literal dots in patterns are matched as dots, not regex wildcards."
  (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/file.txt"))
  (should-not (jf/bash-glob-match-p "/workspace/filextxt" "/workspace/file.txt")))

(ert-deftest test-glob-literal-plus-chars ()
  "Test that literal + characters don't act as regex quantifiers."
  (should (jf/bash-glob-match-p "/workspace/file+name.txt" "/workspace/file+name.txt"))
  (should-not (jf/bash-glob-match-p "/workspace/fileee.txt" "/workspace/file+.txt")))

(ert-deftest test-glob-literal-parens ()
  "Test that literal parentheses are handled correctly."
  (should (jf/bash-glob-match-p "/workspace/file(1).txt" "/workspace/file(1).txt")))

;;; Combined Wildcard Tests

(ert-deftest test-glob-combined-wildcards ()
  "Test patterns combining multiple wildcard types."
  ;; Combine * and **
  (should (jf/bash-glob-match-p "/workspace/src/foo.el" "/workspace/**/*.el"))
  ;; Combine ? and *
  (should (jf/bash-glob-match-p "/workspace/file1.txt" "/workspace/file?.*"))
  ;; Combine [] and *
  (should (jf/bash-glob-match-p "/workspace/file1.txt" "/workspace/file[0-9].*")))

(ert-deftest test-glob-complex-pattern ()
  "Test complex patterns with multiple wildcards."
  (should (jf/bash-glob-match-p "/workspace/src/util/v2/helper.el"
                                "/workspace/**/v[0-9]/*.el"))
  (should-not (jf/bash-glob-match-p "/workspace/src/util/vX/helper.el"
                                    "/workspace/**/v[0-9]/*.el")))

;;; Edge Cases

(ert-deftest test-glob-empty-path ()
  "Test handling of empty path."
  (should-not (jf/bash-glob-match-p "" "/workspace/*")))

(ert-deftest test-glob-empty-pattern ()
  "Test handling of empty pattern."
  (should-not (jf/bash-glob-match-p "/workspace/file.txt" "")))

(ert-deftest test-glob-root-path ()
  "Test matching against root path."
  (should (jf/bash-glob-match-p "/file.txt" "/*.txt"))
  (should (jf/bash-glob-match-p "/a/b/c.txt" "/**/*.txt")))

(ert-deftest test-glob-exact-match ()
  "Test exact path matching (no wildcards)."
  (should (jf/bash-glob-match-p "/workspace/file.txt" "/workspace/file.txt"))
  (should-not (jf/bash-glob-match-p "/workspace/other.txt" "/workspace/file.txt")))

(ert-deftest test-glob-nested-double-stars ()
  "Test patterns with multiple ** wildcards."
  (should (jf/bash-glob-match-p "/a/b/c/d/e/f.txt" "/**/c/**/*.txt"))
  (should (jf/bash-glob-match-p "/workspace/src/a/b/test/foo.el"
                                "/workspace/**/test/**/*.el")))

;;; Helper Functions Tests (when implemented)

;; These tests will validate internal helper functions once they're implemented

(ert-deftest test-glob-to-regex-asterisk ()
  "Scenario: bash-sandbox-security § 'Asterisk to regex'

Test conversion of * to regex pattern."
  (should (string-match-p (jf/bash--glob-to-regex "file*") "file"))
  (should (string-match-p (jf/bash--glob-to-regex "file*") "filename"))
  (should (string-match-p (jf/bash--glob-to-regex "file*") "file.txt")))

(ert-deftest test-glob-to-regex-question ()
  "Scenario: bash-sandbox-security § 'Question mark to regex'

Test conversion of ? to regex pattern."
  (should (string-match-p (jf/bash--glob-to-regex "file?") "file1"))
  (should (string-match-p (jf/bash--glob-to-regex "file?") "fileA"))
  (should-not (string-match-p (jf/bash--glob-to-regex "file?") "file")))

(ert-deftest test-glob-to-regex-character-class ()
  "Scenario: bash-sandbox-security § 'Character class preserved'

Test that character classes are preserved in regex conversion."
  (should (string-match-p (jf/bash--glob-to-regex "file[abc]") "filea"))
  (should (string-match-p (jf/bash--glob-to-regex "file[abc]") "fileb"))
  (should-not (string-match-p (jf/bash--glob-to-regex "file[abc]") "filed")))

(ert-deftest test-glob-to-regex-escapes-special-chars ()
  "Scenario: bash-sandbox-security § 'Escape special regex characters'

Test that regex special characters are properly escaped."
  (should (string-match-p (jf/bash--glob-to-regex "file.txt") "file.txt"))
  (should-not (string-match-p (jf/bash--glob-to-regex "file.txt") "filextxt"))
  (should (string-match-p (jf/bash--glob-to-regex "file+name") "file+name")))

(provide 'test-glob-matching)
;;; test-glob-matching.el ends here
