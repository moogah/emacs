;;; helpers-spec.el --- Unit tests for scope validation helper functions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS: Pattern matching, normalization, and path resolution helpers
;;
;; Tests 30 isolated unit-testable functions:
;; - Pattern matching (glob-to-regex, glob-match-p, path-matches-any-pattern-p)
;; - Key normalization (normalize-keys)
;; - Path resolution helpers
;;
;; Test organization:
;; 1. Glob pattern to regex conversion (12 tests)
;; 2. Glob pattern matching (10 tests)
;; 3. Path matches any pattern (5 tests)
;; 4. Key normalization (3 tests)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name "../.." test-dir))
       (tools-dir test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Pattern Matching Tests

(describe "jf/gptel-scope--glob-to-regex"

  (describe "single star wildcard"
    (it "converts * to [^/]*"
      (let ((regex (jf/gptel-scope--glob-to-regex "file*.txt")))
        (expect regex :to-equal "^file[^/]*\\.txt$")))

    (it "handles multiple single stars"
      (let ((regex (jf/gptel-scope--glob-to-regex "*/test/*.txt")))
        (expect regex :to-match "[^/].*[^/]"))))

  (describe "recursive wildcard /**/"
    (it "converts /**/ to /(?:.*/)?"
      (let ((regex (jf/gptel-scope--glob-to-regex "/workspace/**/")))
        (expect regex :to-equal "^/workspace/\\(?:.*/\\)?$")))

    (it "handles middle recursive wildcard"
      (let ((regex (jf/gptel-scope--glob-to-regex "/workspace/**/file.txt")))
        ;; Should convert to /workspace/(?:.*/)? pattern
        (expect regex :to-equal "^/workspace/\\(?:.*/\\)?file\\.txt$"))))

  (describe "recursive wildcard at end"
    (it "converts /** at end to /.*"
      (let ((regex (jf/gptel-scope--glob-to-regex "/workspace/**")))
        (expect regex :to-equal "^/workspace/.*$")))

    (it "matches everything after slash"
      (let ((regex (jf/gptel-scope--glob-to-regex "/tmp/**")))
        (expect (string-match-p regex "/tmp/foo/bar/baz") :not :to-be nil))))

  (describe "literal characters"
    (it "escapes special regex characters"
      (let ((regex (jf/gptel-scope--glob-to-regex "/path/to/file.txt")))
        (expect regex :to-equal "^/path/to/file\\.txt$")))

    (it "handles paths with dashes and underscores"
      (let ((regex (jf/gptel-scope--glob-to-regex "/my-path/my_file.txt")))
        (expect regex :to-equal "^/my-path/my_file\\.txt$"))))

  (describe "edge cases"
    (it "handles empty pattern"
      (let ((regex (jf/gptel-scope--glob-to-regex "")))
        (expect regex :to-equal "^$")))

    (it "handles pattern with only **"
      (let ((regex (jf/gptel-scope--glob-to-regex "**")))
        (expect regex :to-equal "^.*$")))

    (it "anchors pattern with ^ and $"
      (let ((regex (jf/gptel-scope--glob-to-regex "/path")))
        (expect regex :to-match "^\\^")
        (expect regex :to-match "\\$$")))))

(describe "jf/gptel-scope--glob-match-p"

  (describe "exact matches"
    (it "matches exact path"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/file.txt"
               "/workspace/file.txt")
              :to-be t))

    (it "rejects different path"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/file.txt"
               "/workspace/other.txt")
              :to-be nil)))

  (describe "single star wildcard"
    (it "matches single directory component"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/sub/file.txt"
               "/workspace/*/file.txt")
              :to-be t))

    (it "does not match across slashes"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/a/b/file.txt"
               "/workspace/*/file.txt")
              :to-be nil)))

  (describe "recursive wildcard"
    (it "matches zero directories with /**/"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/file.txt"
               "/workspace/**/file.txt")
              :to-be t))

    (it "matches one directory with /**/"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/sub/file.txt"
               "/workspace/**/file.txt")
              :to-be t))

    (it "matches multiple directories with /**/"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/a/b/c/file.txt"
               "/workspace/**/file.txt")
              :to-be t))

    (it "matches everything with /** at end"
      (expect (jf/gptel-scope--glob-match-p
               "/workspace/any/path/here/file.txt"
               "/workspace/**")
              :to-be t)))

  (describe "case sensitivity"
    (it "is case-sensitive by default"
      (expect (jf/gptel-scope--glob-match-p
               "/Workspace/file.txt"
               "/workspace/**")
              :to-be nil))))

(describe "jf/gptel-scope--path-matches-any-pattern-p"

  (it "returns t if path matches first pattern"
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "/workspace/file.txt"
             '("/workspace/**" "/tmp/**"))
            :to-be t))

  (it "returns t if path matches second pattern"
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "/tmp/file.txt"
             '("/workspace/**" "/tmp/**"))
            :to-be t))

  (it "returns nil if no patterns match"
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "/etc/passwd"
             '("/workspace/**" "/tmp/**"))
            :to-be nil))

  (it "handles empty pattern list"
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "/workspace/file.txt"
             '())
            :to-be nil))

  (it "normalizes path before matching"
    ;; Paths are expanded/normalized internally
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "~/file.txt"  ; Will be expanded
             (list (expand-file-name "~/**")))
            :to-be t)))

;;; Key Normalization Tests

(describe "jf/gptel-scope--normalize-keys"

  (it "converts snake_case to kebab-case"
    (let ((plist '(:auth_detection "warn" :max_coverage_threshold 0.8)))
      (let ((result (jf/gptel-scope--normalize-keys plist)))
        (expect (plist-get result :auth-detection) :to-equal "warn")
        (expect (plist-get result :max-coverage-threshold) :to-equal 0.8))))

  (it "recursively normalizes nested plists"
    (let ((plist '(:cloud (:auth_detection "warn")
                   :security (:enforce_parse_complete t))))
      (let* ((result (jf/gptel-scope--normalize-keys plist))
             (cloud (plist-get result :cloud))
             (security (plist-get result :security)))
        (expect (plist-get cloud :auth-detection) :to-equal "warn")
        (expect (plist-get security :enforce-parse-complete) :to-be t))))

  (it "preserves non-plist values"
    (let ((plist '(:commands ("ls" "cat") :threshold 0.8)))
      (let ((result (jf/gptel-scope--normalize-keys plist)))
        (expect (plist-get result :commands) :to-equal '("ls" "cat"))
        (expect (plist-get result :threshold) :to-equal 0.8)))))

(provide 'helpers-spec)

;;; helpers-spec.el ends here
