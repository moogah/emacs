;;; path-validation-spec.el --- Tests for path validation, glob matching, and error structure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED TESTS: Path validation, pattern matching, and error formatting
;;
;; Consolidates tests from:
;; - tools/test/unit/validators-spec.el (validate-operation: 11 tests, security-config: 4 tests, error structure: 3 tests)
;; - tools/test/unit/helpers-spec.el (glob-to-regex: 11 tests, glob-match-p: 9 tests, path-matches-any-pattern-p: 5 tests, normalize-keys: 3 tests)
;;
;; Test organization:
;; 1. Operation validation with permission hierarchy (11 tests)
;; 2. Security config validation (4 tests)
;; 3. Error message structure (3 tests)
;; 4. Glob pattern to regex conversion (11 tests)
;; 5. Glob pattern matching (9 tests)
;; 6. Path matches any pattern (5 tests)
;; 7. Key normalization (3 tests)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-dir (expand-file-name "../.." test-dir)))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

;;; Helper Functions

(defun test-path--make-paths-config (&rest args)
  "Create paths configuration plist from keyword arguments."
  (list :read (plist-get args :read)
        :write (plist-get args :write)
        :execute (plist-get args :execute)
        :modify (plist-get args :modify)
        :deny (plist-get args :deny)))

;;; Operation Validation Tests (from validators-spec.el)

(describe "jf/gptel-scope--validate-operation"

  (describe "read operation hierarchy"
    (it "allows read if in read patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "allows read if in write patterns (write includes read)"
      (let* ((paths-config (test-path--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "denies read if not in read or write patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :read '("/tmp/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :read))))

  (describe "write operation"
    (it "allows write if in write patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "denies write if not in write patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/file.txt" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :write))))

  (describe "modify operation hierarchy"
    (it "allows modify if in modify patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :modify '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :modify "/workspace/file.txt" paths-config)))
        (expect result :to-be nil)))

    (it "allows modify if in write patterns (write includes modify)"
      (let* ((paths-config (test-path--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :modify "/workspace/file.txt" paths-config)))
        (expect result :to-be nil))))

  (describe "execute operation"
    (it "requires explicit execute permission"
      (let* ((paths-config (test-path--make-paths-config
                            :execute '("/workspace/scripts/**")))
             (result (jf/gptel-scope--validate-operation
                      :execute "/workspace/scripts/deploy.sh" paths-config)))
        (expect result :to-be nil)))

    (it "denies execute even if in write patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-operation
                      :execute "/workspace/script.sh" paths-config)))
        (expect (plist-get result :error) :to-equal "path_out_of_scope"))))

  (describe "deny precedence"
    (it "denies access even if in allow patterns"
      (let* ((paths-config (test-path--make-paths-config
                            :read '("/workspace/**")
                            :deny '("/workspace/secret/**")))
             (result (jf/gptel-scope--validate-operation
                      :read "/workspace/secret/key.pem" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :to-equal "/workspace/secret/key.pem")))

    (it "deny overrides write permission"
      (let* ((paths-config (test-path--make-paths-config
                            :write '("/workspace/**")
                            :deny '("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-operation
                      :write "/workspace/.git/config" paths-config)))
        (expect (plist-get result :error) :to-equal "path_denied")))))

;;; Security Config Validation Tests (from validators-spec.el)

(describe "jf/gptel-scope--validate-security-config"

  (it "accepts valid boolean enforce-parse-complete"
    (let ((security-config '(:enforce-parse-complete t)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-be t)))

  (it "accepts valid threshold in range"
    (let ((security-config '(:max-coverage-threshold 0.8)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-be t)))

  (it "rejects threshold below 0.0"
    (let ((security-config '(:max-coverage-threshold -0.1)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-throw)))

  (it "rejects threshold above 1.0"
    (let ((security-config '(:max-coverage-threshold 1.5)))
      (expect (jf/gptel-scope--validate-security-config security-config) :to-throw)))

  (it "accepts edge values 0.0 and 1.0"
    (let ((config-zero '(:max-coverage-threshold 0.0))
          (config-one '(:max-coverage-threshold 1.0)))
      (expect (jf/gptel-scope--validate-security-config config-zero) :to-be t)
      (expect (jf/gptel-scope--validate-security-config config-one) :to-be t))))

;;; Error Message Structure Tests (from validators-spec.el)

(describe "validation error structure"

  (it "includes all required fields for path_out_of_scope"
    (let* ((paths-config (test-path--make-paths-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-operation
                    :read "/etc/passwd" paths-config)))
      (expect (plist-get result :error) :to-equal "path_out_of_scope")
      (expect (plist-get result :path) :to-equal "/etc/passwd")
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :required-scope) :not :to-be nil)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "includes all required fields for path_denied"
    (let* ((paths-config (test-path--make-paths-config
                          :read '("/workspace/**")
                          :deny '("/workspace/secret/**")))
           (result (jf/gptel-scope--validate-operation
                    :read "/workspace/secret/key.pem" paths-config)))
      (expect (plist-get result :error) :to-equal "path_denied")
      (expect (plist-get result :path) :not :to-be nil)
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "formats human-readable messages"
    (let* ((paths-config (test-path--make-paths-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-operation
                    :write "/tmp/file.txt" paths-config))
           (message (plist-get result :message)))
      (expect message :to-match "Path not in")
      (expect message :to-match "/tmp/file.txt"))))

;;; Glob Pattern to Regex Conversion (from helpers-spec.el)

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

;;; Glob Pattern Matching (from helpers-spec.el)

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

;;; Path Matches Any Pattern (from helpers-spec.el)

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
    (expect (jf/gptel-scope--path-matches-any-pattern-p
             "~/file.txt"
             (list (expand-file-name "~/**")))
            :to-be t)))

;;; Key Normalization Tests (from helpers-spec.el)

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

(provide 'path-validation-spec)

;;; path-validation-spec.el ends here
