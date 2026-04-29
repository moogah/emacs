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
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

;;; Helper Functions

(defun test-path--make-config (&rest args)
  "Create scope config plist with a :paths section from keyword arguments."
  (list :paths
        (list :read (plist-get args :read)
              :write (plist-get args :write)
              :execute (plist-get args :execute)
              :modify (plist-get args :modify)
              :deny (plist-get args :deny))))

;;; Operation Validation Tests (from validators-spec.el)

(describe "jf/gptel-scope--validate-path-operation"

  (describe "read operation hierarchy"
    (it "allows read if in read patterns"
      (let* ((config (test-path--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "allows read if in write patterns (write includes read)"
      (let* ((config (test-path--make-config
                      :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "denies read if not in read or write patterns"
      (let* ((config (test-path--make-config
                      :read '("/tmp/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :operation) :to-equal :read))))

  (describe "write operation"
    (it "allows write if in write patterns"
      (let* ((config (test-path--make-config
                      :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :write config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "denies write if not in write patterns"
      (let* ((config (test-path--make-config
                      :read '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :write config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :operation) :to-equal :write))))

  (describe "modify operation hierarchy"
    (it "allows modify if in modify patterns"
      (let* ((config (test-path--make-config
                      :modify '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :modify config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "allows modify if in write patterns (write includes modify)"
      (let* ((config (test-path--make-config
                      :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/file.txt" :modify config)))
        (expect (plist-get result :allowed) :to-be t))))

  (describe "execute operation"
    (it "requires explicit execute permission"
      (let* ((config (test-path--make-config
                      :execute '("/workspace/scripts/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/scripts/deploy.sh" :execute config)))
        (expect (plist-get result :allowed) :to-be t)))

    (it "denies execute even if in write patterns"
      (let* ((config (test-path--make-config
                      :write '("/workspace/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/script.sh" :execute config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "not-in-scope"))))

  (describe "deny precedence"
    (it "denies access even if in allow patterns"
      (let* ((config (test-path--make-config
                      :read '("/workspace/**")
                      :deny '("/workspace/secret/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/secret/key.pem" :read config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (plist-get result :resource) :to-equal "/workspace/secret/key.pem")))

    (it "deny overrides write permission"
      (let* ((config (test-path--make-config
                      :write '("/workspace/**")
                      :deny '("/workspace/.git/**")))
             (result (jf/gptel-scope--validate-path-operation
                      "/workspace/.git/config" :write config)))
        (expect (plist-get result :allowed) :to-be nil)
        (expect (plist-get result :error) :to-equal "denied-pattern")))))

;;; Security Config Validation Tests
;;
;; The historical `jf/gptel-scope-yaml--validate-security-config' has
;; been deleted (cycle-3 delete-yaml-and-security-residue). The
;; security keys it validated (`enforce_parse_complete',
;; `max_coverage_threshold') are now module-level defconsts; the
;; per-session override is gone. See
;; `register/invariant/scope-parse-complete-is-true' and
;; `register/invariant/scope-coverage-threshold-is-1'.

(describe "scope security defconsts"

  (it "enforce-parse-complete defconst is fixed at t"
    ;; register/invariant/scope-parse-complete-is-true (confirmed cycle-2):
    ;; the validator reads this defconst directly; no per-session override.
    (expect jf/gptel-scope--enforce-parse-complete :to-be t))

  (it "coverage-threshold defconst is fixed at 1.0"
    ;; register/invariant/scope-coverage-threshold-is-1 (confirmed cycle-2):
    ;; the validator reads this defconst directly; no per-session override.
    (expect jf/gptel-scope--coverage-threshold :to-equal 1.0)))

;;; Error Message Structure Tests (from validators-spec.el)

(describe "validation error structure"

  (it "includes all required fields for not-in-scope"
    (let* ((config (test-path--make-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-path-operation
                    "/etc/passwd" :read config)))
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "not-in-scope")
      (expect (plist-get result :resource) :to-equal "/etc/passwd")
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :required-scope) :not :to-be nil)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "includes all required fields for denied-pattern"
    (let* ((config (test-path--make-config
                    :read '("/workspace/**")
                    :deny '("/workspace/secret/**")))
           (result (jf/gptel-scope--validate-path-operation
                    "/workspace/secret/key.pem" :read config)))
      (expect (plist-get result :allowed) :to-be nil)
      (expect (plist-get result :error) :to-equal "denied-pattern")
      (expect (plist-get result :resource) :not :to-be nil)
      (expect (plist-get result :operation) :to-equal :read)
      (expect (plist-get result :message) :not :to-be nil)))

  (it "formats human-readable messages"
    (let* ((config (test-path--make-config :read '("/workspace/**")))
           (result (jf/gptel-scope--validate-path-operation
                    "/tmp/file.txt" :write config))
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

;;; Loader output shape (cycle-2 reconciliation)
;;
;; Buttercup specs for `register/invariant/scope-no-security-key-in-plist'
;; and `register/shape/scope-config-plist'. These exercise the loader
;; output (L1 of the deletion invariant) directly, so the loader
;; cannot accidentally reintroduce a `:security' key without breaking
;; tests. The L2 layer (no `:security' reads in scope-validation.el)
;; is exercised by a structural-audit spec elsewhere.

(describe "invariant: scope-no-security-key-in-plist"

  (it "loader output from a complete drawer omits :security at top level"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/workspace"))
          (:GPTEL_SCOPE_WRITE . ("/workspace/**"))
          (:GPTEL_SCOPE_MODIFY . ("/workspace/config/**"))
          (:GPTEL_SCOPE_EXECUTE . ("/workspace/scripts/**"))
          (:GPTEL_SCOPE_DENY . ("/etc/**"))
          (:GPTEL_SCOPE_CLOUD_AUTH . "warn")
          (:GPTEL_SCOPE_CLOUD_PROVIDERS . ("aws" "gcp")))
      (let ((result (jf/gptel-scope--load-from-buffer (current-buffer))))
        (expect (plist-member result :security) :to-be nil))))

  (it "loader output from a minimal drawer omits :security at top level"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/workspace")))
      (let ((result (jf/gptel-scope--load-from-buffer (current-buffer))))
        (expect (plist-member result :security) :to-be nil))))

  (it "the loader's plist has exactly the top-level keys :paths and :cloud"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/workspace")))
      (let* ((result (jf/gptel-scope--load-from-buffer (current-buffer)))
             (top-keys (cl-loop for k in result by #'cddr collect k)))
        (expect (sort top-keys (lambda (a b) (string< (symbol-name a)
                                                     (symbol-name b))))
                :to-equal '(:cloud :paths))))))

(describe "loader empty-drawer behaviour (cycle-3 disposition)"
  ;; register/boundary/scope-config-loader, Option B: an empty drawer
  ;; (no :GPTEL_SCOPE_* keys) yields the deny-all defaults plist; the
  ;; dispatcher's authorisation outcome is per-violation deny, not
  ;; `no_scope_config'.

  (it "an empty-drawer plist passes --has-any-scope-key-p as nil"
    (jf/gptel-test--with-scope-drawer '()
      (let ((result (jf/gptel-scope--load-from-buffer (current-buffer))))
        (expect (jf/gptel-scope--has-any-scope-key-p result) :to-be nil))))

  (it "deny-all defaults shape matches scope-config-plist contract"
    (let* ((defaults (jf/gptel-scope--deny-all-defaults))
           (paths (plist-get defaults :paths))
           (cloud (plist-get defaults :cloud)))
      (expect (plist-member defaults :security) :to-be nil)
      (expect (plist-get paths :read) :to-be nil)
      (expect (plist-get paths :write) :to-be nil)
      (expect (plist-get paths :modify) :to-be nil)
      (expect (plist-get paths :execute) :to-be nil)
      (expect (plist-get paths :deny) :to-be nil)
      (expect (plist-get cloud :auth-detection) :to-equal "deny")
      (expect (plist-get cloud :allowed-providers) :to-be nil)))

  (it "deny-all defaults deny every per-path operation"
    ;; Per-path validation against the deny-all defaults always returns
    ;; an `:allowed nil :error "not-in-scope"' outcome -- not the
    ;; legacy `no_scope_config' short-circuit.
    (let* ((config (jf/gptel-scope--deny-all-defaults))
           (read-result (jf/gptel-scope--validate-path-operation
                         "/workspace/foo" :read config))
           (write-result (jf/gptel-scope--validate-path-operation
                          "/workspace/foo" :write config)))
      (expect (plist-get read-result :allowed) :to-be nil)
      (expect (plist-get read-result :error) :to-equal "not-in-scope")
      (expect (plist-get write-result :allowed) :to-be nil)
      (expect (plist-get write-result :error) :to-equal "not-in-scope"))))

(provide 'path-validation-spec)

;;; path-validation-spec.el ends here
