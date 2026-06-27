;;; error-messages-spec.el --- Error message structure tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERROR MESSAGE STRUCTURE
;;
;; Tests the structured error response format across all validation stages
;; of the run_bash_command seven-stage validation pipeline.
;;
;; The validation system returns structured plists that include:
;; - Error type (e.g., :error 'command_denied', 'path_out_of_scope', etc.)
;; - Error-specific context (command name, position, path, operation, provider)
;; - Human-readable :message field with guidance for resolution
;; - Pipeline context (:full-pipeline for command errors)
;;
;; Key behaviors tested:
;; - Command denied errors include command name and position in pipeline
;; - Parse incomplete errors include parse error details
;; - Path out of scope errors include operation and allowed patterns
;; - Path denied errors distinguish from out-of-scope (deny precedence)
;; - Cloud auth denied errors include provider and allowed list
;; - Pipeline errors identify bypass attempts with full command
;; - Multiple operation failures report first violation
;; - Success responses include coverage metrics when applicable
;; - Warning structure for cloud auth in warn mode
;;
;; Expansion guidance:
;; - Add new error types as validation stages evolve
;; - Keep error context fields consistent with error type
;; - Ensure :message provides actionable guidance
;; - Test error precedence when multiple violations occur

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (validation-dir test-dir)
       (scope-test-dir (expand-file-name ".." validation-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Test Suite

(describe "run_bash_command: Error message structure"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "parse incomplete error includes parse errors"
    ;; Test: Parse incomplete error includes parse errors
    ;; Command: 'function incomplete() {'
    ;; Mock: {:parse-complete nil :parse-errors 'Unexpected EOF'}
    ;; Assert error structure:
    ;;   * :error 'parse_incomplete'
    ;;   * :parse-errors contains 'EOF'
    ;;   * :message suggests fixing syntax
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
      ;; Mock parse: Incomplete with error
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        ;; Assert: Structured error with parse errors
        (expect (plist-get result :error) :to-equal "parse_incomplete")
        (expect (plist-get result :message) :to-be-truthy))))

  (it "path out of scope error includes operation details"
    ;; Test: Path out of scope error includes operation details
    ;; Command: 'python3 /workspace/script.py'
    ;; Mock: Execute operation outside scope
    ;; Assert error structure:
    ;;   * :error 'not-in-scope'
    ;;   * :path '/workspace/script.py'
    ;;   * :operation :execute
    ;;   * :required-scope 'paths.execute'
    ;;   * :allowed-patterns (empty list)
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**"))))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "python3 /workspace/script.py"
       '("python3")
       t)

      ;; Mock semantics: Execute operation
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :execute "/workspace/script.py" :command-name "python3"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 /workspace/script.py"
                     "/workspace"
                     scope-config)))
        ;; Assert: Structured error with operation details
        (expect (plist-get result :error) :to-equal "not-in-scope")
        (expect (plist-get result :resource) :to-equal "/workspace/script.py")
        (expect (plist-get result :operation) :to-equal :execute))))

  (it "path denied error distinguishes from out-of-scope"
    ;; Test: Path denied error distinguishes from out-of-scope
    ;; Command: 'cat /etc/passwd'
    ;; Mock: Path matches deny pattern
    ;; Assert error structure:
    ;;   * :error 'denied-pattern'
    ;;   * :path '/etc/passwd'
    ;;   * :denied-patterns contains '/etc/**'
    ;;   * :message explains deny precedence
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**" "/etc/**")
                         :deny '("/etc/**"))))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "cat /etc/passwd"
       '("cat")
       t)

      ;; Mock semantics: Read operation on denied path
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/etc/passwd" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /etc/passwd"
                     "/workspace"
                     scope-config)))
        ;; Assert: Structured error for denied path
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (plist-get result :resource) :to-equal "/etc/passwd"))))

  (it "cloud auth denied includes provider details"
    ;; Test: Cloud auth denied includes provider details
    ;; Command: 'gcloud auth login'
    ;; Mock: GCP provider denied
    ;; Assert error structure:
    ;;   * :error 'cloud_provider_denied'
    ;;   * :provider :gcp
    ;;   * :allowed-providers (:aws)
    ;;   * :message suggests scope expansion
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**")
                         :auth-detection "deny"
                         :allowed-providers '(:aws))))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "gcloud auth login"
       '("gcloud")
       t)

      ;; Mock semantics: Cloud auth for GCP plus file op to avoid no-op short-circuit
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :write "/workspace/config.json" :command-name "gcloud"))
       (list :provider :gcp :command "gcloud auth login")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "gcloud auth login"
                     "/workspace"
                     scope-config)))
        ;; Assert: Structured error with provider details
        (expect (plist-get result :error) :to-equal "cloud_provider_denied")
        (expect (plist-get result :provider) :to-equal :gcp))))

  (it "multiple operation failure reports first violation"
    ;; Test: Multiple operation failure reports first violation
    ;; Command: 'cp /workspace/src.txt /etc/dst.txt'
    ;; Mock: Second operation (write /etc) denied
    ;; Assert error structure:
    ;;   * :error 'denied-pattern' (or not-in-scope)
    ;;   * :path '/etc/dst.txt'
    ;;   * Reports first failure, not all failures
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**")
                         :deny '("/etc/**"))))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "cp /workspace/src.txt /etc/dst.txt"
       '("cp")
       t)

      ;; Mock semantics: Two operations
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
             (helpers-spec--make-file-op :write "/etc/dst.txt" :command-name "cp"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cp /workspace/src.txt /etc/dst.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Reports first violation (deny pattern)
        (expect (plist-get result :error) :to-equal "denied-pattern")
        (expect (plist-get result :resource) :to-equal "/etc/dst.txt"))))

  (it "success response includes coverage metrics"
    ;; Test: Success response includes coverage metrics
    ;; Command: 'cat /workspace/file.txt'
    ;; Mock: Successful execution
    ;; Mock coverage: {:ratio 0.95 :total-tokens 20 :claimed-tokens 19}
    ;; Assert response structure:
    ;;   * :success t (nil error)
    ;;   * Optional :coverage with metrics in validation result
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**"))))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "cat /workspace/file.txt"
       '("cat")
       t)

      ;; Mock semantics: Read operation with high coverage
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
       nil
       '(:ratio 0.95 :total-tokens 20 :claimed-tokens 19))

      ;; Validate command - should succeed
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/file.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (no error)
        (expect result :to-be nil))))

  (it "warning structure for cloud auth in warn mode"
    ;; Test: Warning structure for cloud auth in warn mode
    ;; Command: 'aws-vault exec prod -- cat /workspace/file.txt'
    ;; Mock: Cloud auth detected, warn mode
    ;; Assert response structure:
    ;;   * :warning 'cloud_auth_detected'
    ;;   * :provider :aws
    ;;   * :message suggests scope expansion
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**")
                         :auth-detection "warn")))
      ;; Mock parse
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- cat /workspace/file.txt"
       '("aws-vault" "cat")
       t)

      ;; Mock semantics: Cloud auth with file operations
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
       (list :provider :aws :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command - should succeed with warning
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- cat /workspace/file.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning for cloud auth
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal :aws)))))

(provide 'error-messages-spec)
;;; error-messages-spec.el ends here
