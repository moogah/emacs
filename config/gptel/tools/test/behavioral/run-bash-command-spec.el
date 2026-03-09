;;; run-bash-command-spec.el --- Behavioral tests for run_bash_command tool -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; BEHAVIORAL TESTS: End-to-end validation for run_bash_command tool
;;
;; Tests the seven-stage validation pipeline:
;; 1. Session directory detection
;; 2. Parse completeness enforcement (stage 2)
;; 3. Deny list bypass prevention (stage 3)
;; 4. Working directory and path resolution
;; 5. No-op allowance (stage 5 short-circuit)
;; 6. Operation-specific path validation (stage 6)
;; 7. Cloud authentication policy enforcement (stage 7)
;;
;; Additional scenarios:
;; - Complex integration scenarios (multi-stage validation)
;; - Error message structure and expansion guidance
;; - Timeout and resource limits
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through the pipeline.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-root-dir (expand-file-name ".." test-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Test Suite Structure

(describe "run_bash_command: Session directory detection"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "detects session directory for scope.yml loading"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Parse completeness enforcement (stage 2)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "denies incomplete parse when enforce_parse_complete is true"
    :pending "TODO: Implement test")

  (it "allows incomplete parse when enforce_parse_complete is false"
    :pending "TODO: Implement test")

  (it "provides expansion guidance on parse failure"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Deny list bypass prevention (stage 3)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "denies commands in deny list"
    :pending "TODO: Implement test")

  (it "allows commands not in deny list"
    :pending "TODO: Implement test")

  (it "denies commands in deny list even with no file operations"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Working directory and path resolution"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "resolves relative paths against working directory"
    :pending "TODO: Implement test")

  (it "handles absolute paths correctly"
    :pending "TODO: Implement test")

  (it "normalizes paths before validation"
    :pending "TODO: Implement test"))

(describe "run_bash_command: No-op allowance (stage 5 short-circuit)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows version checks without path configuration"
    ;; Setup: Scope with minimal paths (only /workspace)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse result: command 'python3 --version'
      (helpers-spec-mock-bash-parse
       "python3 --version"
       '("python3")
       t)  ; parse-complete = true

      ;; Mock semantics: Zero file operations
      (helpers-spec-mock-bash-semantics
       '()  ; file-ops = empty list
       '(:detected nil)  ; no cloud auth
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "python3 --version"
                     "/workspace"
                     scope-config)))
        ;; Assert: No validation error (nil = success)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows help flags as no-ops"
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Test multiple commands with --help flags
      (dolist (cmd '("ls --help" "grep --help" "gcc --help"))
        ;; Mock parse: Extract command name (first word)
        (helpers-spec-mock-bash-parse
         cmd
         (list (car (split-string cmd)))
         t)

        ;; Mock semantics: Zero file operations
        (helpers-spec-mock-bash-semantics
         '()  ; no file ops
         '(:detected nil)
         '(:ratio 1.0))

        ;; Validate
        (let ((result (jf/gptel-scope--validate-command-semantics
                       cmd
                       "/workspace"
                       scope-config)))
          (expect result :to-be nil)))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows informational commands"
    (let* ((scope-yml (helpers-spec-make-minimal-scope))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Test informational commands
      (dolist (cmd-info '(("which bash" "which")
                          ("type ls" "type")
                          ("command -v python" "command")))
        (let ((cmd (car cmd-info))
              (cmd-name (cadr cmd-info)))
          ;; Mock parse
          (helpers-spec-mock-bash-parse cmd (list cmd-name) t)

          ;; Mock semantics: Zero file operations
          (helpers-spec-mock-bash-semantics
           '()
           '(:detected nil)
           '(:ratio 1.0))

          ;; Validate
          (let ((result (jf/gptel-scope--validate-command-semantics
                         cmd
                         "/workspace"
                         scope-config)))
            (expect result :to-be nil))))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "denies no-op commands in deny list before no-op check"
    ;; Setup: Scope with deny list including sudo and chmod
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny:
    - sudo
    - chmod

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sudo' command
      (helpers-spec-mock-bash-parse
       "sudo --version"
       '("sudo")
       t)

      ;; Mock semantics: Zero file operations
      (helpers-spec-mock-bash-semantics
       '()  ; no file ops - would be no-op
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sudo --version"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied at stage 3 (deny list) BEFORE no-op check
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo")
        (expect (plist-get result :position) :to-equal 0))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "bypasses file path validation for no-op commands"
    ;; Command not in any path configuration, but no file operations
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml))
           (validation-called nil))
      ;; Mock parse: 'ruby --version'
      (helpers-spec-mock-bash-parse
       "ruby --version"
       '("ruby")
       t)

      ;; Mock semantics: Zero file operations
      (helpers-spec-mock-bash-semantics
       '()  ; no file ops
       '(:detected nil)
       '(:ratio 1.0))

      ;; Spy on file validation function to ensure it's NOT called
      (spy-on 'jf/gptel-scope--validate-file-operations
              :and-call-fake (lambda (&rest _)
                               (setq validation-called t)
                               nil))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ruby --version"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success without triggering file validation
        (expect result :to-be nil)
        ;; Verify file validation was NOT called
        (expect validation-called :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "enforces deny list even for zero-file-operation commands"
    ;; Additional test to ensure deny list check happens at stage 3
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       "paths:
  read:
    - \"/workspace/**\"
  write:
    - \"/workspace/**\"
  execute: []
  modify: []
  deny: []

bash_tools:
  deny:
    - systemctl

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: 'systemctl --version' (zero file ops)
      (helpers-spec-mock-bash-parse
       "systemctl --version"
       '("systemctl")
       t)

      ;; Mock semantics: Zero file operations
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "systemctl --version"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied despite being no-op
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "systemctl"))

      ;; Cleanup
      (delete-file scope-yml))))

(describe "run_bash_command: Operation-specific path validation (stage 6)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows read operation within read scope"
    :pending "TODO: Implement test")

  (it "denies read operation outside read scope"
    :pending "TODO: Implement test")

  (it "allows write operation within write scope"
    :pending "TODO: Implement test")

  (it "denies write operation outside write scope"
    :pending "TODO: Implement test")

  (it "allows execute operation within execute scope"
    :pending "TODO: Implement test")

  (it "denies execute operation outside execute scope"
    :pending "TODO: Implement test")

  (it "allows modify operation within modify scope"
    :pending "TODO: Implement test")

  (it "denies modify operation outside modify scope"
    :pending "TODO: Implement test")

  (it "denies all operations on deny list paths"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Cloud authentication policy enforcement (stage 7)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "allows cloud auth when auth_detection is allow"
    :pending "TODO: Implement test")

  (it "warns on cloud auth when auth_detection is warn"
    :pending "TODO: Implement test")

  (it "denies cloud auth when auth_detection is deny"
    :pending "TODO: Implement test")

  (it "checks allowed_providers when specified"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Complex integration scenarios"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "validates multi-stage pipeline with multiple file operations"
    :pending "TODO: Implement test")

  (it "validates pipeline with mixed read/write operations"
    :pending "TODO: Implement test")

  (it "validates pipeline with cloud auth and file operations"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Error message structure"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "provides structured error with operation and path details"
    :pending "TODO: Implement test")

  (it "includes expansion guidance in error messages"
    :pending "TODO: Implement test"))

(describe "run_bash_command: Timeout and resource limits"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "enforces command timeout"
    :pending "TODO: Implement test")

  (it "truncates output at max chars limit"
    :pending "TODO: Implement test"))

(provide 'run-bash-command-spec)

;;; run-bash-command-spec.el ends here
