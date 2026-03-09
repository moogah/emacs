;;; no-op-allowance-spec.el --- No-op allowance short-circuit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; NO-OP ALLOWANCE (Stage 5 Short-Circuit)
;;
;; Tests the no-op allowance validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 5 enforces that commands with zero file operations (no-ops) bypass
;; expensive path validation in stage 6. This optimization allows version
;; checks, help flags, and informational commands to execute without requiring
;; path configuration.
;;
;; Key behaviors tested:
;; - Version checks allowed without path configuration
;; - Help flags allowed as no-ops
;; - Informational commands allowed
;; - Deny list enforced before no-op check (stage 3 ordering)
;; - File path validation bypassed for no-ops
;; - Deny list enforced even for zero-file-operation commands
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 5.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir test-root-dir))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" tools-dir)))

;;; Test Suite

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
       nil  ; no cloud auth
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
         nil
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
           nil
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
       nil
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
       nil
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
       nil
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

(provide 'no-op-allowance-spec)
;;; no-op-allowance-spec.el ends here
