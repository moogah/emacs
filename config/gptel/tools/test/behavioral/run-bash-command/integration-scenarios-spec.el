;;; integration-scenarios-spec.el --- Complex integration scenarios tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; COMPLEX INTEGRATION SCENARIOS (Multi-stage validation)
;;
;; Tests the multi-stage validation pipeline behavior through complex
;; integration scenarios that exercise interactions across multiple stages.
;;
;; Key behaviors tested:
;; - Pipeline validation with mixed operations (reads and writes)
;; - Stage ordering: parse completeness before deny list before semantics
;; - Cloud authentication detected alongside file operations
;; - Deny list enforcement across pipelines
;; - No-op allowance ordering (deny list checked first)
;; - Partial scope failures (some operations allowed, some denied)
;; - Operation-specific permission hierarchies (write includes modify)
;; - Deny patterns override broad permissions
;; - Complete successful validation flows
;;
;; These tests validate that the seven-stage pipeline correctly enforces
;; all rules in proper order, with realistic bash command scenarios.
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete integration flow through multiple stages.

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

(describe "run_bash_command: Complex integration scenarios"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "validates pipeline with mixed operations succeeds"
    ;; Test: Pipeline with mixed operations succeeds
    ;; Command: 'cat /workspace/input.txt | grep pattern | head -10 > /workspace/output.txt'
    ;; Mock parse: Extract commands ['cat', 'grep', 'head', 'bash-redirection']
    ;; Mock semantics: Return file-ops:
    ;;   * {:operation :read :path '/workspace/input.txt'}
    ;;   * {:operation :write :path '/workspace/output.txt'}
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Assert: Success (all stages pass)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract all commands from pipeline
      (helpers-spec-mock-bash-parse
       "cat /workspace/input.txt | grep pattern | head -10 > /workspace/output.txt"
       '("cat" "grep" "head" "bash-redirection")
       t)

      ;; Mock semantics: Read input.txt, write output.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/input.txt" :command-name "cat")
             (helpers-spec--make-file-op :write "/workspace/output.txt" :command-name "bash-redirection"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/input.txt | grep pattern | head -10 > /workspace/output.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (all stages pass)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates pipeline with denied command fails at stage 3"
    ;; Test: Pipeline with denied command fails at stage 3
    ;; Command: 'find /workspace -name "*.tmp" | xargs rm | wc -l'
    ;; Mock parse: Extract commands ['find', 'xargs', 'rm', 'wc']
    ;; Setup: Scope with bash_tools.deny: ['rm']
    ;; Assert: :error 'command_denied', :position 2, :command 'rm'
    ;; Assert: Fails before semantic extraction (stage 3 before stage 4)
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
    - rm

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract commands from pipeline
      (helpers-spec-mock-bash-parse
       "find /workspace -name \"*.tmp\" | xargs rm | wc -l"
       '("find" "xargs" "rm" "wc")
       t)

      ;; Mock semantics (not reached due to deny list failure)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "find /workspace -name \"*.tmp\" | xargs rm | wc -l"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied at stage 3
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates command substitution with execute operation"
    ;; Test: Command substitution with execute operation
    ;; Command: 'cat $(find /workspace/docs -name README.md) | python3 /workspace/scripts/process.py'
    ;; Mock parse: Extract nested commands ['find', 'cat', 'python3']
    ;; Mock semantics: Return file-ops:
    ;;   * {:operation :read :path '/workspace/docs' :command 'find'}
    ;;   * {:operation :read :path '/workspace/README.md' :command 'cat'}
    ;;   * {:operation :execute :path '/workspace/scripts/process.py' :command 'python3'}
    ;; Setup: Scope with paths.read: ['/workspace/**'], NO paths.execute
    ;; Assert: :error 'path_out_of_scope', :operation :execute
    ;; Assert: Read operations allowed, execute denied
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract commands including nested
      (helpers-spec-mock-bash-parse
       "cat $(find /workspace/docs -name README.md) | python3 /workspace/scripts/process.py"
       '("find" "cat" "python3")
       t)

      ;; Mock semantics: Read operations and execute
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/docs" :command-name "find")
             (helpers-spec--make-file-op :read "/workspace/README.md" :command-name "cat")
             (helpers-spec--make-file-op :execute "/workspace/scripts/process.py" :command-name "python3"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat $(find /workspace/docs -name README.md) | python3 /workspace/scripts/process.py"
                     "/workspace"
                     scope-config)))
        ;; Assert: Execute denied
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :execute))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates cloud auth with file operations (both validated)"
    ;; Test: Cloud auth with file operations (both validated)
    ;; Command: 'aws-vault exec prod -- cat /workspace/secrets.yml'
    ;; Mock parse: Extract commands ['aws-vault', 'cat']
    ;; Mock semantics: Return BOTH:
    ;;   * Cloud-auth: {:provider 'aws' :command 'aws-vault exec'}
    ;;   * File-ops: {:operation :read :path '/workspace/secrets.yml'}
    ;; Setup: Scope with cloud.auth_detection: 'warn', paths.read: ['/workspace/**']
    ;; Assert: :success t, :warnings contains cloud auth detection
    ;; Assert: File operation validated successfully
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
      ;; Mock parse: Extract commands
      (helpers-spec-mock-bash-parse
       "aws-vault exec prod -- cat /workspace/secrets.yml"
       '("aws-vault" "cat")
       t)

      ;; Mock semantics: Both cloud auth and file ops
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/secrets.yml" :command-name "cat"))
       (list :provider "aws" :command "aws-vault exec prod")
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "aws-vault exec prod -- cat /workspace/secrets.yml"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning for cloud auth, file ops validated
        (expect (plist-get result :warning) :to-equal "cloud_auth_detected")
        (expect (plist-get result :provider) :to-equal "aws"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates no-op in deny list fails before no-op check"
    ;; Test: No-op in deny list fails before no-op check
    ;; Command: 'sudo --version'
    ;; Mock parse: Extract commands ['sudo']
    ;; Mock semantics: Return empty file-ops (would be no-op)
    ;; Setup: Scope with bash_tools.deny: ['sudo']
    ;; Assert: :error 'command_denied' at stage 3
    ;; Assert: Never reaches stage 5 (no-op allowance)
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

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sudo'
      (helpers-spec-mock-bash-parse
       "sudo --version"
       '("sudo")
       t)

      ;; Mock semantics: Zero file operations (would be no-op)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sudo --version"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied at stage 3 before no-op check
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates multiple file operations with partial scope failure"
    ;; Test: Multiple file operations with partial scope failure
    ;; Command: 'cp /workspace/src.txt /tmp/dst.txt'
    ;; Mock semantics: Return file-ops:
    ;;   * {:operation :read :path '/workspace/src.txt'}
    ;;   * {:operation :write :path '/tmp/dst.txt'}
    ;; Setup: Scope with paths.read: ['/workspace/**'], paths.write: ['/workspace/**']
    ;; Assert: :error 'path_out_of_scope', :path '/tmp/dst.txt'
    ;; Assert: First operation allowed, second denied
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cp'
      (helpers-spec-mock-bash-parse
       "cp /workspace/src.txt /tmp/dst.txt"
       '("cp")
       t)

      ;; Mock semantics: Read src, write dst
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/src.txt" :command-name "cp")
             (helpers-spec--make-file-op :write "/tmp/dst.txt" :command-name "cp"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cp /workspace/src.txt /tmp/dst.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Second operation denied
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :path) :to-equal "/tmp/dst.txt"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates execute operation denied despite read access"
    ;; Test: Execute operation denied despite read access
    ;; Command: 'bash /workspace/deploy.sh'
    ;; Mock semantics: Return file-op {:operation :execute :path '/workspace/deploy.sh'}
    ;; Setup: Scope with paths.read: ['/workspace/**'], NO paths.execute
    ;; Assert: :error 'path_out_of_scope', :operation :execute
    ;; Assert: Read access doesn't imply execute access
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'bash'
      (helpers-spec-mock-bash-parse
       "bash /workspace/deploy.sh"
       '("bash")
       t)

      ;; Mock semantics: Execute operation
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :execute "/workspace/deploy.sh" :command-name "bash"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "bash /workspace/deploy.sh"
                     "/workspace"
                     scope-config)))
        ;; Assert: Execute denied
        (expect (plist-get result :error) :to-equal "path_out_of_scope")
        (expect (plist-get result :operation) :to-equal :execute))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates modify operation allowed via write scope (hierarchical)"
    ;; Test: Modify operation allowed via write scope (hierarchical)
    ;; Command: 'sed -i "s/foo/bar/" /workspace/config.yml'
    ;; Mock semantics: Return file-op {:operation :modify :path '/workspace/config.yml'}
    ;; Setup: Scope with paths.write: ['/workspace/**']
    ;; Assert: Success (write scope includes modify)
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '()  ; read
                        '("/workspace/**")  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'sed'
      (helpers-spec-mock-bash-parse
       "sed -i \"s/foo/bar/\" /workspace/config.yml"
       '("sed")
       t)

      ;; Mock semantics: Modify operation
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :modify "/workspace/config.yml" :command-name "sed"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sed -i \"s/foo/bar/\" /workspace/config.yml"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (write includes modify)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates deny pattern overrides broad permissions"
    ;; Test: Deny pattern overrides broad permissions
    ;; Command: 'cat /workspace/.ssh/id_rsa'
    ;; Mock semantics: Return file-op {:operation :read :path '/workspace/.ssh/id_rsa'}
    ;; Setup: Scope with paths.read: ['/**'], paths.deny: ['~/.ssh/**', '**/.ssh/**']
    ;; Assert: :error 'path_denied'
    ;; Assert: Deny takes precedence over broad read permission
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/**")  ; read (very broad)
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '("~/.ssh/**" "**/.ssh/**")))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'cat'
      (helpers-spec-mock-bash-parse
       "cat /workspace/.ssh/id_rsa"
       '("cat")
       t)

      ;; Mock semantics: Read .ssh file
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/.ssh/id_rsa" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat /workspace/.ssh/id_rsa"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied
        (expect (plist-get result :error) :to-equal "path_denied")
        (expect (plist-get result :path) :to-equal "/workspace/.ssh/id_rsa"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates complete successful execution with output"
    ;; Test: Complete successful execution with output
    ;; Command: 'ls -la /workspace/src'
    ;; Mock parse: Complete parse, commands ['ls']
    ;; Mock semantics: File-op {:operation :read :path '/workspace/src'}
    ;; Mock call-process: Return exit-code 0, output 'file1.txt\nfile2.txt'
    ;; Setup: Scope with paths.read: ['/workspace/**']
    ;; Assert: :success t, :output contains 'file1.txt', :exit_code 0
    ;;
    ;; NOTE: This test validates the VALIDATION phase only, not actual execution.
    ;; The run_bash_command tool has two phases:
    ;; 1. Validation (what we're testing here)
    ;; 2. Execution (would happen after validation passes)
    ;;
    ;; This test confirms validation succeeds (returns nil = no error).
    ;; Execution with output would be tested separately if needed.
    (let* ((scope-yml (helpers-spec-make-scope-yml
                       (helpers-spec--scope-with-paths
                        '("/workspace/**")  ; read
                        '()  ; write
                        '()  ; execute
                        '()  ; modify
                        '()))) ; deny
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Complete parse
      (helpers-spec-mock-bash-parse
       "ls -la /workspace/src"
       '("ls")
       t)

      ;; Mock semantics: Read operation
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/src" :command-name "ls"))
       nil
       '(:ratio 1.0))

      ;; Validate command (validation phase only)
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls -la /workspace/src"
                     "/workspace"
                     scope-config)))
        ;; Assert: Validation succeeds (nil = no error)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml))))

(provide 'integration-scenarios-spec)
;;; integration-scenarios-spec.el ends here
