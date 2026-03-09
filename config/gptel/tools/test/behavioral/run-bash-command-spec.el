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

  (it "blocks simple denied command"
    ;; Test: Simple denied command blocked
    ;; Setup: Scope with bash_tools.deny: ['rm', 'sudo', 'chmod', 'dd']
    ;; Command: 'rm file.txt'
    ;; Mock parse: Extract commands ['rm']
    ;; Assert: :error 'command_denied', :command 'rm', :position 0
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'rm' command
      (helpers-spec-mock-bash-parse
       "rm file.txt"
       '("rm")
       t)  ; parse-complete = true

      ;; Mock semantics: File operations (not relevant for deny list test)
      (helpers-spec-mock-bash-semantics
       '()  ; file-ops (deny list check happens before semantic extraction)
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "rm file.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: Denied at stage 3 (deny list)
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 0))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "blocks denied command in pipeline position 2"
    ;; Test: Denied command in pipeline position 2 blocked
    ;; Command: 'ls | xargs rm'
    ;; Mock parse: Extract commands ['ls', 'xargs', 'rm']
    ;; Assert: :error 'command_denied', :command 'rm', :position 2
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract all three commands in pipeline
      (helpers-spec-mock-bash-parse
       "ls | xargs rm"
       '("ls" "xargs" "rm")
       t)

      ;; Mock semantics (not reached due to deny list failure)
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls | xargs rm"
                     "/workspace"
                     scope-config)))
        ;; Assert: rm blocked at position 2
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2)
        ;; Assert: Error message identifies rm in pipeline
        (expect (plist-get result :message) :to-match "position 2"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "blocks denied command in complex pipeline"
    ;; Test: Complex pipeline with denied command
    ;; Command: 'find . -name "*.tmp" | xargs rm'
    ;; Mock parse: Extract commands ['find', 'xargs', 'rm']
    ;; Assert: Denied at position 2 (rm)
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract all commands
      (helpers-spec-mock-bash-parse
       "find . -name \"*.tmp\" | xargs rm"
       '("find" "xargs" "rm")
       t)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "find . -name \"*.tmp\" | xargs rm"
                     "/workspace"
                     scope-config)))
        ;; Assert: rm blocked at position 2
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "blocks sudo in pipeline"
    ;; Test: Sudo in pipeline blocked
    ;; Command: 'cat data.txt | sudo tee /etc/config'
    ;; Mock parse: Extract commands ['cat', 'sudo', 'tee']
    ;; Assert: :error 'command_denied', :command 'sudo', :position 1
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract all commands
      (helpers-spec-mock-bash-parse
       "cat data.txt | sudo tee /etc/config"
       '("cat" "sudo" "tee")
       t)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat data.txt | sudo tee /etc/config"
                     "/workspace"
                     scope-config)))
        ;; Assert: sudo blocked at position 1
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo")
        (expect (plist-get result :position) :to-equal 1))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates chain operators (&&)"
    ;; Test: Chain operators validated (&&)
    ;; Command: 'ls && rm file.txt'
    ;; Mock parse: Extract commands ['ls', 'rm']
    ;; Assert: rm blocked in chain
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract commands from chain
      (helpers-spec-mock-bash-parse
       "ls && rm file.txt"
       '("ls" "rm")
       t)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls && rm file.txt"
                     "/workspace"
                     scope-config)))
        ;; Assert: rm blocked at position 1
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates chain operators (||)"
    ;; Test: Chain operators validated (||)
    ;; Command: 'cat file || sudo cat /etc/passwd'
    ;; Mock parse: Extract commands ['cat', 'sudo', 'cat']
    ;; Assert: sudo blocked at position 1
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract commands from chain
      (helpers-spec-mock-bash-parse
       "cat file || sudo cat /etc/passwd"
       '("cat" "sudo" "cat")
       t)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat file || sudo cat /etc/passwd"
                     "/workspace"
                     scope-config)))
        ;; Assert: sudo blocked at position 1
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo")
        (expect (plist-get result :position) :to-equal 1))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "validates sequential commands (;)"
    ;; Test: Sequential commands validated (;)
    ;; Command: 'mkdir dir; rm dir; ls'
    ;; Mock parse: Extract commands ['mkdir', 'rm', 'ls']
    ;; Assert: rm blocked at position 1
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract commands from sequence
      (helpers-spec-mock-bash-parse
       "mkdir dir; rm dir; ls"
       '("mkdir" "rm" "ls")
       t)

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "mkdir dir; rm dir; ls"
                     "/workspace"
                     scope-config)))
        ;; Assert: rm blocked at position 1
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows commands not in deny list to proceed to next stage"
    ;; Test: Commands not in deny list proceed to next stage
    ;; Command: 'ls -la'
    ;; Mock parse: Extract commands ['ls']
    ;; Mock deny list: Does not contain 'ls'
    ;; Assert: Deny list check passes (returns nil)
    ;; Verify: Proceeds to stage 4 (semantic extraction)
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract 'ls' command
      (helpers-spec-mock-bash-parse
       "ls -la"
       '("ls")
       t)

      ;; Mock semantics: ls reads from current directory
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace" :command-name "ls"))
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls -la"
                     "/workspace"
                     scope-config)))
        ;; Assert: Deny list check passes, proceeds through pipeline
        ;; Result should be nil (success) since read of /workspace is allowed
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "allows multiple non-denied commands in pipeline to proceed"
    ;; Test: Multiple non-denied commands in pipeline proceed
    ;; Command: 'cat file.txt | grep pattern | head -10'
    ;; Mock parse: Extract commands ['cat', 'grep', 'head']
    ;; Assert: All pass deny list check, proceed to next stage
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
    - sudo
    - chmod
    - dd

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Extract all commands from pipeline
      (helpers-spec-mock-bash-parse
       "cat file.txt | grep pattern | head -10"
       '("cat" "grep" "head")
       t)

      ;; Mock semantics: cat reads file.txt
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
       '(:detected nil)
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat file.txt | grep pattern | head -10"
                     "/workspace"
                     scope-config)))
        ;; Assert: All commands pass deny list check, proceed through pipeline
        ;; Result should be nil (success) since read is allowed
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml))))

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
