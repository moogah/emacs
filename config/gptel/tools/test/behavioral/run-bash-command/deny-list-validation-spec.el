;;; deny-list-validation-spec.el --- Deny list bypass prevention tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; DENY LIST BYPASS PREVENTION (Stage 3)
;;
;; Tests the deny list validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 3 enforces that commands not in the bash_tools.deny list
;; are prevented from execution. This provides a safety mechanism for
;; blocking dangerous commands (rm, sudo, chmod, dd) regardless of
;; path-based semantic constraints.
;;
;; Key behaviors tested:
;; - Simple denied commands blocked at position 0
;; - Denied commands in pipelines blocked at their position
;; - Denied commands in chains (&&, ||, ;) blocked appropriately
;; - Multiple commands in pipelines checked in order
;; - Non-denied commands proceed to next stage (semantic validation)
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 3.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (test-parent-dir (expand-file-name ".." test-dir))
       (test-root-dir (expand-file-name ".." test-parent-dir))
       (tools-dir (expand-file-name ".." test-root-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" test-root-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "../scope/scope-shell-tools.el" tools-dir)))

;;; Test Suite

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
       nil
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
       nil
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
       nil
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
       nil
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
       nil
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
       nil
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
       nil
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

      ;; Mock semantics: ls reads from current directory (simulate reading dir contents)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/files" :command-name "ls"))
       nil
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
       nil
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

(provide 'deny-list-validation-spec)

;;; deny-list-validation-spec.el ends here
