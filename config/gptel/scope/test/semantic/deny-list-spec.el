;;; deny-list-spec.el --- Deny list bypass prevention tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; DENY LIST BYPASS PREVENTION (Stage 3)
;;
;; Moved from: tools/test/behavioral/run-bash-command/deny-list-validation-spec.el
;;
;; Tests the deny list validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 3 enforces that commands not in the bash_tools.deny list
;; are prevented from execution. This provides a safety mechanism for
;; blocking dangerous commands (rm, sudo, chmod, dd) regardless of
;; path-based semantic constraints.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Test Suite

(describe "run_bash_command: Deny list bypass prevention (stage 3)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "blocks simple denied command"
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
      (helpers-spec-mock-bash-parse
       "rm file.txt"
       '("rm")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "rm file.txt"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 0))
      (delete-file scope-yml)))

  (it "blocks denied command in pipeline position 2"
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
      (helpers-spec-mock-bash-parse
       "ls | xargs rm"
       '("ls" "xargs" "rm")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls | xargs rm"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2)
        (expect (plist-get result :message) :to-match "position 2"))
      (delete-file scope-yml)))

  (it "blocks denied command in complex pipeline"
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
      (helpers-spec-mock-bash-parse
       "find . -name \"*.tmp\" | xargs rm"
       '("find" "xargs" "rm")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "find . -name \"*.tmp\" | xargs rm"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 2))
      (delete-file scope-yml)))

  (it "blocks sudo in pipeline"
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
      (helpers-spec-mock-bash-parse
       "cat data.txt | sudo tee /etc/config"
       '("cat" "sudo" "tee")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat data.txt | sudo tee /etc/config"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo")
        (expect (plist-get result :position) :to-equal 1))
      (delete-file scope-yml)))

  (it "validates chain operators (&&)"
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
      (helpers-spec-mock-bash-parse
       "ls && rm file.txt"
       '("ls" "rm")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls && rm file.txt"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))
      (delete-file scope-yml)))

  (it "validates chain operators (||)"
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
      (helpers-spec-mock-bash-parse
       "cat file || sudo cat /etc/passwd"
       '("cat" "sudo" "cat")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat file || sudo cat /etc/passwd"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "sudo")
        (expect (plist-get result :position) :to-equal 1))
      (delete-file scope-yml)))

  (it "validates sequential commands (;)"
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
      (helpers-spec-mock-bash-parse
       "mkdir dir; rm dir; ls"
       '("mkdir" "rm" "ls")
       t)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "mkdir dir; rm dir; ls"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "command_denied")
        (expect (plist-get result :command) :to-equal "rm")
        (expect (plist-get result :position) :to-equal 1))
      (delete-file scope-yml)))

  (it "allows commands not in deny list to proceed to next stage"
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
      (helpers-spec-mock-bash-parse
       "ls -la"
       '("ls")
       t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/files" :command-name "ls"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "ls -la"
                     "/workspace"
                     scope-config)))
        (expect result :to-be nil))
      (delete-file scope-yml)))

  (it "allows multiple non-denied commands in pipeline to proceed"
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
      (helpers-spec-mock-bash-parse
       "cat file.txt | grep pattern | head -10"
       '("cat" "grep" "head")
       t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.txt" :command-name "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "cat file.txt | grep pattern | head -10"
                     "/workspace"
                     scope-config)))
        (expect result :to-be nil))
      (delete-file scope-yml))))

(provide 'deny-list-spec)

;;; deny-list-spec.el ends here
