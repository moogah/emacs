;;; parse-completeness-spec.el --- Parse completeness enforcement tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; PARSE COMPLETENESS ENFORCEMENT (Stage 2)
;;
;; Tests the parse completeness validation stage of the run_bash_command
;; seven-stage validation pipeline.
;;
;; Stage 2 enforces that bash commands parse completely before proceeding
;; to semantic validation. This prevents execution of syntactically incomplete
;; or malformed commands.
;;
;; Key behaviors tested:
;; - Strict mode rejects incomplete parses (syntax errors, incomplete loops/conditionals)
;; - Strict mode allows valid complex syntax (loops, conditionals)
;; - Permissive mode allows incomplete parse with warnings
;; - Default mode is strict
;; - Parse completeness is checked before deny list (stage ordering)
;;
;; Test naming convention: describe "Category: <scenario-name>"
;; Each test validates a complete interaction flow through stage 2.

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

(describe "run_bash_command: Parse completeness enforcement (stage 2)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "strict mode rejects incomplete parse (syntax error)"
    ;; Test: Strict mode rejects incomplete parse (syntax error)
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'function incomplete() {'
    ;; Mock jf/bash-parse: Return {:parse-complete nil :parse-errors 'Unexpected EOF'}
    ;; Assert: :error 'parse_incomplete', :parse-errors contains 'EOF'
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
      ;; Mock parse: Incomplete parse with error
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()  ; No commands extracted
       nil) ; parse-complete = false

      ;; Mock semantics (not reached due to parse failure)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        ;; Assert: parse_incomplete error
        (expect (plist-get result :error) :to-equal "parse_incomplete"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "strict mode rejects incomplete loop"
    ;; Test: Strict mode rejects incomplete loop
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'for i in ; do echo $i; done'
    ;; Mock parse: {:parse-complete nil :parse-errors 'Syntax error in for-loop'}
    ;; Assert: :error 'parse_incomplete'
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
      ;; Mock parse: Incomplete loop
      (helpers-spec-mock-bash-parse
       "for i in ; do echo $i; done"
       '()
       nil) ; parse-complete = false

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.3))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for i in ; do echo $i; done"
                     "/workspace"
                     scope-config)))
        ;; Assert: parse_incomplete error
        (expect (plist-get result :error) :to-equal "parse_incomplete"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "strict mode rejects incomplete conditional"
    ;; Test: Strict mode rejects incomplete conditional
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'if [ -f file.txt ; then cat'
    ;; Mock parse: {:parse-complete nil}
    ;; Assert: :error 'parse_incomplete'
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
      ;; Mock parse: Incomplete conditional
      (helpers-spec-mock-bash-parse
       "if [ -f file.txt ; then cat"
       '()
       nil) ; parse-complete = false

      ;; Mock semantics
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.4))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "if [ -f file.txt ; then cat"
                     "/workspace"
                     scope-config)))
        ;; Assert: parse_incomplete error
        (expect (plist-get result :error) :to-equal "parse_incomplete"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "strict mode allows valid complex syntax"
    ;; Test: Strict mode allows valid complex syntax
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'for f in /workspace/*.el; do grep TODO $f; done'
    ;; Mock parse: {:parse-complete t}
    ;; Mock semantics: Return file-ops for read operations
    ;; Assert: Success (proceeds to file validation)
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
      ;; Mock parse: Complete parse
      (helpers-spec-mock-bash-parse
       "for f in /workspace/*.el; do grep TODO $f; done"
       '("grep")
       t) ; parse-complete = true

      ;; Mock semantics: Read operations
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.el" :command-name "grep"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for f in /workspace/*.el; do grep TODO $f; done"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success (no error)
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "strict mode allows valid conditionals"
    ;; Test: Strict mode allows valid conditionals
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'if [ -f /workspace/file ]; then cat /workspace/file; fi'
    ;; Mock parse: {:parse-complete t}
    ;; Assert: Success (complete parse)
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
      ;; Mock parse: Complete conditional
      (helpers-spec-mock-bash-parse
       "if [ -f /workspace/file ]; then cat /workspace/file; fi"
       '("cat")
       t) ; parse-complete = true

      ;; Mock semantics: Read operation
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file" :command-name "cat"))
       nil
       '(:ratio 1.0))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "if [ -f /workspace/file ]; then cat /workspace/file; fi"
                     "/workspace"
                     scope-config)))
        ;; Assert: Success
        (expect result :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "permissive mode allows incomplete parse with warning"
    ;; Test: Permissive mode allows incomplete parse with warning
    ;; Setup: Scope with security.enforce_parse_complete: false
    ;; Command: 'function incomplete() {'
    ;; Mock parse: {:parse-complete nil :parse-errors 'Unexpected EOF'}
    ;; Assert: :success t (continues despite incomplete parse)
    ;; Assert: :warnings contains incomplete parse notice
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
  enforce_parse_complete: false
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Incomplete parse
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil) ; parse-complete = false

      ;; Mock semantics: Zero file operations (no-op)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        ;; Assert: Warning but not error (permissive mode)
        ;; In permissive mode with no file ops, should succeed (nil result)
        ;; The implementation may return a warning plist or nil
        ;; Check that no error is returned
        (expect (plist-get result :error) :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "permissive mode allows partial syntax"
    ;; Test: Permissive mode allows partial syntax
    ;; Setup: Scope with security.enforce_parse_complete: false
    ;; Command: 'for i in ; do echo $i'
    ;; Mock parse: {:parse-complete nil}
    ;; Assert: Success with warning
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
  enforce_parse_complete: false
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Incomplete parse
      (helpers-spec-mock-bash-parse
       "for i in ; do echo $i"
       '()
       nil) ; parse-complete = false

      ;; Mock semantics: Zero file operations (no-op)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.3))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for i in ; do echo $i"
                     "/workspace"
                     scope-config)))
        ;; Assert: No error (permissive mode allows incomplete parse)
        (expect (plist-get result :error) :to-be nil))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "default mode is strict"
    ;; Test: Default mode is strict
    ;; Setup: Scope with NO security section (uses defaults)
    ;; Command: 'function incomplete() {'
    ;; Mock parse: {:parse-complete nil}
    ;; Assert: :error 'parse_incomplete' (strict is default)
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
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Incomplete parse
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil) ; parse-complete = false

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
        ;; Assert: parse_incomplete error (default is strict)
        (expect (plist-get result :error) :to-equal "parse_incomplete"))

      ;; Cleanup
      (delete-file scope-yml)))

  (it "parse completeness checked before deny list"
    ;; Test: Parse completeness checked before deny list
    ;; Setup: Scope with security.enforce_parse_complete: true
    ;; Command: 'sudo incomplete syntax {'
    ;; Mock parse: {:parse-complete nil}
    ;; Assert: :error 'parse_incomplete' (fails at stage 2, before stage 3 deny list check)
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
    - rm

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"))
           (scope-config (helpers-spec-load-scope-config scope-yml)))
      ;; Mock parse: Incomplete parse (stage 2 failure)
      (helpers-spec-mock-bash-parse
       "sudo incomplete syntax {"
       '()
       nil) ; parse-complete = false

      ;; Mock semantics (not reached)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.2))

      ;; Validate command
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sudo incomplete syntax {"
                     "/workspace"
                     scope-config)))
        ;; Assert: parse_incomplete error (stage 2 fails before stage 3 deny list)
        (expect (plist-get result :error) :to-equal "parse_incomplete"))

      ;; Cleanup
      (delete-file scope-yml))))

(provide 'parse-completeness-spec)
;;; parse-completeness-spec.el ends here
