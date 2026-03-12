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
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))

;;; Test Suite

(describe "run_bash_command: Parse completeness enforcement (stage 2)"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (it "strict mode rejects incomplete parse (syntax error)"
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
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "parse_incomplete"))
      (delete-file scope-yml)))

  (it "strict mode rejects incomplete loop"
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
      (helpers-spec-mock-bash-parse
       "for i in ; do echo $i; done"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.3))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for i in ; do echo $i; done"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "parse_incomplete"))
      (delete-file scope-yml)))

  (it "strict mode rejects incomplete conditional"
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
      (helpers-spec-mock-bash-parse
       "if [ -f file.txt ; then cat"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.4))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "if [ -f file.txt ; then cat"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "parse_incomplete"))
      (delete-file scope-yml)))

  (it "strict mode allows valid complex syntax"
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
      (helpers-spec-mock-bash-parse
       "for f in /workspace/*.el; do grep TODO $f; done"
       '("grep")
       t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file.el" :command-name "grep"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for f in /workspace/*.el; do grep TODO $f; done"
                     "/workspace"
                     scope-config)))
        (expect result :to-be nil))
      (delete-file scope-yml)))

  (it "strict mode allows valid conditionals"
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
      (helpers-spec-mock-bash-parse
       "if [ -f /workspace/file ]; then cat /workspace/file; fi"
       '("cat")
       t)
      (helpers-spec-mock-bash-semantics
       (list (helpers-spec--make-file-op :read "/workspace/file" :command-name "cat"))
       nil
       '(:ratio 1.0))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "if [ -f /workspace/file ]; then cat /workspace/file; fi"
                     "/workspace"
                     scope-config)))
        (expect result :to-be nil))
      (delete-file scope-yml)))

  (it "permissive mode allows incomplete parse with warning"
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
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-be nil))
      (delete-file scope-yml)))

  (it "permissive mode allows partial syntax"
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
      (helpers-spec-mock-bash-parse
       "for i in ; do echo $i"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.3))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "for i in ; do echo $i"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-be nil))
      (delete-file scope-yml)))

  (it "default mode is strict"
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
      (helpers-spec-mock-bash-parse
       "function incomplete() {"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.5))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "function incomplete() {"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "parse_incomplete"))
      (delete-file scope-yml)))

  (it "parse completeness checked before deny list"
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
      (helpers-spec-mock-bash-parse
       "sudo incomplete syntax {"
       '()
       nil)
      (helpers-spec-mock-bash-semantics
       '()
       nil
       '(:ratio 0.2))
      (let ((result (jf/gptel-scope--validate-command-semantics
                     "sudo incomplete syntax {"
                     "/workspace"
                     scope-config)))
        (expect (plist-get result :error) :to-equal "parse_incomplete"))
      (delete-file scope-yml))))

(provide 'parse-completeness-spec)
;;; parse-completeness-spec.el ends here
