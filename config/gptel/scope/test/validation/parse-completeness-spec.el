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
       (validation-dir test-dir)
       (scope-test-dir (expand-file-name ".." validation-dir))
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

  ;; Cycle-2: the per-session enforce-parse-complete override has been
  ;; deleted (`register/invariant/scope-parse-complete-is-true').  The
  ;; validator now reads `jf/gptel-scope--enforce-parse-complete' (a
  ;; module-level defconst, fixed at t) directly.  Tests below cover
  ;; the always-strict behaviour; the legacy "permissive mode" tests
  ;; were removed because that mode no longer exists.

  (it "always rejects incomplete parse (syntax error)"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect (plist-get result :error) :to-equal "parse_incomplete"))))

  (it "always rejects incomplete loop"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect (plist-get result :error) :to-equal "parse_incomplete"))))

  (it "always rejects incomplete conditional"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect (plist-get result :error) :to-equal "parse_incomplete"))))

  (it "allows valid complex syntax"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect result :to-be nil))))

  (it "allows valid conditionals"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect result :to-be nil))))

  (it "enforce-parse-complete defconst is fixed at t"
    ;; Cycle-2 confirmed register/invariant/scope-parse-complete-is-true:
    ;; the constant is the validator's only authority for this flag, and
    ;; it is unconditionally `t'. Replaces the deleted per-session
    ;; security-config override tests.
    (expect jf/gptel-scope--enforce-parse-complete :to-be t))

  (it "parse completeness checked before deny list"
    (let ((scope-config (helpers-spec-make-scope-config
                         :read '("/workspace/**")
                         :write '("/workspace/**"))))
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
        (expect (plist-get result :error) :to-equal "parse_incomplete")))))

(provide 'parse-completeness-spec)
;;; parse-completeness-spec.el ends here
