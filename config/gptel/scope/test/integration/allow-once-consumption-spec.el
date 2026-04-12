;;; allow-once-consumption-spec.el --- Tests for allow-once consumption semantics across layers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Integration tests exposing allow-once double-consumption bug.
;;
;; ISSUE: Allow-once is checked in TWO places during the same call path:
;;   1. gptel-make-scoped-tool macro (scope-core.el ~line 174)
;;   2. jf/gptel-scope--check-tool-permission (scope-core.el ~line 683)
;;
;; The macro short-circuits before reaching check-tool-permission, so
;; the double-check is currently invisible.  But this means:
;;   - check-tool-permission has a hidden dependency on being called
;;     ONLY when allow-once was NOT granted (i.e., the macro pre-filters)
;;   - If the macro is ever refactored to delegate fully to
;;     check-tool-permission, the second check-allow-once would find
;;     an empty list (already consumed) and fall through to validation.
;;
;; These tests document the expected single-consumption contract
;; and expose the architectural redundancy.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (config-dir (expand-file-name ".." gptel-dir))
       (contracts-dir (expand-file-name "test/contracts/" config-dir)))
  ;; Contract infrastructure
  (add-to-list 'load-path contracts-dir)
  (require 'contract-core)
  (require 'contract-scope-config)
  (contract--register-buttercup-matcher)
  ;; Scope helpers
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-shell-tools
           (expand-file-name "scope/scope-shell-tools.el" gptel-dir)))


;;; Single-consumption contract

(describe "allow-once single-consumption contract"

  (before-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (describe "check-allow-once consumes exactly once"

    (it "first check returns t and removes the entry"
      (let ((path (expand-file-name "/workspace/file.txt")))
        (jf/gptel-scope-add-to-allow-once-list "read_file" path)
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (expect (jf/gptel-scope--check-allow-once "read_file" path) :to-be t)
        (expect jf/gptel-scope--allow-once-list :to-be nil)))

    (it "second check on same tool+resource returns nil"
      (let ((path (expand-file-name "/workspace/file.txt")))
        (jf/gptel-scope-add-to-allow-once-list "read_file" path)
        ;; First: consume
        (jf/gptel-scope--check-allow-once "read_file" path)
        ;; Second: should find nothing
        (expect (jf/gptel-scope--check-allow-once "read_file" path) :to-be nil)))

    (it "consuming one entry does not affect other entries"
      (let ((path-a (expand-file-name "/workspace/a.txt"))
            (path-b (expand-file-name "/workspace/b.txt")))
        (jf/gptel-scope-add-to-allow-once-list "read_file" path-a)
        (jf/gptel-scope-add-to-allow-once-list "read_file" path-b)
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 2)
        ;; Consume first
        (jf/gptel-scope--check-allow-once "read_file" path-a)
        ;; Second should still be present
        (expect (length jf/gptel-scope--allow-once-list) :to-equal 1)
        (expect (jf/gptel-scope--check-allow-once "read_file" path-b) :to-be t)))))


;;; check-tool-permission also checks allow-once (redundancy exposure)

(describe "allow-once redundancy in check-tool-permission"

  (before-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "check-tool-permission grants access via allow-once for path tools"
    (let ((path (expand-file-name "/outside/secret.txt"))
          (config '(:paths (:read ("/workspace/**") :write () :execute () :modify () :deny ()))))
      ;; Path is NOT in read scope — would be denied without allow-once
      (jf/gptel-scope-add-to-allow-once-list "read_file" path)
      (let ((result (jf/gptel-scope--check-tool-permission
                     config "read_file" (list "/outside/secret.txt") nil)))
        (expect (plist-get result :allowed) :to-be t))))

  (it "check-tool-permission consumes allow-once (list empty after)"
    (let ((path (expand-file-name "/outside/secret.txt"))
          (config '(:paths (:read ("/workspace/**") :write () :execute () :modify () :deny ()))))
      (jf/gptel-scope-add-to-allow-once-list "read_file" path)
      (jf/gptel-scope--check-tool-permission
       config "read_file" (list "/outside/secret.txt") nil)
      ;; Allow-once should be consumed
      (expect jf/gptel-scope--allow-once-list :to-be nil)))

  (it "after consumption by check-tool-permission, same path is denied"
    (let ((path (expand-file-name "/outside/secret.txt"))
          (config '(:paths (:read ("/workspace/**") :write () :execute () :modify () :deny ()))))
      (jf/gptel-scope-add-to-allow-once-list "read_file" path)
      ;; First call: allow-once grants access
      (jf/gptel-scope--check-tool-permission
       config "read_file" (list "/outside/secret.txt") nil)
      ;; Second call: no allow-once, path not in scope → denied
      (let ((result (jf/gptel-scope--check-tool-permission
                     config "read_file" (list "/outside/secret.txt") nil)))
        (expect (plist-get result :allowed) :to-be nil))))

  (describe "allow-once should only be checked in one place"

    ;; RED PHASE: The macro and check-tool-permission both call
    ;; check-allow-once.  The correct behavior is that a single grant
    ;; works regardless of which layer checks it.  This test asserts
    ;; that check-tool-permission alone is sufficient — it should grant
    ;; access for an allow-once path without the macro pre-checking.

    (it "check-tool-permission should grant access for allow-once path without macro pre-check"
      (let ((path (expand-file-name "/outside/secret.txt"))
            (config '(:paths (:read ("/workspace/**") :write () :execute () :modify () :deny ()))))
        (jf/gptel-scope-add-to-allow-once-list "read_file" path)
        ;; Simulate the macro calling check-allow-once first (consuming it)
        (jf/gptel-scope--check-allow-once "read_file" path)
        ;; CORRECT: check-tool-permission should STILL grant access.
        ;; RED: Fails because check-allow-once already consumed the
        ;; permission, proving the double-check is not idempotent.
        (let ((result (jf/gptel-scope--check-tool-permission
                       config "read_file" (list "/outside/secret.txt") nil)))
          (expect (plist-get result :allowed) :to-be t))))))


;;; Bash tool allow-once (composite key format)

(describe "allow-once with bash tool composite keys"

  (before-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "bash tools use composite key format: command:directory"
    (let ((composite (format "%s:%s" "which brew" (expand-file-name "/"))))
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command" composite)
      (expect (jf/gptel-scope--check-allow-once "run_bash_command" composite)
              :to-be t)))

  (it "different command same directory does not match"
    (let ((stored (format "%s:%s" "which brew" (expand-file-name "/")))
          (lookup (format "%s:%s" "ls -la" (expand-file-name "/"))))
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command" stored)
      (expect (jf/gptel-scope--check-allow-once "run_bash_command" lookup)
              :to-be nil)))

  (it "same command different directory does not match"
    (let ((stored (format "%s:%s" "which brew" (expand-file-name "/")))
          (lookup (format "%s:%s" "which brew" (expand-file-name "/tmp"))))
      (jf/gptel-scope-add-to-allow-once-list "run_bash_command" stored)
      (expect (jf/gptel-scope--check-allow-once "run_bash_command" lookup)
              :to-be nil))))


(provide 'allow-once-consumption-spec)

;;; allow-once-consumption-spec.el ends here
