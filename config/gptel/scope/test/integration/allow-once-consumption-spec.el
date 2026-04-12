;;; allow-once-consumption-spec.el --- Tests for allow-once consumption semantics across layers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Integration tests for allow-once consumption semantics.
;;
;; The gptel-make-scoped-tool macro is now the sole owner of the
;; "check permission and dispatch" flow (scope-tool-wrapper.el).
;; check-tool-permission was removed during the scope-core split, so
;; the double-consumption redundancy these tests originally exposed no
;; longer exists.  What remains documents the single-consumption
;; contract for jf/gptel-scope--check-allow-once and the composite-key
;; format used by bash tools.

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
