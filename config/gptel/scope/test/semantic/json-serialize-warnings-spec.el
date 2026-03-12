;;; json-serialize-warnings-spec.el --- Expect correct behavior for consp nil bugs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;;; Commentary:

;; ROOT CAUSE: "Wrong type argument: consp, nil" from session
;; test-foo-10-2026-03-12/session/branches/main/session.md
;;
;; Two interface bugs found:
;;
;; Bug 1 (crash): jf/gptel-bash--execute-command wraps warnings in
;; (list "string") but json-serialize needs a vector for JSON arrays.
;; Commands with absolute paths trigger warnings → crash on serialize.
;;
;; Bug 2 (silent): bash-parser returns :domains as an alist but scope
;; validation reads it with plist-get, which silently returns nil.
;; File operation validation is completely bypassed.  Test helpers mask
;; this by mocking :domains as a plist.
;;
;; These tests assert CORRECT behavior and should FAIL until fixed.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load scope-shell-tools for execute-command and validation
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (semantic-dir test-dir)
       (scope-test-dir (expand-file-name ".." semantic-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'bash-parser-core)
  (require 'bash-parser-plugins))

;;; ============================================================
;;; Bug 2: scope validation must read domains from alist
;;; ============================================================

(describe "scope validation must read domains from bash-parser alist"

  (it "check-no-op detects file ops in real bash-parser output"
    ;; FAILS: bash-parser returns domains as alist, but check-no-op
    ;; uses plist-get which silently returns nil → skips validation.
    ;; cat /tmp/test.txt has file ops, so check-no-op should return t.
    (let* ((parsed (jf/bash-parse "cat /tmp/test.txt"))
           (semantics (jf/bash-extract-semantics parsed)))
      (expect (jf/gptel-scope--check-no-op semantics) :to-be t)))

  (it "check-no-op returns nil for commands without file ops"
    ;; Baseline: commands like `python3 --version` have no file ops
    (let* ((parsed (jf/bash-parse "python3 --version"))
           (semantics (jf/bash-extract-semantics parsed)))
      (expect (jf/gptel-scope--check-no-op semantics) :to-be nil)))

  (it "validate-command-semantics denies reading /etc/shadow"
    ;; FAILS: alist/plist mismatch causes check-no-op to short-circuit,
    ;; so file path validation never runs → cat /etc/shadow is allowed.
    (let* ((command "cat /etc/shadow")
           (directory "/tmp")
           (scope-config (list :paths (list :read '("/tmp/**")
                                            :write nil
                                            :execute nil
                                            :modify nil
                                            :deny '("/etc/**"))
                               :bash-tools (list :deny '("sudo" "rm"))
                               :cloud (list :auth-detection "warn")
                               :security (list :enforce-parse-complete nil
                                               :max-coverage-threshold 0.9)))
           (result (jf/gptel-scope--validate-command-semantics
                    command directory scope-config)))
      ;; Should return an error plist (path denied or out of scope)
      (expect result :not :to-be nil)
      (expect (plist-get result :error) :to-be-truthy))))

(provide 'json-serialize-warnings-spec)
;;; json-serialize-warnings-spec.el ends here
