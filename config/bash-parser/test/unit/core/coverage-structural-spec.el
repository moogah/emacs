;;; coverage-structural-spec.el --- Structural-token + handler-registered coverage -*- lexical-binding: t; -*-

;;; Commentary:

;; Verifies the orchestrator's claim logic produces full (1.0) coverage for
;; commands whose tokens are either structural (separators, flags,
;; redirections) or belong to a simple command with a registered handler —
;; including no-op handlers for pure stdout/builtin commands like `echo'.
;;
;; Regression context: parse coverage previously stalled at 0.55 for any
;; realistic compound command because `||', `&&', flags, and echo's tokens
;; went unclaimed.

;;; Code:

(require 'buttercup)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (config-dir "/Users/jefffarr/emacs-gptel-bash-parser-contract-integration-tests/config"))
  (ignore test-dir)
  (add-to-list 'load-path (expand-file-name "bash-parser/core/" config-dir))
  (add-to-list 'load-path (expand-file-name "bash-parser/analysis/" config-dir))
  (add-to-list 'load-path (expand-file-name "bash-parser/plugins/" config-dir)))

(require 'bash-parser)
(require 'bash-parser-orchestrator)

(defun coverage-structural--ratio (command)
  "Parse COMMAND and return its coverage ratio."
  (plist-get (plist-get (jf/bash-extract-semantics (jf/bash-parse command))
                        :coverage)
             :coverage-ratio))

(defun coverage-structural--unclaimed (command)
  "Parse COMMAND and return a list of (TYPE . VALUE) pairs for unclaimed tokens."
  (mapcar (lambda (tok)
            (cons (plist-get tok :type) (plist-get tok :value)))
          (plist-get (plist-get (jf/bash-extract-semantics (jf/bash-parse command))
                                :coverage)
                     :unclaimed-tokens)))

(describe "Parse coverage: structural tokens"

  (it "flags are auto-claimed (ls -la /tmp reaches 1.0)"
    (expect (coverage-structural--ratio "ls -la /tmp") :to-equal 1.0))

  (it "|| and && separators are auto-claimed"
    (expect (coverage-structural--ratio "ls /a || ls /b") :to-equal 1.0)
    (expect (coverage-structural--ratio "ls /a && ls /b") :to-equal 1.0))

  (it "; separator is auto-claimed"
    (expect (coverage-structural--ratio "ls /a ; ls /b") :to-equal 1.0))

  (it "pipeline | separator is auto-claimed"
    ;; cat has a handler, sort doesn't — register a no-op first if needed
    (let ((ratio (coverage-structural--ratio "cat /a | cat /b")))
      (expect ratio :to-equal 1.0))))

(describe "Parse coverage: handler-registered non-filesystem commands"

  (it "echo alone reaches 1.0"
    (expect (coverage-structural--ratio "echo hello") :to-equal 1.0))

  (it "echo with multiple positional args reaches 1.0"
    (expect (coverage-structural--ratio "echo foo bar baz") :to-equal 1.0))

  (it "echo with redirection reaches 1.0"
    (expect (coverage-structural--ratio "echo hello > /tmp/out.txt") :to-equal 1.0))

  (it "true and false reach 1.0"
    (expect (coverage-structural--ratio "true") :to-equal 1.0)
    (expect (coverage-structural--ratio "false") :to-equal 1.0))

  (it "printf reaches 1.0"
    (expect (coverage-structural--ratio "printf '%s\\n' hello") :to-equal 1.0)))

(describe "Parse coverage: real-world session cases"

  (it "ls -la ABSPATH 2>/dev/null reaches 1.0"
    (expect (coverage-structural--ratio
             "ls -la /usr/local/bin/brew 2>/dev/null")
            :to-equal 1.0))

  (it "triple-|| chain with echo fallback reaches 1.0 (the reported command)"
    (let* ((cmd (concat "ls -la /usr/local/bin/brew 2>/dev/null || "
                        "ls -la /opt/homebrew/bin/brew 2>/dev/null || "
                        "echo \"Not found in common locations\""))
           (ratio (coverage-structural--ratio cmd))
           (unclaimed (coverage-structural--unclaimed cmd)))
      (expect ratio :to-equal 1.0)
      (expect unclaimed :to-equal nil))))

(provide 'coverage-structural-spec)
;;; coverage-structural-spec.el ends here
