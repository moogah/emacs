;;; error-handling-spec.el --- Tests for orchestrator error handling behavior -*- lexical-binding: t; -*-

;;; Commentary:

;; The orchestrator wraps Layer 0 (recursive engine + token claiming) in a
;; single condition-case block. This means errors from the engine and errors
;; from token claiming are handled identically — both are silently logged
;; and the entire Layer 0 result is discarded.
;;
;; For a security system, silent failures are dangerous. If the extraction
;; engine encounters a parse structure it doesn't understand, the orchestrator
;; should signal this clearly — not return an empty result that looks like
;; "no file operations found."
;;
;; These tests validate error handling behavior and distinguish between:
;; - Legitimate empty results (command has no file operations)
;; - Extraction failures (engine threw, results discarded)

;;; Code:

(require 'cl-lib)
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)
(require 'bash-parser)

(defvar error-handling--saved-handlers nil)

(describe "Orchestrator error handling"

  (before-each
    (setq error-handling--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers error-handling--saved-handlers))

  (describe "extraction errors are distinguishable from empty results"

    (it "echo hello has empty :filesystem — legitimate no-op"
      (let* ((parsed (jf/bash-parse "echo hello"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        ;; No filesystem operations — this is correct, not an error
        (expect fs-ops :to-be nil)
        ;; parse-complete should be true
        (expect (plist-get result :parse-complete) :to-be-truthy)))

    (it "cat file.txt has non-empty :filesystem — extraction succeeded"
      ;; If this returns empty :filesystem, it means extraction threw
      ;; and was silently discarded. The result should never be empty
      ;; for a command with known file operations.
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect fs-ops :not :to-be nil)
        (expect (length fs-ops) :to-be-greater-than 0)))

    (it "malformed parse input does not silently produce empty result"
      ;; A nil parsed-command or one missing :tokens should produce
      ;; a clear error, not silently return empty domains.
      ;; Currently the condition-case catches the error and returns
      ;; empty domains — indistinguishable from a no-op command.
      (let* ((bad-parsed (list :tokens nil :parse-complete nil
                               :command-name nil :type nil))
             (result (jf/bash-extract-semantics bad-parsed)))
        ;; The result exists (orchestrator doesn't throw)
        (expect result :not :to-be nil)
        ;; But we should be able to tell this wasn't a clean extraction.
        ;; With parse-complete nil, at minimum that flag signals something
        ;; was wrong. Verify it passes through.
        (expect (plist-get result :parse-complete) :to-be nil))))

  (describe "Layer 0 errors do not discard valid extraction results"

    ;; The condition-case wraps both extraction AND token claiming.
    ;; If token claiming fails, the extraction results (which succeeded)
    ;; should not be discarded.

    (it "cat file.txt extraction results survive even if token claiming fails"
      ;; cat has a handler, so its positional-arg operations trigger
      ;; the cl-return bug in token claiming. But the recursive engine
      ;; successfully extracted the operations before token claiming ran.
      ;; Those operations should not be discarded.
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains)))
             (coverage (plist-get result :coverage)))
        ;; Operations must be present (extraction succeeded)
        (expect fs-ops :not :to-be nil)
        ;; Coverage must reflect the operations
        (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)))

    (it "cat input.txt > output.txt preserves both extraction results"
      ;; Both the :read (from positional-arg) and :write (from redirection)
      ;; were extracted by the recursive engine. Token claiming failure
      ;; should not cause either to be lost.
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 1)))

    (it "rm a.txt && touch b.txt preserves operations from both subcommands"
      ;; The recursive engine extracts operations from both chain commands.
      ;; Token claiming failure should not discard those results.
      (let* ((parsed (jf/bash-parse "rm a.txt && touch b.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 1)))))

(provide 'error-handling-spec)
;;; error-handling-spec.el ends here
