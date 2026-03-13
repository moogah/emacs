;;; orchestrator-layers-spec.el --- Integration tests for orchestrator two-layer architecture -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that the orchestrator's two-layer architecture works correctly
;; end-to-end through jf/bash-extract-semantics.
;;
;; These tests validate that:
;; 1. Layer 0 (grammar extraction) results survive into the final output
;; 2. Redirections are never silently lost for commands with handlers
;; 3. Compound commands produce operations from all subcommands
;;
;; Each test asserts CORRECT behavior. A passing test means the architecture
;; is working. A failing test means there is a bug.
;;
;; Context: The orchestrator wraps Layer 0 in condition-case. If token
;; claiming throws, the entire Layer 0 result (including redirections
;; extracted by the recursive engine) is discarded. Layer 1 command
;; handlers cannot compensate because they don't extract redirections
;; and cannot dispatch on compound command-names.

;;; Code:

(require 'cl-lib)
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)

;; Ensure jf/bash-parse is available
(require 'bash-parser)

(defvar orchestrator-layers--saved-handlers nil
  "Saved handler registry for test isolation.")

(describe "Orchestrator two-layer architecture"

  (before-each
    (setq orchestrator-layers--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers orchestrator-layers--saved-handlers))

  ;; ─── Hypothesis 1: Layer 0 results survive token claiming ─────────────

  (describe "Layer 0 results are not silently discarded"

    (it "Layer 0 does not throw during extraction of cat file.txt"
      ;; If Layer 0 throws, the condition-case in the orchestrator catches it
      ;; and discards all operations. We detect this by checking that the
      ;; message log does NOT contain the error sentinel.
      (let ((messages-before (current-message)))
        (jf/bash-extract-semantics (jf/bash-parse "cat file.txt"))
        ;; The orchestrator logs "Error in grammar-level extraction: ..."
        ;; when Layer 0 throws. If this message appears, Layer 0 was discarded.
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (expect (search-backward "Error in grammar-level extraction" nil t)
                  :to-be nil))))

    (it "returns operations from Layer 0 for cat with positional arg"
      ;; cat has a registered handler, so this exercises the path where
      ;; Layer 0 extracts operations AND token claiming must succeed.
      ;; If token claiming throws, Layer 0 operations are discarded and
      ;; only Layer 1 handler results remain.
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect fs-ops :not :to-be nil)
        ;; Layer 0 operations include :source metadata from the recursive engine.
        ;; Layer 1 handler operations do not include :source :positional-arg.
        ;; Check that the operation has :source to confirm it came from Layer 0.
        (let ((read-op (cl-find-if
                        (lambda (op)
                          (and (equal (plist-get op :file) "file.txt")
                               (eq (plist-get op :operation) :read)))
                        fs-ops)))
          (expect read-op :not :to-be nil)
          (expect (plist-get read-op :source) :to-be-truthy)))))

  ;; ─── Hypothesis 2: Redirections preserved for handler commands ────────

  (describe "redirections are not lost when command has a handler"

    (it "extracts both :read and :write for cat input.txt > output.txt"
      ;; cat has a handler → Layer 0 token claiming must not discard
      ;; the redirection-extracted :write operation.
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        ;; Must have at least 2 operations: read from positional, write from redirect
        (expect (length fs-ops) :to-be-greater-than 1)
        ;; Positional arg read
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        ;; Redirection write — this is the critical assertion.
        ;; If Layer 0 is discarded, this write operation is lost because
        ;; Layer 1 (cat handler) only knows about positional args.
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts all three operations for cat input.txt > output.txt 2> error.txt"
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt 2> error.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 2)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "error.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts :read and :append for cat input.txt >> output.txt"
      (let* ((parsed (jf/bash-parse "cat input.txt >> output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 1)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "input.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :append)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts redirection :write alongside grep handler operations"
      ;; grep has a handler for :read. The > redirection must also produce :write.
      (let* ((parsed (jf/bash-parse "grep pattern input.txt > matches.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "matches.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy))))

  ;; ─── Hypothesis 3: Compound commands produce all subcommand ops ───────

  (describe "compound commands with handler commands"

    (it "extracts operations from both sides of rm temp.txt && touch new.txt"
      (let* ((parsed (jf/bash-parse "rm temp.txt && touch new.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 1)
        ;; rm's delete
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "temp.txt")
                                (eq (plist-get op :operation) :delete)))
                         fs-ops)
                :to-be-truthy)
        ;; touch's create
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "new.txt")
                                (memq (plist-get op :operation)
                                      '(:create :create-or-modify))))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts all four operations from chain with redirections"
      ;; cat file1.txt > combined.txt; cat file2.txt >> combined.txt
      (let* ((parsed (jf/bash-parse "cat file1.txt > combined.txt; cat file2.txt >> combined.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 3)
        ;; First cat: read + write redirect
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "file1.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "combined.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)
        ;; Second cat: read + append redirect
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "file2.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "combined.txt")
                                (eq (plist-get op :operation) :append)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts operations from pipeline with handler commands"
      ;; cat file.txt | grep pattern > output.txt
      (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        ;; cat's read + grep's redirect write
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "file.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "output.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts all operations from cp source.txt backup.txt && rm source.txt"
      (let* ((parsed (jf/bash-parse "cp source.txt backup.txt && rm source.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (alist-get :filesystem (plist-get result :domains))))
        (expect (length fs-ops) :to-be-greater-than 2)
        ;; cp's read + write
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "source.txt")
                                (eq (plist-get op :operation) :read)))
                         fs-ops)
                :to-be-truthy)
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "backup.txt")
                                (eq (plist-get op :operation) :write)))
                         fs-ops)
                :to-be-truthy)
        ;; rm's delete
        (expect (cl-some (lambda (op)
                           (and (equal (plist-get op :file) "source.txt")
                                (eq (plist-get op :operation) :delete)))
                         fs-ops)
                :to-be-truthy))))

  ;; ─── Coverage accuracy ────────────────────────────────────────────────

  (describe "coverage reflects Layer 0 contributions"

    (it "claims tokens for cat file.txt (not zero coverage)"
      ;; If Layer 0 is discarded, its claimed-token-ids are lost.
      ;; Layer 1 handlers may or may not provide claimed-token-ids.
      ;; The coverage should reflect that the command-name and positional-arg
      ;; tokens were understood.
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)
        ;; cat file.txt has 2 tokens; both should be claimed
        (expect (plist-get coverage :coverage-ratio) :to-be-greater-than 0.5)))

    (it "claims redirection tokens for cat input.txt > output.txt"
      ;; Redirection tokens are only claimed by Layer 0's token claiming.
      ;; If Layer 0 is discarded, redirection tokens are unclaimed.
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage))
             (unclaimed (plist-get coverage :unclaimed-tokens)))
        ;; No redirection tokens should be unclaimed
        (expect (cl-remove-if-not
                 (lambda (tok) (eq (plist-get tok :type) :redirection))
                 unclaimed)
                :to-equal '())))))

(provide 'orchestrator-layers-spec)
;;; orchestrator-layers-spec.el ends here
