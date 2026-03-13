;;; legacy-wrapper-spec.el --- Tests that the deprecated wrapper delegates to the orchestrator -*- lexical-binding: t; -*-

;;; Commentary:

;; The design specifies that jf/bash-extract-file-operations should be a thin
;; wrapper around jf/bash-extract-semantics, extracting only the :filesystem
;; domain operations from the result.
;;
;; Current state: The wrapper calls jf/bash--extract-file-operations-impl
;; directly, bypassing the orchestrator entirely. This creates two divergent
;; code paths — one for callers using the old API (including the security
;; validator) and one for callers using the new API (gptel scope validation).
;;
;; These tests assert the design's intended behavior: that the wrapper
;; produces results consistent with the orchestrator. They will fail if
;; the wrapper bypasses the orchestrator.

;;; Code:

(require 'cl-lib)
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)
(require 'bash-parser)

(defvar legacy-wrapper--saved-handlers nil)

(describe "Legacy wrapper (jf/bash-extract-file-operations)"

  (before-each
    (setq legacy-wrapper--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers legacy-wrapper--saved-handlers))

  (describe "produces same operations as orchestrator :filesystem domain"

    (it "returns same operation set for simple command: cat file.txt"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (wrapper-ops (jf/bash-extract-file-operations parsed))
             (orchestrator-result (jf/bash-extract-semantics parsed))
             (orchestrator-ops (alist-get :filesystem
                                         (plist-get orchestrator-result :domains))))
        ;; Both should return operations
        (expect wrapper-ops :not :to-be nil)
        (expect orchestrator-ops :not :to-be nil)
        ;; Same number of operations
        (expect (length wrapper-ops) :to-equal (length orchestrator-ops))
        ;; Same files and operations (semantic core)
        (dolist (w-op wrapper-ops)
          (expect (cl-some (lambda (o-op)
                             (and (equal (plist-get w-op :file)
                                         (plist-get o-op :file))
                                  (eq (plist-get w-op :operation)
                                      (plist-get o-op :operation))))
                           orchestrator-ops)
                  :to-be-truthy))))

    (it "returns same operation set for command with redirection: cat input.txt > output.txt"
      (let* ((parsed (jf/bash-parse "cat input.txt > output.txt"))
             (wrapper-ops (jf/bash-extract-file-operations parsed))
             (orchestrator-result (jf/bash-extract-semantics parsed))
             (orchestrator-ops (alist-get :filesystem
                                         (plist-get orchestrator-result :domains))))
        ;; Wrapper calls the recursive engine directly so gets both read + write.
        ;; Orchestrator should also get both (if Layer 0 isn't discarded).
        ;; This test exposes divergence when the orchestrator loses the redirect.
        (expect (length wrapper-ops) :to-equal (length orchestrator-ops))))

    (it "returns same operation set for chain: rm a.txt && touch b.txt"
      (let* ((parsed (jf/bash-parse "rm a.txt && touch b.txt"))
             (wrapper-ops (jf/bash-extract-file-operations parsed))
             (orchestrator-result (jf/bash-extract-semantics parsed))
             (orchestrator-ops (alist-get :filesystem
                                         (plist-get orchestrator-result :domains))))
        (expect (length wrapper-ops) :to-equal (length orchestrator-ops))))

    (it "returns same operation set for pipeline: cat file.txt | grep pattern > out.txt"
      (let* ((parsed (jf/bash-parse "cat file.txt | grep pattern > out.txt"))
             (wrapper-ops (jf/bash-extract-file-operations parsed))
             (orchestrator-result (jf/bash-extract-semantics parsed))
             (orchestrator-ops (alist-get :filesystem
                                         (plist-get orchestrator-result :domains))))
        (expect (length wrapper-ops) :to-equal (length orchestrator-ops)))))

  (describe "security validator API compatibility"

    ;; The security validator (bash-parser-security.org:478) calls
    ;; jf/bash-extract-file-operations and iterates the result.
    ;; That code expects a flat list of operation plists.
    ;; The orchestrator's :filesystem domain value is also a flat list.
    ;; These tests verify the wrapper's return format matches what the
    ;; security validator expects AND what the orchestrator produces.

    (it "returns a flat list of plists (not nested in :domains)"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-file-operations parsed)))
        (expect (listp result) :to-be-truthy)
        ;; Each element should be a plist with :file and :operation
        (dolist (op result)
          (expect (plist-get op :file) :not :to-be nil)
          (expect (plist-get op :operation) :not :to-be nil))))

    (it "includes :confidence in operations (security validator checks this)"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-file-operations parsed)))
        (dolist (op result)
          (expect (plist-get op :confidence) :not :to-be nil))))

    (it "includes :source in operations (security validator checks :indirect)"
      ;; The security validator checks (plist-get op :indirect) and
      ;; (plist-get op :source). These metadata keys come from the
      ;; recursive engine. If the wrapper delegates to the orchestrator,
      ;; the orchestrator must preserve these keys.
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (result (jf/bash-extract-file-operations parsed)))
        (dolist (op result)
          (expect (plist-get op :source) :not :to-be nil))))))

(provide 'legacy-wrapper-spec)
;;; legacy-wrapper-spec.el ends here
