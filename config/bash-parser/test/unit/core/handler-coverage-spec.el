;;; handler-coverage-spec.el --- Tests that command handlers contribute to coverage -*- lexical-binding: t; -*-

;;; Commentary:

;; Command handlers are the sole mechanism for domain-specific extraction
;; in the two-layer architecture. When a handler claims operations, it
;; should also claim the tokens it understood so that coverage accurately
;; reflects how much of the command was semantically analyzed.
;;
;; Current state: All 45 command handlers return :claimed-token-ids nil.
;; This means coverage is always 0% for handler-contributed operations.
;; For a security system, this creates a false signal — operations are
;; found but coverage says "we understood nothing about this command."
;;
;; These tests verify that handlers contribute meaningful coverage data.

;;; Code:

(require 'cl-lib)
(require 'bash-parser-orchestrator)
(require 'bash-parser-semantics)
(require 'bash-parser)

(defvar handler-coverage--saved-handlers nil)

(describe "Handler coverage contribution"

  (before-each
    (setq handler-coverage--saved-handlers jf/bash-command-handlers)
    (setq jf/bash-command-handlers (make-hash-table :test 'equal))
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (load index-path nil t)))

  (after-each
    (setq jf/bash-command-handlers handler-coverage--saved-handlers))

  (describe "handlers return non-nil claimed-token-ids"

    ;; These tests check the handler return values directly,
    ;; not through the orchestrator. This isolates the handler
    ;; contract from the orchestrator's bugs.

    (it "cat handler claims command-name and positional-arg tokens"
      (let* ((parsed (jf/bash-parse "cat file.txt"))
             (handlers (jf/bash-lookup-command-handlers "cat"))
             (fs-handlers (gethash :filesystem handlers))
             (result (funcall (car fs-handlers) parsed)))
        (expect result :not :to-be nil)
        (expect (plist-get result :claimed-token-ids) :not :to-be nil)))

    (it "rm handler claims command-name and positional-arg tokens"
      (let* ((parsed (jf/bash-parse "rm file.txt"))
             (handlers (jf/bash-lookup-command-handlers "rm"))
             (fs-handlers (gethash :filesystem handlers))
             (result (funcall (car fs-handlers) parsed)))
        (expect result :not :to-be nil)
        (expect (plist-get result :claimed-token-ids) :not :to-be nil)))

    (it "cp handler claims command-name and positional-arg tokens"
      (let* ((parsed (jf/bash-parse "cp src.txt dst.txt"))
             (handlers (jf/bash-lookup-command-handlers "cp"))
             (fs-handlers (gethash :filesystem handlers))
             (result (funcall (car fs-handlers) parsed)))
        (expect result :not :to-be nil)
        (expect (plist-get result :claimed-token-ids) :not :to-be nil))))

  (describe "orchestrator reports non-zero coverage from handlers"

    ;; When Layer 0 is the sole contributor (no cl-return bug), coverage
    ;; comes from Layer 0's token claiming. But when handlers are the
    ;; sole contributor (Layer 0 discarded, or for non-filesystem domains),
    ;; coverage must come from handlers.

    (it "gcloud produces non-zero coverage for :authentication domain"
      ;; gcloud has no filesystem handler, so Layer 0 provides no
      ;; positional-arg operations (no cl-return bug triggered).
      ;; But the authentication handler must claim its tokens.
      (let* ((parsed (jf/bash-parse "gcloud compute instances list"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        ;; gcloud has 4 tokens; at least the command-name should be claimed
        (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)))

    (it "aws produces non-zero coverage for multi-domain command"
      (let* ((parsed (jf/bash-parse "aws s3 ls"))
             (result (jf/bash-extract-semantics parsed))
             (coverage (plist-get result :coverage)))
        (expect (plist-get coverage :claimed-tokens) :to-be-greater-than 0)))))

(provide 'handler-coverage-spec)
;;; handler-coverage-spec.el ends here
