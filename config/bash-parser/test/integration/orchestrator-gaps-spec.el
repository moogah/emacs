;;; orchestrator-gaps-spec.el --- Layer 3: Orchestrator wiring verification -*- lexical-binding: t; -*-

;; Tests jf/bash-extract-semantics end-to-end to verify the three-layer
;; architecture: unconditional grammar extraction, domain plugins, and
;; command handler dispatch.

;;; Code:

(require 'cl-lib)
(require 'bash-parser-plugins)
(require 'bash-parser-semantics)

(describe "Layer 3: orchestrator wiring"

  (before-all
    ;; Ensure command handlers are loaded (may have been cleared by other tests)
    (let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
      (when (file-exists-p index-path)
        (load index-path nil t))))

  (it "ls command handler is registered"
    (expect (jf/bash-lookup-command-handlers "ls") :not :to-be nil))

  (describe "jf/bash-extract-semantics round-trip"

    (it "extracts :write for echo hello > /tmp/out.txt"
      (let* ((parsed (jf/bash-parse "echo hello > /tmp/out.txt"))
             (semantics (jf/bash-extract-semantics parsed))
             (file-ops (alist-get :filesystem (plist-get semantics :domains))))
        (expect file-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (eq (plist-get op :operation) :write))
                         file-ops)
                :to-be-truthy)))

    (it "extracts :append for echo data >> /tmp/log.txt"
      (let* ((parsed (jf/bash-parse "echo data >> /tmp/log.txt"))
             (semantics (jf/bash-extract-semantics parsed))
             (file-ops (alist-get :filesystem (plist-get semantics :domains))))
        (expect file-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (eq (plist-get op :operation) :append))
                         file-ops)
                :to-be-truthy)))

    (it "extracts :read-directory for ls /tmp"
      (let* ((parsed (jf/bash-parse "ls /tmp"))
             (semantics (jf/bash-extract-semantics parsed))
             (file-ops (alist-get :filesystem (plist-get semantics :domains))))
        (expect file-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (eq (plist-get op :operation) :read-directory))
                         file-ops)
                :to-be-truthy)))))

(provide 'orchestrator-gaps-spec)
;;; orchestrator-gaps-spec.el ends here
