;;; extraction-gaps-spec.el --- Layer 2: Direct extraction verification -*- lexical-binding: t; -*-

;; Tests extraction functions directly to isolate orchestrator wiring issues.
;; Calls handlers and the recursive engine without going through the orchestrator.

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))
(require 'bash-parser-orchestrator)

;; Load command handlers
(let ((commands-dir (expand-file-name "config/bash-parser/commands/" jf/emacs-dir)))
  (load-file (expand-file-name "ls.el" commands-dir)))

(describe "Layer 2: direct extraction"

  (describe "ls command handler"
    (it "produces :read-directory for plain path /tmp"
      (let ((result (jf/bash-command-ls--filesystem-handler
                     '(:command-name "ls" :positional-args ("/tmp")))))
        (expect result :not :to-be nil)
        (expect (plist-get result :domain) :to-equal :filesystem)
        (let ((ops (plist-get result :operations)))
          (expect (length ops) :to-equal 1)
          (expect (plist-get (car ops) :operation) :to-equal :read-directory)
          (expect (plist-get (car ops) :file) :to-equal "/tmp")))))

  (describe "recursive engine (direct call)"
    (it "extracts :write for echo hello > /tmp/out.txt"
      (let* ((parsed (jf/bash-parse "echo hello > /tmp/out.txt"))
             (ops (jf/bash--extract-file-operations-impl parsed nil)))
        (expect ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (eq (plist-get op :operation) :write)
                                (equal (plist-get op :file) "/tmp/out.txt")))
                         ops)
                :to-be-truthy)))

    (it "extracts :append for echo data >> /tmp/log.txt"
      (let* ((parsed (jf/bash-parse "echo data >> /tmp/log.txt"))
             (ops (jf/bash--extract-file-operations-impl parsed nil)))
        (expect ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (eq (plist-get op :operation) :append)
                                (equal (plist-get op :file) "/tmp/log.txt")))
                         ops)
                :to-be-truthy)))))

(provide 'extraction-gaps-spec)
;;; extraction-gaps-spec.el ends here
