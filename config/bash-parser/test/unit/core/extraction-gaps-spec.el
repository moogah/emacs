;;; extraction-gaps-spec.el --- Direct extraction verification -*- lexical-binding: t; -*-

;; Tests extraction functions to verify orchestrator handles all cases.

;;; Code:

(require 'test-helper (expand-file-name "../../test-helper.el"
                                        (file-name-directory
                                         (or load-file-name buffer-file-name))))
(require 'bash-parser-orchestrator)

;; Load command handlers
(let ((commands-dir (expand-file-name "config/bash-parser/commands/" jf/emacs-dir)))
  (load-file (expand-file-name "ls.el" commands-dir)))

(defun extraction-gaps-test--get-filesystem-ops (result)
  "Extract :filesystem operations from orchestrator RESULT."
  (let ((domains (plist-get result :domains)))
    (cdr (assq :filesystem domains))))

(describe "Direct extraction"

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

  (describe "orchestrator extraction"
    (it "extracts :write for echo hello > /tmp/out.txt"
      (let* ((parsed (jf/bash-parse "echo hello > /tmp/out.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (extraction-gaps-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (eq (plist-get op :operation) :write)
                                (equal (plist-get op :file) "/tmp/out.txt")))
                         fs-ops)
                :to-be-truthy)))

    (it "extracts :append for echo data >> /tmp/log.txt"
      (let* ((parsed (jf/bash-parse "echo data >> /tmp/log.txt"))
             (result (jf/bash-extract-semantics parsed))
             (fs-ops (extraction-gaps-test--get-filesystem-ops result)))
        (expect fs-ops :not :to-be nil)
        (expect (cl-some (lambda (op)
                           (and (eq (plist-get op :operation) :append)
                                (equal (plist-get op :file) "/tmp/log.txt")))
                         fs-ops)
                :to-be-truthy)))))

(provide 'extraction-gaps-spec)
;;; extraction-gaps-spec.el ends here
