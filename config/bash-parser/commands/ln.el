;;; ln.el --- ln command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-ln--filesystem-handler (parsed-command)
  "Extract filesystem operations from ln command.
Sources (indices 0..-2) are :read, destination (last arg) is :write."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when (and positional-args (> (length positional-args) 1))
      (let ((sources (butlast positional-args))
            (dest (car (last positional-args))))
        (dolist (src sources)
          (push (list :file src :operation :read :confidence :high :command "ln") operations))
        (push (list :file dest :operation :write :confidence :high :command "ln") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "ln"
  :domain :filesystem
  :handler #'jf/bash-command-ln--filesystem-handler)

(provide 'bash-command-ln)
;;; ln.el ends here
