;;; exec.el --- exec command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-exec--filesystem-handler (parsed-command)
  "Extract filesystem operations from exec command.
First positional arg (index 0) is an :execute operation."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (push (list :file (car positional-args) :operation :execute :confidence :high :command "exec") operations))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "exec"
  :domain :filesystem
  :handler #'jf/bash-command-exec--filesystem-handler)

(provide 'bash-command-exec)
;;; exec.el ends here
