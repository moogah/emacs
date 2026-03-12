;;; dirname.el --- dirname command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-dirname--filesystem-handler (parsed-command)
  "Extract filesystem operations from dirname command.
All positional args are read-metadata operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read-metadata :confidence :high :command "dirname") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "dirname"
  :domain :filesystem
  :handler #'jf/bash-command-dirname--filesystem-handler)

(provide 'bash-command-dirname)
;;; dirname.el ends here
