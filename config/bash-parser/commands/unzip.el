;;; unzip.el --- unzip command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-unzip--filesystem-handler (parsed-command)
  "Extract filesystem operations from unzip command.
All positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "unzip") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "unzip"
  :domain :filesystem
  :handler #'jf/bash-command-unzip--filesystem-handler)

(provide 'bash-command-unzip)
;;; unzip.el ends here
