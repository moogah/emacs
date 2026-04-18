;;; chmod.el --- chmod command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-chmod--filesystem-handler (parsed-command)
  "Extract filesystem operations from chmod command.
First positional arg is the mode (skipped), remaining are :modify operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when (and positional-args (> (length positional-args) 1))
      (dolist (arg (cdr positional-args))
        (push (list :file arg :operation :modify :confidence :high :command "chmod") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "chmod"
  :domain :filesystem
  :handler #'jf/bash-command-chmod--filesystem-handler)

(provide 'bash-command-chmod)
;;; chmod.el ends here
