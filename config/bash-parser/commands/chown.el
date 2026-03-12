;;; chown.el --- chown command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-chown--filesystem-handler (parsed-command)
  "Extract filesystem operations from chown command.
First positional arg is the owner (skipped), remaining are :modify operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when (and positional-args (> (length positional-args) 1))
      (dolist (arg (cdr positional-args))
        (push (list :file arg :operation :modify :confidence :high :command "chown") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "chown"
  :domain :filesystem
  :handler #'jf/bash-command-chown--filesystem-handler)

(provide 'bash-command-chown)
;;; chown.el ends here
