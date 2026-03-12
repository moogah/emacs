;;; touch.el --- touch command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-touch--filesystem-handler (parsed-command)
  "Extract filesystem operations from touch command.
All positional args are create-or-modify operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :create-or-modify :confidence :high :command "touch") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "touch"
  :domain :filesystem
  :handler #'jf/bash-command-touch--filesystem-handler)

(provide 'bash-command-touch)
;;; touch.el ends here
