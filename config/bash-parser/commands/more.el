;;; more.el --- more command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-more--filesystem-handler (parsed-command)
  "Extract filesystem operations from more command.
All positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "more") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "more"
  :domain :filesystem
  :handler #'jf/bash-command-more--filesystem-handler)

(provide 'bash-command-more)
;;; more.el ends here
