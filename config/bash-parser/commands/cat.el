;;; cat.el --- cat command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-cat--filesystem-handler (parsed-command)
  "Extract filesystem operations from cat command.
All positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "cat") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "cat"
  :domain :filesystem
  :handler #'jf/bash-command-cat--filesystem-handler)

(provide 'bash-command-cat)
;;; cat.el ends here
