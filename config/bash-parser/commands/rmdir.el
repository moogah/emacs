;;; rmdir.el --- rmdir command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-rmdir--filesystem-handler (parsed-command)
  "Extract filesystem operations from rmdir command.
All positional args are delete operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :delete :confidence :high :command "rmdir") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "rmdir"
  :domain :filesystem
  :handler #'jf/bash-command-rmdir--filesystem-handler)

(provide 'bash-command-rmdir)
;;; rmdir.el ends here
