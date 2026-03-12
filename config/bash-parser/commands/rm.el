;;; rm.el --- rm command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-rm--filesystem-handler (parsed-command)
  "Extract filesystem operations from rm command.
All positional args are delete operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :delete :confidence :high :command "rm") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "rm"
  :domain :filesystem
  :handler #'jf/bash-command-rm--filesystem-handler)

(provide 'bash-command-rm)
;;; rm.el ends here
