;;; mkdir.el --- mkdir command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-mkdir--filesystem-handler (parsed-command)
  "Extract filesystem operations from mkdir command.
All positional args are create operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :create :confidence :high :command "mkdir") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "mkdir"
  :domain :filesystem
  :handler #'jf/bash-command-mkdir--filesystem-handler)

(provide 'bash-command-mkdir)
;;; mkdir.el ends here
