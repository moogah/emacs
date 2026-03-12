;;; source.el --- source command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-source--filesystem-handler (parsed-command)
  "Extract filesystem operations from source command.
First positional arg (index 0) is an :execute operation."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (push (list :file (car positional-args) :operation :execute :confidence :high :command "source") operations))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "source"
  :domain :filesystem
  :handler #'jf/bash-command-source--filesystem-handler)

(jf/bash-register-command-handler
  :command "."
  :domain :filesystem
  :handler #'jf/bash-command-source--filesystem-handler)

(provide 'bash-command-source)
;;; source.el ends here
