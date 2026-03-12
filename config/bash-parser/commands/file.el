;;; file.el --- file command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-file--filesystem-handler (parsed-command)
  "Extract filesystem operations from file command.
All positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "file") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "file"
  :domain :filesystem
  :handler #'jf/bash-command-file--filesystem-handler)

(provide 'bash-command-file)
;;; file.el ends here
