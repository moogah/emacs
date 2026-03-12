;;; stat.el --- stat command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-stat--filesystem-handler (parsed-command)
  "Extract filesystem operations from stat command.
All positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "stat") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "stat"
  :domain :filesystem
  :handler #'jf/bash-command-stat--filesystem-handler)

(provide 'bash-command-stat)
;;; stat.el ends here
