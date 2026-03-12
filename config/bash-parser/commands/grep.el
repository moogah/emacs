;;; grep.el --- grep/egrep/fgrep command handler -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

(defun jf/bash-command-grep--filesystem-handler (parsed-command)
  "Extract filesystem operations from grep/egrep/fgrep command.
With -l/--files-with-matches flag, file args are :match-pattern operations.
Without -l, file args are :read operations.
First positional arg (the pattern) is always skipped."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (flags (plist-get parsed-command :flags))
        (operations nil))
    (let ((file-args (cdr positional-args)))
      (when file-args
        (let ((op-type (if (or (member "-l" flags) (member "--files-with-matches" flags))
                          :match-pattern
                        :read)))
          (dolist (arg file-args)
            (push (list :file arg :operation op-type :confidence :high :command (plist-get parsed-command :command-name)) operations)))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(dolist (cmd '("grep" "egrep" "fgrep"))
  (jf/bash-register-command-handler
    :command cmd
    :domain :filesystem
    :handler #'jf/bash-command-grep--filesystem-handler))

(provide 'bash-command-grep)
;;; grep.el ends here
