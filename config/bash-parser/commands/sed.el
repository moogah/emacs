;;; sed.el --- sed command handler -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

(defun jf/bash-command-sed--filesystem-handler (parsed-command)
  "Extract filesystem operations from sed command.
With -i/--in-place flag, file args are :modify operations.
Without -i, file args are :read operations.
First positional arg (sed expression) is always skipped."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (flags (plist-get parsed-command :flags))
        (operations nil))
    (let ((file-args (cdr positional-args)))
      (when file-args
        (let ((op-type (if (or (member "-i" flags) (member "--in-place" flags))
                          :modify
                        :read)))
          (dolist (arg file-args)
            (push (list :file arg :operation op-type :confidence :high :command "sed") operations)))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "sed"
  :domain :filesystem
  :handler #'jf/bash-command-sed--filesystem-handler)

(provide 'bash-command-sed)
;;; sed.el ends here
