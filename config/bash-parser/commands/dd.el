;;; dd.el --- dd command handler -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

(defun jf/bash-command-dd--filesystem-handler (parsed-command)
  "Extract filesystem operations from dd command.
Parses named arguments: if=source becomes :read, of=dest becomes :write.
Arguments are found in positional-args list with \"if=\" and \"of=\" prefixes."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (dolist (arg positional-args)
      (cond
       ((string-prefix-p "if=" arg)
        (let ((source (substring arg 3)))
          (when (> (length source) 0)
            (push (list :file source :operation :read :confidence :high :command "dd") operations))))
       ((string-prefix-p "of=" arg)
        (let ((dest (substring arg 3)))
          (when (> (length dest) 0)
            (push (list :file dest :operation :write :confidence :high :command "dd") operations))))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "dd"
  :domain :filesystem
  :handler #'jf/bash-command-dd--filesystem-handler)

(provide 'bash-command-dd)
;;; dd.el ends here
