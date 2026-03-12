;;; head.el --- head command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-head--filesystem-handler (parsed-command)
  "Extract filesystem operations from head command.
Flags -n and -c consume the next positional arg (a number), so those are skipped.
Remaining positional args are read operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (flags (plist-get parsed-command :flags))
        (operations nil)
        (skip-count 0))
    ;; Count how many positional args are consumed by -n/-c flags
    (dolist (flag flags)
      (when (or (string= flag "-n") (string= flag "-c"))
        (setq skip-count (1+ skip-count))))
    ;; Skip the first skip-count positional args (they are numeric values for flags)
    (when positional-args
      (let ((args-to-process (nthcdr skip-count positional-args)))
        (dolist (arg args-to-process)
          (push (list :file arg :operation :read :confidence :high :command "head") operations))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "head"
  :domain :filesystem
  :handler #'jf/bash-command-head--filesystem-handler)

(provide 'bash-command-head)
;;; head.el ends here
