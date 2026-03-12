;;; tail.el --- tail command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-tail--filesystem-handler (parsed-command)
  "Extract filesystem operations from tail command.
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
          (push (list :file arg :operation :read :confidence :high :command "tail") operations))))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "tail"
  :domain :filesystem
  :handler #'jf/bash-command-tail--filesystem-handler)

(provide 'bash-command-tail)
;;; tail.el ends here
