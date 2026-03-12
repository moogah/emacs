;;; tee.el --- tee command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-tee--filesystem-handler (parsed-command)
  "Extract filesystem operations from tee command.
With -a/--append flags, operations are :append; without, :write."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (flags (plist-get parsed-command :flags))
        (operations nil)
        (op-type :write))
    ;; Check for append flags
    (dolist (flag flags)
      (when (or (string= flag "-a") (string= flag "--append"))
        (setq op-type :append)))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation op-type :confidence :high :command "tee") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "tee"
  :domain :filesystem
  :handler #'jf/bash-command-tee--filesystem-handler)

(provide 'bash-command-tee)
;;; tee.el ends here
