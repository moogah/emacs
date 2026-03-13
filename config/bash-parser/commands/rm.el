;;; rm.el --- rm command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-rm--filesystem-handler (parsed-command)
  "Extract filesystem operations from rm command.
All positional args are delete operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (tokens (plist-get parsed-command :tokens))
        (operations nil)
        (claimed-ids nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :delete :confidence :high :command "rm") operations)))
    ;; Claim token IDs: command-name + all positional-arg tokens
    (when (and operations tokens)
      (dolist (token tokens)
        (when (memq (plist-get token :type) '(:command-name :positional-arg))
          (when-let ((id (plist-get token :id)))
            (push id claimed-ids)))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids (nreverse claimed-ids)
            :metadata nil))))

(jf/bash-register-command-handler
  :command "rm"
  :domain :filesystem
  :handler #'jf/bash-command-rm--filesystem-handler)

(provide 'bash-command-rm)
;;; rm.el ends here
