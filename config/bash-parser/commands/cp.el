;;; cp.el --- cp command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-cp--filesystem-handler (parsed-command)
  "Extract filesystem operations from cp command.
Sources (indices 0..-2) are :read, destination (last arg) is :write."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (tokens (plist-get parsed-command :tokens))
        (operations nil)
        (claimed-ids nil))
    (when (and positional-args (> (length positional-args) 1))
      (let ((sources (butlast positional-args))
            (dest (car (last positional-args))))
        (dolist (src sources)
          (push (list :file src :operation :read :confidence :high :command "cp") operations))
        (push (list :file dest :operation :write :confidence :high :command "cp") operations)))
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
  :command "cp"
  :domain :filesystem
  :handler #'jf/bash-command-cp--filesystem-handler)

(provide 'bash-command-cp)
;;; cp.el ends here
