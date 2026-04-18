;;; mv.el --- mv command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-mv--filesystem-handler (parsed-command)
  "Extract filesystem operations from mv command.
Sources (indices 0..-2) are :delete, destination (last arg) is :write."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when (and positional-args (> (length positional-args) 1))
      (let ((sources (butlast positional-args))
            (dest (car (last positional-args))))
        (dolist (src sources)
          (push (list :file src :operation :delete :confidence :high :command "mv") operations))
        (push (list :file dest :operation :write :confidence :high :command "mv") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "mv"
  :domain :filesystem
  :handler #'jf/bash-command-mv--filesystem-handler)

(provide 'bash-command-mv)
;;; mv.el ends here
