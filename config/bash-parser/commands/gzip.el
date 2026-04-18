;;; gzip.el --- gzip command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-gzip--filesystem-handler (parsed-command)
  "Extract filesystem operations from gzip command.
Each positional arg generates a :read and a :write with .gz suffix."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "gzip") operations)
        (push (list :file (concat arg ".gz") :operation :write :confidence :high :command "gzip") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "gzip"
  :domain :filesystem
  :handler #'jf/bash-command-gzip--filesystem-handler)

(provide 'bash-command-gzip)
;;; gzip.el ends here
