;;; bzip2.el --- bzip2 command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-bzip2--filesystem-handler (parsed-command)
  "Extract filesystem operations from bzip2 command.
Each positional arg generates a :read and a :write with .bz2 suffix."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "bzip2") operations)
        (push (list :file (concat arg ".bz2") :operation :write :confidence :high :command "bzip2") operations)))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "bzip2"
  :domain :filesystem
  :handler #'jf/bash-command-bzip2--filesystem-handler)

(provide 'bash-command-bzip2)
;;; bzip2.el ends here
