;;; gunzip.el --- gunzip command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-gunzip--filesystem-handler (parsed-command)
  "Extract filesystem operations from gunzip command.
Each positional arg generates a :read and a :write stripping .gz suffix."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "gunzip") operations)
        (let ((output-file (if (string-suffix-p ".gz" arg)
                               (substring arg 0 -3)
                             arg)))
          (push (list :file output-file :operation :write :confidence :high :command "gunzip") operations))))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "gunzip"
  :domain :filesystem
  :handler #'jf/bash-command-gunzip--filesystem-handler)

(provide 'bash-command-gunzip)
;;; gunzip.el ends here
