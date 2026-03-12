;;; bunzip2.el --- bunzip2 command handler -*- lexical-binding: t; -*-
(require 'bash-parser-semantics)

(defun jf/bash-command-bunzip2--filesystem-handler (parsed-command)
  "Extract filesystem operations from bunzip2 command.
Each positional arg generates a :read and a :write stripping .bz2 suffix."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (push (list :file arg :operation :read :confidence :high :command "bunzip2") operations)
        (let ((output-file (if (string-suffix-p ".bz2" arg)
                               (substring arg 0 -4)
                             arg)))
          (push (list :file output-file :operation :write :confidence :high :command "bunzip2") operations))))
    (list :domain :filesystem
          :operations (nreverse operations)
          :claimed-token-ids nil
          :metadata nil)))

(jf/bash-register-command-handler
  :command "bunzip2"
  :domain :filesystem
  :handler #'jf/bash-command-bunzip2--filesystem-handler)

(provide 'bash-command-bunzip2)
;;; bunzip2.el ends here
