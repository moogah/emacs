;;; chgrp.el --- chgrp command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-chgrp--filesystem-handler (parsed-command)
  "Extract filesystem operations from chgrp command.
First positional arg is the group (skipped), remaining are :modify operations."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when (and positional-args (> (length positional-args) 1))
      (dolist (arg (cdr positional-args))
        (push (list :file arg :operation :modify :confidence :high :command "chgrp") operations)))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "chgrp"
  :domain :filesystem
  :handler #'jf/bash-command-chgrp--filesystem-handler)

(provide 'bash-command-chgrp)
;;; chgrp.el ends here
