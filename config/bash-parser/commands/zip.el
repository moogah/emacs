;;; zip.el --- zip command handler -*- lexical-binding: t; -*-

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-zip--filesystem-handler (parsed-command)
  "Extract filesystem operations from zip command.
First arg is the zip file (:write), remaining args are source files (:read)."
  (let ((positional-args (plist-get parsed-command :positional-args))
        (operations nil))
    (when positional-args
      (let ((zip-file (car positional-args))
            (sources (cdr positional-args)))
        (push (list :file zip-file :operation :write :confidence :high :command "zip") operations)
        (dolist (src sources)
          (push (list :file src :operation :read :confidence :high :command "zip") operations))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
  :command "zip"
  :domain :filesystem
  :handler #'jf/bash-command-zip--filesystem-handler)

(provide 'bash-command-zip)
;;; zip.el ends here
