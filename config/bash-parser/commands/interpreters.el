;;; interpreters.el --- Interpreter command handlers -*- lexical-binding: t; -*-

(require 'bash-parser-semantics)

(defvar jf/bash-command-interpreter--inline-flags
  '(("python" . ("-c" "-m"))
    ("python3" . ("-c" "-m"))
    ("node" . ("-e" "--eval" "-p" "--print"))
    ("bash" . ("-c"))
    ("sh" . ("-c"))
    ("zsh" . ("-c"))
    ("ruby" . ("-e"))
    ("perl" . ("-e" "-E"))
    ("php" . ("-r")))
  "Alist mapping interpreter commands to their inline execution flags.
When any of these flags are present, the command executes inline code
rather than a script file, so no file operation is extracted.")

(defun jf/bash-command-interpreter--filesystem-handler (parsed-command)
  "Extract filesystem operations from interpreter commands.
If an inline execution flag is present, return nil (no file operation).
Otherwise, first positional arg is an :execute operation."
  (let* ((cmd (plist-get parsed-command :command-name))
         (flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (inline-flags (cdr (assoc cmd jf/bash-command-interpreter--inline-flags))))
    (if (seq-some (lambda (f) (member f flags)) inline-flags)
        nil
      (when positional-args
        (list :domain :filesystem
              :operations (list (list :file (car positional-args)
                                     :operation :execute
                                     :confidence :high
                                     :command cmd))
              :claimed-token-ids nil
              :metadata nil)))))

(dolist (cmd '("python" "python3" "node" "bash" "sh" "zsh" "ruby" "perl" "php"))
  (jf/bash-register-command-handler
    :command cmd
    :domain :filesystem
    :handler #'jf/bash-command-interpreter--filesystem-handler))

(provide 'bash-command-interpreters)
;;; interpreters.el ends here
