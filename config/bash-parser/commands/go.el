;;; go.el --- Go command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for go command.
;; Subcommands: run (:execute), test (:execute), build (:read).

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-go--filesystem-handler (parsed-command)
  "Extract filesystem operations from go PARSED-COMMAND.
Subcommands:
  run  -> :execute (index 0 after subcommand)
  test -> :execute (index 0 after subcommand)
  build -> :read (index 0 after subcommand)
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (explicit-subcommand (plist-get parsed-command :subcommand))
         (subcommand (or explicit-subcommand (car positional-args)))
         (sub-args (if explicit-subcommand positional-args (cdr positional-args)))
         (command "go")
         (ops nil))
    (when (and subcommand sub-args)
      (let ((target (car sub-args)))
        (setq ops
              (pcase subcommand
                ("run" (list (list :file target :operation :execute :confidence :high :command command)))
                ("test" (list (list :file target :operation :execute :confidence :high :command command)))
                ("build" (list (list :file target :operation :read :confidence :high :command command)))))))
    (when ops
      (list :domain :filesystem
            :operations ops
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
 :command "go"
 :domain :filesystem
 :handler #'jf/bash-command-go--filesystem-handler)

(provide 'bash-command-go)
;;; go.el ends here
