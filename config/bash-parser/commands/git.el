;;; git.el --- Git command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for git subcommands.
;; Dispatches based on git subcommand to classify file operations.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-git--handle-simple (args operation command)
  "Create operations for all ARGS with OPERATION type.
COMMAND is used for metadata."
  (when args
    (mapcar (lambda (arg)
              (list :file arg :operation operation :confidence :high :command command))
            args)))

(defun jf/bash-command-git--handle-indexed (args index operation command)
  "Create single operation for ARG at INDEX with OPERATION type.
INDEX supports -1 for last element.  COMMAND is used for metadata."
  (when args
    (let ((arg (if (< index 0)
                   (nth (+ (length args) index) args)
                 (nth index args))))
      (when arg
        (list (list :file arg :operation operation :confidence :high :command command))))))

(defun jf/bash-command-git--handle-mv (args command)
  "Create operations for git mv ARGS: sources :delete, dest :write.
COMMAND is used for metadata."
  (when (>= (length args) 2)
    (let ((sources (butlast args))
          (dest (car (last args))))
      (append
       (mapcar (lambda (arg)
                 (list :file arg :operation :delete :confidence :high :command command))
               sources)
       (list (list :file dest :operation :write :confidence :high :command command))))))

(defun jf/bash-command-git--handle-worktree (args command)
  "Create operations for git worktree subcommands.
Only handles 'add' subcommand from ARGS.  COMMAND is used for metadata."
  (when args
    (let ((worktree-sub (car args))
          (worktree-args (cdr args)))
      (when (and (equal worktree-sub "add") worktree-args)
        (list (list :file (car worktree-args) :operation :create :confidence :high :command command))))))

(defun jf/bash-command-git--filesystem-handler (parsed-command)
  "Extract filesystem operations from git PARSED-COMMAND.
Dispatches based on git subcommand to determine file operations.
Uses :subcommand from parsed-command if set by parser, otherwise
falls back to first positional arg as subcommand.
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (explicit-subcommand (plist-get parsed-command :subcommand))
         (subcommand (or explicit-subcommand (car positional-args)))
         (sub-args (if explicit-subcommand
                       positional-args
                     (cdr positional-args)))
         (command "git")
         (ops nil))
    (when subcommand
      (setq ops
            (pcase subcommand
              ("add" (jf/bash-command-git--handle-simple sub-args :read command))
              ("apply" (jf/bash-command-git--handle-indexed sub-args 0 :read command))
              ("checkout" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("clean" (jf/bash-command-git--handle-simple sub-args :delete command))
              ("clone" (jf/bash-command-git--handle-indexed sub-args -1 :write command))
              ("diff" (jf/bash-command-git--handle-simple sub-args :read command))
              ("ls-files" (jf/bash-command-git--handle-simple sub-args :read command))
              ("merge" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("mv" (jf/bash-command-git--handle-mv sub-args command))
              ("pull" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("rebase" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("reset" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("restore" (jf/bash-command-git--handle-simple sub-args :modify command))
              ("rm" (jf/bash-command-git--handle-simple sub-args :delete command))
              ("show" (jf/bash-command-git--handle-simple sub-args :read command))
              ("worktree" (jf/bash-command-git--handle-worktree sub-args command)))))
    (when ops
      (list :domain :filesystem
            :operations ops
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
 :command "git"
 :domain :filesystem
 :handler #'jf/bash-command-git--filesystem-handler)

(provide 'bash-command-git)
;;; git.el ends here
