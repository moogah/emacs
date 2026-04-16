;;; stdout-commands.el --- No-op handlers for pure stdout/builtin commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Registers no-op filesystem handlers for commands that have no filesystem
;; operations of their own: their tokens still need to be claimed for parse
;; coverage accounting, and any file side-effects (like `echo > out.txt') are
;; picked up by the redirection extractor.
;;
;; The handler returns nil — registration alone is the signal. The orchestrator
;; (`jf/bash--claim-tokens-for-operations') checks `jf/bash-command-handlers' to
;; decide which command-name and positional-arg tokens to claim. A registered
;; handler, even one that emits no operations, is enough to mark the command as
;; parse-understood.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-stdout--no-op-handler (_parsed-command)
  "No-op filesystem handler for pure stdout/builtin commands.
Returns nil — registration signals the orchestrator to claim this
command's tokens for coverage. Any file side-effects from redirections
are handled by the redirection extractor, not by per-command handlers."
  nil)

(dolist (cmd '("echo" "printf"
               "true" "false" ":"
               "exit" "return" "break" "continue"
               "shift" "set" "unset"
               "read" "export" "declare" "local" "typeset" "readonly"
               "alias" "unalias" "type" "command" "builtin" "enable"
               "test" "[" "[["
               "pwd" "sleep" "yes"
               "time" "times"
               "eval" "let"))
  (jf/bash-register-command-handler
   :command cmd
   :domain :filesystem
   :handler #'jf/bash-command-stdout--no-op-handler))

(provide 'bash-command-stdout-commands)
;;; stdout-commands.el ends here
