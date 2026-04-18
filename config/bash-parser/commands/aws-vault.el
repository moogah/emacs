;;; aws-vault.el --- Command handler for aws-vault credential wrapper -*- lexical-binding: t; -*-

;;; Commentary:

;; aws-vault wraps AWS commands with temporary credentials from a vault.
;; Typical usage: aws-vault exec PROFILE -- aws s3 ls
;;
;; Contributes to:
;; - :authentication - profile detection from exec/login subcommands

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-aws-vault--auth-handler (parsed-command)
  "Extract authentication context from aws-vault commands.

Detects the profile name from subcommands like exec, login.
Usage: aws-vault exec PROFILE [-- CMD...]
       aws-vault login PROFILE

PARSED-COMMAND is the parsed command plist.
Returns authentication domain result."
  (let* ((args (plist-get parsed-command :args))
         (profile nil)
         (subcommand nil)
         (context nil)
         (i 0))
    ;; Walk args to find: exec/login PROFILE
    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ;; Skip flags
         ((string-prefix-p "-" arg)
          (when (and (string-prefix-p "--" arg)
                     (not (string= arg "--")))
            (setq i (1+ i))))
         ;; Stop at -- separator
         ((string= arg "--")
          (setq i (length args)))
         ;; First positional is subcommand
         ((null subcommand)
          (setq subcommand arg))
         ;; Second positional after exec/login is the profile
         ((and (member subcommand '("exec" "login"))
               (null profile))
          (setq profile arg))))
      (setq i (1+ i)))
    (when profile
      (push (cons :profile profile) context))
    (list :domain :authentication
          :operations (list (append (list :provider :aws-vault
                                          :command "aws-vault")
                                   (when context
                                     (list :context context))))
          :claimed-token-ids nil
          :metadata nil)))

;; Register authentication handler
(jf/bash-register-command-handler
 :command "aws-vault" :domain :authentication :handler #'jf/bash-command-aws-vault--auth-handler)

(provide 'bash-command-aws-vault)
;;; aws-vault.el ends here
