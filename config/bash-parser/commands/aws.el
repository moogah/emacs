;;; aws.el --- Multi-domain command handlers for AWS CLI -*- lexical-binding: t; -*-

;;; Commentary:

;; AWS CLI contributes to three semantic domains:
;; - :filesystem - local file paths in s3 cp/sync/mv operations
;; - :authentication - profile and region detection
;; - :network - implicit HTTPS network access

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-aws--filesystem-handler (parsed-command)
  "Extract filesystem operations from AWS S3 commands.

Detects local file paths in s3 cp, sync, and mv operations.
Paths starting with \"s3://\" are remote and skipped.

PARSED-COMMAND is the parsed command plist.
Returns filesystem domain result or nil if no local file ops."
  (let* ((args (plist-get parsed-command :args))
         (operations nil)
         (subcommand nil)
         (s3-op nil)
         (source nil)
         (dest nil))
    ;; Walk args to find: aws s3 <op> <source> <dest>
    ;; Skip flags and their values
    (let ((i 0)
          (positionals nil))
      (while (< i (length args))
        (let ((arg (nth i args)))
          (cond
           ;; Skip flags and their values
           ((string-prefix-p "--" arg)
            (setq i (1+ i)))  ; skip flag value on next iteration
           ((and (string-prefix-p "-" arg)
                 (not (string-prefix-p "--" arg))
                 (> (length arg) 1))
            ;; Short flag, might consume next arg - skip conservatively
            nil)
           (t
            (push arg positionals))))
        (setq i (1+ i)))
      (setq positionals (nreverse positionals))
      ;; positionals should be: ("aws" "s3" "cp" "source" "dest") or similar
      ;; But :args doesn't include the command name itself typically
      ;; Actually from parsed-command, :args is all tokens after command name
      ;; So positionals = ("s3" "cp" "source" "dest")
      (when (>= (length positionals) 2)
        (setq subcommand (nth 0 positionals))
        (setq s3-op (nth 1 positionals)))
      (when (>= (length positionals) 4)
        (setq source (nth 2 positionals))
        (setq dest (nth 3 positionals)))
      ;; For sync, source might be at index 2 and dest at 3
      (when (and (>= (length positionals) 3)
                 (not source))
        (setq source (nth 2 positionals))))

    (when (and (equal subcommand "s3")
               (member s3-op '("cp" "sync" "mv")))
      ;; Determine operations based on source and dest
      (when (and source (not (string-prefix-p "s3://" source)))
        (let ((op (if (equal s3-op "mv") :delete :read)))
          (push (list :file source
                      :operation op
                      :confidence :high
                      :command "aws")
                operations)))
      (when (and dest (not (string-prefix-p "s3://" dest)))
        (push (list :file dest
                    :operation :write
                    :confidence :high
                    :command "aws")
              operations)))

    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(defun jf/bash-command-aws--auth-handler (parsed-command)
  "Extract authentication context from AWS commands.

Detects --profile and --region flags.

PARSED-COMMAND is the parsed command plist.
Returns authentication domain result."
  (let* ((args (plist-get parsed-command :args))
         (profile nil)
         (region nil)
         (context nil)
         (i 0))
    ;; Walk args looking for --profile VALUE and --region VALUE
    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ((equal arg "--profile")
          (when (< (1+ i) (length args))
            (setq profile (nth (1+ i) args))
            (setq i (1+ i))))
         ((equal arg "--region")
          (when (< (1+ i) (length args))
            (setq region (nth (1+ i) args))
            (setq i (1+ i))))))
      (setq i (1+ i)))
    (when profile
      (push (cons :profile profile) context))
    (when region
      (push (cons :region region) context))
    (list :domain :authentication
          :operations (list (append (list :provider :aws
                                          :command "aws")
                                   (when context
                                     (list :context context))))
          :claimed-token-ids nil
          :metadata nil)))

(defun jf/bash-command-aws--network-handler (_parsed-command)
  "Extract network access from AWS commands.

Any AWS command implies HTTPS access to amazonaws.com.

PARSED-COMMAND is the parsed command plist.
Returns network domain result."
  (list :domain :network
        :operations (list (list :protocol :https
                                :endpoint "amazonaws.com"
                                :command "aws"))
        :claimed-token-ids nil
        :metadata nil))

;; Register all three domain handlers
(jf/bash-register-command-handler
 :command "aws" :domain :filesystem :handler #'jf/bash-command-aws--filesystem-handler)
(jf/bash-register-command-handler
 :command "aws" :domain :authentication :handler #'jf/bash-command-aws--auth-handler)
(jf/bash-register-command-handler
 :command "aws" :domain :network :handler #'jf/bash-command-aws--network-handler)

(provide 'bash-command-aws)
;;; aws.el ends here
