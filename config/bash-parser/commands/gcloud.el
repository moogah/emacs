;;; gcloud.el --- Multi-domain command handlers for Google Cloud CLI -*- lexical-binding: t; -*-

;;; Commentary:

;; Google Cloud CLI contributes to two semantic domains:
;; - :authentication - project and account detection
;; - :network - implicit HTTPS access to googleapis.com

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-gcloud--auth-handler (parsed-command)
  "Extract authentication context from gcloud commands.

Detects --project and --account flags.

PARSED-COMMAND is the parsed command plist.
Returns authentication domain result."
  (let* ((args (plist-get parsed-command :args))
         (project nil)
         (account nil)
         (context nil)
         (i 0))
    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ((equal arg "--project")
          (when (< (1+ i) (length args))
            (setq project (nth (1+ i) args))
            (setq i (1+ i))))
         ((equal arg "--account")
          (when (< (1+ i) (length args))
            (setq account (nth (1+ i) args))
            (setq i (1+ i))))))
      (setq i (1+ i)))
    (when project
      (push (cons :project project) context))
    (when account
      (push (cons :account account) context))
    (list :domain :authentication
          :operations (list (append (list :provider :gcloud
                                          :command "gcloud")
                                   (when context
                                     (list :context context))))
          :claimed-token-ids nil
          :metadata nil)))

(defun jf/bash-command-gcloud--network-handler (_parsed-command)
  "Extract network access from gcloud commands.

Any gcloud command implies HTTPS access to googleapis.com.

PARSED-COMMAND is the parsed command plist.
Returns network domain result."
  (list :domain :network
        :operations (list (list :protocol :https
                                :endpoint "googleapis.com"
                                :command "gcloud"))
        :claimed-token-ids nil
        :metadata nil))

;; Register both domain handlers
(jf/bash-register-command-handler
 :command "gcloud" :domain :authentication :handler #'jf/bash-command-gcloud--auth-handler)
(jf/bash-register-command-handler
 :command "gcloud" :domain :network :handler #'jf/bash-command-gcloud--network-handler)

(provide 'bash-command-gcloud)
;;; gcloud.el ends here
