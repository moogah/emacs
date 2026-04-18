;;; az.el --- Multi-domain command handlers for Azure CLI -*- lexical-binding: t; -*-

;;; Commentary:

;; Azure CLI contributes to two semantic domains:
;; - :authentication - subscription and resource group detection
;; - :network - implicit HTTPS access to azure.com

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-az--auth-handler (parsed-command)
  "Extract authentication context from az commands.

Detects --subscription and --resource-group flags.

PARSED-COMMAND is the parsed command plist.
Returns authentication domain result."
  (let* ((args (plist-get parsed-command :args))
         (subscription nil)
         (resource-group nil)
         (context nil)
         (i 0))
    (while (< i (length args))
      (let ((arg (nth i args)))
        (cond
         ((equal arg "--subscription")
          (when (< (1+ i) (length args))
            (setq subscription (nth (1+ i) args))
            (setq i (1+ i))))
         ((or (equal arg "--resource-group")
              (equal arg "-g"))
          (when (< (1+ i) (length args))
            (setq resource-group (nth (1+ i) args))
            (setq i (1+ i))))))
      (setq i (1+ i)))
    (when subscription
      (push (cons :subscription subscription) context))
    (when resource-group
      (push (cons :resource-group resource-group) context))
    (list :domain :authentication
          :operations (list (append (list :provider :azure
                                          :command "az")
                                   (when context
                                     (list :context context))))
          :claimed-token-ids nil
          :metadata nil)))

(defun jf/bash-command-az--network-handler (_parsed-command)
  "Extract network access from az commands.

Any az command implies HTTPS access to azure.com.

PARSED-COMMAND is the parsed command plist.
Returns network domain result."
  (list :domain :network
        :operations (list (list :protocol :https
                                :endpoint "azure.com"
                                :command "az"))
        :claimed-token-ids nil
        :metadata nil))

;; Register both domain handlers
(jf/bash-register-command-handler
 :command "az" :domain :authentication :handler #'jf/bash-command-az--auth-handler)
(jf/bash-register-command-handler
 :command "az" :domain :network :handler #'jf/bash-command-az--network-handler)

(provide 'bash-command-az)
;;; az.el ends here
