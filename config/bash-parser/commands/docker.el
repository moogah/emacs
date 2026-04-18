;;; docker.el --- Docker command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for docker command.
;; Only handles "cp" subcommand: sources :read, dest :write.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-docker--filesystem-handler (parsed-command)
  "Extract filesystem operations from docker PARSED-COMMAND.
Only handles 'cp' subcommand.
docker cp source dest: sources are :read, dest is :write.
Container paths (containing \":\") are included as-is.
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (explicit-subcommand (plist-get parsed-command :subcommand))
         (subcommand (or explicit-subcommand (car positional-args)))
         (sub-args (if explicit-subcommand positional-args (cdr positional-args)))
         (command "docker")
         (operations nil))
    (when (and (equal subcommand "cp")
               (>= (length sub-args) 2))
      (let ((sources (butlast sub-args))
            (dest (car (last sub-args))))
        (dolist (src sources)
          (push (list :file src :operation :read :confidence :high :command command) operations))
        (push (list :file dest :operation :write :confidence :high :command command) operations))
      (when operations
        (list :domain :filesystem
              :operations (nreverse operations)
              :claimed-token-ids nil
              :metadata nil)))))

(jf/bash-register-command-handler
 :command "docker"
 :domain :filesystem
 :handler #'jf/bash-command-docker--filesystem-handler)

(provide 'bash-command-docker)
;;; docker.el ends here
