;;; find.el --- Find command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for find command.
;; First positional arg is search directory, -name flag argument is pattern.

;;; Code:

(require 'bash-parser-semantics)

(defconst jf/bash-find-exec-command-ops
  '(("rm" . :delete)
    ("cat" . :read)
    ("grep" . :read)
    ("head" . :read)
    ("tail" . :read)
    ("less" . :read)
    ("wc" . :read)
    ("chmod" . :modify)
    ("chown" . :modify)
    ("mv" . :modify)
    ("cp" . :read))
  "Mapping of common -exec commands to their primary file operation type.")

(defun jf/bash-command-find--filesystem-handler (parsed-command)
  "Extract filesystem operations from find PARSED-COMMAND.
First positional arg is search directory (:read-directory).
The -name flag argument is a match pattern (:match-pattern).
-exec blocks (from parser's :exec-blocks key) extract operations based on
the executed command.
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (flags (plist-get parsed-command :flags))
         (tokens (plist-get parsed-command :tokens))
         (exec-blocks (plist-get parsed-command :exec-blocks))
         (command "find")
         (operations nil)
         (claimed-ids nil)
         (search-dir (car positional-args))
         (name-pattern nil))
    ;; Find -name argument: walk flags to find it, use corresponding positional arg
    (let ((arg-consuming-flags '("-name" "-type" "-path" "-iname" "-ipath" "-regex"
                                 "-iregex" "-size" "-user" "-group" "-perm" "-mtime"
                                 "-atime" "-ctime" "-newer"))
          (arg-index 1))  ; Start after directory (index 0)
      (dolist (flag flags)
        (when (member flag arg-consuming-flags)
          (if (string= flag "-name")
              (when (< arg-index (length positional-args))
                (setq name-pattern (nth arg-index positional-args)))
            (setq arg-index (1+ arg-index))))))
    ;; Add :read-directory for search path
    (when search-dir
      (push (list :file search-dir
                  :operation :read-directory
                  :confidence :high
                  :source :positional-arg
                  :command command)
            operations))
    ;; Add :match-pattern for -name pattern
    (when name-pattern
      (push (list :file name-pattern
                  :operation :match-pattern
                  :confidence :high
                  :source :flag-arg
                  :pattern t
                  :search-scope search-dir
                  :command command)
            operations))
    ;; Add operations from parser's :exec-blocks
    ;; Each block has :type, :command-name, :positional-args, :args, :terminator
    (dolist (exec-block exec-blocks)
      (let* ((exec-cmd (plist-get exec-block :command-name))
             (op-type (when exec-cmd
                        (alist-get exec-cmd jf/bash-find-exec-command-ops
                                   nil nil #'string=))))
        (when op-type
          (push (list :file "{}"
                      :operation op-type
                      :confidence :high
                      :source :exec-block
                      :command exec-cmd)
                operations))))
    ;; Claim token IDs: command-name + all positional-arg tokens + flags
    (when (and operations tokens)
      (dolist (token tokens)
        (when (memq (plist-get token :type) '(:command-name :positional-arg :flag))
          (when-let ((id (plist-get token :id)))
            (push id claimed-ids)))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids (nreverse claimed-ids)
            :metadata nil))))

(jf/bash-register-command-handler
 :command "find"
 :domain :filesystem
 :handler #'jf/bash-command-find--filesystem-handler)

(provide 'bash-command-find)
;;; find.el ends here
