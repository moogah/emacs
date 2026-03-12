;;; find.el --- Find command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for find command.
;; First positional arg is search directory, -name flag argument is pattern.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-find--filesystem-handler (parsed-command)
  "Extract filesystem operations from find PARSED-COMMAND.
First positional arg is search directory (:read-directory).
The -name flag argument is a match pattern (:match-pattern).
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (flags (plist-get parsed-command :flags))
         (command "find")
         (operations nil)
         (search-dir (car positional-args))
         (name-pattern nil))
    ;; Find -name argument: walk flags and count arg-consuming flags
    ;; to find the correct positional arg index for -name's value
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
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
 :command "find"
 :domain :filesystem
 :handler #'jf/bash-command-find--filesystem-handler)

(provide 'bash-command-find-handler)
;;; find.el ends here
