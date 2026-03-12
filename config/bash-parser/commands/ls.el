;;; ls.el --- Ls command handler for semantic extraction -*- lexical-binding: t; -*-

;; Filesystem handler for ls command.
;; No args returns nil. Glob patterns produce :match-pattern.
;; Literal "." produces :read-directory.

;;; Code:

(require 'bash-parser-semantics)

(defun jf/bash-command-ls--has-glob-p (path)
  "Return non-nil if PATH contains glob characters (*, ?, [], {,})."
  (and (stringp path)
       (or (string-match-p "[*?\\[]" path)
           (string-match-p "{.*,.*}" path))))

(defun jf/bash-command-ls--filesystem-handler (parsed-command)
  "Extract filesystem operations from ls PARSED-COMMAND.
No args returns nil (no operations).
Glob patterns (*, ?, []) produce :match-pattern.
Literal \".\" produces :read-directory.
Returns plist with :domain, :operations, :claimed-token-ids, :metadata or nil."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (command "ls")
         (operations nil))
    (when positional-args
      (dolist (arg positional-args)
        (cond
         ;; Glob pattern
         ((jf/bash-command-ls--has-glob-p arg)
          (push (list :file arg
                      :operation :match-pattern
                      :confidence :high
                      :command command)
                operations))
         ;; Literal "."
         ((equal arg ".")
          (push (list :file arg
                      :operation :read-directory
                      :confidence :high
                      :command command)
                operations)))))
    (when operations
      (list :domain :filesystem
            :operations (nreverse operations)
            :claimed-token-ids nil
            :metadata nil))))

(jf/bash-register-command-handler
 :command "ls"
 :domain :filesystem
 :handler #'jf/bash-command-ls--filesystem-handler)

(provide 'bash-command-ls)
;;; ls.el ends here
