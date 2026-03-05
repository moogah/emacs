;;; debug-substitution.el --- Debug command substitution processing -*- lexical-binding: t; -*-

(require 'bash-parser (expand-file-name "../bash-parser.el"
                                        (file-name-directory load-file-name)))

(message "\n========================================")
(message "DEBUG: Command Substitution Investigation")
(message "========================================\n")

(let* ((command "cat $(find . -name '*.log')")
       (parsed (jf/bash-parse command)))

  (message "=== Parse Result Structure ===")
  (message "Success: %s" (plist-get parsed :success))
  (message "Type: %s" (plist-get parsed :type))
  (message "Command name: %s" (plist-get parsed :command-name))
  (message "Positional args: %S" (plist-get parsed :positional-args))
  (message "\nCommand substitutions present: %s"
           (if (plist-get parsed :command-substitutions) "YES" "NO"))

  (when-let ((substs (plist-get parsed :command-substitutions)))
    (message "\nNumber of substitutions: %d" (length substs))
    (dolist (subst substs)
      (message "\n  Substitution:")
      (message "    Content: %s" (plist-get subst :content))
      (message "    Parsed present: %s" (if (plist-get subst :parsed) "YES" "NO"))
      (when-let ((parsed-subst (plist-get subst :parsed)))
        (message "    Parsed success: %s" (plist-get parsed-subst :success))
        (message "    Parsed command: %s" (plist-get parsed-subst :command-name))
        (message "    Parsed args: %S" (plist-get parsed-subst :positional-args)))))

  (message "\n=== File Operations Extraction ===")
  (let ((ops (jf/bash-extract-file-operations parsed)))
    (message "Total operations: %d" (length ops))
    (dolist (op ops)
      (message "\n  Operation:")
      (message "    File: '%s'" (plist-get op :file))
      (message "    Operation: %s" (plist-get op :operation))
      (message "    Command: %s" (plist-get op :command))
      (message "    From-substitution: %s" (plist-get op :from-substitution))
      (message "    Pattern: %s" (plist-get op :pattern))
      (message "    Pattern-source: %s" (plist-get op :pattern-source)))))

(message "\n========================================")
(message "DEBUG Complete")
(message "========================================\n")
