;;; tree-sitter-bash-parser-prototype.el --- Parse bash commands using tree-sitter -*- lexical-binding: t; -*-

;; Prototype for parsing bash commands using tree-sitter for gptel scope validation

(require 'tree-sitter)
(require 'tree-sitter-langs)

(defun jf/gptel-bash-ts--parse-command (command-string)
  "Parse COMMAND-STRING using tree-sitter-bash.
Returns a plist with:
  :success - t if parsing succeeded
  :ast - the tree-sitter AST root node
  :error - error message if parsing failed"
  (condition-case err
      (with-temp-buffer
        (insert command-string)
        (tree-sitter-mode)
        ;; Set language to bash
        (setq tree-sitter-language (tree-sitter-require 'bash))
        (tree-sitter--do-parse)
        (list :success t
              :ast (tsc-root-node tree-sitter-tree)
              :command-string command-string))
    (error (list :success nil
                 :error (error-message-string err)))))

(defun jf/gptel-bash-ts--extract-commands (ast)
  "Extract all command nodes from AST.
Returns a list of command node plists."
  (let ((commands '()))
    (jf/gptel-bash-ts--visit-node
     ast
     (lambda (node)
       (when (eq (tsc-node-type node) 'command)
         (push (jf/gptel-bash-ts--analyze-command node) commands))))
    (nreverse commands)))

(defun jf/gptel-bash-ts--visit-node (node visitor-fn)
  "Visit NODE and all children, calling VISITOR-FN on each."
  (funcall visitor-fn node)
  (let ((child-count (tsc-count-children node)))
    (dotimes (i child-count)
      (jf/gptel-bash-ts--visit-node
       (tsc-get-nth-child node i)
       visitor-fn))))

(defun jf/gptel-bash-ts--analyze-command (command-node)
  "Analyze a command NODE and extract semantic information.
Returns plist with:
  :command-name - the base command (e.g., 'git')
  :all-words - all word tokens in command
  :arguments - arguments (excluding command name)
  :flags - arguments starting with - or --
  :positional-args - arguments not starting with -
  :has-dangerous-flags - t if contains -f, --force, -rf, etc.
  :text - original text of command"
  (let* ((text (tsc-node-text command-node))
         (words (jf/gptel-bash-ts--extract-words command-node))
         (command-name (car words))
         (arguments (cdr words))
         (flags (seq-filter (lambda (w) (string-prefix-p "-" w)) arguments))
         (positional-args (seq-remove (lambda (w) (string-prefix-p "-" w)) arguments))
         (dangerous-flags '("-f" "--force" "-rf" "-fr" "-r -f" "--hard" "-D")))

    (list :command-name command-name
          :all-words words
          :arguments arguments
          :flags flags
          :positional-args positional-args
          :has-dangerous-flags (seq-some
                                (lambda (flag)
                                  (member flag dangerous-flags))
                                flags)
          :text text)))

(defun jf/gptel-bash-ts--extract-words (command-node)
  "Extract all word nodes from COMMAND-NODE as strings."
  (let ((words '()))
    (jf/gptel-bash-ts--visit-node
     command-node
     (lambda (node)
       (when (eq (tsc-node-type node) 'word)
         (push (tsc-node-text node) words))))
    (nreverse words)))

(defun jf/gptel-bash-ts--extract-command-name (command-node)
  "Extract the command name from a command NODE."
  (let ((name-node (tsc-get-child-by-field command-node :command_name)))
    (when name-node
      (tsc-node-text name-node))))

(defun jf/gptel-bash-ts--categorize-git-subcommand (parsed-command)
  "Categorize a git command based on subcommand.
PARSED-COMMAND is result of jf/gptel-bash-ts--analyze-command.
Returns :read-only, :safe-write, or :dangerous."
  (let* ((words (plist-get parsed-command :all-words))
         (subcommand (cadr words))) ; git is first, subcommand is second
    (pcase subcommand
      ((or "log" "show" "diff" "status" "branch" "ls-files")
       :read-only)
      ((or "add" "commit" "stash" "checkout" "switch")
       :safe-write)
      ((or "push" "reset" "clean" "rebase" "cherry-pick")
       :dangerous)
      (_ :unknown))))

;; Example usage demonstration
(defun jf/gptel-bash-ts--demo ()
  "Demonstrate bash command parsing."
  (interactive)
  (let* ((commands '("git log --oneline -10"
                    "ls -la | grep foo"
                    "rm -rf /tmp/test"
                    "python --version"
                    "grep -r 'TODO' ./src"))
         (results '()))

    (dolist (cmd commands)
      (let* ((parsed (jf/gptel-bash-ts--parse-command cmd))
             (ast (plist-get parsed :ast)))
        (when ast
          (let ((command-infos (jf/gptel-bash-ts--extract-commands ast)))
            (push (cons cmd command-infos) results)))))

    ;; Display results
    (with-current-buffer (get-buffer-create "*bash-parse-demo*")
      (erase-buffer)
      (insert "=== Bash Command Parsing Demo ===\n\n")
      (dolist (result (nreverse results))
        (insert (format "Command: %s\n" (car result)))
        (dolist (cmd-info (cdr result))
          (insert (format "  Command name: %s\n" (plist-get cmd-info :command-name)))
          (insert (format "  Arguments: %s\n" (plist-get cmd-info :arguments)))
          (insert (format "  Flags: %s\n" (plist-get cmd-info :flags)))
          (insert (format "  Positional: %s\n" (plist-get cmd-info :positional-args)))
          (insert (format "  Dangerous: %s\n" (plist-get cmd-info :has-dangerous-flags))))
        (insert "\n"))
      (display-buffer (current-buffer)))))

;; Integration with scope validation
(defun jf/gptel-bash-ts--validate-semantic (command-string directory scope-config)
  "Validate command using tree-sitter semantic parsing.
COMMAND-STRING is the bash command to validate.
DIRECTORY is the working directory.
SCOPE-CONFIG is the scope configuration plist.

Returns validation result plist."
  (let* ((parsed (jf/gptel-bash-ts--parse-command command-string))
         (ast (plist-get parsed :ast)))

    (unless (plist-get parsed :success)
      (cl-return-from jf/gptel-bash-ts--validate-semantic
        (list :allowed nil
              :reason "parse-error"
              :error (plist-get parsed :error))))

    (let ((commands (jf/gptel-bash-ts--extract-commands ast)))
      ;; Validate first command in pipeline
      (if-let ((first-cmd (car commands)))
          (jf/gptel-bash-ts--validate-command first-cmd directory scope-config)
        (list :allowed nil
              :reason "no-command-found")))))

(defun jf/gptel-bash-ts--validate-command (cmd-info directory scope-config)
  "Validate a single parsed command CMD-INFO.
DIRECTORY is the working directory.
SCOPE-CONFIG is the scope configuration.

This would integrate with existing category-based validation."
  (let* ((cmd-name (plist-get cmd-info :command-name))
         (bash-config (plist-get scope-config :bash-tools))
         (categories (plist-get bash-config :categories)))

    ;; Example: Check if command is in read-only category
    (let ((read-only-commands (plist-get (plist-get categories :read-only) :commands)))
      (if (member cmd-name read-only-commands)
          (list :allowed t
                :category :read-only
                :command cmd-name)
        ;; Continue with more sophisticated checks...
        (list :allowed nil
              :reason "command-not-in-category"
              :command cmd-name)))))

(provide 'tree-sitter-bash-parser-prototype)
;;; tree-sitter-bash-parser-prototype.el ends here
