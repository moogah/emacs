;;; bash-parser-semantics.el --- Command semantics database -*- lexical-binding: t; -*-

(defvar jf/bash-command-file-semantics
  '((cat . (:operations ((:source :positional-args :operation :read))))
    (head . (:operations :custom
             :handler jf/bash--extract-head-tail-operations))
    (tail . (:operations :custom
             :handler jf/bash--extract-head-tail-operations))
    (less . (:operations ((:source :positional-args :operation :read))))
    (more . (:operations ((:source :positional-args :operation :read))))
    (wc . (:operations ((:source :positional-args :operation :read))))
    (file . (:operations ((:source :positional-args :operation :read))))
    (stat . (:operations ((:source :positional-args :operation :read))))
    (grep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (egrep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (fgrep . (:operations ((:source :positional-args :operation :read :skip-indices (0)))))
    (touch . (:operations ((:source :positional-args :operation :create-or-modify))))
    (mkdir . (:operations ((:source :positional-args :operation :create))))
    (rm . (:operations ((:source :positional-args :operation :delete))))
    (rmdir . (:operations ((:source :positional-args :operation :delete))))
    (chmod . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (chown . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (chgrp . (:operations ((:source :positional-args :operation :modify :skip-indices (0)))))
    (ln . (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
                        (:source :positional-args :index -1 :operation :write))))
    (cp . (:operations ((:source :positional-args :indices (0 . -2) :operation :read)
                        (:source :positional-args :index -1 :operation :write))))
    (mv . (:operations ((:source :positional-args :indices (0 . -2) :operation :delete)
                        (:source :positional-args :index -1 :operation :write))))
    (tar . (:operations :flag-dependent
            :flag-handlers ((("-x" "--extract" "--get") . ((:source :positional-args :operation :write)))
                           (("-c" "--create") . ((:source :positional-args :operation :read))))))
    (sed . (:operations :flag-dependent
            :flag-handlers ((("-i" "--in-place") . ((:source :positional-args :operation :modify :skip-indices (0))))
                           (() . ((:source :positional-args :operation :read :skip-indices (0)))))))
    (find . (:operations :custom
             :handler jf/bash--extract-find-operations))
    (tee . (:operations ((:source :positional-args :operation :write))))
    (dd . (:operations ((:source :named-args :names ("if") :operation :read)
                        (:source :named-args :names ("of") :operation :write))))
    (git . (:operations :complex
            :subcommand-handlers ((add . ((:source :positional-args :operation :read)))
                                 (rm . ((:source :positional-args :operation :delete)))
                                 (checkout . ((:source :positional-args :operation :modify)))
                                 (restore . ((:source :positional-args :operation :modify)))
                                 (mv . ((:source :positional-args :indices (0 . -2) :operation :delete)
                                        (:source :positional-args :index -1 :operation :write)))
                                 (diff . ((:source :positional-args :operation :read)))
                                 (show . ((:source :positional-args :operation :read))))))
    (docker . (:operations :complex
               :subcommand-handlers ((cp . ((:source :positional-args :indices (0 . -2) :operation :read)
                                            (:source :positional-args :index -1 :operation :write))))))
    (npm . (:operations :complex
            :subcommand-handlers ()))
    (cargo . (:operations :complex
              :subcommand-handlers ()))
    (kubectl . (:operations :complex
                :subcommand-handlers ())))
  "Database mapping command names to file operation semantics.

Each entry maps a command symbol to a plist describing how the command
interacts with files:

Simple commands:
  (:operations ((operation-spec) ...))

Flag-dependent commands (operation depends on flags):
  (:operations :flag-dependent
   :flag-handlers ((flag-list . operations) ...))

Subcommand-based commands (operation depends on subcommand):
  (:operations :complex
   :subcommand-handlers ((subcommand . operations) ...))

Operation spec format:
  :source - Where file paths come from:
    :positional-args - From positional arguments
    :named-args - From named arguments (like dd if=/path of=/path)
    :redirections - From shell redirections (handled separately)
    :exec-blocks - From find -exec blocks (handled separately)

  :operation - Type of file operation:
    :read - Read file contents
    :write - Create new file or overwrite existing
    :delete - Remove file or directory
    :modify - Change existing file in place
    :create - Create new file (must not exist)
    :create-or-modify - Create or update file

  :index - Single positional argument index (0-based, -1 for last)
  :indices - Range of positional arguments:
    (0 . -2) means all arguments from first to second-to-last
    (0 . -1) means all arguments from first to last
    :all means all positional arguments

  :skip-indices - List of argument indices to skip (e.g., (0) skips first arg)
  :names - List of named argument names (for dd-style commands)")

(defun jf/bash-lookup-command-semantics (command-name)
  "Look up file operation semantics for COMMAND-NAME.

Returns semantics plist from `jf/bash-command-file-semantics', or nil
if command is not in the database.

The returned plist describes how the command interacts with files:
- :operations - Operation specifications or :flag-dependent/:complex
- :flag-handlers - For flag-dependent commands
- :subcommand-handlers - For subcommand-based commands

Example return values:

Simple command:
  (:operations ((:source :positional-args :operation :read)))

Flag-dependent command:
  (:operations :flag-dependent
   :flag-handlers (((\"-i\") . ((:source :positional-args :operation :modify)))))

Subcommand-based command:
  (:operations :complex
   :subcommand-handlers ((add . ((:source :positional-args :operation :read)))))"
  (when command-name
    (alist-get (intern command-name) jf/bash-command-file-semantics)))

(defun jf/bash--extract-find-operations (parsed-command var-context)
  "Custom extraction for find command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Find has special argument handling:
- Initial directory paths are classified as :read-directory
- Flag arguments (after -name, -type, etc.) are skipped from extraction
- When flags are present, only first positional arg is treated as directory
- When no flags, all positional args are directory paths

Returns list of operation plists."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (flags (plist-get parsed-command :flags))
         (command-name (plist-get parsed-command :command-name))
         (operations nil))

    ;; Find argument structure: find [paths...] [expression...]
    ;; The parser puts directory paths AND flag argument values in positional-args
    ;; Flags themselves go into the :flags list

    ;; Strategy: if find has any expression flags, we assume only the first
    ;; positional arg is a directory path, and the rest are flag arguments
    ;; (e.g., find . -name '*.log' → positional-args is ("." "*.log"))

    (when positional-args
      (let ((num-paths-to-extract (if flags 1 (length positional-args))))
        ;; Extract only the first N positional args as directory paths
        (dotimes (idx num-paths-to-extract)
          (let* ((arg (nth idx positional-args))
                 (resolved-path (jf/bash--resolve-path-variables arg var-context))
                 (final-path (if (stringp resolved-path)
                                resolved-path
                              (plist-get resolved-path :path)))
                 (unresolved-vars (when (listp resolved-path)
                                   (plist-get resolved-path :unresolved))))
            (push (append (list :file final-path
                               :operation :read-directory
                               :confidence :high
                               :source :positional-arg
                               :command command-name)
                         (when unresolved-vars
                           (list :unresolved unresolved-vars)))
                  operations)))))

    (nreverse operations)))

(defun jf/bash--extract-head-tail-operations (parsed-command var-context)
  "Custom extraction for head/tail command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Head/tail have flags that consume numeric arguments (-n NUM, -c NUM).
These arguments end up in positional-args and should be skipped.

Strategy: count how many flags that take arguments are present,
then skip that many positional args from the beginning.

Returns list of operation plists."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (flags (plist-get parsed-command :flags))
         (command-name (plist-get parsed-command :command-name))
         (operations nil))

    ;; Flags that consume the next argument
    (let ((arg-consuming-flags '("-n" "-c"))
          (num-args-to-skip 0))

      ;; Count how many positional args are consumed by flags
      (dolist (flag flags)
        (when (member flag arg-consuming-flags)
          (setq num-args-to-skip (1+ num-args-to-skip))))

      ;; Extract file operations from remaining positional args
      (when positional-args
        (let ((file-args (nthcdr num-args-to-skip positional-args)))
          (dolist (arg file-args)
            (let* ((resolved-path (jf/bash--resolve-path-variables arg var-context))
                   (final-path (if (stringp resolved-path)
                                  resolved-path
                                (plist-get resolved-path :path)))
                   (unresolved-vars (when (listp resolved-path)
                                     (plist-get resolved-path :unresolved))))
              (push (append (list :file final-path
                                 :operation :read
                                 :confidence :high
                                 :source :positional-arg
                                 :command command-name)
                           (when unresolved-vars
                             (list :unresolved unresolved-vars)))
                    operations))))))

    (nreverse operations)))

(provide 'bash-parser-semantics)
;;; bash-parser-semantics.el ends here
