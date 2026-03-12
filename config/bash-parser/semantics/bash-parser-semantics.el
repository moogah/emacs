;; Author: Jeff Farr
;; Keywords: bash, parser, semantics, database
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Command semantics database for bash parser.
;; Defines file operation semantics for core bash commands.

;;; Code:

;;; bash-parser-semantics.el --- Command semantics database -*- lexical-binding: t; -*-

;; Require protocol module for forward declarations
(require 'bash-parser-protocol)

(defvar jf/bash-command-handlers (make-hash-table :test 'equal)
  "Registry mapping command names to domain-specific handler functions.

Structure: {command-name => {domain-keyword => [handler-fn ...]}}.

Outer hash table is keyed by command name (string, equal test).
Inner hash table is keyed by domain keyword (e.g., :filesystem, :authentication).
Values are lists of handler functions in registration order.")

(defun jf/bash-register-command-handler (&rest args)
  "Register a handler function for a command and semantic domain.

Keyword arguments:
  :command  - String: command name (e.g., \"aws\", \"cat\")
  :domain   - Keyword: semantic domain (e.g., :filesystem)
  :handler  - Function: handler implementation

All three parameters are required.  Signals an error if any are missing.

Handlers are stored in registration order per domain."
  (let ((command (plist-get args :command))
        (domain (plist-get args :domain))
        (handler (plist-get args :handler)))
    (unless command
      (error "jf/bash-register-command-handler: :command is required"))
    (unless domain
      (error "jf/bash-register-command-handler: :domain is required"))
    (unless handler
      (error "jf/bash-register-command-handler: :handler is required"))
    (let ((domain-table (gethash command jf/bash-command-handlers)))
      (unless domain-table
        (setq domain-table (make-hash-table :test 'eq))
        (puthash command domain-table jf/bash-command-handlers))
      (let ((existing (gethash domain domain-table)))
        (unless (memq handler existing)
          (puthash domain (append existing (list handler)) domain-table))))))

(defun jf/bash-lookup-command-handlers (command-name)
  "Look up all registered handlers for COMMAND-NAME.

Returns a hash table mapping domain keywords to lists of handler functions,
or nil if no handlers are registered for this command."
  (gethash command-name jf/bash-command-handlers))

(defun jf/bash-extract-command-semantics (parsed-command)
  "Execute all registered handlers for PARSED-COMMAND and collect results.

Looks up the command name from PARSED-COMMAND, executes all registered
handlers across all domains, merges results by domain, and collects
claimed token IDs.

Each handler receives PARSED-COMMAND and should return a plist:
  (:domain KEYWORD :operations [...] :claimed-token-ids [...] :metadata {...})
or nil if the handler has nothing to contribute.

Returns a plist:
  (:domains ((domain . operations) ...)
   :claimed-token-ids (id ...))

Errors in individual handlers are logged and isolated - they do not
prevent other handlers from executing."
  (let* ((command-name (plist-get parsed-command :command-name))
         (domain-table (when command-name
                         (jf/bash-lookup-command-handlers command-name)))
         (domains-alist nil)
         (all-claimed-ids nil))
    (when domain-table
      (maphash
       (lambda (domain handlers)
         (dolist (handler handlers)
           (condition-case err
               (let ((result (funcall handler parsed-command)))
                 (when result
                   (let ((ops (plist-get result :operations))
                         (claimed (plist-get result :claimed-token-ids)))
                     (when ops
                       (let ((existing (alist-get domain domains-alist)))
                         (setf (alist-get domain domains-alist)
                               (append existing ops))))
                     (when claimed
                       (setq all-claimed-ids
                             (append all-claimed-ids claimed))))))
             (error
              (message "Handler error for %s/%s: %s"
                       command-name domain (error-message-string err))))))
       domain-table))
    ;; Deduplicate claimed token IDs
    (when all-claimed-ids
      (setq all-claimed-ids (delete-dups all-claimed-ids)))
    (list :domains domains-alist
          :claimed-token-ids all-claimed-ids)))

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
    (ls . (:operations :custom
           :handler jf/bash--extract-ls-operations
           :produces-file-list t
           :pattern-source :positional-args
           :search-scope-arg :implicit))
    (grep . (:operations :flag-dependent
              :flag-handlers ((("-l" "--files-with-matches")
                              . ((:source :positional-args :operation :match-pattern :skip-indices (0))))
                             (()
                              . ((:source :positional-args :operation :read :skip-indices (0)))))
             :produces-file-list t
             :pattern-source :positional-args
             :pattern-requires-flag ("-l" "--files-with-matches")))
    (egrep . (:operations :flag-dependent
              :flag-handlers ((("-l" "--files-with-matches")
                              . ((:source :positional-args :operation :match-pattern :skip-indices (0))))
                             (()
                              . ((:source :positional-args :operation :read :skip-indices (0)))))
             :produces-file-list t
             :pattern-source :positional-args
             :pattern-requires-flag ("-l" "--files-with-matches")))
    (fgrep . (:operations :flag-dependent
              :flag-handlers ((("-l" "--files-with-matches")
                              . ((:source :positional-args :operation :match-pattern :skip-indices (0))))
                             (()
                              . ((:source :positional-args :operation :read :skip-indices (0)))))
             :produces-file-list t
             :pattern-source :positional-args
             :pattern-requires-flag ("-l" "--files-with-matches")))
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
    (tar . (:operations :custom
            :handler jf/bash--extract-tar-operations))
    (zip . (:operations :custom
            :handler jf/bash--extract-zip-operations))
    (unzip . (:operations ((:source :positional-args :operation :read))))
    (gzip . (:operations ((:source :positional-args :operation :read)
                          (:source :positional-args :operation :write :suffix ".gz"))))
    (gunzip . (:operations ((:source :positional-args :operation :read)
                            (:source :positional-args :operation :write :strip-suffix ".gz"))))
    (bzip2 . (:operations ((:source :positional-args :operation :read)
                           (:source :positional-args :operation :write :suffix ".bz2"))))
    (bunzip2 . (:operations ((:source :positional-args :operation :read)
                             (:source :positional-args :operation :write :strip-suffix ".bz2"))))
    (sed . (:operations :flag-dependent
            :flag-handlers ((("-i" "--in-place") . ((:source :positional-args :operation :modify :skip-indices (0))))
                           (() . ((:source :positional-args :operation :read :skip-indices (0)))))))
    (find . (:operations :custom
             :handler jf/bash--extract-find-operations
             :produces-file-list t
             :pattern-source :flag-arg
             :search-scope-arg :first-positional))
    (tee . (:operations :flag-dependent
            :flag-handlers ((("-a" "--append")
                             . ((:source :positional-args :operation :append)))
                           (()
                             . ((:source :positional-args :operation :write))))))
    (dd . (:operations ((:source :named-args :names ("if") :operation :read)
                        (:source :named-args :names ("of") :operation :write))))
    (git . (:operations :complex
            :subcommand-handlers ((add . ((:source :positional-args :operation :read)))
                                 (apply . ((:source :positional-args :operation :read :index 0)))
                                 (checkout . ((:source :positional-args :operation :modify)))
                                 (clean . ((:source :positional-args :operation :delete)))
                                 (clone . ((:source :positional-args :operation :write :index -1)))
                                 (diff . ((:source :positional-args :operation :read)))
                                 (ls-files . (:operations ((:source :positional-args :operation :read))
                                             :produces-file-list t
                                             :pattern-source :positional-args
                                             :search-scope-arg :implicit))
                                 (merge . ((:source :positional-args :operation :modify)))
                                 (mv . ((:source :positional-args :indices (0 . -2) :operation :delete)
                                        (:source :positional-args :index -1 :operation :write)))
                                 (pull . ((:source :positional-args :operation :modify)))
                                 (rebase . ((:source :positional-args :operation :modify)))
                                 (reset . ((:source :positional-args :operation :modify)))
                                 (restore . ((:source :positional-args :operation :modify)))
                                 (rm . ((:source :positional-args :operation :delete)))
                                 (show . ((:source :positional-args :operation :read)))
                                 (worktree . (:operations :complex
                                             :subcommand-handlers ((add . ((:source :positional-args
                                                                           :operation :create :index 0)))))))))
    (docker . (:operations :complex
               :subcommand-handlers ((cp . ((:source :positional-args :indices (0 . -2) :operation :read)
                                            (:source :positional-args :index -1 :operation :write))))))
    ;; NOTE: npm, cargo, kubectl not yet implemented - removed stub entries
    (python . (:operations :flag-dependent
               :flag-handlers ((("-c" "-m") . ())  ; -c and -m execute inline code/module, no file operation
                              (() . ((:source :positional-args :operation :execute :index 0))))))
    (python3 . (:operations :flag-dependent
                :flag-handlers ((("-c" "-m") . ())  ; -c and -m execute inline code/module, no file operation
                               (() . ((:source :positional-args :operation :execute :index 0))))))
    (node . (:operations :flag-dependent
             :flag-handlers ((("-e" "--eval" "-p" "--print") . ())  ; -e/-p execute inline code, no file operation
                            (() . ((:source :positional-args :operation :execute :index 0))))))
    (bash . (:operations :flag-dependent
             :flag-handlers ((("-c") . ())  ; -c executes inline code, no file operation
                            (() . ((:source :positional-args :operation :execute :index 0))))))
    (sh . (:operations :flag-dependent
           :flag-handlers ((("-c") . ())  ; -c executes inline code, no file operation
                          (() . ((:source :positional-args :operation :execute :index 0))))))
    (zsh . (:operations :flag-dependent
            :flag-handlers ((("-c") . ())  ; -c executes inline code, no file operation
                           (() . ((:source :positional-args :operation :execute :index 0))))))
    (ruby . (:operations :flag-dependent
             :flag-handlers ((("-e") . ())  ; -e executes inline code, no file operation
                            (() . ((:source :positional-args :operation :execute :index 0))))))
    (perl . (:operations :flag-dependent
             :flag-handlers ((("-e" "-E") . ())  ; -e/-E execute inline code, no file operation
                            (() . ((:source :positional-args :operation :execute :index 0))))))
    (php . (:operations :flag-dependent
            :flag-handlers ((("-r") . ())  ; -r executes inline code, no file operation
                           (() . ((:source :positional-args :operation :execute :index 0))))))
    (exec . (:operations ((:source :positional-args :operation :execute :index 0))))
    (source . (:operations ((:source :positional-args :operation :execute :index 0))))
    (\. . (:operations ((:source :positional-args :operation :execute :index 0))))
    (go . (:operations :complex
           :subcommand-handlers ((run . ((:source :positional-args :operation :execute :index 0)))
                                (test . ((:source :positional-args :operation :execute :index 0)))
                                (build . ((:source :positional-args :operation :read :index 0))))))
    (which . (:operations ((:source :positional-args :operation :read-metadata))))
    (dirname . (:operations ((:source :positional-args :operation :read-metadata)))))
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
    :execute - Execute file as script or binary
    :match-pattern - Search for files matching pattern
    :read-directory - Read directory contents to find matches

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
  (when (and command-name
             ;; Don't try to intern assignment strings like "DIR=/tmp"
             (not (string-match-p "=" command-name)))
    (alist-get (intern command-name) jf/bash-command-file-semantics)))

(defun jf/bash--extract-find-operations (parsed-command var-context)
  "Custom extraction for find command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Find has special argument handling:
- Initial directory paths are classified as :read-directory
- Flag arguments (after -name, -type, etc.) are patterns with :match-pattern
- Exec blocks (-exec, -execdir) are recursively parsed for operations
- When flags are present, only first positional arg is treated as directory
- When no flags, all positional args are directory paths

Returns list of operation plists with :read-directory for search paths,
:match-pattern for -name patterns, and operations from -exec blocks."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (flags (plist-get parsed-command :flags))
         (command-name (plist-get parsed-command :command-name))
         (operations nil)
         (search-dir nil)
         (name-pattern nil))

    ;; Find argument structure: find [paths...] [expression...]
    ;; The parser puts directory paths AND flag argument values in positional-args
    ;; Flags themselves go into the :flags list

    ;; Extract search directory (first positional arg before flags)
    (when positional-args
      (setq search-dir (car positional-args)))

    ;; Find -name argument (pattern after -name flag)
    ;; Need to skip arguments consumed by other flags like -type
    ;; Flags that consume an argument: -name, -type, -path, -iname, -ipath, etc.
    (let ((arg-consuming-flags '("-name" "-type" "-path" "-iname" "-ipath" "-regex"
                                 "-iregex" "-size" "-user" "-group" "-perm" "-mtime"
                                 "-atime" "-ctime" "-newer"))
          (arg-index 1))  ; Start after directory (index 0)
      ;; Walk through flags and their arguments
      (dolist (flag flags)
        (when (member flag arg-consuming-flags)
          (if (string= flag "-name")
              ;; This is the -name flag, save its argument
              (when (< arg-index (length positional-args))
                (setq name-pattern (nth arg-index positional-args)))
            ;; Other flag with argument, skip it
            (setq arg-index (1+ arg-index))))))

    ;; Add :read-directory operation for search location
    (when search-dir
      (let* ((resolved-path (jf/bash--resolve-path-variables search-dir var-context))
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
                       (list :unresolved t :unresolved-vars unresolved-vars)))
              operations)))

    ;; Add :match-pattern operation for the pattern
    (when name-pattern
      (let* ((resolved-path (jf/bash--resolve-path-variables name-pattern var-context))
             (final-path (if (stringp resolved-path)
                            resolved-path
                          (plist-get resolved-path :path)))
             (unresolved-vars (when (listp resolved-path)
                               (plist-get resolved-path :unresolved))))
        (push (append (list :file final-path
                           :operation :match-pattern
                           :confidence :high
                           :source :flag-arg
                           :pattern t
                           :search-scope search-dir
                           :command command-name)
                     (when unresolved-vars
                       (list :unresolved t :unresolved-vars unresolved-vars)))
              operations)))

    ;; Extract operations from -exec blocks
    (when-let ((exec-ops (jf/bash-extract-from-exec-blocks parsed-command var-context)))
      (setq operations (append operations exec-ops)))

    (nreverse operations)))

(defun jf/bash--extract-ls-operations (parsed-command var-context)
  "Custom extraction for ls command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Ls without args lists current directory (no operation).
Ls with glob patterns searches for matching files (:match-pattern).
Ls with literal directory paths (like .) creates :read-directory operations.

Returns list of operation plists."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (command-name (plist-get parsed-command :command-name))
         (operations nil))

    (when positional-args
      ;; Check each arg
      (dolist (arg positional-args)
        (let* ((resolved-path (jf/bash--resolve-path-variables arg var-context))
               (final-path (if (stringp resolved-path)
                              resolved-path
                            (plist-get resolved-path :path)))
               (unresolved-vars (when (listp resolved-path)
                                 (plist-get resolved-path :unresolved))))
          (cond
           ;; Glob pattern - match-pattern operation
           ((jf/bash--has-glob-pattern-p final-path)
            (push (append (list :file final-path
                               :operation :match-pattern
                               :confidence :high
                               :source :positional-arg
                               :pattern t
                               :command command-name)
                         (when unresolved-vars
                           (list :unresolved t :unresolved-vars unresolved-vars)))
                  operations))
           ;; Literal directory reference (. before resolution)
           ;; Check original arg since jf/bash--resolve-path-variables already resolved it
           ((equal arg ".")
            ;; final-path already has . resolved to PWD by jf/bash--resolve-path-variables
            (push (list :file final-path
                       :operation :read-directory
                       :confidence :high
                       :source :positional-arg
                       :command command-name)
                  operations))))))

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

(defun jf/bash--extract-tar-operations (parsed-command var-context)
  "Custom extraction for tar command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Tar structure:
- Archive file specified by -f flag value
- Operation determined by mode flag: -c (create), -x (extract), -t (list)
- Positional args are source files (when creating) or extracted files (when extracting)

Returns list of operation plists."
  (let* ((flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (args (plist-get parsed-command :args))
         (command-name (plist-get parsed-command :command-name))
         (operations nil)
         (archive-file nil)
         (mode nil))

    ;; Determine mode from flags
    (dolist (flag flags)
      (cond
       ((jf/bash--flag-present-p "-c" (list flag)) (setq mode :create))
       ((jf/bash--flag-present-p "-x" (list flag)) (setq mode :extract))
       ((jf/bash--flag-present-p "-t" (list flag)) (setq mode :list))))

    ;; Find archive file from -f flag
    ;; In combined flags like -czf, the f means next arg is the archive file
    (let ((i 0))
      (while (and (< i (length args)) (not archive-file))
        (let ((arg (nth i args)))
          (cond
           ;; Standalone -f flag, next arg is archive
           ((equal arg "-f")
            (when (< (1+ i) (length args))
              (setq archive-file (nth (1+ i) args))))
           ;; Combined flag with f, next arg is archive
           ((and (string-prefix-p "-" arg)
                 (not (string-prefix-p "--" arg))
                 (string-match-p "f" arg))
            (when (< (1+ i) (length args))
              (setq archive-file (nth (1+ i) args))))))
        (setq i (1+ i))))

    (when (and mode archive-file)
      ;; Handle archive file operation based on mode
      (let* ((resolved-path (jf/bash--resolve-path-variables archive-file var-context))
             (final-path (if (stringp resolved-path)
                            resolved-path
                          (plist-get resolved-path :path)))
             (unresolved-vars (when (listp resolved-path)
                               (plist-get resolved-path :unresolved))))
        (push (append (list :file final-path
                           :operation (cond
                                      ((eq mode :create) :write)
                                      ((eq mode :extract) :read)
                                      ((eq mode :list) :read))
                           :confidence :high
                           :source :positional-arg
                           :command command-name)
                     (when unresolved-vars
                       (list :unresolved unresolved-vars)))
              operations))

      ;; Handle positional args (source files or extracted files)
      (when positional-args
        (dolist (file positional-args)
          ;; Skip if this is the archive file
          (unless (equal file archive-file)
            (let* ((resolved-path (jf/bash--resolve-path-variables file var-context))
                   (final-path (if (stringp resolved-path)
                                  resolved-path
                                (plist-get resolved-path :path)))
                   (unresolved-vars (when (listp resolved-path)
                                     (plist-get resolved-path :unresolved))))
              (push (append (list :file final-path
                                 :operation (if (eq mode :create) :read :write)
                                 :confidence :high
                                 :source :positional-arg
                                 :command command-name)
                           (when unresolved-vars
                             (list :unresolved unresolved-vars)))
                    operations))))))

    (nreverse operations)))

(defun jf/bash--extract-zip-operations (parsed-command var-context)
  "Custom extraction for zip command operations.

PARSED-COMMAND is the parsed command structure.
VAR-CONTEXT is the variable resolution context.

Zip structure:
- First positional arg is the zip file (write)
- Remaining args are source files/directories (read)

Returns list of operation plists."
  (let* ((positional-args (plist-get parsed-command :positional-args))
         (command-name (plist-get parsed-command :command-name))
         (operations nil))

    (when positional-args
      (let ((zip-file (car positional-args))
            (source-files (cdr positional-args)))

        ;; Write the zip file
        (let* ((resolved-path (jf/bash--resolve-path-variables zip-file var-context))
               (final-path (if (stringp resolved-path)
                              resolved-path
                            (plist-get resolved-path :path)))
               (unresolved-vars (when (listp resolved-path)
                                 (plist-get resolved-path :unresolved))))
          (push (append (list :file final-path
                             :operation :write
                             :confidence :high
                             :source :positional-arg
                             :command command-name)
                       (when unresolved-vars
                         (list :unresolved unresolved-vars)))
                operations))

        ;; Read the source files
        (dolist (source source-files)
          (let* ((resolved-path (jf/bash--resolve-path-variables source var-context))
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
                  operations)))))

    (nreverse operations)))

(defun jf/bash--command-produces-file-list-p (command-name &optional subcommand flags)
  "Check if COMMAND-NAME produces a list of file paths as output.

SUBCOMMAND is an optional subcommand name (for commands like git).
FLAGS is an optional list of flags passed to the command.

Pattern-producing commands include:
- ls, find, locate (always produce file lists)
- grep, egrep, fgrep (only with -l or --files-with-matches)
- git ls-files (specific subcommand)

Returns non-nil if the command produces a file list, nil otherwise."
  (let ((semantics (jf/bash-lookup-command-semantics command-name)))
    (when semantics
      (cond
       ;; Handle subcommand-based commands (like git ls-files)
       ((eq (plist-get semantics :operations) :complex)
        (let* ((subcommand-handlers (plist-get semantics :subcommand-handlers))
               (subcommand-spec (when subcommand
                                 (cdr (assoc (intern subcommand) subcommand-handlers)))))
          (and subcommand-spec
               (plist-get subcommand-spec :produces-file-list))))

       ;; Handle flag-dependent commands (like grep -l)
       ((plist-get semantics :pattern-requires-flag)
        (let ((required-flags (plist-get semantics :pattern-requires-flag)))
          ;; Command produces file list only if one of the required flags is present
          (and flags
               (seq-some (lambda (flag) (member flag flags)) required-flags))))

       ;; Simple commands with produces-file-list metadata
       (t
        (plist-get semantics :produces-file-list))))))

(defun jf/bash-validate-semantics-database ()
  "Validate the semantics database for common configuration errors.

Checks for:
1. Empty subcommand handlers - catches stub entries like npm/cargo/kubectl
2. Missing custom handler functions - catches typos in handler names
3. Invalid operation types - catches typos like :writes instead of :write
4. Missing flag-handlers for flag-dependent - catches incomplete configurations

Returns list of validation error strings, or nil if database is valid."
  (let ((errors nil)
        (valid-operation-types '(:read :write :delete :modify :create :create-or-modify
                                :execute :match-pattern :read-directory :read-metadata :append)))

    ;; Check each command entry
    (dolist (entry jf/bash-command-file-semantics)
      (let* ((command (car entry))
             (semantics (cdr entry))
             (operations (plist-get semantics :operations)))

        ;; Check 1: Empty subcommand handlers
        (when (eq operations :complex)
          (let ((subcommand-handlers (plist-get semantics :subcommand-handlers)))
            (unless subcommand-handlers
              (push (format "Command '%s': :complex operations but no :subcommand-handlers"
                           command)
                   errors))
            ;; Check for empty handler lists
            (dolist (subcommand-entry subcommand-handlers)
              (let ((subcommand (car subcommand-entry))
                    (handler-spec (cdr subcommand-entry)))
                (when (null handler-spec)
                  (push (format "Command '%s': subcommand '%s' has empty handler"
                               command subcommand)
                       errors))))))

        ;; Check 2: Missing custom handler functions
        (when (eq operations :custom)
          (let ((handler (plist-get semantics :handler)))
            (unless handler
              (push (format "Command '%s': :custom operations but no :handler specified"
                           command)
                   errors))
            (when (and handler (not (fboundp handler)))
              (push (format "Command '%s': handler function '%s' is not defined"
                           command handler)
                   errors))))

        ;; Check 3 & 4: Validate flag-dependent operations
        (when (eq operations :flag-dependent)
          (let ((flag-handlers (plist-get semantics :flag-handlers)))
            (unless flag-handlers
              (push (format "Command '%s': :flag-dependent operations but no :flag-handlers"
                           command)
                   errors))
            ;; Validate operation types in flag handlers
            (dolist (flag-entry flag-handlers)
              (let ((operation-specs (cdr flag-entry)))
                ;; Check each operation spec in the handler
                (when (listp operation-specs)
                  (dolist (op-spec operation-specs)
                    (when (plistp op-spec)
                      (let ((op-type (plist-get op-spec :operation)))
                        (when (and op-type (not (memq op-type valid-operation-types)))
                          (push (format "Command '%s': invalid operation type '%s' (valid: %s)"
                                       command op-type valid-operation-types)
                               errors))))))))))

        ;; Check 4: Validate simple operation specs
        (when (listp operations)
          (dolist (op-spec operations)
            (when (plistp op-spec)
              (let ((op-type (plist-get op-spec :operation)))
                (when (and op-type (not (memq op-type valid-operation-types)))
                  (push (format "Command '%s': invalid operation type '%s' (valid: %s)"
                               command op-type valid-operation-types)
                       errors))))))))

    ;; Return errors in forward order
    (nreverse errors)))

(provide 'bash-parser-semantics)
;;; bash-parser-semantics.el ends here
