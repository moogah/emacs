;;; bash-parser-extensions.el --- Parser extensions -*- lexical-binding: t; -*-

(defun jf/bash-mark-indirect-operations (operations)
  "Mark OPERATIONS as indirect if they should have stricter security policies.

Operations are marked as indirect (:indirect t) if they come from:
- Exec blocks (find -exec, find -execdir)
- Nested commands (bash -c, sh -c, python -c) [future]
- Command substitution [future]

OPERATIONS is a list of operation plists from extraction functions.

Returns updated operations list with :indirect t added where appropriate.

The :indirect flag is a security signal indicating that the operation
will be executed in a nested context, which may require stricter
validation or explicit approval.

Examples:
  ;; Direct operation - not marked
  (jf/bash-mark-indirect-operations
    '((:file \"file.txt\" :operation :read :source :positional-arg)))
  => ((:file \"file.txt\" :operation :read :source :positional-arg))

  ;; Exec block operation - marked as indirect
  (jf/bash-mark-indirect-operations
    '((:file \"{}\" :operation :delete :source :exec-block)))
  => ((:file \"{}\" :operation :delete :source :exec-block :indirect t))

  ;; Mixed operations - only exec-block marked
  (jf/bash-mark-indirect-operations
    '((:file \"a.txt\" :operation :read :source :positional-arg)
      (:file \"{}\" :operation :delete :source :exec-block)))
  => ((:file \"a.txt\" :operation :read :source :positional-arg)
      (:file \"{}\" :operation :delete :source :exec-block :indirect t))"
  (mapcar
   (lambda (op)
     (let ((source (plist-get op :source)))
       (cond
        ;; Exec blocks are always indirect
        ((eq source :exec-block)
         (plist-put (copy-sequence op) :indirect t))

        ;; Already marked as indirect (from nested command parsing)
        ((plist-get op :indirect)
         op)

        ;; Direct operations remain unmarked
        (t op))))
   operations))

(defvar jf/bash-command-injection-patterns
  '((bash . (:flags ("-c") :arg-after-flag t))
    (sh . (:flags ("-c") :arg-after-flag t))
    (zsh . (:flags ("-c") :arg-after-flag t))
    (env . (:flags ("-S" "--split-string") :arg-after-flag t))
    (eval . (:no-flag-required t :first-arg t)))
  "Database of command injection patterns.

Each entry maps a command symbol to detection parameters:
  :flags - List of flags that indicate command injection
  :arg-after-flag - If t, nested command is the argument after the flag
  :no-flag-required - If t, command doesn't need a flag (like eval)
  :first-arg - If t, first positional arg is the nested command

Command injection detection is for SHELL commands that execute nested shell code.
Non-shell interpreters (python -c, node -e, ruby -e, etc.) are NOT injection
because they execute code in their own language, not bash. File operations in
those languages require language-specific parsing.

Rationale for exclusion:
1. Python/Node/Ruby code cannot directly execute bash commands
2. File operations in those languages use language-specific APIs
3. Detecting file ops requires parsing Python/JS/Ruby syntax
4. Current implementation maintains cleaner security model

Examples:
  bash -c 'rm file.txt'     - Flag: -c, nested: 'rm file.txt' (shell injection)
  sh -c 'cat file'          - Flag: -c, nested: 'cat file' (shell injection)
  zsh -c 'cat file'         - Flag: -c, nested: 'cat file' (shell injection)
  env -S 'prog arg'         - Flag: -S, nested: 'prog arg' (shell injection)
  eval 'rm file.txt'        - No flag, nested: 'rm file.txt' (shell injection)
  python -c 'print(1)'      - NOT injection (Python code, not bash)
  node -e 'console.log(1)'  - NOT injection (JavaScript code, not bash)
  ruby -e 'puts 1'          - NOT injection (Ruby code, not bash)")

(defun jf/bash-detect-command-injection (parsed-command)
  "Detect command injection patterns in PARSED-COMMAND.

Returns plist with command injection metadata if pattern detected, nil otherwise:
  :command-injection - Always t when pattern detected
  :nested-command-string - The nested command string to be executed
  :injection-type - Type of injection (:flag-based or :direct)
  :trigger-flag - The flag that triggers injection (for flag-based)

PARSED-COMMAND is the output from `jf/bash-parse' (simple command only).

Detects patterns like:
  bash -c 'command'    - Flag-based injection
  sh -c 'command'      - Flag-based injection
  python -c 'code'     - Flag-based injection
  env -S 'prog args'   - Flag-based injection
  eval 'command'       - Direct injection (no flag)

Handles flags appearing in different positions:
  bash -x -e -c 'cmd'  - Detects -c despite other flags
  bash -c -x 'cmd'     - Detects -c even with trailing flags

The nested command string is extracted from positional arguments based on
the pattern definition. For flag-based injection, it's typically the first
positional arg after all flags.

Examples:
  (jf/bash-detect-command-injection
    (jf/bash-parse \"bash -c 'rm file.txt'\"))
  => (:command-injection t
      :nested-command-string \"rm file.txt\"
      :injection-type :flag-based
      :trigger-flag \"-c\")

  (jf/bash-detect-command-injection
    (jf/bash-parse \"eval 'rm file.txt'\"))
  => (:command-injection t
      :nested-command-string \"rm file.txt\"
      :injection-type :direct)

  (jf/bash-detect-command-injection
    (jf/bash-parse \"bash script.sh\"))
  => nil  ; No injection pattern detected"
  (let* ((cmd-name (plist-get parsed-command :command-name))
         (flags (plist-get parsed-command :flags))
         (positional-args (plist-get parsed-command :positional-args))
         (pattern nil)
         (result nil))

    ;; Look up command in injection patterns database
    (when (and cmd-name (not (string-match-p "=" cmd-name)))
      (setq pattern (alist-get (intern cmd-name) jf/bash-command-injection-patterns)))

    ;; If pattern found, check if conditions are met
    (when pattern
      (let ((pattern-flags (plist-get pattern :flags))
            (arg-after-flag (plist-get pattern :arg-after-flag))
            (no-flag-required (plist-get pattern :no-flag-required))
            (first-arg (plist-get pattern :first-arg))
            (nested-cmd nil)
            (trigger-flag nil))

        (cond
         ;; No flag required - first positional arg is nested command
         (no-flag-required
          (when (and first-arg positional-args)
            (setq nested-cmd (car positional-args))
            (setq result (list :command-injection t
                              :nested-command-string nested-cmd
                              :injection-type :direct))))

         ;; Flag-based injection - check if trigger flag present
         ((and pattern-flags arg-after-flag)
          ;; Find if any pattern flag is present in command flags
          (dolist (flag pattern-flags)
            (when (and (null trigger-flag)
                      (member flag flags))
              (setq trigger-flag flag)))

          ;; If trigger flag found, extract nested command from positional args
          (when (and trigger-flag positional-args)
            ;; First positional arg (after all flags) is the nested command
            (setq nested-cmd (car positional-args))
            (setq result (list :command-injection t
                              :nested-command-string nested-cmd
                              :injection-type :flag-based
                              :trigger-flag trigger-flag)))))))

    result))

(defun jf/bash--strip-outer-quotes (str)
  "Strip outer single or double quotes from STR.

Preserves inner quotes - only removes matching outermost quote pair.

Handles:
  - Single quotes: 'cmd' -> cmd
  - Double quotes: \"cmd\" -> cmd
  - Preserved inner: 'grep \"pattern\" file' -> grep \"pattern\" file
  - No quotes: Returns str unchanged
  - Escaped quotes: Preserves escaped quotes in result

Examples:
  (jf/bash--strip-outer-quotes \"'rm file.txt'\")
  => \"rm file.txt\"

  (jf/bash--strip-outer-quotes \"\\\"cat file.txt\\\"\")
  => \"cat file.txt\"

  (jf/bash--strip-outer-quotes \"'grep \\\"pattern\\\" file'\")
  => \"grep \\\"pattern\\\" file\"

  (jf/bash--strip-outer-quotes \"no-quotes\")
  => \"no-quotes\"

Limitation: Does not handle escaped outer quotes (\\\\' or \\\\\\\") at the
boundaries. These are rare in practice since bash command strings
typically use different quote types for inner/outer levels."
  (cond
   ;; Single quotes - strip if both start and end match
   ((and (>= (length str) 2)
         (string-prefix-p "'" str)
         (string-suffix-p "'" str))
    (substring str 1 (1- (length str))))

   ;; Double quotes - strip if both start and end match
   ((and (>= (length str) 2)
         (string-prefix-p "\"" str)
         (string-suffix-p "\"" str))
    (substring str 1 (1- (length str))))

   ;; No outer quotes - return unchanged
   (t str)))

(defun jf/bash-parse-nested-command (nested-command-string &optional nesting-level parent-command)
  "Parse nested command string recursively.

NESTED-COMMAND-STRING is the command string to parse (may have outer quotes).
NESTING-LEVEL is optional recursion depth counter (default 1).
PARENT-COMMAND is optional reference to outer command structure.

Returns parsed command structure with metadata:
  :nested-level - Recursion depth (1 = first nested, 2+ = deeper)
  :parent-command - Reference to outer command (optional)
  :success - t if parsing succeeded
  :error - Error message if parsing failed

Recursion termination:
  - Maximum depth: 10 levels (prevents infinite recursion)
  - Detection failure: No more injection patterns found
  - Parse failure: Command string is invalid

Quote handling:
  - Strips outer quotes before parsing: 'cat file' -> cat file
  - Preserves inner quotes: 'grep \"pattern\" file' -> grep \"pattern\" file
  - Handles both single and double quotes

Examples:
  ;; Single nesting
  (jf/bash-parse-nested-command \"'rm file.txt'\")
  => (:command-name \"rm\" :positional-args (\"file.txt\")
      :nested-level 1 :success t ...)

  ;; Multiple nesting
  (jf/bash-parse-nested-command \"'bash -c \\\"rm file\\\"'\")
  => (:command-name \"bash\" :flags (\"-c\")
      :positional-args (\"rm file\")
      :nested-command (:command-name \"rm\" :positional-args (\"file\")
                       :nested-level 2 :success t ...)
      :nested-level 1 :success t ...)

  ;; Maximum depth exceeded
  (jf/bash-parse-nested-command (deeply-nested-command) 11)
  => (:success nil :error \"Maximum nesting depth exceeded\")

Integration:
  This function is called by file operation extraction to recursively
  extract operations from nested commands. Operations from nested commands
  are marked with :indirect t to indicate indirect execution.

Nesting levels:
  - 0: Top-level command (not nested, parsed via jf/bash-parse)
  - 1: First level of nesting (bash -c 'cmd')
  - 2+: Deeper nesting levels (bash -c \"bash -c 'cmd'\")"
  (let ((level (or nesting-level 1)))
    ;; Check recursion depth limit
    (if (>= level jf/bash-recursive-max-depth)
        (list :success nil
              :error (format "Maximum nesting depth exceeded (limit: %d)" jf/bash-recursive-max-depth)
              :nested-level level)

      ;; Strip outer quotes and parse
      (let* ((stripped (jf/bash--strip-outer-quotes nested-command-string))
             (parsed (jf/bash-parse stripped)))

        (when (plist-get parsed :success)
          ;; Add nesting level metadata
          (setq parsed (plist-put parsed :nested-level level))

          ;; Add parent command reference if provided
          (when parent-command
            (setq parsed (plist-put parsed :parent-command parent-command)))

          ;; Check if this parsed command itself contains injection
          (when-let ((injection-info (jf/bash-detect-command-injection parsed)))
            (let ((nested-cmd-string (plist-get injection-info :nested-command-string)))
              ;; Recursively parse the nested injection
              (when nested-cmd-string
                (let ((nested-parsed (jf/bash-parse-nested-command
                                     nested-cmd-string
                                     (1+ level)
                                     parsed)))
                  ;; Add nested command to parsed structure
                  (setq parsed (plist-put parsed :nested-command nested-parsed)))))))

        parsed))))

(provide 'bash-parser-extensions)
;;; bash-parser-extensions.el ends here
