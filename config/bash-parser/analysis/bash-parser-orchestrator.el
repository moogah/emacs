;;; bash-parser-orchestrator.el --- Semantic extraction orchestrator for bash parser -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun jf/bash--decompose-to-simple-commands (parsed-command &optional var-context)
  "Decompose PARSED-COMMAND into simple commands with context (Layer 0).

Pure grammar decomposition. Walks compound structures recursively,
yields simple commands with accumulated var-context and grammar-level
redirection operations. Does NOT call command handlers.

PARSED-COMMAND is the output from `jf/bash-parse'.
VAR-CONTEXT is an optional alist mapping variable names to values.

Returns list of plists, each with:
  :command     - simple command plist
  :var-context - accumulated variable context for this command
  :redirection-ops - operations from redirections
  :metadata    - context flags (:subshell-context, :conditional, :branch, etc.)"
  (jf/bash--decompose-recursive parsed-command (or var-context '()) nil 0))

(defun jf/bash--decompose-recursive (parsed-command var-context metadata depth)
  "Recursively decompose PARSED-COMMAND into simple command entries.

PARSED-COMMAND is a parsed command plist.
VAR-CONTEXT is the accumulated variable context alist.
METADATA is a plist of context flags for the current scope.
DEPTH is the current recursion depth.

Returns a list of entry plists (see `jf/bash--decompose-to-simple-commands')."
  (when (or (null parsed-command) (not (listp parsed-command)))
    (cl-return-from jf/bash--decompose-recursive nil))
  (when (> depth (or jf/bash-recursive-max-depth 10))
    (message "Warning: Decomposition max depth %d exceeded" depth)
    (cl-return-from jf/bash--decompose-recursive nil))

  (let ((type (plist-get parsed-command :type))
        (command-name (plist-get parsed-command :command-name)))
    (cond
     ;; Pipeline: decompose each segment
     ((eq type :pipeline)
      (jf/bash--decompose-pipeline parsed-command var-context metadata depth))

     ;; Chain: decompose with var-context accumulation
     ((eq type :chain)
      (jf/bash--decompose-chain parsed-command var-context metadata depth))

     ;; Conditional: decompose branches
     ((eq type :conditional)
      (jf/bash--decompose-conditional parsed-command var-context metadata depth))

     ;; Subshell: decompose body with isolated context
     ((eq type :subshell)
      (jf/bash--decompose-subshell parsed-command var-context metadata depth))

     ;; For-loop: decompose body with loop variable context
     ((and command-name (string= command-name "for"))
      (jf/bash--decompose-for-loop parsed-command var-context metadata depth))

     ;; Simple command (has command-name or is :simple type)
     ((or (eq type :simple) command-name (plist-get parsed-command :tokens))
      (jf/bash--make-entry parsed-command var-context metadata depth))

     ;; Nothing to decompose
     (t nil))))

(defun jf/bash--make-entry (simple-command var-context metadata &optional depth)
  "Create a decomposition entry for SIMPLE-COMMAND.

SIMPLE-COMMAND is a parsed simple command plist.
VAR-CONTEXT is the variable context alist.
METADATA is a plist of context flags.
DEPTH is the current recursion depth (for processing command substitutions).

Returns a list of entry plists — one for the command itself, plus entries
for any command substitutions found in the command."
  (let ((redir-ops (jf/bash-extract-operations-from-redirections
                    simple-command var-context))
        (entries nil))
    ;; Primary entry for this simple command
    (push (list :command simple-command
                :var-context var-context
                :redirection-ops redir-ops
                :metadata (or metadata '()))
          entries)
    ;; Process command substitutions: recursively decompose each
    ;; substitution's :parsed content with :from-substitution metadata
    (when-let ((substitutions (plist-get simple-command :command-substitutions)))
      (let ((sub-metadata (append (list :from-substitution t) (or metadata '()))))
        (dolist (sub substitutions)
          (when-let ((parsed-sub (plist-get sub :parsed)))
            (when (plist-get parsed-sub :success)
              (let* ((cleaned (copy-sequence parsed-sub))
                     (_ (plist-put cleaned :ast nil))
                     (sub-entries (jf/bash--decompose-recursive
                                   cleaned var-context sub-metadata
                                   (1+ (or depth 0)))))
                (setq entries (append entries sub-entries))))))))
    ;; Shell wrapper -c processing: parse and decompose inner command string
    ;; e.g., `bash -c 'rm file.txt'` → decompose `rm file.txt` as indirect
    (let ((cmd-name (plist-get simple-command :command-name)))
      (when (and cmd-name
                 (member cmd-name '("bash" "sh" "dash" "zsh" "ksh"))
                 (member "-c" (plist-get simple-command :flags)))
        (when-let ((inner-cmd-string (car (plist-get simple-command :positional-args))))
          (condition-case nil
              (let* ((parsed-inner (jf/bash-parse inner-cmd-string))
                     (current-depth (or (plist-get metadata :nesting-depth) 0))
                     ;; Build metadata removing old :indirect/:nesting-depth keys
                     ;; to avoid duplicates (annotate-ops iterates all pairs)
                     (base-metadata
                      (cl-loop for (k v) on (or metadata '()) by #'cddr
                               unless (memq k '(:indirect :nesting-depth))
                               append (list k v)))
                     (wrapper-metadata (append
                                        (list :indirect t
                                              :nesting-depth (1+ current-depth))
                                        base-metadata)))
                (when (plist-get parsed-inner :success)
                  (let* ((cleaned (copy-sequence parsed-inner))
                         (_ (plist-put cleaned :ast nil))
                         (inner-entries (jf/bash--decompose-recursive
                                        cleaned var-context wrapper-metadata
                                        (1+ (or depth 0)))))
                    (setq entries (append entries inner-entries)))))
            (error nil)))))
    entries))

(defun jf/bash--decompose-pipeline (parsed-command var-context metadata depth)
  "Decompose pipeline PARSED-COMMAND into simple command entries.

Each pipeline segment is decomposed independently with the same var-context."
  (let ((entries nil))
    (dolist (cmd (plist-get parsed-command :all-commands))
      (setq entries (append entries
                           (jf/bash--decompose-recursive
                            cmd var-context metadata (1+ depth)))))
    entries))

(defun jf/bash--alist-remove-key (key alist)
  "Return a new alist with all entries whose car is `eq' to KEY removed.

Unlike `assq-delete-all', this function is non-destructive and does not
modify the original ALIST.  This is critical in the chain decomposer where
earlier entries share list structure with the accumulating context."
  (cl-remove-if (lambda (pair) (eq (car pair) key)) alist))

(defun jf/bash--decompose-chain (parsed-command var-context metadata depth)
  "Decompose chain PARSED-COMMAND with var-context accumulation.

Tracks variable assignments, cd, pushd, and popd across chain commands
so that later commands see the accumulated context.

IMPORTANT: Uses non-destructive alist operations to avoid corrupting
contexts already stored in earlier entries.  Previous use of
`assq-delete-all' (destructive) caused later cd/pushd/popd commands
to silently remove PWD from entries created earlier in the same chain."
  (let ((entries nil)
        (chain-context var-context)
        (current-pwd (or (alist-get 'PWD var-context) "/"))
        (dir-stack nil))
    (dolist (cmd (plist-get parsed-command :all-commands))
      ;; Extract variable assignments from this command
      (when (fboundp 'jf/bash--extract-assignments-from-command)
        (let ((assignments (jf/bash--extract-assignments-from-command cmd chain-context)))
          (when assignments
            (setq chain-context (append assignments chain-context))
            (when-let ((new-pwd (alist-get 'PWD assignments)))
              (setq current-pwd new-pwd)))))

      ;; Check for cd command
      (when (and (fboundp 'jf/bash--is-cd-command-p)
                 (jf/bash--is-cd-command-p cmd))
        (when-let ((new-pwd (jf/bash--extract-cd-target cmd chain-context current-pwd)))
          (unless (eq new-pwd :unresolved)
            (let ((old-pwd current-pwd))
              (setq current-pwd new-pwd)
              (setq chain-context (cons (cons 'PWD current-pwd)
                                       (jf/bash--alist-remove-key 'PWD chain-context)))
              (setq chain-context (cons (cons 'OLDPWD old-pwd)
                                       (jf/bash--alist-remove-key 'OLDPWD chain-context)))))))

      ;; Check for pushd command
      (when (and (fboundp 'jf/bash--is-pushd-command-p)
                 (jf/bash--is-pushd-command-p cmd))
        (when-let ((new-pwd (jf/bash--extract-pushd-target cmd chain-context current-pwd)))
          (unless (eq new-pwd :unresolved)
            (push current-pwd dir-stack)
            (setq current-pwd new-pwd)
            (setq chain-context (cons (cons 'PWD current-pwd)
                                     (jf/bash--alist-remove-key 'PWD chain-context))))))

      ;; Check for popd command
      (when (and (fboundp 'jf/bash--is-popd-command-p)
                 (jf/bash--is-popd-command-p cmd))
        (when dir-stack
          (let ((popped-pwd (pop dir-stack)))
            (setq current-pwd popped-pwd)
            (setq chain-context (cons (cons 'PWD current-pwd)
                                     (jf/bash--alist-remove-key 'PWD chain-context))))))

      ;; Decompose this chain command with accumulated context
      (setq entries (append entries
                           (jf/bash--decompose-recursive
                            cmd chain-context metadata (1+ depth)))))
    entries))

(defun jf/bash--extract-test-condition-ops (condition-text var-context metadata)
  "Extract file test operations from CONDITION-TEXT.

Handles test commands: [ -OP file ], test -OP file, [[ -OP file ]].
File test operators (-f, -d, -e, -r, -w, -x, -s, -L, -h) produce
:read-metadata operations.

Returns a list of operation plists or nil."
  (when (and condition-text (stringp condition-text))
    (let ((ops nil))
      ;; Match [ -op file ] or test -op file or [[ -op file ]]
      (when (string-match
             "\\(?:\\[\\[?\\|test\\)\\s-+\\(-[fderwxsLh]\\)\\s-+\\(?:\"\\)?\\([^]\" \t]+\\)"
             condition-text)
        (let* ((operator (match-string 1 condition-text))
               (file (match-string 2 condition-text))
               (resolved-file file))
          ;; Resolve variables and relative paths in the file path
          (when (and var-context
                     (fboundp 'jf/bash--resolve-path-variables))
            (let ((result (jf/bash--resolve-path-variables file var-context)))
              (cond
               ((stringp result) (setq resolved-file result))
               ((and (listp result) (plist-get result :path))
                (setq resolved-file (plist-get result :path))))))
          (push (list :file resolved-file
                      :operation :read-metadata
                      :confidence :high
                      :source :test-condition
                      :test-condition t
                      :test-operator operator)
                ops)))
      ops)))

(defun jf/bash--extract-cd-from-condition (condition-text var-context)
  "Check if CONDITION-TEXT is a cd command and return updated var-context.

For conditionals like `if cd /dir; then ...', the cd in the condition
updates PWD for the then-branch (static analysis assumes success path).

Returns updated var-context with new PWD, or nil if not a cd condition."
  (when (and condition-text (stringp condition-text))
    (condition-case nil
        (let* ((parsed-cond (jf/bash-parse condition-text))
               (cmd-name (plist-get parsed-cond :command-name)))
          (when (and cmd-name (string= cmd-name "cd"))
            (let* ((current-pwd (or (alist-get 'PWD var-context) "/"))
                   (new-pwd (jf/bash--extract-cd-target parsed-cond var-context current-pwd)))
              (when (and new-pwd (not (eq new-pwd :unresolved)))
                (cons (cons 'PWD new-pwd)
                      (jf/bash--alist-remove-key 'PWD var-context))))))
      (error nil))))

(defun jf/bash--decompose-conditional (parsed-command var-context metadata depth)
  "Decompose conditional PARSED-COMMAND into entries from branches.

Extracts test condition operations from :condition-text, then
processes then and else branches with appropriate metadata.
If the condition is a cd command, passes updated PWD to the then-branch."
  (let ((entries nil)
        (condition-text (plist-get parsed-command :condition-text))
        (then-text (plist-get parsed-command :then-text))
        (else-text (plist-get parsed-command :else-text))
        (then-context var-context))

    ;; Extract test condition operations (grammar-level)
    (when condition-text
      (let ((test-ops (jf/bash--extract-test-condition-ops
                       condition-text var-context metadata)))
        (if test-ops
            ;; Bracket-style test: create synthetic entry
            (push (list :command (list :command-name "[" :type :simple)
                        :var-context var-context
                        :redirection-ops test-ops
                        :metadata (append (list :test-condition t) metadata))
                  entries)
          ;; Command-based condition (e.g., `grep -q pattern file.txt`):
          ;; parse and decompose as a regular command with :test-condition metadata
          (condition-case nil
              (let* ((parsed-cond (jf/bash-parse condition-text))
                     (clean-cond (when (plist-get parsed-cond :success)
                                   (let ((cleaned (copy-sequence parsed-cond)))
                                     (plist-put cleaned :ast nil)
                                     cleaned)))
                     (cond-metadata (append (list :test-condition t :conditional t)
                                           metadata))
                     (cond-entries (when clean-cond
                                    (jf/bash--decompose-recursive
                                     clean-cond var-context cond-metadata (1+ depth)))))
                (setq entries (append entries cond-entries)))
            (error nil))))

      ;; Check if condition is a cd command — update context for then-branch
      (when-let ((cd-context (jf/bash--extract-cd-from-condition
                              condition-text var-context)))
        (setq then-context cd-context)))

    ;; Decompose then branch (uses cd-updated context if condition was cd)
    (when then-text
      (condition-case nil
          (let* ((parsed-then (jf/bash-parse then-text))
                 (clean-parsed (when (plist-get parsed-then :success)
                                 (let ((cleaned (copy-sequence parsed-then)))
                                   (plist-put cleaned :ast nil)
                                   cleaned)))
                 (then-metadata (append (list :conditional t :branch :then)
                                        metadata))
                 (then-entries (when clean-parsed
                                (jf/bash--decompose-recursive
                                 clean-parsed then-context then-metadata (1+ depth)))))
            (setq entries (append entries then-entries)))
        (error nil)))

    ;; Decompose else branch
    (when else-text
      (condition-case nil
          (let* ((parsed-else (jf/bash-parse else-text))
                 (clean-parsed (when (plist-get parsed-else :success)
                                 (let ((cleaned (copy-sequence parsed-else)))
                                   (plist-put cleaned :ast nil)
                                   cleaned)))
                 (else-metadata (append (list :conditional t :branch :else)
                                        metadata))
                 (else-entries (when clean-parsed
                                (jf/bash--decompose-recursive
                                 clean-parsed var-context else-metadata (1+ depth)))))
            (setq entries (append entries else-entries)))
        (error nil)))

    entries))

(defun jf/bash--decompose-subshell (parsed-command var-context metadata depth)
  "Decompose subshell PARSED-COMMAND with isolated context and metadata."
  (let ((entries nil))
    (when-let ((subshell-body (plist-get parsed-command :subshell-body)))
      (when (plist-get subshell-body :success)
        (let* ((isolated-context (copy-alist var-context))
               (subshell-metadata (append (list :subshell-context t) metadata))
               (body-entries (jf/bash--decompose-recursive
                              subshell-body isolated-context
                              subshell-metadata (1+ depth))))
          (setq entries body-entries))))
    entries))

(defun jf/bash--extract-find-name-pattern (parsed-sub)
  "Extract the -name pattern from a parsed find command PARSED-SUB.
Returns the pattern string or nil."
  (let ((flags (plist-get parsed-sub :flags))
        (args (plist-get parsed-sub :positional-args))
        (result nil))
    (when (and flags args (>= (length args) 2))
      (let ((arg-idx 1))  ; skip directory (first positional)
        (dolist (flag flags)
          (when (and (string= flag "-name")
                     (< arg-idx (length args)))
            (setq result (nth arg-idx args))))))
    result))

(defun jf/bash--decompose-for-loop (parsed-command var-context metadata depth)
  "Decompose for-loop PARSED-COMMAND body with loop variable in context.

For glob sources, emits a :match-pattern operation at the grammar level.
For substitution sources, decomposes the substitution and binds the loop
variable to the pattern from the substituted command."
  (let* ((entries nil)
         (loop-var (plist-get parsed-command :loop-variable))
         (loop-source-type (plist-get parsed-command :loop-source-type))
         (loop-list (plist-get parsed-command :loop-list))
         (loop-body (plist-get parsed-command :loop-body))
         (loop-var-value nil)
         (loop-metadata (append (list :loop-context t)
                               (when loop-var (list :loop-variable loop-var))
                               metadata)))
    ;; Determine loop variable value
    (cond
     ((eq loop-source-type :glob)
      (setq loop-var-value loop-list))
     ((eq loop-source-type :literal)
      (setq loop-var-value :literal-list))
     ((eq loop-source-type :substitution)
      ;; Decompose command substitutions in the loop source
      (let ((substitutions (plist-get parsed-command :command-substitutions))
            (sub-metadata (append (list :from-substitution t) loop-metadata)))
        (dolist (sub substitutions)
          (let ((parsed-sub (plist-get sub :parsed)))
            (when (and parsed-sub (plist-get parsed-sub :success))
              (let* ((cleaned (copy-sequence parsed-sub))
                     (_ (plist-put cleaned :ast nil))
                     (sub-entries (jf/bash--decompose-recursive
                                   cleaned var-context sub-metadata
                                   (1+ depth))))
                (setq entries (append entries sub-entries)))
              ;; Bind loop variable from substitution's -name pattern
              (unless loop-var-value
                (when (and (plist-get parsed-sub :command-name)
                           (string= (plist-get parsed-sub :command-name) "find"))
                  (setq loop-var-value
                        (jf/bash--extract-find-name-pattern parsed-sub))))))))))

    ;; Emit :match-pattern for glob source (grammar-level operation)
    ;; Resolve relative paths in glob patterns against PWD context
    (when (and (eq loop-source-type :glob) loop-list)
      (let* ((resolved-glob loop-list)
             (_ (when (and var-context (fboundp 'jf/bash--resolve-path-variables))
                  (let ((result (jf/bash--resolve-path-variables loop-list var-context)))
                    (cond
                     ((stringp result) (setq resolved-glob result))
                     ((and (listp result) (plist-get result :path))
                      (setq resolved-glob (plist-get result :path)))))))
             (glob-ops (list (list :file resolved-glob
                                   :operation :match-pattern
                                   :confidence :high
                                   :source :loop-glob
                                   :pattern t
                                   :loop-variable loop-var))))
        (push (list :command (list :command-name "for" :type :simple)
                    :var-context var-context
                    :redirection-ops glob-ops
                    :metadata loop-metadata)
              entries)))

    ;; Parse and decompose loop body
    (when loop-body
      (condition-case nil
          (let* ((parsed-body (jf/bash-parse loop-body))
                 (clean-parsed (when (plist-get parsed-body :success)
                                 (let ((cleaned (copy-sequence parsed-body)))
                                   (plist-put cleaned :ast nil)
                                   cleaned)))
                 (loop-context (if (and loop-var loop-var-value
                                        (not (keywordp loop-var-value)))
                                  (append (list (cons (intern loop-var) loop-var-value))
                                          var-context)
                                var-context))
                 (body-entries (when clean-parsed
                                (jf/bash--decompose-recursive
                                 clean-parsed loop-context
                                 loop-metadata (1+ depth)))))
            (setq entries (append entries body-entries)))
        (error nil)))
    entries))

(defun jf/bash--annotate-ops-with-metadata (operations metadata)
  "Add METADATA plist keys to each operation in OPERATIONS.

OPERATIONS is a list of operation plists.
METADATA is a plist of context flags (e.g., :conditional t :branch :then).

Returns new list with each operation augmented with metadata keys.
Does not modify the original operations."
  (if (or (null metadata) (null operations))
      operations
    (mapcar (lambda (op)
              (let ((annotated (copy-sequence op)))
                (cl-loop for (key val) on metadata by #'cddr
                         do (plist-put annotated key val))
                annotated))
            operations)))

(defun jf/bash--normalize-var-context (var-context)
  "Ensure all keys in VAR-CONTEXT are symbols.

VAR-CONTEXT is an alist mapping variable names to values.
Keys may be strings or symbols; this function normalizes string keys
to symbols so that `jf/bash-resolve-variables' can look them up.

Returns a new alist with all keys as symbols."
  (mapcar (lambda (pair)
            (if (stringp (car pair))
                (cons (intern (car pair)) (cdr pair))
              pair))
          var-context))

(defun jf/bash--claim-tokens-for-results (all-operations entries parsed-command)
  "Claim token IDs based on ALL-OPERATIONS and ENTRIES for PARSED-COMMAND.

ALL-OPERATIONS is the flat list of all operations from both layers.
ENTRIES is the decomposition entries list.
PARSED-COMMAND is the top-level parse result.

Claims tokens by matching against the top-level :tokens list:
  - All :redirection type tokens (grammar-level, always understood)
  - All :operator type tokens (structural, always understood)
  - :command-name tokens matching processed command names
  - Tokens whose :value matches an operation :file path

Returns list of claimed token IDs."
  (let ((claimed-ids '())
        (tokens (plist-get parsed-command :tokens))
        (command-names (mapcar (lambda (e)
                                 (plist-get (plist-get e :command) :command-name))
                               entries))
        (op-files (delq nil (mapcar (lambda (op) (plist-get op :file))
                                     all-operations))))
    (when tokens
      (dolist (token tokens)
        (let ((token-type (plist-get token :type))
              (token-value (plist-get token :value))
              (token-id (plist-get token :id)))
          (when token-id
            (cond
             ;; Always claim redirection and operator tokens
             ((memq token-type '(:redirection :operator))
              (push token-id claimed-ids))
             ;; Claim command-name tokens for processed commands
             ((and (eq token-type :command-name)
                   token-value
                   (member token-value command-names))
              (push token-id claimed-ids))
             ;; Claim tokens whose value matches an operation file path
             ((and token-value
                   (member token-value op-files))
              (push token-id claimed-ids)))))))
    (delete-dups claimed-ids)))

(defun jf/bash--resolve-handler-filesystem-ops (operations var-context)
  "Resolve variable references in file paths of handler OPERATIONS.

OPERATIONS is a list of operation plists from command handlers.
VAR-CONTEXT is the variable context alist.

Returns operations with resolved file paths and updated metadata."
  (let ((resolved nil))
    (dolist (op operations)
      (let* ((file (plist-get op :file))
             (resolved-file file)
             (confidence (or (plist-get op :confidence) :high))
             (has-pattern nil)
             (unresolved-vars nil))
        ;; Full path resolution with context (variables + pwd + relative paths)
        (when (and file var-context (fboundp 'jf/bash--resolve-path-variables))
          (let ((result (jf/bash--resolve-path-variables file var-context)))
            (cond
             ((stringp result)
              (setq resolved-file result))
             ((and (listp result) (plist-get result :path))
              (setq resolved-file (plist-get result :path))
              (when (plist-get result :unresolved)
                (setq unresolved-vars (plist-get result :unresolved))
                (unless (eq (plist-get op :source) :redirection)
                  (setq confidence :medium)))))))
        ;; Without context: detect unresolved variables (no path transforms)
        (when (and file (not var-context) (string-match-p "\\$" file)
                   (fboundp 'jf/bash-resolve-variables))
          (let ((result (jf/bash-resolve-variables file nil)))
            (when (and (listp result) (plist-get result :unresolved))
              (setq unresolved-vars (plist-get result :unresolved))
              (unless (eq (plist-get op :source) :redirection)
                (setq confidence :medium)))))
        ;; Detect glob patterns
        (when (and resolved-file (fboundp 'jf/bash--has-glob-pattern-p))
          (setq has-pattern (or (plist-get op :pattern)
                                (jf/bash--has-glob-pattern-p resolved-file))))
        ;; Build resolved operation preserving all handler properties
        (let ((resolved-op (plist-put (copy-sequence op) :file resolved-file)))
          (plist-put resolved-op :confidence confidence)
          (when (and (not (plist-get resolved-op :source))
                     (plist-get op :command))
            (plist-put resolved-op :source :positional-arg))
          (when has-pattern
            (plist-put resolved-op :pattern t))
          ;; Propagate unresolved variable metadata
          (when unresolved-vars
            (plist-put resolved-op :unresolved t)
            (plist-put resolved-op :unresolved-vars unresolved-vars))
          (push resolved-op resolved))))
    (nreverse resolved)))

(defun jf/bash-extract-semantics (parsed-command &optional var-context)
  "Extract semantic information from PARSED-COMMAND.

Implements a clean two-layer extraction architecture:
  Layer 0: Grammar decomposition (pure)
           Walk compound structures into simple commands.
           Extract redirections per simple command. No handler calls.
  Layer 1: Command handlers (per simple command)
           Called exactly once per simple command from Layer 0.
           Produces all domain operations.
  Merge: Combine Layer 0 redirection ops + Layer 1 handler ops.
         Build claimed-token-ids. Calculate coverage.

PARSED-COMMAND is a plist from `jf/bash-parse' with:
  :tokens - List of token plists
  :parse-complete - Boolean indicating if parse was fully understood

VAR-CONTEXT is an optional alist mapping variable names to values.

Returns a plist with:
  :domains - Alist of (domain . operations) from all extraction layers
  :coverage - Coverage plist from `jf/bash-calculate-coverage'
  :parse-complete - Pass-through from input"
  (let* ((tokens (plist-get parsed-command :tokens))
         (parse-complete (plist-get parsed-command :parse-complete))
         (normalized-context (jf/bash--normalize-var-context var-context))
         (all-claimed-token-ids '())
         (domains-alist '()))

    ;; Layer 0: Decompose into simple commands (pure grammar, no handlers)
    (let ((entries (jf/bash--decompose-to-simple-commands
                    parsed-command normalized-context)))

      ;; Process each simple command entry
      (dolist (entry entries)
        (let* ((cmd (plist-get entry :command))
               (ctx (plist-get entry :var-context))
               (redir-ops (plist-get entry :redirection-ops))
               (metadata (plist-get entry :metadata)))

          ;; Apply inline environment variables (e.g., PWD=/new/path cat file.txt)
          ;; These override var-context for this command only
          (when-let ((env-vars (plist-get cmd :env-vars)))
            (setq ctx (append env-vars ctx)))

          ;; Layer 0 contribution: redirection ops → :filesystem domain
          ;; Annotate with entry metadata (conditional, loop, subshell context)
          (when redir-ops
            (let ((annotated-ops (jf/bash--annotate-ops-with-metadata
                                  redir-ops metadata))
                  (existing (assq :filesystem domains-alist)))
              (if existing
                  (setcdr existing (append (cdr existing) annotated-ops))
                (push (cons :filesystem annotated-ops) domains-alist))))

          ;; Self-execution detection: path-based commands (./script.sh, /usr/bin/tool)
          (when (and (fboundp 'jf/bash--command-executes-self-p)
                     (plist-get cmd :command-name)
                     (jf/bash--command-executes-self-p (plist-get cmd :command-name)))
            (let* ((cmd-name (plist-get cmd :command-name))
                   (positional-args (plist-get cmd :positional-args))
                   (args (plist-get cmd :args))
                   ;; Resolve the command path against PWD (e.g., ./script.sh → /base/dir/script.sh)
                   (resolved-name cmd-name)
                   (_ (when (and ctx (fboundp 'jf/bash--resolve-path-variables))
                        (let ((result (jf/bash--resolve-path-variables cmd-name ctx)))
                          (cond
                           ((stringp result) (setq resolved-name result))
                           ((and (listp result) (plist-get result :path))
                            (setq resolved-name (plist-get result :path)))))))
                   ;; Detect unresolved variables in the command name
                   (unresolved-vars
                    (when (string-match-p "\\$" resolved-name)
                      (if (and ctx (fboundp 'jf/bash--resolve-path-variables))
                          (let ((result (jf/bash--resolve-path-variables resolved-name ctx)))
                            (when (and (listp result) (plist-get result :unresolved))
                              (plist-get result :unresolved)))
                        (when (fboundp 'jf/bash-resolve-variables)
                          (let ((result (jf/bash-resolve-variables resolved-name nil)))
                            (when (and (listp result) (plist-get result :unresolved))
                              (plist-get result :unresolved)))))))
                   (self-op (append
                             (list :file resolved-name
                                   :operation :execute
                                   :confidence :low
                                   :source :command-name
                                   :self-executing t
                                   :script-args (or args positional-args '()))
                             (when unresolved-vars
                               (list :unresolved t
                                     :unresolved-vars unresolved-vars))))
                   (annotated (jf/bash--annotate-ops-with-metadata
                               (list self-op) metadata))
                   (existing (assq :filesystem domains-alist)))
              (if existing
                  (setcdr existing (append (cdr existing) annotated))
                (push (cons :filesystem annotated) domains-alist))))

          ;; Layer 1: Call handler exactly once for this simple command
          (condition-case err
              (let* ((cmd-result (jf/bash-extract-command-semantics cmd))
                     (cmd-domains (plist-get cmd-result :domains))
                     (cmd-claimed-ids (plist-get cmd-result :claimed-token-ids)))

                ;; Resolve variables in filesystem handler ops
                (when-let ((fs-entry (assq :filesystem cmd-domains)))
                  (let ((resolved (jf/bash--resolve-handler-filesystem-ops
                                   (cdr fs-entry) ctx)))
                    (setcdr fs-entry resolved)))

                ;; Merge all domain results, annotating with entry metadata
                (dolist (domain-entry cmd-domains)
                  (let ((domain (car domain-entry))
                        (operations (cdr domain-entry)))
                    (when operations
                      (let ((annotated-ops (jf/bash--annotate-ops-with-metadata
                                            operations metadata))
                            (existing (assq domain domains-alist)))
                        (if existing
                            (setcdr existing (append (cdr existing) annotated-ops))
                          (push (cons domain annotated-ops) domains-alist))))))

                ;; Collect handler claimed token IDs
                (when cmd-claimed-ids
                  (setq all-claimed-token-ids
                        (append cmd-claimed-ids all-claimed-token-ids))))
            (error
             (message "Handler error for %s: %s"
                      (plist-get cmd :command-name)
                      (error-message-string err))))))

      ;; Post-merge: Pattern flow tracking
      ;; Link outer commands to match-pattern ops from their command substitutions
      (when (fboundp 'jf/bash--infer-operation-from-command)
        (let ((sub-patterns nil))
          ;; Collect match-pattern ops produced by substitution entries
          (when-let ((fs-ops (cdr (assq :filesystem domains-alist))))
            (dolist (op fs-ops)
              (when (and (eq (plist-get op :operation) :match-pattern)
                         (plist-get op :from-substitution))
                (push op sub-patterns))))
          ;; For each non-substitution entry with $(…) in positional args
          (when sub-patterns
            (dolist (entry entries)
              (let* ((cmd (plist-get entry :command))
                     (metadata (plist-get entry :metadata))
                     (cmd-name (plist-get cmd :command-name))
                     (positional-args (plist-get cmd :positional-args)))
                (when (and cmd-name
                           (not (plist-get metadata :from-substitution))
                           positional-args
                           (seq-find (lambda (a) (and a (string-prefix-p "$(" a)))
                                     positional-args))
                  ;; Create pattern flow ops for each substitution pattern
                  (dolist (pat-op sub-patterns)
                    (let* ((pattern (plist-get pat-op :file))
                           (operation (jf/bash--infer-operation-from-command
                                      cmd-name pattern)))
                      (when (and pattern operation)
                        (let* ((pat-cmd (plist-get pat-op :command))
                               (search-scope
                                (when (string= pat-cmd "find")
                                  (let ((scope-op (seq-find
                                                   (lambda (o)
                                                     (and (eq (plist-get o :operation)
                                                              :read-directory)
                                                          (string= (plist-get o :command)
                                                                   "find")
                                                          (plist-get o :from-substitution)))
                                                   (cdr (assq :filesystem domains-alist)))))
                                    (when scope-op (plist-get scope-op :file)))))
                               (flow-op (list :file pattern
                                              :operation operation
                                              :confidence :high
                                              :source :positional-arg
                                              :command cmd-name
                                              :pattern t
                                              :from-substitution t
                                              :pattern-source
                                              (append
                                               (list :command pat-cmd
                                                     :from-substitution t)
                                               (when search-scope
                                                 (list :search-scope search-scope)))))
                               (annotated (jf/bash--annotate-ops-with-metadata
                                           (list flow-op) metadata))
                               (existing (assq :filesystem domains-alist)))
                          (if existing
                              (setcdr existing (append (cdr existing) annotated))
                            (push (cons :filesystem annotated) domains-alist))))))))))))

      ;; Token claiming: match operations against top-level tokens
      (let* ((all-operations
              (let ((ops nil))
                (dolist (domain-entry domains-alist)
                  (setq ops (append ops (cdr domain-entry))))
                ops))
             (entry-claimed (jf/bash--claim-tokens-for-results
                             all-operations entries parsed-command)))
        (setq all-claimed-token-ids
              (append entry-claimed all-claimed-token-ids))))

    ;; Deduplicate filesystem operations (same file + operation type)
    (when (fboundp 'jf/bash--deduplicate-operations)
      (when-let ((fs-entry (assq :filesystem domains-alist)))
        (setcdr fs-entry
                (jf/bash--deduplicate-operations (cdr fs-entry)))))

    ;; Deduplicate claimed IDs
    (setq all-claimed-token-ids (delete-dups all-claimed-token-ids))

    ;; Calculate coverage
    (let ((coverage (jf/bash-calculate-coverage tokens all-claimed-token-ids)))
      (list :domains domains-alist
            :coverage coverage
            :parse-complete parse-complete))))

(require 'bash-parser-coverage)

(require 'bash-parser-file-ops)

;; Load command handler index (auto-discovers and loads all command handlers)
(let ((index-path (expand-file-name "config/bash-parser/commands/index.el" jf/emacs-dir)))
  (when (file-exists-p index-path)
    (load index-path nil t)))

(provide 'bash-parser-orchestrator)
;;; bash-parser-orchestrator.el ends here
