;; Dependencies


;; [[file:scope-core.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-logging)
;; Dependencies:1 ends here

;; Tool Category Constant

;; The core categorization system maps each tool to its validation strategy and operation type.


;; [[file:scope-core.org::*Tool Category Constant][Tool Category Constant:1]]
(defconst jf/gptel-scope--tool-categories
  '(;; Path-based: read operations
    ("read_file" . (:validation path :operation read))
    ("list_project_files" . (:validation path :operation read))
    ("list_project_directories" . (:validation path :operation read))
    ("search_project_content" . (:validation path :operation read))
    ("list_test_files" . (:validation path :operation read))
    ("find_related_test" . (:validation path :operation read))
    ("find_related_files" . (:validation path :operation read))
    ("check_ggtags_project" . (:validation path :operation read))
    ("find_definition" . (:validation path :operation read))
    ("find_references" . (:validation path :operation read))
    ("find_symbol" . (:validation path :operation read))
    ("get_node_at_position" . (:validation path :operation read))
    ("get_node_info" . (:validation path :operation read))
    ("get_node_context" . (:validation path :operation read))
    ("get_syntax_tree" . (:validation path :operation read))
    ("list_functions" . (:validation path :operation read))
    ("list_classes" . (:validation path :operation read))
    ("list_imports" . (:validation path :operation read))
    ("extract_definition" . (:validation path :operation read))
    ("query_nodes" . (:validation path :operation read))
    ("find_nodes_by_type" . (:validation path :operation read))
    ("find_nodes_in_range" . (:validation path :operation read))
    ("get_scope_structure" . (:validation path :operation read))
    ("explain_ggtags_indexing" . (:validation path :operation read))

    ;; Path-based: write operations
    ("write_file_in_scope" . (:validation path :operation write))
    ("edit_file_in_scope" . (:validation path :operation write))
    ("create_ggtags_project" . (:validation path :operation write))
    ("update_ggtags_project" . (:validation path :operation write))

    ;; Pattern-based: org-roam operations
    ("create_roam_node_in_scope" . (:validation pattern :operation write))
    ("add_roam_tags_in_scope" . (:validation pattern :operation write))
    ("link_roam_nodes_in_scope" . (:validation pattern :operation write))

    ;; Command-based: shell operations

    ;; Bash validation: scoped bash commands
    ("run_bash_command" . (:validation bash :operation write))

    ;; Meta tools (always pass)
    ("PersistentAgent" . (:validation meta :operation delegate))
    ("request_scope_expansion" . (:validation meta :operation meta)))
  "Tool → validation strategy mapping.

Each tool maps to a plist with:
  :validation - Validation strategy (path, pattern, command, bash, meta)
  :operation - Operation type (read, write, delegate, meta)

Validation strategies:
  path    - Validate against paths.read/write/deny lists
  pattern - Validate against org_roam_patterns
  bash    - Validate against bash_tools.categories and paths
  meta    - Always allowed (no validation)

Operation types:
  read     - Read-only access
  write    - Write/modify access
  delegate - Delegate to sub-agent
  meta     - Meta-operations (inspect scope, request expansion)")
;; Tool Category Constant:1 ends here

;; Argument Normalization

;; Tool functions receive arguments as vectors (from JSON), but we need to work with lists internally.


;; [[file:scope-core.org::*Argument Normalization][Argument Normalization:1]]
(defun jf/gptel-scope--normalize-args (args)
  "Convert ARGS from vector to list if needed.
Tool functions receive vectors from JSON serialization."
  (if (vectorp args)
      (append args nil)
    args))
;; Argument Normalization:1 ends here

;; Generic Scoped Tool Macro

;; This macro wraps the common pattern used by all scope-aware tools:
;; 1. Get session ID
;; 2. Load scope plan
;; 3. Check plan exists
;; 4. Validate tool permission
;; 5. Execute tool body if allowed
;; 6. Return formatted error if denied


;; [[file:scope-core.org::*Generic Scoped Tool Macro][Generic Scoped Tool Macro:1]]
(defmacro gptel-make-scoped-tool (name description args category &rest body)
  "Create a scope-aware gptel tool with automatic validation.

NAME: Tool name string (e.g., \"write_file_in_scope\")
DESCRIPTION: Tool description for LLM
ARGS: List of argument specs (same format as gptel-make-tool :args)
CATEGORY: Resource category (\"filesystem\", \"org_roam\", \"shell\")
BODY: Tool implementation - executed only if scope check passes

The macro automatically:
- Loads scope config from scope.yml in current buffer's directory
- Checks tool permission using v3.0 validation
- Normalizes arguments (vector->list)
- Formats errors on scope violation
- Handles exceptions

BODY is executed with tool arguments available as variables and should
return the success result plist. The first argument in ARGS should be
the primary resource identifier (filepath, node-id, command, etc.)."
  (let* ((arg-names (mapcar (lambda (arg-spec)
                             (intern (plist-get arg-spec :name)))
                           (eval args)))
         (lambda-list arg-names))
    `(gptel-make-tool
      :name ,name
      :description ,description
      :args ,args
      :category ,category
      :function
      (lambda (&rest raw-args)
        (condition-case err
            (cl-block nil
              (let* ((normalized-args (jf/gptel-scope--normalize-args raw-args))
                   ,@(cl-mapcar (lambda (name idx)
                                 `(,name (nth ,idx normalized-args)))
                               arg-names
                               (number-sequence 0 (1- (length arg-names))))
                   (config (jf/gptel-scope--load-config)))

              ;; Check allow-once FIRST (before checking if config exists)
              ;; Allow-once permissions should work even if config loading fails
              (when (jf/gptel-scope--check-allow-once ,name normalized-args config)
                (cl-return-from nil (progn ,@body)))

              ;; Check config exists (only if allow-once didn't grant permission)
              (unless config
                (cl-return-from nil
                  (list :success nil
                        :error "no_scope_config"
                        :message "No scope configuration found. Ensure scope.yml exists with paths section.")))

              ;; Check tool permission
              (let ((check-result (jf/gptel-scope--check-tool-permission
                                  config ,name normalized-args)))
                (if (plist-get check-result :allowed)
                    ;; Execute tool body
                    (progn ,@body)

                  ;; Format and return error
                  ;; Note: Expansion UI is triggered by LLM via request_scope_expansion tool,
                  ;; not automatically here. This maintains LLM-mediated communication pattern.
                  (jf/gptel-scope--format-tool-error
                   ,name
                   (nth 0 normalized-args)  ; Primary resource (first arg)
                   check-result)))))

          ;; Handle unexpected errors
          (error
           (list :success nil
                 :error "tool_exception"
                 :message (format "Tool error: %s" (error-message-string err)))))))))
;; Generic Scoped Tool Macro:1 ends here

;; Load Scope Configuration

;; Load and parse scope configuration from scope.yml.

;; Convention: scope.yml is always in the same directory as session.md.


;; [[file:scope-core.org::*Load Scope Configuration][Load Scope Configuration:1]]
(require 'yaml)  ; Emacs built-in YAML parser

(defun jf/gptel-scope--load-config ()
  "Load scope configuration from scope.yml.
Returns plist with:
  :paths-read - List of allowed read paths
  :paths-write - List of allowed write paths
  :paths-deny - List of denied paths
  :org-roam-patterns - Plist with :subdirectory, :tags, :node-ids
  :bash-tools - Plist with :categories and :deny lists
                (YAML uses snake_case: bash_tools, read_only
                 Elisp uses kebab-case: :bash-tools, :read-only)

Uses buffer-local jf/gptel--branch-dir if available.
Returns nil if scope.yml not found or can't be parsed."
  (condition-case err
      (let ((context-dir (or (and (boundp 'jf/gptel--branch-dir) jf/gptel--branch-dir)
                             (and (buffer-file-name)
                                  (file-name-directory (buffer-file-name))))))
        (when context-dir
          (let ((scope-file (expand-file-name jf/gptel-session--scope-file context-dir)))
            (when (file-exists-p scope-file)
              (jf/gptel-scope--parse-scope-yml scope-file)))))
    (error
     (message "Error loading scope config: %s" (error-message-string err))
     nil)))
;; Load Scope Configuration:1 ends here

;; Key Normalization Helper

;; Normalize YAML keys by converting underscores to hyphens for internal consistency.

;; *Bidirectional conversion:*
;; - *Read path*: YAML =bash_tools= (snake_case) → Elisp =:bash-tools= (kebab-case)
;; - *Write path*: Elisp =:bash-tools= (kebab-case) → YAML =bash_tools= (snake_case)

;; This normalization is automatic and bidirectional:
;; 1. When loading scope.yml, =jf/gptel-scope--normalize-plist-keys= converts all underscore keys to hyphens
;; 2. When writing scope.yml, =jf/gptel-scope--kebab-to-snake= (in scope-expansion.org) converts hyphens back to underscores

;; *Example:* Users write =bash_tools= in YAML files, but code uses =:bash-tools= internally.

;; This is especially important for =bash_tools= since it's the most recent addition to the schema.


;; [[file:scope-core.org::*Key Normalization Helper][Key Normalization Helper:1]]
(defun jf/gptel-scope--normalize-plist-keys (plist)
  "Normalize all keys in PLIST by converting underscores to hyphens.
Recursively processes nested plists.
Example: :bash_tools -> :bash-tools, :read_only -> :read-only"
  (when plist
    (let ((result nil))
      (while plist
        (let* ((key (car plist))
               (value (cadr plist))
               (normalized-key (if (keywordp key)
                                   (intern (concat ":" (replace-regexp-in-string
                                                       "_" "-"
                                                       (substring (symbol-name key) 1))))
                                 key))
               (normalized-value (if (and (listp value)
                                         (keywordp (car value)))
                                    (jf/gptel-scope--normalize-plist-keys value)
                                  value)))
          (setq result (append result (list normalized-key normalized-value)))
          (setq plist (cddr plist))))
      result)))
;; Key Normalization Helper:1 ends here

;; Parse Scope YAML

;; Parse scope configuration from a plain YAML file (no frontmatter).
;; Automatically normalizes all keys from underscore to hyphenated format.


;; [[file:scope-core.org::*Parse Scope YAML][Parse Scope YAML:1]]
(defun jf/gptel-scope--parse-scope-yml (scope-file)
  "Parse scope configuration from SCOPE-FILE (plain YAML, no frontmatter).
Returns plist with :paths-read, :paths-write, :paths-deny,
:org-roam-patterns, and :bash-tools.

Automatically normalizes all keys: underscored keys (bash_tools) are
converted to hyphenated keys (bash-tools) for consistency."
  (with-temp-buffer
    (insert-file-contents scope-file)
    (let* ((parsed (yaml-parse-string (buffer-string)
                                      :object-type 'plist
                                      :sequence-type 'list))
           (normalized (jf/gptel-scope--normalize-plist-keys parsed))
           (paths (plist-get normalized :paths))
           (org-roam (plist-get normalized :org-roam-patterns))
           (bash-tools (plist-get normalized :bash-tools)))
      (list :paths-read (plist-get paths :read)
            :paths-write (plist-get paths :write)
            :paths-deny (plist-get paths :deny)
            :org-roam-patterns org-roam
            :bash-tools bash-tools))))
;; Parse Scope YAML:1 ends here

;; Buffer-Local Allow-Once List


;; [[file:scope-core.org::*Buffer-Local Allow-Once List][Buffer-Local Allow-Once List:1]]
(defvar-local jf/gptel-scope--allow-once-list nil
  "List of (tool-name . resource) pairs allowed for current LLM turn.
Each entry grants temporary permission for one tool call to one resource.
Cleared automatically after LLM response completes via gptel-post-response-functions.")
;; Buffer-Local Allow-Once List:1 ends here

;; Check Allow-Once


;; [[file:scope-core.org::*Check Allow-Once][Check Allow-Once:1]]
(defun jf/gptel-scope--check-allow-once (tool-name args config)
  "Check if TOOL-NAME with ARGS is in allow-once list.
Extracts resource from ARGS based on tool category.
Returns t if found (and consumes the permission), nil otherwise.

IMPORTANT: Permission is consumed when this function returns t, BEFORE the
tool body executes. This ensures single-use semantics but means permissions
cannot be retried if the tool body fails. See 'Consumption Semantics' above.

CONFIG is the scope configuration (used to determine tool category)."
  (when jf/gptel-scope--allow-once-list
    (let* ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
           (validation-type (plist-get category :validation))
           (resource (pcase validation-type
                      ('path (expand-file-name (car args)))
                      ('pattern (format "%s:%s" tool-name (car args)))
                      ('command (car args))
                      ('bash (format "%s:%s" (car args) (expand-file-name (cadr args))))  ; Composite: command:directory
                      (_ nil))))
      (when resource
        (when-let ((entry (assoc tool-name jf/gptel-scope--allow-once-list)))
          (when (equal (cdr entry) resource)
            ;; Consume permission (single-use)
            (setq jf/gptel-scope--allow-once-list
                  (delq entry jf/gptel-scope--allow-once-list))
            t))))))
;; Check Allow-Once:1 ends here

;; Add to Allow-Once List


;; [[file:scope-core.org::*Add to Allow-Once List][Add to Allow-Once List:1]]
(defun jf/gptel-scope-add-to-allow-once-list (tool-name resource)
  "Add TOOL-NAME and RESOURCE to allow-once list.
Permission is valid only for current LLM turn.

This is a public API function used by scope-expansion and other modules."
  (push (cons tool-name resource) jf/gptel-scope--allow-once-list))
;; Add to Allow-Once List:1 ends here

;; Clear Allow-Once After Response


;; [[file:scope-core.org::*Clear Allow-Once After Response][Clear Allow-Once After Response:1]]
(defun jf/gptel-scope--clear-allow-once (&rest _)
  "Clear allow-once list after LLM response completes.
Hooked into gptel-post-response-functions."
  (when (boundp 'jf/gptel-scope--allow-once-list)
    (setq-local jf/gptel-scope--allow-once-list nil)))

;; Hook into gptel response lifecycle
(add-hook 'gptel-post-response-functions #'jf/gptel-scope--clear-allow-once)
;; Clear Allow-Once After Response:1 ends here

;; Pattern Matching Helper

;; Check if a path matches any pattern in a list.


;; [[file:scope-core.org::*Pattern Matching Helper][Pattern Matching Helper:1]]
(defun jf/gptel-scope--matches-any-pattern (path patterns)
  "Check if PATH matches any pattern in PATTERNS list.
Returns t if any pattern matches, nil otherwise."
  (when patterns
    (cl-some (lambda (pattern)
              (jf/gptel-scope--matches-pattern path pattern))
            patterns)))
;; Pattern Matching Helper:1 ends here

;; Path-Based Validator

;; Validates filesystem, projectile, and treesitter tools against path lists.


;; [[file:scope-core.org::*Path-Based Validator][Path-Based Validator:1]]
(defun jf/gptel-scope--validate-path-tool (tool-name args category config)
  "Validate path-based tool against read/write/deny path lists.
TOOL-NAME is the tool being validated.
ARGS is the tool arguments list (first arg should be filepath).
CATEGORY is the tool category plist (:validation :operation).
CONFIG is the scope configuration plist.

Returns plist with:
  :allowed t/nil
  :reason STRING (if denied)
  :resource STRING (the filepath, if denied)
  :tool STRING (tool name, if denied)
  :allowed-patterns LIST (if denied for not matching)."
  (cl-block jf/gptel-scope--validate-path-tool
    (let* ((operation (plist-get category :operation))
           (filepath (car args))  ; First arg is always filepath
           (full-path (expand-file-name filepath))
           (read-paths (plist-get config :paths-read))
           (write-paths (plist-get config :paths-write))
           (deny-paths (plist-get config :paths-deny))
           (target-paths (if (eq operation 'read) read-paths write-paths)))

      ;; Check deny first (highest priority)
      (when (jf/gptel-scope--matches-any-pattern full-path deny-paths)
        (cl-return-from jf/gptel-scope--validate-path-tool
          (list :allowed nil
                :reason "denied-pattern"
                :resource full-path
                :tool tool-name)))

      ;; Check allow patterns
      (unless (jf/gptel-scope--matches-any-pattern full-path target-paths)
        (cl-return-from jf/gptel-scope--validate-path-tool
          (list :allowed nil
                :reason "not-in-scope"
                :resource full-path
                :tool tool-name
                :allowed-patterns target-paths)))

      ;; Passed
      (list :allowed t))))
;; Path-Based Validator:1 ends here

;; Pattern-Based Validator

;; Validates org-roam tools against org_roam_patterns section.


;; [[file:scope-core.org::*Pattern-Based Validator][Pattern-Based Validator:1]]
(defun jf/gptel-scope--validate-pattern-tool (tool-name args config)
  "Validate org-roam tool against org_roam_patterns section.
TOOL-NAME is the tool being validated.
ARGS is the tool arguments list (varies by tool).
CONFIG is the scope configuration plist.

Returns plist with:
  :allowed t/nil
  :reason STRING (if denied)
  :resource STRING (description of what was denied)
  :tool STRING (tool name, if denied)."
  (let ((org-roam-config (plist-get config :org-roam-patterns)))
    (pcase tool-name
      ("create_roam_node_in_scope"
       (let ((subdirectory (nth 1 args))  ; 2nd arg
             (tags (nth 2 args))          ; 3rd arg
             (allowed-subdirs (plist-get org-roam-config :subdirectory))
             (allowed-tags (plist-get org-roam-config :tags))
             (allowed nil))

         ;; Check subdirectory patterns (format: "gptel/**")
         (when (and subdirectory allowed-subdirs)
           (dolist (pattern allowed-subdirs)
             (when (string-match-p (jf/gptel-scope--glob-to-regex pattern)
                                 subdirectory)
               (setq allowed t))))

         ;; Check tag patterns (format: "gptel")
         (when (and tags allowed-tags)
           (dolist (tag tags)
             (when (member tag allowed-tags)
               (setq allowed t))))

         (if allowed
             (list :allowed t)
           (list :allowed nil
                 :reason "not-in-org-roam-patterns"
                 :resource (format "subdirectory:%s tags:%s"
                                 (or subdirectory "none")
                                 (or (mapconcat #'identity tags ",") "none"))
                 :tool tool-name))))

      ("add_roam_tags_in_scope"
       (let ((tags (nth 1 args))  ; 2nd arg
             (allowed-tags (plist-get org-roam-config :tags))
             (allowed nil))

         ;; Check tag patterns
         (when (and tags allowed-tags)
           (dolist (tag tags)
             (when (member tag allowed-tags)
               (setq allowed t))))

         (if allowed
             (list :allowed t)
           (list :allowed nil
                 :reason "not-in-org-roam-patterns"
                 :resource (format "tags:%s"
                                 (mapconcat #'identity tags ","))
                 :tool tool-name))))

      ("link_roam_nodes_in_scope"
       (let ((node-ids (plist-get org-roam-config :node-ids)))
         ;; Check if wildcard "*" is in allowed node_ids
         (if (member "*" node-ids)
             (list :allowed t)
           ;; Could extend to check specific node IDs here
           (list :allowed nil
                 :reason "not-in-org-roam-patterns"
                 :resource "node linking"
                 :tool tool-name))))

      (_
       ;; Unknown org-roam tool
       (list :allowed nil
             :reason "unknown-org-roam-tool"
             :resource tool-name
             :tool tool-name)))))
;; Pattern-Based Validator:1 ends here

;; Validator Return Values

;; The bash validator returns structured error information to help the LLM understand what went wrong and how to fix it.

;; *Success case:*

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:1]]
(:allowed t)
;; Validator Return Values:1 ends here



;; *Error cases:*

;; 1. *Command not allowed* (command not in any category):

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:2]]
(:allowed nil
 :reason "command-not-allowed"
 :tool "run_bash_command"
 :resource "tree"
 :command "tree"
 :message "Command 'tree' is not in any bash_tools category (read_only, safe_write, dangerous). Use request_scope_expansion to request adding this command to an appropriate category.")
;; Validator Return Values:2 ends here



;; 2. *Denied command* (in bash_tools.deny list):

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:3]]
(:allowed nil
 :reason "denied-command"
 :tool "run_bash_command"
 :resource "rm -rf /tmp/file"
 :command "rm -rf /tmp/file"
 :message "Command 'rm' is explicitly denied in bash_tools.deny list.")
;; Validator Return Values:3 ends here



;; 3. *Directory not in scope* (directory doesn't match paths.read or paths.write):

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:4]]
(:allowed nil
 :reason "directory-not-in-scope"
 :tool "run_bash_command"
 :resource "/Users/jefffarr/other-project"
 :command "ls -la"
 :directory "/Users/jefffarr/other-project"
 :required-scope "read"
 :allowed-patterns ("/Users/jefffarr/emacs/**" "/Users/jefffarr/projects/**")
 :message "Directory '/Users/jefffarr/other-project' is not in scope for read operations. Command 'ls' requires read access. Use request_scope_expansion to request access.")
;; Validator Return Values:4 ends here



;; 4. *Dangerous command* (requires explicit approval):

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:5]]
(:allowed nil
 :reason "dangerous-command"
 :tool "run_bash_command"
 :resource "dangerous-cmd"
 :command "dangerous-cmd"
 :message "Command 'dangerous-cmd' is categorized as dangerous and requires explicit user approval via request_scope_expansion.")
;; Validator Return Values:5 ends here



;; 5. *No bash_tools configuration*:

;; [[file:scope-core.org::*Validator Return Values][Validator Return Values:6]]
(:allowed nil
 :reason "command-not-allowed"
 :tool "run_bash_command"
 :resource "ls"
 :command "ls"
 :message "No bash_tools configuration found. All commands denied by default.")
;; Validator Return Values:6 ends here

;; Implementation


;; [[file:scope-core.org::*Implementation][Implementation:1]]
(defun jf/gptel-scope--validate-bash-tool (tool-name args config)
  "Validate bash command against bash_tools categories and paths.
TOOL-NAME is the tool being validated.
ARGS is the tool arguments list (command and directory).
CONFIG is the scope configuration plist.

Returns plist with:
  :allowed t/nil
  :reason STRING (if denied)
  :resource STRING (the denied resource - command or directory)
  :command STRING (the command, if denied)
  :directory STRING (the directory, if denied)
  :tool STRING (tool name, if denied)
  :message STRING (descriptive message)
  :required-scope STRING (read/write, if denied)
  :allowed-patterns LIST (if denied for directory not in scope)."
  (cl-block jf/gptel-scope--validate-bash-tool
    (let* ((command-full (car args))
           (directory (cadr args))
           (bash-config (plist-get config :bash-tools))
           (categories (plist-get bash-config :categories))
           (deny-list (plist-get bash-config :deny))
           (read-paths (plist-get config :paths-read))
           (write-paths (plist-get config :paths-write))
           (deny-paths (plist-get config :paths-deny)))

      ;; If bash_tools section is missing, deny all commands
      (unless bash-config
        (cl-return-from jf/gptel-scope--validate-bash-tool
          (list :allowed nil
                :reason "command-not-allowed"
                :tool tool-name
                :resource command-full
                :command command-full
                :message "No bash_tools configuration found. All commands denied by default.")))

      ;; Validate bash_tools structure is well-formed
      (unless (and categories (listp categories))
        (cl-return-from jf/gptel-scope--validate-bash-tool
          (list :allowed nil
                :reason "malformed-config"
                :tool tool-name
                :resource command-full
                :command command-full
                :message "Malformed bash_tools configuration: missing or invalid 'categories' section.")))

      ;; Validate required category sections exist
      (let ((read-only-section (plist-get categories :read-only))
            (safe-write-section (plist-get categories :safe-write))
            (dangerous-section (plist-get categories :dangerous)))
        (unless (and read-only-section safe-write-section dangerous-section)
          (cl-return-from jf/gptel-scope--validate-bash-tool
            (list :allowed nil
                  :reason "malformed-config"
                  :tool tool-name
                  :resource command-full
                  :command command-full
                  :message "Malformed bash_tools configuration: missing required category (read_only, safe_write, or dangerous).")))

        ;; Validate each category has a commands list
        (unless (and (plist-get read-only-section :commands)
                     (plist-get safe-write-section :commands)
                     (plist-get dangerous-section :commands))
          (cl-return-from jf/gptel-scope--validate-bash-tool
            (list :allowed nil
                  :reason "malformed-config"
                  :tool tool-name
                  :resource command-full
                  :command command-full
                  :message "Malformed bash_tools configuration: category missing 'commands' list."))))

      ;; Parse command to extract base command
      (let* ((command-parts (split-string command-full "[ |><;&]+" t))
             (base-command (car command-parts)))

        ;; Check deny list first (highest priority)
        (when (member base-command deny-list)
          (cl-return-from jf/gptel-scope--validate-bash-tool
            (list :allowed nil
                  :reason "denied-command"
                  :tool tool-name
                  :resource command-full
                  :command command-full
                  :message (format "Command '%s' is explicitly denied in bash_tools.deny list."
                                 base-command))))

        ;; Categorize command
        (let ((category nil)
              (read-only (plist-get (plist-get categories :read-only) :commands))
              (safe-write (plist-get (plist-get categories :safe-write) :commands))
              (dangerous (plist-get (plist-get categories :dangerous) :commands)))

          (cond
           ((member base-command read-only) (setq category 'read-only))
           ((member base-command safe-write) (setq category 'safe-write))
           ((member base-command dangerous) (setq category 'dangerous))
           (t
            ;; Unknown command - deny and suggest scope expansion
            (cl-return-from jf/gptel-scope--validate-bash-tool
              (list :allowed nil
                    :reason "command-not-allowed"
                    :tool tool-name
                    :resource command-full
                    :command command-full
                    :message (format "Command '%s' is not in any bash_tools category (read_only, safe_write, dangerous). Use request_scope_expansion to request adding this command to an appropriate category."
                                   base-command)))))

          ;; Dangerous commands always denied (require explicit expansion)
          (when (eq category 'dangerous)
            (cl-return-from jf/gptel-scope--validate-bash-tool
              (list :allowed nil
                    :reason "dangerous-command"
                    :tool tool-name
                    :resource command-full
                    :command command-full
                    :message (format "Command '%s' is categorized as dangerous and requires explicit user approval via request_scope_expansion."
                                   base-command))))

          ;; Resolve directory to absolute path
          (let* ((abs-directory (expand-file-name directory))
                 (real-directory (file-truename abs-directory))
                 (required-scope (if (eq category 'read-only) "read" "write")))

            ;; Check deny paths first (overrides all)
            (when (jf/gptel-scope--matches-any-pattern real-directory deny-paths)
              (cl-return-from jf/gptel-scope--validate-bash-tool
                (list :allowed nil
                      :reason "denied-path"
                      :tool tool-name
                      :resource real-directory
                      :command command-full
                      :directory real-directory
                      :message (format "Directory '%s' is explicitly denied in paths.deny list."
                                     real-directory))))

            ;; Validate directory against category requirement
            (pcase category
              ('read-only
               ;; Read commands allowed in paths.read OR paths.write
               (unless (or (jf/gptel-scope--matches-any-pattern real-directory read-paths)
                          (jf/gptel-scope--matches-any-pattern real-directory write-paths))
                 (cl-return-from jf/gptel-scope--validate-bash-tool
                   (list :allowed nil
                         :reason "directory-not-in-scope"
                         :tool tool-name
                         :resource real-directory
                         :command command-full
                         :directory real-directory
                         :required-scope required-scope
                         :allowed-patterns (append read-paths write-paths)
                         :message (format "Directory '%s' is not in scope for read operations. Command '%s' requires read access. Use request_scope_expansion to request access."
                                        real-directory base-command)))))

              ('safe-write
               ;; Write commands require paths.write only
               (unless (jf/gptel-scope--matches-any-pattern real-directory write-paths)
                 (cl-return-from jf/gptel-scope--validate-bash-tool
                   (list :allowed nil
                         :reason "directory-not-in-scope"
                         :tool tool-name
                         :resource real-directory
                         :command command-full
                         :directory real-directory
                         :required-scope required-scope
                         :allowed-patterns write-paths
                         :message (format "Directory '%s' is not in scope for write operations. Command '%s' requires write access. Use request_scope_expansion to request access."
                                        real-directory base-command))))))

            ;; Passed all checks
            (list :allowed t)))))))
;; Implementation:1 ends here

;; Tool Permission Dispatch

;; Central dispatcher that routes permission checks to tool-specific validators based on tool categories.


;; [[file:scope-core.org::*Tool Permission Dispatch][Tool Permission Dispatch:1]]
(defun jf/gptel-scope--check-tool-permission (config tool-name args)
  "Validate TOOL-NAME with ARGS against CONFIG.
CONFIG is the scope configuration plist from scope.yml.
TOOL-NAME is the tool being validated.
ARGS is the tool arguments list.

Returns plist with:
  :allowed t/nil
  :reason STRING (if denied)
  :resource STRING (what was denied, if applicable)
  :tool STRING (tool name, if denied)
  :allowed-patterns LIST (if denied for not matching)."

  ;; Check allow-once first (highest priority)
  (when (jf/gptel-scope--check-allow-once tool-name args config)
    (cl-return-from jf/gptel-scope--check-tool-permission
      (list :allowed t :reason "allow-once")))

  ;; Lookup tool category
  (let* ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories)))
         (validation-type (plist-get category :validation)))

    ;; Meta tools always pass
    (when (eq validation-type 'meta)
      (cl-return-from jf/gptel-scope--check-tool-permission
        (list :allowed t)))

    ;; Route to validator
    (pcase validation-type
      ('path (jf/gptel-scope--validate-path-tool tool-name args category config))
      ('pattern (jf/gptel-scope--validate-pattern-tool tool-name args config))
      ('bash (jf/gptel-scope--validate-bash-tool tool-name args config))
      (_
       ;; Unknown tool - deny by default
       (list :allowed nil
             :reason "unknown-tool"
             :resource tool-name
             :tool tool-name)))))
;; Tool Permission Dispatch:1 ends here

;; Convert Glob to Regex

;; Convert glob patterns to regex, supporting =**=, =*=, and =?= wildcards.


;; [[file:scope-core.org::*Convert Glob to Regex][Convert Glob to Regex:1]]
(defun jf/gptel-scope--glob-to-regex (pattern)
  "Convert glob PATTERN to regex string.
Supports:
  ** - Match any characters including /
  *  - Match any characters except /
  ?  - Match single character"
  (let ((regex pattern))
    ;; Escape regex special characters (except * and ?)
    (setq regex (replace-regexp-in-string "[.+^$()\\[\\]{}|\\\\]" "\\\\\\&" regex))
    ;; Convert glob wildcards to regex
    (setq regex (replace-regexp-in-string "\\*\\*" "DOUBLESTAR" regex))
    (setq regex (replace-regexp-in-string "\\*" "[^/]*" regex))
    (setq regex (replace-regexp-in-string "DOUBLESTAR" ".*" regex))
    (setq regex (replace-regexp-in-string "\\?" "." regex))
    ;; Anchor pattern
    (concat "^" regex "$")))
;; Convert Glob to Regex:1 ends here

;; Test Path Against Pattern

;; Check if a path matches a glob pattern.


;; [[file:scope-core.org::*Test Path Against Pattern][Test Path Against Pattern:1]]
(defun jf/gptel-scope--matches-pattern (path pattern)
  "Check if PATH matches glob PATTERN.
Expands relative paths to absolute, resolves symlinks."
  (let* ((abs-path (expand-file-name path))
         (real-path (file-truename abs-path))
         (expanded-pattern (expand-file-name pattern))
         (regex (jf/gptel-scope--glob-to-regex expanded-pattern)))
    (or (string-match-p regex real-path)
        (string-match-p regex abs-path))))
;; Test Path Against Pattern:1 ends here

;; Format Tool Error

;; Create standardized error response for tool-level permission violations.


;; [[file:scope-core.org::*Format Tool Error][Format Tool Error:1]]
(defun jf/gptel-scope--format-tool-error (tool-name resource check-result)
  "Format tool permission error for LLM.
TOOL-NAME: Name of the tool that was denied
RESOURCE: The resource that was denied (path, node-id, command, etc.)
CHECK-RESULT: Plist from validator with :allowed, :patterns/:allowed-patterns, :deny-patterns, :reason/:error"
  (let ((patterns (or (plist-get check-result :allowed-patterns)  ; Bash validator uses :allowed-patterns
                      (plist-get check-result :patterns)))        ; Other validators use :patterns
        (deny-patterns (plist-get check-result :deny-patterns))
        (error-type (or (plist-get check-result :reason)          ; Bash validator uses :reason
                        (plist-get check-result :error)           ; Other validators use :error
                        "scope-violation"))
        (custom-message (plist-get check-result :message))
        ;; Extract bash-specific fields (will be nil for non-bash tools)
        (command (plist-get check-result :command))
        (directory (plist-get check-result :directory))
        (required-scope (plist-get check-result :required-scope)))
    (list :success nil
          :error error-type
          :tool tool-name
          :resource resource
          :command command                    ; Pass through bash command field
          :directory directory                ; Pass through bash directory field
          :required-scope required-scope      ; Pass through required scope level
          :allowed-patterns patterns
          :deny-patterns deny-patterns
          :message (or custom-message
                      (format "Tool '%s' denied for resource '%s'. Use request_scope_expansion to ask user for approval."
                             tool-name
                             resource)))))
;; Format Tool Error:1 ends here

;; Provide Feature


;; [[file:scope-core.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-core)
;;; scope-core.el ends here
;; Provide Feature:1 ends here
