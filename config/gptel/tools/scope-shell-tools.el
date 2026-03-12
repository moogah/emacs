;;; scope-shell-tools.el --- GPTEL Scope Shell and Meta Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope-aware shell execution and meta tools for v3.0 preset-based permission management.

;;; Code:

;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)  ; For jf/gptel-scope-prompt-expansion
(require 'bash-parser-core)  ; For jf/bash-parse
(require 'bash-parser-plugins)  ; For jf/bash-extract-semantics
;; Dependencies:1 ends here

;; Constants

;; Security and resource limits for bash command execution.


;; [[file:scope-shell-tools.org::*Constants][Constants:1]]
(defconst jf/gptel-bash--max-output-chars 10000
  "Maximum characters in command output before truncation.
Prevents context window overflow from large command output.")

(defconst jf/gptel-bash--command-timeout 30
  "Timeout in seconds for bash command execution.
Prevents runaway processes from consuming resources indefinitely.")
;; Constants:1 ends here

;; Schema Defaults

;; Safe defaults for scope.yml schema structure with operation-specific paths, cloud config, and security settings.


;; [[file:scope-shell-tools.org::*Schema Defaults][Schema Defaults:1]]
(defconst jf/gptel-scope-schema-defaults
  '(:paths (:read ()
            :write ()
            :execute ()
            :modify ()
            :deny ())
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Safe defaults for scope.yml schema.
Missing sections in YAML are merged with these defaults.

Operation-specific paths:
- read: Files/directories that can be read
- write: Files/directories that can be written
- execute: Files/directories where commands can execute
- modify: Files/directories where modifications allowed
- deny: Patterns never allowed

Cloud configuration:
- auth-detection: \"allow\", \"warn\", or \"deny\"

Security settings:
- enforce-parse-complete: Whether to require complete bash parsing
- max-coverage-threshold: Maximum parse coverage ratio (0.0-1.0)")
;; Schema Defaults:1 ends here

;; Load Schema

;; Load scope.yml schema and merge with safe defaults.


;; [[file:scope-shell-tools.org::*Load Schema][Load Schema:1]]
(defun jf/gptel-scope--load-schema (schema-plist)
  "Load scope schema from SCHEMA-PLIST and merge with defaults.
Missing sections get safe defaults from `jf/gptel-scope-schema-defaults'.
Present sections are validated for correctness.

Returns merged plist with normalized kebab-case keys.

VALIDATION: Rejects schemas with bash_tools.categories section.
Migration: Remove categories section, keep only deny list."
  (let* ((defaults jf/gptel-scope-schema-defaults)
         ;; Normalize snake_case to kebab-case
         (normalized (jf/gptel-scope--normalize-keys schema-plist))
         ;; Extract sections
         (paths (plist-get normalized :paths))
         (cloud (plist-get normalized :cloud))
         (security (plist-get normalized :security))
         (bash-tools (plist-get normalized :bash-tools))
         ;; Validate: reject if categories section present
         (categories (when bash-tools (plist-get bash-tools :categories))))

    ;; Early validation: reject profiles with categories
    (when categories
      (error "bash_tools.categories section no longer supported. Migration: Remove categories section, keep only deny list. See CLAUDE.md for migration guide"))

    ;; Merge with defaults (missing sections → defaults)
    (let ((merged-paths (if paths
                            (list :read (or (plist-get paths :read) ())
                                  :write (or (plist-get paths :write) ())
                                  :execute (or (plist-get paths :execute) ())
                                  :modify (or (plist-get paths :modify) ())
                                  :deny (or (plist-get paths :deny) ()))
                          (plist-get defaults :paths)))
          (merged-cloud (if cloud
                            (list :auth-detection (or (plist-get cloud :auth-detection)
                                                     (plist-get (plist-get defaults :cloud) :auth-detection))
                                  :allowed-providers (plist-get cloud :allowed-providers))
                          (plist-get defaults :cloud)))
          (merged-security (if security
                               (list :enforce-parse-complete
                                     (if (plist-member security :enforce-parse-complete)
                                         (plist-get security :enforce-parse-complete)
                                       (plist-get (plist-get defaults :security) :enforce-parse-complete))
                                     :max-coverage-threshold
                                     (if (plist-member security :max-coverage-threshold)
                                         (plist-get security :max-coverage-threshold)
                                       (plist-get (plist-get defaults :security) :max-coverage-threshold)))
                             (plist-get defaults :security))))
      ;; Validate present sections
      (when cloud
        (jf/gptel-scope--validate-cloud-config merged-cloud))
      (when security
        (jf/gptel-scope--validate-security-config merged-security))
      ;; Return merged schema
      (list :paths merged-paths
            :cloud merged-cloud
            :security merged-security
            :bash-tools bash-tools))))
;; Load Schema:1 ends here

;; Normalize Keys

;; Convert YAML snake_case keys to elisp kebab-case.


;; [[file:scope-shell-tools.org::*Normalize Keys][Normalize Keys:1]]
(defun jf/gptel-scope--normalize-keys (plist)
  "Normalize PLIST keys from snake_case to kebab-case.
Recursively processes nested plists.

Examples:
  :auth_detection → :auth-detection
  :max_coverage_threshold → :max-coverage-threshold
  :enforce_parse_complete → :enforce-parse-complete"
  (let ((result nil))
    (while plist
      (let* ((key (car plist))
             (value (cadr plist))
             (normalized-key (intern (replace-regexp-in-string
                                     "_" "-"
                                     (symbol-name key))))
             (normalized-value (if (and (listp value)
                                       (not (null value))
                                       (keywordp (car value)))
                                  (jf/gptel-scope--normalize-keys value)
                                value)))
        (setq result (plist-put result normalized-key normalized-value))
        (setq plist (cddr plist))))
    result))
;; Normalize Keys:1 ends here

;; Validate Schema

;; Validate scope schema structure and values.


;; [[file:scope-shell-tools.org::*Validate Schema][Validate Schema:1]]
(defun jf/gptel-scope--validate-schema (schema-plist)
  "Validate scope SCHEMA-PLIST structure.
Checks that present sections have valid values.
Fails fast on invalid configuration.

Returns t if valid, signals error otherwise."
  (let ((paths (plist-get schema-plist :paths))
        (cloud (plist-get schema-plist :cloud))
        (security (plist-get schema-plist :security)))
    ;; Validate paths section if present
    (when paths
      (unless (listp paths)
        (error "Scope schema: paths must be a plist"))
      (dolist (key '(:read :write :execute :modify :deny))
        (when (plist-member paths key)
          (let ((value (plist-get paths key)))
            (unless (listp value)
              (error "Scope schema: paths.%s must be a list, got %S" key value))))))
    ;; Validate cloud section if present
    (when cloud
      (jf/gptel-scope--validate-cloud-config cloud))
    ;; Validate security section if present
    (when security
      (jf/gptel-scope--validate-security-config security))
    t))
;; Validate Schema:1 ends here

;; Load Cloud Config

;; Extract and validate cloud configuration.


;; [[file:scope-shell-tools.org::*Load Cloud Config][Load Cloud Config:1]]
(defun jf/gptel-scope--load-cloud-config (cloud-plist)
  "Load cloud configuration from CLOUD-PLIST.
Validates auth-detection setting.

Returns normalized cloud config plist."
  (when cloud-plist
    (let ((auth-detection (plist-get cloud-plist :auth-detection)))
      (jf/gptel-scope--validate-cloud-config cloud-plist)
      (list :auth-detection auth-detection))))
;; Load Cloud Config:1 ends here

;; Validate Cloud Config

;; Validate cloud configuration values.


;; [[file:scope-shell-tools.org::*Validate Cloud Config][Validate Cloud Config:1]]
(defun jf/gptel-scope--validate-cloud-config (cloud-plist)
  "Validate CLOUD-PLIST configuration.
Checks auth-detection value is valid.

Valid values: \"allow\", \"warn\", \"deny\"
Signals error if invalid."
  (when cloud-plist
    (let ((auth-detection (plist-get cloud-plist :auth-detection)))
      (unless (member auth-detection '("allow" "warn" "deny"))
        (error "Scope schema: cloud.auth-detection must be \"allow\", \"warn\", or \"deny\", got %S"
               auth-detection))))
  t)
;; Validate Cloud Config:1 ends here

;; Load Security Config

;; Extract and validate security configuration.


;; [[file:scope-shell-tools.org::*Load Security Config][Load Security Config:1]]
(defun jf/gptel-scope--load-security-config (security-plist)
  "Load security configuration from SECURITY-PLIST.
Validates enforce-parse-complete and max-coverage-threshold.

Returns normalized security config plist."
  (when security-plist
    (let ((enforce-parse-complete (plist-get security-plist :enforce-parse-complete))
          (max-coverage-threshold (plist-get security-plist :max-coverage-threshold)))
      (jf/gptel-scope--validate-security-config security-plist)
      (list :enforce-parse-complete enforce-parse-complete
            :max-coverage-threshold max-coverage-threshold))))
;; Load Security Config:1 ends here

;; Validate Security Config

;; Validate security configuration values.


;; [[file:scope-shell-tools.org::*Validate Security Config][Validate Security Config:1]]
(defun jf/gptel-scope--validate-security-config (security-plist)
  "Validate SECURITY-PLIST configuration.
Checks:
- enforce-parse-complete is boolean
- max-coverage-threshold is in [0.0, 1.0]

Signals error if invalid."
  (when security-plist
    (let ((enforce-parse-complete (plist-get security-plist :enforce-parse-complete))
          (max-coverage-threshold (plist-get security-plist :max-coverage-threshold)))
      ;; Validate enforce-parse-complete
      (when (plist-member security-plist :enforce-parse-complete)
        (unless (or (eq enforce-parse-complete t)
                   (eq enforce-parse-complete nil))
          (error "Scope schema: security.enforce-parse-complete must be boolean, got %S"
                 enforce-parse-complete)))
      ;; Validate max-coverage-threshold
      (when (plist-member security-plist :max-coverage-threshold)
        (unless (and (numberp max-coverage-threshold)
                    (>= max-coverage-threshold 0.0)
                    (<= max-coverage-threshold 1.0))
          (error "Scope schema: security.max-coverage-threshold must be in [0.0, 1.0], got %S"
                 max-coverage-threshold)))))
  t)
;; Validate Security Config:1 ends here

;; Stage 2: Extract All Commands

;; Extract ALL commands from a pipeline or chain using bash-parser results.


;; [[file:scope-shell-tools.org::*Stage 2: Extract All Commands][Stage 2: Extract All Commands:1]]
(defun jf/gptel-scope--extract-pipeline-commands (parsed-command)
  "Stage 2: Extract all commands from PARSED-COMMAND pipeline/chain.
PARSED-COMMAND is the plist returned by jf/bash-parse.
Returns list of command names in execution order.

Examples (from parsed results):
  'ls | xargs rm' → (\"ls\" \"xargs\" \"rm\")
  'cd /tmp; ls' → (\"cd\" \"ls\")
  'mkdir foo && cd foo' → (\"mkdir\" \"cd\")
  'grep pattern . | head -20 | tail -5' → (\"grep\" \"head\" \"tail\")"
  (let ((all-commands (plist-get parsed-command :all-commands))
        (command-names nil))
    ;; Extract command-name from each parsed command
    (dolist (cmd all-commands)
      (when-let ((name (plist-get cmd :command-name)))
        (push name command-names)))
    (nreverse command-names)))
;; Stage 2: Extract All Commands:1 ends here

;; Stage 3: Validate Pipeline Commands

;; Validate each command in a pipeline against deny list only (no category checking).


;; [[file:scope-shell-tools.org::*Stage 3: Validate Pipeline Commands][Stage 3: Validate Pipeline Commands:1]]
(defun jf/gptel-scope--validate-pipeline-commands (commands bash-tools)
  "Stage 3: Validate each command in COMMANDS against BASH-TOOLS deny list.
COMMANDS is a list of command strings.
BASH-TOOLS is the bash_tools plist from scope.yml.

Returns nil if all commands allowed (not in deny list).
Returns error plist if validation fails with:
  :error - Error type
  :position - Index of first failing command (0-based)
  :command - The failing command name
  :message - Human-readable error message

Examples:
  (validate-pipeline-commands '(\"ls\" \"head\") bash-tools)
    → nil  ; both allowed

  (validate-pipeline-commands '(\"ls\" \"xargs\" \"rm\") bash-tools)
    → (:error \"command_denied\" :position 2 :command \"rm\" :message \"...\")"
  (let ((deny-list (plist-get bash-tools :deny))
        (pos 0))
    (catch 'validation-failed
      (dolist (cmd commands)
        ;; Check deny list - if command is in deny list, reject it
        (when (member cmd deny-list)
          (throw 'validation-failed
                 (list :error "command_denied"
                       :position pos
                       :command cmd
                       :message (format "Command '%s' at position %d is in deny list" cmd pos))))
        (setq pos (1+ pos)))
      ;; All commands validated successfully (none in deny list)
      nil)))
;; Stage 3: Validate Pipeline Commands:1 ends here

;; Stage 4: No-op Command Allowance

;; Check if command has zero file operations and allow by default (short-circuit validation).


;; [[file:scope-shell-tools.org::*Stage 4: No-op Command Allowance][Stage 4: No-op Command Allowance:1]]
(defun jf/gptel-scope--check-no-op (semantics)
  "Stage 4: Check if command is a no-op (zero file operations).
SEMANTICS is the plist returned by jf/bash-extract-semantics.

Returns nil if command has no file operations (allowed, short-circuit).
Returns t if command has file operations (continue validation).

Design pattern: No-op short-circuit - commands with zero extracted
file operations are allowed by default, skipping file path validation.

Examples:
  python3 --version → no file ops → nil (allowed)
  python3 script.py → execute op → t (continue to file validation)
  ls /tmp → read op → t (continue to file validation)"
  (let* ((domains (plist-get semantics :domains))
         (file-ops (plist-get domains :filesystem)))
    ;; If no file operations, allow (return nil to short-circuit)
    ;; If file operations exist, continue validation (return t)
    (if (or (null file-ops) (zerop (length file-ops)))
        nil  ; No file ops - allowed (short-circuit)
      t)))   ; File ops exist - continue validation
;; Stage 4: No-op Command Allowance:1 ends here

;; Check Absolute Paths

;; Check if command contains absolute paths that bypass directory scoping.


;; [[file:scope-shell-tools.org::*Check Absolute Paths][Check Absolute Paths:1]]
(defun jf/gptel-bash--check-absolute-paths (command)
  "Check if COMMAND contains absolute paths.
Returns warning string if found, nil otherwise."
  (when (string-match "/[[:alnum:]_/-]+" command)
    "Warning: Command contains absolute path arguments. Directory scope may not protect these paths."))
;; Check Absolute Paths:1 ends here

;; Execute Command

;; Execute command in directory with timeout and output truncation.


;; [[file:scope-shell-tools.org::*Execute Command][Execute Command:1]]
(defun jf/gptel-bash--execute-command (command directory)
  "Execute COMMAND in DIRECTORY with timeout and output truncation.
Returns (:output OUTPUT :exit_code CODE :truncated BOOL :warnings LIST :error ERROR-TYPE).
Warnings are returned in a separate field to allow LLM to distinguish them from command output.
Error types: timeout, execution-failed."
  (cl-block jf/gptel-bash--execute-command
    (let* ((default-directory (file-truename (expand-file-name directory)))
           (output nil)
           (exit-code nil)
           (truncated nil)
           (warnings nil)
           (error-type nil)
           (max-output-chars jf/gptel-bash--max-output-chars))

      (condition-case err
          (with-timeout (jf/gptel-bash--command-timeout
                         (progn
                           (setq error-type "timeout")
                           (setq exit-code 124)  ; GNU timeout exit code
                           (setq output (format "Command execution timed out after %d seconds. Output may be incomplete."
                                                jf/gptel-bash--command-timeout))
                           (setq warnings (list "Command timed out - use more specific filters to reduce execution time"))))
            (setq output
                  (with-temp-buffer
                    (setq exit-code
                          (call-process shell-file-name nil t nil
                                        shell-command-switch command))
                    (buffer-string))))
        (error
         (cl-return-from jf/gptel-bash--execute-command
           (list :output (format "Command execution failed: %s" (error-message-string err))
                 :exit_code 1
                 :error "execution-failed"
                 :truncated nil
                 :warnings nil))))

      ;; Truncate output if too long
      (let ((original-length (length output)))
        (when (> original-length max-output-chars)
          (setq truncated t)
          (setq output
                (concat (substring output 0 max-output-chars)
                        (format "\n\n[Output truncated at %d chars. Total: %d chars. Use filters like 'head', 'grep', or 'tail' to narrow results.]"
                                max-output-chars original-length)))))

      ;; Check for warnings (collect in list, don't modify output)
      (let ((path-warning (jf/gptel-bash--check-absolute-paths command)))
        (when path-warning
          (setq warnings (list path-warning))))

      (list :output output
            :exit_code exit-code
            :truncated truncated
            :warnings warnings
            :error error-type))))
;; Execute Command:1 ends here

;; Main Pipeline Function

;; Seven-stage validation pipeline with early exit on failure.


;; [[file:scope-shell-tools.org::*Main Pipeline Function][Main Pipeline Function:1]]
(defun jf/gptel-scope--validate-command-semantics (command directory scope-config)
  "Seven-stage validation pipeline with early exit on failure.
COMMAND is the bash command string to validate.
DIRECTORY is the working directory for path resolution.
SCOPE-CONFIG is the scope configuration plist.

Returns nil if all validations pass, error plist on first failure.

Stages:
  1. Parse command with bash-parser
  2. Extract pipeline commands
  3. Validate pipeline commands against deny list
  4. Check for no-op (zero file operations) - short-circuit if true
  5. Extract and validate file operations
  6. Detect and enforce cloud auth policy
  7. Check coverage threshold (warning only)"
  (cl-block jf/gptel-scope--validate-command-semantics
    (let* ((parsed (jf/bash-parse command))
           (semantics (jf/bash-extract-semantics parsed))
           (security-config (plist-get scope-config :security)))

      ;; Stage 1: Parse completeness
      (when-let ((error (jf/gptel-scope--validate-parse-completeness parsed security-config)))
        (cl-return-from jf/gptel-scope--validate-command-semantics error))

      ;; Stage 2: Extract pipeline commands
      (let ((commands (jf/gptel-scope--extract-pipeline-commands parsed)))

        ;; Stage 3: Validate pipeline commands (deny list checked here)
        (when-let ((error (jf/gptel-scope--validate-pipeline-commands
                           commands (plist-get scope-config :bash-tools))))
          (cl-return-from jf/gptel-scope--validate-command-semantics error))

        ;; Stage 4: No-op check - short-circuit if zero file operations
        (unless (jf/gptel-scope--check-no-op semantics)
          ;; No file operations - allowed by default (short-circuit)
          (cl-return-from jf/gptel-scope--validate-command-semantics nil))

        ;; Stage 5: File operations validation (only if file ops exist)
        (when-let ((file-ops (plist-get (plist-get semantics :domains) :filesystem)))
          (when-let ((error (jf/gptel-scope--validate-file-operations
                             file-ops directory scope-config)))
            (cl-return-from jf/gptel-scope--validate-command-semantics error)))

        ;; Stage 6: Cloud auth detection and policy enforcement
        (when-let ((cloud-auth (plist-get (plist-get semantics :domains) :cloud-auth)))
          (when-let ((error (jf/gptel-scope--validate-cloud-auth
                             cloud-auth (plist-get scope-config :cloud))))
            (cl-return-from jf/gptel-scope--validate-command-semantics error)))

        ;; Stage 7: Coverage check (warning only, doesn't block)
        (jf/gptel-scope--check-coverage-threshold semantics security-config)

        ;; All validations passed
        nil))))
;; Main Pipeline Function:1 ends here

;; Stage 1: Parse Completeness Checking

;; Check if bash-parser successfully parsed the entire command.
;; An incomplete parse indicates syntax errors or unsupported constructs.


;; [[file:scope-shell-tools.org::*Stage 1: Parse Completeness Checking][Stage 1: Parse Completeness Checking:1]]
(defun jf/gptel-scope--validate-parse-completeness (parse-result security-config)
  "Stage 1: Check parse completeness.
PARSE-RESULT is the plist returned by bash-parser.
SECURITY-CONFIG is the security configuration plist.

Returns nil if validation passes, error plist if validation fails.

Checks :parse-complete flag from bash-parser result.
If incomplete and enforce=true: error with parse-errors.
If incomplete and enforce=false: warn.

Example PARSE-RESULT:
  (:parse-complete nil
   :parse-errors \"Unexpected token at line 2\")

Example SECURITY-CONFIG:
  (:enforce-parse-complete t)"
  (let ((complete (plist-get parse-result :parse-complete))
        (enforce (plist-get security-config :enforce-parse-complete))
        (errors (plist-get parse-result :parse-errors)))
    (when (not complete)
      (if enforce
          ;; Strict mode: error on incomplete parse
          (list :error "parse_incomplete"
                :message (format "Parse incomplete: %s" errors)
                :parse-errors errors)
        ;; Permissive mode: warn but continue
        (warn "Parse incomplete: %s" errors)
        nil))))
;; Stage 1: Parse Completeness Checking:1 ends here

;; Tool Implementation


;; [[file:scope-shell-tools.org::*Tool Implementation][Tool Implementation:1]]
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command in specified directory with scope validation.

Commands are categorized:
- read_only: ls, grep, find, cat, head, tail, wc, file, git log, git show, git diff
- safe_write: mkdir, touch, echo, git add, git commit
- dangerous: (empty by default, requires explicit approval)

Directory must be in scope for command category:
- read_only commands: directory must match paths.read OR paths.write
- safe_write commands: directory must match paths.write

Shell composition allowed (pipes, redirects, command substitution), but only base command is validated.

Security features:
- 30-second timeout
- Output truncation at 10,000 chars
- Warnings for absolute paths in arguments
- Deny list blocks dangerous commands (rm, rmdir, mv, cp, ln, scp, rsync, chmod, sudo, crontab, iptables, systemctl, useradd, shutdown, reboot, halt, poweroff, etc.)

Examples:
  run_bash_command('ls -la', '/Users/jefffarr/emacs')
  run_bash_command('grep -r TODO . | head -20', '/Users/jefffarr/projects/myapp')
  run_bash_command('git log --oneline -10', '/Users/jefffarr/emacs')
  run_bash_command('mkdir scratch', '/tmp')"

 (list '(:name "command"
         :type string
         :description "Shell command to execute (pipes and redirects allowed)")
       '(:name "directory"
         :type string
         :description "Working directory (must be in scope for command category)"))

 "bash"

 :async  ; Enable inline expansion for scope violations

 ;; Tool body - executed only if validation passes
 (let* ((result (jf/gptel-bash--execute-command command directory))
        (exit-code (plist-get result :exit_code))
        (output (plist-get result :output))
        (truncated (plist-get result :truncated))
        (warnings (plist-get result :warnings))
        (success (zerop exit-code)))
   (list :success success
         :output output
         :exit_code exit-code
         :truncated truncated
         :warnings warnings)))
;; Tool Implementation:1 ends here

;; Request Scope Expansion Tool (Meta Tool, v3.0 Async)

;; LLM uses this tool to explicitly request user approval for expanding scope using transient menu.


;; [[file:scope-shell-tools.org::*Request Scope Expansion Tool (Meta Tool, v3.0 Async)][Request Scope Expansion Tool (Meta Tool, v3.0 Async):1]]
(gptel-make-tool
 :name "request_scope_expansion"
 :async t  ; MUST be async for transient menu
 :description "Request user approval to expand scope with new patterns.

Displays interactive menu with 3 options:
1. Deny - Reject the expansion request
2. Add to scope - Permanently add patterns to scope.yml
3. Allow once - Temporarily allow for current turn only

The user will see:
- Which tool needs access
- What patterns/resources you want to add
- Your justification for why access is needed

Returns:
- success: true if approved (add-to-scope or allow-once)
- success: false if denied
- allowed_once: true if temporary permission granted
- patterns_added: list of patterns if permanently added"
 :args (list '(:name "tool_name"
               :type string
               :description "Tool name (e.g., 'read_file', 'write_file_in_scope')")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "Patterns to add (e.g., [\"/tmp/**\"] for files, [\"npm\"] for shell)")
             '(:name "justification"
               :type string
               :description "Explain why this access is needed. Be specific."))
 :category "scope"
 :function
 (lambda (callback tool_name patterns justification)  ; callback first!
   ;; Convert patterns from vector to list
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))

   ;; Build violation info for transient menu
   (let* ((violation-info
           (list :tool tool_name
                 :resource (car patterns)  ; First pattern as resource
                 :reason justification
                 :validation-type (jf/gptel-scope--infer-validation-type tool_name)
                 :patterns patterns)))

     ;; Show transient menu - pass callback, patterns, tool_name directly in scope
     (jf/gptel-scope-prompt-expansion violation-info callback patterns tool_name))))
;; Request Scope Expansion Tool (Meta Tool, v3.0 Async):1 ends here

;; Helper: Infer Validation Type

;; Helper to infer validation type from tool name using tool categories.


;; [[file:scope-shell-tools.org::*Helper: Infer Validation Type][Helper: Infer Validation Type:1]]
(defun jf/gptel-scope--infer-validation-type (tool-name)
  "Infer validation type from TOOL-NAME using tool categories."
  (let ((category (cdr (assoc tool-name jf/gptel-scope--tool-categories))))
    (plist-get category :validation)))
;; Helper: Infer Validation Type:1 ends here

;; Validate Operation

;; Validate a single operation against path configuration with hierarchical permissions.


;; [[file:scope-shell-tools.org::*Validate Operation][Validate Operation:1]]
(defun jf/gptel-scope--validate-operation (operation path paths-config)
  "Check if OPERATION on PATH is allowed by PATHS-CONFIG.
Returns nil if allowed, error plist if denied.

Permission hierarchy:
- read: requires paths.read OR paths.write (write includes read)
- write: requires paths.write
- modify: requires paths.modify OR paths.write (write includes modify)
- execute: requires paths.execute (explicit permission only)
- delete: requires paths.write (deletion is write operation)

Deny precedence: paths.deny overrides all allow patterns."
  (cl-block jf/gptel-scope--validate-operation
    (let ((deny-patterns (plist-get paths-config :deny))
          (read-patterns (plist-get paths-config :read))
          (write-patterns (plist-get paths-config :write))
          (modify-patterns (plist-get paths-config :modify))
          (execute-patterns (plist-get paths-config :execute)))

      ;; Deny takes precedence over all allow patterns
      (when (jf/gptel-scope--path-matches-any-pattern-p path deny-patterns)
        (cl-return-from jf/gptel-scope--validate-operation
          (list :error "path_denied"
                :path path
                :operation operation
                :message (format "Path denied by scope: %s" path))))

      ;; Operation-specific validation with hierarchy
      (let ((allowed
             (pcase operation
               (:read
                ;; Read allowed if in read OR write patterns (write includes read)
                (or (jf/gptel-scope--path-matches-any-pattern-p path read-patterns)
                    (jf/gptel-scope--path-matches-any-pattern-p path write-patterns)))

               (:write
                ;; Write requires explicit write permission
                (jf/gptel-scope--path-matches-any-pattern-p path write-patterns))

               (:modify
                ;; Modify allowed if in modify OR write patterns (write includes modify)
                (or (jf/gptel-scope--path-matches-any-pattern-p path modify-patterns)
                    (jf/gptel-scope--path-matches-any-pattern-p path write-patterns)))

               (:execute
                ;; Execute requires explicit execute permission (high risk)
                (jf/gptel-scope--path-matches-any-pattern-p path execute-patterns))

               (:delete
                ;; Delete is a write operation
                (jf/gptel-scope--path-matches-any-pattern-p path write-patterns))

               (_ nil))))  ; Unknown operation - deny by default

        (if allowed
            nil  ; Success - operation allowed
          ;; Return error with context
          (list :error "path_out_of_scope"
                :path path
                :operation operation
                :required-scope (format "paths.%s" operation)
                :message (format "Path not in %s scope: %s" operation path)))))))
;; Validate Operation:1 ends here

;; Path Pattern Matching Helper

;; Helper function to check if a path matches any pattern in a list using glob matching.


;; [[file:scope-shell-tools.org::*Path Pattern Matching Helper][Path Pattern Matching Helper:1]]
(defun jf/gptel-scope--path-matches-any-pattern-p (path patterns)
  "Return t if PATH matches any pattern in PATTERNS using glob matching.
Supports wildcards: * (single level), ** (recursive)."
  (when (and path patterns)
    (let ((normalized-path (expand-file-name path)))
      (catch 'matched
        (dolist (pattern patterns)
          (when (jf/gptel-scope--glob-match-p normalized-path pattern)
            (throw 'matched t)))))))
;; Path Pattern Matching Helper:1 ends here

;; Glob Pattern Matching

;; Implement glob pattern matching for path validation.


;; [[file:scope-shell-tools.org::*Glob Pattern Matching][Glob Pattern Matching:1]]
(defun jf/gptel-scope--glob-match-p (path pattern)
  "Return t if PATH matches glob PATTERN.
Supports:
- * matches any characters except /
- ** matches any characters including /
- Literal path components

Pattern matching is case-sensitive."
  (let* ((case-fold-search nil)  ; Force case-sensitive matching
         (pattern-regex (jf/gptel-scope--glob-to-regex pattern))
         (result (string-match-p pattern-regex path)))
    (if result t nil)))

(defun jf/gptel-scope--glob-to-regex (pattern)
  "Convert glob PATTERN to regular expression.
- /** becomes /(.*/)? (matches slash followed by zero or more path components)
- **/ becomes (.*/)? (matches zero or more directories ending in slash)
- ** at end becomes .* (matches everything)
- * becomes [^/]* (matches everything except /)
- Other chars are escaped

Special handling for middle recursive wildcards:
  /workspace/**/file.txt → /workspace/(.*/)file.txt matches:
    /workspace/file.txt (zero dirs - empty capture)
    /workspace/sub/file.txt (one dir)
    /workspace/a/b/file.txt (multiple dirs)"
  (let ((regex "^"))
    (while (not (string-empty-p pattern))
      (cond
       ;; Handle /** (slash before recursive wildcard)
       ;; This is the most common case: /workspace/** or /workspace/**/file
       ((string-prefix-p "/**/" pattern)
        ;; Match /dir1/dir2/.../ or just / (zero components)
        (setq regex (concat regex "/\\(?:.*/\\)?"))
        (setq pattern (substring pattern 4)))

       ((string-prefix-p "/**" pattern)
        ;; /** at end or before non-slash: match everything after /
        (setq regex (concat regex "/.*"))
        (setq pattern (substring pattern 3)))

       ;; Handle **/ (recursive wildcard before path separator, no leading /)
       ;; This would be rare but handle it
       ((string-prefix-p "**/" pattern)
        (setq regex (concat regex "\\(?:.*/\\)?"))
        (setq pattern (substring pattern 3)))

       ;; Handle ** alone (at end of pattern or before non-slash)
       ((string-prefix-p "**" pattern)
        (setq regex (concat regex ".*"))
        (setq pattern (substring pattern 2)))

       ;; Handle single *
       ((string-prefix-p "*" pattern)
        (setq regex (concat regex "[^/]*"))
        (setq pattern (substring pattern 1)))

       ;; Handle literal characters
       (t
        (let ((char (substring pattern 0 1)))
          (setq regex (concat regex (regexp-quote char)))
          (setq pattern (substring pattern 1))))))
    (concat regex "$")))
;; Glob Pattern Matching:1 ends here

;; Validate Single File Operation

;; Validate a single file operation plist against path configuration.


;; [[file:scope-shell-tools.org::*Validate Single File Operation][Validate Single File Operation:1]]
(defun jf/gptel-scope--validate-file-operation (file-op directory paths-config)
  "Validate single FILE-OP against PATHS-CONFIG.
FILE-OP format: (:operation OP :path PATH :absolute-path ABS-PATH :command-name CMD)
DIRECTORY is the working directory for resolving relative paths.
Returns nil if allowed, error plist if denied."
  (let* ((operation (plist-get file-op :operation))
         (path (plist-get file-op :path))
         (absolute-path (plist-get file-op :absolute-path))
         (command-name (plist-get file-op :command-name))
         ;; Resolve path relative to directory if needed
         (resolved-path (if (file-name-absolute-p path)
                            (expand-file-name path)
                          (expand-file-name path directory))))

    ;; Use absolute-path from parser if available, otherwise use resolved path
    (let ((final-path (or absolute-path resolved-path)))
      (jf/gptel-scope--validate-operation operation final-path paths-config))))
;; Validate Single File Operation:1 ends here

;; Validate All File Operations

;; Validate all file operations extracted from command semantics.


;; [[file:scope-shell-tools.org::*Validate All File Operations][Validate All File Operations:1]]
(defun jf/gptel-scope--validate-file-operations (file-ops directory scope-config)
  "Validate all FILE-OPS against SCOPE-CONFIG.
FILE-OPS is a list of operation plists from bash-parser.
DIRECTORY is the working directory for path resolution.
SCOPE-CONFIG contains :paths section with read/write/execute/modify/deny patterns.
Returns nil if all operations allowed, error plist for first violation."
  (let ((paths-config (plist-get scope-config :paths)))
    (catch 'error-found
      (dolist (file-op file-ops)
        (when-let ((error (jf/gptel-scope--validate-file-operation
                           file-op directory paths-config)))
          (throw 'error-found error))))))
;; Validate All File Operations:1 ends here

;; Implementation


;; [[file:scope-shell-tools.org::*Implementation][Implementation:1]]
(defun jf/gptel-scope--validate-cloud-auth (cloud-auth-ops cloud-config)
  "Stage 6: Detect and enforce cloud authentication policy.

CLOUD-AUTH-OPS is the :cloud-auth domain from bash-parser semantics.
CLOUD-CONFIG is the :cloud section from scope configuration.

Returns nil if validation passes, error plist if denied."
  (cl-block jf/gptel-scope--validate-cloud-auth
    (when cloud-auth-ops
      (let* ((mode (or (plist-get cloud-config :auth-detection) "warn"))
             (allowed-providers (plist-get cloud-config :allowed-providers))
             (provider (plist-get cloud-auth-ops :provider))
             (command (plist-get cloud-auth-ops :command)))

        ;; Check provider filtering if configured
        (when (and allowed-providers
                   (not (member provider allowed-providers)))
          (cl-return-from jf/gptel-scope--validate-cloud-auth
            (list :error "cloud_provider_denied"
                  :provider provider
                  :command command
                  :allowed-providers allowed-providers
                  :message (format "Cloud provider '%s' not in allowed list: %s"
                                   provider allowed-providers))))

        ;; Enforce policy mode
        (cond
         ((string= mode "allow")
          nil)  ; Allow silently

         ((string= mode "warn")
          ;; Return warning (non-blocking)
          (list :warning "cloud_auth_detected"
                :provider provider
                :command command
                :message (format "Cloud authentication detected: %s (%s provider)"
                                 command provider)))

         ((string= mode "deny")
          ;; Return error (blocking)
          (list :error "cloud_auth_denied"
                :provider provider
                :command command
                :message (format "Cloud authentication denied: %s (%s provider)"
                                 command provider)))

         (t
          ;; Invalid mode - fail closed
          (list :error "invalid_cloud_auth_mode"
                :mode mode
                :message (format "Invalid cloud.auth_detection mode: %s (must be allow/warn/deny)"
                                 mode))))))))
;; Implementation:1 ends here

;; Stage 7: Coverage Threshold Checking

;; Check parse coverage and warn if below threshold. Non-blocking stage that only warns.


;; [[file:scope-shell-tools.org::*Stage 7: Coverage Threshold Checking][Stage 7: Coverage Threshold Checking:1]]
(defun jf/gptel-scope--check-coverage-threshold (semantics security-config)
  "Stage 7: Check coverage threshold (warning only, non-blocking).
SEMANTICS is the plist returned by jf/bash-extract-semantics.
SECURITY-CONFIG is the security configuration plist.

Returns nil always (warnings don't block execution)."
  (when-let* ((coverage (plist-get semantics :coverage))
              (threshold (plist-get security-config :max-coverage-threshold))
              (coverage-ratio (plist-get coverage :coverage-ratio)))
    (when (and threshold (< coverage-ratio threshold))
      (warn "Parse coverage %.2f below threshold %.2f - semantic validation may be incomplete"
            coverage-ratio threshold)))
  nil)
;; Stage 7: Coverage Threshold Checking:1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
