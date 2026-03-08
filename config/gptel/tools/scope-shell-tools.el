;; File Header


;; [[file:scope-shell-tools.org::*File Header][File Header:1]]
;;; scope-shell-tools.el --- GPTEL Scope Shell and Meta Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope-aware shell execution and meta tools for v3.0 preset-based permission management.

;;; Code:
;; File Header:1 ends here

;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-core)
(require 'jf-gptel-scope-expansion)  ; For jf/gptel-scope-prompt-expansion
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

;; V4 Schema Defaults

;; Safe defaults for scope.yml v4 schema structure with operation-specific paths, cloud config, and security settings.


;; [[file:scope-shell-tools.org::*V4 Schema Defaults][V4 Schema Defaults:1]]
(defconst jf/gptel-scope-v4-defaults
  '(:paths (:read ()
            :write ()
            :execute ()
            :modify ()
            :deny ())
    :cloud (:auth-detection "warn")
    :security (:enforce-parse-complete t
               :max-coverage-threshold 0.8))
  "Safe defaults for scope.yml v4 schema.
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
;; V4 Schema Defaults:1 ends here

;; Load V4 Schema

;; Load scope.yml v4 schema and merge with safe defaults.


;; [[file:scope-shell-tools.org::*Load V4 Schema][Load V4 Schema:1]]
(defun jf/gptel-scope--load-v4-schema (schema-plist)
  "Load v4 schema from SCHEMA-PLIST and merge with defaults.
Missing sections get safe defaults from `jf/gptel-scope-v4-defaults'.
Present sections are validated for correctness.

Returns merged plist with normalized kebab-case keys."
  (let* ((defaults jf/gptel-scope-v4-defaults)
         ;; Normalize snake_case to kebab-case
         (normalized (jf/gptel-scope--normalize-keys schema-plist))
         ;; Extract sections
         (paths (plist-get normalized :paths))
         (cloud (plist-get normalized :cloud))
         (security (plist-get normalized :security))
         ;; Merge with defaults (missing sections → defaults)
         (merged-paths (if paths
                          (list :read (or (plist-get paths :read) ())
                                :write (or (plist-get paths :write) ())
                                :execute (or (plist-get paths :execute) ())
                                :modify (or (plist-get paths :modify) ())
                                :deny (or (plist-get paths :deny) ()))
                        (plist-get defaults :paths)))
         (merged-cloud (if cloud
                          (list :auth-detection (or (plist-get cloud :auth-detection)
                                                   (plist-get (plist-get defaults :cloud) :auth-detection)))
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
          :security merged-security)))
;; Load V4 Schema:1 ends here

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

;; Validate Schema V4

;; Validate v4 schema structure and values.


;; [[file:scope-shell-tools.org::*Validate Schema V4][Validate Schema V4:1]]
(defun jf/gptel-scope--validate-schema-v4 (schema-plist)
  "Validate v4 SCHEMA-PLIST structure.
Checks that present sections have valid values.
Fails fast on invalid configuration.

Returns t if valid, signals error otherwise."
  (let ((paths (plist-get schema-plist :paths))
        (cloud (plist-get schema-plist :cloud))
        (security (plist-get schema-plist :security)))
    ;; Validate paths section if present
    (when paths
      (unless (listp paths)
        (error "V4 schema: paths must be a plist"))
      (dolist (key '(:read :write :execute :modify :deny))
        (when (plist-member paths key)
          (let ((value (plist-get paths key)))
            (unless (listp value)
              (error "V4 schema: paths.%s must be a list, got %S" key value))))))
    ;; Validate cloud section if present
    (when cloud
      (jf/gptel-scope--validate-cloud-config cloud))
    ;; Validate security section if present
    (when security
      (jf/gptel-scope--validate-security-config security))
    t))
;; Validate Schema V4:1 ends here

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
        (error "V4 schema: cloud.auth-detection must be \"allow\", \"warn\", or \"deny\", got %S"
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
          (error "V4 schema: security.enforce-parse-complete must be boolean, got %S"
                 enforce-parse-complete)))
      ;; Validate max-coverage-threshold
      (when (plist-member security-plist :max-coverage-threshold)
        (unless (and (numberp max-coverage-threshold)
                    (>= max-coverage-threshold 0.0)
                    (<= max-coverage-threshold 1.0))
          (error "V4 schema: security.max-coverage-threshold must be in [0.0, 1.0], got %S"
                 max-coverage-threshold)))))
  t)
;; Validate Security Config:1 ends here

;; Implementation


;; [[file:scope-shell-tools.org::*Implementation][Implementation:1]]
(defun jf/gptel-bash--parse-command (cmd-string)
  "Extract base command from CMD-STRING.
Handles pipes, redirects, command substitution.
Examples:
  'grep foo | head' → 'grep'
  'ls -la > output.txt' → 'ls'
  'echo $(date)' → 'echo'"
  (let* ((trimmed (string-trim cmd-string))
         ;; Split on shell metacharacters
         (parts (split-string trimmed "[ |><;&]+" t))
         (base (car parts)))
    base))
;; Implementation:1 ends here

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
          :error error-type)))
;; Execute Command:1 ends here

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

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
