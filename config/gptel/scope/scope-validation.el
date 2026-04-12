;;; scope-validation.el --- Scope validation engine -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Unified validation module for scope system.  Contains:
;; - Glob pattern matching (single implementation)
;; - Shared path operation validator (used by filesystem tools AND bash pipeline)
;; - Bash 7-stage validation pipeline
;; - Error codes, violation-info building, error formatting

;;; Code:

;; Dependencies


;; [[file:scope-validation.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'gptel-session-constants)
(require 'jf-gptel-scope-yaml)
(require 'jf-gptel-scope-metadata)
(require 'bash-parser-core)
(require 'bash-parser-orchestrator)
;; Dependencies:1 ends here

;; Glob Pattern Matching

;; Single glob-to-regex implementation used by all path matching in the scope system.
;; Character-by-character parser that handles =/**/=, =**/=, =**=, =*= patterns.


;; [[file:scope-validation.org::*Glob Pattern Matching][Glob Pattern Matching:1]]
(defun jf/gptel-scope--glob-to-regex (pattern)
  "Convert glob PATTERN to regular expression.
- /** becomes /(.*/)? (matches slash followed by zero or more path components)
- **/ becomes (.*/)? (matches zero or more directories ending in slash)
- ** at end becomes .* (matches everything)
- * becomes [^/]* (matches everything except /)
- Other chars are escaped"
  (let ((regex "^"))
    (while (not (string-empty-p pattern))
      (cond
       ((string-prefix-p "/**/" pattern)
        (setq regex (concat regex "/\\(?:.*/\\)?"))
        (setq pattern (substring pattern 4)))

       ((string-prefix-p "/**" pattern)
        (setq regex (concat regex "/.*"))
        (setq pattern (substring pattern 3)))

       ((string-prefix-p "**/" pattern)
        (setq regex (concat regex "\\(?:.*/\\)?"))
        (setq pattern (substring pattern 3)))

       ((string-prefix-p "**" pattern)
        (setq regex (concat regex ".*"))
        (setq pattern (substring pattern 2)))

       ((string-prefix-p "*" pattern)
        (setq regex (concat regex "[^/]*"))
        (setq pattern (substring pattern 1)))

       (t
        (let ((char (substring pattern 0 1)))
          (setq regex (concat regex (regexp-quote char)))
          (setq pattern (substring pattern 1))))))
    (concat regex "$")))

(defun jf/gptel-scope--glob-match-p (path pattern)
  "Return t if PATH matches glob PATTERN.
Case-sensitive matching."
  (let* ((case-fold-search nil)
         (pattern-regex (jf/gptel-scope--glob-to-regex pattern))
         (result (string-match-p pattern-regex path)))
    (if result t nil)))

(defun jf/gptel-scope--path-matches-any-pattern-p (path patterns)
  "Return t if PATH matches any pattern in PATTERNS."
  (when (and path patterns)
    (let ((normalized-path (expand-file-name path)))
      (catch 'matched
        (dolist (pattern patterns)
          (when (jf/gptel-scope--glob-match-p normalized-path pattern)
            (throw 'matched t)))))))
;; Glob Pattern Matching:1 ends here

;; Shared Path Operation Validator

;; Single function that answers: "Is this path allowed for this operation type?"
;; Used by both =validate-filesystem-tool= and bash pipeline stage 6.


;; [[file:scope-validation.org::*Shared Path Operation Validator][Shared Path Operation Validator:1]]
(defun jf/gptel-scope--validate-path-operation (path operation config)
  "Validate PATH for OPERATION against CONFIG.
PATH is an absolute file path.
OPERATION is a keyword symbol: :read, :write, :execute, :modify,
  or extended forms from bash-parser (:read-directory, :read-metadata,
  :match-pattern, :create, :create-or-modify, :append, :delete).
CONFIG is the scope config plist (must contain :paths section).

Permission hierarchy:
- read-like (:read, :read-directory, :read-metadata, :match-pattern):
    requires paths.read OR paths.write (write includes read)
- write-like (:write, :create, :create-or-modify, :append, :delete):
    requires paths.write
- :modify: requires paths.modify OR paths.write
- :execute: requires paths.execute only (high risk)

Deny patterns take absolute precedence.

Returns (:allowed t) on success.
Returns (:allowed nil :error CODE :resource PATH :message STR ...) on denial.
Error codes: \"denied-pattern\" or \"not-in-scope\" (canonical)."
  (cl-block jf/gptel-scope--validate-path-operation
    (let* ((paths-config (plist-get config :paths))
           (deny-patterns (plist-get paths-config :deny))
           (read-patterns (plist-get paths-config :read))
           (write-patterns (plist-get paths-config :write))
           (modify-patterns (plist-get paths-config :modify))
           (execute-patterns (plist-get paths-config :execute)))

      ;; Deny takes precedence over all allow patterns
      (when (jf/gptel-scope--path-matches-any-pattern-p path deny-patterns)
        (cl-return-from jf/gptel-scope--validate-path-operation
          (list :allowed nil
                :error "denied-pattern"
                :resource path
                :operation operation
                :message (format "Path denied by scope: %s" path))))

      ;; Operation-specific validation with hierarchy
      (let ((allowed
             (pcase operation
               ((or :read :read-directory :read-metadata :match-pattern)
                (or (jf/gptel-scope--path-matches-any-pattern-p path read-patterns)
                    (jf/gptel-scope--path-matches-any-pattern-p path write-patterns)))

               ((or :write :create :create-or-modify :append :delete)
                (jf/gptel-scope--path-matches-any-pattern-p path write-patterns))

               (:modify
                (or (jf/gptel-scope--path-matches-any-pattern-p path modify-patterns)
                    (jf/gptel-scope--path-matches-any-pattern-p path write-patterns)))

               (:execute
                (jf/gptel-scope--path-matches-any-pattern-p path execute-patterns))

               (_ nil))))

        (if allowed
            (list :allowed t)
          (list :allowed nil
                :error "not-in-scope"
                :resource path
                :operation operation
                :required-scope (format "paths.%s" operation)
                :allowed-patterns (pcase operation
                                    ((or :read :read-directory :read-metadata :match-pattern)
                                     (append read-patterns write-patterns))
                                    ((or :write :create :create-or-modify :append :delete)
                                     write-patterns)
                                    (:modify (append modify-patterns write-patterns))
                                    (:execute execute-patterns)
                                    (_ nil))
                :message (format "Path not in %s scope: %s" operation path)))))))
;; Shared Path Operation Validator:1 ends here

;; Filesystem Tool Validation

;; Thin wrapper over =validate-path-operation= for filesystem tools.
;; Extracts path from args, converts operation symbol to keyword, calls shared core.


;; [[file:scope-validation.org::*Filesystem Tool Validation][Filesystem Tool Validation:1]]
(defun jf/gptel-scope--validate-filesystem-tool (tool-name operation args config metadata)
  "Validate filesystem tool TOOL-NAME with OPERATION against CONFIG.
OPERATION is a symbol: read, write, execute, modify (from tool definition).
ARGS is the tool arguments list (first arg = filepath).
CONFIG is the scope configuration plist.
METADATA is the file metadata plist (from scope-metadata module).

If the expanded path is denied and differs from its resolved symlink
target, the resolved path is re-validated so a symlink that points into
scope is not blocked by a deny on its containing directory (or vice
versa).  The first passing path wins; otherwise the original denial is
returned annotated with :tool.

Returns a validation result plist."
  (ignore metadata)
  (let* ((filepath (car args))
         (full-path (expand-file-name filepath))
         (real-path (file-truename full-path))
         (op-keyword (intern (concat ":" (symbol-name operation))))
         (result (jf/gptel-scope--validate-path-operation full-path op-keyword config)))
    (cond
     ((plist-get result :allowed)
      result)
     ((not (string= full-path real-path))
      (let ((real-result (jf/gptel-scope--validate-path-operation
                          real-path op-keyword config)))
        (if (plist-get real-result :allowed)
            real-result
          (append result (list :tool tool-name)))))
     (t
      (append result (list :tool tool-name))))))
;; Filesystem Tool Validation:1 ends here

;; Bash Tool Entry Point


;; [[file:scope-validation.org::*Bash Tool Entry Point][Bash Tool Entry Point:1]]
(defun jf/gptel-scope--validate-bash-tool (tool-name args config)
  "Validate bash command using semantic validation pipeline.
TOOL-NAME is the tool being validated.
ARGS is the tool arguments list (command and directory).
CONFIG is the scope configuration plist.

Returns (:allowed t) on success or (:allowed nil :error CODE ...) on
failure. The denial plist preserves the canonical fields produced by the
inner validator (notably :resource for path errors); the wrapper adds
:tool and :command for downstream context."
  (let* ((command-full (car args))
         (directory (cadr args))
         (validation-error (jf/gptel-scope--validate-command-semantics
                            command-full directory config)))
    (if validation-error
        ;; validation-error keys come first so plist-get prefers them.
        ;; The wrapper only adds :tool and :command; it must NOT supply
        ;; a default :resource or it would shadow the canonical :resource
        ;; from validate-file-operation.
        (append validation-error
                (list :tool tool-name
                      :command command-full))
      (list :allowed t))))
;; Bash Tool Entry Point:1 ends here

;; Pipeline Orchestrator

;; Runs each stage in order and exits on the first error. The no-op gate
;; (stage 2) is a deliberate short-circuit: commands with zero filesystem
;; operations are considered safe and bypass the remaining stages. This
;; is what lets =echo hello= and =python3 --version= through without
;; config gymnastics, and is why a command with no file ops is never
;; subjected to cloud-auth checks.


;; [[file:scope-validation.org::*Pipeline Orchestrator][Pipeline Orchestrator:1]]
(defun jf/gptel-scope--validate-command-semantics (command directory scope-config)
  "Run the bash validation pipeline and return nil on success.
COMMAND is the bash command string.
DIRECTORY is the working directory.
SCOPE-CONFIG is the scope configuration plist.

Returns nil if all validations pass, error plist on first failure."
  (cl-block jf/gptel-scope--validate-command-semantics
    (let* ((parsed (jf/bash-parse command))
           (semantics (jf/bash-extract-semantics parsed))
           (security-config (plist-get scope-config :security)))

      ;; Stage 1: Parse completeness
      (when-let ((error (jf/gptel-scope--validate-parse-completeness parsed security-config)))
        (cl-return-from jf/gptel-scope--validate-command-semantics error))

      ;; Stage 2: No-op gate — commands with zero file ops exit the
      ;; pipeline early and bypass file-op + cloud-auth checks.
      (unless (jf/gptel-scope--check-no-op semantics)
        (cl-return-from jf/gptel-scope--validate-command-semantics nil))

      ;; Stage 3: File operations validation
      (when-let ((file-ops (alist-get :filesystem (plist-get semantics :domains))))
        (when-let ((error (jf/gptel-scope--validate-file-operations
                           file-ops directory scope-config)))
          (cl-return-from jf/gptel-scope--validate-command-semantics error)))

      ;; Stage 4: Cloud auth policy
      (when-let ((cloud-auth (alist-get :authentication (plist-get semantics :domains))))
        (when-let ((error (jf/gptel-scope--validate-cloud-auth
                           cloud-auth (plist-get scope-config :cloud))))
          (cl-return-from jf/gptel-scope--validate-command-semantics error)))

      ;; Non-blocking: emit warning if parse coverage is below threshold
      (jf/gptel-scope--check-coverage-threshold semantics security-config)

      nil)))
;; Pipeline Orchestrator:1 ends here

;; Stage 1: Parse Completeness


;; [[file:scope-validation.org::*Stage 1: Parse Completeness][Stage 1: Parse Completeness:1]]
(defun jf/gptel-scope--validate-parse-completeness (parse-result security-config)
  "Stage 1: Check parse completeness.
Returns nil if valid, error plist if incomplete and enforced."
  (let ((complete (plist-get parse-result :parse-complete))
        (enforce (plist-get security-config :enforce-parse-complete))
        (errors (plist-get parse-result :parse-errors)))
    (when (not complete)
      (if enforce
          (list :error "parse_incomplete"
                :message (format "Parse incomplete: %s" errors)
                :parse-errors errors
                :command (plist-get parse-result :input))
        (warn "Parse incomplete: %s" errors)
        nil))))
;; Stage 1: Parse Completeness:1 ends here

;; Stage 2: No-op Gate

;; Predicate used by the orchestrator: commands whose filesystem domain
;; is empty are treated as safe and short-circuit the rest of the
;; pipeline.


;; [[file:scope-validation.org::*Stage 2: No-op Gate][Stage 2: No-op Gate:1]]
(defun jf/gptel-scope--check-no-op (semantics)
  "Return nil if SEMANTICS describes a no-op command, t otherwise.
A no-op is a command whose filesystem domain is empty."
  (let* ((domains (plist-get semantics :domains))
         (file-ops (alist-get :filesystem domains)))
    (if (or (null file-ops) (zerop (length file-ops)))
        nil
      t)))
;; Stage 2: No-op Gate:1 ends here

;; Stage 3: File Operations Validation

;; Uses shared =validate-path-operation= for each extracted file operation.


;; [[file:scope-validation.org::*Stage 3: File Operations Validation][Stage 3: File Operations Validation:1]]
(defun jf/gptel-scope--validate-file-operation (file-op directory config)
  "Validate single FILE-OP against CONFIG using shared path validator.
FILE-OP format: (:file PATH :operation OP :command CMD :confidence CONF)
DIRECTORY is the working directory for resolving relative paths.

Returns nil if allowed, otherwise the denial plist from the shared
validator verbatim.  The shared validator already produces the canonical
contract in `scope/interface--error-codes' (:allowed nil :error CODE
:resource PATH :operation OP :allowed-patterns ... :message STR)."
  (let* ((operation (plist-get file-op :operation))
         (path (plist-get file-op :file))
         (resolved-path (when path
                          (if (file-name-absolute-p path)
                              (expand-file-name path)
                            (expand-file-name path directory)))))
    (when resolved-path
      (let ((result (jf/gptel-scope--validate-path-operation
                     resolved-path operation config)))
        (unless (plist-get result :allowed)
          result)))))

(defun jf/gptel-scope--validate-file-operations (file-ops directory scope-config)
  "Validate all FILE-OPS against SCOPE-CONFIG.
Returns nil if all allowed, error plist for first violation."
  (catch 'error-found
    (dolist (file-op file-ops)
      (when-let ((error (jf/gptel-scope--validate-file-operation
                         file-op directory scope-config)))
        (throw 'error-found error)))))
;; Stage 3: File Operations Validation:1 ends here

;; Stage 4: Cloud Auth Policy


;; [[file:scope-validation.org::*Stage 4: Cloud Auth Policy][Stage 4: Cloud Auth Policy:1]]
(defun jf/gptel-scope--validate-cloud-auth (cloud-auth-ops cloud-config)
  "Detect and enforce cloud authentication policy.
Returns nil if passes, error plist if denied."
  (cl-block jf/gptel-scope--validate-cloud-auth
    (when cloud-auth-ops
      (let* ((mode (or (plist-get cloud-config :auth-detection) "warn"))
             (allowed-providers (plist-get cloud-config :allowed-providers))
             (provider (plist-get cloud-auth-ops :provider))
             (command (plist-get cloud-auth-ops :command)))

        ;; Check provider filtering
        (when (and allowed-providers
                   (not (member provider allowed-providers)))
          (cl-return-from jf/gptel-scope--validate-cloud-auth
            (list :error "cloud_provider_denied"
                  :provider provider
                  :command command
                  :allowed-providers allowed-providers
                  :message (format "Cloud provider '%s' not in allowed list: %s"
                                   provider allowed-providers))))

        (cond
         ((string= mode "allow") nil)

         ((string= mode "warn")
          (list :warning "cloud_auth_detected"
                :provider provider
                :command command
                :message (format "Cloud authentication detected: %s (%s provider)"
                                 command provider)))

         ((string= mode "deny")
          (list :error "cloud_auth_denied"
                :provider provider
                :command command
                :message (format "Cloud authentication denied: %s (%s provider)"
                                 command provider)))

         (t
          (list :error "invalid_cloud_auth_mode"
                :mode mode
                :message (format "Invalid cloud.auth_detection mode: %s" mode))))))))
;; Stage 4: Cloud Auth Policy:1 ends here

;; Coverage Threshold (non-blocking)


;; [[file:scope-validation.org::*Coverage Threshold (non-blocking)][Coverage Threshold (non-blocking):1]]
(defun jf/gptel-scope--check-coverage-threshold (semantics security-config)
  "Check parse coverage ratio and emit a warning if below threshold.
Non-blocking: returns nil always."
  (when-let* ((coverage (plist-get semantics :coverage))
              (threshold (plist-get security-config :max-coverage-threshold))
              (coverage-ratio (plist-get coverage :coverage-ratio)))
    (when (and threshold (< coverage-ratio threshold))
      (warn "Parse coverage %.2f below threshold %.2f"
            coverage-ratio threshold)))
  nil)
;; Coverage Threshold (non-blocking):1 ends here

;; Configuration Loading

;; Load =scope.yml= for the current context. Uses buffer-local
;; =jf/gptel--branch-dir= when available, else falls back to the current
;; buffer's directory. Returns nil if no config can be found or parsed.


;; [[file:scope-validation.org::*Configuration Loading][Configuration Loading:1]]
(defun jf/gptel-scope--load-config ()
  "Load scope configuration from scope.yml.
Uses buffer-local jf/gptel--branch-dir if available.
Returns nil if not found or can't be parsed."
  (condition-case err
      (let ((context-dir (or (and (boundp 'jf/gptel--branch-dir) jf/gptel--branch-dir)
                             (and (buffer-file-name)
                                  (file-name-directory (buffer-file-name))))))
        (when context-dir
          (let ((scope-file (expand-file-name jf/gptel-session--scope-file context-dir)))
            (when (file-exists-p scope-file)
              (jf/gptel-scope-yaml--load-schema scope-file)))))
    (error
     (message "Error loading scope config: %s" (error-message-string err))
     nil)))
;; Configuration Loading:1 ends here

;; Tool Call Validation Entrypoint

;; Single public entrypoint used by =gptel-make-scoped-tool=. Owns config
;; load, metadata gather (for filesystem ops), and bash-vs-filesystem
;; dispatch. OPERATION discriminates:

;; - =read=, =write=, =modify=, =execute= → filesystem path validation
;; - =nil= → semantic extraction (bash pipeline, operations computed from
;;   the command itself)

;; Returns a plist whose keys are guaranteed to include =:allowed= plus
;; =:validation-type= tagging the dispatch branch (=path= or =bash=) so
;; expansion and error formatting can use it without recomputing. When
;; config is missing, returns =(:allowed nil :error "no_scope_config" ...)=.


;; [[file:scope-validation.org::*Tool Call Validation Entrypoint][Tool Call Validation Entrypoint:1]]
(defun jf/gptel-scope-validate-tool-call (tool-name operation args)
  "Validate a scope-aware tool call. Single entrypoint.
TOOL-NAME is the tool name string.
OPERATION is the declared operation symbol (read/write/modify/execute)
or nil for tools whose operations must be extracted from input (bash).
ARGS is the normalized tool argument list.

Returns a plist containing :allowed plus either success data or denial
context. Always includes :validation-type (path or bash)."
  (let ((config (jf/gptel-scope--load-config)))
    (cond
     ((null config)
      ;; Missing config is not a scope violation and does not go through
      ;; expansion, so no :validation-type tag is needed. Keeping the
      ;; plist free of Lisp symbols also lets callers json-serialize it
      ;; directly.
      (list :success nil
            :allowed nil
            :error "no_scope_config"
            :message "No scope configuration found."))
     ((null operation)
      (append (jf/gptel-scope--validate-bash-tool tool-name args config)
              (list :validation-type 'bash)))
     (t
      (let ((metadata (jf/gptel-scope--gather-file-metadata (car args))))
        (append (jf/gptel-scope--validate-filesystem-tool
                 tool-name operation args config metadata)
                (list :validation-type 'path)))))))
;; Tool Call Validation Entrypoint:1 ends here

;; Authorization Dispatcher

;; One entry point per sync/async flow that wraps the full
;; validate → expansion → final-response decision. Callers provide only
;; the tool identity, arguments, and what to do on allow/deny — they do
;; not need to know about =no_scope_config=, expansion UI, or error
;; formatting.

;; =jf/gptel-scope--final-deny-response= centralizes the "what plist does
;; a denied call return to its caller" decision. =no_scope_config= is not
;; a scope violation and is surfaced verbatim so the LLM sees the raw
;; error; real violations go through =format-tool-error=.

;; =jf/gptel-scope-authorize-tool-call= is the async dispatcher. Sync
;; tools don't have a dispatcher because they can't escalate to expansion
;; — they call =jf/gptel-scope-validate-tool-call= and
;; =jf/gptel-scope--final-deny-response= directly.


;; [[file:scope-validation.org::*Authorization Dispatcher][Authorization Dispatcher:1]]
(defun jf/gptel-scope--final-deny-response (tool-name args check-result)
  "Return the response plist that should be delivered for a denied call.
TOOL-NAME is the denied tool. ARGS is the tool's normalized argument
list. CHECK-RESULT is the validation plist. A missing scope config is
returned verbatim; real scope violations are formatted for the LLM."
  (if (equal (plist-get check-result :error) "no_scope_config")
      check-result
    (jf/gptel-scope--format-tool-error
     tool-name (nth 0 args) check-result)))

(defun jf/gptel-scope-authorize-tool-call (tool-name operation args on-allow on-deny)
  "Validate a tool call and run ON-ALLOW or ON-DENY.
TOOL-NAME, OPERATION, and ARGS match `jf/gptel-scope-validate-tool-call'.

ON-ALLOW is a thunk invoked when the call is authorized — either
because validation passed outright or because the user approved
through the expansion UI. ON-DENY is invoked with a response plist
suitable for serializing back to the LLM.

This function is async: expansion UI may resolve on a later turn, so
the callbacks must be prepared to run after this function returns."
  (let ((check-result (jf/gptel-scope-validate-tool-call
                       tool-name operation args)))
    (cond
     ((plist-get check-result :allowed)
      (funcall on-allow))

     ((equal (plist-get check-result :error) "no_scope_config")
      (funcall on-deny check-result))

     (t
      (jf/gptel-scope--trigger-inline-expansion
       check-result tool-name
       (lambda (expansion-result)
         (if (plist-get expansion-result :approved)
             (funcall on-allow)
           (funcall on-deny
                    (jf/gptel-scope--final-deny-response
                     tool-name args check-result)))))))))
;; Authorization Dispatcher:1 ends here

;; Violation-Info Building

;; Transform validation error plists into format expected by expansion UI.


;; [[file:scope-validation.org::*Violation-Info Building][Violation-Info Building:1]]
(defun jf/gptel-scope--build-violation-info (validation-error tool-name)
  "Transform VALIDATION-ERROR into violation-info for expansion UI.
TOOL-NAME is the denied tool.
VALIDATION-ERROR must carry :validation-type (attached by the
validation entrypoint).

Returns plist with :tool, :resource, :operation, :reason,
:validation-type, :metadata."
  (let* ((error-type (or (plist-get validation-error :error) "unknown"))
         (validation-type (plist-get validation-error :validation-type))
         (resource (pcase error-type
                     ("denied-pattern" (plist-get validation-error :resource))
                     ("not-in-scope" (plist-get validation-error :resource))
                     ("parse_incomplete" (plist-get validation-error :command))
                     ("cloud_auth_denied" (plist-get validation-error :provider))
                     ("cloud_provider_denied" (plist-get validation-error :provider))
                     (_ (or (plist-get validation-error :resource)
                           (plist-get validation-error :path)))))
         (operation (plist-get validation-error :operation))
         (reason (plist-get validation-error :message))
         (metadata (plist-get validation-error :metadata)))
    (list :tool tool-name
          :resource resource
          :operation operation
          :reason reason
          :validation-type validation-type
          :metadata metadata)))
;; Violation-Info Building:1 ends here

;; Error Formatting

;; Format validation errors for LLM consumption.


;; [[file:scope-validation.org::*Error Formatting][Error Formatting:1]]
(defun jf/gptel-scope--format-tool-error (tool-name resource check-result)
  "Format tool permission error for LLM.
CHECK-RESULT is the validation result plist."
  (let ((patterns (or (plist-get check-result :allowed-patterns)
                      (plist-get check-result :patterns)))
        (deny-patterns (plist-get check-result :deny-patterns))
        (error-type (or (plist-get check-result :error) "scope-violation"))
        (custom-message (plist-get check-result :message))
        (command (plist-get check-result :command))
        (directory (plist-get check-result :directory))
        (required-scope (plist-get check-result :required-scope)))
    (list :success nil
          :error error-type
          :tool tool-name
          :resource resource
          :command command
          :directory directory
          :required-scope required-scope
          :allowed-patterns patterns
          :deny-patterns deny-patterns
          :message (or custom-message
                      (format "Tool '%s' denied for resource '%s'. Use request_scope_expansion to ask user for approval."
                             tool-name resource)))))
;; Error Formatting:1 ends here

;; Trigger Inline Expansion

;; Invoke expansion UI. The wrapper passes in the validation error (which
;; already carries =:validation-type=) plus a callback. The expansion UI's
;; chosen action — approve, allow-once, or deny — resolves the callback
;; with =(:approved t)= or =(:approved nil ...)=. The wrapper trusts that
;; answer as authorization for the single pending tool invocation; there
;; is no re-validation.


;; [[file:scope-validation.org::*Trigger Inline Expansion][Trigger Inline Expansion:1]]
(defun jf/gptel-scope--trigger-inline-expansion (validation-error tool-name wrapper-callback)
  "Trigger inline expansion UI for VALIDATION-ERROR.
TOOL-NAME is the denied tool. WRAPPER-CALLBACK is invoked with a plist
carrying :approved t/nil once the user chooses. VALIDATION-ERROR must
include :validation-type (attached upstream by the validation entry
point)."
  (let* ((violation-info (jf/gptel-scope--build-violation-info
                          validation-error tool-name))
         (resource (plist-get violation-info :resource))
         (patterns (list resource))
         (expansion-callback
          (lambda (result-json)
            (condition-case err
                ;; json-parse-string returns the keyword `:false` for JSON
                ;; `false` under the default :false-object, which is truthy
                ;; in elisp. Only `t` counts as approval here.
                (let* ((parsed (json-parse-string result-json :object-type 'plist))
                       (approved (eq (plist-get parsed :success) t)))
                  (if approved
                      (funcall wrapper-callback (list :approved t))
                    (funcall wrapper-callback (list :approved nil :reason "user_denied"))))
              (error
               (message "Error in expansion-callback: %s" (error-message-string err))
               (funcall wrapper-callback (list :approved nil :reason "callback_error")))))))

    (jf/gptel-scope-prompt-expansion violation-info expansion-callback patterns tool-name)
    nil))
;; Trigger Inline Expansion:1 ends here

;; Provide Feature


;; [[file:scope-validation.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-validation)
;;; scope-validation.el ends here
;; Provide Feature:1 ends here
