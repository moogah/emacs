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
(require 'jf-gptel-scope-metadata)
(require 'bash-parser-core)
(require 'bash-parser-orchestrator)
;; Dependencies:1 ends here

;; Constants

;; These are module-level structural constants of the validator. They are
;; *not* user-tunable and *not* per-session configurable: the public
;; contract is that parse-completeness is enforced unconditionally and the
;; coverage warning threshold is exactly 100%. Any caller that reaches for
;; a per-session override here is fighting the contract — see
;; =interfaces.org= § Scope Config Shape.


;; [[file:scope-validation.org::*Constants][Constants:1]]
(defconst jf/gptel-scope--enforce-parse-complete t
  "Whether Stage 1 of the bash validation pipeline refuses commands with
incomplete parses. Fixed at module load; not configurable per session.")

(defconst jf/gptel-scope--coverage-threshold 1.0
  "Threshold below which the bash semantic-plugin coverage check emits
a non-blocking warning. Fixed at module load; not configurable per session.")
;; Constants:1 ends here

;; Operation → Scope Section Mapping

;; Canonical mapping from bash-parser operation keywords to the three scope
;; sections (=paths.read=, =paths.write=, =paths.execute=). The bash-parser
;; emits 11 operation types, but the scope schema only has three section
;; keys; this function reduces one vocabulary to the other.

;; Rule: every site that routes by operation — section targeting in the
;; expansion UI, drawer updates, etc. — must call this function rather than
;; inlining its own =pcase=.

;; Fail-safe default: unknown operations (and =nil=) map to =:write=. Writing
;; is the most restrictive section, so misrouting unknowns there requires the
;; user to grant broader permission deliberately rather than silently granting
;; read access.


;; [[file:scope-validation.org::*Operation → Scope Section Mapping][Operation → Scope Section Mapping:1]]
(defun jf/gptel-scope--map-operation-to-scope-section (operation)
  "Map OPERATION keyword to the scope section key it belongs in.
Returns one of :read, :write, :execute.
Unknown operations (including nil) map to :write (fail-safe)."
  (pcase operation
    ((or :read :read-directory :read-metadata :match-pattern) :read)
    ((or :write :create :create-or-modify :append :delete :modify) :write)
    (:execute :execute)
    (_ :write)))
;; Operation → Scope Section Mapping:1 ends here

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
ARGS is the tool arguments list (single-element: the command string).
Relative paths in the command are resolved against `default-directory',
which is bound from the session context before this function runs.
CONFIG is the scope configuration plist.

Returns (:allowed t) on success or (:allowed nil :error CODE ...) on
failure. The denial plist preserves the canonical fields produced by the
inner validator (notably :resource for path errors); the wrapper adds
:tool and :command for downstream context."
  (let* ((command-full (car args))
         (directory default-directory)
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
SCOPE-CONFIG is the scope configuration plist (top-level keys :paths
and :cloud only; see
`register/invariant/scope-no-security-key-in-plist').

Returns nil if all validations pass, error plist on first failure."
  (cl-block jf/gptel-scope--validate-command-semantics
    (let* ((parsed (jf/bash-parse command))
           (semantics (jf/bash-extract-semantics parsed)))

      ;; Stage 1: Parse completeness
      (when-let ((error (jf/gptel-scope--validate-parse-completeness parsed)))
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
      (jf/gptel-scope--check-coverage-threshold semantics)

      nil)))
;; Pipeline Orchestrator:1 ends here

;; Stage 1: Parse Completeness

;; Reads the module-level constant =jf/gptel-scope--enforce-parse-complete=
;; directly. The constant is fixed at =t= by contract (see =* Constants=
;; and =register/invariant/scope-parse-complete-is-true=); the former
;; "warn when not enforced" branch is gone with the per-session override.


;; [[file:scope-validation.org::*Stage 1: Parse Completeness][Stage 1: Parse Completeness:1]]
(defun jf/gptel-scope--validate-parse-completeness (parse-result)
  "Stage 1: Check parse completeness.
Returns nil if valid, error plist if incomplete.
Parse completeness is always enforced (see
`jf/gptel-scope--enforce-parse-complete')."
  (let ((complete (plist-get parse-result :parse-complete))
        (errors (plist-get parse-result :parse-errors)))
    (when (and (not complete)
               jf/gptel-scope--enforce-parse-complete)
      (list :error "parse_incomplete"
            :message (format "Parse incomplete: %s" errors)
            :parse-errors errors
            :command (plist-get parse-result :input)))))
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

;; Reads the module-level constant =jf/gptel-scope--coverage-threshold=
;; directly. The constant is fixed at =1.0= by contract (see =* Constants=
;; and =register/invariant/scope-coverage-threshold-is-1=).


;; [[file:scope-validation.org::*Coverage Threshold (non-blocking)][Coverage Threshold (non-blocking):1]]
(defun jf/gptel-scope--check-coverage-threshold (semantics)
  "Check parse coverage ratio and emit a warning if below threshold.
Non-blocking: returns nil always.  Threshold is fixed at
`jf/gptel-scope--coverage-threshold' by module contract."
  (when-let* ((coverage (plist-get semantics :coverage))
              (coverage-ratio (plist-get coverage :coverage-ratio)))
    (when (< coverage-ratio jf/gptel-scope--coverage-threshold)
      (warn "Parse coverage %.2f below threshold %.2f"
            coverage-ratio jf/gptel-scope--coverage-threshold)))
  nil)
;; Coverage Threshold (non-blocking):1 ends here

;; Stage 1 — buffer-first read

;; Walks the file-level =:PROPERTIES:= drawer at =point-min= via
;; =org-entry-get= (scalars) and =org-entry-get-multivalued-property=
;; (lists). Follows the existing =gptel-chat--declared-preset= convention
;; in =config/gptel/chat/menu.el= for accessing the file-level drawer
;; (=save-excursion= + =save-restriction= + =widen=).

;; Missing list keys default to nil (empty list); missing
;; =:auth-detection= defaults to ="warn"=. An invalid scalar value for
;; =GPTEL_SCOPE_CLOUD_AUTH= raises a structured error rather than
;; silently falling back, since the closed value set is part of the
;; vocabulary contract.


;; [[file:scope-validation.org::*Stage 1 — buffer-first read][Stage 1 — buffer-first read:1]]
(defun jf/gptel-scope--normalize-provider-keyword (s)
  "Normalize a drawer provider value S to a keyword.
The drawer carries provider names as bare strings (e.g. \"aws\",
\"gcp\"); the validator's `(member provider allowed-providers)' check
compares against the cloud-auth ops detector's keyword providers
(`:aws', `:aws-cli', etc.).  This boundary helper converts strings to
keywords so the loader's :allowed-providers list matches the
validator's expected shape (cycle-4 Ask 2)."
  (intern (concat ":" (string-remove-prefix ":" s))))

(defun jf/gptel-scope--load-from-buffer (buffer)
  "Read scope configuration from BUFFER's file-level `:PROPERTIES:' drawer.
Returns a plist of the canonical shape (:paths (:read ... :write ... :modify
... :execute ... :deny ...) :cloud (:auth-detection ... :allowed-providers ...))
with empty lists for missing list keys and \"warn\" as the default for
missing :auth-detection.  Does not consult the file on disk; the buffer is
the source of truth.

Cloud provider drawer values (`GPTEL_SCOPE_CLOUD_PROVIDERS') are
normalized to keywords at the parse boundary so the in-memory shape
matches the cloud-auth ops detector's keyword producers
(register/shape/scope-config-plist `:cloud.allowed-providers').

Raises an error when GPTEL_SCOPE_CLOUD_AUTH carries a scalar outside the
closed set defined by `register/vocabulary/drawer-key-set'."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((auth (or (org-entry-get (point) "GPTEL_SCOPE_CLOUD_AUTH") "warn")))
          (unless (member auth '("allow" "warn" "deny"))
            (error "Scope schema: GPTEL_SCOPE_CLOUD_AUTH must be \"allow\", \"warn\", or \"deny\", got %S" auth))
          (list :paths
                (list :read          (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_READ")
                      :read-metadata (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_READ_METADATA")
                      :write         (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_WRITE")
                      :modify        (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_MODIFY")
                      :execute       (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_EXECUTE")
                      :deny          (org-entry-get-multivalued-property (point) "GPTEL_SCOPE_DENY"))
                :cloud
                (list :auth-detection auth
                      :allowed-providers
                      (mapcar #'jf/gptel-scope--normalize-provider-keyword
                              (org-entry-get-multivalued-property
                               (point) "GPTEL_SCOPE_CLOUD_PROVIDERS")))))))))
;; Stage 1 — buffer-first read:1 ends here

;; Stage 2 — file-fallback read

;; Same reader, but opens the file via =with-temp-buffer= +
;; =insert-file-contents=, enables =org-mode=, and discards the temp
;; buffer. The plist shape is *byte-for-byte identical* to the stage-1
;; output for the same drawer text — see the round-trip invariant in
;; =register/boundary/scope-config-loader=.


;; [[file:scope-validation.org::*Stage 2 — file-fallback read][Stage 2 — file-fallback read:1]]
(defun jf/gptel-scope--load-from-file (file)
  "Read scope configuration from FILE's `:PROPERTIES:' drawer headlessly.
Same return shape as `jf/gptel-scope--load-from-buffer'.  Used when no live
chat buffer is available for the session (e.g. programmatic
`request_scope_expansion' callers)."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (jf/gptel-scope--load-from-buffer (current-buffer))))
;; Stage 2 — file-fallback read:1 ends here

;; Helpers

;; =--find-session-buffer-for-dir= maps a branch-dir back to its live chat
;; buffer. The session registry (=jf/gptel--session-registry=) is a hash
;; table keyed by ="session-id/branch-name"= — *not* by branch-dir — so
;; the lookup must scan registry *values* for a matching =:branch-dir=.
;; The buffer-list scan is a redundant fallback for the case where the
;; registry has not been initialised (or where =jf/gptel--branch-dir= was
;; set buffer-locally without going through =jf/gptel--register-session=).

;; =--has-any-scope-key-p= is a pure predicate used by the loader to
;; distinguish "drawer present but empty" (no =:GPTEL_SCOPE_*:= keys —
;; e.g. a =session.org= carrying only =:GPTEL_PRESET:=) from "drawer
;; populated with scope keys". When the predicate returns nil, the
;; loader composes deny-all defaults via =--deny-all-defaults=; the
;; dispatcher then proceeds to validation and per-violation deny is the
;; authorisation outcome (not =no_scope_config=).

;; =--deny-all-defaults= returns the canonical deny-all scope-config
;; plist: empty =:paths= sub-lists and ="deny"= cloud-auth. An empty
;; drawer therefore behaves as a deny-all configuration rather than a
;; config-missing signal — see =register/boundary/scope-config-loader=
;; for the cycle-3 disposition history.


;; [[file:scope-validation.org::*Helpers][Helpers:1]]
(defun jf/gptel-scope--find-session-buffer-for-dir (dir)
  "Find a live chat buffer whose `jf/gptel--branch-dir' equals DIR.
Returns the buffer or nil.  Uses the session registry when available;
falls back to scanning the buffer list when no registry entry matches."
  (when dir
    (or (and (boundp 'jf/gptel--session-registry)
             (hash-table-p jf/gptel--session-registry)
             (let ((match nil))
               (maphash
                (lambda (_key entry)
                  (when (and (null match)
                             (equal (plist-get entry :branch-dir) dir)
                             (buffer-live-p (plist-get entry :buffer)))
                    (setq match (plist-get entry :buffer))))
                jf/gptel--session-registry)
               match))
        (seq-find (lambda (buf)
                    (and (buffer-local-value 'jf/gptel--branch-dir buf)
                         (string= dir
                                  (buffer-local-value 'jf/gptel--branch-dir buf))))
                  (buffer-list)))))

(defun jf/gptel-scope--has-any-scope-key-p (config)
  "Return non-nil iff CONFIG has at least one populated :GPTEL_SCOPE_* key.
Pure predicate (no side effects). Returns t when CONFIG carries any
non-empty path list, any allowed cloud provider, or a non-default
auth-detection value; returns nil for an empty plist (every list
empty, auth-detection \"warn\" or unset).

Callers compose deny-all defaults (via
`jf/gptel-scope--deny-all-defaults') when this returns nil — an empty
drawer is a deny-all configuration, not a config-missing signal."
  (let ((paths (plist-get config :paths))
        (cloud (plist-get config :cloud)))
    (or (plist-get paths :read)
        (plist-get paths :read-metadata)
        (plist-get paths :write)
        (plist-get paths :modify)
        (plist-get paths :execute)
        (plist-get paths :deny)
        (plist-get cloud :allowed-providers)
        ;; A non-default auth value also indicates intent.
        (let ((auth (plist-get cloud :auth-detection)))
          (and auth (not (string= auth "warn")))))))

(defun jf/gptel-scope--deny-all-defaults ()
  "Return the canonical deny-all scope-config plist.
Used by `jf/gptel-scope--load-config' Stage 3 when the resolved
drawer has no :GPTEL_SCOPE_* keys (or no drawer is found at all).

Shape conforms to `register/shape/scope-config-plist': empty :paths
sub-lists and \"deny\" cloud auth-detection. Every read/write/modify/
execute path validation will deny against this config; cloud auth is
denied for all providers. Replaces the historical Stage-3 collapse-
to-nil semantic — see the cycle-3 disposition documented in
`register/boundary/scope-config-loader'."
  (list :paths (list :read nil
                     :read-metadata nil
                     :write nil
                     :modify nil
                     :execute nil
                     :deny nil)
        :cloud (list :auth-detection "deny"
                     :allowed-providers nil)))
;; Helpers:1 ends here

;; Dispatcher

;; The dispatcher is buffer-first: when a chat buffer exists for the
;; session's branch-dir, its drawer is the source of truth (so a user's
;; just-typed edit is visible to validation before they save). The file
;; fallback covers programmatic callers (and headless tests).

;; Stage 3 — empty-drawer composition: when neither stage 1 (buffer
;; read) nor stage 2 (file read) yields a plist with at least one
;; =:GPTEL_SCOPE_*:= key, =--load-config= returns the deny-all defaults
;; plist composed by =--deny-all-defaults= (empty =:paths= sub-lists,
;; ="deny"= cloud auth-detection). The authorization dispatcher
;; proceeds to validation; per-violation deny is the authorisation
;; outcome — not =no_scope_config=. See
;; =register/boundary/scope-config-loader= for the cycle-3 disposition
;; history (Option B).

;; A drawer with at least one scope key set returns the full plist; the
;; underlying buffer/file readers default missing list keys to the empty
;; list and missing =:auth-detection= to ="warn"=, so consumers always
;; see a complete shape (per =register/shape/scope-config-plist=).


;; [[file:scope-validation.org::*Dispatcher][Dispatcher:1]]
(defun jf/gptel-scope--load-config (&optional branch-dir)
  "Resolve scope configuration for the current session, drawer-first.
Buffer-first: if a chat buffer exists for BRANCH-DIR (default: buffer-local
`jf/gptel--branch-dir', then `default-directory'), reads its drawer.
Otherwise reads the file at <branch-dir>/session.org.

Stage 3 — empty-drawer composition: when neither stage produces a
plist with at least one :GPTEL_SCOPE_* key (no buffer + no file, or
file present but drawer empty), returns the deny-all defaults plist
composed by `jf/gptel-scope--deny-all-defaults' rather than nil. The
dispatcher proceeds to validation and per-violation deny is the
authorisation outcome.

The returned plist conforms to `register/shape/scope-config-plist':
two top-level keys, =:paths= and =:cloud=.  Stage 1 of the bash
pipeline reads the parse-completeness flag from the module-level
constant `jf/gptel-scope--enforce-parse-complete'; Stage 5 reads
`jf/gptel-scope--coverage-threshold' similarly."
  (condition-case err
      (let* ((dir (or branch-dir
                      (and (boundp 'jf/gptel--branch-dir) jf/gptel--branch-dir)
                      default-directory))
             (buffer (jf/gptel-scope--find-session-buffer-for-dir dir))
             (file (and dir (expand-file-name "session.org" dir)))
             (config (cond
                      (buffer (jf/gptel-scope--load-from-buffer buffer))
                      ((and file (file-exists-p file))
                       (jf/gptel-scope--load-from-file file))
                      (t nil))))
        (cond
         ((null config) (jf/gptel-scope--deny-all-defaults))
         ((jf/gptel-scope--has-any-scope-key-p config) config)
         (t (jf/gptel-scope--deny-all-defaults))))
    (error
     (message "Error loading scope config: %s" (error-message-string err))
     (jf/gptel-scope--deny-all-defaults))))
;; Dispatcher:1 ends here

;; Multi-violation expansion loop

;; Bash commands frequently carry multiple file operations (e.g.
;; =ls /foo 2>/dev/null= is both a read on =/foo= and a write on
;; =/dev/null=). Stage 3 of the pipeline returns on the first denied
;; op, so a single validation pass surfaces exactly one violation.

;; After the user approves *add-to-scope*, the dispatcher re-invokes
;; itself so the pipeline re-validates against the now-updated config.
;; If another op is still denied, the expansion UI prompts again for
;; the new first violation; the loop continues until validation passes
;; or the user denies.

;; *Allow-once* is exempt from re-validation: it is a single-use bypass
;; that authorizes the whole pending invocation. Re-validating would
;; prompt the user again for the remaining violations, which defeats
;; the "just let this one through" intent.

;; *Origin-buffer is preserved across re-entry.* The expansion-UI
;; wrapper-callback fires from inside the action handler, whose
;; =current-buffer= is the buffer the user clicked from — typically NOT
;; the buffer that owns the pending tool call (e.g. for PersistentAgents
;; the click happens in the parent's overlay buffer while the pending
;; tool call belongs to the agent's invisible session buffer). Both
;; =jf/gptel-scope--load-config= (which reads buffer-local
;; =jf/gptel--branch-dir= to pick a drawer) and any subsequent
;; =jf/gptel-scope-prompt-expansion= (which captures =(current-buffer)=
;; as =:chat-buffer= for the writer) read =current-buffer=, so the
;; re-entrant call MUST run in the request's original buffer or
;; re-validation routes to the wrong session and writes contaminate
;; the wrong drawer. We capture =origin-buffer= at the top of
;; =authorize-tool-call= and re-establish it via =with-current-buffer=
;; across the wrapper-callback so on-allow / on-deny / re-entry all run
;; in the same buffer context as the original call. See
;; =openspec/specs/gptel/scope.md= § Authorize-tool-call origin buffer.


;; [[file:scope-validation.org::*Multi-violation expansion loop][Multi-violation expansion loop:1]]
(defun jf/gptel-scope--final-deny-response (tool-name args check-result)
  "Return the response plist that should be delivered for a denied call.
TOOL-NAME is the denied tool. ARGS is the tool's normalized argument
list. CHECK-RESULT is the validation plist. All denials are formatted
for the LLM via `jf/gptel-scope--format-tool-error' — the historical
no_scope_config short-circuit was removed when an empty drawer became
a deny-all configuration (Option B; see
register/boundary/scope-config-loader)."
  (jf/gptel-scope--format-tool-error
   tool-name (nth 0 args) check-result))

(defun jf/gptel-scope--validate-tool-call (tool-name operation args config)
  "Dispatch TOOL-NAME's ARGS against CONFIG and return a validation plist.
OPERATION is the declared filesystem operation symbol, or nil for
tools whose operations are extracted from input (bash). The returned
plist always includes :validation-type (path or bash)."
  (if (null operation)
      (append (jf/gptel-scope--validate-bash-tool tool-name args config)
              (list :validation-type 'bash))
    (let ((metadata (jf/gptel-scope--gather-file-metadata (car args))))
      (append (jf/gptel-scope--validate-filesystem-tool
               tool-name operation args config metadata)
              (list :validation-type 'path)))))

(defun jf/gptel-scope-authorize-tool-call (tool-name operation args on-allow on-deny)
  "Validate a scope-aware tool call and run ON-ALLOW or ON-DENY.
TOOL-NAME is the tool name string.
OPERATION is the declared operation symbol (read/write/modify/execute)
or nil for tools whose operations must be extracted from input (bash).
ARGS is the normalized tool argument list.

ON-ALLOW is a thunk invoked when the call is authorized — either
because validation passed outright or because the user approved
through the expansion UI. ON-DENY is invoked with a response plist
suitable for serializing back to the LLM.

When the user approves an add-to-scope expansion, the dispatcher
re-invokes itself to re-validate against the updated config.  That
surfaces subsequent denials (one per prompt) until every op in the
command passes.  Allow-once bypasses re-validation and authorizes
the pending invocation outright.

This function is async: expansion UI may resolve on a later turn, so
the callbacks must be prepared to run after this function returns.

The wrapper-callback re-establishes ORIGIN-BUFFER (the buffer current
at first entry, typically the request-owning buffer) for on-allow,
on-deny, and the re-entrant authorize-tool-call call, so re-validation
loads the right session's drawer and any follow-up prompt-expansion
captures the right `:chat-buffer'."
  (let ((origin-buffer (current-buffer))
        (config (jf/gptel-scope--load-config)))
    ;; --load-config always returns a plist (deny-all defaults when
    ;; the drawer is missing/empty per Option B); proceed directly to
    ;; validation. An empty drawer denies every op as a per-violation
    ;; deny, surfaceable through the expansion UI like any other
    ;; scope violation.
    (let ((check-result (jf/gptel-scope--validate-tool-call
                         tool-name operation args config)))
      (cond
       ((plist-get check-result :allowed)
        (funcall on-allow))

       (t
        (jf/gptel-scope--trigger-inline-expansion
         check-result tool-name
         (lambda (expansion-result)
           ;; Re-establish the origin buffer.  Without this, the
           ;; recursive authorize-tool-call below reads
           ;; `jf/gptel--branch-dir' from the action handler's
           ;; current-buffer (the buffer the user clicked from),
           ;; causing re-validation to load the wrong session's
           ;; drawer; subsequent prompt-expansion captures the wrong
           ;; `:chat-buffer' and the writer contaminates the wrong
           ;; drawer.  See section commentary above.
           (with-current-buffer (if (buffer-live-p origin-buffer)
                                    origin-buffer
                                  (current-buffer))
             (cond
              ((not (plist-get expansion-result :approved))
               (funcall on-deny
                        (jf/gptel-scope--final-deny-response
                         tool-name args check-result)))
              ;; Allow-once: single-use bypass, skip re-validation so
              ;; remaining denials in the same command don't prompt.
              ((plist-get expansion-result :allowed-once)
               (funcall on-allow))
              ;; Add-to-scope: re-validate against the updated config.
              ;; If another op is still denied, the next iteration
              ;; prompts for it.
              (t
               (jf/gptel-scope-authorize-tool-call
                tool-name operation args on-allow on-deny)))))))))))
;; Multi-violation expansion loop:1 ends here

;; Violation-Info Building

;; Transform validation error plists into format expected by expansion UI.


;; [[file:scope-validation.org::*Violation-Info Building][Violation-Info Building:1]]
(defun jf/gptel-scope--build-violation-info (validation-error tool-name)
  "Transform VALIDATION-ERROR into violation-info for expansion UI.
TOOL-NAME is the denied tool.
VALIDATION-ERROR must carry :validation-type (attached by the
validation entrypoint).

Returns plist with :tool, :resource, :operation, :reason,
:validation-type, :metadata, :error.

The :error field (machine-readable error code, e.g. \"parse_incomplete\",
\"cloud_auth_denied\") is preserved for action-handler dispatch — the
expansion UI's --add-to-scope refuses on parse_incomplete and routes
cloud_auth_denied to the providers drawer rather than the operation
collapse (see register/boundary/scope-expansion-action-handler).
register/shape/violation-info marks :error as optional."
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
          :metadata metadata
          :error error-type)))
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
;; chosen action — add-to-scope, allow-once, or deny — resolves the
;; callback with one of:

;; - =(:approved t)= — user added the denied resource to the session
;;   drawer. The dispatcher re-validates against the updated config; if
;;   another op is still denied, the next iteration prompts for it.
;; - =(:approved t :allowed-once t)= — user granted a single-use bypass.
;;   The dispatcher runs the body without re-validating.
;; - =(:approved nil :reason ...)= — user denied or the callback errored.


;; [[file:scope-validation.org::*Trigger Inline Expansion][Trigger Inline Expansion:1]]
(defun jf/gptel-scope--trigger-inline-expansion (validation-error tool-name wrapper-callback)
  "Trigger inline expansion UI for VALIDATION-ERROR.
TOOL-NAME is the denied tool. WRAPPER-CALLBACK is invoked with a plist
carrying :approved t/nil once the user chooses. When the result JSON
carries :allowed_once t, the wrapper plist includes :allowed-once t so
the dispatcher knows to skip re-validation.  VALIDATION-ERROR must
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
                       (approved (eq (plist-get parsed :success) t))
                       (allowed-once (eq (plist-get parsed :allowed_once) t)))
                  (if approved
                      (funcall wrapper-callback
                               (list :approved t :allowed-once allowed-once))
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
