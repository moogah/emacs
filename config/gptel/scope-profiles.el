;;; scope-profiles.el --- GPTEL Scope Profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Scope profile templates for gptel sessions.
;; Loads YAML profile templates, resolves them for presets,
;; expands variables, and renders the result as an org `:PROPERTIES:'
;; drawer block (Mode 2a) or applies it to an open buffer's drawer
;; (Mode 2b).  The previous `scope.yml' writer has been removed.

;;; Code:

(require 'cl-lib)
(require 'yaml)
(require 'org)
(require 'gptel-session-constants)
(require 'gptel-session-logging)

(defun jf/gptel-scope-profile--normalize-boolean (value)
  "Normalize YAML boolean keyword VALUE to elisp boolean.
Converts :true → t, :false/:null → nil.
Returns VALUE unchanged if it's not a boolean keyword."
  (cond
   ((eq value :true) t)
   ((eq value :false) nil)
   ((eq value :null) nil)
   (t value)))

(defun jf/gptel-scope-profile--normalize-keys (plist)
  "Normalize PLIST keys from snake_case to kebab-case.
Also normalizes YAML boolean keywords (:true, :false, :null) to elisp
booleans.  Recursively processes nested plists."
  (let ((result nil))
    (while plist
      (let* ((key (car plist))
             (value (cadr plist))
             (normalized-key (intern (replace-regexp-in-string
                                      "_" "-"
                                      (symbol-name key))))
             (normalized-value
              (cond
               ;; Nested plist: recurse
               ((and (listp value)
                     (not (null value))
                     (keywordp (car value)))
                (jf/gptel-scope-profile--normalize-keys value))
               ;; Boolean keywords: normalize via shared helper
               (t (jf/gptel-scope-profile--normalize-boolean value)))))
        (setq result (plist-put result normalized-key normalized-value))
        (setq plist (cddr plist))))
    result))

(defun jf/gptel-scope-profile--load (profile-name)
  "Load scope profile PROFILE-NAME from the profiles directory.
PROFILE-NAME is the base name without .yml extension.
Returns a plist with :paths, :shell-commands, :bash-tools.
Returns nil and logs a warning if the file is missing or cannot be parsed.

VALIDATION: Rejects profiles with bash_tools.categories section.
Migration: Remove categories section, keep only deny list."
  (let ((profile-file (expand-file-name
                       (concat profile-name ".yml")
                       jf/gptel--scope-profiles-directory)))
    (if (not (file-exists-p profile-file))
        (progn
          (jf/gptel--log 'warn "Scope profile file not found: %s" profile-file)
          nil)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents profile-file)
            (let* ((parsed (yaml-parse-string (buffer-string) :object-type 'plist))
                   (normalized (jf/gptel-scope-profile--normalize-keys parsed))
                   (bash-tools (plist-get normalized :bash-tools))
                   (categories (when bash-tools (plist-get bash-tools :categories))))
              ;; Validate: reject if categories section present
              (when categories
                (error "bash_tools.categories section no longer supported. Migration: Remove categories section, keep only deny list. See CLAUDE.md for migration guide"))
              (jf/gptel--log 'debug "Loaded scope profile: %s" profile-name)
              normalized))
        (error
         (jf/gptel--log 'warn "Failed to parse scope profile %s: %s"
                       profile-name (error-message-string err))
         nil)))))

(defun jf/gptel-scope-profile--resolve (preset-name)
  "Resolve scope profile for PRESET-NAME.
Looks up `jf/gptel-preset--scope-defaults' for the preset.
Priority:
  1. Named profile via :scope-profile key -> load from file
  2. Inline scope defaults (plist with :paths etc.) -> return directly
  3. No scope defaults -> return nil

Returns a scope plist or nil."
  (let ((scope-config (alist-get preset-name jf/gptel-preset--scope-defaults)))
    (cond
     ;; No scope configuration for this preset
     ((null scope-config)
      (jf/gptel--log 'debug "No scope defaults for preset: %s" preset-name)
      nil)

     ;; Named profile reference
     ((plist-get scope-config :scope-profile)
      (let ((profile-name (plist-get scope-config :scope-profile)))
        (jf/gptel--log 'debug "Resolving named scope profile '%s' for preset: %s"
                      profile-name preset-name)
        (let ((loaded (jf/gptel-scope-profile--load profile-name)))
          (unless loaded
            (jf/gptel--log 'warn "Profile file missing for preset %s, profile: %s"
                          preset-name profile-name))
          loaded)))

     ;; Inline scope defaults (the plist itself is the scope config)
     (t
      (jf/gptel--log 'debug "Using inline scope defaults for preset: %s" preset-name)
      scope-config))))

(defun jf/gptel-scope-profile--expand-string (str project-root)
  "Expand ${project_root} in STR with PROJECT-ROOT.
If PROJECT-ROOT is nil and STR contains ${project_root}, return nil
to signal the pattern should be removed."
  (if (not (stringp str))
      str
    (if (string-match-p "\\${project_root}" str)
        (if project-root
            (replace-regexp-in-string "\\${project_root}" project-root str t t)
          nil)
      str)))

(defun jf/gptel-scope-profile--expand-list (lst project-root)
  "Expand variables in all strings in LST.
Remove entries where expansion returns nil (unresolvable variables).
Returns a new list."
  (if (not (listp lst))
      lst
    (cl-remove nil (mapcar (lambda (item)
                             (jf/gptel-scope-profile--expand-string item project-root))
                           lst))))

(defun jf/gptel-scope-profile--expand-variables (scope-plist project-root)
  "Expand ${project_root} in all string values within SCOPE-PLIST.
If PROJECT-ROOT is nil, patterns containing ${project_root} are removed
and a warning is logged.
Returns a new plist with expanded values."
  (when (and (null project-root)
             scope-plist)
    (jf/gptel--log 'warn "No project-root provided; patterns with ${project_root} will be removed"))
  (let ((result nil))
    (cl-loop for (key val) on scope-plist by #'cddr
             do (let ((expanded
                       (cond
                        ;; Nested plist (e.g., :paths has :read, :write, :deny)
                        ((and (listp val) (keywordp (car-safe val)))
                         (jf/gptel-scope-profile--expand-variables val project-root))
                        ;; List of strings (e.g., list of patterns)
                        ((and (listp val) (or (null val) (stringp (car-safe val))))
                         (jf/gptel-scope-profile--expand-list val project-root))
                        ;; Vector (yaml arrays sometimes parse as vectors)
                        ((vectorp val)
                         (jf/gptel-scope-profile--expand-list (append val nil) project-root))
                        ;; Single string
                        ((stringp val)
                         (or (jf/gptel-scope-profile--expand-string val project-root) ""))
                        ;; Anything else, pass through
                        (t val))))
                  (setq result (plist-put result key expanded))))
    result))

(defconst jf/gptel-scope-profile--cloud-auth-values
  '("allow" "warn" "deny")
  "Closed-set of accepted values for `:GPTEL_SCOPE_CLOUD_AUTH:'.
Mirrors the loader's enforcement at
`jf/gptel-scope--load-from-buffer'; mutating one without the other is
a vocabulary-mismatch finding.")

(defun jf/gptel-scope-profile--validate-cloud-auth (auth)
  "Signal an error when AUTH is not in the closed cloud-auth value set.
Returns AUTH unchanged on success.  Accepts nil (treated as `unset' —
the renderer / applicator decide whether to omit the key)."
  (cond
   ((null auth) auth)
   ((and (stringp auth)
         (member auth jf/gptel-scope-profile--cloud-auth-values))
    auth)
   (t
    (error "scope-profile: cloud :auth-detection must be one of %S, got %S"
           jf/gptel-scope-profile--cloud-auth-values auth))))

(declare-function gptel--model-name "gptel" (model))
(declare-function gptel-backend-name "gptel" (backend))
(declare-function gptel-tool-name "gptel" (tool))
(declare-function gptel--modify-value "gptel" (original new-spec))

(defun jf/gptel-scope-profile--resolve-tool-names (tools-spec)
  "Resolve TOOLS-SPEC to a list of tool name strings.

TOOLS-SPEC is the value of a preset's `:tools' field.  Three shapes
are supported:

- A modify-list spec (a list whose first element is one of
  `:append', `:prepend', `:remove', `:eval', `:function', or
  `:merge') — resolved against the current `gptel-tools' value via
  `gptel--modify-value'.  Returns nil when `gptel-tools' is unbound,
  since we cannot resolve a modifier without a base list.
- A bare list of tool structs / name symbols / name strings —
  mapped element-wise to their names.
- A single tool struct / symbol / string — wrapped as a singleton.

Per Decision 4 of gptel-drawer-as-source-of-truth: the renderer
emits the *resolved* tool name list, mirroring the buffer-local
state that preset application installs.  Modify-list specs are
resolved against the current global default; this matches what
`gptel--apply-preset' would install at session creation time."
  (let* ((base (and (boundp 'gptel-tools) (default-value 'gptel-tools)))
         (resolved
          (cond
           ;; Modify-list spec: dispatch to upstream resolver.
           ((and (consp tools-spec)
                 (keywordp (car tools-spec)))
            (when base
              (gptel--modify-value base tools-spec)))
           ;; List of tools (or names).
           ((listp tools-spec) tools-spec)
           ;; Single tool / symbol / string.
           (t (list tools-spec)))))
    (delq nil
          (mapcar (lambda (tool)
                    (cond
                     ((stringp tool) tool)
                     ((symbolp tool) (symbol-name tool))
                     ((and (recordp tool)
                           (fboundp 'gptel-tool-name)
                           (eq (type-of tool) 'gptel-tool))
                      (gptel-tool-name tool))
                     ;; cl-defstruct on older Emacs may produce vectors.
                     ((and (vectorp tool) (fboundp 'gptel-tool-name))
                      (ignore-errors (gptel-tool-name tool)))
                     (t nil)))
                  resolved))))

(defun jf/gptel-scope-profile--snapshot-format-model (value)
  "Format a `:model' snapshot VALUE as a drawer scalar string.
Returns nil when VALUE is nil so the key is omitted."
  (and value (gptel--model-name value)))

(defun jf/gptel-scope-profile--snapshot-format-backend (value)
  "Format a `:backend' snapshot VALUE as a drawer scalar string.
VALUE is either a backend struct or a bare backend-name string.
Returns nil when VALUE is nil so the key is omitted."
  (and value (if (stringp value) value (gptel-backend-name value))))

(defun jf/gptel-scope-profile--snapshot-format-number (value)
  "Format a numeric snapshot VALUE as a drawer scalar string.
Returns nil for non-numbers (including nil) so the key is omitted."
  (and (numberp value) (number-to-string value)))

(defconst jf/gptel-scope-profile--snapshot-spec
  (list
   (list "GPTEL_MODEL" :model :scalar
         #'jf/gptel-scope-profile--snapshot-format-model
         'gptel-model)
   (list "GPTEL_BACKEND" :backend :scalar
         #'jf/gptel-scope-profile--snapshot-format-backend
         'gptel-backend)
   (list "GPTEL_TOOLS" :tools :multivalued
         #'jf/gptel-scope-profile--resolve-tool-names
         'gptel-tools)
   (list "GPTEL_TEMPERATURE" :temperature :scalar
         #'jf/gptel-scope-profile--snapshot-format-number
         'gptel-temperature)
   (list "GPTEL_MAX_TOKENS" :max-tokens :scalar
         #'jf/gptel-scope-profile--snapshot-format-number
         'gptel-max-tokens)
   (list "GPTEL_NUM_MESSAGES_TO_SEND" :num-messages-to-send :scalar
         #'jf/gptel-scope-profile--snapshot-format-number
         'gptel--num-messages-to-send))
  "Registry of the chat-mode snapshot drawer keys.

This is the *single* enumeration of the snapshot key set.  Each row
is a list (DRAWER-KEY PLIST-KEY KIND EXTRACTOR SOURCE-VAR):

- DRAWER-KEY  — the bare org property name (no surrounding colons).
- PLIST-KEY   — the keyword the value is read under from a snapshot
                plist (matching the resolved-preset plist shape).
- KIND        — `:scalar' (rendered value is a string) or
                `:multivalued' (a list of strings).
- EXTRACTOR   — a one-argument function mapping the raw plist value
                to its formatted form (string for `:scalar', list of
                strings for `:multivalued'), or nil to omit the key.
- SOURCE-VAR  — the gptel variable whose buffer-local value the
                chat-mode save path reads (see
                `jf/gptel-scope-profile--buffer-snapshot-plist').

Every snapshot producer derives from this table:
`jf/gptel-scope-profile--snapshot-entries' (and through it
`--snapshot-lines' / the string renderer and `--apply-to-drawer'),
`jf/gptel-scope-profile--snapshot-keys', and the chat-mode save
path's `gptel-chat--write-config-drawer'.  Adding a seventh snapshot
key is therefore a single edit here — no producer needs changing.

`:GPTEL_SYSTEM:' is *deliberately absent*: long, multi-line, special-
character system prompts are unwieldy as a single org property value,
so the writer never round-trips them (Decision 2 of gptel-drawer-as-
source-of-truth, `register/invariant/drawer-system-key-write-
exclusion'; the read-side overlay still respects manually authored
entries).")

(defun jf/gptel-scope-profile--snapshot-keys ()
  "Return the canonical ordered list of snapshot drawer key strings.
Derived from `jf/gptel-scope-profile--snapshot-spec' — the single
registry of the snapshot key set."
  (mapcar #'car jf/gptel-scope-profile--snapshot-spec))

(defun jf/gptel-scope-profile--buffer-snapshot-plist ()
  "Build a snapshot plist from the current buffer's gptel configuration.

Reads the buffer-local value of each snapshot key's SOURCE-VAR (see
`jf/gptel-scope-profile--snapshot-spec') and returns a plist keyed by
the same keywords a resolved preset spec uses (`:model', `:backend',
`:tools', `:temperature', `:max-tokens', `:num-messages-to-send').

The result is a valid SNAPSHOT-PLIST argument for
`jf/gptel-scope-profile--snapshot-entries', so the chat-mode save
path (`gptel-chat--write-config-drawer') and the preset-spec render
path share one key enumeration and cannot drift.

Unbound source variables are omitted.  Type and emptiness filtering
is `--snapshot-entries'' responsibility, not this function's."
  (let (plist)
    (dolist (row jf/gptel-scope-profile--snapshot-spec)
      (pcase-let ((`(,_key ,plist-key ,_kind ,_extractor ,var) row))
        (when (boundp var)
          (setq plist (plist-put plist plist-key (symbol-value var))))))
    plist))

(defun jf/gptel-scope-profile--snapshot-entries (snapshot-plist)
  "Return ordered snapshot entries derived from SNAPSHOT-PLIST.

SNAPSHOT-PLIST is a plist keyed by `:model', `:backend', `:tools',
`:temperature', `:max-tokens', and `:num-messages-to-send'.  Two
sources produce it:

- a resolved preset plist from `gptel-get-preset' (the preset-spec
  pipeline — session creation and the string renderer); or
- `jf/gptel-scope-profile--buffer-snapshot-plist', built from the
  current buffer's gptel configuration (the chat-mode save path).

Either may be nil when no configuration is in effect.

Each returned entry is a list (DRAWER-KEY VALUE KIND) where
DRAWER-KEY is the bare org property name (no surrounding colons),
KIND is `:scalar' (VALUE is a string) or `:multivalued' (VALUE is a
list of strings), and the order is canonical: model, backend, tools,
temperature, max-tokens, num-messages-to-send.  Entries whose source
value is missing, wrong-typed, or (for tools) resolves to the empty
list are omitted.

The key set, ordering, per-key extractor, and `:GPTEL_SYSTEM:' write-
side exclusion all come from `jf/gptel-scope-profile--snapshot-spec'
\(the single registry).  Every snapshot producer routes through this
function or `jf/gptel-scope-profile--snapshot-keys', so the cross-
stage idempotency invariant of
`register/boundary/scope-profile-applicator' cannot drift between
producers.

Returns nil when SNAPSHOT-PLIST is nil or carries none of the
snapshot keys."
  (when snapshot-plist
    (delq nil
          (mapcar
           (lambda (row)
             (pcase-let ((`(,key ,plist-key ,kind ,extractor ,_var) row))
               (let ((value (funcall extractor
                                     (plist-get snapshot-plist plist-key))))
                 (pcase kind
                   (:scalar
                    (when (and (stringp value)
                               (not (string-empty-p value)))
                      (list key value kind)))
                   (:multivalued
                    (when (consp value)
                      (list key value kind)))))))
           jf/gptel-scope-profile--snapshot-spec))))

(defun jf/gptel-scope-profile--snapshot-lines (preset-spec)
  "Build the chat-mode snapshot drawer lines from PRESET-SPEC.

Thin formatter over `jf/gptel-scope-profile--snapshot-entries' (the
canonical enumeration of the snapshot key set; see its docstring and
`jf/gptel-scope-profile--snapshot-spec' for the included keys,
ordering, and the `:GPTEL_SYSTEM:' write-side exclusion).  Each entry
is rendered as a single `:KEY: VALUE' line: scalars verbatim,
multivalued lists space-joined.

Multivalued values are escaped element-wise with
`org-entry-protect-space' before joining — exactly what
`org-entry-put-multivalued-property' does in the buffer-mode
applicator `jf/gptel-scope-profile--apply-to-drawer'.  This keeps the
two producers in lockstep: a value containing whitespace (or a
newline) renders as one escaped token in both modes, so it round-
trips through `org-entry-get-multivalued-property' as a single
element and the cross-mode idempotency invariant of
`register/boundary/scope-profile-applicator' holds for *all* inputs,
not only the whitespace-free tool identifiers that are the common
case.

Returns nil when PRESET-SPEC is nil or carries none of the snapshot
keys."
  (mapcar (lambda (entry)
            (pcase-let ((`(,key ,value ,kind) entry))
              (format ":%s: %s"
                      key
                      (if (eq kind :multivalued)
                          (mapconcat #'org-entry-protect-space value " ")
                        value))))
          (jf/gptel-scope-profile--snapshot-entries preset-spec)))

(defun jf/gptel-scope-profile--render-drawer-text
    (preset-name parent-session-id scope-plist &optional preset-spec)
  "Render a `:PROPERTIES:' drawer block as a string.

PRESET-NAME (symbol or string) becomes the value of `:GPTEL_PRESET:'.
PARENT-SESSION-ID (string or nil) becomes `:GPTEL_PARENT_SESSION_ID:'
when non-nil and non-empty.  SCOPE-PLIST has the canonical shape
described in `register/shape/scope-config-plist' — i.e.
\(:paths (:read (...) :write (...) :modify (...) :execute (...) :deny (...))
 :cloud (:auth-detection STRING :allowed-providers (...))).

PRESET-SPEC, when non-nil, is the resolved preset plist (as returned
by `gptel-get-preset').  Its non-nil snapshot fields are emitted as
the upstream-compatible chat-mode keys (`:GPTEL_MODEL:',
`:GPTEL_BACKEND:', `:GPTEL_TOOLS:', `:GPTEL_TEMPERATURE:',
`:GPTEL_MAX_TOKENS:', `:GPTEL_NUM_MESSAGES_TO_SEND:') alongside the
existing scope keys.  `:GPTEL_SYSTEM:' is *never* emitted, even when
PRESET-SPEC has a non-nil `:system' (Decision 2 of gptel-drawer-as-
source-of-truth, register/invariant/drawer-system-key-write-
exclusion).  When PRESET-SPEC is nil, only the existing preset-name /
parent-session-id / scope keys are emitted (legacy shape).

Empty path lists are omitted from the rendered drawer.  Cloud auth-
detection equal to \"warn\" with no allowed providers is treated as the
default and omitted.

Returns a string of `register/shape/drawer-text-block':

  :PROPERTIES:
  :GPTEL_PRESET: <preset>
  [:GPTEL_PARENT_SESSION_ID: <parent>]
  [:GPTEL_MODEL: <model-name>]
  [:GPTEL_BACKEND: <backend-name>]
  [:GPTEL_TOOLS: <tool0> <tool1> ...]
  [:GPTEL_TEMPERATURE: <number>]
  [:GPTEL_MAX_TOKENS: <number>]
  [:GPTEL_NUM_MESSAGES_TO_SEND: <number>]
  [:GPTEL_SCOPE_<KEY>: value0 value1 ... valueN]
  :END:
\\n

Multi-value list entries render as a single space-separated `:KEY:
value0 value1 ...' line.  This matches the line shape that
`org-entry-put-multivalued-property' produces, which is critical for
the cross-mode idempotency invariant of
`register/boundary/scope-profile-applicator': writing the rendered
text verbatim to a fresh `session.org' and then re-applying the same
SCOPE-PLIST via `--apply-to-drawer' must leave the buffer unchanged.

Spaces inside individual values are escaped with `%20' (and newlines
with `%0A') via `org-entry-protect-space', matching the on-the-wire
format `org-entry-get-multivalued-property' expects."
  (let* ((paths (plist-get scope-plist :paths))
         (cloud (plist-get scope-plist :cloud))
         (lines (list ":PROPERTIES:"))
         (parent (and (stringp parent-session-id)
                      (not (string-empty-p parent-session-id))
                      parent-session-id)))
    (when preset-name
      (push (format ":GPTEL_PRESET: %s"
                    (if (symbolp preset-name)
                        (symbol-name preset-name)
                      preset-name))
            lines))
    (when parent
      (push (format ":GPTEL_PARENT_SESSION_ID: %s" parent) lines))
    ;; Chat-mode snapshot keys (Decision 4): emit before scope keys so
    ;; the upstream-compatible block reads naturally above the scope
    ;; block.
    (dolist (line (jf/gptel-scope-profile--snapshot-lines preset-spec))
      (push line lines))
    (dolist (op '((:read    . "GPTEL_SCOPE_READ")
                  (:write   . "GPTEL_SCOPE_WRITE")
                  (:modify  . "GPTEL_SCOPE_MODIFY")
                  (:execute . "GPTEL_SCOPE_EXECUTE")
                  (:deny    . "GPTEL_SCOPE_DENY")))
      (let ((vals (plist-get paths (car op)))
            (key (cdr op)))
        (when vals
          (push (format ":%s: %s"
                        key
                        (mapconcat #'org-entry-protect-space vals " "))
                lines))))
    (let ((auth (jf/gptel-scope-profile--validate-cloud-auth
                 (plist-get cloud :auth-detection)))
          (providers (plist-get cloud :allowed-providers)))
      (when (and (stringp auth) (not (string= auth "warn")))
        (push (format ":GPTEL_SCOPE_CLOUD_AUTH: %s" auth) lines))
      (when providers
        (push (format ":GPTEL_SCOPE_CLOUD_PROVIDERS: %s"
                      (mapconcat #'org-entry-protect-space providers " "))
              lines)))
    (push ":END:" lines)
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

(defun jf/gptel-scope-profile--apply-to-drawer
    (buffer scope-plist &optional preset-spec)
  "Apply SCOPE-PLIST to BUFFER's `:PROPERTIES:' drawer at `point-min'.

Writes each `:GPTEL_SCOPE_*' key via `org-entry-put' (scalars) or
`org-entry-put-multivalued-property' (list-valued keys).  Existing non-
scope drawer keys (e.g. `:GPTEL_PRESET:', `:GPTEL_PARENT_SESSION_ID:')
are preserved.  The buffer is saved when mutation completes.

SCOPE-PLIST has the canonical shape `register/shape/scope-config-plist'
\(see also `jf/gptel-scope-profile--render-drawer-text').

PRESET-SPEC, when non-nil, is the resolved preset plist (as from
`gptel-get-preset').  Its non-nil snapshot fields are written as the
upstream-compatible chat-mode keys (`:GPTEL_MODEL:', `:GPTEL_BACKEND:',
`:GPTEL_TOOLS:', `:GPTEL_TEMPERATURE:', `:GPTEL_MAX_TOKENS:',
`:GPTEL_NUM_MESSAGES_TO_SEND:').  `:GPTEL_SYSTEM:' is *never* written
here (Decision 2 of gptel-drawer-as-source-of-truth, register/
invariant/drawer-system-key-write-exclusion).  When PRESET-SPEC is
nil, only the scope keys are written (legacy behaviour).

Empty path lists clear nothing — to remove a key, the caller should
pass an explicit empty list and call `org-entry-delete' separately.
This function only writes the keys that are present and non-empty in
SCOPE-PLIST."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Snapshot keys first so they read above the scope block in the
        ;; drawer (matches --render-drawer-text's emission order).  Both
        ;; producers consume the same `--snapshot-entries' enumeration so
        ;; the chat-mode key set cannot drift between modes 2a and 2b
        ;; (cross-stage idempotency invariant of
        ;; `register/boundary/scope-profile-applicator').
        (dolist (entry (jf/gptel-scope-profile--snapshot-entries preset-spec))
          (pcase-let ((`(,key ,value ,kind) entry))
            (pcase kind
              (:scalar
               (org-entry-put (point) key value))
              (:multivalued
               (apply #'org-entry-put-multivalued-property
                      (point) key value)))))
        (let ((paths (plist-get scope-plist :paths))
              (cloud (plist-get scope-plist :cloud)))
          (dolist (op '((:read    . "GPTEL_SCOPE_READ")
                        (:write   . "GPTEL_SCOPE_WRITE")
                        (:modify  . "GPTEL_SCOPE_MODIFY")
                        (:execute . "GPTEL_SCOPE_EXECUTE")
                        (:deny    . "GPTEL_SCOPE_DENY")))
            (let ((vals (plist-get paths (car op))))
              (when vals
                ;; org-entry-put-multivalued-property uses &rest VALUES;
                ;; values must be spliced in via apply.
                (apply #'org-entry-put-multivalued-property
                       (point) (cdr op) vals))))
          (let ((auth (jf/gptel-scope-profile--validate-cloud-auth
                       (plist-get cloud :auth-detection)))
                (providers (plist-get cloud :allowed-providers)))
            (when (and (stringp auth) (not (string= auth "warn")))
              (org-entry-put (point) "GPTEL_SCOPE_CLOUD_AUTH" auth))
            (when providers
              (apply #'org-entry-put-multivalued-property
                     (point) "GPTEL_SCOPE_CLOUD_PROVIDERS" providers))))
        (when (buffer-file-name)
          (save-buffer))))))

(defun jf/gptel-scope-profile--write-scope-yml (_target-dir _scope-plist)
  "Stub for the removed scope.yml writer.

Signals an error.  Callers must be migrated to either
`jf/gptel-scope-profile--render-drawer-text' (string mode, embed in
fresh `session.org' initial content) or
`jf/gptel-scope-profile--apply-to-drawer' (buffer mode, mutate an
already-open chat buffer's `:PROPERTIES:' drawer).

Tracked in change `gptel-scope-in-org-properties'; rewires live in the
`rewire-session-creation' and `rewire-persistent-agent' tasks."
  (error "jf/gptel-scope-profile--write-scope-yml has been removed; use --render-drawer-text or --apply-to-drawer"))

(defun jf/gptel-scope-profile--merge-lists (list1 list2)
  "Merge LIST1 and LIST2, removing duplicates.
Preserves order: LIST1 items first, then new items from LIST2.
Returns nil if both lists are nil."
  (when (or list1 list2)
    (let ((result (copy-sequence list1)))
      (dolist (item list2)
        (unless (member item result)
          (setq result (append result (list item)))))
      result)))

(defun jf/gptel-scope-profile--deep-merge (base override)
  "Deep merge two plists, with OVERRIDE taking precedence.

Merging rules:
- Nested plists: recursively merge
- Lists: concatenate and deduplicate (base first, then override)
- Scalars: override wins
- nil: treated as absence (override's nil doesn't clear base's value)

Returns a new plist with merged values.

This is schema-agnostic - works for any plist structure including
future additions to scope configuration."
  (if (null override)
      base
    (let ((result (copy-sequence base)))
      ;; Iterate over all keys in override
      (cl-loop for (key val) on override by #'cddr
               do (let ((base-val (plist-get base key)))
                    (setq result
                          (plist-put result key
                                     (cond
                                      ;; Both are plists: recursively merge
                                      ((and (listp base-val) (listp val)
                                            (keywordp (car-safe base-val))
                                            (keywordp (car-safe val)))
                                       (jf/gptel-scope-profile--deep-merge base-val val))

                                      ;; Both are lists (not plists): merge and deduplicate
                                      ((and (listp base-val) (listp val)
                                            (not (keywordp (car-safe base-val)))
                                            (not (keywordp (car-safe val))))
                                       (jf/gptel-scope-profile--merge-lists base-val val))

                                      ;; Override is nil: keep base value (nil means absence)
                                      ((null val)
                                       base-val)

                                      ;; Otherwise: override wins
                                      (t val))))))
      result)))

(defun jf/gptel-scope-profile--create-for-session
    (preset-name &optional project-root worktree-paths parent-session-id preset-spec)
  "Resolve the scope profile for PRESET-NAME and return drawer text.

Returns a `register/shape/drawer-text-block' string ready to be
prepended to a fresh `session.org's initial content (Mode 2a).

PRESET-NAME selects the scope profile via
`jf/gptel-scope-profile--resolve'.  PROJECT-ROOT, when non-nil, is used
to expand ${project_root} variables.  WORKTREE-PATHS, when provided, is
deep-merged on top of the preset's resolved scope configuration.
PARENT-SESSION-ID, when a non-empty string, becomes the
`:GPTEL_PARENT_SESSION_ID:' line.

PRESET-SPEC, when non-nil, is the resolved preset plist (as from
`gptel-get-preset').  Its non-nil snapshot keys (`:model', `:backend',
`:tools', `:temperature', `:max-tokens', `:num-messages-to-send') are
threaded through to `--render-drawer-text' and emitted as the
upstream-compatible chat-mode keys in the rendered drawer.  When
PRESET-SPEC is omitted, the legacy preset-name-only drawer shape is
rendered (no chat-mode snapshot keys, only `:GPTEL_PRESET:' and the
scope keys).  Callers that want the full snapshot must pass
PRESET-SPEC explicitly."
  (let* ((resolved (jf/gptel-scope-profile--resolve preset-name))
         (expanded (when resolved
                     (jf/gptel-scope-profile--expand-variables resolved project-root)))
         (scope-plist
          (cond
           ;; Deep merge worktree paths with preset's scope configuration
           ((and worktree-paths expanded)
            (jf/gptel--log 'info "Deep-merging worktree paths with preset scope configuration")
            (jf/gptel-scope-profile--deep-merge expanded worktree-paths))

           ;; Only worktree paths (no preset scope config)
           (worktree-paths
            (jf/gptel--log 'info "Using explicit worktree paths (preset has no scope config)")
            worktree-paths)

           ;; Only preset scope config (no worktree paths)
           (expanded
            (jf/gptel--log 'info "Using preset scope configuration")
            expanded)

           ;; Neither available
           (t
            (jf/gptel--log 'warn "No scope configuration available for preset: %s" preset-name)
            nil))))
    (jf/gptel-scope-profile--render-drawer-text
     preset-name parent-session-id scope-plist preset-spec)))

(provide 'gptel-scope-profiles)
;;; scope-profiles.el ends here
