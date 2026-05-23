# Scope Expansion

## Purpose

Defines the interactive UI that handles scope violations in gptel tool calls. The expansion UI has two entry points — **inline** (triggered by `scope-validation` when a scoped tool's validation fails) and **pre-emptive** (invoked by the LLM through the `request_scope_expansion` tool before attempting a call it expects will be denied) — that both land in the same transient menu. Every interaction resolves to exactly one of three outcomes for the pending tool call: **deny** → the tool errors back to the LLM; **allow-once** → the tool runs this one time with no persisted state; **add-to-scope** → the session's `:PROPERTIES:` drawer is updated and the tool runs.

## Inline vs Pre-emptive Expansion

- **Inline.** `scope-validation` calls `jf/gptel-scope--trigger-inline-expansion` when `jf/gptel-scope-authorize-tool-call` receives a denial. The trigger builds a violation-info plist via `jf/gptel-scope--build-violation-info` and invokes `jf/gptel-scope-prompt-expansion`. The user's choice resolves through the wrapper's on-allow/on-deny thunks — there is no retry.
- **Pre-emptive.** The `request_scope_expansion` gptel tool (defined in `scope-shell-tools.el`) lets the LLM construct a violation-info plist directly and call `jf/gptel-scope-prompt-expansion`, so it can ask for permission before invoking a tool it knows would be denied.

Both entry points share the transient menu, the action handlers, the drawer writers, and the expansion queue.

## Requirements

### Requirement: Inline expansion trigger on validation failure

The scope validation layer SHALL route every denial through the expansion UI instead of returning errors directly to the LLM, so the user can approve, deny, or extend scope before the tool completes.

**Implementation**: `config/gptel/scope/scope-validation.org` — §Authorize Tool Call, §Trigger Inline Expansion

#### Scenario: Denied tool call invokes the expansion UI
- **WHEN** `jf/gptel-scope-authorize-tool-call` receives a non-allowed validation result
- **THEN** it calls `jf/gptel-scope--trigger-inline-expansion` with the validation error, tool name, and a wrapper callback
- **AND** the trigger builds a violation-info plist via `jf/gptel-scope--build-violation-info` and invokes `jf/gptel-scope-prompt-expansion`
- **AND** neither the on-allow nor the on-deny thunk runs until the user chooses

#### Scenario: Approval resolves the pending invocation
- **WHEN** the user chooses Add to Scope, Add Wildcard, Add Custom Pattern, or Allow Once
- **THEN** the trigger callback receives `(:approved t)` (or `(:approved t :allowed-once t)` for Allow Once)
- **AND** Allow Once invokes the on-allow thunk without re-validation
- **AND** Add-to-scope variants re-enter `jf/gptel-scope-authorize-tool-call` so the updated drawer is re-validated (which may trigger another prompt if a different operation in the same command is still denied)

#### Scenario: Denial resolves to a final error
- **WHEN** the user chooses Deny (or the expansion callback errors)
- **THEN** the trigger callback receives `(:approved nil :reason …)`
- **AND** the wrapper invokes its on-deny thunk with the original validation result formatted for the LLM
- **AND** the tool body never runs

### Requirement: request_scope_expansion tool

The system SHALL expose a regular gptel tool that lets the LLM pre-emptively request user approval before invoking a scoped tool it expects will be denied.

**Implementation**: `config/gptel/scope/scope-shell-tools.org` — §request_scope_expansion Tool

#### Scenario: Tool is registered under the scope category
- **WHEN** `scope-shell-tools` loads
- **THEN** it defines `request_scope_expansion` via `gptel-make-tool` with `:async t` and `:category "scope"`
- **AND** the tool is a regular gptel tool, not routed through any meta validation strategy

#### Scenario: LLM invokes request_scope_expansion
- **WHEN** the LLM calls `request_scope_expansion` with `tool_name`, `patterns`, and `justification`
- **THEN** the tool function constructs a violation-info plist (`:tool`, `:resource`, `:reason`, `:validation-type path`) and calls `jf/gptel-scope-prompt-expansion` directly
- **AND** the user sees the same six-option transient menu as the inline flow

#### Scenario: Approved pre-emptive request
- **WHEN** the user chooses Add to Scope (or a wildcard/custom variant)
- **THEN** the session's drawer is updated and the LLM's response contains `:success t :patterns_added [...]`
- **AND** the LLM may then invoke the originally intended tool, which will pass validation against the updated drawer

### Requirement: Transient menu six-choice UI

The expansion UI SHALL display one transient menu, `jf/gptel-scope-expansion-menu`, offering six choices in a fixed order: Deny, Add to Scope, Add Wildcard, Add Custom Pattern, Allow Once, Edit Manually.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Transient Menu

#### Scenario: Menu header displays full violation context
- **WHEN** the menu renders
- **THEN** the header shows `Scope Violation: Access Denied`, the tool name, the resource, the git-tracked status (when metadata is present), the file-exists status (when metadata is present), and the validator's reason

#### Scenario: Menu offers six options in the documented order
- **WHEN** the menu renders
- **THEN** the suffix commands appear as: `d` Deny, `a` Add to Scope, `w` Add `<parent>/**` (hidden when the resource is a directory), `c` Add Custom Pattern, `o` Allow Once, `e` Edit Scope Manually, plus `q` Cancel
- **AND** the wildcard option is suppressed for directory resources because the parent-wildcard is not meaningful there

#### Scenario: Each suffix resolves the transient and processes the queue
- **WHEN** the user selects any action
- **THEN** the suffix calls `transient-quit-one` and `jf/gptel-scope--process-expansion-queue`
- **AND** if another expansion was queued while the current one was showing, the queue head is popped and a new transient is opened

### Requirement: Violation-info shape

Every violation-info plist produced by validation or by `request_scope_expansion` SHALL carry `:tool`, `:resource`, `:reason`, and `:validation-type`, all four non-nil; `:validation-type` SHALL be `path` or `bash`.

**Implementation**: `config/gptel/scope/interfaces.org` — §Violation-Info Shape; `config/gptel/scope/scope-validation.org` — §Violation-Info Building

#### Scenario: build-violation-info pulls resource from the correct field per error code
- **WHEN** `jf/gptel-scope--build-violation-info` receives a validation error
- **THEN** it maps `denied-pattern` and `not-in-scope` to `:resource`, `parse_incomplete` to `:command`, and `cloud_auth_denied` and `cloud_provider_denied` to `:provider`, and places the extracted value under `:resource` in the violation-info
- **AND** it copies `:validation-type` straight through from the validation error (already attached by the validation entrypoint)

#### Scenario: Operation keyword passes through to the UI
- **WHEN** a path denial carries `:operation` (e.g. `:read`, `:write`, `:modify`, `:execute`)
- **THEN** `build-violation-info` includes it in the violation-info under `:operation`
- **AND** the Add-to-scope handlers use it to pick the target `:GPTEL_SCOPE_*` key in the session's `:PROPERTIES:` drawer

### Requirement: Deny action

Selecting Deny SHALL reject the pending invocation and cause the tool to return an error to the LLM without modifying the session's drawer.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Deny Expansion

#### Scenario: Deny funcalls the callback with user_denied
- **WHEN** the user selects `d`
- **THEN** `jf/gptel-scope--deny-expansion` funcalls the wrapper callback with JSON `{:success nil :user_denied t :message "User denied scope expansion request."}`
- **AND** the inline-flow trigger translates that into `(:approved nil …)` so the wrapper runs its on-deny thunk

### Requirement: Allow-once action (stateless)

"Allow once" SHALL authorize the single pending invocation with no persistence, no bookkeeping list, and no post-response hook. A subsequent denial of the same resource SHALL prompt again.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Allow Once Action; `config/gptel/scope/interfaces.org` — §Allow-Once Semantics

#### Scenario: Allow-once funcalls the callback and returns
- **WHEN** the user selects `o`
- **THEN** `jf/gptel-scope--allow-once-action` funcalls the wrapper callback with JSON `{:success t :allowed_once t :message "Permission granted for this invocation only."}`
- **AND** that is the entire mechanism — no list is updated, no hook is registered, no state survives the call

#### Scenario: Re-prompting on the next denial is by design
- **WHEN** the same resource is denied again in a later tool call
- **THEN** the expansion UI is shown again
- **AND** the user must choose again — there is no consumed-on-use permission

### Requirement: Add to scope action (exact pattern)

"Add to Scope" SHALL write the exact denied resource into the session's `:PROPERTIES:` drawer under the `:GPTEL_SCOPE_*` key matching the denied operation, save the buffer, then authorize the invocation.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add to Scope

#### Scenario: Resource is routed by validation-type to the right drawer key
- **WHEN** the user selects `a`
- **THEN** `jf/gptel-scope--add-to-scope` reads `:validation-type` and routes through `jf/gptel-scope--write-pattern-to-drawer` to either the path-drawer-writer (for `path`) or the bash-drawer-writer (for `bash`)
- **AND** the operation keyword (`:read`, `:write`, `:modify`, `:execute`, or `:deny`) selects the target drawer key (`:GPTEL_SCOPE_READ:`, etc.)
- **AND** the denied resource is written verbatim (no wildcard added) except that directory paths with a trailing `/` are normalized to `.../**`

#### Scenario: Drawer mutation is buffer-side and saved
- **WHEN** the writer runs against a live chat buffer
- **THEN** it uses `org-entry-put` (for the first value) or the multi-value helper (for subsequent values) to update the drawer at `point-min`
- **AND** calls `save-buffer` so the on-disk `session.org` reflects the change
- **AND** the buffer's undo history records the mutation (so `undo` can revert the add-to-scope)

#### Scenario: Callback reports patterns added
- **WHEN** the write succeeds
- **THEN** the wrapper callback receives `{:success t :patterns_added [...] :message "Scope expanded. Added N pattern(s) to <tool>"}`
- **AND** the inline-flow trigger translates that into `(:approved t)` so the wrapper re-enters `jf/gptel-scope-authorize-tool-call`

### Requirement: Add wildcard action (parent directory /**)

"Add Wildcard" SHALL write `<parent-directory>/**` into the drawer's operation-matched key and authorize the invocation. This action is hidden when the resource is itself a directory.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Wildcard to Scope; §Parent Wildcard For

#### Scenario: Parent directory is computed and written to the drawer
- **WHEN** the user selects `w` for resource `~/foo/bar.txt` with `:operation :read`
- **THEN** `jf/gptel-scope--parent-wildcard-for` returns `~/foo/**`
- **AND** the writer appends that pattern to `:GPTEL_SCOPE_READ:` (or starts a new `:GPTEL_SCOPE_READ:` line if absent)
- **AND** the buffer is saved
- **AND** the callback reports `:patterns_added [parent-wildcard]`

#### Scenario: Wildcard option is suppressed for directory resources
- **WHEN** the denied resource is a directory
- **THEN** the `w` suffix's `:if` predicate returns nil and the option is not shown

### Requirement: Add custom pattern action (user-editable)

"Add Custom Pattern" SHALL prompt the user with the denied resource as the initial text, accept any edited pattern, and write it to the operation-matched drawer key.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Custom Pattern to Scope

#### Scenario: User edits the pattern and it is written to the drawer
- **WHEN** the user selects `c` and submits an edited pattern
- **THEN** `jf/gptel-scope--add-custom-to-scope` writes that pattern via `jf/gptel-scope--write-pattern-to-drawer`
- **AND** the buffer is saved
- **AND** the callback reports `:success t :patterns_added [custom-pattern]`

#### Scenario: Cancel (C-g) is treated as a silent deny
- **WHEN** the user presses `C-g` at the prompt
- **THEN** the callback receives `{:success nil :user_denied t}` with no error to the user
- **AND** no drawer mutation occurs

### Requirement: Edit scope manually action

"Edit Manually" SHALL bring the chat buffer's `session.org` into focus (and unfold the `:PROPERTIES:` drawer at `point-min`) and quit the transient, leaving the pending tool call unresolved until the user drives it through another path.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Edit Scope Manually

#### Scenario: session.org is brought into focus
- **WHEN** the user selects `e`
- **THEN** `jf/gptel-scope--edit-scope` switches to the chat buffer (if a different buffer is current), points at `point-min`, unfolds the `:PROPERTIES:` drawer, and calls `transient-quit-one`
- **AND** if no chat buffer can be resolved, the action signals `user-error`

### Requirement: Section-targeted writes

Add-to-scope variants SHALL use the denied operation to target the correct `:GPTEL_SCOPE_*` drawer key (not the tool category, not the command name).

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Path to Scope; `config/gptel/scope/scope-validation.org` — §Operation → Drawer Key Mapping

#### Scenario: Operation keyword maps to drawer key
- **WHEN** the violation carries `:operation :read`, `:write`, `:modify`, or `:execute`
- **THEN** `jf/gptel-scope--map-operation-to-drawer-key` returns `:GPTEL_SCOPE_READ`, `:GPTEL_SCOPE_WRITE`, `:GPTEL_SCOPE_MODIFY`, or `:GPTEL_SCOPE_EXECUTE` and the pattern is written under that key
- **AND** read-like granular operations (`:read-directory`, `:read-metadata`, `:match-pattern`) collapse to `:GPTEL_SCOPE_READ`; write-like granular operations (`:create`, `:create-or-modify`, `:append`, `:delete`, `:modify`) collapse to `:GPTEL_SCOPE_WRITE`

#### Scenario: Bash file-op denials route to drawer keys (no command-name expansion)
- **WHEN** a bash validation denies a file operation on an absolute path, tilde path, or glob pattern
- **THEN** `jf/gptel-scope--add-bash-to-scope` delegates to `jf/gptel-scope--add-path-to-scope` with the denied operation
- **AND** when the resource is a bare command name with no path characters, the handler emits a user message and does not write — there is no command-name expansion path in the operation-first model

#### Scenario: Missing operation falls back safely
- **WHEN** no `:operation` is present on the violation
- **THEN** `jf/gptel-scope--add-path-to-scope` defaults the target drawer key to `:GPTEL_SCOPE_READ:` (the safest choice for filesystem tools whose category the caller did not pass through)

### Requirement: Drawer writer preserves structure

The drawer writer SHALL update only the `:GPTEL_SCOPE_*` key being modified, leaving every other property in the drawer (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, other scope keys) unchanged. Mutations are append-only for list keys and idempotent (already-present patterns are not duplicated).

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Drawer Writer Helpers

#### Scenario: Existing drawer keys are preserved
- **WHEN** the writer adds a pattern to `:GPTEL_SCOPE_READ:`
- **THEN** the existing `:GPTEL_SCOPE_READ:` line is rewritten with the new pattern appended (single-line space-separated emission, e.g. `:GPTEL_SCOPE_READ: existing /new`)
- **AND** `:GPTEL_PRESET:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, etc. are emitted unchanged

#### Scenario: Duplicate patterns are skipped
- **WHEN** the writer is asked to add a pattern that already appears under the target drawer key
- **THEN** the drawer is not modified
- **AND** the callback still reports `:success t` (no-op success), with `:patterns_added` reflecting the deduplicated additions (possibly empty)

#### Scenario: Drawer values round-trip through `org-entry-get-multivalued-property`
- **WHEN** the writer emits `:GPTEL_SCOPE_<KEY>: v1 v2 v3` as a single line and the reader consumes the same drawer
- **THEN** `(org-entry-get-multivalued-property POINT "GPTEL_SCOPE_<KEY>")` returns `("v1" "v2" "v3")` regardless of whether the values were written across multiple add-to-scope operations or as one initial seeding
- **AND** the writer never emits `:GPTEL_SCOPE_<KEY>+:` continuation lines (org's multi-value reader accepts both forms but the writer canonicalises on the single-line form)

### Requirement: Expansion queue serialization

When multiple scoped tools fire in parallel and more than one is denied, the expansion UI SHALL serialize prompts so no callback is lost. Serialization is **frame-global**: the queue and active flag live as `defvar` (not `defvar-local`) so a flip-set in one buffer is observed and cleared from any buffer. Per-entry `:chat-buffer` continues to route drawer writes (via `jf/gptel-scope--current-chat-buffer` and the writer pipeline) to the session that owns each pending prompt.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Expansion Queue, §Entry Point

#### Scenario: First call shows, later calls queue
- **WHEN** `jf/gptel-scope-prompt-expansion` is called while `jf/gptel-scope--expansion-active` is nil
- **THEN** it sets the active flag and opens the transient immediately
- **AND** subsequent calls while active is set append their `{:violation :callback :patterns :tool-name :chat-buffer}` plist to `jf/gptel-scope--expansion-queue` instead of opening a second transient

#### Scenario: Each resolution pops the next queued prompt
- **WHEN** the user resolves the active prompt
- **THEN** the suffix action funcalls its callback and then calls `jf/gptel-scope--process-expansion-queue`
- **AND** if the queue has entries, the head is popped and a new transient is opened with that scope
- **AND** if the queue is empty, the active flag is cleared

#### Scenario: Queue and active flag are frame-global
- **WHEN** `jf/gptel-scope-prompt-expansion` is called from buffer A and the action handler later runs in buffer B (e.g. a PersistentAgent fires the prompt from its invisible `session.org` buffer while the user answers from the parent's overlay buffer)
- **THEN** the buffer-A flip-set of `jf/gptel-scope--expansion-active` is observed by `--process-expansion-queue` running in buffer B because both variables are declared with `defvar` (not `defvar-local`)
- **AND** a queue entry pushed from buffer A is drainable from buffer B without buffer-switching
- **AND** drawer writes for that entry still target buffer A's session because the entry's `:chat-buffer` plist key is consulted by `jf/gptel-scope--current-chat-buffer`, not `(current-buffer)`

#### Scenario: Frame-modality enforces single in-flight prompt
- **WHEN** any expansion prompt is currently shown via `transient-setup`
- **THEN** any further call to `jf/gptel-scope-prompt-expansion` (from any buffer, any session, any agent) appends to the global queue rather than opening a second transient
- **AND** the user always sees exactly one prompt at a time, in the order the underlying tool calls were authorized

### Requirement: Callback response shapes

The JSON payloads handed to the wrapper's async callback SHALL follow three canonical shapes so downstream consumers (`scope-validation`'s inline trigger, the LLM for the pre-emptive path) can discriminate without inspecting message text.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Deny Expansion, §Allow Once Action, §Add to Scope (and variants)

#### Scenario: Deny payload
- **WHEN** the user selects Deny (or C-g's out of the custom prompt)
- **THEN** the callback receives `{:success nil :user_denied t …}`

#### Scenario: Allow-once payload
- **WHEN** the user selects Allow Once
- **THEN** the callback receives `{:success t :allowed_once t …}`

#### Scenario: Add-to-scope payloads
- **WHEN** the user selects Add to Scope, Add Wildcard, or Add Custom Pattern
- **THEN** the callback receives `{:success t :patterns_added [...] …}` with `:patterns_added` as a JSON array (vector in Elisp)
- **AND** `:allowed_once` is absent from add-to-scope payloads

## Integration Points

- **`scope-validation`** — on denial, calls `jf/gptel-scope--trigger-inline-expansion`, which builds violation-info via `jf/gptel-scope--build-violation-info` and calls `jf/gptel-scope-prompt-expansion`. On approval-by-add-to-scope, re-enters `jf/gptel-scope-authorize-tool-call` against the updated drawer.
- **`scope-shell-tools`** — defines the `request_scope_expansion` gptel tool, the pre-emptive entry point into `jf/gptel-scope-prompt-expansion`.
- **gptel async callback** — every action funcalls the wrapper's callback with a JSON string; the wrapper delivers that JSON as the tool's response.
- **`session.org` `:PROPERTIES:` drawer** — the expansion writer is the only component that mutates the drawer's `:GPTEL_SCOPE_*` keys; all other modules treat them as read-only configuration. Drawer mutations go through `org-entry-put` and multi-value helpers — there is no parse / re-emit cycle.
