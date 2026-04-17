# Scope Expansion

## Purpose

Defines the interactive UI that handles scope violations in gptel tool calls. The expansion UI has two entry points — **inline** (triggered by `scope-validation` when a scoped tool's validation fails) and **pre-emptive** (invoked by the LLM through the `request_scope_expansion` tool before attempting a call it expects will be denied) — that both land in the same transient menu. Every interaction resolves to exactly one of three outcomes for the pending tool call: **deny** → the tool errors back to the LLM; **allow-once** → the tool runs this one time with no persisted state; **add-to-scope** → `scope.yml` is updated and the tool runs.

## Inline vs Pre-emptive Expansion

- **Inline.** `scope-validation` calls `jf/gptel-scope--trigger-inline-expansion` when `jf/gptel-scope-authorize-tool-call` receives a denial. The trigger builds a violation-info plist via `jf/gptel-scope--build-violation-info` and invokes `jf/gptel-scope-prompt-expansion`. The user's choice resolves through the wrapper's on-allow/on-deny thunks — there is no retry.
- **Pre-emptive.** The `request_scope_expansion` gptel tool (defined in `scope-shell-tools.el`) lets the LLM construct a violation-info plist directly and call `jf/gptel-scope-prompt-expansion`, so it can ask for permission before invoking a tool it knows would be denied.

Both entry points share the transient menu, the action handlers, the `scope.yml` writers, and the expansion queue.

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
- **AND** Add-to-scope variants re-enter `jf/gptel-scope-authorize-tool-call` so the updated `scope.yml` is re-validated (which may trigger another prompt if a different operation in the same command is still denied)

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
- **THEN** `scope.yml` is updated and the LLM's response contains `:success t :patterns_added [...]`
- **AND** the LLM may then invoke the originally intended tool, which will pass validation against the updated `scope.yml`

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
- **AND** the Add-to-scope handlers use it to pick the target `paths.*` section in `scope.yml`

### Requirement: Deny action

Selecting Deny SHALL reject the pending invocation and cause the tool to return an error to the LLM without modifying `scope.yml`.

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

"Add to Scope" SHALL write the exact denied resource to `scope.yml` under the section that matches the denied operation, then authorize the invocation.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add to Scope

#### Scenario: Resource is routed by validation-type
- **WHEN** the user selects `a`
- **THEN** `jf/gptel-scope--add-to-scope` reads `:validation-type` and routes through `jf/gptel-scope--write-pattern-to-scope` to either `jf/gptel-scope--add-path-to-scope` (for `path`) or `jf/gptel-scope--add-bash-to-scope` (for `bash`)
- **AND** the denied resource is written verbatim (no wildcard added) except that directory paths with a trailing `/` are normalized to `.../**`

#### Scenario: Callback reports patterns added
- **WHEN** the write succeeds
- **THEN** the wrapper callback receives `{:success t :patterns_added [...] :message "Scope expanded. Added N pattern(s) to <tool>"}`
- **AND** the inline-flow trigger translates that into `(:approved t)` so the wrapper re-enters `jf/gptel-scope-authorize-tool-call`

### Requirement: Add wildcard action (parent directory /**)

"Add Wildcard" SHALL write `<parent-directory>/**` — the parent of the denied resource with a recursive glob — to `scope.yml` and authorize the invocation. This action is hidden when the resource is itself a directory.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Wildcard to Scope; §Parent Wildcard For

#### Scenario: Parent directory is computed and written
- **WHEN** the user selects `w` for resource `~/foo/bar.txt`
- **THEN** `jf/gptel-scope--parent-wildcard-for` returns `~/foo/**`
- **AND** `jf/gptel-scope--add-wildcard-to-scope` writes that pattern into the section matching `:operation`
- **AND** the callback reports `:patterns_added [parent-wildcard]`

#### Scenario: Wildcard option is suppressed for directory resources
- **WHEN** the denied resource is a directory
- **THEN** the `w` suffix's `:if` predicate returns nil and the option is not shown

### Requirement: Add custom pattern action (user-editable)

"Add Custom Pattern" SHALL prompt the user with the denied resource as the initial text, accept any edited pattern, and write it to `scope.yml`.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Custom Pattern to Scope

#### Scenario: User edits the pattern and it is written
- **WHEN** the user selects `c` and submits an edited pattern
- **THEN** `jf/gptel-scope--add-custom-to-scope` writes that pattern via `jf/gptel-scope--write-pattern-to-scope`
- **AND** the callback reports `:success t :patterns_added [custom-pattern]`

#### Scenario: Cancel (C-g) is treated as a silent deny
- **WHEN** the user presses `C-g` at the prompt
- **THEN** the callback receives `{:success nil :user_denied t}` with no error to the user
- **AND** no write to `scope.yml` occurs

### Requirement: Edit scope manually action

"Edit Manually" SHALL open `scope.yml` in a buffer and quit the transient, leaving the pending tool call unresolved until the user drives it through another path.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Edit Scope Manually

#### Scenario: scope.yml is opened for editing
- **WHEN** the user selects `e`
- **THEN** `jf/gptel-scope--edit-scope` finds the context's `scope.yml`, opens it with `find-file`, and calls `transient-quit-one`
- **AND** if no `scope.yml` can be resolved, the action signals `user-error`

### Requirement: Section-targeted writes

Add-to-scope variants SHALL use the denied operation to target the correct `paths.*` subsection of `scope.yml` (not the tool category, not the command name).

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Add Path to Scope; `config/gptel/scope/scope-validation.org` — §Operation → Scope Section Mapping

#### Scenario: Operation keyword maps to section
- **WHEN** the violation carries `:operation :read`, `:write`, `:modify`, or `:execute`
- **THEN** `jf/gptel-scope--map-operation-to-scope-section` returns `:read`, `:write`, or `:execute` and the pattern is written under `paths.<section>`
- **AND** read-like granular operations (`:read-directory`, `:read-metadata`, `:match-pattern`) collapse to `:read`; write-like granular operations (`:create`, `:create-or-modify`, `:append`, `:delete`, `:modify`) collapse to `:write`

#### Scenario: Bash file-op denials route to path sections (no command-name expansion)
- **WHEN** a bash validation denies a file operation on an absolute path, tilde path, or glob pattern
- **THEN** `jf/gptel-scope--add-bash-to-scope` delegates to `jf/gptel-scope--add-path-to-scope` with the denied operation
- **AND** when the resource is a bare command name with no path characters, the handler emits a user message and does not write — there is no command-name expansion path in the operation-first model

#### Scenario: Missing operation falls back safely
- **WHEN** no `:operation` is present on the violation
- **THEN** `jf/gptel-scope--add-path-to-scope` defaults the target section to `:read` (the safest choice for filesystem tools whose category the caller did not pass through)

### Requirement: scope.yml writer preserves structure

The YAML writer SHALL round-trip the parsed `scope.yml` plist, converting kebab-case Elisp keys back to snake_case for YAML output, without dropping sections or keys that the expansion action did not touch.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §YAML Writer Helpers, §YAML Writer Main Function

#### Scenario: Existing paths section is merged, not replaced
- **WHEN** the writer adds a pattern to `paths.read`
- **THEN** existing entries under `paths.read` are preserved and the new pattern is appended (deduplicated — already-present patterns are skipped)
- **AND** other top-level sections (`paths.write`, `paths.deny`, `cloud`, `security`, `tools`) are emitted unchanged

#### Scenario: Keys are converted from kebab-case to snake_case on output
- **WHEN** the writer emits a key like `:auth-detection` or `:enforce-parse-complete`
- **THEN** `jf/gptel-scope-yaml--kebab-to-snake` converts it to `auth_detection` / `enforce_parse_complete`
- **AND** simple values, nested lists, and tool maps are serialized via the structure-specific helpers without losing boolean, numeric, or list fidelity

### Requirement: Expansion queue serialization

When multiple scoped tools fire in parallel and more than one is denied, the expansion UI SHALL serialize prompts so no callback is lost.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Expansion Queue, §Entry Point

#### Scenario: First call shows, later calls queue
- **WHEN** `jf/gptel-scope-prompt-expansion` is called while `jf/gptel-scope--expansion-active` is nil
- **THEN** it sets the active flag and opens the transient immediately
- **AND** subsequent calls while active is set append their `{:violation :callback :patterns :tool-name}` plist to `jf/gptel-scope--expansion-queue` instead of opening a second transient

#### Scenario: Each resolution pops the next queued prompt
- **WHEN** the user resolves the active prompt
- **THEN** the suffix action funcalls its callback and then calls `jf/gptel-scope--process-expansion-queue`
- **AND** if the queue has entries, the head is popped and a new transient is opened with that scope
- **AND** if the queue is empty, the active flag is cleared

#### Scenario: Queue is buffer-local
- **WHEN** multiple gptel buffers are running concurrent sessions
- **THEN** each buffer has its own `jf/gptel-scope--expansion-queue` and `jf/gptel-scope--expansion-active` (both `defvar-local`) so sessions do not interfere

### Requirement: Context directory resolution

The UI SHALL resolve `scope.yml` from the current gptel session's context directory and fail cleanly when that cannot be determined or when the file is missing or unwritable.

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Get Scope File Path, §Validate Scope File Writable

#### Scenario: branch-dir takes precedence over buffer directory
- **WHEN** `jf/gptel-scope--get-scope-file-path` runs
- **THEN** it uses `jf/gptel--branch-dir` when bound and non-nil; otherwise the current buffer's file directory
- **AND** it joins the result with `jf/gptel-session--scope-file` and returns an absolute path (or nil if no context is resolvable)

#### Scenario: Missing or unwritable scope.yml raises a user error
- **WHEN** `jf/gptel-scope--validate-scope-file-writable` sees no file, a missing path, or a non-writable file
- **THEN** it signals `user-error` with a message the user can act on — it does not silently fail the write

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

- **`scope-validation`** — on denial, calls `jf/gptel-scope--trigger-inline-expansion`, which builds violation-info via `jf/gptel-scope--build-violation-info` and calls `jf/gptel-scope-prompt-expansion`. On approval-by-add-to-scope, re-enters `jf/gptel-scope-authorize-tool-call` against the updated `scope.yml`.
- **`scope-yaml`** — the expansion writer delegates parse, key normalization, and kebab/snake conversion to `jf/gptel-scope-yaml--parse-file`, `jf/gptel-scope-yaml--normalize-keys`, and `jf/gptel-scope-yaml--kebab-to-snake`.
- **`scope-shell-tools`** — defines the `request_scope_expansion` gptel tool, the pre-emptive entry point into `jf/gptel-scope-prompt-expansion`.
- **gptel async callback** — every action funcalls the wrapper's callback with a JSON string; the wrapper delivers that JSON as the tool's response.
- **`scope.yml`** on disk — the expansion writer is the only component that mutates `scope.yml`; all other modules treat it as read-only configuration.
