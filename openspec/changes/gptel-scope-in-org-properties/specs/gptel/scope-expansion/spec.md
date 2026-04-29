# Scope Expansion (Delta Spec)

This delta switches the expansion UI's writer from a YAML-file mutator to a property-drawer mutator. The transient menu, queue, callbacks, and inline/pre-emptive entry points are unchanged. Only the persistence layer underneath the action handlers changes.

## MODIFIED Requirements

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

## ADDED Requirements

### Requirement: Drawer writer preserves structure

The drawer writer SHALL update only the `:GPTEL_SCOPE_*` key being modified, leaving every other property in the drawer (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, other scope keys) unchanged. Mutations are append-only for list keys and idempotent (already-present patterns are not duplicated).

**Implementation**: `config/gptel/scope/scope-expansion.org` — §Drawer Writer Helpers

#### Scenario: Existing drawer keys are preserved
- **WHEN** the writer adds a pattern to `:GPTEL_SCOPE_READ:`
- **THEN** any existing `:GPTEL_SCOPE_READ:` / `:GPTEL_SCOPE_READ+:` lines are preserved and the new pattern is appended as a `:GPTEL_SCOPE_READ+:` line
- **AND** `:GPTEL_PRESET:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, etc. are emitted unchanged

#### Scenario: Duplicate patterns are skipped
- **WHEN** the writer is asked to add a pattern that already appears under the target drawer key
- **THEN** the drawer is not modified
- **AND** the callback still reports `:success t` (no-op success), with `:patterns_added` reflecting the deduplicated additions (possibly empty)

#### Scenario: First addition for a key uses the bare form, not `+`
- **WHEN** the writer is asked to add a pattern to a drawer key that has no existing entry
- **THEN** it emits `:GPTEL_SCOPE_<KEY>: <pattern>` (no `+` suffix)
- **AND** subsequent additions use `:GPTEL_SCOPE_<KEY>+:`

## REMOVED Requirements

### Requirement: scope.yml writer preserves structure

**Reason**: Replaced by the drawer writer above. The YAML round-trip (parse → modify → re-emit with snake_case keys) no longer exists; the drawer is mutated in place by `org-entry-put` and multi-value helpers, with no parse / re-emit cycle.

**Migration**: Delete the YAML writer helpers (`jf/gptel-scope--write-pattern-to-scope`'s YAML branch, the YAML emitter functions in `scope-expansion.el`, the kebab-to-snake conversion call). Replace their single dispatch entry with `jf/gptel-scope--write-pattern-to-drawer`.

### Requirement: Context directory resolution

**Reason**: The expansion writer no longer needs to resolve a `scope.yml` path on disk and validate that it exists and is writable. The new write target is the live chat buffer (or, in the rare buffer-less case, the `session.org` file is opened transiently). Resolution becomes "find the chat buffer for this session", which is already implicit in being called from a chat-buffer context, and validation is implicit in `org-entry-put` succeeding.

**Migration**: Delete `jf/gptel-scope--get-scope-file-path` and `jf/gptel-scope--validate-scope-file-writable`. The remaining "no chat buffer found" failure mode is handled by the action handlers signaling `user-error` when there is no buffer to mutate (used only by the manual-edit action).
