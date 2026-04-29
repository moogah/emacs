# Scope Profiles (Delta Spec)

This delta switches the terminal output of the profile resolver from a `scope.yml` sidecar writer to a property-drawer writer that targets `session.org`. Profile templates themselves remain plain YAML on disk; only the *output sink* changes.

## MODIFIED Requirements

### Requirement: Mutable scope drawer in session.org

Each session SHALL receive a populated scope drawer in its `session.org` at creation time. The drawer is the single source of truth for enforcement; the profile template and the registered preset are never consulted by validators.

**Implementation**: `jf/gptel-scope-profile--apply-to-drawer` in `config/gptel/scope-profiles.org` (replaces the previous `--write-scope-yml`).

#### Scenario: Drawer populated at session creation
- **WHEN** a session is created with preset `executor`
- **THEN** the new `session.org` is written with a `:PROPERTIES:` drawer at `point-min` containing `:GPTEL_PRESET:`, the preset's resolved scope keys (`:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:`, `:GPTEL_SCOPE_CLOUD_AUTH:`, etc.), and the chat-mode initial content (`#+begin_user\n\n#+end_user\n`)
- **AND** no `scope.yml` is written

#### Scenario: Drawer is mutable
- **WHEN** scope expansion adds a path during a session
- **THEN** the chat buffer's drawer is updated in place via `org-entry-put` / multi-value helpers
- **AND** the buffer is saved
- **AND** the registered preset and the profile template remain unchanged

#### Scenario: Drawer is the enforcement source
- **WHEN** a tool invocation requires path validation
- **THEN** `scope-validation` reads the chat buffer's drawer (or the file's drawer if no buffer is open)
- **AND** does NOT read from `gptel--known-presets` or from profile files
- **AND** does NOT read any `scope.yml` (no such file exists)

### Requirement: Integration with session creation

The profile module SHALL expose a single creation entrypoint that, given a preset name, target session.org path, optional project root, and optional worktree paths, returns the drawer text (as a string) or applies it to a freshly created file.

**Implementation**: `jf/gptel-scope-profile--create-for-session` in `config/gptel/scope-profiles.org` — signature unchanged, but the implementation no longer performs any YAML I/O.

#### Scenario: Returns drawer text for embedding in initial session.org
- **WHEN** session creation calls `--create-for-session` with a preset and target directory before opening the buffer
- **THEN** the function resolves the profile, expands variables, and returns a string of the form `:PROPERTIES:\n:GPTEL_PRESET: ...\n:GPTEL_SCOPE_READ: ...\n...\n:END:\n` ready to prepend to the chat-mode initial content
- **AND** session creation writes the resulting `session.org` in one shot

#### Scenario: Applies to an existing session.org buffer
- **WHEN** `--create-for-session` is called against an already-open chat buffer (e.g. an agent or branched session being initialized in a buffer)
- **THEN** the function uses `org-entry-put` and multi-value helpers to write each `:GPTEL_SCOPE_*` key into the buffer's `:PROPERTIES:` drawer at `point-min`
- **AND** existing non-scope drawer keys (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`) are preserved

## REMOVED Requirements

### Requirement: scope.yml writer (`jf/gptel-scope-profile--write-scope-yml`)

**Reason**: With scope persisted in the property drawer, the YAML serializer is dead code. Its callers (session creation, persistent-agent creation) now go through the drawer applicator.

**Migration**: Delete `jf/gptel-scope-profile--write-scope-yml` and the `to-yaml-helpers` (key conversion, list/scalar emit) it depends on. Replace its single call site in `--create-for-session` with the drawer applicator. The "write to scope.yml at TARGET-DIR" docstring contract is replaced by "apply to the session's drawer (buffer or initial content)".
