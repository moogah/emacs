# Sessions Persistence (delta for gptel-chat-mode)

This delta replaces key aspects of `sessions-persistence` to reflect the move from `gptel-mode` + markdown session files to `gptel-chat-mode` + org session files. All unchanged requirements from the base spec continue to apply.

## MODIFIED Requirements

### Requirement: Session file format is `session.org` in chat-mode format

Session files SHALL be stored as `session.org` in each branch (and agent) directory. The file format SHALL be the `gptel-chat-mode` block format (symmetric `#+begin_user` / `#+begin_assistant` blocks with nested `#+begin_tool` blocks, delimiter lines at column 0).

This replaces the previous "session.md format" requirement. `session.md` is no longer written, read, or referenced by the sessions subsystem.

**Directory structure** (updated):

```
~/.gptel/sessions/<session-id>/
├── branches/<branch-name>/
│   ├── session.org         # Conversation in chat-mode block format
│   ├── metadata.yml        # Session metadata (unchanged)
│   ├── scope.yml           # Scope configuration (unchanged)
│   ├── branch-metadata.yml # Branch info (unchanged)
│   ├── tools.org           # Tool log (optional, unchanged)
│   ├── system-prompts.org  # Prompt log (optional, unchanged)
│   └── agents/             # Sub-agents (optional, unchanged)
└── current -> branches/<branch-name>  # Active branch symlink (unchanged)
```

**Initial content**: a fresh `session.org` contains exactly `#+begin_user\n\n#+end_user\n` (an empty user block with point on the inner blank line). No Local Variables block, no metadata keywords, no markdown heading.

**Persistence**: saving a session buffer uses plain `save-buffer`. The sessions subsystem SHALL NOT invoke `gptel--save-state` or `gptel--restore-state`. The sessions subsystem MAY maintain a separate `before-save-hook` that touches `metadata.yml`'s `:updated` timestamp — that hook does not modify the session file itself.

#### Scenario: Initial session.org
- **WHEN** creating a session
- **THEN** `session.org` is created with content `#+begin_user\n\n#+end_user\n`
- **AND** the file is valid chat-mode (parseable, empty conversation)
- **AND** no Local Variables block is present

#### Scenario: Conversation persistence
- **WHEN** the user makes requests and saves (`C-x C-s`)
- **THEN** `save-buffer` writes the chat-mode block structure to `session.org`
- **AND** no `gptel--bounds` Local Variables block is appended
- **AND** `metadata.yml`'s `:updated` field is refreshed (via a separate sessions before-save-hook that writes metadata only)

#### Scenario: Legacy `session.md` branches are not enumerated

Legacy invisibility is enforced structurally at the **branch-enumeration boundary**. `jf/gptel--find-all-branches-with-agents` filters every candidate branch through `jf/gptel--valid-branch-directory-p`, which requires `session.org` to exist. Branches containing only legacy `session.md` never reach any consumer (registry initialization, listing, agent discovery).

`jf/gptel--valid-session-directory-p` remains intentionally permissive — it only checks that the `branches/` subdirectory exists. A pre-rename session directory whose branches all use legacy `session.md` still passes session-level validity, but contributes zero entries to enumeration because each of its branches fails branch-level validity. Session-level tightening is deliberately avoided: it would conflate "no branches/ dir" with "branches/ dir holding no chat-mode branches" and provide no behavioural benefit beyond the existing branch-level filter.

- **WHEN** the sessions subsystem scans for session files (e.g., in `jf/gptel--init-registry` or `jf/gptel--find-all-branches-with-agents`)
- **THEN** only branch directories containing `session.org` are surfaced
- **AND** branch directories containing only legacy `session.md` are filtered out inside the enumeration helper
- **AND** a session directory whose branches all use legacy `session.md` still passes `jf/gptel--valid-session-directory-p` but contributes zero entries to enumeration

### Requirement: Auto-initialization enables `gptel-chat-mode`

The auto-init hook (`jf/gptel--auto-init-session-buffer`) SHALL detect session files by matching the path pattern `*/branches/<branch-name>/session.org` (or `*/agents/<agent-name>/session.org`). On match, it SHALL:

1. Extract `session-id` and `branch-name` from the path
2. Set the five buffer-local session variables
3. Register the buffer in `jf/gptel--session-registry`
4. Read `metadata.yml` from the branch directory
5. Apply the preset named in `metadata.yml` via `gptel--apply-preset` with a buffer-local setter
6. Ensure the major mode is `gptel-chat-mode` (switching if necessary)
7. Update the `current` symlink to point at this branch

This replaces the previous Auto-initialization requirement in two respects:

- The path pattern matches `session.org`, not `session.md`
- The hook enables `gptel-chat-mode` (major mode) — it does NOT call `(gptel-mode 1)` (minor mode), does NOT invoke `gptel--save-state`, and does NOT invoke `gptel--restore-state`

All other behavior (buffer-local vars, registry entry, preset application, symlink update) is unchanged.

#### Scenario: Session file detection
- **WHEN** a file matches `*/branches/<branch-name>/session.org` pattern
- **THEN** auto-init recognizes as branch session
- **AND** extracts session-id and branch-name from path
- **AND** enables `gptel-chat-mode` as the major mode

#### Scenario: Agent file detection
- **WHEN** a file matches `*/agents/<agent-name>/session.org` pattern
- **THEN** auto-init recognizes as agent session
- **AND** sets branch-name to `"main"`
- **AND** enables `gptel-chat-mode`

#### Scenario: New session (preset from metadata.yml)
- **WHEN** a freshly created `session.org` is opened for the first time
- **THEN** auto-init reads `preset` from `metadata.yml`
- **AND** applies it via `gptel--apply-preset` with `(lambda (sym val) (set (make-local-variable sym) val))`
- **AND** `gptel-chat-mode` is active
- **AND** `gptel-mode` minor mode is NOT enabled

#### Scenario: Existing session (no Local Variables round-trip)
- **WHEN** a previously-saved `session.org` is reopened
- **THEN** auto-init reads `preset` from `metadata.yml` (the authoritative source)
- **AND** applies it buffer-locally
- **AND** does NOT call `gptel--restore-state` or parse any Local Variables block

### Requirement: Session creation writes `session.org`

`jf/gptel-persistent-session` SHALL create new sessions with `session.org` as the session file, populated with chat-mode initial content (`#+begin_user\n\n#+end_user\n`). All other creation behavior (directory structure, metadata.yml population, scope.yml generation from preset profile) is unchanged.

#### Scenario: Create session with default preset
- **WHEN** `M-x jf/gptel-persistent-session` with default preset
- **THEN** `branches/main/session.org` is created with the chat-mode initial content
- **AND** `metadata.yml` is populated with `session_id`, `created`, `updated`, `preset`
- **AND** the file is opened in `gptel-chat-mode` with the preset applied

## REMOVED Requirements

### Requirement: Session file is markdown (upstream gptel format)

**Removed.** Session files are now org-format chat-mode files (see MODIFIED Requirement: Session file format is `session.org` in chat-mode format). The behaviors tied to markdown session files — upstream `gptel--save-state` writing Local Variables, `gptel--restore-state` reading them on open, and the initial `"###\n"` content — are removed from the sessions subsystem.

### Requirement: Auto-initialization enables `gptel-mode`

**Removed.** Session auto-init enables `gptel-chat-mode` instead (see MODIFIED Requirement: Auto-initialization enables `gptel-chat-mode`). The helper `jf/gptel--ensure-mode-once` is rewritten to ensure `gptel-chat-mode` is active; it does not enable `gptel-mode` as a minor mode.
