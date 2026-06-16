# gptel/sessions-persistence (delta)

## ADDED Requirements

### Requirement: Drawer-resident session identity

The system SHALL store a session's identity in its `session.org` file-level `:PROPERTIES:` drawer at `point-min`, so that identity travels with the file and does not depend on the filesystem path.

The drawer SHALL carry:
- `:GPTEL_SESSION_ID:` — the session id (format `<slug>-<timestamp>`, see Session Identification).
- `:GPTEL_BRANCH:` — the branch name (e.g. `main`).

Session creation, agent creation, and branch creation SHALL emit these keys into the drawer they already render (alongside `:GPTEL_PRESET:`, the chat-mode snapshot keys, and the `:GPTEL_SCOPE_*:` keys).

Identity resolution SHALL be **drawer-first, basename-fallback**:
- When `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` are present in the drawer, those values are authoritative.
- When absent (a pre-existing session created before this change), the system SHALL fall back to deriving the id from the directory basename (`jf/gptel--session-id-from-directory`) and the branch from the enclosing `branches/<branch>/` segment, preserving today's behavior. This fallback is a back-compat grace path, NOT an on-disk migration; no rewrite of old files is performed.

Session type SHALL be inferred from drawer content, not from the path layout: a drawer carrying `:GPTEL_PARENT_SESSION_ID:` denotes an agent session; its absence denotes a branch session.

#### Scenario: Fresh session carries identity keys in its drawer
- **WHEN** a session `react-refactoring-20260120153042` is created on branch `main`
- **THEN** the `session.org` drawer at `point-min` contains `:GPTEL_SESSION_ID: react-refactoring-20260120153042`
- **AND** the drawer contains `:GPTEL_BRANCH: main`

#### Scenario: Identity read from the drawer, not the path
- **WHEN** a session buffer is activated and its drawer carries `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:`
- **THEN** the buffer-local `jf/gptel--session-id` / `jf/gptel--branch-name` take the drawer values
- **AND** the directory basename is NOT consulted

#### Scenario: Identity stable across a directory move
- **WHEN** a session directory carrying drawer identity keys is moved or renamed on disk
- **AND** its `session.org` is reopened
- **THEN** the resolved session-id and branch-name are unchanged (they come from the drawer)

#### Scenario: Old session falls back to basename identity
- **WHEN** a `session.org` whose drawer omits `:GPTEL_SESSION_ID:` is opened
- **THEN** the session-id is derived from the directory basename and the branch from the `branches/<branch>/` segment
- **AND** no rewrite of the file is performed

### Requirement: Content-addressed activation and binding

The system SHALL activate and bind a session buffer through the major mode, not through a global file-open hook. When `gptel-chat-mode` becomes active (by any route — `magic-mode-alist` signature, mode cookie, `M-x`, or `find-file-noselect` of a signature-bearing file), `gptel-chat-mode-hook` SHALL run a guarded binder that establishes all session context.

The binder SHALL:
1. Guard on session content, not path: it SHALL proceed only when the buffer carries the session signature (a `point-min` drawer with a `:GPTEL_`-prefixed key). On a non-session chat buffer (e.g. a `gptel-chat-new` scratch buffer with no drawer) it SHALL be a no-op for session-identification.
2. Resolve identity (drawer-first, basename-fallback per "Drawer-resident session identity") and set the four buffer-local session-identification variables: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`. `jf/gptel--branch-dir` SHALL be the directory of the buffer's own file (`file-name-directory` of `buffer-file-name`) — derived, not reverse-engineered.
3. Register the buffer in `jf/gptel--session-registry` under the drawer-resolved key.
4. Enable autosave (`jf/gptel-autosave-enabled`).
5. Update the `current` symlink for branch sessions (suppressed where no `branches/` directory exists).

The binder SHALL NOT enable `gptel-mode` (minor mode), SHALL NOT invoke `gptel--save-state`, and SHALL NOT invoke `gptel--restore-state`. Preset application, drawer overlay, and parent-session-id installation continue to be performed by `gptel-chat--apply-declared-preset` on the same hook (unchanged).

**Implementation**: `config/gptel/sessions/commands.org` — the binder runs from `gptel-chat-mode-hook` (registered in `config/gptel/chat/`); the `magic-mode-alist` signature is registered alongside the mode definition.

#### Scenario: Binding happens on mode activation
- **WHEN** a signature-bearing `session.org` is opened and `gptel-chat-mode` activates
- **THEN** the four buffer-local session variables are set from drawer-resolved identity
- **AND** the buffer is registered in `jf/gptel--session-registry`
- **AND** `gptel-mode` minor mode is NOT enabled

#### Scenario: Non-session chat buffer is not bound as a session
- **WHEN** `gptel-chat-mode` is activated in a buffer with no `point-min` `:GPTEL_` drawer (e.g. a scratch chat buffer)
- **THEN** the binder performs no session-identification and registers nothing
- **AND** the buffer remains a usable chat buffer

#### Scenario: branch-dir is the file's own directory
- **WHEN** the binder sets `jf/gptel--branch-dir`
- **THEN** the value is `(file-name-directory (buffer-file-name))`
- **AND** no `../..` path-walking is performed to compute it

## MODIFIED Requirements

### Requirement: Session discovery and registry

The system SHALL maintain a global in-memory registry for active sessions.

**Registry structure**:
- Key: `"session-id/branch-name"`
- Value: `(:session-id :session-dir :branch-name :branch-dir :buffer)`
- Metadata: NOT cached (read from disk on-demand)

Registry initialization (`jf/gptel--init-registry`) and the filesystem discovery helpers SHALL learn each session's identity by reading the `session.org` drawer (a cheap head-read reusing the session-signature parse), NOT by deriving it from directory names. The registry key is sourced from the drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` values, with the basename/segment fallback for files that lack them. Consequently the filesystem layout (`branches/`, `agents/`, the `current` symlink, directory names) is pure storage convention and carries no identity meaning.

**Implementation**: `config/gptel/sessions/registry.org`, `config/gptel/sessions/filesystem.org`.

#### Scenario: Registry initialization reads drawers
- **WHEN** gptel initializes
- **THEN** `jf/gptel--init-registry` enumerates session files and reads each one's drawer head
- **AND** creates an entry per session/branch keyed by the drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` (basename fallback when absent)
- **AND** registry count matches the number of valid branches

#### Scenario: Registry lookup
- **WHEN** looking up a session
- **THEN** `jf/gptel-session-find` returns the plist for `session-id/branch-name`
- **AND** returns nil if not found

#### Scenario: Buffer association
- **WHEN** a session is opened
- **THEN** the registry stores the buffer reference
- **AND** `jf/gptel--update-session-buffer` updates the registry

#### Scenario: Session-level configuration read on-demand
- **WHEN** code needs the active preset, parent-session-id, or scope for a session
- **THEN** the value is read from the session buffer's `:PROPERTIES:` drawer (or, when no buffer is open, from `session.org`'s drawer on disk)
- **AND** the registry holds NO cached copy — `gptel-chat--apply-declared-preset` re-reads the drawer at mode activation

### Requirement: Buffer-local session state

The system SHALL track session metadata in buffer-local variables for runtime access. The identity variables are populated from the drawer (drawer-first, basename-fallback), not from the file path.

#### Scenario: Buffer-local vars set on session open
- **WHEN** the user opens a session's `session.org`
- **THEN** the `gptel-chat-mode-hook` binder sets:
  - `jf/gptel--session-id` (from the drawer's `:GPTEL_SESSION_ID:`, basename fallback)
  - `jf/gptel--branch-name` (from the drawer's `:GPTEL_BRANCH:`, segment fallback)
  - `jf/gptel--branch-dir` (the file's own directory)
  - `jf/gptel--session-dir` (the session root for this branch)
- **AND** `gptel-chat-mode-hook` runs `gptel-chat--apply-declared-preset`, which reads the file-level `:PROPERTIES:` drawer and sets `jf/gptel--parent-session-id` from `:GPTEL_PARENT_SESSION_ID:` when present (agent sessions)

#### Scenario: Agent session vars
- **WHEN** opening an agent session
- **THEN** `jf/gptel--parent-session-id` is set from the drawer's `:GPTEL_PARENT_SESSION_ID:` value
- **AND** `jf/gptel--branch-name` is `main` (from the drawer's `:GPTEL_BRANCH:` or the default; agents don't branch)

## REMOVED Requirements

### Requirement: Auto-initialization enables `gptel-chat-mode`

**Reason**: The global `find-file-hook` mechanism (`jf/gptel--auto-init-session-buffer`) is retired. It ran on every file open, recognized sessions by matching three hardcoded path-layout regexes (branch / nested-agent / flat-agent) with `../..` / `../../..` walks, and derived identity from directory basenames. Activation is replaced by content-addressed recognition (`magic-mode-alist` session signature, see `gptel/chat-mode`), and binding/identity move into a guarded `gptel-chat-mode-hook` function reading drawer-resident identity (see "Content-addressed activation and binding" and "Drawer-resident session identity" above).

**Migration**: None required for behavior — existing sessions are still recognized (their drawers carry `:GPTEL_PRESET:`, which the signature matches) and still bind (identity falls back to the directory basename when `:GPTEL_SESSION_ID:` is absent). The `add-hook 'find-file-hook #'jf/gptel--auto-init-session-buffer` registration, the `jf/gptel--auto-init-session-buffer` function, and its three layout regexes SHALL be removed entirely from `config/gptel/sessions/commands.org`.
