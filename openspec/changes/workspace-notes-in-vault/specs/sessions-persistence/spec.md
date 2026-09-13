# sessions-persistence capability — delta for workspace-notes-in-vault

gptel sessions become a self-contained package: one sessions root, no
consult of any other package, two lifecycle hooks for others to subscribe
to, a title and a self-identifying filetag on every session file, and a
hidden `.agents/` directory. Nothing here names workspaces or org-graph.

## ADDED Requirements

### Requirement: Single sessions root

All session creation SHALL target the directory named by the
`jf/gptel-sessions-directory` defcustom. The sessions module SHALL NOT
consult any other package to choose a location, SHALL NOT provide a
per-context alternative root, and SHALL NOT expose a separate
"global"-suffixed creation command. Session enumeration, registry
initialization, and branch/agent discovery SHALL walk this one root, so
every session the module creates is visible to every listing surface.
The default value SHALL remain `~/.gptel/sessions/`; a configuration MAY
point it elsewhere (for example inside a note vault) without the module
knowing why.

#### Scenario: Every created session is enumerable
- **WHEN** the user creates a session from any tab or context
- **THEN** its directory is created under `jf/gptel-sessions-directory`
- **AND** `jf/gptel--init-registry` in a fresh Emacs lists it

#### Scenario: No other package is consulted
- **WHEN** the tangled `config/gptel/sessions/*.el` files are grepped for
  `workspace-` or `org-graph` symbols
- **THEN** there are zero matches

### Requirement: Session lifecycle hooks

The sessions module SHALL run `jf/gptel-session-created-hook` after a new
session's `session.org` (and any sibling files) exist on disk with their
identity keys and org ID stamped, passing one plist argument with at
least `:session-file`, `:session-id`, `:branch`, `:session-dir`, and
`:project-root` (nil when none was supplied). It SHALL run
`jf/gptel-branch-created-hook` after a new branch's files exist, passing
a plist with at least `:branch-file`, `:parent-file`, `:session-id`, and
`:branch`. Hook functions SHALL be run under an error guard: a failing
hook function is reported but SHALL NOT abort creation or leave the
session unregistered. The module SHALL NOT itself register any function
on these hooks.

#### Scenario: Session-created hook fires with file identity
- **WHEN** a hook function is added to `jf/gptel-session-created-hook` and
  the user creates session `notes-20260913120000`
- **THEN** the function is called once, after `session.org` exists on
  disk, with `:session-id "notes-20260913120000"`, `:branch "main"`, and
  `:session-file` naming that file

#### Scenario: Failing hook does not break creation
- **WHEN** a hook function signals an error during session creation
- **THEN** the session is created, opened, and registered normally
- **AND** the error is reported in `*Messages*`

### Requirement: Session file title and filetag

At creation the sessions module SHALL emit, immediately after the
file-level `:PROPERTIES:` drawer, a `#+title:` keyword whose value is the
session ID (for branches: `<session-id> (<branch>)`) and a `#+filetags:`
keyword carrying `gptel_session`. Both are ordinary org keywords the user
may edit; the module SHALL NOT rewrite them after creation. The title
makes indexed sessions distinguishable; the filetag lets an index
recognise session files without the sessions module naming any indexer.

#### Scenario: New session carries title and filetag
- **WHEN** the user creates session `notes-20260913120000`
- **THEN** `session.org` contains `#+title: notes-20260913120000` and
  `#+filetags: :gptel_session:` after the drawer and before the first user
  block

#### Scenario: User-edited title survives saves
- **WHEN** the user changes the title to `Retry design` and saves, then
  sends a turn and saves again
- **THEN** the title keyword still reads `Retry design`

### Requirement: Hidden agents directory

Sub-agent sessions SHALL live under `<branch-dir>/.agents/` rather than
`<branch-dir>/agents/`. All path helpers, enumerators, and the branch
agent-replication step SHALL use the hidden name. The rename keeps agent
transcripts out of any indexer that skips hidden directories, without the
module naming such an indexer.

#### Scenario: Agent directory is hidden
- **WHEN** a PersistentAgent is created under `branches/main/`
- **THEN** its directory is `branches/main/.agents/<preset>-<ts>-<slug>/`
- **AND** `jf/gptel--find-all-branches-with-agents` finds it

## MODIFIED Requirements

### Requirement: Session creation

The system SHALL provide an interactive command for creating persistent
sessions. `jf/gptel-persistent-session` SHALL create new sessions under
`jf/gptel-sessions-directory` with `session.org` as the session file,
populated with a pre-configured `:PROPERTIES:` drawer carrying:

- `:GPTEL_PRESET: <name>`
- `:GPTEL_PARENT_SESSION_ID: <id>` for agent sessions
- The full upstream-compatible chat-mode snapshot drawn from the resolved
  preset spec: `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`,
  `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`,
  `:GPTEL_NUM_MESSAGES_TO_SEND:` (each emitted only when the preset
  declares a non-nil value)
- The scope keys `:GPTEL_SCOPE_*:` resolved from the preset's scope profile
- `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` when the resolved preset has a
  non-empty `:system` and a sibling file is written
- A file-level org `:ID:` (additive and idempotent)

The drawer is followed by the `#+title:` and `#+filetags:` keywords
(Requirement: Session file title and filetag), a blank line, and the
chat-mode empty user block (`#+begin_user\n\n#+end_user\n`). No
`* System Prompt` heading and no `* Chat` heading is emitted.

Session creation SHALL additionally write a sibling system-prompt file
when the resolved preset declares a non-empty `:system`, named
`system-prompt.<ext>` from the preset's source extension, into the same
directory as `session.org`, verbatim. `:GPTEL_SYSTEM:` is NOT emitted as
a drawer property. `metadata.yml` is NOT created.

A programmatic entry point (`jf/gptel--create-session-core`) SHALL accept
an optional `project-root` used for `${project_root}` scope expansion and
for `GPTEL_WORK_ROOT`; callers outside the module supply it explicitly,
the module never derives it from another package. After the files exist
the module SHALL run `jf/gptel-session-created-hook` (Requirement: Session
lifecycle hooks).

#### Scenario: Create session with default preset writes drawer, keywords, bare user block, and sibling file
- **WHEN** run `M-x jf/gptel-persistent-session`
- **THEN** prompted for name
- **AND** generates session-id with timestamp
- **AND** creates `branches/main/session.org` under
  `jf/gptel-sessions-directory` whose drawer contains `:GPTEL_PRESET:`,
  `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_SCOPE_*:`
  keys, and an `:ID:`
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`
  when the preset has a non-empty `:system`, with the sibling file present
- **AND** the drawer is followed by `#+title:` and `#+filetags:` and then
  the empty user block, with no heading
- **AND** does NOT create `metadata.yml`
- **AND** opens session.org in `gptel-chat-mode` with the preset applied

#### Scenario: Create session with selected preset writes its snapshot and sibling file
- **WHEN** run `C-u M-x jf/gptel-persistent-session`
- **THEN** prompted for preset selection
- **AND** the selected preset's `:model`, `:backend`, `:tools`, etc. are
  written to the drawer, scope keys from its profile are written, and the
  sibling system-prompt file is written when it has a `:system`

#### Scenario: Create agent session writes full snapshot plus parent-session-id plus sibling file
- **WHEN** `PersistentAgent` creates an agent under parent session
  `p-abc-20260424000000` with preset `executor`
- **THEN** the agent `session.org` under `.agents/` carries
  `:GPTEL_PRESET: executor`, `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`,
  the preset snapshot, `:GPTEL_SYSTEM_PROMPT_FILE:` with its sibling file,
  no `:GPTEL_SYSTEM:`, and no `metadata.yml`

#### Scenario: Preset with no :system produces no sibling file and no drawer link
- **WHEN** creating a session with a preset whose `:system` is nil or empty
- **THEN** the drawer lacks `:GPTEL_SYSTEM_PROMPT_FILE:` and no
  `system-prompt.<ext>` file is created

#### Scenario: Preset with sparse keys produces a sparse drawer
- **WHEN** the preset declares only `:model` and `:tools`
- **THEN** the drawer contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`,
  `:GPTEL_TOOLS:` (plus scope keys and the prompt-file key as applicable)
  and none of `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`,
  `:GPTEL_NUM_MESSAGES_TO_SEND:`

#### Scenario: Create session with projects
- **WHEN** user selects projects during creation
- **THEN** first project used as project-root for scope expansion
- **AND** `${project_root}` variables expanded to project path

#### Scenario: Programmatic creation with an explicit project root
- **WHEN** a caller invokes the programmatic entry point with
  `project-root` `~/emacs-workspaces/myproj/`
- **THEN** the drawer's `GPTEL_WORK_ROOT` is that path and
  `GPTEL_SCOPE_WRITE` expands `${project_root}` against it
- **AND** the session-created hook receives `:project-root` equal to it

## REMOVED Requirements

### Requirement: Activities integration

**Reason**: The activities-extensions integration was removed when the
`workspaces` package replaced it; this requirement described code that no
longer exists. Cross-package session association now happens through the
lifecycle hooks, subscribed to by the integration layer.

**Migration**: None; no code implements it. Sessions under
`~/emacs-activities/*/session/` (if any remain) are ordinary session trees
and may be moved under the sessions root by hand.
