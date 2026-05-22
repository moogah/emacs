## MODIFIED Requirements

### Requirement: session.org as authoritative session file

`session.org` SHALL be the single authoritative file for session content and per-buffer configuration. It is in `gptel-chat-mode` block format (`#+begin_user` / `#+begin_assistant` / nested `#+begin_tool`), with a `:PROPERTIES:` drawer at point-min that carries a **full snapshot** of the active configuration: `GPTEL_PRESET`, `GPTEL_PARENT_SESSION_ID` (when applicable), the upstream-compatible chat-mode keys (`GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`), and the scope keys (`GPTEL_SCOPE_*`).

The drawer does NOT carry `:GPTEL_SYSTEM:` — the system prompt is not a drawer property value (long, multi-line, special-character strings are unwieldy as a single property value). Per §Addendum Decision B the system prompt is instead a visible `* System Prompt` heading body in the document; an org heading body carries multi-line, special-character text with no escaping. See `gptel/chat-mode` Requirement: Configuration drawer save on buffer save for the writer contract.

The drawer does NOT carry `:GPTEL_BOUNDS:` — incompatible with the chat-mode block format.

Initial content for a fresh branch session SHALL be the file-level config drawer at `point-min`, then a folded `* System Prompt` heading, then a `* Chat` heading holding the empty user block:
```
:PROPERTIES:
:GPTEL_PRESET: <name>
:GPTEL_MODEL: <preset-model>
:GPTEL_BACKEND: <preset-backend>
:GPTEL_TOOLS: <preset-tool-names>
[:GPTEL_TEMPERATURE: <preset-temperature>]
[:GPTEL_MAX_TOKENS: <preset-max-tokens>]
[:GPTEL_NUM_MESSAGES_TO_SEND: <preset-num-messages-to-send>]
[:GPTEL_SCOPE_READ: ...]
[:GPTEL_SCOPE_WRITE: ...]
[other :GPTEL_SCOPE_* keys]
:END:
* System Prompt
:PROPERTIES:
:VISIBILITY: folded
:END:
<preset :system text — seeds the body; empty when the preset has no :system>

* Chat
#+begin_user

#+end_user
```

The file-level config drawer location is unchanged (`point-min`, no heading or blank line precedes `:PROPERTIES:`). The `* System Prompt` heading carries its own `:PROPERTIES:`/`:VISIBILITY: folded`/`:END:` drawer so org folds the subtree on open; its body is seeded at creation from the active preset's `:system` text, and is empty (heading present, no body text) when the preset declares no `:system`. Turn blocks (`#+begin_user` / `#+begin_assistant`) live under the `* Chat` heading and never appear above it. The chat parser stays heading-indifferent — it locates turn blocks by `#+begin_*` markers, not headings — so the `* System Prompt` body is commentary to it and `gptel-chat-new` scratch buffers (bare `#+begin_user` blocks, no headings) remain valid input.

Initial content for a fresh agent session additionally carries `:GPTEL_PARENT_SESSION_ID: <parent-id>` after `:GPTEL_PRESET:`. Only the keys whose preset value is non-nil are emitted; presets that don't declare e.g. `:max-tokens` produce a drawer without the corresponding line.

#### Scenario: Fresh branch session.org has config drawer, folded System Prompt heading, and Chat heading
- **WHEN** creating a session with preset `coding`
- **THEN** `session.org` starts with a file-level `:PROPERTIES:` config drawer at `point-min` containing `:GPTEL_PRESET: coding`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` populated from the `coding` preset
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer is followed by exactly one `* System Prompt` heading carrying `:VISIBILITY: folded`, its body seeded from the `coding` preset's `:system` text
- **AND** then exactly one `* Chat` heading, under which the empty `#+begin_user\n\n#+end_user\n` block appears
- **AND** no `metadata.yml` is written in the branch directory

#### Scenario: Fresh session.org with a preset that has no system prompt
- **WHEN** creating a session with a preset that declares no `:system`
- **THEN** the `* System Prompt` heading is still emitted (with `:VISIBILITY: folded`) but with an empty body
- **AND** the document layout is otherwise identical — the canonical shape does not depend on whether the preset declares a system prompt

#### Scenario: Fresh agent session.org carries parent session id and full snapshot
- **WHEN** creating an agent session under parent `p-abc-20260424000000` with preset `executor`
- **THEN** the agent `session.org` drawer contains `:GPTEL_PRESET: executor` AND `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`
- **AND** the drawer contains `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` populated from the `executor` preset
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer is followed by a folded `* System Prompt` heading and a `* Chat` heading

### Requirement: Session creation

The system SHALL provide an interactive command for creating persistent sessions. `jf/gptel-persistent-session` SHALL create new sessions with `session.org` as the session file, populated with a pre-configured `:PROPERTIES:` drawer carrying:

- `:GPTEL_PRESET: <name>`
- `:GPTEL_PARENT_SESSION_ID: <id>` for agent sessions
- The full upstream-compatible chat-mode snapshot drawn from the resolved preset spec: `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:` (each emitted only when the preset declares a non-nil value for the corresponding key)
- The scope keys `:GPTEL_SCOPE_*:` resolved from the preset's scope profile

The file-level config drawer is followed by a folded `* System Prompt` heading (whose body is seeded from the preset's `:system` text, or empty when the preset has none) and a `* Chat` heading holding the chat-mode empty user block (`#+begin_user\n\n#+end_user\n`).

`:GPTEL_SYSTEM:` is NOT emitted as a drawer property at creation. The preset's `:system` text seeds the `* System Prompt` heading body instead (§Addendum Decision B).

`metadata.yml` is NOT created.

**Implementation**: `config/gptel/sessions/commands.org` — `jf/gptel-persistent-session`, `jf/gptel--create-session-core`, `jf/gptel--initial-session-body`, `jf/gptel--session-headings-block`. The drawer-text rendering is delegated to `config/gptel/scope-profiles.org`'s `jf/gptel-scope-profile--render-drawer-text` (which accepts the resolved preset spec and emits the full snapshot in addition to scope keys); the `* System Prompt` / `* Chat` heading shape is built by `jf/gptel--session-headings-block`, the single source of truth for the heading layout.

#### Scenario: Create session with default preset writes config drawer plus headings
- **WHEN** run `M-x jf/gptel-persistent-session`
- **THEN** prompted for name
- **AND** generates session-id with timestamp
- **AND** creates directory structure
- **AND** creates `branches/main/session.org` whose file-level config drawer at point-min contains `:GPTEL_PRESET: <default-preset>`, `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:` from that preset, plus `:GPTEL_SCOPE_*:` keys
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** the drawer is followed by a folded `* System Prompt` heading (body seeded from the preset's `:system`) and a `* Chat` heading holding `#+begin_user\n\n#+end_user\n`
- **AND** does NOT create `metadata.yml`
- **AND** opens session.org in `gptel-chat-mode` with the preset applied via the drawer

#### Scenario: Create session with selected preset writes its snapshot
- **WHEN** run `C-u M-x jf/gptel-persistent-session`
- **THEN** prompted for preset selection
- **AND** the selected preset's `:model`, `:backend`, `:tools`, etc. are written to the drawer
- **AND** scope from preset's profile is written to the drawer's `:GPTEL_SCOPE_*:` keys (no `scope.yml` is created)

#### Scenario: Create agent session writes full snapshot plus parent-session-id
- **WHEN** `PersistentAgent` creates an agent under parent session `p-abc-20260424000000` with preset `executor`
- **THEN** agent `session.org` drawer contains `:GPTEL_PRESET: executor`, `:GPTEL_PARENT_SESSION_ID: p-abc-20260424000000`, and the `executor` preset's snapshot keys
- **AND** the drawer does NOT contain `:GPTEL_SYSTEM:`
- **AND** does NOT create `metadata.yml`

#### Scenario: Preset with sparse keys produces a sparse drawer
- **WHEN** the preset declares only `:model` and `:tools` (no `:temperature`, no `:max-tokens`, no `:num-messages-to-send`)
- **AND** a session is created with that preset
- **THEN** the drawer contains `:GPTEL_PRESET:`, `:GPTEL_MODEL:`, `:GPTEL_TOOLS:` only (plus `:GPTEL_SCOPE_*:` if scope-profile applies)
- **AND** the drawer does NOT contain `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, or `:GPTEL_NUM_MESSAGES_TO_SEND:`
