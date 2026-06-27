## Components

### Chat-mode public programmatic-send API (modified)

**Location**: `config/gptel/chat/{parser,send,stream}.{org,el}`

**Responsibility**: Expose the chat-mode buffer-parse → messages → assistant-block → stream-callback → fsm-handlers pipeline as a stable public API consumable by non-interactive callers.

**Public symbols** (all renamed from existing `gptel-chat--`-prefixed internals; no `defalias` shims kept):

| Symbol | Kind | Source file | Replaces |
|---|---|---|---|
| `gptel-chat-parse-buffer` | function | `parser.org/el` | `gptel-chat--parse-buffer` |
| `gptel-chat-turns-to-messages` | function | `parser.org/el` | `gptel-chat--turns-to-messages` |
| `gptel-chat-open-assistant-block` | function | `send.org/el` | `gptel-chat--open-assistant-block` |
| `gptel-chat-stream-callback` | function | `stream.org/el` | `gptel-chat--stream-callback` |
| `gptel-chat-fsm-handlers` | variable | `send.org/el` | `gptel-chat--fsm-handlers` |

`gptel-chat-send` is rewritten as a thin wrapper that composes these public symbols (plus its in-flight guard and user-block resolution helpers, which remain internal).

### Persistent-agent tool (rebuilt)

**Location**: `config/gptel/tools/persistent-agent.{org,el}`

**Responsibility**: Spawn a chat-mode agent session under the parent's branch, drive a single autonomous request through chat-mode's public API with composed FSM handlers, and return the final assistant text to the caller's `main-cb`.

**New public surface** (the gptel tool itself; argument schema):

| Argument | Type | Required | Notes |
|---|---|---|---|
| `preset` | string | yes | Symbol name in `gptel--known-presets` |
| `description` | string | yes | Slug for directory naming + overlay header |
| `prompt` | string | yes | Inserted into the agent's empty user block |
| `allowed_paths` | array of string | optional | Glob patterns; nil/empty → `paths.read: []` |

`denied_paths` is dropped.

**Internal functions** (illustrative — final names settled in design.md):

- `jf/gptel-persistent-agent--task` — entry point bound to the gptel tool
- `jf/gptel-persistent-agent--write-scope-file` — wraps the scope-yaml writer
- `jf/gptel-persistent-agent--build-fsm-handlers` — composes overlay + chat-lifecycle + upstream + agent-completion handlers
- `jf/gptel-persistent-agent--on-done`, `--on-errs`, `--on-abrt` — completion handlers per terminal FSM state
- `jf/gptel-persistent-agent--extract-final-text` — reads the agent buffer's last `#+begin_assistant` block, returns the trailing text segment

### Local overlay helpers (kept)

The current persistent-agent has its own copies of three overlay helpers, renamed under the `jf/` prefix:

- `jf/gptel-persistent-agent--indicate-wait` — overlay update for WAIT state
- `jf/gptel-persistent-agent--indicate-tool-call` — overlay update for TOOL state with tool-call rendering
- `jf/gptel-persistent-agent--task-overlay` — creates the parent overlay at a marker position (currently named `--create-overlay`; renamed to `--task-overlay` in this change for naming parity with the upstream pattern)

Plus the supporting `jf/gptel-persistent-agent--hrule` constant.

These stay local. The upstream `gptel-agent` package is not a dependency of this project (commit `eebbc18`, Feb 27, removed it when local preset registration replaced gptel-agent's preset system; no live function call into gptel-agent remains in `config/`). The rebuild keeps the project's "self-contained gptel layer" trajectory rather than reversing that decision for one feature.

The functions are functionally equivalent to upstream's `gptel-agent--{indicate-wait,indicate-tool-call,task-overlay}` (verified by diffing the inlined sources against `runtime/straight/build/gptel-agent/gptel-agent-tools.el:1112-1180`). They are NOT byte-identical — the project's copies use the overlay marker property `'gptel-persistent-agent` (not `'gptel-agent`) and have minor control-flow rewrites.

### Reused codebase infrastructure (no changes)

- `jf/gptel--create-agent-directory` (sessions/filesystem) — agent dir creation
- `jf/gptel--create-session-core` (sessions/commands) — `:PROPERTIES:` drawer + initial content
- `jf/gptel--auto-init-session-buffer` (sessions/commands) — recognizes `nested-agent-re`, fires on `find-file-hook`
- `gptel-chat-mode` major mode + `gptel-chat--apply-declared-preset` — drawer-driven config
- `jf/gptel--register-session` (sessions/registry) — registry entry; called by auto-init

The agent code does not reach into these — it relies on the `find-file` → auto-init contract.

### Removed components

The following local code is removed entirely:

- `jf/gptel--auto-save-session-buffer` (auto-init owns autosave; this hook is no longer added by the agent)
- The dual-duty `:callback` lambda (replaced by chat-mode's stream callback + DONE/ERRS/ABRT handlers)
- The hand-formatted YAML emission for `scope.yml` (replaced by scope-module helper — see "Open question" below)
- The `denied-paths` argument and any code paths handling it
- The `jf/gptel-persistent-agent--fsm-handlers` defvar (replaced by the programmatic `--build-fsm-handlers` builder; the local overlay helpers it referenced are kept and called from the builder)

### Renamed (local overlay helpers, kept)

- `jf/gptel-persistent-agent--create-overlay` → `jf/gptel-persistent-agent--task-overlay` (naming parity with upstream's `--task-overlay`; signature unchanged)

## Interfaces

### Tool invocation → agent buffer (creation flow)

```
jf/gptel-persistent-agent--task(main-cb preset description prompt &optional allowed-paths)
  │
  ├─ validate jf/gptel--session-dir         (parent session check)
  ├─ validate (gptel-get-preset preset)     (preset existence check)
  ├─ session-dir = jf/gptel--create-agent-directory(...)
  ├─ jf/gptel-persistent-agent--write-scope-file(session-dir allowed-paths)
  ├─ session-info = jf/gptel--create-session-core(
  │      session-id session-dir preset
  │      initial-content     ; drawer + #+begin_user PROMPT #+end_user
  │      nil                 ; worktree-paths
  │      nil                 ; project-root
  │      parent-session-id)  ; from jf/gptel--session-id
  ├─ agent-buffer = find-file-noselect(session-info :session-file)
  │     │
  │     └─ find-file-hook fires
  │         └─ jf/gptel--auto-init-session-buffer recognizes nested-agent-re
  │             ├─ activates gptel-chat-mode
  │             ├─ gptel-chat-mode-hook → gptel-chat--apply-declared-preset
  │             ├─ jf/gptel--register-session(...)
  │             ├─ updates current symlink
  │             └─ sets jf/gptel-autosave-enabled t
  │
  └─ proceed to send (next interface)
```

### Agent buffer → gptel-request (send flow)

```
in agent-buffer:
  turns         = gptel-chat-parse-buffer(agent-buffer)
  user-turn     = last-user-turn(turns)              ; the one with the prompt
  messages      = gptel-chat-turns-to-messages(turns)
  insertion     = gptel-chat-open-assistant-block(user-turn)
  stream-cb     = gptel-chat-stream-callback(insertion)

in parent-buffer:
  where         = (or :tracking-marker :position) of gptel--fsm-last
  overlay       = jf/gptel-persistent-agent--task-overlay(where preset description)

in agent-buffer:
  fsm           = gptel-make-fsm
                    :handlers (jf/gptel-persistent-agent--build-fsm-handlers
                               gptel-chat-fsm-handlers
                               main-cb
                               agent-buffer)

  gptel-request messages
    :stream   t
    :callback stream-cb
    :context  overlay
    :fsm      fsm
```

### FSM handler composition (request lifecycle)

```
WAIT  : jf/gptel-persistent-agent--indicate-wait ; overlay UI (parent)
        gptel-chat--on-wait                      ; lifecycle indicator (agent)
        gptel--handle-wait                       ; upstream: fire request

TYPE  : gptel-chat--on-type                      ; lifecycle indicator (agent)

TOOL  : jf/gptel-persistent-agent--indicate-tool-call ; overlay UI (parent)
        gptel-chat--on-tool                      ; lifecycle indicator (agent)
        gptel--handle-tool-use                   ; upstream: run tools

DONE  : jf/gptel-persistent-agent--on-done       ; extract last assistant text,
                                                 ; main-cb, delete overlay
        gptel-chat--on-done                      ; lifecycle indicator (agent)
        gptel--handle-post                       ; upstream: :post hooks

ERRS  : jf/gptel-persistent-agent--on-errs       ; main-cb error, delete overlay
        gptel-chat--on-errs                      ; lifecycle indicator (agent)
        gptel--handle-post

ABRT  : jf/gptel-persistent-agent--on-abrt       ; main-cb abort, delete overlay
        gptel-chat--on-abrt                      ; lifecycle indicator (agent)
        gptel--handle-post
```

Handler ordering is significant — the agent's overlay/completion handlers run *first* on each transition, then chat-mode's lifecycle handler, then the upstream state-driving handler. This mirrors the chaining pattern documented in `config/gptel/chat/send.org` and the existing `jf/gptel-persistent-agent--fsm-handlers` defvar (which the new code replaces with a programmatic builder).

### Final-text extraction (DONE handler)

```
jf/gptel-persistent-agent--extract-final-text(agent-buffer) → string
  ├─ (with-current-buffer agent-buffer
  │     turns = gptel-chat-parse-buffer
  │     last-assistant = last assistant turn in turns
  │     segments = (plist-get last-assistant :segments)
  │     trailing-text-segment = last segment of type 'text in segments
  │                             (skipping any tool-call segments after the
  │                             last text segment results in nil; in that
  │                             case the function returns the empty string
  │                             or a sentinel — settled in design.md)
  │     return (plist-get trailing-text-segment :content))
```

## Boundaries

### In scope

- Persistent-agent module rewrite (`config/gptel/tools/persistent-agent.{org,el}`).
- Chat-mode public API renames + docstrings (`config/gptel/chat/{parser,send,stream}.{org,el}`).
- Updating in-tree callers of the renamed chat-mode internals (the chat module itself; verify with grep).
- Updating `openspec/specs/gptel/{persistent-agent,chat-mode}.md` at archive time.
- Tests for the new agent flow and the new public API contract.

### Out of scope

- **Migration of pre-existing on-disk agent sessions.** Files written by the old implementation are not auto-init-compatible (no drawer); they are archive-only. No migration script.
- **Promoting scope-module YAML writers to public API.** A small helper for "write a fresh scope.yml from a path-spec plist" is needed; whether to add it as a public symbol or keep the wrapper inside persistent-agent is decided in design.md (see Open Questions). Either way, the broader scope module's API surface is not refactored in this change.
- **Cleanup of `denied_paths` references in agent prompts.** The argument is removed from the tool surface; in-tree agent prompt files (`config/gptel/agents/*.md`) are scanned and any references removed, but documentation site / external docs are out of scope.
- **`gptel-agent` upstream coordination.** Not a dependency. The project removed the `gptel-agent` package in commit `eebbc18` (Feb 27) when local preset registration replaced it; no live function call into `gptel-agent` remains. The rebuild keeps local copies of the three overlay helpers under `jf/gptel-persistent-agent--*` rather than reversing that decision.

### Internal vs external

- **External (cross-module) contracts**: the five public chat-mode symbols, the gptel tool's argument schema, the on-disk session.org format (drawer + chat-mode block layout). Any change to these should appear in a future spec delta.
- **Internal (within persistent-agent)**: handler builder, completion handlers, final-text extractor, scope-yml writer. May be refactored without spec impact.

## Testing Approach

### Test Framework

**Buttercup** (preferred per repo CLAUDE.md). The chat-mode tests already use Buttercup with shared fixtures (`config/gptel/chat/test/test-helpers.el`). New tests for the chat-mode public API extend the existing chat-mode test tree. New tests for the rebuilt agent extend the existing tools test tree.

Rationale:
- The change is dominantly behavioral (FSM composition, file-format reload, scope-yml shape) — Buttercup's `describe`/`it`/`expect` and spy system fit the workload.
- The change is incremental on existing chat-mode tests (renames + new public-contract specs) — staying in Buttercup avoids splitting the suite.
- ERT remains acceptable for any small unit helpers that ship with the change but the default for new specs is Buttercup.

### Test Organization

Tests are co-located with the modules they cover, mirroring the existing layout:

```
config/gptel/chat/test/
├── test-helpers.el                       (shared fixtures, existing)
├── parser/
│   ├── buffer-format-spec.el             (existing — updated for renames)
│   ├── escape-round-trip-spec.el         (existing — updated for renames)
│   ├── message-construction-spec.el      (existing — updated for renames)
│   └── public-api-spec.el                (NEW — public-contract assertions
│                                           for parse-buffer + turns-to-messages)
├── send/
│   ├── send-command-spec.el              (existing — updated for renames)
│   ├── backend-invocation-spec.el        (existing — updated for renames)
│   └── public-api-spec.el                (NEW — open-assistant-block +
│                                           fsm-handlers public contract)
└── stream/
    └── public-api-spec.el                (NEW — stream-callback public contract)

config/gptel/tools/test/
└── persistent-agent/                     (NEW directory)
    ├── helpers-spec.el                   (NEW — shared agent test fixtures:
    │                                       mock parent session dir, mock preset)
    ├── creation-spec.el                  (NEW — directory + drawer + scope.yml)
    ├── auto-init-reload-spec.el          (NEW — saved file reloads as
    │                                       interactive chat session)
    ├── send-and-completion-spec.el       (NEW — FSM composition,
    │                                       extract-final-text from buffer)
    └── error-handling-spec.el            (NEW — ERRS / ABRT / unknown preset)
```

The agent tests' fixture file (`helpers-spec.el`) provides:
- `jf/persistent-agent-test--with-mock-parent-session` — sets up `jf/gptel--session-dir`, branch dir, scope.yml in a temp directory
- `jf/persistent-agent-test--with-mock-preset` — registers an ephemeral preset in `gptel--known-presets` for the duration of a test
- `jf/persistent-agent-test--mock-gptel-request` — a `cl-letf` form that stubs `gptel-request` and captures its arguments + FSM for inspection

### Naming Conventions

- Buttercup spec files: `<topic>-spec.el` (existing convention).
- Top-level `describe`: subject under test, e.g., `(describe "gptel-chat-parse-buffer (public API)" ...)`.
- `it` strings: scenario phrasing, mirroring the spec scenario name when one applies, e.g., `(it "parses a chat-mode buffer from outside chat-mode context" ...)`.
- A leading comment in each `it` block names the spec scenario it maps to:
  ```elisp
  (it "writes session.org with a self-describing :PROPERTIES: drawer"
    ;; Scenario: specs/persistent-agent/spec.md §
    ;;   "Agent session creation" → "session.org carries a self-describing
    ;;   :PROPERTIES: drawer"
    ...)
  ```

### Running Tests

```bash
# All chat-mode + persistent-agent tests
./bin/run-tests.sh -d config/gptel/chat
./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent

# Targeted: just the new public-API specs
./bin/run-tests.sh -d config/gptel/chat/test/parser/public-api-spec.el
./bin/run-tests.sh -d config/gptel/chat/test/send/public-api-spec.el

# Targeted: just the rebuilt agent
./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent

# Whole gptel subsystem (regression)
./bin/run-tests.sh -d config/gptel

# Make wrappers
make test-buttercup-directory DIR=config/gptel/tools/test/persistent-agent
make test-report DIR=config/gptel
```

### Test Patterns

- **Setup / teardown**: Buttercup `before-each` / `after-each`. Shared fixtures live in `test-helpers.el` (chat) and `helpers-spec.el` (agent).
- **Mocking external boundaries**: `cl-letf`-based, scoped to the test body, never global (per CLAUDE.md "behavioral tests" guidance):
  - `gptel-request` — stubbed to capture args and synthesize FSM transitions.
  - `gptel-make-fsm` — let through, but the resulting FSM is inspected for handler composition.
  - `find-file-noselect` — let through (real file I/O against a temp directory) so auto-init runs.
  - `jf/gptel-persistent-agent--task-overlay`, `--indicate-wait`, `--indicate-tool-call` — let through (they're pure overlay manipulation; tests assert on overlay text).
  - Network / API calls — never reached; the mocked `gptel-request` short-circuits before any backend is hit.
- **Real file I/O for auto-init paths**: tests that exercise the `find-file` → auto-init contract create real session directories under a `make-temp-file 'directory` root and clean up in `after-each`. This is the only practical way to verify that the agent layout's regex matches and the hook fires.
- **FSM composition tests**: build a fake FSM via `gptel-make-fsm` with the agent's composed handlers, drive it with synthesized state transitions (`gptel--fsm-transition` or direct `funcall` of the handler entries in order), and assert on overlay text + `main-cb` invocations.
- **Scenario assertions go through observable outputs**: file contents, buffer contents, overlay `after-string`, `main-cb` arguments. Internal data structures (FSM `info` plist, buffer-locals beyond the four session vars) are not asserted on directly — they're testable via the public outputs above.

### Scenario Mapping

Every scenario in `specs/persistent-agent/spec.md` and `specs/chat-mode/spec.md` (delta) maps to at least one Buttercup `it` block. Mapping is recorded in a leading comment as shown above. The mapping is one-to-many (a single scenario may produce both a happy-path test and edge-case variants); it is not many-to-one (a single test does not cover multiple scenarios).

For scenarios that are about *absence* (e.g., "no double-dash internal aliases remain"), the test asserts on `(fboundp 'gptel-chat--parse-buffer)` returning nil after the module loads.

For scenarios that are about *file contents* (e.g., "session.org carries a self-describing :PROPERTIES: drawer"), the test reads the file from disk after creation and asserts on its content with regex or org-element parsing.

For scenarios that are about *FSM ordering* (e.g., "WAIT state updates the parent overlay"), the test inspects the `gptel-make-fsm` `:handlers` alist for the expected ordered handler list at each state.

## Dependencies

### Direct dependencies (existing)

- `gptel` — `gptel-make-fsm`, `gptel-request`, `gptel-make-tool`, `gptel--known-presets`, `gptel-get-preset`.
- In-tree session/scope/chat modules — see "Reused codebase infrastructure" above.
- Local overlay helpers in `persistent-agent.org` itself — see "Local overlay helpers (kept)" above.

**No dependency on the upstream `gptel-agent` package.** Removed Feb 27; see the boundary note above.

### New direct dependencies

None. Everything needed already exists.

### Test-only dependencies

- `buttercup` — already a dev dependency; no version bump required.
- `cl-lib` — `cl-letf`, `cl-loop`, etc. — already required everywhere.

## Constraints

### Mode-line-sensitive lexical binding

Per project memory: org-tangle blocks placed before the file header break `lexical-binding: t` detection (Emacs only honours it on line 1). The persistent-agent `.org` file's first babel block must use `:comments no` (matching existing modules). Existing tests in `config/core/test/test-lexical-binding-headers.el` will catch regressions.

### `find-file-hook` ordering

`jf/gptel--auto-init-session-buffer` runs as part of `find-file-hook`. The agent's creation flow assumes that by the time `find-file-noselect` returns, auto-init has finished and the buffer is fully configured. This is true under normal `find-file-hook` semantics but worth pinning explicitly in design.md.

### Performance / latency

The completion handler reads and re-parses the entire agent buffer to extract the final text segment. For typical agent sessions (single-page assistant output), this is O(buffer-size) and trivially fast. If buffers grow large in future use, the cost is borne once per terminal state, not per stream chunk.

### Backwards compatibility

Saved agent files written by the *old* implementation will not auto-init under the new code. This is a knowingly accepted breaking change (recorded in proposal.md). No `defalias` or compatibility shims for the renamed chat-mode internals — call sites are all in-tree and updated within this change.

### Open questions to resolve in design.md

1. **Scope-yml writer surface.** Should a small `gptel-scope-write-fresh-scope-file` (or similar) be promoted to public, mirroring the chat-mode public-API treatment? Or does the agent's scope-yml writing remain a private helper inside the agent module that hand-formats YAML (matching the current implementation's approach but routed through the existing `--write-yaml-*` internals)?
2. **Empty assistant-text fallback.** If the agent's last assistant block ends with a tool-call segment and has no trailing text segment, what does `--extract-final-text` return? Empty string? A sentinel like `"(no final text)"`? An error to the parent?
3. **`find-file-noselect` vs `find-file`.** The agent creation flow runs server-side (no user interaction during creation). `find-file-noselect` returns the buffer without selecting it; `find-file` would select. Which does the spec require? Settled in design.md based on whether the parent buffer should retain focus.
4. **Initial-content shape.** `jf/gptel--create-session-core` accepts an `initial-content` string. We need to inject the prompt into a `#+begin_user` block. Either the initial-content already contains a populated user block, or it contains an empty user block and the agent code inserts the prompt after creation. The first option is simpler; the second is more flexible. Settled in design.md.
