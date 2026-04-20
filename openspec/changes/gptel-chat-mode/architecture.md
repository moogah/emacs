## Components

This change introduces chat-mode as a new subsystem under `config/gptel/chat/` **and** revises four modules in `config/gptel/sessions/` so that sessions use chat-mode as their canonical mode. Component inventory is in two groups: new modules and modified modules.

### New modules (under `config/gptel/chat/`)

The chat-mode subsystem is factored into six cohesive modules. Each has a narrow responsibility and a small public surface.

### `gptel-chat-mode` (mode)
- Defines the `gptel-chat-mode` major mode, derived from `org-mode`
- Registers the keymap (`C-c C-c` send, `C-c C-n` / `C-c C-p` navigation, `C-c C-r` regenerate, display toggle)
- Handles mode activation and deactivation (installs/removes font-lock rules and overlay refreshers)
- Sets mode-local variables required by the other modules

### `gptel-chat-parser` (parser)
- Walks the buffer with a small state machine and produces a turn list: an ordered list of `(:role user|assistant :content STR :tool-calls (...))` plists
- Recognizes **outer** `#+begin_user` / `#+begin_assistant` blocks (blocks not nested inside another user or assistant block) regardless of org heading depth, and nested `#+begin_tool` blocks inside assistant blocks
- Treats content outside turn blocks — org headings, paragraphs, drawers, `#+keyword:` lines — as human organization/commentary and skips it without interpretation
- Validates structural integrity (matched delimiters, tool blocks only inside assistant blocks, no turn-inside-turn) and signals user-visible errors at the offending line
- Converts the turn list to a `gptel-request`-compatible message list, un-escaping `,#+end_...` lines as it does so

### `gptel-chat-send` (send)
- Provides the interactive `gptel-chat-send` command
- Validates send preconditions (point in a non-empty user block, buffer well-formed)
- Constructs the message list via the parser and invokes `gptel-request` with the appropriate `:prompt`, `:stream t`, and `:callback` keys
- Manages the open-assistant-block lifecycle (insert opening delimiter, register streaming callback, insert closing delimiter on completion, handle abort)

### `gptel-chat-stream` (stream)
- Implements the streaming callback passed to `gptel-request`
- Maintains a small state machine: accumulator for partial-line holdback, current-position marker, abort flag
- Performs delimiter-collision sanitization line-by-line on each chunk, escaping `^#\+end_\(user\|assistant\|tool\)\b` to `,#+end_...`
- Handles tool-call events by emitting `#+begin_tool`/`#+end_tool` nested blocks inside the active assistant block and inserting tool results upon completion

### `gptel-chat-nav` (navigation)
- `gptel-chat-next-turn`, `gptel-chat-previous-turn`: move point to the start of the next/previous outer turn block (regardless of org heading context)
- `gptel-chat-regenerate`: remove the last `#+begin_assistant` block and re-issue the send for the preceding user turn
- No coupling to parser internals — uses `re-search-forward` on block delimiters

### `gptel-chat-display` (display)
- Applies the v1 display layer: distinct faces (or a `line-prefix` overlay) on user-block content and assistant-block content
- Provides `gptel-chat-toggle-display-layer` to enable/disable without modifying buffer text
- Pure presentation: no callers depend on its side effects, and removing it leaves the buffer semantically identical

### `gptel-chat-menu` (menu, preset-wiring)
- Applies a preset at mode activation: parses the buffer's `GPTEL_PRESET` Org property drawer or file-local `gptel--preset` value, calls `gptel--apply-preset` with a buffer-local setter
- Defines `gptel-chat-menu` — a transient prefix reusing `gptel-menu`'s configuration infixes with the Send suffix rebound to `gptel-chat-send`
- Does **not** enable `gptel-mode`; chat-mode owns the buffer role exclusively (design.md Decision 16)

### Modified modules (under `config/gptel/sessions/`)

The sessions subsystem is updated so session buffers use `gptel-chat-mode` as their mode. Four modules change; the rest are untouched.

### `sessions/commands` (modified)
- Auto-init hook `jf/gptel--auto-init-session-buffer` now keys on `*/branches/<branch>/session.org` (renamed from `session.md`)
- On match: extracts session-id / branch-name, sets buffer-local session vars, registers in `jf/gptel--session-registry`, applies preset from `metadata.yml` via `gptel--apply-preset` with buffer-local setter, ensures `gptel-chat-mode` is the active major mode
- Does **not** call `(gptel-mode 1)`; does **not** invoke `gptel--save-state` / `gptel--restore-state`
- `jf/gptel-persistent-session` creates `session.org` with chat-mode initial content (`#+begin_user`/`#+end_user`)

### `sessions/filesystem` (modified)
- Directory templates reference `session.org` (not `session.md`) for branches and agents
- No change to session-id format, `current` symlink behavior, or branch-metadata handling

### `sessions/branching` (modified)
- Branch-point selection enumerates outer `#+begin_user` blocks via the chat-mode parser (`gptel-chat--parse-buffer`) — replaces the `gptel` text-property scan
- Context truncation copies buffer content up to a turn-boundary position (end-of-user-block for "include prompt"; start-of-user-block for "exclude prompt") — replaces `gptel--bounds`-filtering
- Writes truncated content to the new branch's `session.org`
- Registry, metadata, and directory-creation paths unchanged

### `sessions/activities-integration` (modified)
- Session-creation helpers (`jf/gptel-session-create-persistent`) emit `session.org` unconditionally; no mode-selection parameter
- Worktree tracking (`gptel-activity-worktrees` buffer-local) and activity directory layout unchanged

## Interfaces

### Module boundaries

```
 user ─(C-c C-c)─▶ gptel-chat-send
                        │
                        │ (1) parse buffer
                        ▼
                  gptel-chat-parser ──▶ turn list ──▶ message list
                        │
                        │ (2) invoke backend
                        ▼
                   gptel-request (upstream)
                        │
                        │ (3) stream callback
                        ▼
                 gptel-chat-stream ──▶ buffer (sanitized inserts)
                                       ─ tool calls ─▶ nested #+begin_tool blocks

 user ─(nav keys)─▶ gptel-chat-nav ──▶ (move point / delete last assistant)

 mode-activation ──▶ gptel-chat-display (install overlays)
```

### Public functions

| Function | Signature | Module |
|---|---|---|
| `gptel-chat-mode` | `()` (interactive major-mode function) | mode |
| `gptel-chat-send` | `()` (interactive) | send |
| `gptel-chat-next-turn` | `()` (interactive) | nav |
| `gptel-chat-previous-turn` | `()` (interactive) | nav |
| `gptel-chat-regenerate` | `()` (interactive) | nav |
| `gptel-chat-toggle-display-layer` | `()` (interactive) | display |

### Internal functions (tested, not user-facing)

| Function | Purpose | Module |
|---|---|---|
| `gptel-chat--parse-buffer` | Buffer → turn list | parser |
| `gptel-chat--turns-to-messages` | Turn list → `gptel-request` message list (un-escaping applied) | parser |
| `gptel-chat--sanitize-chunk` | Chunk string → escaped chunk string (handles partial-line holdback via state) | stream |
| `gptel-chat--install-stream-callback` | Builds the closure passed to `gptel-request` | stream |

### Data contracts

**Turn list element**:
```elisp
(:role 'user :content "..." :start N :end M)
(:role 'assistant :segments ((:type text :content "...")
                             (:type tool-call :name "..." :args "..." :result "...")
                             (:type text :content "...")))
```

**Message list** (what `gptel-request` receives via `:prompt`): a list of plists or alists in `gptel-request`'s documented shape — role, content, and tool-call fields matching upstream expectations. Exact shape per `gptel-request`'s `:prompt` contract.

## Boundaries

**In scope for this change:**
- All seven new chat-mode modules above (six core + preset/menu)
- Four modified session modules (`commands`, `filesystem`, `branching`, `activities-integration`)
- A test suite covering every chat-mode spec scenario plus updated session scenarios (auto-init for `session.org`, branching via turn list)
- Integration into `jf/enabled-modules` so chat-mode loads before the sessions modules (sessions depends on chat-mode)
- Basic default keybindings for the six chat-mode interactive commands

**Explicitly out of scope (tracked for follow-up changes):**
- Automated migration from pre-chat-mode `session.md` files — clean break (see design.md Decision 19).
- Display-layer delimiter hiding (the "store-symmetric-display-asymmetric" refinement). v1's display layer is presentation of block content; delimiters remain visible.
- `org-edit-special`-style indirect buffer for rich prompt composition.
- Per-chat preset/model selection UI *beyond* what `gptel-menu` (with our Send-suffix rebind) already provides.
- Persistence of in-progress streaming state across Emacs restarts.

**Seams kept explicit** (so follow-up changes can extend without rework):
- Parser produces a structured turn list rather than returning the message list directly — the branching rewrite consumes that same turn list, as can future session features (search, summarization, export).
- Send is factored into validation, parsing, and backend invocation so session-aware send (e.g., session-level logging) can layer on as a wrapping hook.
- Display is isolated as a module so adding delimiter hiding later is a local change.
- Auto-init is a single entry point — adding new path patterns (e.g., a different session layout) is a one-line extension.

## Testing Approach

### Test Framework

**Buttercup** (new-test default per repo convention; documented in `CLAUDE.md` §Testing Infrastructure). Rationale:
- BDD `describe` / `it` / `expect` syntax maps cleanly to the "Requirement / Scenario" structure of the spec (one `describe` per requirement, one `it` per scenario).
- Built-in `before-each` / `after-each` is a better fit than ERT's manual `let`-wrapping for tests that set up buffers and mock `gptel-request`.
- `spy-on` is useful for verifying `gptel-request` is called with expected arguments.

ERT is not used for this module; existing ERT suites elsewhere in the repo are not touched.

### Test Organization

Chat-mode tests live under `config/gptel/chat/test/` and mirror the chat-mode module layout. Session-integration tests extend the existing `config/gptel/sessions/test/` tree.

```
config/gptel/chat/test/
├── parser/
│   ├── buffer-format-spec.el        — Requirement: Buffer format validation
│   ├── message-construction-spec.el — Requirement: Message construction
│   └── escape-round-trip-spec.el    — Delimiter escape / un-escape
├── send/
│   ├── send-command-spec.el         — Requirement: Send command
│   └── backend-invocation-spec.el   — Requirement: gptel-request backend usage
├── stream/
│   ├── streaming-spec.el            — Requirement: Response streaming and sanitization
│   ├── chunk-split-spec.el          — Split-across-chunks collision
│   └── tool-call-spec.el            — Requirement: Tool-call rendering
├── nav/
│   ├── navigation-spec.el           — Requirement: Turn navigation
│   └── regenerate-spec.el           — Requirement: Regenerate last response
├── display/
│   └── display-layer-spec.el        — Requirement: Display-layer role distinction
├── menu/
│   ├── preset-wiring-spec.el        — Preset parsing + apply on mode activation
│   └── menu-send-rebind-spec.el     — gptel-chat-menu Send invokes gptel-chat-send
├── integration/
│   └── end-to-end-spec.el           — Full send round-trip with stubbed backend
└── helpers-spec.el                  — Shared fixtures and matchers

config/gptel/sessions/test/
├── commands/
│   ├── auto-init-chat-mode-spec.el     — Auto-init enables chat-mode on session.org
│   ├── session-org-creation-spec.el    — jf/gptel-persistent-session writes session.org
│   └── preset-application-spec.el      — Preset applied from metadata.yml (unchanged)
├── filesystem/
│   └── directory-templates-spec.el     — session.org path pattern throughout
├── branching/
│   ├── branch-point-selection-spec.el  — Turn-list enumeration replaces text-property scan
│   ├── context-truncation-spec.el      — Truncate at turn boundary (include/exclude)
│   └── branching-integration-spec.el   — Full branch create with chat-mode buffer
└── activities/
    └── activity-session-chat-spec.el   — Activity-backed sessions emit session.org
```

Source modules live in `config/gptel/chat/` as literate `.org` files tangling to `.el`, per the repo's literate-programming convention.

### Naming Conventions

- **Files**: `<concern>-spec.el` (Buttercup convention in this repo).
- **`describe` string**: the requirement name from the spec, e.g. `"Message construction from buffer"`.
- **`it` string**: the scenario name or a paraphrase, e.g. `"assistant turn with one tool call produces tool_call/tool_result pair"`.
- **Internal helpers**: prefixed `gptel-chat-test--` to avoid collision with production code.

### Running Tests

Primary commands:

```bash
# All chat-mode tests (Buttercup, directory-scoped)
./bin/run-tests.sh -d config/gptel/chat

# Single concern
./bin/run-tests.sh -d config/gptel/chat/test/parser
./bin/run-tests.sh -d config/gptel/chat/test/stream

# Full repo test run includes chat-mode tests automatically
make test
```

Interactive: `C-c t` opens the transient test menu.

### Test Patterns

**Backend stubbing** — `gptel-request` is stubbed synchronously via `cl-letf` in each test that exercises send. The stub:
1. Captures the arguments it was called with (so tests can `expect` the `:prompt` shape).
2. Invokes the streaming `:callback` synchronously with a scripted sequence of chunks followed by a completion signal.
3. For tool-call scenarios, emits a scripted tool-call event and waits for (or simulates) the tool-result message.

Example shape (pseudocode):

```elisp
(describe "Send command"
  (before-each
    (setq-local gptel-chat-test--captured-prompt nil)
    (spy-on 'gptel-request
            :and-call-fake
            (lambda (&rest plist)
              (setq gptel-chat-test--captured-prompt
                    (plist-get plist :prompt))
              (funcall (plist-get plist :callback)
                       "Paris." 'complete))))
  (it "sends constructed messages to gptel-request"
    (with-gptel-chat-buffer "#+begin_user\nCapital of France?\n#+end_user\n"
      (goto-char (point-min))
      (gptel-chat-send)
      (expect gptel-chat-test--captured-prompt
              :to-equal '((:role "user" :content "Capital of France?"))))))
```

**Buffer fixtures** — tests use an inline helper (`with-gptel-chat-buffer`) from `helpers-spec.el` that creates a temp buffer, inserts the given content, activates `gptel-chat-mode`, and runs the body.

**No org-element dependency in tests** — tests that assert on block structure use string comparison or regex on buffer contents, not `org-element-parse-buffer`. This avoids coupling tests to org-mode internals.

**Streaming split tests** — the chunk-split scenario is exercised by scripting the stub to call the callback with multiple small chunks that split a `#+end_assistant` collision across the boundary. The test asserts the final buffer content contains the escaped form.

**Abort tests** — the stub synthesizes an abort by invoking the callback with an error signal (per `gptel-request` abort conventions). The test asserts the block closes cleanly with the interruption marker.

### Scenario Mapping

Every scenario in `specs/gptel-chat-mode/spec.md` maps to at least one `it` block in the test tree. The mapping is one-to-one except where a scenario is a natural subcase of another; in those cases both are covered by a single `it` with explicit assertions for each subcase.

Mapping table (requirement → test file):

| Spec requirement | Test file(s) |
|---|---|
| Mode definition and activation | `parser/buffer-format-spec.el` (basic activation) |
| Buffer format validation | `parser/buffer-format-spec.el` |
| Message construction from buffer | `parser/message-construction-spec.el`, `parser/escape-round-trip-spec.el` |
| Send command | `send/send-command-spec.el` |
| Response streaming and sanitization | `stream/streaming-spec.el`, `stream/chunk-split-spec.el` |
| Tool-call rendering inside assistant blocks | `stream/tool-call-spec.el` |
| Turn navigation | `nav/navigation-spec.el` |
| Regenerate last response | `nav/regenerate-spec.el` |
| Display-layer role distinction | `display/display-layer-spec.el` |
| `gptel-request` backend usage | `send/backend-invocation-spec.el` |

Shared fixtures and matchers live in `helpers-spec.el`.

## Dependencies

**Runtime:**
- `gptel` — public `gptel-request` API (stable; no internal gptel symbols referenced except for standard customization variables like `gptel-model` and `gptel-backend`)
- `org` — derived mode; uses `org-escape-code-in-string` for sanitization
- `cl-lib`, `subr-x` — standard

**Test:**
- `buttercup` — test framework (already in repo via straight.el)

**No new external dependencies.**

## Constraints

- **Literate programming**: all source modules are `.org` files tangling to `.el` via `./bin/tangle-org.sh`. The first block of each `.org` file uses `:comments no` to keep the `lexical-binding: t` mode line on file line 1 (per repo memory on lexical-binding regression).
- **Lexical binding**: required; callback closures (streaming state, abort flags) depend on it.
- **No upstream gptel patches**: everything is downstream; if `gptel-request` behavior changes upstream, only the `send` and `stream` modules are affected and the change is localized.
- **Backwards compatibility**: none required — this is a new mode, not modifying existing behavior. Existing `gptel-mode` buffers and `session.*` files are untouched.
- **Performance**: target is interactive responsiveness. Parsing a buffer with ~100 turns should complete in under 50ms. The parser uses `re-search-forward` and buffer positions rather than `org-element-parse-buffer` to stay fast on large logs.
- **Emacs version**: same baseline as the rest of the repo (no new minimum required).
