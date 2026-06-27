## Components

The change touches two existing modules and an existing module-pair (preset registration is read-only here, only relied upon to derive the source file extension). No new modules.

- **`config/gptel/sessions/commands.org`** — owner of `jf/gptel--create-session-core`, `jf/gptel--initial-session-body`, `jf/gptel--session-headings-block` (to be deleted). Gains a small sibling-file writer helper (e.g., `jf/gptel--write-system-prompt-sibling-file`) and a one-liner that threads `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` into the drawer-text composition. `jf/gptel--initial-session-body` reverts to its pre-Addendum signature (no `system-prompt` argument, no heading composition).

- **`config/gptel/chat/menu.org`** — owner of the chat-mode restore path (`gptel-chat--apply-declared-preset`, the heading-read helpers being deleted, the new sibling-file reader) and the save path (`gptel-chat--save-state`, the heading-write helpers being deleted). Gains `gptel-chat--apply-system-prompt-file` (restore) and a small file-resolution helper `gptel-chat--system-prompt-file-path` shared between restore, pre-send refresh, and the menu affordance. Gains an "Edit system prompt" transient suffix that replaces the upstream `gptel-system-prompt` infix in the chat-mode menu.

- **`config/gptel/chat/mode.org`** (only if the `:VISIBILITY: folded` startup hook lives there) — strip the wiring that activates `org-set-visibility-according-to-property` for chat-mode. With no headings carrying `:VISIBILITY:`, the hook has nothing to fold and can be removed cleanly.

- **`config/gptel/tools/persistent-agent.org`** — `jf/gptel-persistent-agent--initial-body` currently delegates to `jf/gptel--session-headings-block` (per the prior change's spec). With that helper deleted, the agent body emitter reverts to producing a bare drawer + populated user block, with no heading composition. The sibling-file writer in `commands.org` is also called for agent creation, so the agent path picks up the new contract without bespoke code.

- **`config/gptel/preset-registration.org`** — read-only here. The new code derives a preset's source file extension by re-resolving `<jf/gptel-presets-directory>/<preset-name>.<ext>` against the on-disk preset file. Since today's scanner only registers `*.md`, the registry does not need to grow a "source path" field; we look it up by convention. (When `.org`-format preset support lands as a follow-up, this resolver is the single point that needs updating.)

The pre-send refresh hook needs a place to live. Two natural options:

- **Option A — wire into the chat-mode menu's `Send` suffix.** Local to `menu.org`; the refresh runs before the request is dispatched. Doesn't cover requests dispatched outside the menu.
- **Option B — wire into `gptel-request` / `gptel-send` via advice or a buffer-local hook.** Covers all request paths in chat-mode buffers. Slightly more intrusive (advising upstream).

We default to **Option B** with a narrow filter: `gptel-pre-send-hook` (or, if upstream lacks one, `advice-add 'gptel-request :before` filtered on `(derived-mode-p 'gptel-chat-mode)`). Resolves the cache-staleness concern uniformly. Easy to test in isolation.

## Interfaces

**Deleted public/internal symbols** (chat/menu.org):
- `gptel-chat--system-prompt-heading-body-region`
- `gptel-chat--system-prompt-heading-body`
- `gptel-chat--apply-system-prompt-heading`
- `gptel-chat--write-system-prompt-heading`
- `gptel-chat--config-drawer-end`
- `gptel-chat--turn-block-marker-re`

**Deleted internal symbols** (sessions/commands.org):
- `jf/gptel--session-headings-block`

**New internal symbols** (sessions/commands.org):
- `jf/gptel--preset-source-file-extension` — given a preset name (symbol), return the extension (`"md"` or `"org"`) of the on-disk file in `jf/gptel-presets-directory`. Returns `"md"` as the default when the file is absent (defensive — shouldn't happen in practice since the preset is registered).
- `jf/gptel--write-system-prompt-sibling-file` — given a session directory, a preset spec (plist), and the resolved extension, write `system-prompt.<ext>` with the preset's `:system` body verbatim. Returns the basename for threading into the drawer. Returns `nil` (and does not write) when the preset has no non-empty `:system`.

**New internal symbols** (chat/menu.org):
- `gptel-chat--system-prompt-file-path` — resolve `:GPTEL_SYSTEM_PROMPT_FILE:` from the drawer at point-min relative to `(file-name-directory buffer-file-name)`. Returns the absolute path, or `nil` when the property is unset.
- `gptel-chat--apply-system-prompt-file` — call site of the resolver. When the file exists and is non-empty, install its contents as buffer-local `gptel--system-message`. No-op otherwise.
- `gptel-chat--refresh-system-prompt-from-file` — pre-send refresh. Same resolution/read logic as `--apply-system-prompt-file`, called from the request submission path.
- `gptel-chat--edit-system-prompt-file` — transient command. Resolves (or prompts to create) the sibling file and `find-file-other-window`s it.

**Modified call sites**:
- `jf/gptel--create-session-core` (`commands.org`): after rendering the drawer text and before the body composition, call `jf/gptel--write-system-prompt-sibling-file`. When it returns a basename, append `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` to the drawer text just before the `:END:` line.
- `jf/gptel--initial-session-body` (`commands.org`): signature reverts to `()` (no `system-prompt` parameter). Returns the bare-user-block string the pre-Addendum body produced.
- `gptel-chat--apply-declared-preset` (`menu.org`): last step changes from `apply-system-prompt-heading` to `apply-system-prompt-file`.
- `gptel-chat--save-state` (`menu.org`): drop the `write-system-prompt-heading` call. The drawer write is unchanged. The function gets shorter.
- Chat-mode transient menu: replace the upstream `gptel-system-prompt` infix entry with `gptel-chat--edit-system-prompt-file`.

**External (upstream) interfaces touched**:
- We advise `gptel-request` (or wire into a pre-send hook upstream provides) for the pre-send refresh, filtered to chat-mode buffers. The advice is `:before`, side-effect-only, and never short-circuits the underlying call.

## Boundaries

**In scope:**
- Removing all `* System Prompt` / `* Chat` heading composition, reading, and writing from the session creation, chat-mode restore, and chat-mode save paths.
- Adding the sibling-file writer at session creation.
- Adding the sibling-file reader at chat-mode restore.
- Adding the pre-send refresh.
- Replacing the chat-mode menu's system-prompt infix with the file-opener affordance.
- Spec deltas on `gptel/chat-mode` and `gptel/sessions-persistence`.
- Reconciling the interfaces register (mark superseded entries).

**Out of scope:**
- A migration command that walks existing on-disk sessions and rewrites them. Stray headings in old session files will sit there until the user removes them; the restore fall-through to the preset means functionality is preserved.
- `.org`-format preset support. The sibling-file design is extension-agnostic (the writer derives the extension from the preset source file), but enabling preset authoring in `.org` requires widening `jf/gptel-preset-register-all`'s scanner regex (`\\.md\\'`) and adding an org-frontmatter parser. Deferred to a separate change.
- Reworking the legacy `:GPTEL_SYSTEM:` drawer overlay. It remains in place as the back-compat middle tier of the restore precedence. Removing it is orthogonal and would warrant its own change.
- Auto-watching the sibling file via `file-notify`. The pre-send refresh is sufficient; a watcher would add a subsystem with lifecycle bookkeeping for little additional benefit.
- A "reload system prompt now" interactive command. The pre-send refresh fires automatically; `revert-buffer` on `session.org` also re-triggers chat-mode restore.

## Testing Approach

### Test Framework

**Buttercup** for all new tests. Existing chat-mode and sessions-persistence test suites are already Buttercup-first (`*-spec.el` under `config/gptel/chat/test/menu/` and `config/gptel/sessions/test/commands/`). No ERT for new work.

### Test Organization

Tests are co-located with the modules under test:

- **Session creation**: `config/gptel/sessions/test/commands/`
- **Chat-mode restore / save / menu**: `config/gptel/chat/test/menu/`

Test files (new or modified) for this change:

| File | Purpose |
|---|---|
| `config/gptel/sessions/test/commands/session-org-creation-spec.el` | Update — assert new layout (drawer + bare user block, no headings) and sibling-file emission |
| `config/gptel/sessions/test/commands/preset-application-spec.el` | Update — sibling file present after creation; no headings in `session.org` |
| `config/gptel/sessions/test/commands/sibling-system-prompt-file-spec.el` | **New** — focused tests for `jf/gptel--write-system-prompt-sibling-file` and `jf/gptel--preset-source-file-extension` |
| `config/gptel/chat/test/menu/preset-wiring-spec.el` | Update — drop heading-restore scenarios, add sibling-file restore scenarios; precedence sibling-file > legacy drawer > preset |
| `config/gptel/chat/test/menu/save-state-spec.el` | Update — drop heading-write scenarios; assert save never touches the sibling file; drawer still excludes `:GPTEL_SYSTEM:` |
| `config/gptel/chat/test/menu/system-prompt-file-spec.el` | **New** — focused tests for `gptel-chat--apply-system-prompt-file`, `gptel-chat--refresh-system-prompt-from-file`, `gptel-chat--edit-system-prompt-file` |
| `config/gptel/chat/test/menu/pre-send-refresh-spec.el` | **New** — modify sibling file on disk, simulate a send, assert dispatched system message reflects new content |

**Deletions:**
- Any heading-specific `it` blocks in `preset-wiring-spec.el` and `save-state-spec.el` (the make-system-prompt-heading-authoritative scaffolded scenarios and the materialise scenarios).
- The scaffolded invariants test at `openspec/changes/archive/2026-05-23-gptel-drawer-as-source-of-truth/scaffolding/invariants/system-prompt-heading-authoritative.test.el` is archived; the live tests under `config/gptel/chat/test/menu/` that mirror it should be removed.

**Kept:**
- The parser regression "parses to a single user turn even when the preset has a markdown system prompt" (`config/gptel/chat/test/parser/`) stays — heading-indifference is an independent contract worth pinning even when headings are gone today.
- `register/invariant/drawer-system-key-write-exclusion` and the test asserting "drawer never contains `:GPTEL_SYSTEM:`" stay.

### Naming Conventions

Per the existing pattern:
- Files: `*-spec.el`
- Top-level structure: `(describe "<scope>" (it "<behavior>" ...))`
- One `describe` per file when the file targets a single capability; nested `describe` for shared setup across related `it`s.

### Running Tests

```
./bin/run-tests.sh -d config/gptel/sessions/test/commands     # creation + sibling-file
./bin/run-tests.sh -d config/gptel/chat/test/menu             # restore + save + menu + refresh
./bin/run-tests.sh -d config/gptel/sessions                   # all session tests
./bin/run-tests.sh -d config/gptel/chat                       # all chat-mode tests
./bin/run-tests.sh -d config/gptel                            # all gptel tests
```

Snapshots (`--snapshot`) are available; not required for this change but useful when iterating on the new layout assertions.

### Test Patterns

Match the existing conventions in `config/gptel/chat/test/menu/` and `config/gptel/sessions/test/`:

- **Setup/teardown**: `before-each` writes a temp session directory (under `make-temp-file`-created prefix) with a synthetic `session.org` whose drawer carries the keys under test. `after-each` deletes the temp directory.
- **Mocks**: `spy-on` for upstream surfaces (`gptel--apply-preset`, `find-file-other-window`). Avoid mocking the chat-mode restore chain itself — the round-trip is the contract we want to assert.
- **Fixtures**: `config/gptel/chat/test/menu/helpers-spec.el` and `config/gptel/sessions/test/commands/helpers-spec.el` already exist for shared matchers and temp-buffer setup; extend rather than fork.
- **Assertions**: `expect ... :to-equal` for string round-trips; `:to-have-been-called-with` for menu-affordance file-opening; `file-exists-p` / `with-temp-buffer (insert-file-contents ...)` for sibling-file assertions.

### Scenario Mapping

Each scenario from the delta specs maps to one Buttercup `it` block:

| Spec scenario | Test |
|---|---|
| Restore reads sibling file when property and file are present | `system-prompt-file-spec.el` — `it "installs file body as buffer-local gptel--system-message"` |
| Restore falls back to preset when sibling file is absent | same — `it "no-ops when file missing, preset prevails"` |
| Restore falls back to preset when property is unset | same — `it "no-ops when property unset"` |
| Restore respects legacy drawer entry when no sibling file | `preset-wiring-spec.el` — `it "legacy :GPTEL_SYSTEM: applies in absence of sibling file"` (existing scenario, retained) |
| Sibling file wins over legacy drawer entry | `preset-wiring-spec.el` — `it "sibling file supersedes legacy drawer entry"` |
| Empty sibling file is a no-op | `system-prompt-file-spec.el` — `it "empty file falls through to preset"` |
| Pre-send refresh picks up file edit | `pre-send-refresh-spec.el` — `it "re-reads sibling file before dispatch"` |
| Save does not write the sibling file | `save-state-spec.el` — `it "leaves sibling file untouched on save"` |
| Save preserves sibling-file drawer link | `save-state-spec.el` — `it "preserves :GPTEL_SYSTEM_PROMPT_FILE: across save"` |
| Save does not emit `* System Prompt` heading | `save-state-spec.el` — `it "produces no * System Prompt heading"` |
| GPTEL_SYSTEM never written by the save path | `save-state-spec.el` — `it "saved drawer contains no :GPTEL_SYSTEM:"` (existing scenario) |
| Edit opens the resolved sibling file | `system-prompt-file-spec.el` — `it "opens sibling file in other window"` |
| Edit creates and opens a file when property is unset | `system-prompt-file-spec.el` — `it "creates sibling file and adds drawer property when absent"` |
| Fresh branch session.org carries drawer and bare user block | `session-org-creation-spec.el` — `it "emits drawer + bare user block with no headings"` |
| Fresh branch with system-prompt-bearing preset | `session-org-creation-spec.el` — `it "writes sibling file and drawer link"` |
| Fresh branch with no-system-prompt preset | `session-org-creation-spec.el` — `it "omits sibling file and drawer link"` |
| Sibling file extension mirrors preset source | `sibling-system-prompt-file-spec.el` — `it "uses preset file extension for sibling"` |
| Fresh agent session.org | `session-org-creation-spec.el` agent describe block — `it "agent emits drawer + bare user block with no headings"` |

## Dependencies

No new external libraries. Relies on what is already in-tree:
- `buttercup`
- `cl-lib`
- `org` / `org-element-properties` (for drawer parsing — already a dependency of chat-mode)
- `gptel-org` (for `gptel-org--entry-properties` — already loaded lazily by chat-mode save path)
- `jf/gptel-presets-directory` and the registered preset symbol set (for extension lookup)

## Constraints

- **No migration code.** Existing sessions on disk with `* System Prompt` / `* Chat` headings will not be auto-converted. The graceful degrade is "sibling file absent → preset wins" and stray headings sit in the file until the user removes them. The proposal calls this out explicitly; tests must not assume migration occurs.

- **No `:GPTEL_SYSTEM:` writes ever.** The drawer write-exclusion (`register/invariant/drawer-system-key-write-exclusion`, load-bearing) continues to hold. The sibling-file writer is a separate path; it composes with the drawer write without conflict.

- **No reading via `org-element-parse-buffer`.** Chat-mode parser Decision 1 forbids full-document parsing on a hot path. The sibling-file reader uses `insert-file-contents` into a temp buffer (or returns the string via `with-temp-buffer`); the `:GPTEL_SYSTEM_PROMPT_FILE:` drawer lookup uses `gptel-org--entry-properties` (already used elsewhere in the restore path).

- **Pre-send refresh must not block on file I/O.** The sibling file is small (a few KB at most for any plausible system prompt); a synchronous `insert-file-contents` is acceptable. If the file is absent, the refresh is a no-op — no error, no warning.

- **Tangle + check-parens after every `.org` edit.** `./bin/tangle-org.sh <file>` is the validator. No `.el`-only edits.

- **Extension lookup defaults defensively.** When `jf/gptel--preset-source-file-extension` cannot find the source file (preset registered but file deleted), return `"md"` rather than erroring — the writer will produce `system-prompt.md`, which matches the dominant convention and is unambiguous when the user goes to find it.
