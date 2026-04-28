## Why

The current `PersistentAgent` tool is broken (calling it appears to hang) and has drifted into an isolated implementation that bypasses the codebase's session-init pipeline. It hand-rolls buffer creation, mode setup, scope.yml writing, auto-save, and a custom dual-duty `:callback`, all of which exist as shared infrastructure elsewhere in `config/gptel/`. As a result, agent files saved to disk cannot be reopened as interactive sessions — they don't carry the `:PROPERTIES:` drawer that auto-init expects, they're in `gptel-mode` rather than `gptel-chat-mode`, and the registry/symlink/autosave machinery never installs. Aligning the agent path with chat-mode's session pipeline fixes the hang, restores reload-as-interactive-session, and removes ~130 lines of reinvented infrastructure.

## What Changes

- **BREAKING**: `PersistentAgent` agent buffers now use `gptel-chat-mode` (not `gptel-mode`). Existing on-disk agent files written by the old implementation will not auto-init correctly when reopened — they are missing the `:PROPERTIES:` drawer and may use legacy formatting. Migration is out of scope; affected users should treat existing agent sessions as archive-only.
- Rebuild `jf/gptel-persistent-agent--task` on top of the standard session pipeline:
  - Create the agent session via `jf/gptel--create-session-core` (writes `:PROPERTIES:` drawer with `GPTEL_PRESET` and `GPTEL_PARENT_SESSION_ID`).
  - Open with `find-file` so `jf/gptel--auto-init-session-buffer` activates `gptel-chat-mode`, applies the preset from the drawer, registers the buffer, sets `jf/gptel-autosave-enabled`, and updates the `current` symlink.
  - Replace hand-written YAML with scope-module helpers for `scope.yml` creation.
- Promote the chat-mode send pipeline from `gptel-chat--`-prefixed internals to a documented public programmatic-send API (`gptel-chat-` prefix). The agent is the first non-interactive caller and shouldn't depend on internal-by-naming functions. New public surface: `gptel-chat-parse-buffer`, `gptel-chat-turns-to-messages`, `gptel-chat-open-assistant-block`, `gptel-chat-stream-callback`, `gptel-chat-fsm-handlers`.
- Replace the custom `:callback` with the chat-mode public stream callback plus an extended FSM handler alist. Our overlay-update handlers chain on `WAIT`/`TOOL`; new completion handlers chain on `DONE`/`ERRS`/`ABRT` to read the agent buffer's last assistant block, extract the trailing text segment (skipping tool blocks), and call `main-cb` with that text. Final text only — no streaming accumulator.
- Keep the existing local overlay helpers under `jf/gptel-persistent-agent--*` (`--hrule`, `--indicate-wait`, `--indicate-tool-call`, plus `--create-overlay` renamed to `--task-overlay`). Drop the static `--fsm-handlers` defvar in favour of a programmatic builder. (The upstream `gptel-agent` package was removed as a project dependency in commit `eebbc18`, Feb 27; the rebuild does not re-add it. See design.md §Decision 5.)
- **BREAKING**: Drop the unused `denied_paths` tool argument. The current implementation advertises it but ignores it.

## Capabilities

### New Capabilities

None. This change rebuilds and extends two existing capabilities.

### Modified Capabilities

- `gptel/persistent-agent`: Replace the standalone session-buffer construction with the standard chat-mode session pipeline. Specifies that an agent session is a regular `gptel-chat-mode` session distinguished only by (a) being created with an initial prompt and (b) returning the final assistant text to a caller-supplied callback. Removes `denied_paths` from the tool surface. Saved agent files are reloadable as interactive chat sessions. Zero-inheritance is now enforced via the drawer-declared preset, not via runtime ignore-the-parent logic.
- `gptel/chat-mode`: Add a public programmatic-send API. Promotes the parse → messages → open-assistant-block → stream-callback pipeline and the FSM handler alist from internal (`gptel-chat--`) names to public (`gptel-chat-`) names with documented contracts. Existing `gptel-chat-send` is rewritten to call the new public functions. The internal-prefixed names are removed (all in-tree callers updated in this change).

## Impact

**Affected code**:
- `config/gptel/tools/persistent-agent.{org,el}` — main rewrite (~170 → ~40 lines).
- `config/gptel/chat/parser.{org,el}` — rename `gptel-chat--parse-buffer` → `gptel-chat-parse-buffer`, `gptel-chat--turns-to-messages` → `gptel-chat-turns-to-messages`. Add docstring framing as public API.
- `config/gptel/chat/send.{org,el}` — rename `gptel-chat--open-assistant-block` → `gptel-chat-open-assistant-block`, `gptel-chat--fsm-handlers` → `gptel-chat-fsm-handlers`. Update `gptel-chat-send` to use public names.
- `config/gptel/chat/stream.{org,el}` — rename `gptel-chat--stream-callback` → `gptel-chat-stream-callback`. Add public-API docstring.
- All in-tree callers of the renamed symbols (chat module is the primary user).

**Affected tests**:
- `config/gptel/chat/test/` — update tests that reference renamed symbols.
- New tests for the public-API contract (parse → messages round-trip, send-callback shape, FSM handler composition).
- New tests for the rebuilt agent flow (session creation, drawer presence, reload-as-interactive, completion-handler buffer-extraction).

**Affected specs**:
- `openspec/specs/persistent-agent/spec.md` — major rewrite to reflect the chat-mode pipeline alignment.
- `openspec/specs/chat-mode/spec.md` — add public programmatic-send API section.

**Dependencies / external systems**:
- No new external dependencies. The upstream `gptel-agent` package is not re-added; the local overlay helpers stay (Decision 5). The change consumes only existing in-tree modules (chat, sessions, scope) plus core `gptel`.

**Migration**:
- Existing on-disk agent sessions will not auto-init under the new code (no drawer). Users who want to preserve them should convert by hand or accept them as archive. No automated migration script.
