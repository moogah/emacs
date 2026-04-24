## Why

The upstream `gptel-mode` is designed as an invisible integration layer — "assisted document editing" where LLM interaction blends into any buffer and is not meant to be structurally visible. That design is well-suited to its goal but is a poor fit for sustained multi-turn chat or work-log usage, where the sequence of turns *is* the artifact. In our sessions subsystem — where every session *is* a long-lived chat log — the upstream design produces three compounding friction points: model-emitted `* Heading` lines corrupt the outline and force manual turn-management, implicit prefix-based turn boundaries are brittle against freeform model output, and there is no first-class container for an assistant response (so no reliable folding, per-turn navigation, or opaque rendering). These are product-level mismatches, not bugs — upstream has explicitly declined structural wrapping on philosophical grounds (karthink, Discussion #321).

The sessions subsystem is where we feel this most acutely: every session is a chat/log. A mode that owns its conventions and targets that job directly is the cleaner path — and the sessions subsystem is the right place to adopt it. This change introduces `gptel-chat-mode` **and replaces `gptel-mode` with it throughout our session workflow**. Upstream `gptel-mode` remains available for its intended use (ad-hoc LLM assistance in arbitrary buffers); it is simply no longer the mode our sessions use.

## What Changes

- **New `gptel-chat-mode`** derived from `org-mode`, providing the structural chat/log container with first-class turn blocks. Replaces `gptel-mode` for session buffers; upstream `gptel-mode` is untouched and remains available for ad-hoc use elsewhere.
- **Symmetric special-block turn structure** as the canonical on-disk format: every turn is a `#+begin_user`/`#+end_user` or `#+begin_assistant`/`#+end_assistant` block whose delimiter lines start at column 0 (no leading whitespace), with `#+begin_tool` blocks nested inside assistant blocks for tool calls. Org headings are permitted as human organizational structure for long buffers but do not affect turn ordering or message construction.
- **Direct `gptel-request` backend**: the mode builds the message list by walking its own block structure; it does not rely on `gptel--parse-buffer` or on text-property-based turn tracking.
- **Streaming insertion with sanitization**: assistant response chunks are inserted inside an open `#+begin_assistant` block; lines that would collide with `#+end_*` delimiters are escaped using a targeted line-level regex (three delimiters only, case-insensitive).
- **Minimal interactive surface**: send current user turn, navigate to next/previous turn, regenerate last response. Prompt editing uses standard org-mode keys inside the user block.
- **Preset system and `gptel-menu` integration**: per-buffer configuration (model, backend, system message, tools) is delivered by the upstream preset mechanism via `gptel--apply-preset`; `gptel-menu` is supported for interactive configuration, with the menu's Send suffix rebound to the chat-mode send path.
- **Session filename and format changed**: session files are now `session.org` (chat-mode format) instead of `session.md` (markdown + upstream Local Variables). The sessions subsystem's auto-init, persistence, and branching code is updated to match.
- **Branching reimplemented on turn structure**: branch-point selection uses the chat-mode turn list (outer `#+begin_user` blocks) rather than `gptel` text properties; context truncation copies the buffer up to the Nth `#+end_assistant` boundary rather than filtering `gptel--bounds`.
- **Persistence simplified**: chat-mode's block format is self-describing. Sessions no longer need upstream's `gptel--save-state` to write Local Variables blocks; saving a chat-mode buffer is a plain `save-buffer`.
- **Clean break, no migration**: there is no automated migration from pre-existing `session.md` files to the new `session.org` format. Existing sessions remain on disk in their old format; they are not read by the new sessions code. Users who need to reference old sessions can continue to open them manually in upstream `gptel-mode`.
- **Optional display-layer overlays** (kept simple for v1): faces or line-prefixes to visually distinguish user vs. assistant regions without changing on-disk content. Delimiter-hiding overlays (display-asymmetric-while-storing-symmetric) are explicitly deferred as a later refinement.
- **No upstream gptel patches**: all behavior is downstream; `gptel-mode` itself is untouched.

## Capabilities

### New Capabilities

- `gptel-chat-mode`: A dedicated major mode for multi-turn chat/work-log interaction with an LLM. Covers buffer structure (symmetric special-block turns with nested tool blocks), message construction from buffer state, streaming response insertion with delimiter sanitization, mode-provided navigation and send commands, and a minimal display layer for role distinction. Integrates with the upstream preset system and `gptel-menu`. Serves as the canonical mode for session buffers in the sessions subsystem.

### Modified Capabilities

- `sessions-persistence`: session files are now `session.org` in chat-mode format; auto-init enables `gptel-chat-mode` instead of `gptel-mode`; save persistence is a plain `save-buffer` (no `gptel--bounds` Local Variables). Preset application path is unchanged (`gptel--apply-preset` with buffer-local setter, driven by `metadata.yml`).
- `sessions-branching`: branch-point selection enumerates outer `#+begin_user` blocks via the chat-mode parser; context truncation copies the buffer up to the Nth `#+end_assistant` boundary. The `gptel--bounds`-filtering requirement is removed (bounds do not exist in chat-mode format). Registry, directory structure, and `current` symlink behavior are unchanged.

### Untouched Capabilities

- `preset-registration`, `scope`, `scope-profiles`, `scope-expansion`, `persistent-agent` — all continue to work; chat-mode consumes the preset system the same way any `gptel-request` caller does.

## Impact

**New code:**
- New module directory `config/gptel/chat/` with the mode definition, block parser, send/receive pipeline, display overlays, navigation commands, preset wiring, and menu integration
- Added to `jf/enabled-modules` load list (before the sessions modules, since they now depend on it)

**Modified code:**
- `config/gptel/sessions/commands.org` — auto-init hook enables `gptel-chat-mode` and keys on `session.org` path; `jf/gptel-persistent-session` creates `session.org` with chat-mode initial content
- `config/gptel/sessions/filesystem.org` — directory templates use `session.org`
- `config/gptel/sessions/branching.org` — branch-point selection and context truncation rewritten against chat-mode turn list
- `config/gptel/sessions/activities-integration.org` — activity-backed session creation emits `session.org` unconditionally
- `config/gptel/sessions/metadata.org` — no schema change; the `mode` field is implicit (sessions are chat-mode by definition)

**Dependencies:**
- Consumes `gptel-request`, `gptel-make-fsm`, `gptel--apply-preset` (public/stable API)
- Reuses `gptel-menu` (transient prefix) for configuration, with Send suffix rebound
- Reuses standard `hack-local-variables` and org property-drawer parsing for per-file config
- No new external package dependencies

**User impact:**
- **Breaking**: sessions created with pre-chat-mode code (expecting `session.md` + `gptel-mode`) will not be recognized by the new session commands. Users who want to continue referring to old sessions open them manually in upstream `gptel-mode`; the new session code only reads `session.org`.
- `gptel-mode` itself is untouched — users can still use it for ad-hoc LLM assistance in arbitrary buffers.
- All session workflows (create, open, branch, resume via activities) continue to work the same way from the user's perspective, with the new format applied to new sessions.

**Out of scope (deferred to later changes):**
- Automated migration of existing `session.md` files to `session.org` (clean break is the explicit choice; see design.md)
- Delimiter-hiding display layer (store-symmetric-display-asymmetric)
- `org-edit-special`-style indirect buffer for prompt editing
- Per-chat preset selection UI beyond what `gptel-menu` already provides
