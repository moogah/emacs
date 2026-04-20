## Why

The upstream `gptel-mode` is designed as an invisible integration layer — "assisted document editing" where LLM interaction blends into any buffer and is not meant to be structurally visible. That design is well-suited to its goal but is a poor fit for sustained multi-turn chat or work-log usage, where the sequence of turns *is* the artifact. In that usage, the current model produces three compounding friction points: model-emitted `* Heading` lines corrupt the outline and force manual turn-management, implicit prefix-based turn boundaries are brittle against freeform model output, and there is no first-class container for an assistant response (so no reliable folding, per-turn navigation, or opaque rendering). These are product-level mismatches, not bugs — upstream has explicitly declined structural wrapping on philosophical grounds (karthink, Discussion #321). A separate mode that owns its conventions and targets the chat/log job directly is the cleaner path.

## What Changes

- **New `gptel-chat-mode`** derived from `org-mode`, targeting dedicated multi-turn chat/work-log buffers. Parallel to `gptel-mode`, not a replacement.
- **Symmetric special-block turn structure** as the canonical on-disk format: every turn is a `#+begin_user`/`#+end_user` or `#+begin_assistant`/`#+end_assistant` block, flat (no outer heading hierarchy), with `#+begin_tool` blocks nested inside assistant blocks for tool calls.
- **Direct `gptel-request` backend**: the mode builds the message list by walking its own block structure; it does not rely on `gptel--parse-buffer` or on text-property-based turn tracking.
- **Streaming insertion with sanitization**: assistant response chunks are inserted inside an open `#+begin_assistant` block; lines that would collide with `#+end_*` delimiters are escaped using the existing `org-escape-code-in-string` pattern already used for tool blocks.
- **Minimal interactive surface**: send current user turn, navigate to next/previous turn, regenerate last response. Prompt editing uses standard org-mode keys inside the user block.
- **Optional display-layer overlays** (kept simple for v1): faces or line-prefixes to visually distinguish user vs. assistant regions without changing on-disk content. Delimiter-hiding overlays (display-asymmetric-while-storing-symmetric) are explicitly deferred as a later refinement.
- **Standalone persistence**: the mode reads and writes `.org` files in its own block format. Integration with the existing sessions/branching/activities infrastructure is explicitly **out of scope** for this change and will be tackled in a follow-up.
- **No upstream gptel patches**: all behavior is downstream; `gptel-mode` is untouched.

## Capabilities

### New Capabilities

- `gptel-chat-mode`: A dedicated major mode for multi-turn chat/work-log interaction with an LLM. Covers buffer structure (symmetric special-block turns with nested tool blocks), message construction from buffer state, streaming response insertion with delimiter sanitization, mode-provided navigation and send commands, and a minimal display layer for role distinction.

### Modified Capabilities

None. Existing capabilities (`sessions-persistence`, `sessions-branching`, `preset-registration`, etc.) are untouched; chat mode operates on its own files and uses `gptel-request` as a backend.

## Impact

**New code:**
- New module directory under `config/gptel/chat/` (or similar) containing the mode definition, block parser, send/receive pipeline, display overlays, and navigation commands
- Added to `jf/enabled-modules` load list

**Dependencies:**
- Consumes `gptel-request` (public API, stable)
- Reuses `org-escape-code-in-string` (upstream org) for delimiter sanitization
- No new external package dependencies

**User impact:**
- New mode available for opt-in use on designated chat files
- Existing `gptel-mode` workflows and all existing session files are unaffected
- No migration required; users adopt incrementally by opening new files in `gptel-chat-mode`

**Out of scope (deferred):**
- Migrating existing `session.org` files to the chat-mode block format
- Integration with the existing session branching/activities/scope infrastructure
- Display-layer delimiter hiding (store-symmetric-display-asymmetric)
- `org-edit-special`-style indirect buffer for prompt editing
- Per-chat preset selection UI
