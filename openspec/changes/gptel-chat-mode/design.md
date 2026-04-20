## Context

The proposal and architecture establish a new major mode (`gptel-chat-mode`) for sustained multi-turn chat/log usage, using symmetric `#+begin_user` / `#+begin_assistant` special blocks with nested `#+begin_tool` blocks, and `gptel-request` as the backend. This document records the concrete technical decisions needed to implement that architecture: how the parser walks the buffer, how the streaming state machine handles partial-line holdback and delimiter sanitization, how the display layer is wired in without disturbing buffer content, and where the implementation intentionally leans on — or deliberately avoids — upstream gptel internals.

Reference:
- `proposal.md` — motivation (gptel-mode vs. chat/log-mode mismatch; upstream's stated invisible-integration philosophy)
- `specs/gptel-chat-mode/spec.md` — 10 testable requirements, every scenario mapped to at least one Buttercup `it` block
- `architecture.md` — six-module breakdown, Buttercup test layout, `gptel-request` stubbed synchronously with `cl-letf`

## Goals / Non-Goals

**Goals:**
- Make the chat-mode buffer the single source of truth. Re-opening a `.org` file and walking its blocks is sufficient to reconstruct conversation state; no sidecar property drawers or bounds tracking required.
- Keep the implementation testable in isolation. Each of the six modules exposes a small public surface with narrow dependencies; the parser and the stream sanitizer can be tested without any Emacs UI state, and send can be tested without a real network call.
- Isolate coupling to upstream `gptel`. Only `gptel-request` is a load-bearing dependency. No reliance on `gptel--parse-buffer`, `gptel-prompt-prefix-alist`, `:GPTEL_BOUNDS:`, or other internals that may change without notice.
- Ship a v1 that a human can use for an hour without hitting a structural bug. Completeness matters more than polish; the display layer is intentionally minimal.

**Non-Goals:**
- Integration with `config/gptel/sessions/` infrastructure (branching, activities, metadata.yml, scope.yml). Chat-mode files are plain `.org` files in v1.
- Automatic migration of existing `session.md` / `session.org` files into the chat format.
- Delimiter-hiding display layer ("store-symmetric, render-asymmetric"). Deferred to a later change.
- `org-edit-special`-style indirect buffer for prompt editing. Users edit prompts in place with standard org keys.
- Per-chat preset/model selection UI. Users control `gptel-model` / `gptel-backend` via existing mechanisms.
- Upstream patches to `gptel`.

## Decisions

### Decision 1: Parser — state-machine regex walk, not `org-element-parse-buffer`

**Choice:** hand-rolled `re-search-forward` parser that walks the buffer with a small state machine tracking whether the scan is currently inside an outer turn block. When outside, it looks for `^#\+begin_\(user\|assistant\)`; once inside, it looks only for the matching `#+end_*` (and, inside assistant blocks, for nested `#+begin_tool` pairs). Content outside turn blocks — headings, paragraphs, drawers, `#+keyword:` lines, blank lines — is skipped without interpretation. Case-insensitive (`case-fold-search t`) to match org's own case handling for block delimiters.

The state machine disambiguates two senses of "top-level" that a naive scan conflates:

- **Sense A** (structural, the one we enforce): a turn block is not nested inside another user or assistant block.
- **Sense B** (outline, irrelevant): a turn block is at document root, outside any heading section.

Decision 12 establishes that headings are organizational and do not affect turn ordering, so only Sense A matters. The state machine also naturally resists false positives: a user block whose body contains the literal line `#+begin_assistant` is not misread as a second turn, because the scanner is in the "inside user block" state until the matching `#+end_user`.

**Alternatives considered:**
- `org-element-parse-buffer`: allocates a full AST of the buffer on every parse. Correct but expensive (10-100ms on large logs) and couples tests and behavior to org-mode's internal element representation. We'd need to unwrap `special-block` / `src-block` / `paragraph` nodes to reconstruct content anyway.
- Per-buffer text-property tracking (what upstream gptel does): requires maintaining properties across edits and persisting them in drawers. We rejected this in the proposal — the spec explicitly prohibits text-property or `:GPTEL_BOUNDS:` dependence.
- Stateless line-by-line scan (early draft of this design): depended on "all turn blocks are at document top level." Fails as soon as a user block body mentions `#+begin_assistant`, and fails more broadly once heading-organized buffers are in scope (Decision 12).

**Rationale:** we own the format. Blocks are always paired, always at column 0, always the exact delimiters we emit. A state-machine regex walk is deterministic, fast, and robust against the two edge cases (literal delimiters in block bodies, turns under headings). The parser's output is a structured turn list (per the architecture data contract), so any future replacement can preserve the same interface.

### Decision 2: Message list shape — match `gptel-request`'s documented `:prompt` contract

**Choice:** `gptel-chat--turns-to-messages` produces the shape `gptel-request` expects for `:prompt`: an ordered list where each element is either a `(role . content)` cons (for simple text turns) or a more structured plist for assistant turns containing tool calls. The exact shape follows upstream `gptel-request` documentation; we do not invent a parallel message protocol.

**Alternatives considered:**
- Emitting internal gptel message structs directly. Rejected: couples us to gptel internals.
- Always sending a single flat user message constructed by concatenating turns. Rejected: loses role distinction and prevents the model from distinguishing prior user vs. assistant turns in multi-turn context.

**Rationale:** `gptel-request` is the one public API we commit to. Shaping the message list to match its contract keeps the coupling one-directional and documented.

**Open**: the exact shape of tool-call / tool-result messages in the `:prompt` list needs verification against upstream gptel source during implementation. Listed in Open Questions.

### Decision 3: Streaming state machine — closure with markers and a one-line holdback

**Choice:** the streaming callback is a closure (captured on send) carrying a state plist: `(:buffer :insertion-marker :holdback-string :active-tool-marker :aborted)`. The marker is a proper Emacs marker (not an integer position), so concurrent user edits above the insertion point don't corrupt it. On each chunk:

1. Prepend any previous holdback to the new chunk.
2. Split at `\n`; the trailing partial line (if any) becomes the new holdback.
3. For each complete line, apply delimiter sanitization (Decision 4) and insert at the marker.
4. On stream completion, flush any holdback (cannot be a full `#+end_...` line because it has no newline), insert `#+end_assistant`, and position point for the next turn (Decision 8).
5. On abort, insert an interruption marker line (`,#+end_assistant  [interrupted]` or equivalent), close the block, and clear closure state.

**Alternatives considered:**
- Append directly at `(point-max)`: naive; fails if the user moves point or if streaming happens into a not-at-end buffer (e.g., regenerate inserts into the middle).
- Buffer-local state variables: harder to test, easier to leak between sends. Closure state is per-request by construction.

**Rationale:** closures keep per-send state isolated and testable. Markers are the standard Emacs idiom for tracking buffer positions across edits. Line-buffered holdback is the well-known streaming-safe pattern for line-oriented sanitization (prevents splitting a collision across chunks).

### Decision 4: Sanitization — targeted scanner for our three delimiters, case-insensitive

**Choice:** `gptel-chat--sanitize-chunk` uses a line-level regex: if a line matches `\\`^#\\+end_\\(user\\|assistant\\|tool\\)\\b` (case-fold), prepend `,`. Nothing else is escaped; the content of an assistant block is not otherwise mutated.

**Alternatives considered:**
- `org-escape-code-in-string`: upstream gptel's tool-result rendering uses this. It escapes all org directive-looking lines, including `* Heading`, `#+begin_src`, etc. Correct but over-broad — we don't need to escape headings (special blocks already absorb them) or other block delimiters (they're orthogonal to our three). Using it would clutter assistant content with extra commas on benign lines.
- Encoding to a unicode-safe form (zero-width joiner in the delimiter): considered and rejected in earlier exploration. Fragile under file normalization.

**Rationale:** minimal output perturbation. Only the three collisions we actually care about are escaped; all other content passes through untouched. On read-back, `gptel-chat--turns-to-messages` strips leading commas from lines matching the same pattern — a one-liner inverse.

The exact regex is case-insensitive because org's block-end matching is case-insensitive (`#+END_ASSISTANT` terminates `#+begin_assistant`).

### Decision 5: Display layer — overlays, refreshed on change

**Choice:** the display layer uses **overlays** (not font-lock) applied to block content ranges. v1 applies a single subtle face (user vs. assistant) via the overlay's `face` property. Overlays are added/removed by a block-boundary scanner registered on `after-change-functions` with a short idle timer for debouncing, plus a one-shot initial pass on mode activation.

**Alternatives considered:**
- Font-lock keywords: integrates with org's font-lock pipeline but is awkward for multi-line block content (would need a state machine in `font-lock-extend-after-change-region-function`), and font-lock re-fontification during streaming is wasteful.
- Line-prefix overlays (gutter symbols like `▸` per user line): discussed in exploration. Adds visual noise and cursor-column confusion. Deferred.
- `display` property text overlays: reserved for the deferred delimiter-hiding refinement.

**Rationale:** overlays with `face` are the least invasive mechanism and cleanly separate from buffer content. The debounced after-change refresh handles streaming without churning on every chunk. The display layer is cleanly toggleable — `gptel-chat-toggle-display-layer` just enables/disables the refresher and removes existing overlays.

### Decision 6: Mode derivation — derive from `org-mode`, override minimally

**Choice:** `(define-derived-mode gptel-chat-mode org-mode ...)`. No parent overrides for indentation, fontification, or folding. The only org-mode behaviors we touch are the keymap entries we rebind (Decision 7).

**Rationale:** we get `#+begin_src` syntax highlighting, block folding, and all standard org editing for free inside user and assistant blocks. This is the key reason for choosing special blocks in the first place — they inherit all of org's inside-block machinery. Deriving from `text-mode` or defining a standalone mode would forfeit this.

### Decision 7: Keybindings — small, opinionated, non-shadowing

**Choice:**

| Key | Command | Notes |
|-----|---------|-------|
| `C-c C-c` | `gptel-chat-send` | Matches `gptel-mode` muscle memory |
| `C-c n` | `gptel-chat-next-turn` | Single-`C-c` prefix; does not shadow org heading nav |
| `C-c p` | `gptel-chat-previous-turn` | Single-`C-c` prefix; does not shadow org heading nav |
| `C-c C-r` | `gptel-chat-regenerate` | |
| `C-c C-t` | `gptel-chat-toggle-display-layer` | |

**Rationale:** because chat-mode buffers MAY use org headings for organizational structure (Decision 12), shadowing `C-c C-n` / `C-c C-p` would remove useful heading navigation. The single-`C-c` prefix is conventional for downstream/minor-mode bindings, so binding turn nav to `C-c n` / `C-c p` keeps both navigation systems available side by side.

**Trade-off:** turn-nav and heading-nav use parallel but distinct bindings; users developing muscle memory across `gptel-mode` and `gptel-chat-mode` must remember this. Users who prefer to shadow `C-c C-n` / `C-c C-p` can do so in their own config — chat-mode doesn't prescribe it.

### Decision 8: Shell-like append flow after response completes

**Choice:** on stream completion, insert a new empty `#+begin_user`/`#+end_user` block after the closing `#+end_assistant`, move point inside it, and leave the display layer to refresh.

**Alternatives considered:**
- Leave point after `#+end_assistant`, require user to type `#+begin_user` manually or press a "new turn" key. More explicit; more friction.
- Only auto-insert if point was "at the end" before streaming started. Too clever; surprising when it does nothing.

**Rationale:** matches the shell-like ergonomics that motivated the mode. The cost (one extra block pair on disk) is trivial. Users who want to regenerate or go back to edit a past turn can still do so with `gptel-chat-regenerate` or normal navigation.

### Decision 9: New-chat initialization

**Choice:** `gptel-chat-new` command creates a fresh buffer in `gptel-chat-mode` with the initial content:

```
#+begin_user

#+end_user
```

Point is positioned on the empty line inside the block. No metadata is pre-populated; users who want `#+gptel-model:` or `#+gptel-system:` keywords add them manually.

**Rationale:** minimum viable initialization. Metadata keywords are a v2 concern.

### Decision 10: Tool-call callback handling

**Choice:** the streaming callback pattern-matches the callback argument:
- String → text insertion with line-level sanitization (Decisions 3, 4)
- Plist with a tool-call sentinel → open a `#+begin_tool` block inside the assistant block, formatted as `#+begin_tool (<name> :args <sexp>)\n(:name ... :args ...)\n` to match the existing session-file convention shown in the user's test fixture
- Plist with a tool-result sentinel → insert the result (stringified) into the active tool block, then `\n#+end_tool\n`
- Completion sentinel → Decision 3 finalization

**Open:** the exact sentinel/plist shape gptel-request passes to the callback for tool events needs verification during implementation. The pattern-matching dispatcher is small (one `cond` form), so adapting to the actual protocol is a narrow change.

**Reference implementation to study:** `config/gptel/tools/persistent-agent.el` already calls `gptel-request` with full tool history and handles streaming tool-call events inside a ~300-line callback closure. That file is the canonical worked example for both the `:prompt` message shape with tool history (Open Question 1) and the streaming tool-event protocol (Open Question 2). Implementation should start there rather than reading upstream gptel source cold.

### Decision 11: Send-during-stream is an error

**Choice:** invoking `gptel-chat-send` while a stream is in progress (detected via a buffer-local `gptel-chat--stream-active-p` flag) signals a user-visible error. No queueing.

**Rationale:** streaming inserts at a fixed marker. A concurrent send would need a second marker and a second closure; the complexity isn't worth it for v1. Users waiting on a response have the obvious option of waiting.

### Decision 12: Heading-allowed buffer structure — "blocks-only" model

**Choice:** org headings (at any depth) are permitted in chat-mode buffers as organizational/commentary structure for the human reader. They do NOT participate in message construction. The LLM sees only the content of outer `#+begin_user` / `#+begin_assistant` blocks and their nested `#+begin_tool` blocks, in document order. Prose, drawers, and `#+keyword:` lines outside turn blocks are similarly invisible to the model.

This is the **blocks-only model**, chosen from three candidates considered in exploration:

- **(A) Blocks-only** — CHOSEN. Headings are human-only affordances (outline-cycle, sparse-tree, imenu, agenda). Model sees the turn sequence, nothing else.
- **(B) Headings-as-context** — rejected. Would inject heading text into the model's view as implicit system/context messages. Too much interpretation surface area for v1; raises ordering questions (does a heading attach to the following turn, the section, both?) that blocks-only avoids entirely.
- **(C) Section-as-conversation** — rejected. Each top-level heading would become an independent thread with its own history. A different product, not a refinement of the current design.

**Rationale:** preserves the invariant "the buffer is the source of truth; turns are the message list" while letting users organize long chat logs with familiar org tooling. The parser's state-machine walk (Decision 1) is indifferent to heading depth, so the implementation cost of this decision is effectively zero — it is primarily a specification/intent choice.

**Implications:**

- Turn navigation (Decision 7) and org heading navigation are complementary, not competing — reflected in the non-shadowing keybinding choice.
- Text inside a user or assistant block is sent verbatim to the LLM, including lines that look like org headings or `#+begin_*` delimiters. Heading-like prose inside a user block becomes part of that user message — if the user writes `* Context` inside their prompt, the model sees `* Context` as user text.
- Validation (spec §Buffer format validation) is narrower than an earlier draft's "all content outside blocks is invalid": the parser accepts any content between turn blocks and flags only structural problems (unmatched delimiters, tool-block-outside-assistant, turn-inside-turn).
- Future scope (out of v1): a later change could add heading-aware features — per-section chat history branching, heading-to-system-message promotion, section-scoped regenerate — without changing the v1 on-disk format. The turn-list contract is stable either way.

## Risks / Trade-offs

**[Risk]** `gptel-request` tool-call callback protocol differs from what we assume → **Mitigation:** the dispatcher in the stream module is the single integration point. We can adjust pattern-matching in one place. Decision 10's "open" flag reflects this.

**[Risk]** Case-insensitive sanitization misses an unusual collision form (e.g., model emits `#+End_Assistant` with odd capitalization, or includes zero-width characters between `end_` and `assistant`) → **Mitigation:** the regex covers org's own matching behavior (case-insensitive, `\b` word-boundary). Exotic cases are low-probability; if they occur, the block parser will fail on next send with a clear error identifying the offending line.

**[Risk]** Display-layer overlay refresh during streaming creates visual flicker → **Mitigation:** debounced with `run-with-idle-timer` (100-200ms). If flicker is still noticeable, refresh only on stream completion, not per chunk.

**[Risk]** Shadowing `C-c C-n`/`C-c C-p` from org-mode surprises users who switch between `org-mode` and `gptel-chat-mode` buffers → **Mitigation:** document clearly in the mode commentary; provide the opt-out helper noted in Decision 7.

**[Risk]** Tool-call handling assumes a specific `#+begin_tool` format in history replay → **Mitigation:** the existing session format already uses exactly this form; parser and stream both use the same code path, so round-trip consistency is enforced by construction.

**[Risk]** Performance degradation on very long logs (hundreds of turns) → **Mitigation:** regex parser is linear in buffer size; overlay scanner is incremental via `after-change-functions`. If profiling shows issues, the parser can cache the last turn-list and invalidate only the tail on edit.

**[Risk]** No upstream-compatibility path if we later want sessions integration → **Mitigation:** the parser emits a structured turn list as the contract boundary (per architecture data contract). A session-persistence module consumes that same turn list without touching the buffer format.

## Migration Plan

Not applicable. This is a new, additive mode:

1. Land the `config/gptel/chat/` modules behind an entry in `jf/enabled-modules` (opt-in).
2. User activates `gptel-chat-mode` on a buffer manually (or via `M-x gptel-chat-new`) to adopt.
3. No existing files are modified. Existing `gptel-mode` workflows continue to function.

Rollback: remove the entry from `jf/enabled-modules`. Any chat-mode files on disk are still readable as plain `.org` files; the block structure is fully self-describing.

## Open Questions

1. **Exact `:prompt` shape for tool-call history in `gptel-request`.** Upstream gptel constructs tool-call messages via its backend-specific `gptel--parse-buffer` methods. We need to confirm the canonical message shape `gptel-request` accepts for `:prompt` when the history contains prior tool calls. Verify by reading `gptel-request` documentation and the OpenAI/Anthropic backend implementations during implementation.

2. **Streaming callback protocol for tool events.** The stream callback needs to distinguish text chunks, tool-call events, tool-result events, and completion. The explore-agent survey noted that gptel uses pattern-matching in `gptel--insert-response` (gptel.el:1471+) and `gptel-curl--stream-insert-response` (gptel.el:1537+). Read these to extract the protocol shape, or mirror the dispatcher pattern if the callback interface exposes it directly.

3. **Debounce timing for display-layer refresh.** 100ms is a guess. Implementation should make this a `defcustom` (`gptel-chat-display-refresh-delay`) so we can tune after seeing real behavior.

4. **Does `auto-save-mode` cause problems during streaming?** Org's `auto-save-visited-file-name` may write partial buffers. Streaming inserts are append-mostly, so partial saves should be recoverable. Verify during manual testing; if problematic, disable auto-save while `gptel-chat--stream-active-p` is set.

5. **Should the display-toggle state persist per-buffer or globally?** v1 choice: per-buffer, via a buffer-local variable, with no persistence across Emacs restarts. If users want a global default, add a `defcustom` in a follow-up.

6. **Keybinding for new-chat command.** `gptel-chat-new` creates a buffer; there's no obvious key for "I'm not in a chat buffer yet." Probably leave as `M-x`-only for v1; users can add their own global binding.
