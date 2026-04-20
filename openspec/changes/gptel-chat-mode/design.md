## Context

The proposal and architecture establish a new major mode (`gptel-chat-mode`) for sustained multi-turn chat/log usage, using symmetric `#+begin_user` / `#+begin_assistant` special blocks with nested `#+begin_tool` blocks, and `gptel-request` as the backend. This document records the concrete technical decisions needed to implement that architecture: how the parser walks the buffer, how per-request lifecycle is driven by upstream gptel's public FSM, how the streaming callback safely line-buffers and sanitizes incoming chunks, how the display layer is wired in without disturbing buffer content, and where the implementation intentionally leans on — or deliberately avoids — upstream gptel internals.

**State-machine taxonomy.** Three distinct concerns in this mode are each worth naming before they blur together:

| Concern | Mechanism | Lives where |
|---|---|---|
| Where in the request protocol are we? (`INIT` → `WAIT` → `TYPE` → {`TOOL` → `WAIT` \| `DONE` \| `ERRS`}) | Upstream `gptel-fsm` via `:fsm` + custom handlers | Decision 3 |
| How do we safely insert streaming text into the buffer line-by-line? | Per-request closure state (insertion marker, line holdback) | Decision 3b |
| Is the buffer currently well-formed turns? What is the next turn? | Parser walk (Decision 1), heading-aware but structure-first | Decision 1 |

Decisions 3, 3b, 10, and 11 collectively wire the mode onto upstream's FSM rather than paralleling it.

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

**Rationale:** we own the format. Turn-block delimiters are always paired, always begin at start-of-line with no leading whitespace (see Decision 14), and always use the exact delimiter strings we emit. A state-machine regex walk is deterministic, fast, and robust against the two edge cases (literal delimiters in block bodies, turns under headings). The parser's output is a structured turn list (per the architecture data contract), so any future replacement can preserve the same interface.

### Decision 2: Message list shape — match `gptel-request`'s documented `:prompt` contract

**Choice:** `gptel-chat--turns-to-messages` produces the shape `gptel-request` expects for `:prompt`: an ordered list where each element is either a `(role . content)` cons (for simple text turns) or a more structured plist for assistant turns containing tool calls. The exact shape follows upstream `gptel-request` documentation; we do not invent a parallel message protocol.

**Alternatives considered:**
- Emitting internal gptel message structs directly. Rejected: couples us to gptel internals.
- Always sending a single flat user message constructed by concatenating turns. Rejected: loses role distinction and prevents the model from distinguishing prior user vs. assistant turns in multi-turn context.

**Rationale:** `gptel-request` is the one public API we commit to. Shaping the message list to match its contract keeps the coupling one-directional and documented.

**Open**: the exact shape of tool-call / tool-result messages in the `:prompt` list needs verification against upstream gptel source during implementation. Listed in Open Questions.

### Decision 3: Request lifecycle — upstream `gptel-fsm` via `:fsm` and custom handlers

**Choice:** per-request lifecycle is driven by upstream gptel's public FSM abstraction. We pass `(gptel-make-fsm :handlers gptel-chat--fsm-handlers)` as `:fsm` to `gptel-request`. We do **not** build a parallel lifecycle state machine.

Upstream defines the states `INIT → WAIT → TYPE → {TOOL → WAIT | ERRS | DONE}` with a transitions table (`gptel-request--transitions`) and a handler alist (`gptel-request--handlers`) where each state maps to a list of functions invoked on entry — see `gptel-request.el:1570-1636`. Callers can supply augmented handlers; this is the public extension point. `gptel-request` returns the FSM, and `gptel-fsm-state` / `gptel-fsm-info` let any caller inspect it.

Our handlers chain **before** the upstream handlers — the pattern already used by `config/gptel/tools/persistent-agent.org`:

```elisp
(defvar gptel-chat--fsm-handlers
  `((WAIT ,#'gptel-chat--on-wait   ,#'gptel--handle-wait)
    (TYPE ,#'gptel-chat--on-type)
    (TOOL ,#'gptel-chat--on-tool   ,#'gptel--handle-tool-use)
    (DONE ,#'gptel-chat--on-done)
    (ERRS ,#'gptel-chat--on-errs)))

(gptel-request prompt
  :fsm      (gptel-make-fsm :handlers gptel-chat--fsm-handlers)
  :callback #'gptel-chat--stream-callback
  :stream   t)
```

Our handlers do UI-only work (update the display-layer overlay to reflect "waiting / streaming / tool-running / done / error"). Upstream's handlers continue to drive the actual protocol transitions — we neither fire `gptel--fsm-transition` directly nor replace the core handler list.

**Reference implementation:** `config/gptel/tools/persistent-agent.org` is the canonical worked example of this handler-augmentation pattern in this repo. Start there, not from upstream source.

**Alternatives considered:**
- Hand-rolled per-request closure with its own state enum (an earlier draft of this decision). Rejected: it would parallel, not integrate with, the upstream FSM — leaving us stranded when the upstream tool loop advances (`TOOL → WAIT` for multi-turn agentic tool use) and we are stuck re-deriving it.
- Ignoring the FSM entirely and relying only on `:callback`. Rejected: the callback exposes wire events (chunks, tool-call / tool-result cons cells, completion sentinels); it does not expose higher-level lifecycle state like "between tool calls during a multi-turn assistant turn." The FSM exposes that directly via `gptel-fsm-state`, which the display layer (Decision 5) and send-guard (Decision 11) both want.

**Rationale:** the FSM is the one durable lifecycle abstraction upstream gives us. Plugging in at the handler layer keeps our UI reactive to state changes while letting upstream own transition correctness, tool dispatch, and error propagation. If upstream adds new intermediate states (e.g., a `REASONING` phase), we inherit that for free.

### Decision 3b: Per-chunk text hygiene — closure with marker and line holdback

**Choice:** separate from the request FSM, the streaming `:callback` closure maintains a small amount of *text-processing* state for safe line-by-line insertion: an insertion marker for the active assistant block, a one-line holdback string, and (when a tool call is in flight) a second marker for the active `#+begin_tool` block.

This is not a state machine. It is a streaming text sanitizer. The separation matters because the two concerns have different lifetimes and different test surfaces:

| Concern | Mechanism |
|---|---|
| Where in the protocol are we? (`WAIT` / `TYPE` / `TOOL` / `DONE`) | Upstream FSM (Decision 3) |
| Where in the buffer are we inserting? What line is partially received? | Callback closure (this decision) |
| What is the line-level delimiter escape rule? | `gptel-chat--sanitize-chunk` (Decision 4) |

On each text chunk the callback:

1. Prepends any carry-over holdback to the new chunk.
2. Splits at `\n`; trailing partial line (if any) becomes the new holdback.
3. For each complete line, runs `gptel-chat--sanitize-chunk` (Decision 4) and inserts at the assistant insertion marker (or the tool-block marker, if one is active — Decision 10).
4. On completion (`t` from the callback — see Decision 10), flushes the holdback (cannot be a full `#+end_*` line by construction — no newline), inserts `#+end_assistant`, and positions point per Decision 8.

The markers are proper Emacs markers (not integer positions) so concurrent user edits above the insertion point don't corrupt them.

**Alternatives considered:**
- Insert directly at `(point-max)`. Fails when the buffer has content after the active assistant block (e.g., regenerate inserts into the middle of the buffer; the user edits below the cursor during stream).
- Buffer-local state variables. Harder to test, easier to leak between sends — and concurrent sends are disallowed anyway (Decision 11), so per-request closure scope is the right granularity.

**Rationale:** closures give per-send isolation and are directly testable by invoking the returned callback with scripted chunks. Markers are the standard Emacs idiom for position-tracking across edits. Line-buffered holdback is the well-known streaming-safe pattern for line-oriented sanitization (it prevents splitting a `#+end_...` collision across a chunk boundary — spec scenario "Response contains collision split across chunks").

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

### Decision 10: Callback dispatch — `pcase` on upstream's actual response shapes

**Choice:** the streaming callback receives `(response info)` from `gptel-request`. It dispatches via `pcase` on the documented response shapes. These are the shapes upstream emits at `gptel-curl--stream-filter` and `gptel--handle-tool-use` (see `gptel-request.el:1684-1752`) and that our own `config/gptel/tools/persistent-agent.org` already pattern-matches:

| Response | Case |
|---|---|
| string | text chunk — sanitize (Decision 4), line-buffer (Decision 3b), insert at active marker |
| `` `(reasoning . ,chunk) `` | v1: ignore. A future change may render these into a `#+begin_reasoning` nested block; out of scope for v1. |
| `` `(tool-call . ,calls) `` | open a nested `#+begin_tool (<name> :args <sexp>)` block inside the active assistant block; set the tool-block marker (Decision 3b) as the new insertion target |
| `` `(tool-result . ,results) `` | insert stringified result into the active tool block, append `#+end_tool`, clear the tool-block marker |
| `t` | normal completion — flush holdback, close `#+end_assistant`, append a fresh user block (Decision 8) |
| `nil` | error / network failure — close the block with an error marker |
| `'abort` | user abort — close the block with an interruption marker |

Each element of a `tool-call` or `tool-result` list is a plist carrying `:name`, `:args`, and — for results — `:result`. Our `#+begin_tool` opening line formats `:name` and `:args` as `(<name> :args <sexp>)` to match the existing session-file convention.

**Corrected from earlier draft:** an earlier version of this decision described tool events as "plists with a tool-call sentinel" — that was wrong. Upstream emits cons cells, not tagged plists; dispatch via `pcase` backquote patterns is both the upstream idiom and what `persistent-agent.org` already does.

**Note on transitioning the FSM:** `gptel--fsm-transition` is the mechanism by which handlers advance state. Our callback does not call it directly — the default `WAIT` and `TOOL` handlers (still present in our chained handler list, Decision 3) drive transitions for us. If a future UX wants to intercept (e.g., confirm-before-tool-call), the intercept goes in our `TOOL` handler, not in the callback.

**Reference implementation:** `config/gptel/tools/persistent-agent.org` calls `gptel-request` with full tool history and pattern-matches every shape above in a single `pcase`. Start there for both the callback dispatcher and the `:prompt` message shape with tool history (Open Question 1).

### Decision 11: Send-during-stream protection — via upstream FSM state, not a parallel flag

**Choice:** to detect an in-flight request, inspect the buffer-local `gptel--fsm-last` that upstream sets on every `gptel-request` (see `gptel.el:1104, 1328`). If it is non-nil and `(gptel-fsm-state gptel--fsm-last)` is not in `(DONE ERRS)`, signal a user-visible error. No queueing. We do **not** introduce a parallel `gptel-chat--stream-active-p` flag.

**Rationale:** streaming inserts at a fixed marker. A concurrent send would need a second marker and a second closure; the complexity isn't worth it for v1. Users waiting on a response have the obvious option of waiting.

Reusing `gptel--fsm-last` additionally buys us:

- **One source of truth.** A bug in upstream that leaves the FSM stuck in `TYPE` presents to us the same way it presents to every other gptel caller — we don't mask it with a separate flag that drifts out of sync.
- **State-aware policy.** We can decide send policy per state. A future UX might queue sends during `TOOL` (waiting on a tool result the user is confirming) but reject them during `TYPE` (actively streaming) — both are expressible with no additional bookkeeping.
- **No cleanup code to write on abort.** The FSM self-manages; we don't need an `unwind-protect` or error handler whose only job is to clear our flag.

**Trade-off:** we depend on `gptel--fsm-last` remaining the canonical handle for in-flight requests. It is marked internal by name convention but is used pervasively across upstream (rewrite mode, tool confirmation UI, send menus), which makes it de facto stable. If upstream renames it, our check is a one-line edit in one place.

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

### Decision 13: User blocks support the full org-mode editing experience

**Choice:** the body of a `#+begin_user` block is an unrestricted org editing surface. Users may compose prompts using org headings, source blocks, lists, tables, links, emphasis, footnotes, TODOs, timestamps, or any other org feature. The entire block body (verbatim, minus the delimiter lines) is sent as the user message.

**Rationale:** this is the principal ergonomic payoff of deriving from `org-mode` (Decision 6). A chat-mode buffer is intended to be used for sustained, structured prompt composition — the exact use case where org's editing affordances (outline cycling, source-block tangling, list demotion, table editing, link insertion) matter most. Forbidding any of these would push users back toward plain-text chat and defeat the reason for building a dedicated mode.

No implementation cost: the parser's state machine (Decision 1) is delimiter-matched, not structure-aware. From `#+begin_user` to the matching `#+end_user`, every line is body, whether it looks like a heading, a source block, another special block, or arbitrary prose.

**Implications:**

- A `* Heading` line inside a user block is part of the user message, not a document-level heading. The LLM receives `* Heading` as user text.
- A `#+begin_src ... #+end_src` block inside a user block is part of the user message. Org's inside-block syntax highlighting still works (a free consequence of org-mode derivation).
- Any line beginning with `#+begin_user`, `#+begin_assistant`, or `#+begin_tool` inside a user block is body text, not a new turn (the state machine stays in the "inside user block" state until the matching `#+end_user`).
- Bare `#+end_user` / `#+end_assistant` / `#+end_tool` lines inside a user block DO end the containing block — they are what the state machine matches. Users who need to include one of those literals in their prompt prefix it with `,` (org's own escape convention). The parser un-escapes `,#+end_...` on send, identically to the assistant-path round-trip in Decision 4.

**Follow-up (not v1):** a helper command (e.g., `gptel-chat-insert-literal-delimiter`) or a minor-mode indicator could make the `,`-escape convention discoverable for users who hit the edge case. Not worth the surface area for v1.

### Decision 14: Disable `org-adapt-indentation` locally; delimiters pinned to column 0

**Choice:** `gptel-chat-mode` sets `org-adapt-indentation` to `nil` as a buffer-local variable on activation. Turn-block delimiter lines always begin at the start of a line with no leading whitespace.

**Rationale:** org's adaptive indentation can, depending on the user's global setting, indent content under a heading to align with the heading's star column. For chat-mode that would mean turn blocks under deeper headings got indented — visually consistent with other org files, but it breaks the parser's `^#\+begin_...` anchor and complicates delimiter-collision sanitization (which would need to reason about prefix indentation). Pinning delimiters to column 0 regardless of the user's global org settings keeps the parser, the sanitizer, and the on-disk format uniform.

**Alternatives considered:**

- **Permissive parser** (accept leading whitespace on delimiter lines; regex becomes `^[ \t]*#\+begin_...`). Simple for the read path, but the write path (streaming insertion, assistant-block opening, sanitizer) would need to know what indentation to apply per line. Worse: two chat files authored under different `org-adapt-indentation` settings would have different on-disk shapes.
- **No local override** (respect the user's global `org-adapt-indentation`). Rejected as too fragile — a config change elsewhere could silently break every existing chat file.

**Trade-off:** chat-mode buffers don't match the indentation style of the user's other org files if they've chosen adaptive indentation globally. That's a minor cosmetic inconsistency and is acceptable in exchange for parser simplicity and on-disk uniformity.

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

1. **Exact `:prompt` shape for tool-call history in `gptel-request`.** The *callback* shape for tool events is settled (Decision 10: cons cells of plists). What still needs verification is the reverse direction — how tool-call / tool-result history is encoded in the `:prompt` value passed **into** `gptel-request` on subsequent turns, once our parser has reconstructed it from the buffer. Upstream's backend-specific `gptel--parse-buffer` methods build this shape today; we are bypassing them. `config/gptel/tools/persistent-agent.org` passes full tool history through `:prompt` and is the canonical worked example — extract the shape from there during implementation.

2. **Streaming callback protocol for tool events.** *Answered.* See Decision 10 for the full `pcase` table. The upstream emit sites are `gptel-curl--stream-filter` (text and reasoning chunks) and `gptel--handle-tool-use` (`gptel-request.el:1684-1752`, tool-call / tool-result cons cells).

3. **Debounce timing for display-layer refresh.** 100ms is a guess. Implementation should make this a `defcustom` (`gptel-chat-display-refresh-delay`) so we can tune after seeing real behavior.

4. **Does `auto-save-mode` cause problems during streaming?** Org's `auto-save-visited-file-name` may write partial buffers. Streaming inserts are append-mostly, so partial saves should be recoverable. Verify during manual testing; if problematic, disable auto-save while `gptel-chat--stream-active-p` is set.

5. **Should the display-toggle state persist per-buffer or globally?** v1 choice: per-buffer, via a buffer-local variable, with no persistence across Emacs restarts. If users want a global default, add a `defcustom` in a follow-up.

6. **Keybinding for new-chat command.** `gptel-chat-new` creates a buffer; there's no obvious key for "I'm not in a chat buffer yet." Probably leave as `M-x`-only for v1; users can add their own global binding.
