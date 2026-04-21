## Context

The proposal and architecture establish a new major mode (`gptel-chat-mode`) for sustained multi-turn chat/log usage, using symmetric `#+begin_user` / `#+begin_assistant` special blocks with nested `#+begin_tool` blocks, and `gptel-request` as the backend. This document records the concrete technical decisions needed to implement that architecture: how the parser walks the buffer, how per-request lifecycle is driven by upstream gptel's public FSM, how the streaming callback safely line-buffers and sanitizes incoming chunks, how the display layer is wired in without disturbing buffer content, and where the implementation intentionally leans on — or deliberately avoids — upstream gptel internals.

**State-machine taxonomy.** Three distinct concerns in this mode are each worth naming before they blur together:

| Concern | Mechanism | Lives where |
|---|---|---|
| Where in the request protocol are we? (`INIT` → `WAIT` → `TYPE` → {`TOOL` → `WAIT` \| `DONE` \| `ERRS` \| `ABRT`}) | Upstream `gptel-fsm` via `:fsm` + custom handlers | Decision 3 |
| How do we safely insert streaming text into the buffer line-by-line? | Per-request closure state (insertion marker, line holdback) | Decision 3b |
| Is the buffer currently well-formed turns? What is the next turn? | Parser walk (Decision 1), heading-aware but structure-first | Decision 1 |

Decisions 3, 3b, 10, and 11 collectively wire the mode onto upstream's FSM rather than paralleling it.

Reference:
- `proposal.md` — motivation (gptel-mode vs. chat/log-mode mismatch; upstream's stated invisible-integration philosophy)
- `specs/gptel-chat-mode/spec.md` — 10 testable requirements, every scenario mapped to at least one Buttercup `it` block
- `architecture.md` — seven-module breakdown, Buttercup test layout, `gptel-request` stubbed synchronously with `cl-letf`

## Goals / Non-Goals

**Goals:**
- Make the chat-mode buffer the single source of truth. Re-opening a `.org` file and walking its blocks is sufficient to reconstruct conversation state; no sidecar property drawers or bounds tracking required.
- Replace `gptel-mode` with `gptel-chat-mode` throughout our sessions subsystem as part of this change — not a separate follow-up. Sessions are where the chat/log ergonomics matter most; splitting the work leaves two parsers and two save paths coexisting in a way that's more expensive than the clean-break rewrite.
- Keep the implementation testable in isolation. Each of the chat-mode modules exposes a small public surface with narrow dependencies; the parser and the stream sanitizer can be tested without any Emacs UI state, and send can be tested without a real network call. Session-integration seams (auto-init, branching) are tested with fixtures mounted on tempdir directory structures.
- Isolate coupling to upstream `gptel`. The load-bearing surface is `gptel-request`, `gptel-make-fsm`, `gptel--apply-preset`, and `gptel-menu` — all documented or de-facto-stable public API. No reliance on `gptel--parse-buffer`, `gptel-prompt-prefix-alist`, `:GPTEL_BOUNDS:`, or gptel-mode's text-property conventions.
- Ship a v1 that a human can use for an hour (including creating sessions, branching, and resuming via activities) without hitting a structural bug. Completeness matters more than polish; the display layer is intentionally minimal.

**Non-Goals:**
- Automated migration of existing pre-chat-mode sessions (see Decision 19). Clean break: old `session.md` files remain on disk in their old format; the new sessions code does not read them.
- Coexistence of `gptel-mode` and `gptel-chat-mode` inside a single session buffer. Session buffers use chat-mode *instead of* gptel-mode — the two modes do not run together (see Decision 16).
- Delimiter-hiding display layer ("store-symmetric, render-asymmetric"). Deferred to a later change.
- `org-edit-special`-style indirect buffer for prompt editing. Users edit prompts in place with standard org keys.
- A new per-chat preset/model selection UI. **Provided by upstream `gptel-menu`** — see Decision 15.
- Upstream patches to `gptel`. (We patch our own sessions modules, not upstream.)

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

Upstream defines the states `INIT → WAIT → TYPE → {TOOL → WAIT | ERRS | DONE | ABRT}` with a transitions table (`gptel-request--transitions`) and a handler alist (`gptel-request--handlers`) where each state maps to a list of functions invoked on entry — see `gptel-request.el:1570-1636`. Callers can supply augmented handlers; this is the public extension point. `gptel-request` returns the FSM, and `gptel-fsm-state` / `gptel-fsm-info` let any caller inspect it.

**Full state taxonomy observed in chat-mode's handler alist:**

| State | Meaning | Upstream handler we chain |
|---|---|---|
| `WAIT` | Request sent; awaiting first byte | `gptel--handle-wait` (fires the network request) |
| `TYPE` | Response body streaming in | *(none — curl/url pipeline advances)* |
| `TOOL` | Running tool calls | `gptel--handle-tool-use` (executes queued tools) |
| `DONE` | Request complete | `gptel--handle-post` (runs caller `:post` hooks) |
| `ERRS` | Request failed | `gptel--handle-post` |
| `ABRT` | User invoked `gptel-abort` (see `gptel-request.el:2124`) | `gptel--handle-post` |

Our handlers chain **before** the upstream handlers — the pattern already used by `config/gptel/tools/persistent-agent.org`:

```elisp
(defvar gptel-chat--fsm-handlers
  `((WAIT ,#'gptel-chat--on-wait   ,#'gptel--handle-wait)
    (TYPE ,#'gptel-chat--on-type)
    (TOOL ,#'gptel-chat--on-tool   ,#'gptel--handle-tool-use)
    (DONE ,#'gptel-chat--on-done   ,#'gptel--handle-post)
    (ERRS ,#'gptel-chat--on-errs   ,#'gptel--handle-post)
    (ABRT ,#'gptel-chat--on-abrt   ,#'gptel--handle-post)))

(gptel-request prompt
  :fsm      (gptel-make-fsm :handlers gptel-chat--fsm-handlers)
  :callback #'gptel-chat--stream-callback
  :stream   t)
```

Our handlers do UI-only work (update the display-layer overlay to reflect "waiting / streaming / tool-running / done / error / aborted"). Upstream's handlers continue to drive the actual protocol transitions — we neither fire `gptel--fsm-transition` directly nor replace the core handler list.

**Why `gptel--handle-post` is chained on DONE / ERRS / ABRT.** Upstream's default alist maps each terminal state to `gptel--handle-post`, which iterates any caller-supplied `:post` hooks on the request's `info` plist (`gptel-request.el:1589-1594` and `:1754-1758`). If our alist replaced those entries without chaining `gptel--handle-post`, any caller passing `:post` to `gptel-request` — including future session-export, budget-tracking, or activities-integration paths — would silently never run their hook. That is precisely the "hard-to-debug silent failure" class this decision rejects; we chain to preserve upstream's contract.

**Why `ABRT` is in the alist at all.** `gptel-abort` (upstream's user command, bound in chat-mode to `C-c C-k` per Decision 7 implementation) drives the FSM into `ABRT` (`gptel-request.el:2124`). Without a chat-mode handler on `ABRT`, the lifecycle indicator stays stuck at `waiting` / `streaming` / `tool-running` after a user abort. That wedges the send-guard (Decision 11) on the next send because our idle check inspects the indicator, not just `gptel--fsm-last`'s protocol state. Chaining `gptel-chat--on-abrt` clears the indicator to `'aborted`, which the send-guard treats as idle (see Decision 11 below).

**Reference implementation:** `config/gptel/tools/persistent-agent.org` is the canonical worked example of this handler-augmentation pattern in this repo. Start there, not from upstream source.

**Alternatives considered:**
- Hand-rolled per-request closure with its own state enum (an earlier draft of this decision). Rejected: it would parallel, not integrate with, the upstream FSM — leaving us stranded when the upstream tool loop advances (`TOOL → WAIT` for multi-turn agentic tool use) and we are stuck re-deriving it.
- Ignoring the FSM entirely and relying only on `:callback`. Rejected: the callback exposes wire events (chunks, tool-call / tool-result cons cells, completion sentinels); it does not expose higher-level lifecycle state like "between tool calls during a multi-turn assistant turn." The FSM exposes that directly via `gptel-fsm-state`, which the display layer (Decision 5) and send-guard (Decision 11) both want.

**Rationale:** the FSM is the one durable lifecycle abstraction upstream gives us. Plugging in at the handler layer keeps our UI reactive to state changes while letting upstream own transition correctness, tool dispatch, and error propagation. If upstream adds new intermediate states (e.g., a `REASONING` phase), we inherit that for free.

### Decision 3b: Per-chunk text hygiene — per-request stream handle (cl-struct) with insertion marker, holdback, and tool-marker

**Choice:** separate from the request FSM, each send creates a per-request *stream handle*: a small `cl-defstruct` record whose slots are the text-processing state for safe line-by-line insertion. The handle owns:

- an insertion marker for the active assistant block,
- a one-line holdback string (internal; not exposed on the handle),
- a tool-marker slot (nil when no tool call is in flight; a live marker when one is),
- and a set of typed function slots the caller invokes to drive the closure (`insert`, `set-tool-marker`, `clear-tool-marker`).

This is not a state machine. It is a streaming text sanitizer packaged as a typed per-send handle. The separation matters because the two concerns have different lifetimes and different test surfaces:

| Concern | Mechanism |
|---|---|
| Where in the protocol are we? (`WAIT` / `TYPE` / `TOOL` / `DONE`) | Upstream FSM (Decision 3) |
| Where in the buffer are we inserting? What line is partially received? | Stream handle (this decision) |
| What is the line-level delimiter escape rule? | `gptel-chat--sanitize-chunk` (Decision 4) |

**Handle shape.** The factory `gptel-chat--make-stream-inserter` returns a `gptel-chat-stream` struct (defined via `cl-defstruct`) with at minimum:

| Slot | Purpose |
|---|---|
| `insert` | Function of one argument (a string chunk, or the flush sentinel `t`). The primary entry point the callback invokes on each response event. |
| `set-tool-marker` | Function of one argument (a live marker). Sets the handle's tool-marker slot; subsequent inserts route there instead of the assistant marker. Called by `stream-callback` on `tool-call` events (Decision 10). |
| `clear-tool-marker` | Zero-argument function. Clears the tool-marker slot so subsequent inserts route back to the assistant marker. Called on `tool-result` events (Decision 10). |

The holdback string and the raw tool-marker value remain captured inside the lambda closure bound to `insert` — they are per-send scratch state with no legitimate external reader. Only the routing slot (tool-marker) is externally controllable, and only via the typed setter/clearer, not as a raw assignable field.

On each text chunk the `insert` function:

1. Prepends any carry-over holdback to the new chunk.
2. Splits at `\n`; trailing partial line (if any) becomes the new holdback.
3. For each complete line, runs `gptel-chat--sanitize-chunk` (Decision 4) and inserts at the active marker — tool-marker if one is live, otherwise the assistant insertion marker (Decision 10). The routing choice is resolved **once per `insert` invocation** via `gptel-chat--stream-active-marker`, a pure helper directly unit-testable. A single `insert` call's routing applies to every complete line within that call. `stream-callback` MUST issue any `set-tool-marker` / `clear-tool-marker` call *between* distinct `insert` calls. Tool-marker changes apply from the next `insert` call onward; they never affect the routing of the in-flight call.
4. On completion (`t` from the callback — see Decision 10), flushes the holdback (cannot be a full `#+end_*` line by construction — no newline), inserts `#+end_assistant`, and positions point per Decision 8.

The markers are proper Emacs markers (not integer positions) so concurrent user edits above the insertion point don't corrupt them.

**Alternatives considered:**
- *Bare lambda with tool-marker captured internally and no setter.* Rejected (this is what the earlier draft of Decision 3b specified). The tool-marker slot exists but is unreachable from `stream-callback` without `cl-letf` surgery on the captured environment, which is the worst-of-both-worlds: the slot is documented behaviour yet dead code under YAGNI. Tests end up doing direct surgery to exercise the routing arm — an explicit anti-pattern the review on `sanitize-chunks` flagged.
- *Plist handle* `(:insert ... :set-tool-marker ... :clear-tool-marker ...)`. Tempting for its minimal ceremony, but a plist is the idiomatic Emacs Lisp shape for *options* (keyword-arg dictionaries), not for a typed per-request record. Accessors are untyped (`plist-get`), there is no predicate, and call sites become noisy with `(funcall (plist-get handle :insert) chunk)`. Ship cost is similar to cl-defstruct; readability at call sites is worse.
- *Caller-owned mutable cell* (factory takes a `(list nil)` tool-marker cell, closure reads `(car cell)` each chunk). Technically works, and lifetime can be kept per-send by the caller. Rejected because the wiring contract — "the `car` of this cell is the routing marker" — is an un-named, undocumented protocol every call site must learn separately. The `cl-defstruct` alternative names the protocol explicitly via generated accessors (`gptel-chat-stream-set-tool-marker`, `gptel-chat-stream-insert`) and a generated predicate (`gptel-chat-stream-p`), so call sites and tests both get typed, self-documenting access. That is the core win, not lifetime coupling or indirection cost.
- *Insert directly at `(point-max)`.* Fails when the buffer has content after the active assistant block (e.g., regenerate inserts into the middle of the buffer; the user edits below the cursor during stream).
- *Buffer-local state variables.* Harder to test, easier to leak between sends — and concurrent sends are disallowed anyway (Decision 11), so per-request scope is the right granularity.

**Rationale:** the stream handle keeps lifetime coupled to the send (handle and its underlying closure are created together and become unreachable together once the FSM reaches `DONE`) while making the routing arm directly testable: a test binds a buffer and two markers, calls `make-stream-closure`, calls the handle's `insert` once to verify assistant-marker routing, then calls `set-tool-marker`, calls `insert` again, and asserts that text lands at the tool marker. No `cl-letf` on the factory and no surgery on captured variables. `cl-defstruct` is the idiomatic Emacs Lisp construct for a small typed record with named slots and a generated predicate — exactly the situation here. Line-buffered holdback remains the well-known streaming-safe pattern for line-oriented sanitization (it prevents splitting a `#+end_...` collision across a chunk boundary — spec scenario "Response contains collision split across chunks").

**Follow-up task:** implementation of this refactor is owned by `expose-tool-marker-setter`. That task's Option B (cl-struct) is the authoritative framing; Options A (plist) and C (caller-owned cell) are superseded by this decision.

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

Point is positioned on the empty line inside the block. No model, system prompt, tools, or preset metadata is pre-populated in the buffer.

Per-buffer configuration (model, backend, system message, tools, temperature) is delivered by the upstream preset system and `gptel-menu`, wired in by Decision 15. Users who want a starting preset invoke `M-x gptel-menu` and pick one, or save the buffer with a `:PROPERTIES: ... :GPTEL_PRESET: name :END:` drawer; chat-mode activation will apply it.

**Rationale:** minimum viable initialization. The preset system is the right layer for per-buffer configuration — inventing a parallel keyword set here would duplicate upstream.

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

**Sequencing invariant (cross-reference to Decision 3b):** routing changes (`set-tool-marker` / `clear-tool-marker`) MUST be interleaved *between* distinct `insert` calls on the stream handle, not within. The active marker is resolved once per `insert` invocation, so a routing change issued mid-call has no effect on the in-flight call — it applies from the next `insert` onward. See Decision 3b step 3 for the underlying rule.

**Corrected from earlier draft:** an earlier version of this decision described tool events as "plists with a tool-call sentinel" — that was wrong. Upstream emits cons cells, not tagged plists; dispatch via `pcase` backquote patterns is both the upstream idiom and what `persistent-agent.org` already does.

**Note on transitioning the FSM:** `gptel--fsm-transition` is the mechanism by which handlers advance state. Our callback does not call it directly — the default `WAIT` and `TOOL` handlers (still present in our chained handler list, Decision 3) drive transitions for us. If a future UX wants to intercept (e.g., confirm-before-tool-call), the intercept goes in our `TOOL` handler, not in the callback.

**How `'abort` is triggered:** the upstream command `M-x gptel-abort` (`gptel-request.el:2099-2125`) operates on the current buffer, finds its active FSM, invokes our callback with `'abort`, and then drives the FSM into `ABRT` via `gptel--fsm-transition` (line 2124). The FSM transition in turn fires our `gptel-chat--on-abrt` handler (chained with `gptel--handle-post`, Decision 3) — that handler sets `gptel-chat--lifecycle-state` to `'aborted` so the send-guard in Decision 11 will permit the next send. Chat-mode does not need a `gptel-chat-abort` wrapper — binding `C-c C-k` or similar directly to `gptel-abort` is sufficient.

**Upstream response hooks are intentionally bypassed.** `gptel-post-response-functions` and friends are consumed by gptel-mode's default callback, not by `gptel-request`. Our callback does not invoke them, on purpose — those hooks assume gptel-mode's prompt/response-prefix insertion conventions, which we don't use. If a follow-up change needs a chat-mode-specific post-response hook (e.g., for session-save integration), add one with its own name.

**Reference implementation:** `config/gptel/tools/persistent-agent.org` calls `gptel-request` with full tool history and pattern-matches every shape above in a single `pcase`. Start there for both the callback dispatcher and the `:prompt` message shape with tool history (Open Question 1).

### Decision 11: Send-during-stream protection — via upstream FSM state, not a parallel flag

**Choice:** to detect an in-flight request, inspect the buffer-local `gptel--fsm-last` that upstream sets on every `gptel-request` (see `gptel.el:1104, 1328`). If it is non-nil and `(gptel-fsm-state gptel--fsm-last)` is not in the idle set `(DONE ERRS ABRT)`, signal a user-visible error. No queueing. We do **not** introduce a parallel `gptel-chat--stream-active-p` flag.

**Idle set includes ABRT.** An aborted request has had its process torn down and its FSM transitioned to `ABRT` by `gptel-abort` (`gptel-request.el:2124`, see Decision 3 and Decision 10). Treating `ABRT` as busy would wedge the buffer after every user abort, forcing the user to kill and recreate the session buffer to recover — a regression from upstream gptel-mode, which has no such failure mode. The send-guard MUST therefore accept `ABRT` alongside `DONE` and `ERRS`.

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

### Decision 15: Preset system and `gptel-menu` integration

**Choice:** chat-mode participates in the upstream preset system via `gptel--apply-preset` and supports `M-x gptel-menu` for interactive configuration, while routing the menu's Send action through `gptel-chat-send` (not upstream's `gptel--suffix-send`).

#### Preset application

All per-buffer configuration — model, backend, system message, tools, temperature, reasoning, context flags — flows through upstream's preset mechanism. This collapses what would otherwise be a handful of parallel `#+gptel-*:` keyword conventions into one:

- **On mode activation**, `gptel-chat-mode` looks for a preset declaration in the buffer (in this order):
  1. An Org `:PROPERTIES:` drawer at point-min with a `:GPTEL_PRESET: name` line — matching how `gptel-mode` handles the same property (`gptel-org.el:564-572`).
  2. A file-local `gptel--preset: name` via the standard `-*- ... -*-` or `Local Variables:` mechanism — `gptel--preset` is already declared `safe-local-variable`.
- If found, chat-mode calls `(gptel--apply-preset 'name (lambda (sym val) (set (make-local-variable sym) val)))`. The preset's `:backend`, `:model`, `:system`, `:tools`, etc. are installed as buffer-local values; subsequent `gptel-request` calls read them directly.
- If no preset is declared, chat-mode does nothing — the buffer inherits the global defaults, same as any other `gptel-request` caller.

This is exactly the pattern `config/gptel/tools/persistent-agent.org:646-650` already uses. We do not turn on `gptel-mode`; we only borrow its preset-parsing behavior.

#### `gptel-menu` integration — configuration is free, Send is rebound

`gptel-menu` is a `transient-define-prefix` with no mode guard. Its **configuration** suffixes (preset pick/save, model, backend, system message, tools, context, temperature) are pure buffer-local variable mutation and work unchanged in a chat-mode buffer. No action required — a user can invoke `M-x gptel-menu` today and everything but Send is correct.

**The menu's Send suffix (`gptel--suffix-send`) is the wrong path for chat-mode buffers.** It assumes gptel-mode's prompt/response-prefix conventions and `gptel` text-property markers. Invoking it in a chat-mode buffer would insert response text outside our block structure and ignore the canonical on-disk format.

**Choice:** chat-mode provides its own transient, `gptel-chat-menu`, that reuses `gptel-menu`'s configuration layout and replaces the Send suffix with one that invokes `gptel-chat-send`. `gptel-chat-menu` is bound on the chat-mode keymap (suggested: `C-c C-,` to mirror common gptel muscle memory — exact key decided during implementation). Users who type `M-x gptel-menu` directly still get the upstream prefix; the chat-mode key gets the rebound one.

**Alternatives considered:**

- Do nothing; let users invoke `M-x gptel-menu` for configuration and `C-c C-c` for send, never using the menu's Send entry. Rejected: the menu's Send button is too obvious a trap — a user will press it eventually and get wrong behavior inserted into their buffer.
- Advise `gptel--suffix-send` to delegate to `gptel-chat-send` when `(derived-mode-p 'gptel-chat-mode)`. Rejected: advice on upstream internals is fragile, and affects `M-x gptel-menu` globally in a way that surprises users who expect upstream behavior.
- Duplicate the whole `gptel-menu` layout verbatim. Rejected: maintenance burden — any new infix upstream adds is one we'd need to mirror.

**Rationale:** replacing a single suffix is the narrowest possible fork. Transient's public API supports it (`transient-replace-suffix`, or defining a new prefix that shares infix symbols), and the cost is a few lines of code. The user gets the full upstream menu with exactly one behaviorally different button.

**Implementation note (not a decision):** the exact transient mechanism — replacing the suffix on a copy of `gptel-menu`'s layout, or defining `gptel-chat-menu` as a fresh prefix that references the same infixes — is deferred to implementation. Both produce the same user-visible behavior.

### Decision 16: Sessions use `gptel-chat-mode`, never `gptel-mode`

**Choice:** session buffers are in `gptel-chat-mode` exclusively. The sessions subsystem does not enable `gptel-mode` (minor mode) on session buffers; chat-mode owns the full buffer role.

Three concrete wiring changes fall out of this:

1. **Auto-init hook (`jf/gptel--auto-init-session-buffer` in `config/gptel/sessions/commands.org`).** The hook keys on the path pattern `*/branches/<branch>/session.org` (renamed from `session.md` — see Decision 18). On match, it parses `metadata.yml`, applies the session's preset via `gptel--apply-preset` with a buffer-local setter (same as today), and ensures `gptel-chat-mode` is the active major mode. It does **not** call `(gptel-mode 1)`.
2. **Preset-application path.** `gptel-chat-mode` activation itself performs preset parsing (Decision 15); the sessions auto-init hook plays the same role but driven by `metadata.yml` instead of a `GPTEL_PRESET` property drawer. Both paths call `gptel--apply-preset` with a buffer-local setter — behaviorally identical. If both sources are present, `metadata.yml` wins (sessions are the authoritative configuration for session files).
3. **`gptel-mode`-dependent behavior is removed from the session code path.** The `jf/gptel--ensure-mode-once` helper (which today calls `(gptel-mode 1)`) is rewritten to call `(gptel-chat-mode)`. Upstream's `gptel--save-state` / `gptel--restore-state` are no longer invoked by session auto-init; chat-mode's format is self-describing (Decision 18).

**Rationale:** the chat-mode buffer format and the `gptel-mode` buffer conventions (text-property bounds, prompt/response-prefix insertion, Local Variables for `gptel--bounds`) are incompatible by construction. Running both modes on the same buffer produces a mixed-format file that neither parser can read cleanly. Clean separation avoids every collision class at once.

**Alternatives considered:**
- Let the auto-init hook enable both modes and let users choose which sends through which path. Rejected: every save path then has to know which format is authoritative; a single buffer can end up with both `#+begin_user` blocks *and* `gptel--bounds` text properties after a few edits, and there is no right way to reconcile them.
- Keep `gptel-mode` on for the minor-mode utilities it provides (some keybindings, `gptel-send` wiring). Rejected: chat-mode provides its own send command and its own keymap; nothing in `gptel-mode`'s minor-mode surface is useful to a chat-mode buffer.

### Decision 17: Session auto-init contract under chat-mode

**Choice:** on opening a file matching `*/branches/<branch>/session.org` (or `*/agents/<agent-name>/session.org` — agents follow the same rule), the session auto-init hook:

1. Extracts `session-id` and `branch-name` from the path.
2. Sets the five buffer-local session variables already defined by `sessions-persistence`: `jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`, and `jf/gptel--parent-session-id` (when applicable).
3. Registers the buffer in `jf/gptel--session-registry` keyed `"<session-id>/<branch-name>"` (unchanged from today).
4. Reads `metadata.yml` from the branch directory and calls `(gptel--apply-preset (intern (plist-get meta :preset)) (lambda (sym val) (set (make-local-variable sym) val)))`.
5. Ensures the major mode is `gptel-chat-mode`. If the file's auto-mode-alist entry or mode cookie already brought up chat-mode, this is a no-op; if another mode is active, the hook switches to chat-mode.
6. Updates the `current` symlink to point at this branch (unchanged from today).

This is a single-pass, idempotent function keyed on path — same shape as today's auto-init, with three substantive changes: the path regex matches `.org` not `.md`, the mode switch goes to `gptel-chat-mode`, and no `gptel--save-state` / `gptel--restore-state` round-trip happens.

**Rationale:** keeps the auto-init's existing contract (path-based recognition, five buffer-local vars, registry entry, preset applied, mode enabled) while swapping the mode and format. Activities integration, branching, and any future session-aware tool that relies on the buffer-local vars keeps working unchanged.

### Decision 18: Session file format is `session.org` in chat-mode syntax

**Choice:** new sessions are written as `session.org` with chat-mode's block format. The file's initial content on creation is:

```org
#+begin_user

#+end_user
```

(matching Decision 9's new-chat initial content, so a fresh session looks the same as a fresh standalone chat buffer). Branch directories and agent directories follow the same pattern — `branches/<name>/session.org`, `agents/<name>/session.org`.

**Rationale:** chat-mode's delimiters are org special blocks, which are not valid markdown. Keeping the `.md` extension would mislead external tooling, org-mode's own file-association logic, and contributors reading the filesystem. Renaming to `.org` makes the format match the extension and aligns with how chat-mode activates (via `auto-mode-alist` entries on `.org` plus a mode cookie, or an `.dir-locals.el` entry under the sessions root directory).

**Persistence mechanism:** chat-mode buffers save via plain `save-buffer`. There is no `before-save-hook` that writes Local Variables; the block structure is self-describing. `metadata.yml` continues to track preset, `session_id`, `created`, `updated`; the `:updated` timestamp is refreshed via a `before-save-hook` in the sessions subsystem that touches `metadata.yml` only, not the session file.

**Alternatives considered:**
- Keep `session.md` and treat it as org internally. Rejected: tooling friction (Emacs file-associations, tree-sitter grammars, other users' tooling), plus contributors reading the directory would be confused.
- Use a new extension like `.chat` or `.gptel`. Rejected: chat-mode is derived from `org-mode` and its files *are* valid org files; `.org` is the honest extension.

### Decision 19: Clean break — no migration from pre-chat-mode sessions

**Choice:** existing pre-chat-mode sessions (files named `session.md` in the gptel-mode format with `gptel--bounds` Local Variables) are not migrated. The new sessions code:
- Does not read `session.md` files.
- Does not provide a migration command.
- Does not attempt to detect or convert pre-existing sessions.

Users with pre-existing sessions can still open them manually in upstream `gptel-mode` (`M-x gptel-mode`) — that code path is untouched — but they are not visible to `jf/gptel--init-registry`, `jf/gptel-session-find`, or any of the chat-mode-based session commands.

**Rationale:** migration tooling is substantial engineering (parsing `gptel--bounds`, reconstructing turn structure from prompt/response prefixes, handling tool-result rendering divergences, verifying round-trip) for a one-time benefit. The user explicitly directed a clean break. New sessions get the new format; old sessions stay where they are.

**Alternatives considered:**
- Write an on-open converter that transparently rewrites `session.md` to `session.org` the first time a user opens a legacy session. Rejected: destructive edits on first open are a user-trust footgun, and "the first time" is ambiguous across machines with synced sessions.
- Provide an explicit `M-x jf/gptel-session-migrate` command. Rejected in this change; could be added as a follow-up if users request it, but not part of the scope here.

**Trade-off:** a user with valuable pre-existing sessions who wants them accessible via the new workflow has no automated path. They can copy content manually from an open-in-gptel-mode buffer into a new chat-mode session, or maintain legacy sessions in parallel with new ones. This is acceptable given the user's explicit guidance.

## Risks / Trade-offs

**[Risk]** `gptel-request` callback shapes shift in a future upstream version → **Mitigation:** the `pcase` dispatcher in Decision 10 is the single integration point. Adapting to a new shape (or a new sentinel like a hypothetical `'reasoning-done`) is a one-file change. Current shapes are cross-validated against upstream (`gptel-request.el:1684-1752`) and against our in-repo `persistent-agent.org`, which uses the same dispatch.

**[Risk]** Case-insensitive sanitization misses an unusual collision form (e.g., model emits `#+End_Assistant` with odd capitalization, or includes zero-width characters between `end_` and `assistant`) → **Mitigation:** the regex covers org's own matching behavior (case-insensitive, `\b` word-boundary). Exotic cases are low-probability; if they occur, the block parser will fail on next send with a clear error identifying the offending line.

**[Risk]** Display-layer overlay refresh during streaming creates visual flicker → **Mitigation:** debounced with `run-with-idle-timer` (100-200ms). If flicker is still noticeable, refresh only on stream completion, not per chunk.

**[Risk]** Shadowing `C-c C-n`/`C-c C-p` from org-mode surprises users who switch between `org-mode` and `gptel-chat-mode` buffers → **Mitigation:** document clearly in the mode commentary; provide the opt-out helper noted in Decision 7.

**[Risk]** Tool-call handling assumes a specific `#+begin_tool` format in history replay → **Mitigation:** the existing session format already uses exactly this form; parser and stream both use the same code path, so round-trip consistency is enforced by construction.

**[Risk]** Performance degradation on very long logs (hundreds of turns) → **Mitigation:** regex parser is linear in buffer size; overlay scanner is incremental via `after-change-functions`. If profiling shows issues, the parser can cache the last turn-list and invalidate only the tail on edit.

**[Risk]** Sessions subsystem rewrite introduces regressions in registry / activities / branching paths that were working under `gptel-mode` → **Mitigation:** the registry, `metadata.yml` schema, directory layout, `current` symlink, and activities-integration worktree tracking are all unchanged — only the mode, filename, and persistence mechanism change. The refactor surface is narrow (auto-init hook, filesystem templates, branching truncation algorithm) and has direct test coverage via the existing session spec scenarios, rewritten for the chat-mode format.

**[Risk]** User opens a `session.org` file outside the sessions auto-init path (e.g., via `find-file` with a mistyped path that misses the `branches/<branch>/` regex) and chat-mode activates without session state → **Mitigation:** this is the same behavior as opening any standalone chat file — preset is not applied, registry is not updated, but the buffer is fully functional for chat. No data corruption; user can move the file into the right path if they wanted session participation.

**[Risk]** Users with valuable pre-existing `session.md` sessions discover they are orphaned from the new session workflow → **Mitigation:** Decision 19's clean-break is explicit and documented in the proposal's **Breaking** section. Upstream `gptel-mode` continues to open and edit the old files normally; users lose only the new-workflow features (registry, branching, activities-resume) for legacy sessions.

**[Risk]** Branching rewrite on the turn list silently differs from the bounds-based branching's truncation semantics (e.g., includes/excludes the selected prompt differently) → **Mitigation:** the sessions-branching delta spec specifies the include/exclude semantics in chat-mode terms ("truncate at the start of the Nth `#+begin_user` to exclude the prompt; truncate at the end of its paired `#+end_user` to include it"). Scenario coverage is preserved from the existing spec.

## Migration Plan

This change replaces the mode and file format of our session workflow. It is not a migration in the traditional sense (no data transformation happens) — it is a cut-over.

1. **Implement chat-mode modules** under `config/gptel/chat/`, wired into `jf/enabled-modules` *before* the sessions modules (sessions now depends on chat-mode).
2. **Rewrite session modules**:
   - `config/gptel/sessions/commands.org`: auto-init keys on `session.org` and enables `gptel-chat-mode`; session-creation writes `session.org` with chat-mode initial content.
   - `config/gptel/sessions/filesystem.org`: directory templates use `session.org`.
   - `config/gptel/sessions/branching.org`: branch-point selection iterates chat-mode turn list; context truncation copies buffer up to the chosen turn boundary (see Decision 18 / sessions-branching delta spec).
   - `config/gptel/sessions/activities-integration.org`: activity session creation emits `session.org` unconditionally; `jf/gptel-session-create-persistent` no longer takes a mode-selection parameter.
3. **Update specs** — `sessions-persistence.md` and `sessions-branching.md` delta specs in this change; chat-mode spec gains session-integration scenarios.
4. **Retire legacy paths** in the sessions code: `gptel--save-state`, `gptel--restore-state`, `gptel-mode` enable/disable, and any `gptel--bounds` reasoning are removed from the session modules. Upstream `gptel-mode` itself is untouched.
5. **Rollback plan** if the cut-over is aborted mid-way: revert the change; new sessions created under chat-mode remain on disk as readable `.org` files but are no longer recognized by the session registry. Users can hand-convert or discard them.

**Pre-existing `session.md` files**: untouched. They continue to open in upstream `gptel-mode` (users invoke `M-x gptel-mode` in the buffer). They are not auto-recognized by the new session commands. This is Decision 19.

## Open Questions

1. **Exact `:prompt` shape for tool-call history in `gptel-request`.** The *callback* shape for tool events is settled (Decision 10: cons cells of plists). What still needs verification is the reverse direction — how tool-call / tool-result history is encoded in the `:prompt` value passed **into** `gptel-request` on subsequent turns, once our parser has reconstructed it from the buffer. Upstream's backend-specific `gptel--parse-buffer` methods build this shape today; we are bypassing them. `config/gptel/tools/persistent-agent.org` passes full tool history through `:prompt` and is the canonical worked example — extract the shape from there during implementation.

2. **Transient mechanism for the `gptel-chat-menu` send replacement.** Decision 15 commits to rebinding the menu's Send suffix but defers the exact mechanism (replace-suffix on a copy, or parallel prefix referencing shared infixes). Pick during implementation based on what produces the smallest maintenance surface against upstream `gptel-transient.el` drift.

3. **Debounce timing for display-layer refresh.** 100ms is a guess. Implementation should make this a `defcustom` (`gptel-chat-display-refresh-delay`) so we can tune after seeing real behavior.

4. **Does `auto-save-mode` cause problems during streaming?** Org's `auto-save-visited-file-name` may write partial buffers. Streaming inserts are append-mostly, so partial saves should be recoverable. Verify during manual testing; if problematic, disable auto-save while a request is in flight (per Decision 11, detectable via `gptel--fsm-last`).

5. **Should the display-toggle state persist per-buffer or globally?** v1 choice: per-buffer, via a buffer-local variable, with no persistence across Emacs restarts. If users want a global default, add a `defcustom` in a follow-up.

6. **Keybinding for new-chat command.** `gptel-chat-new` creates a buffer; there's no obvious key for "I'm not in a chat buffer yet." Probably leave as `M-x`-only for v1; users can add their own global binding.
