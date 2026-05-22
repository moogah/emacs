# Design: Chat-mode body indentation

## Context

`research.md` establishes the empirical findings; this document records the design decisions for the implementation.

**This design supersedes an earlier one.** The change first shipped a per-`*` heading escape (14 tasks, now in `tasks/closed/`): a column-0 `*` inside a chat block received a leading space so org's `^\*+ ` regex no longer matched. Real-world use showed the per-token escape leaves bodies visually ragged. The decisions below replace it with a uniform body-indentation model. The superseded decisions are not preserved here; the closed tasks remain as history.

## Decision 1: Indent the whole body, not per-token escape

**Choice:** a chat-block body is an indented region — every body line is indented by N≥1 spaces. This single mechanism replaces both per-token escapes from the superseded design (the per-`*` heading escape and the `,`-prefix `#+end_*` delimiter escape).

**Rationale:**

- **Subsumption.** Org's structural scanners are all anchored at column 0 — the heading regex `^\*+ `, the special-block closers `^#\+end_…`, `#+begin_…`, drawers, keywords. Indenting every body line by ≥1 space moves all content off column 0, so no body line can be read as structure. One mechanism neutralises every collision class, present and future, instead of a growing list of per-token escapes.
- **A legible invariant.** `column 0 = structure (delimiters)` · `indented = content`. A reader, a maintainer, or org's own parser can tell content from structure by indentation alone.
- **Visual uniformity.** The per-`*` escape left bodies ragged: escaped `*` lines at column 1, every other line at column 0. Real-world use surfaced this immediately — it is the complaint that triggered this redesign. A uniform indent reads as a coherent block.
- **Babel parity.** `org-edit-src-content-indentation` already establishes "block content is an indented region" as an org idiom. Chat blocks adopt the same shape.

**Alternatives considered:**

- **Per-token escape (superseded design).** Pros: minimal on-disk footprint; only offending lines change. Cons: ragged; needs a distinct escape per collision class, each with its own write producers and read inverse.
- **Display-only / virtual indentation** (`line-prefix` overlay over real column-0 content). Pros: zero round-trip risk; no on-disk change. Cons: the corruption is structural — org parses buffer text, not display. Virtual indent does not move content off column 0, so it does not fix the bug. Rejected for body content. (It *is* used for tool delimiter lines — Decision 5 — precisely because those are not corruption sources.)

**Implications:** the escape subsystem retires entirely — `gptel-chat--sanitize-chunk`'s heading and delimiter rules, `--escape-typed-heading`, `--escape-inserted-headings`, `--escape-headings-in-region`, `--heading-escape-prefix`, `--unescape-headings`, `--unescape-end-delimiters`, `--migrate-headings`. A body line at column 0 becomes the only malformed state; the indentation invariant is what every write path maintains and migration repairs.

## Decision 2: Indentation width is a defcustom; default 2

**Choice:** `gptel-chat-content-indentation` (the existing defcustom, repurposed) controls the body indent width. Its meaning shifts from "heading-escape prefix width" to "chat-block body indent width." Default changes `1 → 2`.

**Rationale:**

- Default 2 matches `org-edit-src-content-indentation` (org's src-block default) and reads as a deliberate indent rather than a near-invisible 1-space nudge.
- 1 is the structural minimum (any leading whitespace breaks `^\*+ `) but is visually marginal — the very complaint behind this redesign. The user explicitly asked for ≈2.
- `:type 'natnum`; the runtime floor is clamped to ≥1 by the accessor — a 0-width indent would not move content off column 0.

**Alternatives considered:** hardcode 2 (loses tunability); keep default 1 (visually marginal).

**Implications:** a `gptel-chat--body-indent` accessor replaces `gptel-chat--heading-escape-prefix` — it returns the configured width, clamped to a floor of 1, defaulting to 2 when unbound. All write producers route through it. The width affects only *new* writes and migration's target; the read-side dedent measures actual indentation (Decision 3), so the round-trip never depends on the current defcustom value.

## Decision 3: Read side is per-segment measure-and-strip

**Choice:** before a turn's content reaches the model, the parser strips the *common minimum leading indentation* from each message segment — measured per segment, not assumed. This is org's `org-do-remove-indentation` model.

**Rationale:**

- **Stateless and drift-proof.** Nothing records "this file was written at width N." Whatever indentation a segment actually carries is measured and removed. A file written at width 2, a legacy file at width 1, a file edited under a changed defcustom — all dedent correctly.
- **Per-segment, not per-buffer or per-turn-body.** An assistant body is split by the parser into text segments and tool-call segments around `#+begin_tool` blocks. Measuring per segment means a column-0 tool delimiter line (which stays at column 0 — Decision 5) never drags a segment's measured minimum to 0. The parser already produces per-segment strings (`gptel-chat--flush-text-segment`, `--scan-assistant-body`), so this is the natural grain.
- **Symmetric by intent, not by character count.** Write indents by exactly N; read strips whatever common indent it finds. The asymmetry is deliberate — it is what makes the round-trip robust against width changes and legacy content.

**Alternatives considered:**

- **Store the width per session** (e.g., a `:GPTEL_CHAT_BODY_INDENT:` drawer property) and strip exactly that. Pros: exact even for a message whose every line is indented (no column-0 line to anchor the measure). Cons: carries state that can desync from the buffer. The measure-and-strip edge case (a uniformly-indented message loses its outermost indent level) is exactly org-src-block behaviour and was accepted as "good enough."
- **Strip exactly the current defcustom width.** Breaks on any width change between write and read.

**Implications:** `gptel-chat--unescape-headings` and `gptel-chat--unescape-end-delimiters` are deleted; one `gptel-chat--dedent` (measure-and-strip a string) replaces both, wired into `gptel-chat--turn-to-messages` and `gptel-chat--segment-to-messages`. Tool-result capture in `gptel-chat--scan-assistant-body` currently does `(string-trim (buffer-substring …))` — `string-trim` collapses the whole string's leading whitespace and would corrupt per-line indentation; the capture changes to dedent first, then trim leading/trailing blank lines.

## Decision 4: Outer turn delimiters stay strictly column-0-anchored

**Choice:** `#+begin_user` / `#+end_user` / `#+begin_assistant` / `#+end_assistant` are never indented; the parser anchors them strictly at column 0 (unchanged from the existing parser).

**Rationale:** this is what makes indentation an escape. A user can type the literal text `#+end_user` inside a message — it is body content, so it is indented, and the parser, matching `^#\+end_user` strictly at column 0, does not see it as a real closer. If the parser tolerated indented delimiters, indenting body content would no longer protect it. The outer delimiters bound the body; they are the frame, not content — they sit at column 0 the way an org src block's `#+begin_src` / `#+end_src` do.

**Implications:** the parser's outer-delimiter regexes are unchanged. The `,#+end_*` escape that previously protected model-emitted `#+end_*` lines is unnecessary — those lines are body content and get indented.

## Decision 5: Tool blocks — real column-0 delimiters, cosmetic display alignment

**Choice:** nested `#+begin_tool` / `#+end_tool` delimiter lines stay at *real* column 0 in the buffer and on disk. The display layer paints an N-space `line-prefix` on those two lines so they *render* aligned with the indented prose around them. Tool *result* content (between the header line and `#+end_tool`) is real-indented like all body content.

**Rationale:**

- Tool delimiters are structure, and structure lives at column 0 (Decision 4's invariant). The parser finds `#+begin_tool` / `#+end_tool` with column-0-anchored regexes; moving them to column N would force the parser to recognise structure *inside the content zone* — reintroducing the ambiguity Decisions 1 and 4 eliminate (an indented `#+end_tool` the model writes in prose would be indistinguishable from a real one).
- Keeping them at column 0 means zero parser change, zero streaming-emitter relocation, and zero re-fixturing of the tool-block test surface.
- The cost is purely visual — tool delimiter lines sit flush-left amid indented prose. A `line-prefix` property (the mechanism `org-indent-mode` uses) closes that gap cosmetically. A display-only nudge is sound here — and *not* sound for body content (Decision 1) — because tool delimiter lines are never sent to the model: the parser consumes them as structure, so a cosmetic change to them has no round-trip consequence.

**Alternatives considered:**

- **Really indent the tool delimiters (Path B).** Requires a strict/tolerant anchor split in the parser (`#+begin_tool` strict at top level for the `tool-block-outside-assistant` error, tolerant inside an assistant block), reworking the `point-in-block-body-p` delimiter regex, relocating the streaming emitters, re-fixturing every tool-block test — and it breaks the column-0 invariant. Rejected.
- **Leave them flush-left (Path A).** Correct and zero-cost, but visibly ragged — the complaint this redesign exists to fix.

**Implications:** a display-layer helper scans assistant-body ranges for `^#\+begin_tool` / `^#\+end_tool` lines and installs the `line-prefix`, refreshed by `gptel-chat--refresh-overlays`. The read-side dedent (Decision 3) measures per segment, so the column-0 tool delimiter lines (not part of any segment string) do not affect any segment's measured indent.

## Decision 6: Maintenance model — how body content stays indented

The per-`*` escape worked off a detectable trigger (`*` at column 0). Uniform indentation has no such trigger — an indented line is indistinguishable from a line a user happened to indent. So maintenance is not a reactive per-token rewrite; it is four ordinary editing affordances:

- **Streaming.** `gptel-chat--sanitize-chunk` becomes a per-line indenter — each completed line of model output is prefixed with the body indent before insertion (blank lines untouched). Idempotent by construction: streamed content is fresh, never re-processed. The line-holdback machinery is unchanged.
- **Typing.** A buffer-local `indent-line-function` (delimiter line → column 0; body line → column N) plus electric-indent. RET in a body starts the next line at column N; once a line starts at column N, a `*` typed there is already off column 0. No per-keystroke hook.
- **Paste / yank.** The `after-change-functions` handler (repurposed from the heading-escape filter) shifts an inserted region inside a body so its minimum-indented line lands at column N — `shift = max(0, N − region-min-indent)`. Structure-preserving (the region shifts as a unit, relative indentation intact) and idempotent (already-indented paste → shift 0). Gated by `gptel-chat--point-in-block-body-p`.
- **Migration on read.** See Decision 7.

**Rationale:** this matches org-babel — org does not police src-block indentation on every keystroke; the content *is* an indented region maintained by ordinary indent affordances and normalised at well-defined moments. The user explicitly accepted "behaves like org-babel blocks — good enough even if not perfect."

**Alternatives considered:** a `post-self-insert-hook` / `post-command-hook` "ensure ≥ N on the current line" guard — idempotent and single-line (no structure distortion), but it polices every keystroke and fights a user deliberately editing indentation. Held as a fallback only if electric-indent proves unreliable in the org-derived mode (see Risks); not the primary mechanism.

**Implications:** `gptel-chat--point-in-block-body-p` survives unchanged (paste still needs it); its docstring is updated. The `post-self-insert-hook` and `gptel-chat--escape-typed-heading` are removed.

## Decision 7: Migration on read normalises legacy and current formats alike

**Choice:** when `gptel-chat-mode` activates, a migration pass normalises every chat-block body to the indented form. Per content-region — the run of lines between delimiter lines (outer or tool) within a body — it (a) strips legacy escape-era artifacts (a leading `,` from `,#+end_*` lines, the old 1-space prefix from `*` lines), then (b) shifts the region so its minimum indent is N. Idempotent; non-destructive (in-buffer only — the next save persists). A clean (already-indented) buffer is left unmodified.

**Rationale:**

- Three on-disk generations must converge: pre-escape (column-0 content), escape-era (1-space `*` escapes, `,#+end_*`), and current (indented). One pass that un-escapes-then-indents handles all three — on a current buffer both steps are no-ops.
- Per content-region, not per-whole-body: the same reason as Decision 3 — column-0 tool delimiter lines must not be shifted (Decision 5) and must not anchor a region's measured minimum.
- Read-time and in-buffer, not a destructive on-disk sweep: reversible via `revert-buffer`, no filesystem ceremony, and the buffer's `org-element` view is correct from the moment the mode activates.

**Alternatives considered:** a one-time on-disk migration command (destructive, needs discovery); lazy migration on first edit (leaves the buffer's `org-element` view broken until a trigger fires).

**Implications:** `gptel-chat--migrate-headings` is replaced by a body-normalising migration. It calls `set-buffer-modified-p t` only if it changed the buffer; a clean session opens unmodified. Malformed buffers (unclosed blocks) cause `gptel-chat-parse-buffer` to signal `user-error`; migration traps that and skips, so the mode still activates for interactive repair.

## Decision 8: Defer indirect editing (`gptel-chat-edit-turn`)

**Choice:** the `C-c '`-style indirect-editing kernel that pops a fresh org buffer for real heading affordances inside a turn is **not** part of this change.

**Rationale:** the corruption bug is fixed by indentation alone. Indirect editing is an ergonomic upgrade for the rare "I want real heading affordances inside this turn" workflow. The indentation model is already the right on-disk shape for it — an indirect-edit kernel would dedent on entry and re-indent on exit, exactly `org-edit-src`'s strip/apply pattern, so deferring it strands no architectural choice.

**Implications:** the chat-mode spec carveout reads "column-0 org structure inside a chat block renders as indented plain text; real structural affordances require [follow-up change]."

## Obsoleted decision

The superseded design deferred "visual font-lock to render escaped `*` lines with `org-level-N` faces." That deferral is obsoleted: an indented body is plain text, with no heading faces to render. No follow-up is needed.

## Open coherence check

Reversible if real-world use turns up surprises:

1. **Decision 2 (default width).** Trivial defcustom change.
2. **Decision 6 (typing mechanism).** `indent-line-function` + electric-indent vs. the `ensure-≥N` guard fallback — independently swappable; neither locks in an on-disk choice.

Decisions 1 (indent the body), 3 (measure-and-strip), 4 (column-0 outer delimiters), 5 (Path C tool blocks), and 7 (migration on read) are the load-bearing architecture; they are mutually consistent and revising any one would be a fresh design pass.
