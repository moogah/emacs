# Chat-mode heading scoping — research handoff

> **Addendum (2026-05-22).** This change first shipped a per-`*` heading
> escape; real-world use found it visually ragged. The design was
> reframed to **full chat-block body indentation** — every body line is
> indented, which neutralises *all* column-0 org structure at once, not
> just `*`. The findings below still hold and still motivate the fix:
> Finding 2 ("a leading space breaks `^\*+ `") is exactly *why*
> indentation works. See the current `proposal.md` and `design.md`.

**Status:** Research / brainstorming. Not yet an OpenSpec change.
**Date:** 2026-04-30
**Originating bug:** `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org` — `* Test Heading` inside a `#+begin_user` block visually absorbs the rest of the buffer; the user block disappears from the org AST.
**Related archived change:** `openspec/changes/archive/2026-04-24-gptel-chat-mode/` (Decisions 12, 13)

## What this document is

A captured set of empirical findings, verified hypotheses, and surviving solution-space candidates from a deep investigation into a structural bug in `gptel-chat-mode`. The chat-mode design assumed `#+begin_user` / `#+begin_assistant` special blocks would safely contain user prompts written using full org syntax, including `*` headings. They don't — and the implications run deep enough that several previously-rejected design candidates need revisiting before this gap is closed.

The next maintainer of this work should read this document end-to-end, then decide between the surviving paths (α / γ / δ below). All discarded paths are documented with the evidence that disqualified them so we don't re-research them.

## Problem statement

### The immediate bug

A user-typed `* Heading` line at column 0 inside a `#+begin_user` block:

- Is recognized by `org-at-heading-p` as a level-1 heading.
- Causes `org-element-parse-buffer` to **lose the special-block entirely** — the AST shows `paragraph | headline | paragraph`, with the `#+begin_user` and `#+end_user` lines demoted to paragraph content of the heading's section.
- Visually absorbs every line below it (including subsequent `#+begin_assistant`, `#+end_assistant`, etc.) into the heading's outline subtree.
- The `gptel-chat-parse-buffer` function still parses the file correctly because it's regex-based and ignores org structure — so the chat session still works mechanically. The break is in the **editing surface**, not the data path.

### The deeper miss

Decision 13 of the archived chat-mode design (`openspec/changes/archive/2026-04-24-gptel-chat-mode/design.md`) committed to: "User blocks support the full org-mode editing experience." The rationale was that org's editing affordances (outline cycling, source-block tangling, list demotion, table editing, link insertion) are exactly the value of deriving from `org-mode`. Headings are arguably the most important of those affordances for "structured prompt composition," and they don't actually work inside chat blocks. The original validation only tested the parser, not the editing surface.

Decision 4 explicitly rejected `org-escape-code-in-string` (which would have escaped `*` lines) on the grounds that broader escaping "clutters assistant content with extra commas on benign lines." That trade-off was made without realizing that an unescaped `* heading` line **structurally collapses the enclosing block** — not merely renders it oddly.

## Research methodology

This investigation ran a sequence of:

1. **Source reading** — org-mode (org-element, org-src.el), polymode core, poly-org, and the chat-mode implementation modules.
2. **Hands-on tests** — small batch elisp scripts run via `./bin/emacs-isolated.sh --batch --quick`, parsing real and contrived buffers with `org-element-parse-buffer`, `org-at-heading-p`, `outline-next-heading`, `font-lock-ensure`, and inspecting `face` text properties.
3. **Web research** — polymode and poly-org GitHub issues; alternative-package surveys (mmm-mode, code-cells.el, edit-indirect, narrow-indirect.el, outshine, outli, org-special-block-extras, tree-sitter-org); LLM-chat Emacs packages (gptel, org-ai, ellama, chatgpt-shell, aidermacs).
4. **Hypothesis falsification** — every assumption that led to a design decision was tested against an actual buffer, not inferred.

Test scripts produced during research are in `/tmp/test-*.el`. They are not retained in the repo because they were exploratory; their findings (below) are.

## Verified findings (with evidence)

### Finding 1: Org has exactly one outline domain per buffer

`outline-regexp` is a single buffer-local string. Org's heading machinery (`org-element-parse-buffer`, `org-at-heading-p`, `outline-next-heading`, `org-cycle`, `org-fold-core`, agenda, refile, sparse-tree, imenu) all read this single regex. There is no "scope reset" syntax in org and no per-region override mechanism. **A `*` line at column 0 is always a level-1 heading of THE one document outline.**

This is architectural, not a missing feature.

### Finding 2: A leading space takes a `*` line out of heading recognition

The org heading regex is `^\\*+ ` — anchored at column 0. A single leading space at column 0 (so the `*` is at column 1) breaks the match. Empirically:

| Content | `org-at-heading-p` | `org-element-parse-buffer` says |
|---|---|---|
| `* Test` (col 0) inside `#+begin_user` | **`t`** | `paragraph` + `headline` + `paragraph` (block destroyed) |
| ` * Test` (col 1) inside `#+begin_user` | **`nil`** | `special-block` intact with `paragraph` inside |
| `* Test` (col 0) inside `#+begin_src emacs-lisp` | **`t`** | block destroyed |
| ` * Test` (col 1) inside `#+begin_src emacs-lisp` | **`nil`** | `src-block` intact |

This is **the whole mechanism** behind why org-babel users never observe this bug: `org-edit-src-content-indentation` (default `2`) re-indents all src-block content by 2 spaces on `org-edit-special` exit. The on-disk file has indented content; `*` is never at column 0; the parser is happy.

### Finding 3: The `,*` comma-escape also works

Org's documented mechanism for "this `*` is content, not a heading" is the comma-escape: `,* Heading` is parsed as paragraph content, the leading comma is stripped on tangle/export.

Both leading-space and `,*` give structural integrity. They differ in:

- **Leading-space** is what babel uses transparently. Less visually intrusive on disk.
- **`,*`** is the canonical documented escape. More universally portable across org tooling.

For chat-mode, leading-space is the more natural fit because nested tool blocks and src blocks inside chat blocks already need their own indentation, and a single uniform indent simplifies the round-trip story.

### Finding 4: Polymode does NOT solve multi-outline scoping

Hands-on test (run with a fresh polymode + poly-org clone, configured as org-host with org-as-inner for `#+begin_user` blocks) shows:

- `* Heading` inside the inner chunk: `org-at-heading-p` returns `t`, level 1.
- Base buffer view: same result.
- `org-element-parse-buffer` from inside the inner buffer: shows the heading at level 1 and the user special-block is **structurally absent** from the AST.

**Identical breakage to no-polymode.** Three independent reasons in polymode source:

1. `polymode-move-these-vars-from-base-buffer` (`polymode-core.el:999`) **copies `outline-regexp` and `outline-level` from base to inner buffers**. Inner buffers cannot have their own heading regex.
2. Inner buffers are **not auto-narrowed to their chunk** during normal editing. `pm-narrow-to-span` exists but is interactive and not used in the default flow; `pm-map-over-spans` calls `widen` first.
3. `poly-org.el:116` advises `org-element-at-point` to **always run on the base buffer**. The base buffer's parser sees the entire text including chunk contents.

The only public attempt at org-as-inner mode (issue #47, opened 2014, closed-stale 2018) reported headings being broken in exactly the way our test reproduces. The maintainer offered to add a "generic minor mode" but the work was never done. **Polymode's claim to "multiple modes in one buffer" is true for fontification, indentation, and per-chunk syntax tables; it is not true for outline scoping.**

### Finding 5: Polymode and poly-org maturity

- polymode core: 753 stars, 86 open issues, single primary maintainer (vspinu), ~1 commit/month cadence in 2025, no dated GitHub Releases. **Active but slow.**
- poly-org: 63 stars, 26 open issues against ~63 stars, last substantive fix Dec 2024, with critical open bugs around marker insertion and font-lock disable. **Thinner foundation.**

Maturity is sufficient for established uses (RMarkdown, Quarto). It is not sufficient for novel architectural bets in territory the maintainers have not validated — and the relevant territory (org-as-inner) is exactly the unvalidated case.

### Finding 6: No other Emacs package solves multi-outline scoping

Surveyed: mmm-mode, multi-mode (Eglen), narrow-indirect.el, edit-indirect, code-cells.el, org-edit-special, org-special-block-extras, outshine, outli, org-fold-core, jupyter.el / ein, tree-sitter-org. Of these:

- **`org-src.el` (org-edit-special) is the canonical mechanism that works**, but only on-demand: `C-c '` pops an indirect buffer narrowed to a src/example/export/comment block, with its own outline. Inside that buffer, full org features are native. This is "multi-outline implemented as one-outline-at-a-time, swap on demand."
- **mmm-mode and multi-mode** swap buffer-local variables; they do not narrow or scope structural parsing. Same outline limitation as polymode.
- **code-cells.el** does outline *demotion* (a `*` inside a cell becomes some level >1 in the global outline) but does not give a scoped fresh outline.
- **narrow-indirect / edit-indirect** are kernels; they expose Emacs's indirect-buffer mechanism to be triggered manually.

The unanimous finding: **the only mechanism in Emacs that genuinely scopes `outline-regexp` to a region is the indirect-buffer pattern.** Every "in-place visual scoping" path leads to either advising org primitives (fragile) or back to indirect buffers under the hood.

### Finding 7: Every other LLM-chat Emacs package avoided this problem

- **karthink/gptel**: when `gptel-org-branching-context` is enabled, the manual *advises switching from headings to `@user`/`@assistant` text markers* — the project explicitly hit and routed around this collision.
- **rksm/org-ai**: uses `[ME]:` / `[AI]:` markers inside `#+begin_ai` blocks. Turns are not heading-based.
- **chatgpt-shell**, **ellama**, **aidermacs**: comint-based or custom-format; not heading-based at all.

No surveyed package delivers "full org outside *and* full org inside chat blocks." Building it makes us first; that doesn't make it impossible, just first.

### Finding 8: With leading-space normalization, almost all org features Just Work

Hands-on test of nested blocks with leading-space normalization:

| Feature inside chat block | Works inline? |
|---|---|
| Nested `#+begin_tool ... #+end_tool` (fontification, fold) | **Yes** — assistant + tool special-blocks both intact in AST |
| Nested `#+begin_src python ... #+end_src` with native lang fontification | **Yes** — `org-src-fontify-natively` works |
| Tables, lists, links, emphasis, footnotes, timestamps, TODO markers | **Yes** |
| yasnippet / org snippets / tab expansion | **Yes** (none care about outline) |
| Properties drawers (free-floating) | **Yes** |
| `*` line as a **real navigable heading** (cycle / fold / imenu / sparse-tree / agenda) | **No** — requires multi-outline |
| `*` line **visually styled** as a heading (`org-level-N` face) | **No by default** — recoverable via custom font-lock rules |

The only thing that *requires* multi-outline is the last row. Visual styling is recoverable with ~30-50 lines of font-lock rules that match `^[ \t]+\\*+ ` inside chat block bodies and apply `org-level-N` faces. Functional heading affordances inside the main buffer view are not recoverable without indirect-buffer-scoped editing.

### Finding 9: `org-src--edit-element` is genuinely extensible

`org-src.el:547-650` defines the kernel: takes a `datum` (an org-element), a buffer name, a target major mode, and a write-back function. Pops an indirect-buffer-narrowed-to-the-element with that mode, and on exit re-applies the write-back transform.

`org-edit-comment-block` (`org-src.el:1297-1318`) is a 22-line implementation that uses this kernel to edit comment blocks in `org-mode`. **It is structurally identical to what we'd need for `gptel-chat-edit-turn`** — only the element-type check (`'comment-block` → `'special-block` with name `user`/`assistant`) and the buffer-name suffix differ.

The cost of building "edit a chat turn in a fresh org buffer with `C-c '`" is roughly **25-40 lines of elisp** that mirror `org-edit-comment-block`. The mechanism is production-tested and ships with org core.

## Surviving solution-space candidates

After eliminating polymode (Finding 4), all multi-mode frameworks (Finding 6), and "do nothing" (the bug breaks promised editing experience), three candidates remain. They share a common kernel: leading-space normalization on disk + `org-src--edit-element` for indirect editing. They differ only in *when* the indirect editing kicks in.

### Path α — On-demand indirect editing (lowest cost)

- **On-disk format inside chat blocks:** content indented by 2 (or configurable) spaces.
- **Inline editing in main buffer:** all features work *except* heading affordances. `*` lines are paragraph text; with optional custom font-lock rules they look like headings.
- **Heading work:** user invokes `C-c '` while inside a chat block. Indirect buffer pops with content un-indented; full org features including real heading machinery. `C-c '` again to exit; content re-indents and writes back.
- **Cost:** Smallest. ~25-40 lines for the edit command + ~30-50 lines for visual heading font-lock + indentation hooks for streaming/typing.
- **UX trade:** a mode switch (one keystroke) to do real heading work. Familiar to every org-babel user.

### Path γ — Auto-narrow on entry (inline-feeling)

- Same on-disk format and same kernel as α.
- **Inline editing:** when point enters a chat block body, transparently switch to (or display) the indirect buffer for that block, narrowed to the chat block content. Real heading machinery is active because we're literally in the indirect buffer. When point leaves, switch back to the host.
- **Cost:** Medium. Adds navigation glue (widen/renarrow on turn-to-turn motion), entry/exit hooks, and the inevitable edge cases at chunk boundaries.
- **UX trade:** "snapping" feel between widened (between turns) and narrowed (in turn) views. Has to be felt to know whether it's natural or jarring. Auto-narrow is reversible — failing this UX falls back to α.

### Path δ — Custom multi-outline machinery (largest scope)

- **No indirect buffers.** Reimplement heading recognition, fold, cycle, navigation, and outline-related commands specifically for chat-block-scoped `*` lines, in-place in the host buffer.
- **Cost:** Multi-week. Reinvents large pieces of org. Loses agenda / refile / imenu integration unless reimplemented.
- **UX trade:** Most native-feeling if it works. Highest maintenance burden. Highest risk of subtle org-version-skew breakage.

### Why no Path β (auto-comma-escape only)

Considered and rejected: auto-prefixing `,*` to all heading lines without indirect editing solves structural integrity but never gives the user real heading features. It's a narrower version of α with no upside vs α — the leading-space mechanism does the same job and is what babel already uses. β is dominated by α.

## Recommendation

**α first, with γ as the natural escalation if the UX requires it.**

Rationale:
- α delivers a coherent product immediately. Tool block fontification, snippets, all the org editing affordances inside chat blocks are inline. The one gap (real heading work) is one keystroke away via a mechanism every org user already knows.
- γ shares α's entire kernel — same on-disk format, same `org-src--edit-element` integration, same write-back. γ is α plus auto-trigger; if α ships first and γ later, no architecture is wasted.
- δ is justified only if (a) heading work *must* be inline without any mode switch, and (b) γ's auto-narrow UX proves unacceptable. Both should be falsified empirically before committing to δ scope.

## Open questions before OpenSpec

1. **Decision 13 reframe.** The archived design committed to "full org features inline." That promise is now known to be *incomplete by org-mode's architecture* for headings. Decision 13 needs an explicit revision: full org features inline, **with the exception of heading affordances which require `C-c '` (α) or auto-narrow (γ)**. This needs to be acknowledged as a refinement, not a regression — the original promise was unachievable.

2. **Indentation depth on disk.** `org-edit-src-content-indentation` defaults to 2. Chat blocks could pick anything. Affects only the on-disk look; doesn't change semantics. **Recommendation:** match babel (2) for cognitive consistency.

3. **Streaming insertion compatibility.** The streaming sanitizer (`gptel-chat--sanitize-chunk` in `config/gptel/chat/stream.org`) currently escapes only `^#\+end_\(user\|assistant\|tool\)\b`. Under leading-space normalization, **assistant-streamed lines need a leading indent applied per line** before insertion. Confirm that the existing streaming closure (the per-request marker / line holdback in `stream.org`) is the natural place to apply this.

4. **Existing-session migration.** Sessions on disk today are written without leading-space normalization. A migration path is needed: either `gptel-chat-mode` activation re-indents on read, or a one-time migration command sweeps the sessions directory. **Recommendation:** indent-on-read is simpler and reversible.

5. **Visual heading rendering in the host buffer.** Custom font-lock rules to paint `org-level-N` faces on leading-space `*` lines are achievable. Decide whether this is in-scope for the first OpenSpec or a follow-up.

6. **Path γ UX validation.** Whether auto-narrow feels good cannot be answered by source-reading. A prototype is required if γ is a serious candidate.

## Suggested next concrete action

A **two-day spike** that builds:

- A minimum `gptel-chat-edit-turn` command (mirrors `org-edit-comment-block`, ~30 lines).
- A `post-self-insert-hook` or `before-change-functions` filter that auto-prefixes a leading space when the user types `*` at column 0 inside a chat block.
- Streaming-side leading-space application for assistant chunks.
- Optional: an auto-narrow toggle (`gptel-chat-auto-narrow-on-entry`) so α and γ can be A/B compared in the same prototype.

Goal of the spike: feel both UXes in a real workflow before committing to one in an OpenSpec. Output: confidence in path selection plus a tested kernel that the OpenSpec implementation can build on.

## Files and references

### In this repo

- `config/gptel/chat/chat.org` — chat-mode loader.
- `config/gptel/chat/mode.org` — major mode definition; needs the auto-indent hook.
- `config/gptel/chat/parser.org` — parser; not affected (it's already structurally indifferent to indentation).
- `config/gptel/chat/stream.org` — streaming sanitizer; needs leading-space-on-line logic.
- `openspec/changes/archive/2026-04-24-gptel-chat-mode/design.md` — Decisions 4, 12, 13 are the relevant prior decisions.
- `openspec/specs/gptel/chat-mode.md` — current behavioral contract.
- `~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org` — original repro.

### Upstream

- `runtime/straight/repos/org/lisp/org-src.el` — `org-src--edit-element` (line 547), `org-edit-comment-block` (line 1297). The reference implementations to mirror.
- Polymode source: `https://github.com/polymode/polymode` — disqualified as a foundation; useful only for understanding what doesn't work.
- Poly-org source: `https://github.com/polymode/poly-org` — same.
- Issue #47 (poly-org headings broken): `https://github.com/polymode/polymode/issues/47` — the closed-stale precedent that should be cited if anyone proposes polymode again.

### Web research

- Mastering Emacs, "Polymode": configuration walk-through; does not address scoped outlines.
- Polymode docs: `https://polymode.github.io/concepts/`, `https://polymode.github.io/synchronization/`.
- karthink/gptel `gptel-org-branching-context` documentation: confirms the heading-collision problem is acknowledged upstream and worked around by switching turn delimiters away from headings.
- rksm/org-ai README: confirms the convention of using non-heading delimiters inside content blocks.

## What we are NOT doing

- Modifying upstream org. The fix lives entirely in `config/gptel/chat/`.
- Adopting polymode or any multi-mode framework as a foundation.
- Promising "outline scope reset" as a feature; that does not exist in org-mode and we are not building it.
- Rewriting the on-disk format of existing sessions destructively. Migration is read-time, reversible.

## Decision log for the next maintainer

If you pick up this work, the path through is:

1. Read this document end-to-end.
2. Skim `org-src.el:547-650` (`org-src--edit-element`) and `org-src.el:1297-1318` (`org-edit-comment-block`) — those are the kernel and the prototype.
3. Build the spike (suggested next action above).
4. Use the spike to make the α-vs-γ call.
5. Open an OpenSpec change with the picked path. Reuse the open questions above as a starting `open-questions.md`. Revise Decision 13 explicitly in the change's design.md.
6. The implementation should reuse the existing `,#+end_*` sanitize/unescape symmetry pattern in `stream.org` / `parser.org` — leading-space normalization is the same idiom one layer up.

If the spike reveals the UX is unworkable for both α and γ, the conversation moves to δ (custom multi-outline) — but only at that point, with empirical evidence that the cheaper paths fail.
