---
name: decision-3b-review-followups
description: Tighten Decision 3b prose and reconcile related doc/spec artifacts per the revisit-decision-3b review
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:revisit-decision-3b-tool-marker
---

## Files to modify
- `openspec/changes/gptel-chat-mode/design.md` (Decision 3b,
  Decision 10 cross-reference)
- `openspec/changes/gptel-chat-mode/architecture.md` (line ~116,
  `gptel-chat--make-stream-closure` row)
- `openspec/changes/gptel-chat-mode/tasks/closed/expose-tool-marker-setter.md`
  (prune Options A and C)

## Implementation steps

This task groups five findings from the `revisit-decision-3b-tool-marker`
adversarial review. Address them together since they all touch the same
design/spec cluster and reference each other.

1. **(Finding #1, blocking) Per-chunk vs per-insert routing claim.**
   `design.md` Decision 3b step 3 (currently around line 132) says:
   "The routing choice is made per chunk via
   `gptel-chat--stream-active-marker`, a pure helper directly
   unit-testable." The implementation resolves the active marker
   **once per `insert` invocation** and iterates `dolist` over the
   complete lines with that captured value. Choose one of:
   - Rewrite the prose: "The routing choice is resolved **once per
     `insert` invocation** via `gptel-chat--stream-active-marker`,
     a pure helper directly unit-testable. A single `insert` call's
     routing applies to every complete line within that call."
   - OR move the helper call inside the `dolist` in stream.el so
     the prose is accurate as written. Only worthwhile if there is
     a real reason to reroute mid-chunk; current callers receive
     one wire event at a time, so the per-insert framing is the
     honest one.

   Pick the prose rewrite unless there's a concrete downstream
   need for per-line re-resolution.

2. **(Finding #6, folded into #1) Sequencing invariant between
   Decisions 3b and 10.** Immediately after the routing-resolution
   sentence, add: "`stream-callback` MUST issue any
   `set-tool-marker` / `clear-tool-marker` call *between* distinct
   `insert` calls. Tool-marker changes apply from the next `insert`
   call onward; they never affect the routing of the in-flight
   call." In Decision 10 (dispatch), add a one-line cross-reference
   to Decision 3b's sequencing invariant so readers of Decision 10
   know to interleave routing changes between `insert` calls, not
   within.

3. **(Finding #2, blocking for review accuracy) Frame B rejection
   prose.** `design.md` Decision 3b Alternatives bullet for
   "Caller-owned mutable cell" (around line 140) rejects Frame B
   with two arguments: (a) it "violates Decision 3b's core premise
   that text-processing state is *per-send* and lives with the
   closure"; and (b) "costs one `car` indirection per chunk in the
   hot path". (a) is circular — the "core premise" is asserted in
   the same decision being revised, so rejecting an alternative
   *by* that premise is question-begging. (b) is weak — splitting
   a string and writing to a marker dwarfs a `car` deref by orders
   of magnitude. Replace both with the real argument:

   > *Caller-owned mutable cell* (factory takes a `(list nil)`
   > tool-marker cell, closure reads `(car cell)` each chunk).
   > Technically works, and lifetime can be kept per-send by the
   > caller. Rejected because the wiring contract — "the `car` of
   > this cell is the routing marker" — is an un-named, undocumented
   > protocol every call site must learn separately. The
   > `cl-defstruct` alternative names the protocol explicitly via
   > generated accessors (`gptel-chat-stream-set-tool-marker`,
   > `gptel-chat-stream-insert`) and a generated predicate
   > (`gptel-chat-stream-p`), so call sites and tests both get
   > typed, self-documenting access. That is the core win, not
   > lifetime coupling or indirection cost.

4. **(Finding #3) architecture.md drift.**
   `architecture.md` line 116 currently describes
   `gptel-chat--make-stream-closure` as a "Holdback-bearing closure
   factory; owns partial-line state and marker-based insertion".
   After `expose-tool-marker-setter` merged, the factory returns a
   `gptel-chat-stream` cl-struct, not a closure. Rewrite the
   Purpose column to: "Returns a `gptel-chat-stream` handle
   (cl-struct) exposing `insert`, `set-tool-marker`, and
   `clear-tool-marker`. Owns holdback and partial-line state via
   the underlying closure bound to the `insert` slot." Do NOT rename
   the function here — the rename is owned by the existing
   `rename-make-stream-closure` task.

5. **(Finding #4) `expose-tool-marker-setter.md` stale options.**
   The closed task file at
   `tasks/closed/expose-tool-marker-setter.md` still reads "Choose
   ONE shape" and lists Options A (plist), B (cl-struct), and C
   (caller-owned cell) at lines 22-33. This contradicts
   `design.md:146` which states Option B is the authoritative
   framing. Retroactively amend step 2 of that task file so it
   reads:

   > 2. Refactor `gptel-chat--make-stream-closure` to return a
   >    `cl-defstruct gptel-chat-stream` with
   >    `insert`, `set-tool-marker`, and `clear-tool-marker` slots,
   >    per Decision 3b. Options A (plist) and C (caller-owned
   >    cell) previously listed here are superseded — see
   >    `design.md` Decision 3b Alternatives.

   This is a retroactive spec-alignment edit of a closed task file.
   It does not re-open or re-execute the task.

## Design rationale
Grouping the five findings keeps the cross-references consistent:
#1 and #6 both amend the same Decision 3b step; #2 amends the
adjacent Alternatives block; #3 and #4 are the downstream artifacts
that must be kept in sync with the tightened decision. Splitting
them would produce five small diffs that each have to re-read the
surrounding context. One task, one coherent patch.

## Verification
- `design.md` Decision 3b routing-resolution sentence says "per
  `insert` invocation" (not "per chunk") and is followed by the
  sequencing invariant for `set-tool-marker` /
  `clear-tool-marker`.
- `design.md` Decision 10 contains a cross-reference to Decision
  3b's sequencing invariant.
- `design.md` Decision 3b Alternatives bullet for caller-owned
  cell uses the named-protocol / typed-accessor argument (no
  circular-premise wording, no hot-path cost argument).
- `architecture.md` row for `gptel-chat--make-stream-closure` no
  longer says "closure factory"; it describes the struct return.
- `tasks/closed/expose-tool-marker-setter.md` step 2 names Option
  B (cl-struct) as authoritative and marks A and C as superseded
  with a pointer to Decision 3b.
- `grep -n "per chunk" openspec/changes/gptel-chat-mode/design.md`
  has no matches in Decision 3b's routing-resolution sentence.
- `grep -n "closure factory" openspec/changes/gptel-chat-mode/architecture.md`
  returns no matches.

## Context
- Adversarial review of `revisit-decision-3b-tool-marker`
  (orchestrator session `orch-1776713064`, 2026-04-20). Findings
  #1 and #2 were flagged blocking; #3, #4, and #6 are
  doc-alignment follow-ups folded in per user direction.
- Parent task at
  `openspec/changes/gptel-chat-mode/tasks/closed/revisit-decision-3b-tool-marker.md`
  stays at `status: needs-review` until this task closes.
