---
name: document-typed-escape-single-char-scope
description: Document in design.md Decision 2 that typed-escape covers only single-char keystroke path; multi-char paths (C-u N *, repeat, paste, query-replace) fall through to paste-escape on after-change-functions. Add direct-call test exercising the predicate's delimiter-line rejection at the typed-escape's surface.
change: gptel-chat-heading-scoping
status: done
merge_commit: 412dc88
inline_fix_commit: 185fe2d
relations:
  - discovered-from:add-user-typed-heading-escape
  - blocked-by:widen-paste-escape-gate-to-cover-replacements
---

## Files to modify

- `openspec/changes/gptel-chat-heading-scoping/design.md` (Decision 2 prose — add scope-and-fall-through explanation)
- `config/gptel/chat/test/mode/user-typed-escape-spec.el` (add direct-call delimiter-line test; optionally clean up the existing test's misleading comment)

Explicitly NOT modified:
- `config/gptel/chat/mode.el` / `mode.org` — no behavior change. Typed-escape stays narrow to the single-char keystroke path.
- `interfaces.org` register entries — the boundary contract ("every column-0 `*` line in a chat-block body is escaped on every write path") is unchanged. The contract is upheld by the *combination* of producers (typed-escape + paste-escape + sanitize-chunk + migration), and that decomposition is documented in `register/boundary/chat-heading-collision-escape` already.

## Implementation steps

### Part A — design.md Decision 2 documentation

1. Locate Decision 2 in `design.md` (the post-self-insert-hook decision).
2. Add a new subsection titled "Scope: single-char keystroke path only" (or similar) with prose along these lines:
   > The typed-escape function fires once per `self-insert-command` invocation, with `last-command-event` set to the triggering character. Its column-1 guard correctly catches the dominant single-char path (user types `*` at column 0). It deliberately does NOT cover multi-char insertion paths:
   >
   > | Path | Caught by |
   > |---|---|
   > | Single `*` keystroke | typed-escape (this hook) |
   > | `C-u N *` (prefix-arg multi-insert) | paste-escape on `after-change-functions` |
   > | `M-x repeat` of a `*` keystroke | paste-escape on `after-change-functions` |
   > | Yank / paste of `* H1` | paste-escape on `after-change-functions` |
   > | `query-replace foo → * H1` | paste-escape on `after-change-functions` (after gate widening from ask-cycle-1777624502-4 follow-up; ordering implies that follow-up should land first) |
   >
   > Hook firing order: `self-insert-command` calls `insert` which fires `after-change-functions` first; `post-self-insert-hook` runs after. Paste-escape (on `after-change-functions`) therefore catches column-0 `*` lines from any insertion path, then point shifts past the inserted prefix and typed-escape's column-1 guard naturally rejects on the same call. No double-escape.
   >
   > Rationale for keeping typed-escape narrow: the function runs every keystroke in chat-mode buffers. Widening its scan (to inspect the triggering region rather than `(current-column)`) would add work to the hot path and duplicate paste-escape's logic. The producer-decomposition (cheap hook for the dominant path; general hook for everything else) keeps each producer's correctness localizable.
3. Add a "Coupling" note: this scope decision is satisfied by the paste-escape gate widening from ask-cycle-1777624502-4 (see `widen-paste-escape-gate-to-cover-replacements.md`). After the gate widens from `(zerop length)` to `(> end beg)`, paste-escape covers ALL non-keystroke insertion paths into a chat-block body — including `query-replace`-shaped replacement events. The producer-decomposition (typed-escape for the cheap dominant path; paste-escape for everything else) is then locally inspectable: the boundary contract is upheld by the union of the two producers, not silently dependent on either being "wide enough" to cover the other's residuals.

### Part B — direct-call delimiter-line test

4. In `config/gptel/chat/test/mode/user-typed-escape-spec.el`, around the existing "does not escape on a delimiter line" describe-block (line ~102), add a sibling spec that calls `gptel-chat--escape-typed-heading` directly (bypassing `self-insert-command`) so the column-1 guard succeeds and the predicate's delimiter-line rejection is the load-bearing check.
5. Sketch:
   ```elisp
   (it "does not escape when called with point on a delimiter line (predicate enforcement)"
     (with-temp-buffer
       (gptel-chat-mode)
       (insert "#+begin_user\nhi\n#+end_user\n")
       (goto-char (point-min))
       (forward-char 1)              ; point at column 1 of "#+begin_user"
       (let ((last-command-event ?*)
             (before (buffer-string)))
         (gptel-chat--escape-typed-heading)
         (expect (buffer-string) :to-equal before))))
   ```
   Adjust to local conventions if buttercup setup helpers exist.
6. Optionally update the existing line-102 test's embedded comment (lines 120-135) to clarify that it exercises the *column-1 guard's* short-circuit, while the new sibling test exercises the *predicate's* delimiter-line rejection — together they cover both arms.

## Design rationale

Two failure modes prompted this task, both surfaced by the same review:

1. **Prefix-arg multi-char self-insert** is silently covered by paste-escape today, but the design didn't say so. The brief and Decision 2 read as if typed-escape was the sole producer for typed paths. Documenting the producer-decomposition makes the boundary's correctness inspectable: a future maintainer reading Decision 2 can see *why* typed-escape's column-1 guard is sufficient (because paste-escape exists, not because multi-char paths are impossible).

2. **Predicate-call IS the enforcement** (per `register/invariant/chat-block-delimiter-lines-stay-at-column-0`) — but the existing delimiter-line test never reaches the predicate. A future maintainer dropping the third condition would not break any test. Adding one direct-call test gives the invariant a witness at the function's surface, not just at the predicate's standalone tests in `parser.el`.

Why fold both into one task: same file, same review cycle, same context. Two-task split would multiply orchestrator overhead without adding clarity.

## Verification

- `grep -n "single-char keystroke" openspec/changes/gptel-chat-heading-scoping/design.md` shows the new prose.
- `grep -n "Coupling" openspec/changes/gptel-chat-heading-scoping/design.md` shows the ask-4 dependency note.
- `grep -n "predicate enforcement\|delimiter-line rejection" config/gptel/chat/test/mode/user-typed-escape-spec.el` shows the new test.
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes (existing tests + new one).
- Behavioral smoke: in a fresh emacs, open a chat buffer, type `C-u 3 *` at column 0 of an assistant body, confirm the buffer ends up with the column-0 `*` escaped (by paste-escape, not typed-escape; the user sees this as ` ***` rather than `***`).

## Context

- Resolves user ask `ask-cycle-1777624502-3` (see `.orchestrator/handshake-cycle-1777624502.json` `asks_for_user_resolved`).
- Reviewer findings: `.orchestrator/cycles/cycle-1777624502/reviews/add-user-typed-heading-escape.md` Findings 1 and 2.
- Coupled with ask `ask-cycle-1777624502-4`: the documented exclusion is correct iff paste-escape's gate is widened from `(zerop length)` to `(> end beg)`. Resolution: ask-4 accepted Option B (gate widening); follow-up `widen-paste-escape-gate-to-cover-replacements.md` must land before this task to make the documented exclusion truthful. Encoded as `blocked-by:` in frontmatter.
- Original task: `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-user-typed-heading-escape.md` (status: done).
- Merged code: `config/gptel/chat/mode.el:304-335` (typed-escape function); `config/gptel/chat/parser.el:169-264` (predicate).

## Observations

- Decision 2 prose extended with a "Scope: single-char keystroke path only" subsection (between the existing Implications bullets and Decision 3). Subsection includes: (a) the producer-decomposition table mapping each insertion path to its catching producer; (b) hook firing-order explanation showing why no double-escape occurs (after-change-functions runs before post-self-insert-hook; paste-escape's prefix shifts point past column 1); (c) rationale for keeping typed-escape narrow (hot-path cost, avoid duplicating paste-escape's logic); (d) Coupling note tying this scope decision to the now-closed `widen-paste-escape-gate-to-cover-replacements` task (path corrected from `tasks/open/` to `tasks/closed/` to reflect post-merge state of the orchestrator cycle).
- Direct-call delimiter-line spec added as a sibling to the existing column-1-guard spec (lines ~108-152 of the test file). The new spec calls `gptel-chat--escape-typed-heading` directly with point at column 1 of `#+begin_user`, `last-command-event` bound to `?*`, and asserts buffer-string is unchanged. This makes the predicate's delimiter-line arm load-bearing; without that arm the test would fail (the column-1 guard alone would not catch it).
- The existing column-1-guard spec's embedded comment was rewritten to clearly call out which arm it exercises (the `(= (current-column) 1)` short-circuit) and to cross-reference the new sibling spec as the one exercising the predicate arm. Both specs now explicitly cite `register/invariant/chat-block-delimiter-lines-stay-at-column-0`.
- No mode.el / mode.org / interfaces.org / parser.el changes — strictly within the documented scope (design.md + test file only).
- Tests not run in this worktree (runtime/ is not initialised here per orchestrator convention); orchestrator runs the suite after merge.

## Discoveries

- (none)
