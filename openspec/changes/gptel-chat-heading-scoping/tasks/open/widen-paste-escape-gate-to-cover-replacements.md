---
name: widen-paste-escape-gate-to-cover-replacements
description: Widen gptel-chat--escape-inserted-headings gate from (zerop length) to (> end beg) so replacement-shaped after-change-functions events (query-replace, replace-match, replace-region-contents) that introduce a column-0 * inside a chat-block body are escaped. Correct false rationale in docstring, org commentary, and design.md Decision 3. Add buttercup scenario covering the replacement path.
change: gptel-chat-heading-scoping
status: needs-review
merge_commit: 537f34f
relations:
  - discovered-from:add-paste-heading-escape
  - enables:document-typed-escape-single-char-scope
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
  - Gate at the top of `gptel-chat--escape-inserted-headings`
  - Docstring lines (~578-583) that claim deletions/replacements cannot introduce column-0 `*` lines
  - Org commentary above the function (~mode.org:404-406) with the same false rationale
- `openspec/changes/gptel-chat-heading-scoping/design.md` (Decision 3 prose)
- `config/gptel/chat/test/mode/paste-escape-spec.el` (add scenario for replacement-shaped event)
- `openspec/changes/gptel-chat-heading-scoping/tasks/open/document-typed-escape-single-char-scope.md` (update its Coupling note from "depends on" to "satisfied by" once this task lands)

## Implementation steps

1. **Edit the gate** in `config/gptel/chat/mode.org` (the `gptel-chat--escape-inserted-headings` block). Change:
   ```elisp
   (when (and (zerop length)
              (> end beg))
     ...)
   ```
   to:
   ```elisp
   (when (> end beg)
     ...)
   ```

2. **Rewrite the docstring** (currently `mode.el:576-611` after tangle). The first paragraph claims:
   > "When LENGTH is 0 (pure insertion, not a deletion or replacement), walk the inserted range line by line."

   Replace with a phrasing along these lines:
   > "When the change region is non-empty (END > BEG), walk it line by line and escape column-0 `*' lines that fall inside a chat-block body. Pure deletions (END = BEG) gate out; insertions, including replacements (LENGTH > 0 AND END > BEG via `replace-match' / `replace-region-contents' / `query-replace'), flow through. Idempotence holds: an already-escaped line (whitespace before `*') fails the per-line `\\*+ ' regex, so re-running on previously-escaped content is a no-op."

   Drop or rewrite any sentence asserting that "deletions and replacements cannot introduce new column-0 `*` lines that were not already in the buffer" — it is false for replacements.

3. **Rewrite the org commentary** in `mode.org` (the prose block above the function, currently around lines 392-411 in source — locate the section discussing the LENGTH=0 choice). Same correction: replacements CAN introduce new column-0 `*` lines (e.g., `query-replace foo → * H1`); the gate's job is to skip pure deletions, not all non-pure-inserts.

4. **Rewrite design.md Decision 3** rationale. Currently asserts that LENGTH=0 is the only case worth handling. Replace with the corrected analysis:
   - The hook fires for any `after-change-functions` event whose [BEG, END) is non-empty.
   - Pure deletions (END = BEG) don't introduce content; gate out.
   - Insertions and replacements both introduce content into [BEG, END); gate in.
   - Per-line check `(looking-at "\\*+ ")` short-circuits cheaply for content without column-0 `*`.
   - Predicate consultation per matched line ensures only chat-block-body lines are escaped (delimiter lines, between-blocks prose, outside-any-block all return nil).
   - `inhibit-modification-hooks` is bound in the let head before any rewrite, so widening the input gate does not widen re-entry.
   - Cross-reference: this resolves the gap noted in `register/invariant/all-write-paths-apply-heading-escape` for replacement-shaped events; closes ask-cycle-1777624502-4.

5. **Add a new buttercup scenario** in `config/gptel/chat/test/mode/paste-escape-spec.el`, near scenario 7 (the deletion-defensive test). Sketch:
   ```elisp
   (it "escapes column-0 * introduced via replacement (query-replace path)"
     (gptel-chat-paste-test--with-chat-buffer
         (concat "#+begin_user\n"
                 "some foo here\n"
                 "#+end_user\n")
       (goto-char (point-min))
       (search-forward "foo")
       ;; Simulate query-replace's atomic replace: delete-and-insert
       ;; in one event. replace-match would fire after-change-functions
       ;; with LENGTH=3 (length of "foo") AND END > BEG (length of
       ;; replacement). The simplest deterministic way to trigger that
       ;; without a real query-replace UI is replace-match after a
       ;; search:
       (replace-match "* H1" t t)
       (expect (buffer-string)
               :to-equal
               (concat "#+begin_user\n"
                       "some * H1 here\n"      ; mid-line — no escape
                       "#+end_user\n"))))

   (it "escapes column-0 * when replacement spans BOL (multi-line replacement)"
     (gptel-chat-paste-test--with-chat-buffer
         (concat "#+begin_user\n"
                 "foo\n"
                 "trailing\n"
                 "#+end_user\n")
       (goto-char (point-min))
       (search-forward "foo")
       (beginning-of-line)
       (let ((beg (point)))
         (forward-line 1)
         (delete-region beg (point))
         (insert "* H1\n"))                      ; emulates a replacement that lands a col-0 *
       (expect (buffer-string)
               :to-equal
               (concat "#+begin_user\n"
                       " * H1\n"                 ; escaped by widened gate
                       "trailing\n"
                       "#+end_user\n"))))
   ```
   Adjust to local conventions if buttercup setup helpers exist. The first case is the canonical `query-replace` shape (single mid-line replace); the second exercises the load-bearing case (replacement that introduces a column-0 `*` at BOL).

   Note: Emacs' `replace-match` after `search-forward` does fire `after-change-functions` with `LENGTH > 0` — that is the contract under test. If a buttercup convention requires direct hook simulation rather than indirect via `replace-match`, the scenario can call `gptel-chat--escape-inserted-headings` directly with synthesised arguments; the more indirect form above is preferable because it tests the actual contract surface.

6. **Update the coupled task** (`document-typed-escape-single-char-scope.md`):
   - In the "Coupling" note, change "this scope decision is load-bearing on the paste-escape gate widening from ask-cycle-1777624502-4" to "this scope decision is satisfied by the paste-escape gate widening from ask-cycle-1777624502-4 (see `widen-paste-escape-gate-to-cover-replacements.md`)".
   - Add `blocked-by:widen-paste-escape-gate-to-cover-replacements` to its frontmatter `relations:` list.

## Design rationale

The `(zerop length)` gate was prescribed by the brief and faithfully implemented, but the brief's underlying assumption — "deletions and replacements cannot introduce new column-0 `*` lines that were not already in the buffer" — is incorrect for replacements. `query-replace foo → * H1` inside a chat-block body is a perfectly normal user action that violates `register/invariant/chat-block-body-no-column-zero-stars` and corrupts the parser's send path. The gap is not closed by any other producer in-session (streaming, typed-escape, and migration each cover orthogonal paths).

Widening the gate is safe per the per-line short-circuits already in place: `looking-at` is idempotent on already-escaped content; the predicate is sub-millisecond and returns nil for non-body positions; `inhibit-modification-hooks` is already bound. The cost is a single regex match per line in the change region for replacements that introduce no column-0 `*` — negligible.

Why option (b) over option (a) (document the exclusion): option (a) leaves a real within-session corruption window. Migration runs only at mode activation, so a `query-replace` mid-session corrupts the buffer until the next file-open. The user-visible symptom is scrambled LLM output. The boundary register's "every write path" contract should be enforceable at the function level, not deferred to a quasi-periodic migration.

## Verification

- `grep -n "zerop length" config/gptel/chat/mode.el` returns no matches in `gptel-chat--escape-inserted-headings`.
- `grep -n "deletions and replacements cannot introduce" config/gptel/chat/mode.org config/gptel/chat/mode.el` returns no matches.
- `grep -n "query-replace\|replace-match\|replace-region-contents" config/gptel/chat/test/mode/paste-escape-spec.el` shows the new scenario(s).
- `grep -n "Decision 3" openspec/changes/gptel-chat-heading-scoping/design.md` shows the updated rationale (no longer claiming LENGTH=0 is the only case worth handling).
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes (existing 8 scenarios + new replacement scenario(s)).
- Behavioral smoke: in a fresh emacs, open a chat buffer with `* foo` text inside a `#+begin_user` body (well, place `foo` and run `query-replace foo → * H1`), confirm the resulting `* H1` at column 0 gets a leading space.

## Context

- Resolves user ask `ask-cycle-1777624502-4` (see `.orchestrator/handshake-cycle-1777624502.json` `asks_for_user_resolved`).
- Reviewer Finding 2: `.orchestrator/cycles/cycle-1777624502/reviews/add-paste-heading-escape.md`.
- Finding 1 from the same review (test fixture vs migration interaction) already resolved by architect-fix commit `0ffcdb5` — no action needed here.
- Enables: `document-typed-escape-single-char-scope.md` (its documented exclusion presumes paste-escape covers all insertion paths, which holds after this gate widening).
- Original task: `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-paste-heading-escape.md` (status: done).
- Boundary contract: `interfaces.org` — `register/invariant/all-write-paths-apply-heading-escape`.
- Merged code: `config/gptel/chat/mode.el:574-624`.

## Observations

- Step 6 (the coupled task `document-typed-escape-single-char-scope.md` update) was already complete on arrival: the frontmatter `relations:` list already contained `blocked-by:widen-paste-escape-gate-to-cover-replacements`, and the body's Coupling note already used "satisfied by" wording (lines 39, 82). No edit was required to the coupled task; the implementation note here records that fact for the reviewer.
- The gate change itself is one-line: `(when (and (zerop length) (> end beg)) ...)` → `(when (> end beg) ...)`. The bulk of the diff is in surrounding documentation (function docstring, org commentary "Why after-change-functions" subsection, design.md Decision 3 prose) and three new buttercup scenarios.
- Three new buttercup scenarios were added (the task body sketched two; I expanded to three for clearer coverage):
  - **Scenario A** ("leaves mid-line replacement alone"): exercises a single `replace-match` event in a body with no column-0 `*` — confirms the widened gate fires but the BOL >= BEG mid-line guard correctly leaves the result unmolested. This is the canonical `query-replace foo → * H1` shape from the task body.
  - **Scenario B** ("escapes column-0 `*` introduced via single-event replacement (replace-match) at BOL"): the load-bearing case — a single replacement event whose match starts at column 0 of a body line, lands a column-0 `*` that must be escaped. Without the widening, the original `(zerop length)` gate would short-circuit on `LENGTH = 3` and leave the column-0 `*` in place. This is the test that would FAIL on the un-widened code.
  - **Scenario C** ("escapes column-0 `*` on later lines of a multi-line replacement"): a single replacement event whose replacement spans two lines, the second of which starts with `*` at column 0. Mirrors scenario 1's multi-line yank shape but via `replace-match` rather than `insert`. Validates that the per-line scan inside the helper handles multi-line replacements correctly when fed through the widened gate.
- The deletion-defensive scenario's docstring/comment was updated from "deletion (LENGTH > 0)" to "pure deletion (END = BEG)" to match the new gate's gate predicate. The test mechanism is unchanged (still `delete-region` of `trailing`).
- The mode.org "Why after-change-functions" subsection was rewritten to (a) describe the new `(> end beg)` gate, (b) explicitly call out the replacement case as load-bearing (with `query-replace foo → * H1` as the worked example), and (c) cross-reference `register/invariant/chat-block-body-no-column-zero-stars` so a reader knows which invariant the widening is upholding.
- The function docstring was rewritten to match: leads with "non-empty change regions", describes the deletion / insertion / replacement trichotomy, calls out idempotence via the per-line `\\*+ ` regex on already-escaped lines, and explicitly notes that `inhibit-modification-hooks` is bound BEFORE any rewrite so widening the input gate does not widen re-entry.
- design.md Decision 3 was rewritten with a tabular trichotomy (deletions / insertions / replacements), an explicit "Alternatives considered" entry for the original `(zerop length)` gate (recording why it was rejected), and a cross-reference to the boundary register entry. The "Implications" subsection was updated to note the predicate runs once per matched line (not once per change), which is the actual cost.
- Tangle output: `Tangled 13 code blocks from mode.org` then `Validation passed`. No paren errors.
- Tests not run from this worktree per the task brief (`runtime/` is not initialised here); orchestrator runs the full suite at merge time.

## Discoveries

- (none)

