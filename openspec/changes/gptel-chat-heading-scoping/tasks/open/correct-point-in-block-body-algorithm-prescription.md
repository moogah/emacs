---
name: correct-point-in-block-body-algorithm-prescription
description: Replace unsound naive-scan algorithm prescription in the predicate task brief and design.md with the merged stack-walk algorithm; remove bogus gptel-chat--block-opener-regexp reference
change: gptel-chat-heading-scoping
status: needs-review
merge_commit: 3b604e4
relations:
  - discovered-from:add-point-in-block-body-predicate
---

## Files to modify

- `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-point-in-block-body-predicate.md` (closed task brief — step 2 algorithm prescription and Discoveries annotation)
- `openspec/changes/gptel-chat-heading-scoping/design.md` (line 45 prose describing the predicate algorithm)

Explicitly NOT modified:
- Wave-2 briefs (`add-user-typed-heading-escape`, `add-paste-heading-escape`, `add-migration-on-read`) — they already cite `gptel-chat--point-in-block-body-p` by name, which is the correct level of encapsulation. Surfacing the internal `gptel-chat--re-block-delimiter` regex to consumers would invert the encapsulation.
- `interfaces.org` register entries — they describe the behavioral contract, not the algorithm. The contract ("POS strictly inside a chat-block body") is unchanged.

## Implementation steps

1. Open `tasks/open/add-point-in-block-body-predicate.md`. Step 2 currently reads:
   > "scan backward from POS using `re-search-backward` against the existing `gptel-chat--block-opener-regexp` (or define a small helper if it doesn't already exist) to find the nearest `#+begin_*` opener. Then scan forward from POS for the nearest `#+end_*` closer. Return non-nil iff opener exists, closer exists, and POS lies strictly between..."

   Replace with the canonical algorithm:
   > "Define a delimiter-union regex `gptel-chat--re-block-delimiter` matching `^#\\+\\(begin\\|end\\)_\\(user\\|assistant\\|tool\\)\\b` (group 1 = side, group 2 = kind). Walk backward from POS through delimiter lines maintaining a closer-stack: each `#+end_*` pushes (stack += 1); each `#+begin_*` pops if stack non-empty (matched closed pair) or stops as the enclosing opener if stack is empty. Then forward-scan for the matching closer of the same kind, using a same-kind depth counter so a nested same-kind block does not fool the closer match. Return non-nil iff POS lies strictly past the opener's end-of-line and before the closer's beginning-of-line."

   Add a brief sentence explaining *why* the naive scan is unsound, with a one-line counter-example: "A buffer with a closed `#+begin_assistant ... #+begin_tool ... #+end_tool ... #+end_assistant` followed by between-blocks prose: naive backward scan finds `#+begin_tool` (already closed); naive forward scan finds the next block's `#+end_user`; predicate falsely returns non-nil for between-blocks POS."

2. Remove the bogus `gptel-chat--block-opener-regexp` symbol reference from the brief (it does not exist in `parser.el`; the closest real symbol is `gptel-chat--re-outer-opener`, but neither is what the predicate uses).

3. Add a "Resolution" annotation under "Discoveries" in the same brief: "Algorithm prescription corrected by ask-cycle-1777624502-2. The merged stack-walk algorithm is the canonical implementation; the original step 2 prescription was unsound."

4. Open `design.md` line 45. The prose currently reads:
   > "We provide a fast `gptel-chat--point-in-block-body-p` helper that scans backward for the nearest `#+begin_*` / `#+end_*` line (line-by-line `re-search-backward`), bounded by a small look-back window or by `point-min`."

   Replace with:
   > "We provide a fast `gptel-chat--point-in-block-body-p` helper that walks backward through `#+begin_*` / `#+end_*` delimiter lines maintaining a closer-stack (each `#+end_*` pushes; each `#+begin_*` pops if non-empty, otherwise it is the enclosing opener), then forward-scans for the matching closer using a same-kind depth counter. The naive 'nearest opener backward / nearest closer forward' algorithm is unsound for buffers with closed inner blocks (a closed `#+begin_tool ... #+end_tool` upstream of POS would be misread as the enclosing opener). The stack-walk is sub-millisecond on typical chat buffers — bounded by the number of delimiter lines, not buffer size."

5. Sanity-check: re-run `./bin/run-tests.sh -d config/gptel/chat` to confirm no regressions (artifact-only changes shouldn't affect tests, but the change is cheap to verify).

## Design rationale

The merged code is correct. This task only fixes documentation/artifact drift so that:
(a) future LLM-driven re-implementation in another cycle does not regenerate the broken algorithm by reading the brief;
(b) the design.md prose accurately describes the predicate's actual behavior;
(c) the closed task brief carries provenance pointing at the resolution decision.

The reviewer's broader recommendation (cite `gptel-chat--re-block-delimiter` in wave-2 briefs) is deliberately rejected. Wave-2 consumers reach the predicate by its public name; the union regex is an internal detail. Pushing the regex into consumer briefs would be a layering inversion — consumers would then "know" the predicate's algorithm and could be tempted to bypass it, breaking the encapsulation that makes the stack-walk correctness localizable.

## Verification

- `grep -n "block-opener-regexp" openspec/changes/gptel-chat-heading-scoping/` returns no matches.
- `grep -n "stack-walk\|closer-stack\|delimiter-union" openspec/changes/gptel-chat-heading-scoping/tasks/open/add-point-in-block-body-predicate.md openspec/changes/gptel-chat-heading-scoping/design.md` shows the new prose in both files.
- Wave-2 briefs unchanged: `git diff openspec/changes/gptel-chat-heading-scoping/tasks/open/add-user-typed-heading-escape.md openspec/changes/gptel-chat-heading-scoping/tasks/open/add-paste-heading-escape.md openspec/changes/gptel-chat-heading-scoping/tasks/open/add-migration-on-read.md` returns no output.
- `./bin/run-tests.sh -d config/gptel/chat` still passes.

## Context

- Resolves user ask `ask-cycle-1777624502-2` (see `.orchestrator/handshake-cycle-1777624502.json` `asks_for_user_resolved`).
- Reviewer finding: `.orchestrator/cycles/cycle-1777624502/reviews/add-point-in-block-body-predicate.md` Finding 1.
- Merged primitives: `config/gptel/chat/parser.el:63` (`gptel-chat--re-block-delimiter`), `config/gptel/chat/parser.el:169` (`gptel-chat--point-in-block-body-p`).
- Original task: `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-point-in-block-body-predicate.md` (status: done).

## Observations

- Step 5 of the brief (`./bin/run-tests.sh -d config/gptel/chat`) was skipped per the project convention noted in the orchestrator instructions: this worktree's `runtime/` is not initialised, so tests cannot run here. The change is artifact-only (two `.md` files) and could not affect test outcomes; the orchestrator runs tests post-merge.
- Verify-1 (`grep -n "block-opener-regexp" openspec/changes/gptel-chat-heading-scoping/`) is a non-recursive grep against a directory and consequently always returns no matches regardless of file contents. A deeper recursive sweep finds two surviving references to the bogus symbol in `add-point-in-block-body-predicate.md` lines 46 and 98 — both inside the existing Observations / Discoveries narrative that explains *why* the symbol was wrong. I deliberately left those two references in place: the Discoveries `disc-add-point-in-block-body-predicate-1` (class `vocabulary-mismatch`) is a self-contained finding that names the bogus symbol as the subject of the mismatch, and the Resolution annotation now points at this corrective task. Stripping the symbol from those narrative blocks would erase the provenance of the error rather than correct it. The prescription itself (step 2) no longer mentions the bogus name.
- Departure from the literal "Replace with:" text in the brief: I rendered the regex `^#\+\(begin\|end\)_\(user\|assistant\|tool\)\b` with single backslashes (matching the on-disk regex form) rather than the brief's double-escaped `^#\\+\\(begin\\|end\\)_\\(user\\|assistant\\|tool\\)\\b` form. The double-escaped form is the elisp source-string form; the markdown prescription reads more clearly with the single-backslash regex form. This is cosmetic and matches how the same regex is rendered elsewhere in the brief.
- The "why naive scan is unsound" counter-example was added as a follow-on paragraph after the algorithm description (per the brief: "Add a brief sentence explaining *why*"), preserving the prescription's two-sentence-paragraph rhythm rather than inlining it.

## Discoveries

- discovery_id: disc-correct-algorithm-1
  class: spec-signal
  description: |
    The brief's Verify-1 grep (`grep -n "block-opener-regexp"
    openspec/changes/gptel-chat-heading-scoping/`) is non-recursive
    against a directory and so vacuously passes irrespective of
    file contents. The intended check is presumably recursive
    (`grep -rn`) or scoped to specific files. The same brief-pattern
    Verify-2 explicitly enumerates files, which is the safer form
    per CLAUDE.md's "Verification grep scoping" guidance.
  recommendation: |
    No spec or code change required. If a future task wants to
    enforce "no `block-opener-regexp` references anywhere in the
    change directory," tighten Verify-1 to `grep -rn` or list the
    target files explicitly. For this task the intent (remove the
    bogus name from the prescription) is satisfied.
