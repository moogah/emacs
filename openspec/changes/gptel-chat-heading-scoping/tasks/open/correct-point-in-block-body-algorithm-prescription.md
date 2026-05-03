---
name: correct-point-in-block-body-algorithm-prescription
description: Replace unsound naive-scan algorithm prescription in the predicate task brief and design.md with the merged stack-walk algorithm; remove bogus gptel-chat--block-opener-regexp reference
change: gptel-chat-heading-scoping
status: ready
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
