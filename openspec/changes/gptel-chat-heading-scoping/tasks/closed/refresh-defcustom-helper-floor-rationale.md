---
name: refresh-defcustom-helper-floor-rationale
description: Refresh the gptel-chat-content-indentation defcustom commentary at mode.el:156-160 (and corresponding mode.org block) to acknowledge the helper-side clamp introduced by clamp-heading-escape-prefix-at-one. Reframe :type 'natnum as customize-time defense-in-depth rather than the sole validation layer. Optionally promote the "helper owns the floor" contract to a register entry that both the defcustom and the helper reference.
change: gptel-chat-heading-scoping
status: done
merge_commit: 72543fb
relations:
  - discovered-from:clamp-heading-escape-prefix-at-one
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
  - `gptel-chat-content-indentation` defcustom docstring (currently mode.el:156-160)
- Optionally `interfaces.org` (new register/invariant or extension to existing
  `register/invariant/chat-block-body-no-column-zero-stars`)

## Implementation steps

1. Read the current defcustom docstring at `config/gptel/chat/mode.org` (the `gptel-chat-content-indentation` block, around line 175 source / line 156 tangled). It currently reads:

   > "The `:type' is `natnum' (non-negative integer) rather than `integer' because the consumer is `(make-string gptel-chat-content-indentation ?\\s)', which signals `wrong-type-argument wholenump' on negatives. Tightening the type pushes validation to `customize-variable' time instead of first-write time."

2. Replace with a phrasing that acknowledges the helper clamp as the runtime floor and frames `:type 'natnum` as customize-time defense-in-depth:

   > "The `:type' is `natnum' (non-negative integer) rather than `integer' as customize-time defense-in-depth: it prevents a user from persisting a negative value via `customize-variable'. The runtime floor is enforced by `gptel-chat--heading-escape-prefix', which clamps any value below 1 (including an explicit 0 or a misset negative) up to 1 — a 0-width prefix would silently violate `register/invariant/chat-block-body-no-column-zero-stars'. So `(setq gptel-chat-content-indentation -3)' followed by an escape pass produces a single-space prefix, not an error: the helper owns the floor; the `:type' is the upstream guard."

3. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/mode.org`.

4. Decide whether to promote the "helper owns the floor" contract to a register entry. Recommendation: extend `register/invariant/chat-block-body-no-column-zero-stars`'s `enforcement_mechanism` block (or add a new `chat-heading-escape-prefix-floor-clamp` invariant) noting that the prefix length is enforced at the helper level, not at the defcustom. This makes the layered enforcement explicit and prevents a future "tighten :type" task from removing the helper clamp under the assumption that customize already guards the floor.

5. Verify:
   - `grep -n "defense-in-depth\|helper owns the floor" config/gptel/chat/mode.el` shows the new wording.
   - `grep -n "wrong-type-argument wholenump" config/gptel/chat/mode.el` returns no match for the defcustom docstring (it remains correct in helper context where applicable).
   - `./bin/run-tests.sh -d config/gptel/chat/test/mode` still passes.

## Design rationale

After `clamp-heading-escape-prefix-at-one` landed, `gptel-chat--heading-escape-prefix` clamps any value below 1 up to 1. This means a misset negative (`(setq gptel-chat-content-indentation -3)`) silently produces a single-space prefix at write time, never reaching `make-string`'s raw `wrong-type-argument wholenump` signal. The defcustom commentary still reads as if `make-string` is the validation gate, which is now misleading.

The two layers serve different audiences:
- **`:type 'natnum`** prevents a user from saving nonsense through `M-x customize-variable`. This is upstream and visible to the user.
- **The helper clamp** silently coerces any value, including explicit 0 and post-`setq` negatives, to a value that preserves the column-0-stars invariant. This is downstream and invisible to the user.

Refreshing the docstring keeps the defcustom self-documenting against the current contract and prevents a future maintainer from removing the helper clamp under the assumption that customize already guards the floor.

The optional register-entry promotion (step 4) makes this explicit at the architectural level: a register-entry note that "the helper, not the defcustom, owns the runtime floor" is more durable than docstring prose because the register is consulted on every relevant change cycle.

## Verification

- `grep -n "defense-in-depth\|helper owns the floor" config/gptel/chat/mode.el` returns matches in the defcustom docstring.
- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds with paren validation.
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` still passes (31+ specs).
- (If step 4 done): `grep -n "helper owns the floor\|prefix-floor-clamp" interfaces.org` returns matches.

## Context

- Discovered by reviewer of `clamp-heading-escape-prefix-at-one` in cycle-1777823910 (review file: `.orchestrator/cycles/cycle-1777823910/reviews/clamp-heading-escape-prefix-at-one.md`, Finding 1).
- Discovery class: spec-signal — the merged code is correct; the surrounding documentation is the load-bearing problem.
- Related cycle-1777823910 task: `tighten-content-indentation-defcustom-type` introduced the docstring paragraph being refreshed here. `clamp-heading-escape-prefix-at-one` introduced the helper-side clamp that made the paragraph stale. Both tasks landed cleanly; this is the seam between them.
- Boundary contract: `interfaces.org` — `register/boundary/chat-heading-collision-escape` stage 1.
- Related invariant: `register/invariant/chat-block-body-no-column-zero-stars` (load-bearing).
