---
name: clamp-heading-escape-prefix-at-one
description: Clamp gptel-chat--heading-escape-prefix at minimum 1 (not 0) so explicit gptel-chat-content-indentation = 0 is silently coerced for invariant-preservation parity with the unbound case. Update helper docstring; cross-reference fallback contract from migration docstring (Finding 2 docstring drift); delete the misleading migration-spec.el test that locked in the invariant violation as a regression check.
change: gptel-chat-heading-scoping
status: ready
relations:
  - discovered-from:add-migration-on-read
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
  - `gptel-chat--heading-escape-prefix` body — change clamp from `(max 0 ...)` to `(max 1 ...)`
  - `gptel-chat--heading-escape-prefix` docstring — extend fallback contract to cover explicit 0
  - `gptel-chat--migrate-headings` docstring — cross-reference helper's fallback (Finding 2)
  - Org commentary "Behavioural contract" bullets above the migration function (~mode.org:160-198)
- `config/gptel/chat/test/mode/migration-spec.el` — delete the `gptel-chat-content-indentation = 0` test (lines 260-267 plus surrounding describe/it scaffold and comment)

Explicitly NOT modified:
- `gptel-chat-content-indentation` defcustom `:type` — stays at `natnum` per ask-1's resolution. Customize doesn't enforce the boundary's semantic floor; the helper does. The two layers serve different purposes: `:type` keeps customize from accepting nonsense (negatives), the helper clamps at the minimum *meaningful* value (1, the smallest indent that breaks `^\\*+ `).
- Other consumers of the variable (`stream.el`, paste-escape, typed-escape) — they all reach the indent width through `gptel-chat--heading-escape-prefix` already (per architect-fix `0ffcdb5`), so the clamp is centralised. Confirm during implementation that no consumer reads `gptel-chat-content-indentation` directly with `(make-string ... ?\s)`; if any does, route it through the helper.

## Implementation steps

1. **Clamp the helper**. In `mode.org` (the `gptel-chat--heading-escape-prefix` block), change the body from:
   ```elisp
   (make-string (max 0 (or (bound-and-true-p gptel-chat-content-indentation) 1))
                ?\s)
   ```
   to:
   ```elisp
   (make-string (max 1 (or (bound-and-true-p gptel-chat-content-indentation) 1))
                ?\s)
   ```

2. **Extend the helper docstring** to document the new clamp contract. Current docstring (mode.el:191-204) describes the unbound fallback. Add: "An explicitly-set 0 is also clamped to 1: a 0-width prefix would not break the `^\\*+ ` heading regex and would silently violate `register/invariant/chat-block-body-no-column-zero-stars`. The clamp ensures the helper always returns a prefix with column-0 separation, matching the unbound-fallback rationale."

3. **Update the migration docstring** (Finding 2). At `mode.el:425-453` (mirrored in `mode.org`), the current text reads:
   > "Walks each outer `#+begin_user' / `#+begin_assistant' block returned by `gptel-chat-parse-buffer' and prefixes every `^\\*+ ' body line with `gptel-chat-content-indentation' spaces."

   Add a follow-up sentence:
   > "Indentation width comes from `gptel-chat--heading-escape-prefix' which falls back to 1 when `gptel-chat-content-indentation' is unbound and clamps an explicitly-set 0 to 1 — a 0-width prefix would silently violate the column-0-stars invariant. Migration is therefore never a no-op solely on indent grounds; it is a no-op only when the buffer has no `^\\*+ ' lines inside chat-block bodies."

4. **Update the org commentary bullets** at `mode.org:160-198` ("Behavioural contract" or equivalent section above the migration function). Specifically, any bullet of the form "buffer with no in-block `*` lines is a no-op" should be the only no-op condition (drop any bullet implying that an unset/zero indentation is also a no-op path — that path no longer exists post-clamp).

5. **Delete the migration-spec test** at `config/gptel/chat/test/mode/migration-spec.el:260-267` (and any wrapping `describe`/`it` scaffold and explanatory comment that exists solely to frame this case). After the clamp lands, this scenario is functionally identical to the default `gptel-chat-content-indentation = 1` scenario — keeping it would just duplicate coverage and re-introduce the misleading "indentation 0 is a no-op" framing.

   Add a brief replacement comment near where the deleted test lived (or in the spec file's header comment) noting: "Explicit `gptel-chat-content-indentation = 0` is clamped at the helper to 1; the resulting behaviour is covered by the default `= 1` scenarios."

6. **Re-tangle and verify**:
   ```
   ./bin/tangle-org.sh config/gptel/chat/mode.org
   grep -n "max 1 (or" config/gptel/chat/mode.el        # confirms clamp landed
   grep -n "max 0 (or" config/gptel/chat/mode.el        # should return no matches in heading-escape-prefix
   ./bin/run-tests.sh -d config/gptel/chat/test/mode    # all migration + paste + typed-escape specs pass
   ```

## Design rationale

The architect's commit `0ffcdb5` introduced the unbound-fallback (`(or ... 1)`) specifically to preserve `register/invariant/chat-block-body-no-column-zero-stars` when `gptel-chat-content-indentation` is unbound. The same reasoning applies to explicit 0: a 0-width prefix produces a column-0 `*` line inside a chat-block body, which is exactly what the invariant forbids. The fact that the user explicitly set 0 doesn't make the result less invariant-violating; it just makes the violation user-attributed rather than tooling-attributed.

Why clamp at the helper rather than reject at customize-time:

- The defcustom `:type` is customize's contract; clamping at the helper is the boundary's contract. They serve different layers — customize prevents nonsense input (negatives), the helper enforces semantic correctness (must produce a prefix that actually escapes).
- A `setq gptel-chat-content-indentation 0` bypasses customize anyway. If the boundary's correctness depends on the value, it can't trust customize's `:type` alone; it has to clamp at use.
- The existing pattern (`max 0` clamp at the helper) was already a "trust nothing, clamp at use" stance. We're tightening the clamp threshold from "no negatives" to "no zero or below," which is the same pattern, just with the right floor.

Why delete rather than rewrite the migration-spec test:

- After the clamp, `gptel-chat-content-indentation = 0` is observationally equivalent to `= 1`. There's already a default-case test exercising the `= 1` path. A "what happens with 0" test would either duplicate the default-case test (no value) or assert "the user's 0 was silently coerced" (which is testing implementation, not contract).
- The misleading framing ("Pathological config: indentation 0 cannot break the heading regex, so the migration simply does nothing") was the load-bearing problem. Deletion is cleaner than rewriting prose around a test that no longer carves out distinct behaviour.

## Verification

- `grep -n "max 1 (or (bound-and-true-p gptel-chat-content-indentation)" config/gptel/chat/mode.el` returns one match in `gptel-chat--heading-escape-prefix`.
- `grep -n "max 0 (or (bound-and-true-p gptel-chat-content-indentation)" config/gptel/chat/mode.el` returns no matches.
- `grep -n "clamps an explicitly-set 0\|column-0-stars invariant" config/gptel/chat/mode.el` shows the new docstring text in both helper and migration.
- `grep -n "indentation 0 cannot break\|gptel-chat-content-indentation 0)" config/gptel/chat/test/mode/migration-spec.el` returns no matches (test deleted).
- `./bin/run-tests.sh -d config/gptel/chat/test/mode` passes (migration spec count is 1 lower than before, all remaining pass).
- Behavioral smoke: in a fresh emacs, `(let ((gptel-chat-content-indentation 0)) (gptel-chat--heading-escape-prefix))` returns `" "` (single space, not empty string).

## Context

- Resolves user ask `ask-cycle-1777624502-5` (see `.orchestrator/handshake-cycle-1777624502.json` `asks_for_user_resolved`).
- Reviewer findings: `.orchestrator/cycles/cycle-1777624502/reviews/add-migration-on-read.md` Findings 1 and 2 (folded together — both touch the same helper / migration documentation).
- Architect's prior fix establishing the clamp pattern: commit `0ffcdb5`.
- Related ask: `ask-cycle-1777624502-1` resolution (`tighten-content-indentation-defcustom-type.md`) — that task changes the defcustom `:type` to `natnum`. This task does not depend on it; the clamp at the helper is independent of the customize-side type. They can land in either order.
- Original task: `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-migration-on-read.md` (status: done).
- Merged code: `config/gptel/chat/mode.el:185-208` (heading-escape-prefix), `config/gptel/chat/mode.el:423-486` (migration), `config/gptel/chat/test/mode/migration-spec.el:260-267` (test to delete).
- Boundary contract: `interfaces.org` — `register/invariant/chat-block-body-no-column-zero-stars` (load-bearing).
