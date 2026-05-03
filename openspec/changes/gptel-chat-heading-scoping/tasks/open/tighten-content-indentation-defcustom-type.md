---
name: tighten-content-indentation-defcustom-type
description: Tighten gptel-chat-content-indentation defcustom :type from integer to natnum so customize-time validation matches the boundary's wholenum consumer domain
change: gptel-chat-heading-scoping
status: ready
relations:
  - discovered-from:add-content-indentation-defcustom
---

## Files to modify

- `config/gptel/chat/mode.org` (and tangled `mode.el`)
- `interfaces.org` (register/boundary/chat-heading-collision-escape stage-1 input/output notes)
- `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-content-indentation-defcustom.md` (closed task brief annotation)
- `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 8 (if it specifies `:type 'integer`)

## Implementation steps

1. In `config/gptel/chat/mode.org` (the `defcustom gptel-chat-content-indentation` block, currently around line 175), change `:type 'integer` to `:type 'natnum`.
2. Re-tangle: `./bin/tangle-org.sh config/gptel/chat/mode.org` and confirm `mode.el` reflects the change.
3. In `interfaces.org`, in the `register/boundary/chat-heading-collision-escape` entry (stage 1 `output:` block around line 1230), add a note that `gptel-chat-content-indentation` is a `wholenum` (a.k.a. `natnum`) — the consumer is `(make-string gptel-chat-content-indentation ?\s)` which signals `wholenump` on negatives. The `:type` constraint is the contract that pushes validation to customize-time rather than first-write-time.
4. In `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-content-indentation-defcustom.md` (the closed brief), add a brief annotation under "Discoveries" or as a trailing note: the brief's "Custom type: `integer`" was wrong; canonical is `natnum`. Resolved by ask-cycle-1777624502-1.
5. In `openspec/changes/gptel-chat-heading-scoping/design.md` Decision 8, if the prose specifies `:type 'integer`, correct it to `:type 'natnum` with the same rationale.
6. Decide whether to keep or drop the defensive `(max 0 ...)` coercion at `mode.el:205`. Recommendation: keep it. The fallback chain `(or (bound-and-true-p ...) 1)` still serves callers from `stream.el` where the variable may be a `defvar` shadow. The `(max 0 ...)` clamp becomes dead code under the new `:type` but documents the consumer-domain expectation locally; removal is cosmetic. Document the decision in observations.

## Design rationale

`make-string` requires `wholenump`. `:type 'integer` accepts negatives, so a user customizing the variable to `-1` passes type validation and crashes the write pipeline at the first column-0 `*` line — far from the offending customization. `natnum` (alias for non-negative integer) maps directly onto the consumer's domain and surfaces the validation error at `customize-variable` time, where it belongs.

Choice of spelling: prefer `natnum` (Emacs 28+) over `(integer 0)`. Both work; `natnum` is the idiomatic spelling for count-shaped variables and aligns with `wholenump` predicate naming.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/mode.org` succeeds.
- `grep -n ":type 'natnum" config/gptel/chat/mode.el` shows the new type.
- `grep -n ":type 'integer" config/gptel/chat/mode.el` returns no match for `gptel-chat-content-indentation`.
- In a fresh emacs: `(customize-set-variable 'gptel-chat-content-indentation -1)` should signal a type error rather than silently accepting.
- Unit-level smoke: `(make-string gptel-chat-content-indentation ?\s)` succeeds for default value.
- `./bin/run-tests.sh -d config/gptel/chat` still passes (no regressions in user-typed-escape spec which exercises `gptel-chat-content-indentation = 2`).

## Context

- Resolves user ask `ask-cycle-1777624502-1` (see `.orchestrator/handshake-cycle-1777624502.json` `asks_for_user_resolved`).
- Reviewer finding: `.orchestrator/cycles/cycle-1777624502/reviews/add-content-indentation-defcustom.md` Finding 1.
- Register entry: `interfaces.org` — `register/boundary/chat-heading-collision-escape` stage 1.
- Original task: `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-content-indentation-defcustom.md` (status: done).
