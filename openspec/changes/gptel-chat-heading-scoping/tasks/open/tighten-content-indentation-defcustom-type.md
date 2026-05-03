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

## Observations

- Step 6 decision (defensive `(max 0 ...)` clamp at `mode.el:205`,
  source `mode.org` in the `gptel-chat--heading-escape-prefix`
  defun): kept as-is per task recommendation. With `:type 'natnum`,
  `customize-variable` will reject negatives at set-time, but the
  helper is also reachable when the variable is unbound (the
  `(or (bound-and-true-p ...) 1)` branch is the relevant one in
  that case) and from any caller that bypasses `customize-set-value`
  by direct `setq` — the clamp documents the wholenum consumer
  contract locally and costs essentially nothing. The fallback chain
  is independently load-bearing (callers in `stream.el` may run before
  `mode.el` defines the defcustom, depending on require order), so
  this code stays. Removal would be cosmetic only.
- The docstring update I added to the defcustom mentions the `natnum`
  contract and the `wholenump` consumer signal so a reader of
  `M-x describe-variable` sees the rationale without having to chase
  down design.md or interfaces.org. This is an addition beyond the
  prescribed steps but stays purely within scope (same defcustom,
  same docstring section).
- The defcustom block in `mode.org` (line ~175) sits in a new
  "Buffer content indentation" top-level section that the previous
  task `add-content-indentation-defcustom` introduced specifically
  to host this single defcustom. The section therefore carries no
  other knobs; future indentation/width customizations for chat-mode
  bodies would naturally land here. Out of scope for this task.
- Did not modify `interfaces.org`'s `status:` field for the
  `chat-heading-collision-escape` register entry. The orchestrator
  state has it as `reconciled` (cycle-1 carry-forward) but the file
  text still says `speculated`. The task body explicitly directs
  content-only edits and leaves the status field to the integrate
  phase — followed that guidance.

## Discoveries

- discovery_id: disc-tighten-defcustom-type-1
  class: vocabulary-mismatch
  description: |
    The closed task brief `add-content-indentation-defcustom` (line 21)
    specified `Custom type: integer`, but the consumer
    `(make-string gptel-chat-content-indentation ?\s)` requires
    `wholenump`. The mismatch was caught by the cycle-1 reviewer
    (ask-cycle-1777624502-1). The canonical `:type` for a
    count-shaped variable consumed by `make-string` is `natnum`
    (Emacs 28+), aliased predicate `wholenump`. Both `:type 'natnum`
    and `:type '(integer 0)` work; `natnum` is idiomatic and
    aligns with the predicate name. Resolved here by flipping the
    defcustom and back-annotating the original brief, design.md
    Decision 8, and the boundary register entry's stage-1 output.
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    Integrate phase: leave the change in place. The stage-1 output
    description now carries an explicit `wholenum`/`natnum`
    contract note, which is content-only and does not alter the
    pipeline's shape. No status flip needed beyond the existing
    reconciled mark.

- discovery_id: disc-tighten-defcustom-type-2
  class: spec-signal
  description: |
    The boundary register entry for `chat-heading-collision-escape`
    currently has `status: speculated` in the file text but the
    orchestrator state carries `reconciled` from cycle-1 (per the
    task-body callout). This is a known drift between two sources
    of truth for register status. Flagging only — task body
    forbids editing the status field directly here.
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    Integrate phase: reconcile the file text's `status:` field with
    the orchestrator's reconciled mark when next touching the
    entry, or accept the orchestrator state as authoritative and
    document the file-vs-state convention somewhere durable.

- discovery_id: disc-tighten-defcustom-type-3
  class: deviation
  description: |
    Added a paragraph to the defcustom docstring documenting the
    `natnum` rationale and the `wholenump` consumer signal. This
    exceeds the literal "change `:type 'integer` to `:type 'natnum`"
    instruction in step 1 but stays within the same defcustom block
    and aligns with the task's intent (push validation to
    customize-time and document the contract).
  affected_register_entry: register/boundary/chat-heading-collision-escape
  recommendation: |
    Integrate phase: leave alone. The docstring change is local,
    on-topic, and improves discoverability of the contract.
