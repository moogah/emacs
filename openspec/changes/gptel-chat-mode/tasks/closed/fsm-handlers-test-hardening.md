---
name: fsm-handlers-test-hardening
description: Tighten gptel-chat--state accessor's struct predicate and add INIT/ABRT/unknown-state test coverage
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:fsm-handlers
---

## Files to modify
- `config/gptel/chat/send.org` (`gptel-chat--state` accessor guard)
- `config/gptel/chat/send.el` (re-tangled)
- `config/gptel/chat/test/send/backend-invocation-spec.el`

## Implementation steps
1. Replace the `cl-struct-p` guard in `gptel-chat--state` with a
   run-time-resolved `gptel-fsm-p` check:
   ```elisp
   (and (fboundp 'gptel-fsm-p)
        (gptel-fsm-p gptel--fsm-last)
        (gptel-fsm-state gptel--fsm-last))
   ```
   The `fboundp` wrapper preserves the load-order safety the current
   guard targets, while narrowing the predicate from "any struct" to
   "an actual gptel-fsm".

2. Add tests that drive the INIT state (entered unconditionally by
   upstream before the user-visible sequence starts) and verify the
   accessor returns `'INIT` or nil predictably.

3. Add a negative test: pass a non-`gptel-fsm` `cl-defstruct` instance
   to the accessor and assert the return value is nil (not an error
   raised from slot-access).

4. After `fsm-handlers-upstream-integration` lands, add the ABRT
   accessor test here too (or fold into that task).

## Design rationale
The `fsm-handlers` review flagged the accessor's guard as "overly loose"
and called out the INIT / ABRT / unknown-state coverage gap. Neither is
blocking (the accessor works today for every case `gptel-fsm` actually
returns), but the weak predicate plus absent negative tests mean a
future refactor that renames `gptel-fsm` or introduces a sibling struct
could silently pass.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/send` passes.
- Negative test (non-gptel-fsm struct) returns nil without signalling.

## Context
- Review of `fsm-handlers` (2026-04-21, orch-review-1776770835),
  Findings 3 and 4.
