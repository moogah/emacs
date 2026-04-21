---
name: branching-edge-case-test-hardening
description: Pin two under-tested edge cases in the branching suite (EOF INCLUDE and registry-update semantics)
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:sessions-branching
---

## Files to modify
- `config/gptel/sessions/test/branching/context-truncation-spec.el`
  (add one spec or docstring clarification)
- `config/gptel/sessions/test/branching/branching-integration-spec.el`
  (add one spec that exercises the registry-update ordering)

## Implementation steps
1. **EOF INCLUDE coverage** — add a Buttercup spec to
   `context-truncation-spec.el` that asserts the INCLUDE branch
   behaviour when the selected `#+end_user` is the parent buffer's
   last non-empty line with **no trailing newline**. Expected
   behaviour: `forward-line 1` lands at `point-max`, and the
   resulting branch `session.org` contains the full parent file
   verbatim.

   Alternative if the test harness makes "no trailing newline"
   awkward to fixture: document the behaviour in the
   `jf/gptel--branching-turn-branch-point` docstring explicitly,
   stating that missing-trailing-newline EOF INCLUDE is equivalent
   to "include everything" and is considered benign.

2. **Registry-update ordering note** — add either:
   - A spec in `branching-integration-spec.el` that invokes
     `find-file` on the new branch's `session.org`, triggers
     `jf/gptel--auto-init-session-buffer`, and asserts the session
     registry now contains an entry for the new branch. This pins
     the "registry is populated lazily, on open" contract.
   - Or a one-line explanatory comment at the top of
     `branching-integration-spec.el` (and in the `branching.org`
     module comment) stating that `jf/gptel--create-branch-session`
     does NOT register the new branch; registration happens when
     the auto-init hook first opens the branch's `session.org`.

   Pick whichever matches the sessions subsystem's existing test
   style. A spec is preferable if registry state is observable in
   unit tests.

## Design rationale
The sessions-branching review flagged two small coverage gaps:

- **Finding #2 (EOF INCLUDE)**: `forward-line 1` silently degrades
  to `point-max` when the selected `#+end_user` is the last line
  with no trailing newline. Behaviour is correct (full file
  copied, INCLUDE semantics preserved at EOF), but no spec or
  docstring documents it. A regression that introduced different
  EOF behaviour would not be caught.

- **Finding #4 (registry update asymmetry)**: The task body claimed
  "registry updates" are in-scope and unchanged, but the integration
  tests do not verify registry state. Inspection shows
  `jf/gptel--create-branch-session` does **not** populate the
  registry directly; the auto-init hook does so when the new
  branch is first opened. This is correct but asymmetric with the
  task body's phrasing, and the tests never exercise the
  first-open path that actually performs registration.

Neither is a correctness bug; both are small gaps between what the
code does and what the test suite / docs document it as doing. Group
because they both touch the branching test suite and are minor
hardening rather than functional changes.

## Design pattern
When a code path's behaviour is partially implicit (e.g. "registry
entry is a side effect of open, not of create"), pin it either in a
test that exercises the full flow or in a prominent docstring/comment.
Split the two choices per concern — tests if state is observable in
unit scope, documentation otherwise.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/branching.org` succeeds
  (if docstring is edited).
- `./bin/run-tests.sh -d config/gptel/sessions/test/branching`
  passes with any new spec(s).
- If the documentation-only path is chosen, the new comments are
  visible in both `.org` and `.el` output.

## Context
- Review of sessions-branching (2026-04-21, orch session
  `orch-review-1776796835`) Findings #2 and #4.
- `config/gptel/sessions/branching.el` —
  `jf/gptel--branching-turn-branch-point` (`forward-line 1` at EOF)
  and `jf/gptel--create-branch-session` (no registry write).
- `config/gptel/sessions/test/branching/
  branching-integration-spec.el` — target for any added integration
  spec.
