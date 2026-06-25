---
name: consolidate-session-enumerator-twins
description: Extract the twinned drawer-read+resolve+skip preamble shared by jf/gptel--init-registry and jf/gptel--find-all-branches-with-agents into one helper.
source: openspec/changes/gptel-content-addressed-session-activation
status: open
discovered_from: discovery-reads-drawers
discovered_by: architect
discovered_class: duplication
relations:
  - "discovered-from:discovery-reads-drawers"
---

## Finding

The end-of-cycle architect (cycle-1781453946, advisory) found the
drawer-read + resolve + skip-on-no-drawer preamble is now twinned verbatim
across two session enumerators:

- `jf/gptel--init-registry` (registry.el ~:44-56)
- `jf/gptel--find-all-branches-with-agents` (filesystem.el ~:312-326)

Both: locate branch `session.org` → `jf/gptel--read-session-drawer-head` →
`jf/gptel--resolve-session-id` / `--resolve-branch-name` (passing the
session-file path for the trailing-slash fallback) → skip-and-debug-log if
no `:GPTEL_` drawer. This is a DEEPENING of pre-existing enumerator twinning
(the two functions were already structurally parallel), not fresh duplication
from cycle-3's two tasks colliding — each copy is independently correct and
tested.

## Why this is `.tasks/` (cross-cutting backlog), not in-change

Both copies are correct and covered by specs; the change's stated outcome
(content-addressed session activation) does not require the consolidation.
This is next-maintainer hygiene for the sessions enumerators. Per the
architect: flag as backlog, do not reopen either cycle-3 task.

## Proposed work

Extract a helper, e.g. `jf/gptel--resolve-branch-identity (branch-session-file)`
returning the resolved `(session-id . branch-name)` (or nil to signal skip),
and call it from both enumerators. Keep directory traversal (the file
*locator*) where it is; only the identity-resolution preamble is shared.

## Verification

- `./bin/tangle-org.sh` on `registry.org` and `filesystem.org` succeed.
- `./bin/run-tests.sh -d config/gptel/sessions` green (registry + filesystem
  discovery specs unchanged and passing).
- Done = one canonical drawer-resolve-or-skip helper; both enumerators call it.

## Context

- Architect finding: `.orchestrator/cycles/cycle-1781453946/findings/end-of-cycle-architect.md`
- Caller-obligation note: `interfaces.org` register/boundary/drawer-first-identity-resolution boundary_contract (trailing-slash).
