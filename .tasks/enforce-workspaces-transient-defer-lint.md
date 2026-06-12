---
name: enforce-workspaces-transient-defer-lint
description: Add a lint (or reorder load) enforcing that no config/workspaces/ module bare-requires transient
status: open
source: openspec/changes/workspace-integration-menu (cycle-20260612-143558 integrate)
relations:
  discovered-from: workspaces-transient-menu
provenance:
  discovered_from: arch-cycle-20260612-143558-eoc-01
  discovered_by: architect
  discovered_class: invariant-gap
---

## Problem
`register/invariant/workspaces-transient-deferred-require` (load_bearing) is
enforced only by review-discipline. A bare `(require 'transient)` in any
future `config/workspaces/` module pulls the stale built-in transient and
defeats `config/transient.el`'s straight override, failing init globally
(`void-function transient--set-layout`, exit 255). The symptom is whole-suite,
not local — nothing fails fast.

## Options
1. **Lint** (preferred, low-risk): a buttercup spec that greps `.el` under
   `config/workspaces/` for a top-level `(require 'transient)` and fails if
   present. Mirror the glob approach of the directionality lint in
   `config/workspaces/test/gptel-integration-spec.el`
   (`directory-files-recursively`).
2. **Reorder**: move `config/transient` before `workspaces` in `init.el`'s
   `jf/enabled-modules` so the constraint dissolves. Higher blast radius
   (touches a load-order invariant); verify the transient straight override
   still initialises and no other module depends on the current order.

## Verification
- Option 1: the new spec is RED when a bare `(require 'transient)` is
  temporarily added to a workspaces module, GREEN otherwise; full suite green.
- Option 2: full init + `./bin/run-tests.sh` green; `transient-showcase` and
  the workspace menu both function.

## Why externalised (not in-change)
This is defensive hygiene for the NEXT maintainer of the workspaces/transient
boundary, not part of the workspace-integration-menu proposal's stated outcome
(menu + registry + gptel integration, all shipped). Per externalisation.md.
