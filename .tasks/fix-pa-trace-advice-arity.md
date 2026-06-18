---
name: fix-pa-trace-advice-arity
description: Update jf/gptel-pa-trace--around-task to the post-split --task signature (work_root, read_paths, write_paths); it currently forwards a stale single allowed-paths arg and silently drops scope on traced spawns
source: gptel-work-root-default-directory (cycle-1781723720 integrate; end-of-cycle Architect audit)
status: active
relations:
  discovered-from: agent-work-root-auto-read
---

## Provenance
- discovered_from: agent-work-root-auto-read (relative to source change)
- discovered_by: architect (end-of-cycle audit arch-cycle-1781723720-eoc1); first
  spotted by the docs-allowed-paths-rename implementor (disc, interface-drift)
- discovered_class: dead-branch / interface-drift
- severity: advisory (latent — the trace harness is opt-in debugging tooling, loaded by
  no spec and never on a production path)

## Why this is external to the change
The `gptel-work-root-default-directory` change renamed the PersistentAgent tool surface
from `allowed_paths` to the closed set `{work_root, read_paths, write_paths}` and made the
work root readable by construction. The around-advice in `persistent-agent-trace.org` is a
debugging trace harness — not part of the change's stated outcome (the work-root /
relative-path / scope behavior). Fixing it does not advance the proposal; it is a
cross-cutting cleanup the next maintainer of the trace tooling should pick up. Hence
`.tasks/` rather than an in-change task.

## The defect
`jf/gptel-pa-trace--around-task` (`config/gptel/tools/persistent-agent-trace.org`,
~lines 196-215) still declares `(orig main-cb preset description prompt &optional
allowed-paths)` and forwards `(funcall orig wrapped-cb preset description prompt
allowed-paths)`. The real `jf/gptel-persistent-agent--task` signature is now
`(... &optional work-root read-paths write-paths)`. A traced spawn therefore passes the
single old positional arg into `work-root` and drops `read-paths` / `write-paths`
entirely — the traced agent gets the wrong work root and no read/write scope.

## Files to modify
- `config/gptel/tools/persistent-agent-trace.org` (+ regenerated `.el`):
  - Update the advice arglist to `(orig main-cb preset description prompt &optional
    work-root read-paths write-paths)`.
  - Update the trace-logging line (~:202) that prints `allowed-paths` to print the three
    new params.
  - Update the forwarding call (~:214) to `(funcall orig wrapped-cb preset description
    prompt work-root read-paths write-paths)`.

## Implementation steps
1. Edit the `.org` (never the `.el`); run `./bin/tangle-org.sh
   config/gptel/tools/persistent-agent-trace.org` (tangles + check-parens); stage both.
2. If a trace spec exists, update/add one asserting the advice forwards all three scope
   params. (Search `config/gptel/tools/test` for trace coverage; if none, a small
   buttercup `*-spec.el` that spies the orig and asserts the forwarded arglist is the
   minimal guard.)

## Verification
- `grep -nE "allowed-paths" config/gptel/tools/persistent-agent-trace.org` returns
  nothing (or only an intentional historical comment).
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent-trace.org` clean.
- `./bin/run-tests.sh -d config/gptel/tools` green (21-floor unchanged elsewhere).

## Context
.orchestrator/cycles/cycle-1781723720/findings/arch-eoc-cycle-1781723720.md (eoc1);
.orchestrator/cycles/cycle-1781723720/implementor-reports/docs-allowed-paths-rename.md
(the implementor's original interface-drift discovery);
register/vocabulary/agent-path-params (the closed param set this advice must match).
