---
name: pa-trace-task-advice-arity-drift
description: persistent-agent-trace around-advice uses stale --task arity, drops work_root/read_paths/write_paths
status: ready
source: openspec/changes/gptel-work-root-default-directory
relations:
  - discovered-from:agent-workroot-and-paths
---

## Problem

`jf/gptel-pa-trace--around-task` (config/gptel/tools/persistent-agent-trace.org,
≈`:87`) is `:around` advice on `jf/gptel-persistent-agent--task`. After the
`agent-workroot-and-paths` task BREAKING-renamed the `--task` signature to
`(main-cb preset description prompt &optional work-root read-paths write-paths)`,
the advice still declares the OLD arglist
`(orig main-cb preset description prompt &optional allowed-paths)` and re-invokes
`orig` with only those positionals (`(funcall orig wrapped-cb preset description
prompt allowed-paths)`).

Consequence: whenever the trace advice is active (opt-in via
`M-x jf/gptel-pa-trace-start`), every agent spawned drops its `work_root`,
`read_paths`, and `write_paths` — the agent gets the default parent work root
and scratch-only write regardless of what the parent passed. Silent
mis-scoping during exactly the debugging sessions where the trace is on.

This is latent (advice is off by default; the default tool path and the test
suite are unaffected) but it is a genuine defect for the trace-module
maintainer.

## Files to modify

- `config/gptel/tools/persistent-agent-trace.org`
  (`jf/gptel-pa-trace--around-task`, ≈`:87-106`)

## Implementation steps

1. Update the advice arglist to
   `(orig main-cb preset description prompt &optional work-root read-paths write-paths)`.
2. Update the trace log line to report `work-root`/`read-paths`/`write-paths`
   (drop the obsolete `allowed` field, or add the new ones).
3. Update the `(funcall orig ...)` to pass all positionals through:
   `(funcall orig wrapped-cb preset description prompt work-root read-paths write-paths)`.
4. Tangle + validate: `./bin/tangle-org.sh config/gptel/tools/persistent-agent-trace.org`.

## Verification

- `./bin/tangle-org.sh config/gptel/tools/persistent-agent-trace.org` passes.
- With `jf/gptel-pa-trace-start` active, spawning an agent with explicit
  `work_root`/`read_paths`/`write_paths` still writes those into the agent
  drawer (manual check, or a buttercup spec that activates the advice and
  asserts the drawer keys).

## Context

Discovered while implementing
openspec/changes/gptel-work-root-default-directory/tasks/closed/agent-workroot-and-paths.md
(disc-agent-workroot-and-paths-2).
