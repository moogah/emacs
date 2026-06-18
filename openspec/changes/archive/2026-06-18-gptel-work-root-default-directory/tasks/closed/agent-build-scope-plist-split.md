---
name: agent-build-scope-plist-split
description: build-scope-plist takes read-paths + write-paths; /tmp/** appended as scratch
change: gptel-work-root-default-directory
status: done
relations:
  - enables:agent-workroot-and-paths
---

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify) — `jf/gptel-persistent-agent--build-scope-plist`
  (≈`persistent-agent.org:202`)

## Implementation steps
1. Change the signature from `(allowed-paths)` to `(read-paths write-paths)`.
2. Build the plist as:
   ```elisp
   (list :paths
         (list :read  (or read-paths nil)
               :write (append write-paths '("/tmp/**"))
               :deny  jf/gptel-persistent-agent--standard-deny-paths))
   ```
   `/tmp/**` is the LAST element of the `:write` list — guaranteed scratch space,
   NOT the write default it used to be when `:write` was hardcoded to `'("/tmp/**")`.
3. Update the docstring: `:read` = supplied read-paths (nil/empty ⇒ no read access);
   `:write` = supplied write-paths plus `/tmp/**` scratch; deny = standard set.
   Note the register shape reference (`register/shape/scope-config-plist`) is unchanged.
4. Do NOT change `jf/gptel-persistent-agent--standard-deny-paths` (≈`:194`).
5. Tangle + validate: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.

NOTE: the sole caller (`--task`) is updated in task `agent-workroot-and-paths`; this
task changes the helper and its spec only. Tangling will succeed (the caller is updated
in the dependent task); if you run the full agent suite before that task lands, expect
the caller arity to be stale — scope this task's test run to the helper spec.

## Design rationale
The "agent write is always only `/tmp`" behavior was an accident, never intended
design — the parent is supposed to pass real read AND write paths, with `/tmp` merely
included as scratch. Splitting the helper into read/write and appending `/tmp/**`
makes write scope parent-controlled while preserving the scratch grant (design D6).
Serialization needs nothing new: `/tmp/**` rides the existing `+`-multivalue drawer
convention (`:GPTEL_SCOPE_WRITE+: /tmp/**`) as the last list element.

## Design pattern
The current body is a single `(list :paths (list :read … :write '("/tmp/**") :deny …))`.
Keep that exact shape; only the `:read` source and the `:write` construction change.

## Verification
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` passes.
- Buttercup spec under `config/gptel/tools/test/persistent-agent/`:
  - WHEN called with `read-paths ("/p/**")` `write-paths ("/p/**")` THEN `:read` is
    `("/p/**")`, `:write` is `("/p/**" "/tmp/**")`, `:deny` is the standard set.
  - WHEN `write-paths` is nil THEN `:write` is `("/tmp/**")` (scratch only).
  - WHEN `read-paths` is nil THEN `:read` is nil (no read access).
- Run: `./bin/run-tests.sh -d config/gptel/tools` (after the dependent caller task, or
  scope to the build-scope-plist spec in isolation).

## Context
design.md § Decisions 'D6 — Symmetric read_paths / write_paths; /tmp is appended scratch'
specs/persistent-agent/spec.md § 'Requirement: Agent session creation' (write scope = write_paths + /tmp scratch)

## Observations

- Signature changed `(allowed-paths)` → `(read-paths write-paths)`. `:read`
  is `(or read-paths nil)` (verbatim, nil/empty ⇒ no read access). `:write`
  is `(append write-paths '("/tmp/**"))`, so `/tmp/**` is always the LAST
  element (scratch grant), and scratch-only write when `write-paths` is nil.
  `:deny` unchanged (`jf/gptel-persistent-agent--standard-deny-paths`).
- `register/shape/scope-config-plist` shape preserved exactly:
  `(:paths (:read … :write … :deny …))`. No shape divergence — only the
  `:read` source and `:write` construction changed.
- `jf/gptel-persistent-agent--standard-deny-paths` (≈:194) untouched.
- Tangle + check-parens passed. New spec
  `config/gptel/tools/test/persistent-agent/build-scope-plist-spec.el`
  (4 `it` blocks) passes in isolation.
- SOLE CALLER `jf/gptel-persistent-agent--task` (persistent-agent.org:560,
  tangled persistent-agent.el:337) still passes ONE arg
  (`allowed-paths-list`). This is the documented stale-caller arity owned by
  the separate `agent-workroot-and-paths` task; it produces 15 pre-existing
  `wrong-number-of-arguments` failures in the full persistent-agent suite
  that are NOT a regression from this task. Left untouched per scoping note.

## Discoveries

(none)
