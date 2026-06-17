---
name: agent-work-root-auto-read
description: Auto-include work_root in the agent's read scope (work_root/** prepended to :read); remove the now-redundant D7 guardrail
change: gptel-work-root-default-directory
status: ready
relations:
  - discovered-from:agent-workroot-and-paths
---

## Provenance
- discovered_from: agent-workroot-and-paths (author-blind review spec-signal, cycle-1781718724)
- discovered_by: reviewer (corroborated by the implementor's own observation)
- discovered_class: spec-signal
- user_decision: "A work_root which is unreadable to the agent serves no purpose;
  redundancy between read_paths and work_root is acceptable for now." (resolves ask
  cycle-1781718724-d7-guardrail-prefix-match — chose Option D: auto-include, not a
  guardrail-matching tweak)

## Why
Today `work_root` and `read_paths` are fully decoupled: `--task` writes `work_root` to
the `:GPTEL_WORK_ROOT:` drawer key and feeds `read_paths` VERBATIM into `:read`, with no
link between them. That made it possible to spawn an agent whose working directory is
unreadable (read_paths omitted, or pointing elsewhere) — relative reads then silently
DENY. The cycle-2 D7 guardrail tried to *warn* about that mismatch, but (a) it fired a
false alarm on the normal `work_root=/p` + `read_paths=[/p/**]` case (the `/p/**` glob
compiles to `^/p/.*$`, which does not match the no-trailing-slash root `/p`), and (b)
the user decided the underlying decoupling isn't worth keeping: a work_root the agent
can't read serves no purpose.

Decision: make the work root readable BY CONSTRUCTION and delete the guardrail.

## Files to modify
- `config/gptel/tools/persistent-agent.org` (+ regenerated `.el`):
  - `--task` (~:560-600): after resolving `resolved-work-root` and normalizing
    `read-paths-list`, PREPEND the work-root read pattern to the read list before
    calling `build-scope-plist`. Pattern = `(concat (directory-file-name
    resolved-work-root) "/**")` (so it compiles to `^<root>/.*$` and covers every
    relative file read under the work root). Avoid a duplicate if the exact pattern is
    already present (cosmetic dedup with `member`).
  - REMOVE the D7 guardrail block (~:670-685): the `(unless
    (jf/gptel-scope--path-matches-any-pattern-p resolved-work-root read-paths-list)
    (display-warning ...))` form is now dead — the work root is always in read scope.
  - REMOVE the now-unused `(require 'jf-gptel-scope-validation)` at ~:67 (it was added
    solely for the guardrail's path-matcher; confirmed no other use in this module).
- `config/gptel/tools/test/persistent-agent/work-root-spec.el`:
  - REPLACE the two guardrail specs ("warns when work_root outside read_paths" / "no
    warn complement") with specs asserting the work-root read pattern (`<root>/**`)
    appears in `:GPTEL_SCOPE_READ:` (the work root is readable by construction), and
    that a relative file under the work root validates as ALLOWED even when the caller
    passed NO read_paths.
- `openspec/changes/gptel-work-root-default-directory/design.md`:
  - D6: add that `work_root/**` is auto-included in the agent's read scope (write stays
    separately scoped: `/tmp/**` + explicit `write_paths`).
  - D7: mark the consistency-guardrail decision SUPERSEDED — record the rationale (a
    work_root unreadable to the agent serves no purpose; read/work_root redundancy
    accepted; guardrail removed as redundant).

## Design rationale
A relative path in the agent resolves against `default-directory` (= work_root). Making
`work_root/**` part of `:read` guarantees those relative reads land in scope without the
caller having to remember to grant it. This upholds the user's principle and removes a
band-aid (the guardrail) and its cross-module `require`. It does NOT violate the
zero-inheritance invariant: the agent's own work_root joining its own read scope is
self-consistency, not inheritance of the parent's scope. We give up the (speculative,
unused) "work_root broader than read scope" case; the redundancy is accepted.

## Verification
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` clean.
- `./bin/run-tests.sh -d config/gptel/tools` green; new specs assert work_root is in
  read scope and a relative read under work_root is ALLOWED with empty read_paths; the
  old guardrail specs are gone.
- `grep -n "path-matches-any-pattern-p\|jf-gptel-scope-validation\|display-warning" config/gptel/tools/persistent-agent.org`
  returns nothing (guardrail + require fully removed).

## Context
.orchestrator/cycles/cycle-1781718724/reviews/agent-workroot-and-paths.md (the finding);
.orchestrator/cycles/cycle-1781718724/reconciliations/vocabulary-agent-path-params.md;
register/vocabulary/agent-path-params (work_root maps_to now: :GPTEL_WORK_ROOT: drawer
key AND prepended to :GPTEL_SCOPE_READ:); design.md D6/D7.
