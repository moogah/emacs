---
name: fix-stale-metadata-yml-assertion-in-filesystem-test
description: The ERT test test-directory-creation-org-session-structure in config/gptel/sessions/filesystem-test.el asserts (file-exists-p "metadata.yml"), but session creation stopped writing metadata.yml sidecars before this point (Decision 6 — "no metadata.yml is written"). The test has been stale since commit be6b80c and is one of the pre-existing baseline ERT failures. Remove the obsolete assertion and rename the test to reflect what it actually verifies.
source: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:emit-system-prompt-and-chat-headings-at-creation
---

## Files to modify

- `config/gptel/sessions/filesystem-test.el` (modify) — remove the obsolete `metadata.yml` assertion from `test-directory-creation-org-session-structure`; rename the test if it no longer verifies a metadata file

## Why

Author-blind Reviewer finding on `emit-system-prompt-and-chat-headings-at-creation` (severity: spec-signal). The ERT test `test-directory-creation-org-session-structure` asserts `(should (file-exists-p (expand-file-name "metadata.yml" branch-dir)))`. This contradicts the established design — Decision 6 ("No `metadata.yml` is written. No `scope.yml` is written."), restated in the cycle-7 `emit` diff's own comments and in `sessions-persistence.md`. The test was left stale when an earlier task (`be6b80c`, "rewire-session-creation: ... stop writing scope.yml") stopped emitting sidecar files; the `metadata.yml` assertion was never removed.

This is **externalised to `.tasks/`** rather than kept in-change because it is genuinely pre-existing and cross-cutting: `filesystem-test.el` is untouched by any cycle-7 task, the staleness pre-dates the change's merge base (`63192f4`), and it is one of the stable baseline ERT failures (the `test-directory-creation-org-session-structure` item in the cycle-7 baseline note). It is the next maintainer of the sessions-filesystem area's pickup, not cycle-7's own contract.

## Implementation steps

1. Open `config/gptel/sessions/filesystem-test.el`, locate `test-directory-creation-org-session-structure` (~line 43–64).
2. Remove the `(should (file-exists-p ... "metadata.yml" ...))` assertion.
3. If the test no longer verifies a metadata file at all, rename it to describe what it does verify (the org session directory structure).
4. Re-run `./bin/run-tests.sh -d config/gptel/sessions` and confirm the ERT unexpected-failure count drops by one.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/sessions
grep -n 'metadata.yml' config/gptel/sessions/filesystem-test.el
```

Expect: no `metadata.yml` assertion remains; `test-directory-creation-org-session-structure` (or its renamed successor) passes.

## Context

Provenance: author-blind Reviewer finding on `emit-system-prompt-and-chat-headings-at-creation` (cycle-7 execute), `discovered_by: reviewer`, `discovered_class: spec-signal`. Review file: `.orchestrator/cycles/cycle-1779477564/reviews/emit-system-prompt-and-chat-headings-at-creation.md`, Finding 1. Removing this stale assertion will reduce the repo's pre-existing baseline ERT failure load (currently 10 unexpected) by one.
