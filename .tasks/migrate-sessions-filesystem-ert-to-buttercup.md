---
name: migrate-sessions-filesystem-ert-to-buttercup
description: Delete the .agents-path ert-deftest now duplicated by hidden-agents-dir-spec.el and migrate the remaining config/gptel/sessions/filesystem-test.el cases into the Buttercup specs under config/gptel/sessions/test/filesystem/, then delete the ERT file.
status: externalised
source: openspec/changes/workspace-notes-in-vault
cites_register_entries:
  - register/vocabulary/session-tree-directory-names
relations:
  - discovered-from:arch-cycle-1789334430-eoc-4
  - discovered-from:gptel-hidden-agents
discovered_from: arch-cycle-1789334430-eoc-4
discovered_by: architect
discovered_class: duplication
created_at: 2026-09-17T00:00:00Z
---

> Externalised by cycle-1789334430 integrate (change workspace-notes-in-vault). Cross-cutting:
> test-framework consolidation in the sessions subsystem, not part of the proposal's outcome.

## Finding
The end-of-cycle architect found `config/gptel/sessions/filesystem-test.el:148-156` (ERT) and
`config/gptel/sessions/test/filesystem/hidden-agents-dir-spec.el:60` (Buttercup) both pin the
`.agents` path composition. Each framework runs in its own Emacs process and each file is
green alone, so nothing reported two suites asserting the same contract until the rename
forced both to change. The remaining six ERT cases in filesystem-test.el already have
Buttercup counterparts (directory-templates, session-dir-walk, single-root specs).

## Steps
1. Delete the agents-dir `ert-deftest` (filesystem-test.el:148-156); the Buttercup spec is the
   sole pin (record on `register/vocabulary/session-tree-directory-names`).
2. For each remaining `ert-deftest`, confirm an equivalent `it` exists under
   config/gptel/sessions/test/filesystem/; add one where missing.
3. Delete filesystem-test.el; update any snapshot / test-results.txt.

## Verification
```
./bin/run-tests.sh -d config/gptel/sessions --report
grep -rn 'ert-deftest' config/gptel/sessions/ | wc -l
```
