---
name: data-profile-early-override
description: "Data profiles: env var read before modules load; derive org-graph/org-roam/agenda/gptel paths from it"
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "enables:emacs-agent-wrapper-targets"
---

## Files to modify
- early-init.el (modify) — read the profile env var (name per design Open Questions, working name `JF_ORG_DIR` / `JF_DATA_PROFILE`)
- config/core/defaults.org (modify) — `jf/org-directory` honours it
- config/org-graph/org-graph.org (modify) — vault/roam roots default from `jf/org-directory`
- config/org-graph/discovery.org (modify) — `vulpea-default-notes-directory` derived, not a load-time copy of ~/org
- config/gptel/gptel.org (modify) — dailies directory derived from `jf/org-directory`
- config/core/test/data-profile-spec.el (new)

## Implementation steps
1. Specs first (batch, with the env var bound per test):
   - unset → `jf/org-directory` is `~/org` and org-graph roots are `~/org/`,
     `~/org/roam/` (no behavior change);
   - set to a temp dir → `jf/org-directory`, `org-graph-vault-root`,
     `org-graph-roam-root`, `vulpea-default-notes-directory`, `org-roam-directory`,
     the gptel dailies dir all live under it;
   - profile `none` → `org-graph-watch-workspace-homes` nil and org-graph sync not
     started; profile `scratch` → workspace-home indexing off and
     `workspace-state-directory-override` set to a temp dir.
2. Implement: early-init reads the env vars into variables consulted by
   `defaults.org` (`defvar` honours a pre-set value); replace hard-coded `"~/org/"`
   defaults with expressions over `jf/org-directory`.
3. Confirm the live Emacs.app launch (no env vars) is unaffected.

## Design rationale
An isolated instance today indexes, watches and writes the real ~/org/ while
bypassing the live session's in-process write coordinator. Several paths are fixed at
module load, before command-line `--eval` runs, so the profile must be applied in
early-init. One root variable keeps all note paths consistent.

## Verification
- ./bin/tangle-org.sh config/core/defaults.org
- ./bin/tangle-org.sh config/org-graph/org-graph.org
- ./bin/tangle-org.sh config/org-graph/discovery.org
- ./bin/tangle-org.sh config/gptel/gptel.org
- ./bin/run-tests.sh -d config/core
- ./bin/run-tests.sh -d config/org-graph

## Context
- design.md § Decisions 'D15. Isolated targets carry a data profile, applied before modules load'
- specs/isolated-emacs-targets/spec.md § 'Data Profiles'
