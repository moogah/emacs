---
name: configure-migrate-and-verify
description: Point the sessions root into the vault, enable the integration module, run the migration on this machine, verify prompt hygiene and discovery in a live boot, and run the full suite.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-workspace-commands
  - blocked-by:int-migration-command
---

## Files to modify
- init.org or config/local/<role>.org (modify; tangle — `(setq jf/gptel-sessions-directory "~/org/sessions/")`)
- config/integrations/workspaces-org-graph.org (modify — load-time check that the sessions root is under the vault root, warn otherwise)
- config/org-graph/docs/spike-eval.org (modify — Discovery section; add a "Workspace notes in vault" section with the human checks below)
- config/gptel/chat/test/*send*-spec.el (add one spec — see step 3)

## Implementation steps
1. Configuration: set `jf/gptel-sessions-directory` to `~/org/sessions/`
   (choose init.org for all machines unless a role-specific vault exists).
   Add the load-time check in the integration layer:
   `(unless (file-in-directory-p jf/gptel-sessions-directory org-graph-vault-root) (display-warning ...))`.
2. Live boot (`./bin/emacs-isolated.sh`): run `M-x workspace-migrate-to-vault`;
   inspect the `*workspace-migration*` log; confirm `~/emacs-workspaces/*/`
   contain no `home.org` / `sessions/`, `~/org/sessions/` holds all
   sessions, every `agents/` is `.agents/`. Commit the new notes in `~/org`.
3. D7 verification: add a spec on the chat send path asserting that a
   file-level `EDGES` drawer (outside any `#+begin_user`/`#+begin_assistant`
   block) is absent from the prompt text handed to gptel. If it is present,
   set `gptel-org-ignore-elements` to include `drawer` buffer-locally in
   `gptel-chat-mode` and re-run.
4. Human checks (record in the runbook): tab label shows the project note
   title; home layout opens the project note with a populated connected
   block listing the migrated session(s); `SPC v` shows the Workspace group;
   `find-in-workspace` lists only this workspace's notes; creating a session
   on a workspace tab yields a `part-of` row (`org-graph-query/incoming`);
   branching that session yields a `branched-from` row and no duplicate IDs
   (`vulpea-doctor` clean); `M-x jf/gptel-org-new-topic-with-id` then
   `find-in-workspace` offers the topic; a full re-scan indexes nothing
   under `.agents/`; a file with an `eval:` local under the vault does not
   prompt or error during scan.
5. `./bin/run-tests.sh` (both frameworks) — green; `make test-report` snapshot
   if the repo tracks one for touched dirs.
6. Record findings and any deviations in this task's Observations before
   closing.

## Design rationale
The packages are independent, so nothing exercises the whole path until
configuration links them. The migration is a one-shot per machine; the
runbook captures what a human must see. The chat-parser prompt-hygiene
check is the one design assumption (D7) not provable by unit tests of a
single package.

## Design pattern
Runbook style from `config/org-graph/docs/spike-eval.org`; role-specific
config placement per CLAUDE.md "Machine roles".

## Verification
- `./bin/run-tests.sh` — all green.
- `grep -n 'jf/gptel-sessions-directory' init.el config/local/*.el` → set to the vault sessions directory.
- Runbook section ticked; migration log reviewed; `git -C ~/org status` shows only the expected new/moved files.

## Context
design.md § D7, § D9, § Migration Plan (steps 2–3)
specs/workspace-graph-integration/spec.md § 'Sessions root within the vault', 'Migration of co-located workspaces'

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/invariant/sessions-single-root`: speculated → reconciled. `jf/gptel-sessions-directory` is the ONLY knob; creation resolves through the mkdir-ing `jf/gptel--ensure-sessions-root`, enumerators expand the variable directly and never create the root. **Implication for step 1**: the load-time check compares `(expand-file-name jf/gptel-sessions-directory)` against the expanded vault root; setting the variable in init.org is sufficient — no other root exists to redirect. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-sessions-single-root.md`.
- `register/invariant/index-parser-never-evaluates-file-locals`: speculated → reconciled — the seam is `:around vulpea-db--parse-file` (discovery.el:68/:113), spec-covered by discovery-spec.el:289-383. Step 4's "`eval:` local does not prompt" human check remains the live-boot confirmation. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-index-parser-never-evaluates-file-locals.md`.
- `register/invariant/index-sync-per-file-errors-contained`: reconciled, SOFTENED — containment does not cover the force full scan or a manual `vulpea-db-update-file` (ask-11). **Implication for step 4**: run the `eval:`-local check through BOTH the non-force scan and a force scan and record which path was exercised. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-index-sync-per-file-errors-contained.md`.
- `register/invariant/bounded-discovery-roots` / `register/boundary/session-dir-marker-walk`: reconciled / confirmed. Hidden-dir exclusion is vulpea's at both enumeration paths (sync predicate `vulpea-db-sync--org-file-p`; async `fd --exclude '.*'`), only the sync form is spec-covered; the `.agents/` walk is e2e-tested in hidden-agents-dir-spec.el:139-190. **Implication for step 4**: the "full re-scan indexes nothing under `.agents/`" check should exercise the async (fd) path as well. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-bounded-discovery-roots.md`.
- `register/invariant/edge-drawer-immediately-after-property-drawer`: DIVERGENT (ask-8). **Implication for step 3/4**: alongside the D7 prompt-hygiene spec, add a human check that `vulpea-note-meta-get` on a migrated session note is nil and a project note's `status` still reads `active` after edges exist — whichever placement option the user chose. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`.

### Already-shipped inline fixes
- `f014ee38`: the spike-eval runbook's stale passages were partially rewritten. **Implication for this task**: cross-check `config/org-graph/docs/spike-eval.org` § "Capability: Discovery" (:120-176) and § Findings/Discovery (:417) against the vault-only discovery before adding the "Workspace notes in vault" section, so the runbook does not contradict itself.

### Meta-discoveries
- `register-status-lags-commit-prose`: when recording findings in step 6, cite the reconciliation-note paths above (statuses live in interfaces.org), not commit messages.
- `stub-bounded-regression`: this task IS the un-stubbed end-to-end gate for the change; do not close it on suite-green alone — the step 4 human checks are the requirement.
