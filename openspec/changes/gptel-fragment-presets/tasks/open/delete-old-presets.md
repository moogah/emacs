---
name: delete-old-presets
description: Verify no snapshot/count test depends on the presets directory, delete the legacy .md presets, drop the yaml dependency if now unused, and refresh test-report snapshots.
change: gptel-fragment-presets
status: ready
relations:
  - "blocked-by:registration-rewrite"
  - "blocked-by:preset-workspace-assistant"
  - "blocked-by:preset-system-explorer"
  - "blocked-by:workspace-flip"
  - "blocked-by:wire-fragment-sources-load"
---

## Files to modify

- `config/gptel/presets/*.md` (delete) — `executor.md`, `explore.md`, `plan.md`,
  `research.md`, `zettelkasten.md`, `perplexity-researcher.md`, `minimal.md`,
  `system-explorer.md`, `test-agent-basic.md`, `test-agent-fs-scope.md`,
  `test-agent-bash-scope.md`.
- Any test asserting a preset **count** or loading `jf/gptel-presets-directory`
  (modify) — align to the new preset set.
- `config/gptel/preset-registration.org/el` (delete, if fully superseded by the
  presets sub-module registration).
- `test-report` snapshot files (refresh).

## Implementation steps

1. **Pre-deletion safety grep** (resolve design §Open Question):
   - `grep -rn "jf/gptel-presets-directory" config/ --include=*.el | grep -v runtime`
   - `grep -rn "Registered .* presets\|preset count\|test-agent-" config/gptel --include=*.el | grep -v runtime`
   - Inspect any `test-report`/snapshot that enumerates presets.
   - If a test (e.g. a scope or registration spec) loads a `test-agent-*` preset
     **as a fixture from this directory**, give it a dedicated fixture under the
     test tree instead of relying on a shipped preset, then proceed.
2. Delete the legacy `.md` presets listed above.
3. Remove `config/gptel/preset-registration.{org,el}` if the presets sub-module
   fully replaces it; update `gptel.org` load order accordingly.
4. Drop `(require 'yaml)` from the preset path; confirm no remaining consumer:
   `grep -rn "require 'yaml\|yaml-parse" config/gptel --include=*.el | grep -v runtime`.
5. Refresh test-report snapshots:
   `./bin/run-tests.sh --report` / `make test-report` and commit the updated
   snapshot files.
6. Run the full gptel suite; confirm no orphaned references to deleted presets.

## Design rationale

Fresh-start the preset set (proposal.md "remove all"); the new presets and the
fragment pipeline have replaced the legacy `.md` files and the workspace
`executor` coupling. Deletion is deliberately last and gated on the safety grep
so a fixture dependency cannot silently break the suite (design Risks).

## Verification

- `ls config/gptel/presets/*.md 2>/dev/null` (expect none).
- `grep -rn "executor" config/gptel --include=*.el | grep -v runtime` (expect no
  live references; research/corpus data files under bash-parser are unrelated).
- `./bin/run-tests.sh -d config/gptel`
- `git diff --stat` shows refreshed test-report snapshots only where expected.

## Context pointers

- Old presets: `config/gptel/presets/*.md`.
- `test-agent-*` references found in tests are buffer-name/agent-name strings, not
  file loads (verify in step 1): `config/gptel/tools/test/persistent-agent/error-handling-spec.el`,
  `config/gptel/tools/test/test-org-roam-integration.el`.
- Design Risks/Migration Plan step 6–7.

## Observations (cycle-1781944619 — executed inline by orchestrator)

- **Safety grep (design step 1) cleared deletion.** No tracked snapshot or test
  enumerates the preset directory or a preset count; every `jf/gptel-presets-directory`
  consumer reads the NEW `<name>/preset.el` + sibling `system-prompt.<ext>` structure
  (or is a test that rebinds it to a temp dir), none load the flat `.md` files. The
  `test-agent-*` hits are `"*test-agent-buffer*"` buffer names; the `executor` hits are
  the `'executor` preset NAME symbol, not `.md` file loads. Deleting the 11 flat `.md`
  files (`executor explore minimal perplexity-researcher plan research system-explorer
  test-agent-{basic,fs-scope,bash-scope} zettelkasten`) is pure cleanup — they were
  already inert at runtime (the registration only descends `<name>/preset.el` subdirs).
  Full-suite failing-set stayed BYTE-IDENTICAL to baseline (5 buttercup + 9 ERT) after
  deletion.

- **`yaml` dependency NOT dropped — there is a remaining live consumer.** Design step 4
  says drop `(require 'yaml)` only after confirming no remaining consumer. There IS one:
  `config/gptel/scope-profiles.el` (a production module, not test) still
  `(require 'yaml)` and calls `yaml-parse-string`. (`config/gptel/test/persistence-test-helpers.el`
  is a second, test-only, consumer.) So `yaml` stays. This matches the Cycle 2 stanza:
  registration-rewrite already removed yaml from the PRESET path in cycle 2; the
  project-wide drop is correctly blocked by the scope-profiles consumer. No yaml change
  this task.

- **Pre-existing, out-of-scope: `'executor` is still the session-creation default.**
  `config/gptel/sessions/commands.el` defaults `preset-name` to `'executor`
  (`(or preset-name 'executor)`), and several session specs assert `:GPTEL_PRESET: executor`
  in the drawer. There is NO `executor/preset.el`, so `'executor` already resolves to an
  UNregistered preset — this was true BEFORE this deletion (executor.md was inert since
  cycle 2). Deleting executor.md introduces no new breakage (those references use the
  name symbol / drawer string; none load the file; suite stays green). But the
  session-creation default still naming a now-nonexistent preset is a latent gap the
  proposal's "fresh-start the preset set" arguably implies cleaning. It is NOT in this
  task's write-set (sessions/commands.org) and NOT a named proposal outcome → see
  Discovery `executor-default-still-unregistered` (recommend externalise).

- **Snapshot-refresh (design step 5) is a no-op this cycle — refusing to churn.** The
  git-tracked `test-report.txt` snapshots are inconsistently maintained (e.g.
  `config/gptel/chat/test/menu/test-report.txt` reads "0 ran"; `config/gptel/test-report.txt`
  is a scoped subset). They were captured GREEN, but the current full suite carries the
  pre-existing 5-buttercup + 9-ERT failing-set, some of which live under
  `config/gptel/{tools,scope}`. Regenerating them would inject those pre-existing flaky
  failures into snapshots that don't currently show them — churn that misrepresents
  pre-existing flakiness as this change's doing, the opposite of the verification's
  "refreshed snapshots ONLY where expected." Nothing this task changes maps to a tracked
  snapshot (no snapshot enumerates presets; the loader specs live under `presets/` which
  has no tracked snapshot). So no snapshot is refreshed. See Discovery
  `stale-gptel-test-report-snapshots` (recommend externalise: a dedicated snapshot-hygiene
  pass that first triages the pre-existing failing-set).

## Discoveries

- discovery_id: disc-delete-old-presets-1
  class: invariant-gap
  description: |
    Design step 4 (drop the yaml dependency) is unsatisfiable project-wide:
    config/gptel/scope-profiles.el (production) still requires 'yaml and calls
    yaml-parse-string. The yaml dep is correctly retained. The "drop yaml" sub-goal
    was already scoped to the preset path only (done in cycle 2); the global drop is
    out of reach while scope-profiles consumes yaml.
  affected_register_entry: register/boundary/preset-org-to-registration
  recommendation: |
    No action. Note in the change record that yaml remains a live gptel dependency
    via scope-profiles; the preset pipeline no longer uses it (the proposal's stated
    "removes the yaml.el dependency from the preset pipeline" is satisfied — it is the
    PIPELINE, not the project, that drops yaml).

- discovery_id: executor-default-still-unregistered
  class: dead-branch
  description: |
    sessions/commands.el defaults session-creation preset to 'executor, which has no
    <name>/preset.el and is therefore unregistered (already true pre-deletion). The
    workspace initial preset was flipped to 'workspace-assistant (workspace-flip), but
    the general session-creation default still names 'executor. Latent: a default-path
    session records a :GPTEL_PRESET: executor drawer for a preset gptel can't resolve.
  affected_register_entry: register/boundary/preset-org-to-registration
  recommendation: |
    Externalise (cross-cutting; sessions/commands.org is outside this change's write-set
    and "pick the new default session preset" is not a named gptel-fragment-presets
    outcome). Follow-up: repoint the session-creation default to a registered preset
    (workspace-assistant or system-explorer) and update the asserting session specs.

- discovery_id: stale-gptel-test-report-snapshots
  class: invariant-gap
  description: |
    The git-tracked config/gptel/**/test-report.txt snapshots are inconsistent (some
    "0 ran"/"not run", top-level is a scoped subset) and were captured green while the
    live suite carries a stable 5-buttercup + 9-ERT pre-existing failing-set. They are
    not maintained per-cycle and don't track presets, so they are not a reliable
    regression artifact in their current state.
  affected_register_entry: null
  recommendation: |
    Externalise: a snapshot-hygiene pass that first triages/owns the pre-existing
    failing-set (.tasks already has scope/bash items for several), then regenerates the
    snapshots from a known state so they become a trustworthy regression baseline.
    Refreshing them mechanically THIS cycle would bake pre-existing flakiness into the
    committed artifacts and is therefore deliberately deferred.

## Cycle 2 updates (cycle-1781885402)

> registration-rewrite already did part of this task's deletion work — **scope reduced**.

- **Already deleted by registration-rewrite:** `config/gptel/preset-registration.org`
  and `.el` (the whole YAML parse/normalize/coerce module), and `(require 'yaml)` was
  removed from the **preset path**. Do not re-attempt those deletions.
- **Remaining scope for this task:** delete the old `.md` presets themselves (`executor`,
  `explore`, `plan`, `research`, `zettelkasten`, `perplexity-researcher`, `minimal`,
  `system-explorer.md`, `test-agent-*`) AFTER the snapshot/count-test grep (design step 6),
  and the **global** yaml-dep check (verify no OTHER module still requires `yaml` before
  dropping it project-wide — registration-rewrite only cleared the preset path).
- `register/boundary/preset-org-to-registration` is **confirmed**; the new registration
  ignores flat `.md` files in `presets/` (only descends `<name>/preset.el` subdirs), so
  the old `.md` presets are already inert at runtime — deletion is cleanup, not behavior.

## Cycle 5 updates (cycle-1781944619 — final cycle)

> This is the change's **final** cycle. This task is batched with two
> robustness/test-quality tasks and **merges LAST** so its snapshot refresh
> captures the complete, final test suite.

- **Batch = [extract-shared-loader-helper-failpolicy, harden-fragment-txt-golden,
  delete-old-presets]**; merge order: loader-helper → txt-golden → **this task last**.
  Both sibling tasks ADD/restructure specs (a load-error spec in
  `load-sources-spec.el`/`registration-spec.el`; a drift-catching golden in
  `creation-spec.el`/`environment-preamble-spec.el`/`composer-spec.el`). The
  test-report snapshot this task refreshes (step 5) MUST be taken **after** those
  land — refreshing earlier would commit a snapshot that goes stale the moment the
  sibling specs merge.
- **Re-run the snapshot refresh against the merged tree**, not against an
  intermediate state. If executed in a worktree, rebase/refresh the snapshot at
  merge time (merge-order-3) rather than at implementation time.
- Write-set is otherwise disjoint from the siblings: this task deletes flat
  `presets/*.md` and refreshes `test-report` snapshots; it does **not** touch
  `registration.{org,el}` (owned by extract-shared-loader-helper-failpolicy) or the
  golden specs (owned by harden-fragment-txt-golden).
- Everything from the Cycle 4 stanza below still holds (protect active
  `presets/registration.*` and `presets/sources/*`; only flat legacy `.md` in scope;
  safety grep first).

## Cycle 4 updates (cycle-1781941375)

> ALL blockers cleared — this task is now **ready** and is the last remaining
> substantive in-change task (alongside the test-quality followup
> `harden-fragment-txt-golden`). It is the change's final cleanup cycle.

- **Blockers satisfied:** `wire-fragment-sources-load`, `migrate-prelude-preamble`,
  and `workspace-flip` all landed done+reviewed this cycle (eda66755, a63f90e0,
  b4d68f82). registration-rewrite + both presets were already done. Status flipped
  blocked → ready.
- **DO NOT delete `config/gptel/presets/registration.{org,el}`.** That is the ACTIVE
  presets sub-module and now hosts BOTH loaders: `jf/gptel-preset-register-all`
  (`<name>/preset.el` discovery) AND the new `jf/gptel-fragment-sources-directory` /
  load-sources-all (flat `presets/sources/*.el` discovery, added by
  wire-fragment-sources-load). The deletion target named in step 3 was the OLD
  top-level `config/gptel/preset-registration.{org,el}` (YAML pipeline) — already
  deleted by registration-rewrite in cycle 2. Re-confirm it's gone; do not touch the
  active `presets/registration.*`.
- **New source files exist under `presets/sources/`** (`environment.*`,
  `emacs-prelude.*`, `agent-preamble.*` + committed `.txt`). These are live fragment
  sources, NOT legacy presets — do not delete them. Only the flat legacy `.md` files
  directly under `config/gptel/presets/` are in scope.
- `register/boundary/preset-org-to-registration` gained a cycle-4 addendum (the
  sources-discovery sibling loader); `register/boundary/sources-directory-load` is now
  confirmed. Neither expands this task's deletion scope.
- Safety grep (design step 1) still stands before any deletion.

## Cycle 3 updates (cycle-1781900938)

> Both new presets landed; this task picked up a new blocker.

- **New blocker: `wire-fragment-sources-load`** (created this cycle). Do not delete legacy
  presets / drop `yaml` until the fragment SOURCE-load wiring is real — deleting while the
  env (and next cycle's prelude/preamble) seams are still dark would conflate two failures.
  Current `blocked_by`: `workspace-flip`, `wire-fragment-sources-load` (the two presets and
  registration-rewrite blockers are now satisfied).
- **Both new presets exist and register** (`workspace-assistant`, `system-explorer` via
  `<name>/preset.el`). The legacy `system-explorer.md` (flat file) is now safely deletable —
  it is shadowed by the new `system-explorer/preset.el` and inert at runtime.
- `register/boundary/preset-org-to-registration` gained an addendum (forwards `:backend`
  unchanged; render-locus = load-time defconst) and `register/shape/preset-config-plist`
  was reconciled (`:backend` = gptel backend NAME STRING). Neither changes this task's
  deletion scope, but the `:backend "Claude"` form is what the surviving presets carry.
- Pre-deletion safety grep (design step 1) still stands; additionally confirm no test loads
  a legacy `.md` preset as a fixture now that the new pipeline is the only live path.
