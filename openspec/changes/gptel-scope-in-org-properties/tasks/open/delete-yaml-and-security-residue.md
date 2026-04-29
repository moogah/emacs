---
name: delete-yaml-and-security-residue
description: Delete scope-yaml module, the yaml/ test directory, dead scope-file helpers, YAML-era prose drift, and any remaining :security plist references
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

- `register/invariant/scope-no-security-key-in-plist` — confirm zero references to `:security` survive across the cleanup. This task is the lint-style enforcement layer of the invariant (the test enforces it on loader output; this task enforces it on source code).
- `register/shape/scope-config-plist` — verify the shape contract documentation in `interfaces.org` matches the post-cleanup reality.

## Files to modify
- `config/gptel/scope/scope-yaml.org` (DELETE)
- `config/gptel/scope/scope-yaml.el` (DELETE — tangled output)
- `config/gptel/scope/test/yaml/` (DELETE entire directory recursively)
- `config/gptel/gptel.org` (modify) — remove the `(jf/load-module .../scope/scope-yaml.el)` line and its accompanying comment.
- `config/gptel/scope-profiles.org` (modify) — remove `(require 'jf-gptel-scope-yaml ...)` if it remains after `implement-profile-drawer-applicator`.
- `config/gptel/scope/scope-expansion.org` (modify) — remove `(require 'jf-gptel-scope-yaml)` if it remains after `rewire-expansion-writer`.
- `config/gptel/scope/interfaces.org` (modify) — confirm `:security` is gone from the canonical scope-config-shape (this should already be done by `add-drawer-encoding-contract`; this task verifies and removes any straggler text). **Cycle-2 extension**: also rewrite the YAML-era architectural prose at lines 15, 21, 29, 32, 95, 112-114, 181, 339, 347-350, 416, 430 — see § "Audit project-internal architecture docs" below.
- `config/gptel/scope/scope-validation.org` (modify, audit) — confirm no `:security` plist reads remain.
- `config/gptel/scope/scope-shell-tools.org` (modify) — line 171 LLM-prompt prose says "Add to scope - Permanently add patterns to scope.yml"; rewrite to reference the `:PROPERTIES:` drawer.
- `config/gptel/sessions/filesystem.org` (modify) — DELETE `jf/gptel--scope-file-path` (zero callers post cycle-2).
- `config/gptel/sessions/constants.org` (modify) — DELETE `(defconst jf/gptel-session--scope-file ...)` (only consumer was the helper above).

## Implementation steps

1. Delete the YAML module:

   ```bash
   git rm config/gptel/scope/scope-yaml.org
   git rm config/gptel/scope/scope-yaml.el
   git rm -r config/gptel/scope/test/yaml/
   ```

2. Update `gptel.org` to remove the loader line. In `config/gptel/gptel.org`, find:

   ```elisp
   ;; Load scope-yaml boundary module (required by all scope modules)
   (jf/load-module (expand-file-name "config/gptel/scope/scope-yaml.el" jf/emacs-dir))
   ```

   Delete both lines. Tangle `gptel.org`.

3. Search for and remove any remaining `(require 'jf-gptel-scope-yaml ...)`:

   ```bash
   grep -rln 'jf-gptel-scope-yaml' config/
   ```

   Each hit should be in a `.el` file generated from a `.org` source — edit the source, tangle.

4. Search for any remaining `:security` plist reads:

   ```bash
   grep -rn ':security' config/gptel/scope/
   ```

   Audit each hit. Anything reading `(plist-get config :security ...)` is dead code; remove it. Anything in a docstring or comment describing the old shape should be updated to reference the new constants.

5. Audit `interfaces.org` for any remaining `:security` mention in the canonical shape; remove.

5b. **Audit project-internal architecture docs for YAML-era prose** (cycle-2 finding `arch-cycle-1777470320-2`):

    ```bash
    grep -n 'scope-yaml\|scope\.yml\|YAML' config/gptel/scope/interfaces.org
    grep -n 'scope-yaml\|scope\.yml' config/gptel/scope/scope-shell-tools.org
    ```

    For `interfaces.org`: rewrite each hit to reference the `:PROPERTIES:` drawer instead of `scope.yml`. The ASCII-art diagram at lines 28-95 needs to be redrawn to omit the `scope-yaml.el` module entirely. The module-table row for `scope-yaml.el` (lines 112-114) is deleted; the `scope-expansion.el` row's "scope.yml writer" text becomes "drawer writer (`org-entry-put-multivalued-property`)".

    For `scope-shell-tools.org`: the LLM-prompt line ("Add to scope - Permanently add patterns to scope.yml") becomes "Add to scope - Permanently add patterns to the session's `:PROPERTIES:` drawer in `session.org`."

5c. **Delete dead scope-file helpers** (cycle-2 finding `arch-cycle-1777470320-1`):

    ```bash
    grep -rn 'jf/gptel--scope-file-path\|jf/gptel-session--scope-file' config/
    ```

    Both names should have zero callers after cycle-2's rewires. Delete:
    - `(defun jf/gptel--scope-file-path ...)` block in `config/gptel/sessions/filesystem.org`
    - `(defconst jf/gptel-session--scope-file ...)` block in `config/gptel/sessions/constants.org`

    Tangle both `.org` files. Verify the tangled `.el` files no longer carry these symbols.

6. Tangle every modified `.org`:

   ```bash
   ./bin/tangle-org.sh config/gptel/gptel.org
   ./bin/tangle-org.sh config/gptel/scope/scope-validation.org
   ./bin/tangle-org.sh config/gptel/scope/scope-expansion.org
   ./bin/tangle-org.sh config/gptel/scope-profiles.org
   ./bin/tangle-org.sh config/gptel/scope/interfaces.org
   ```

7. Reload Emacs (or evaluate the modules) and run `./bin/run-tests.sh` for the full suite. Anything that referenced `jf-gptel-scope-yaml` will fail loudly.

## Design rationale

This is the cleanup task. By the time it runs, every caller has been rewired to the drawer reader/writer/applicator and the test suites have been migrated. Deleting the dead code in one task gives a clean reviewable diff: "everything that referenced YAML scope is gone."

Combining the `:security` cleanup with the YAML module deletion (rather than splitting into two tasks) reflects that they're the same removal: both were tied to the scope-yaml schema. Anything that reads `:security` after the rewires is dead.

## Design pattern

Single grep-and-delete sweep, with tangle after each `.org` edit, then full test run. Standard literate-programming cleanup workflow.

## Verification

- `./bin/run-tests.sh` passes (full suite).
- `find config/gptel -name 'scope-yaml*' -type f` returns no results.
- `find config/gptel/scope/test/yaml -type f` returns no results (the directory is gone).
- `grep -rn 'scope-yaml\|jf-gptel-scope-yaml\|:security' config/gptel/scope/` returns no results.
- `grep -rn 'scope.yml' config/gptel/` returns only historical references (e.g. archive/, openspec/, migration documentation comments) — no live code references.
- `grep -rn 'jf/gptel--scope-file-path\|jf/gptel-session--scope-file' config/` returns no results (both deleted per cycle-2 finding-1).
- `grep -n 'scope-yaml\|scope\.yml\|YAML' config/gptel/scope/interfaces.org config/gptel/scope/scope-shell-tools.org` returns no results except possibly historical commits or change-log notes.
- Emacs starts cleanly via `./bin/emacs-isolated.sh -nw --batch --kill` (or whatever the smoke check is) without "void-function jf/gptel-scope-yaml--..." errors.

## Context

design.md § Migration Plan steps 9, 10
architecture.md § Components (Components removed)
specs/gptel/scope/spec.md § REMOVED Requirements / "scope-yaml boundary module", "scope.yml on-disk persistence", ":security configuration section"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/invariant/scope-no-security-key-in-plist`: speculated → reconciled. L1 (loader output omits `:security`) holds today. **L2 (no readers in `scope-validation.el`) is what THIS task ships** — the grep step 4 ("Search for any remaining `:security` plist reads") is the structural-audit enforcement. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-no-security-key-in-plist.md`.
- `register/shape/scope-config-plist`: speculated → reconciled. Producers field updated; verify `interfaces.org`'s canonical shape documentation matches the new producer set (`--load-from-buffer`, `--load-from-file`, and `--load-config` post-rename). See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-scope-config-plist.md`.

### Meta-discoveries
- `invariant-gap-class/deletion-invariant-L1-L2-split`: future "X is removed" invariants should template L1+L2 from the start. **Implication for this task**: in addition to deleting YAML/`:security`, consider adding a permanent grep-based buttercup spec (per finding-6 `recommended_resolution`) that asserts `grep -rn ':security' config/gptel/scope/` returns no results. This codifies the L2 enforcement as a runtime regression check, not a one-time cleanup.

### Open follow-up: expected test failures co-owned with rewire-session-creation
> Per `state.json::implement-profile-drawer-applicator.execute.expected_test_failures_owner`, this task and `rewire-session-creation` jointly own 13 expected test failures introduced in cycle 1. The 8 YAML Boolean Normalization specs become obsolete once the YAML module + `test/yaml/` directory are deleted (this task's step 1).

## Cycle 2 updates (cycle-1777470320)

### Cited register entries — disposition flips

- `register/invariant/scope-no-security-key-in-plist`: speculated → **confirmed**. L1 (loader output) and L2 (no `:security` reads in `scope-validation.el`) both hold. This task is the **lint-style sweep** across the rest of `config/gptel/`. See `.orchestrator/cycles/cycle-1777470320/reconciliations/invariant-scope-no-security-key-in-plist.md`.
- `register/shape/scope-config-plist`: speculated → **confirmed**. Now includes `:read-metadata`. See `.orchestrator/cycles/cycle-1777470320/reconciliations/shape-scope-config-plist.md`.

### Cycle-2 architect findings absorbed by this task

- **`arch-cycle-1777470320-1` (advisory, dead-branch)**: `jf/gptel--scope-file-path` (in `sessions/filesystem.org:187`) and `jf/gptel-session--scope-file` (in `sessions/constants.org:91`) have zero callers. Folded into "Files to modify" and step 5c. See `.orchestrator/cycles/cycle-1777470320/findings/arch-cycle-1777470320-1.md`.
- **`arch-cycle-1777470320-2` (advisory, interface-drift)**: ~14 lines of YAML-era prose in `config/gptel/scope/interfaces.org` and 1 line in `config/gptel/scope/scope-shell-tools.org`. Folded into step 5b. See `.orchestrator/cycles/cycle-1777470320/findings/arch-cycle-1777470320-2.md`.

### Cycle-2 implementation details to honour

- `scope-validation.el`: cycle-2's rewire-validator-config-load (commit `a813d53`) removed the YAML loader and all `:security` plist reads. Verification step 4 is now mostly a regression check rather than active cleanup.
- `scope-expansion.el`: cycle-2's rewire-expansion-writer (commit `18e290a`) removed the YAML writer family and the `(require 'jf-gptel-scope-yaml)` import. Step 3 is now a regression check.
- `persistent-agent.el`: cycle-2's rewire-persistent-agent (commit `486d09f`) removed the `--write-scope-file` writer and any `scope.yml` writes from agent creation. The `(require 'gptel-scope-profiles)` import was added; this task does NOT remove it.
- `sessions/commands.el` and `sessions/branching.el`: cycle-2's rewire-session-creation (commit `be6b80c`) removed the `scope.yml` write paths. Comments referencing `scope.yml` ("No scope.yml is written", "scope.yml is no longer copied") are correct documentation and should be retained.

### Now unblocked

- `rewire-validator-config-load` (closed cycle-2)
- `rewire-expansion-writer` (closed cycle-2)
- `rewire-session-creation` (closed cycle-2)
- `rewire-persistent-agent` (closed cycle-2)

### Independence

This task is no longer blocked by anything. It can run as part of cycle-3 alongside the migrate-* tasks (the migrate-* tasks operate on test files; this operates on production files; no overlap).
