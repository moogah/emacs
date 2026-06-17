---
name: agent-workroot-and-paths
description: PersistentAgent passes work_root + read_paths/write_paths; drawer GPTEL_WORK_ROOT; guardrail
change: gptel-work-root-default-directory
status: ready
relations:
  - blocked-by:binder-default-directory
  - blocked-by:agent-build-scope-plist-split
---

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify) — `--task` (≈`:514`), drawer build
  (≈`:560-620`), tool `:args` registration (≈`:681`)

## Implementation steps
1. **Tool schema (`gptel-make-tool` `:args`, ≈`:681`)** — BREAKING change to the tool
   surface. Replace the single `allowed_paths` array param with three params, in this
   order (the tool is `:async t`, so gptel prepends `main-cb`; `:args` order maps to
   positional params AFTER it):
   - `work_root` (string) — absolute dir the agent works in; default = parent's work root.
   - `read_paths` (array of glob strings) — agent read scope (replaces `allowed_paths`).
   - `write_paths` (array of glob strings) — agent write scope (`/tmp/**` auto-added).
   Update the `:description` to guide the model: pass `read_paths`/`write_paths` from the
   parent's `:GPTEL_SCOPE_*` drawer keys; pass `work_root` (typically a worktree); omit
   `work_root` to default to the parent's. Remove the `allowed_paths` text.
2. **`--task` signature (`:514`)** — add the new params after `prompt`
   (`preset description prompt work-root read-paths write-paths`), normalizing arrays
   from vectors as the existing `allowed-paths-list` code does.
3. **Resolve work_root + default** — when `work-root` is nil/empty, default to the
   parent buffer's work root: `(or work-root default-directory)` evaluated in the parent
   buffer context (the `--task` runs there), normalized with `expand-file-name`. Freeze
   the resolved value (it goes into the agent's own drawer — not a live link).
4. **build-scope-plist call** — call the new `(read-paths write-paths)` signature from
   task `agent-build-scope-plist-split`.
5. **Drawer write (`:560-620`)** — splice `:GPTEL_WORK_ROOT:` into the agent drawer via
   `jf/gptel--append-drawer-property` (same helper used for `GPTEL_SESSION_ID` /
   `GPTEL_BRANCH`), value = resolved absolute work root. On activation the binder
   (`binder-default-directory` task) sets the agent buffer's `default-directory` from it.
6. **Guardrail (design D7)** — after resolving work_root and read-paths, if the resolved
   work_root is not matched by any read pattern (reuse
   `jf/gptel-scope--path-matches-any-pattern-p` if available, else a glob check), emit a
   `display-warning` / `jf/gptel--log 'warn` noting the work root is outside read scope.
   Do NOT abort creation.
7. Tangle + validate: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.

## Design rationale
Agents have ZERO inheritance from the parent (existing critical invariant). The parent
*passes* the agent's work root and read/write paths explicitly; the values are frozen in
the agent's own drawer at spawn, so a parent-supplied default is not runtime inheritance
(design D5). This also fixes the accidental `/tmp`-only write (via the split helper) and
gives the parent flexibility to assign each agent to a specific worktree. The guardrail
warns (not errors) because a parent may legitimately grant only absolute paths; warning
catches the silent-DENY trap without forbidding that case (design D7).

## Design pattern
- Drawer splice: copy the existing `(drawer-text (jf/gptel--append-drawer-property
  drawer-text "GPTEL_SESSION_ID" agent-session-id))` pattern in the same `--task` let*.
- Array normalization: copy the existing `allowed-paths-list` vector→list coercion.

## Verification
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` passes.
- Buttercup specs under `config/gptel/tools/test/persistent-agent/`:
  - Tool `:args` lists exactly `preset, description, prompt, work_root, read_paths,
    write_paths` and excludes `allowed_paths` / `denied_paths`.
  - WHEN created with explicit `work_root` THEN agent drawer has `:GPTEL_WORK_ROOT: <abs>`;
    on activation the agent buffer's `default-directory` is that dir.
  - WHEN `work_root` omitted THEN drawer records the parent's work root (frozen).
  - WHEN created with `read_paths`/`write_paths` THEN drawer `:GPTEL_SCOPE_READ:` /
    `:GPTEL_SCOPE_WRITE:` carry them and `/tmp/**` appears in write as scratch.
  - WHEN `work_root` is outside `read_paths` THEN a warning is emitted AND the agent is
    still created.
- Run: `./bin/run-tests.sh -d config/gptel/tools`.

## Context
design.md § Decisions 'D5 — Agent: parent passes work_root', 'D6', 'D7 — Consistency guardrail = warn'
specs/persistent-agent/spec.md § 'Requirement: Agent working directory (parent-supplied)',
  'Requirement: Tool invocation and validation', 'Requirement: Agent session creation',
  'Requirement: Configuration isolation (zero inheritance)'
