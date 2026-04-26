---
name: fix-agent-flat-layout
description: Fix --task to write flat agent layout (no branches/main/ subdirectory) per spec
change: persistent-agent-rebuild
status: ready
relations:
  - discovered-from:add-agent-creation-tests
---

## Problem

`add-agent-creation-tests` surfaced a real spec/implementation drift:

- `openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md` § "Agent session creation" requires the flat agent layout: `<branch-dir>/agents/<agent>/session.org` with no `branches/` subdirectory and no `current` symlink (agents are single-timeline).
- The current implementation in `jf/gptel-persistent-agent--task` (`config/gptel/tools/persistent-agent.el`) routes session creation through `jf/gptel--create-session-core`, which is hard-coded to create `<session-dir>/branches/main/` plus a `current` symlink. With `session-dir = agent-dir`, the result is `<agent-dir>/branches/main/session.org` — doubly nested.

The bug also makes the auto-init's "branch session" regex accidentally match the doubly-nested path, masking the layout violation; the spec-correct "nested-agent" regex (`branches/<branch>/agents/<agent>/session.org`) was bypassed.

## Files to modify

- `config/gptel/tools/persistent-agent.org` (rewrite the post-creation block in `--task`)
- `config/gptel/tools/persistent-agent.el` (tangled output)

## Implementation steps

1. **Locate the existing creation block in `--task`** (around the `(jf/gptel--create-session-core …)` call). Currently it looks like:
   ```elisp
   (let* ((session-info
           (jf/gptel--create-session-core
            session-id session-dir preset-sym initial-content
            nil nil parent-id))
          (session-file (plist-get session-info :session-file))
          (agent-buffer (find-file-noselect session-file))
          ...))
   ```

2. **Replace `--create-session-core` with direct session-file creation**. The agent dir already exists (created by `jf/gptel--create-agent-directory`), `scope.yml` is already written by `--write-scope-file`, and the orchestrator only uses `:session-file` from the returned plist. Drop the indirection:
   ```elisp
   (let* ((session-file (jf/gptel--context-file-path session-dir))
          (_ (with-temp-file session-file
               (insert initial-content)))
          (_ (jf/gptel--log 'info "Created agent session file: %s" session-file))
          (agent-buffer (find-file-noselect session-file))
          (overlay (jf/gptel-persistent-agent--task-overlay
                    where preset description)))
     ...)
   ```
   Notes:
   - `jf/gptel--context-file-path` returns `<dir>/session.org` (defined in `config/gptel/sessions/filesystem.el`).
   - No `--update-current-symlink`, no `--create-branch-directory`, no preset-driven `--create-for-session` scope.yml. Each of those is either unwanted (symlink, branch dir) or already handled by `--write-scope-file`.
   - The `parent-id` argument is no longer passed to a creation function — it's already embedded in `initial-content` (Decision 4 of design.md), which is what `--create-session-core` would have used anyway.

3. **Tangle**:
   ```
   ./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
   ```

4. **Re-run `add-agent-creation-tests`** in the existing worktree at `.worktrees/task-add-agent-creation-tests-1777201126/`. The 2 previously-failing assertions should now pass:
   - `creates the agent directory under the parent branch` — asserts no `branches/` subdir and no `current` symlink.
   - `writes session.org with a self-describing :PROPERTIES: drawer` — asserts file at `<agent-dir>/session.org`, not the doubly-nested path.

5. **Re-run `add-agent-auto-init-reload-tests`** to confirm the workaround still produces a green result. After the layout fix, the agent's `session.org` is matched by the nested-agent regex (`branches/<branch>/agents/<agent>/session.org`) instead of accidentally by the branch-session regex. The test's observable assertions (mode is `gptel-chat-mode`, registry populated, drawer-restored locals) should remain green either way; the change is which auto-init code path fires.

## Verification

- `grep -n create-session-core config/gptel/tools/persistent-agent.{org,el}` returns no hits — the indirection is gone.
- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` passes including all 7 `it` blocks of `creation-spec.el`.
- A manual sanity check: drive `--task` against a temp parent session and confirm the on-disk shape is `<branch>/agents/<agent>/session.org` (no nested `branches/main/`).
- Full suite regression: `./bin/run-tests.sh --snapshot` matches the established baseline (10 unexpected ERT failures from bash-parser; everything else green).

**Done means**: `--task` writes the flat agent layout per spec, `add-agent-creation-tests` is green end-to-end, no regression in other persistent-agent specs.

## Context

This task was discovered during review of `add-agent-creation-tests` (commit `f056baa`) when 2 of its 7 `it` blocks failed against the existing `rebuild-persistent-agent-module` implementation. The reviewed task body for the rebuild specified `(jf/gptel--create-session-core …)` directly — the friction is in the design, not the implementation. Future spec-driven work touching agent creation should be aware that `--create-session-core` is branch-shaped by construction; agents need a different code path.

specs/persistent-agent/spec.md (delta) § "Agent session creation"
design.md § "Decisions" 4 (initial-content shape — unchanged)
config/gptel/sessions/commands.el — `jf/gptel--create-session-core` (the function the rebuild routed through)
config/gptel/sessions/filesystem.el — `jf/gptel--context-file-path`, `jf/gptel--create-agent-directory`
