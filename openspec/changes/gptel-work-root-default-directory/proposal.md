## Why

A gptel session's `default-directory` is never set after its `session.org`
is opened, so it silently falls through to the session's own bookkeeping
directory (the `branches/<branch>/` metadata dir). When the model emits a
*relative* path, the validator resolves it against that bookkeeping dir
rather than the project the model meant — so every scope decision is "about
the wrong file": sometimes spuriously permissive, sometimes spuriously
restrictive, never meaningful. This is the root of the cwd bug surfaced
during the first live PersistentAgent test.

The scope validator itself is sound and fail-closed on *absolute* paths
(verified on this branch: deny-all defaults on missing config; deny-beats-allow
precedence; denials carry the `allowed-patterns` the expansion UI offers).
The only missing piece is giving each session an explicit working directory
("work root") so the model's relative paths land inside that already-correct
boundary. This change supplies it.

## What Changes

- Introduce a `GPTEL_WORK_ROOT` drawer key (Role 2 / work root), written into
  `session.org`'s `:PROPERTIES:` drawer at session-creation time.
- `jf/gptel--create-session-core` persists `GPTEL_WORK_ROOT` verbatim from the
  *same* `project-root` input it already expands into `GPTEL_SCOPE_*`. One
  input → two persisted outputs; cwd and scope agree by construction.
- The session-buffer binder (on `gptel-chat-mode-hook`) reads `GPTEL_WORK_ROOT`
  from the drawer it already scans and sets buffer-local `default-directory`
  from it. Keyless (pre-existing / hand-authored) sessions fall back to the
  current behavior (`branch-dir`) — graceful degradation, no migration.
- Tools (filesystem and bash, chat and agent alike) then resolve relative
  paths against the work root, because `gptel--handle-tool-use` runs every
  tool call inside `(with-current-buffer info:buffer)` — the originating
  session buffer (verified in `gptel-request.el`).
- PersistentAgent: the parent **passes** the agent's work root and access
  paths explicitly (upholding the existing ZERO-inheritance principle):
  - New `work_root` tool parameter (default: the parent's own work root),
    written into the agent's own drawer as `GPTEL_WORK_ROOT`.
  - **BREAKING (tool schema only):** split the single `allowed_paths`
    parameter into symmetric `read_paths` / `write_paths`. The accidental
    "write is always only `/tmp`" behavior is removed; `/tmp/**` is retained
    as auto-appended scratch space, not the write default.
  - A consistency guardrail warns when `work_root` falls outside the agent's
    read scope (the silent-DENY trap this change otherwise eliminates).

Not in scope: the worktree *manifest / discovery* source the parent model
uses to choose a `work_root` (workspace-neutral enumeration via the preamble)
— that is a separate follow-up (exploration-log Thread D).

## Capabilities

### New Capabilities

<!-- None. This change modifies behavior of existing capabilities; it
     introduces no new spec file. -->

### Modified Capabilities

- `sessions-persistence`: adds the `GPTEL_WORK_ROOT` drawer key and its
  create-time write (derived from the `project-root` input); the buffer
  binder sets `default-directory` from it on activation, with a `branch-dir`
  fallback when the key is absent.
- `scope`: states the cwd↔scope agreement invariant explicitly — relative
  paths resolve against the work root, which must equal the root used to
  expand `GPTEL_SCOPE_*`. Affirms (does not change) the verified fail-closed
  absolute-path resolution baseline.
- `persistent-agent`: the parent passes `work_root` and `read_paths` /
  `write_paths`; the agent's own drawer carries them (no inheritance);
  `/tmp/**` becomes auto-appended scratch rather than the write default; a
  work-root-within-read-scope consistency check is added.

## Impact

- **Code (literate, edit `.org` → tangle):**
  - `config/gptel/sessions/commands.org` — `create-session-core` (write
    `GPTEL_WORK_ROOT`); the binder (read it → `setq-local default-directory`).
  - `config/gptel/tools/persistent-agent.org` — `--task` signature + drawer
    write; tool `:args` schema (`work_root`, `read_paths`, `write_paths`);
    `--build-scope-plist` (read/write split, `/tmp` scratch).
- **Behavior:** relative-path scope decisions become meaningful for all
  sessions and agents; agents can write outside `/tmp` when the parent grants
  it. No on-disk migration (beta / no-migration default): existing sessions
  without the key behave exactly as today.
- **Decoupling invariant (unchanged):** gptel's work-root setup works with the
  workspaces package absent. Workspaces is one *producer* of the value (via the
  integration-registry `:home` payload at `:on-create`); gptel never names a
  workspaces symbol.
- **Tests:** Buttercup specs under `config/gptel/sessions/test/` (binder sets
  `default-directory`; keyless fallback) and `config/gptel/tools/test/` (agent
  drawer carries passed work_root + read/write paths; `/tmp` scratch retained;
  guardrail warning). Scope validation suite already green (195/195).
- **Tool API consumers:** the PersistentAgent schema change is re-read by the
  model each session (no persisted data), so no migration; documentation /
  preset guidance referencing `allowed_paths` must be updated.
