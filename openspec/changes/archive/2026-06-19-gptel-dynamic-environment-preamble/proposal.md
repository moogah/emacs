## Why

A gptel session's model is told its *role* (the system prompt) but not its
*environment* — it does not know its working directory or its file-access
boundary, so it wastes tool calls discovering them (and an agent assigned to a
specific worktree cannot tell where it is). The work root and scope are already
recorded in the session drawer (`GPTEL_WORK_ROOT`, `GPTEL_SCOPE_*`); this change
surfaces them to the model as a short, always-current environment block.

## What Changes

- Add a **dynamic environment block** to the composed system message of every
  `gptel-chat-mode` buffer (interactive chat *and* persistent agents — both are
  chat-mode), built per-send from the buffer's own drawer.
- Source the block from intrinsic, **workspace-neutral** inputs only: the work
  root (`GPTEL_WORK_ROOT` → `default-directory`) and the scope
  (`GPTEL_SCOPE_*`). No dependency on the workspaces package, `home.org`, or any
  external producer at runtime.
- Inject it at the **tail** of the system message, appended after the sibling
  system-prompt body, via the existing `gptel-request :before` pre-send seam —
  one seam, covering chat and agents uniformly.
- Recompute the block **wholesale each send** (idempotent: composed from the
  sibling file + the drawer every time, never read back from the prior value),
  so scope grown mid-session by `request_scope_expansion` stays accurate.
- Degrade gracefully on a chat-mode buffer with no scope drawer (plain chat):
  report `default-directory` and "no scope restrictions" rather than erroring.

Out of scope (deliberately): a worktree manifest / discovery list (deferred);
the agent write-scope fix (tracked separately in
`.tasks/agent-work-root-into-write-scope.md`; this change *assumes* agents have
appropriate write permissions).

## Capabilities

### New Capabilities
- `environment-preamble`: a per-send, workspace-neutral environment block (work
  root + file-access scope, with a short prose framing and a note that the
  information is live) appended to the composed system message of every
  gptel-chat-mode buffer, so chat sessions and agents know their cwd and scope
  boundary without discovery tool calls.

### Modified Capabilities
<!-- None at the requirement level. This change adds prompt-composition behavior
     not currently constrained by a chat-mode.md requirement; it does not alter
     scope-validation, activation, or buffer-format requirements. -->

## Impact

- **Code:** `config/gptel/chat/menu.org` — the pre-send seam (`:before` advice on
  `gptel-request`, currently `gptel-chat--refresh-system-prompt-from-file`) gains
  an unconditional environment-block append after the (conditional) sibling
  refresh; a new workspace-neutral builder reads the buffer's drawer
  (`GPTEL_WORK_ROOT`, `GPTEL_SCOPE_*`).
- **Depends on (already shipped):** the `GPTEL_WORK_ROOT` drawer key + binder
  (`default-directory`), and the `GPTEL_SCOPE_*` drawer keys
  (archive `2026-06-18-gptel-work-root-default-directory`).
- **Assumes (separate):** agents carry correct write scope —
  `.tasks/agent-work-root-into-write-scope.md`.
- **Coverage:** interactive chat buffers and persistent-agent buffers, with no
  agent-specific code (uniform builder reading each buffer's own drawer).
- **Tests:** Buttercup specs co-located under `config/gptel/chat/test/` covering
  the builder output and the pre-send composition/idempotency.
- **No breaking changes**, no on-disk format change, no migration.
