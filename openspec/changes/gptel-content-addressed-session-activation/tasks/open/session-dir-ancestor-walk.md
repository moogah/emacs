---
name: session-dir-ancestor-walk
description: Derive session-dir for an open buffer by walking up to the nearest ancestor containing a branches/ child (agents use their own dir), replacing fixed ../.. layout walks.
change: gptel-content-addressed-session-activation
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — add `jf/gptel--session-dir-from-branch-dir` (the ancestor-marker walk).
- `config/gptel/sessions/test/filesystem/session-dir-walk-spec.el` (new) — Buttercup specs for branch and agent layouts plus depth-independence.

## Implementation steps

1. Write the spec first. Cover:
   - branch buffer at `<root>/branches/main/session.org` → session-dir = `<root>`;
   - a deeper/relocated layout (e.g. an extra wrapping dir) → still resolves to the dir whose child is `branches/` (depth-independent; no fixed `../..` count);
   - agent buffer at `<root>/branches/main/agents/<agent>/session.org` → session-dir = the agent's own directory.
2. Implement `jf/gptel--session-dir-from-branch-dir`:
   - Given the branch-dir (the file's own directory) and the session type (from `jf/gptel--session-type`, or the presence of `:GPTEL_PARENT_SESSION_ID:`):
     - `branch`: walk up from branch-dir via `locate-dominating-file` (or an explicit parent loop) to the nearest ancestor `D` such that `(file-directory-p (expand-file-name "branches" D))` and branch-dir is under `D/branches/`; return `D`.
     - `agent`: return branch-dir itself (agents do not branch; their own dir is their session root).
   - Be robust when no `branches/` ancestor is found (corrupt/standalone) — return branch-dir and log at debug.
3. Tangle and run the spec.

## Design rationale

After the `current` symlink is retired (see retire-current-symlink), session-dir's only job is "the directory under which `branches/` lives" — which this walk computes as its literal definition. It is move-safe and stores nothing. dir-locals was rejected (static-literal value is move-unsafe; cascade hands agents the parent's session-dir) and a drawer key would be redundant with what the walk derives for free. Finding a *container by structural marker* is legitimate navigation, distinct from the rejected practice of parsing *identity* out of path segments. (design.md §Decision D5.)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` — green, including the depth-independence case.
- Done = session-dir resolves correctly for branch and agent layouts without fixed `../..` walks.

## Context

design.md § Decision "D5. session-dir: ancestor-marker walk".

## Cycle 1 updates (cycle-1781448273)

- This task was **deferred from the cycle-1 batch** to avoid a `filesystem.org` collision with
  `drawer-signature-and-head-read` (now merged, commit 15f76fb). Implement against the **current**
  `filesystem.org`, which already contains the new "Session Content Signature" section + the
  `jf/gptel--scan-session-drawer-keys` engine — add the ancestor-walk alongside, do not disturb them.
- `register/boundary/session-dir-marker-walk` remains **speculated**; this task carries its disposition.
