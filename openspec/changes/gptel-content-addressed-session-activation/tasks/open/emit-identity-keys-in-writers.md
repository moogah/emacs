---
name: emit-identity-keys-in-writers
description: "Emit GPTEL_SESSION_ID and GPTEL_BRANCH drawer keys at session creation, agent creation, and branch creation (agents also get their own id plus the parent link)."
change: gptel-content-addressed-session-activation
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — `jf/gptel--create-session-core` (`:544`) emits `:GPTEL_SESSION_ID:` and `:GPTEL_BRANCH:` into the rendered drawer.
- `config/gptel/tools/persistent-agent.org` (modify) — `jf/gptel-persistent-agent--task` emits the agent's own `:GPTEL_SESSION_ID:` and `:GPTEL_BRANCH: main` (alongside the existing `:GPTEL_PARENT_SESSION_ID:`).
- `config/gptel/sessions/branching.org` (modify) — branch creation sets the new branch's `:GPTEL_BRANCH:` to the new branch name and `:GPTEL_SESSION_ID:` to the shared session id (overwriting the values copied verbatim from the parent drawer).
- `config/gptel/sessions/test/commands/identity-keys-emission-spec.el` (new) — Buttercup specs asserting drawer content for branch, agent, and post-branch.

## Implementation steps

1. Write the spec first. Cover:
   - fresh branch session drawer contains `:GPTEL_SESSION_ID: <id>` and `:GPTEL_BRANCH: main`;
   - fresh agent drawer contains its own `:GPTEL_SESSION_ID:`, `:GPTEL_BRANCH: main`, and `:GPTEL_PARENT_SESSION_ID: <parent>`;
   - a branch created from a parent: the new branch's drawer carries the SAME `:GPTEL_SESSION_ID:` as the parent but its own new `:GPTEL_BRANCH:` (parent's branch value not leaked).
2. In `create-session-core`, splice the two keys using `jf/gptel--append-drawer-property` (`commands.org:445`), preserving the `:PROPERTIES:`/`:END:` adjacency invariant. Thread the session-id and branch-name already available to the function.
3. In `persistent-agent--task`, emit the agent's own session-id (the agent's generated id) and `:GPTEL_BRANCH: main` next to the existing parent-id emission.
4. In branch creation, after the parent drawer is copied verbatim, overwrite `:GPTEL_BRANCH:` with the new branch name (and ensure `:GPTEL_SESSION_ID:` equals the shared id). Use a drawer-key replace (not append) so duplicates are not produced.
5. Tangle each edited `.org`; run the new spec and the branching spec.

## Design rationale

Identity travels with the file. Writers own emission; the resolver (drawer-identity-resolver task) owns reading. Agents get their own `:GPTEL_SESSION_ID:` for a single uniform identity rule across branches and agents, with the parent link kept separate. Splicing via the existing helper keeps the canonical drawer concatenation invariant intact. (design.md §Decisions D2, D3; specs `sessions-branching` Requirement "Configuration inheritance via drawer".)

## Verification

- `./bin/tangle-org.sh` on all three edited files succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` and `-d config/gptel/sessions/test/branching` — green.
- Done = all three creation paths emit the identity keys; branch creation does not leak the parent's branch name.

## Context

design.md § Decisions "D2. Identity" and "D3. Agent identity"; specs `sessions-persistence` Requirement "Drawer-resident session identity", `persistent-agent` Requirement "Agent identity in the drawer", `sessions-branching` Requirement "Configuration inheritance via drawer".

## Observations

- The identity keys spliced in `jf/gptel--create-session-core` are discarded
  on the caller-supplied `INITIAL-CONTENT` override path (the `(or
  initial-content (concat drawer-text ...))` short-circuit), exactly as the
  pre-existing `:GPTEL_SYSTEM_PROMPT_FILE:` splice already is. The interactive
  command passes nil, so identity keys are always emitted for real session
  creation. No caller in tree passes non-nil `initial-content`, so this is
  latent; flagged only so a future override caller knows it owns the whole
  drawer (including identity keys) in that case.
- `jf/gptel--create-branch-session` is NOT the buffer-save path: it rewrites
  the on-disk `session.org` directly (the new branch's buffer is opened
  afterward by `jf/gptel-branch-session` via `find-file`). The identity-key
  rewrite therefore lands before the branch buffer is ever visited — no
  buffer/disk race.

## Discoveries

- discovery_id: disc-emit-identity-keys-in-writers-1
  class: duplication
  description: |
    The task asked for a drawer-key REPLACE helper. I added TWO:
    (a) `jf/gptel--replace-drawer-property` (string→string, in
        `commands.org`, mirroring `jf/gptel--append-drawer-property`)
        for use by string-shaped drawer producers, and
    (b) `jf/gptel--set-drawer-property-in-buffer` +
        `jf/gptel--rewrite-branch-identity-keys` (buffer/file-shaped,
        in `branching.org`) for the branch rewrite.
    The branch rewrite does NOT reuse (a) even though it could, because
    `branching.el` loads BEFORE `commands.el` in the gptel.org session
    module order (branching at gptel.org:337, commands at :346).
    `(require 'gptel-session-commands)` from `branching.el` is a
    load-time failure (confirmed by a red test run). The buffer-shaped
    helper in branching is self-contained and avoids the cycle.
    So (a) currently has no production caller — it is the canonical
    string REPLACE companion to the existing append helper, added for
    symmetry/discoverability. If the register prefers a single REPLACE
    primitive, the load-order constraint must be resolved first (e.g.
    hoist the splice helpers into a lower-level module both `commands`
    and `branching` can require).
  affected_register_entry: register/shape/drawer-text-block
  recommendation: |
    Either (1) accept the two-helper split as a load-order
    accommodation and note it on drawer-text-block, or (2) hoist
    `jf/gptel--append-drawer-property` / `jf/gptel--replace-drawer-
    property` into a dependency-free module (e.g. a new
    `gptel-session-drawer.el` required by both commands and branching)
    so branch rewrite reuses the one string REPLACE primitive.
- discovery_id: disc-emit-identity-keys-in-writers-2
  class: invariant-gap
  description: |
    register/invariant/branch-drawer-shares-id-not-branch assumes the
    parent drawer ALREADY carries `:GPTEL_SESSION_ID:`/`:GPTEL_BRANCH:`
    (verbatim copy then overwrite). For sessions created AFTER this
    task that holds. But a parent `session.org` predating identity-key
    emission has neither key. My branch rewrite handles this: the
    setter is append-if-absent, so a legacy parent gains `:GPTEL_
    SESSION_ID:` (= shared session id, derived from the session-dir
    basename) and `:GPTEL_BRANCH:` (= new branch name) on first branch.
    The invariant statement says "overwriting values copied verbatim
    from the parent drawer" — for legacy parents there is nothing to
    overwrite, so the behavior is insert, not replace. Worth noting on
    the invariant so the migration case is explicit.
  affected_register_entry: register/invariant/branch-drawer-shares-id-not-branch
  recommendation: |
    Amend the invariant to cover the legacy-parent case: the branch
    writer must SET the keys (replace-or-insert), not assume they are
    present to overwrite. Current implementation already does this.
