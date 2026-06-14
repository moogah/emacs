---
name: emit-identity-keys-in-writers
description: "Emit GPTEL_SESSION_ID and GPTEL_BRANCH drawer keys at session creation, agent creation, and branch creation (agents also get their own id plus the parent link)."
change: gptel-content-addressed-session-activation
status: ready
cites_register_entries:
  - register/vocabulary/identity-drawer-keys
  - register/shape/drawer-text-block
  - register/invariant/branch-drawer-shares-id-not-branch
  - register/boundary/drawer-first-identity-resolution
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
