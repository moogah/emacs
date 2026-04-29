---
name: rewire-persistent-agent
description: Update persistent-agent creation to embed scope drawer in initial session.org; stop writing scope.yml for agents
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:implement-profile-drawer-applicator
---

## Cites register entries

- `register/boundary/scope-profile-applicator` — agent creation also dispatches through `--create-for-session` (or the `--render-drawer-text` helper directly, for the file-write-in-one-shot path described in the architecture).
- `register/shape/drawer-text-block` — what agent creation embeds in the agent's initial `session.org`.
- `register/invariant/scope-drawer-no-duplication` — exactly one drawer in the agent's `session.org`.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-profile-applicator.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify) — replace the `scope.yml` write step with drawer-text embedding in the agent's initial `session.org` content.
- Tangle: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.

## Implementation steps

1. Locate the agent-creation function in `persistent-agent.org` (today it's around `persistent-agent.el:111` — `Write SESSION-DIR/scope.yml with read paths from ALLOWED-PATHS`). It currently:
   - Builds a scope plist from `allowed-paths`, denied-paths, and a hardcoded set of standard denies (`**/.git/**`, `**/runtime/**`, `**/.env`, `**/node_modules/**`).
   - Writes `scope.yml` to the agent dir.
   - Writes `session.org` (currently with a `:PROPERTIES:` drawer containing `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:`) — see `persistent-agent.el:139`.

2. Build the scope plist the same way (read paths from `allowed-paths`, write `/tmp/**`, deny standard set).

3. Render drawer text via `jf/gptel-scope-profile--render-drawer-text` with the agent's preset name, the parent session ID, and the scope plist. Confirm `--render-drawer-text` emits `:GPTEL_PARENT_SESSION_ID:` correctly (it should, per `implement-profile-drawer-applicator`).

4. Write the agent's `session.org` in one shot using the rendered drawer text + chat-mode initial content. Replace the existing `(format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n" ...)` literal with the call to `--render-drawer-text` plus the chat-mode initial content.

5. Delete the `scope.yml` write step entirely. Verify no `scope.yml` is created in the agent directory anywhere in the lifecycle.

6. Update any messages or docstrings that reference `scope.yml` to reference the agent's drawer instead. There's at least one user-facing message at `persistent-agent.el:283` ("Use the read_file_in_scope tool on scope.yml to get your current allowed...") — rewrite to point at the agent's `session.org` drawer.

7. Tangle. Run `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` — tests that fixture or assert on `scope.yml` will fail; those are migrated in `migrate-persistent-agent-tests`.

## Design rationale

Per Decision 5 in design.md, agent creation embeds drawer text in initial content rather than opening a headless buffer. This preserves the existing single-shot file-write pattern (already used for the `:GPTEL_PRESET:` / `:GPTEL_PARENT_SESSION_ID:` drawer) and avoids the buffer-lifecycle dance during creation.

Configuration isolation (zero inheritance) is unchanged — the agent's drawer is built from the explicit `allowed-paths` parameter, never from the parent's drawer. The spec scenario "Path configuration never inherited" carries forward verbatim, just with "drawer" substituted for "scope.yml".

## Design pattern

Single-shot file write for new agent files (the existing pattern). See `persistent-agent.el:139` for the current literal `(format ":PROPERTIES: ... :END:\n#+begin_user...")` shape — keep that structure, but build the drawer block via `--render-drawer-text` instead of the inline `format`.

## Verification

- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` succeeds.
- `grep -n 'scope\.yml' config/gptel/tools/persistent-agent.el` returns no results (or only in deprecated docstring text scheduled for cleanup).
- After a manual smoke (create a parent session, invoke PersistentAgent with `allowed_paths`), the agent's directory contains `session.org` with a populated drawer (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, `:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:` ×4) — and no `scope.yml`.
- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` runs (tests will fail until `migrate-persistent-agent-tests`; the tangling and macro behavior pass).

## Context

design.md § Decision 5 (Persistent-agent embeds drawer in initial content)
design.md § Migration Plan step 7
specs/gptel/persistent-agent/spec.md § MODIFIED Requirements / "Agent session creation", "Configuration isolation (zero inheritance)"
specs/gptel/persistent-agent/spec.md § REMOVED Requirements / "scope.yml in agent directory"
