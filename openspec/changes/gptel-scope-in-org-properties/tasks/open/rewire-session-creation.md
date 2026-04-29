---
name: rewire-session-creation
description: Update session creation to embed drawer text in initial session.org; stop writing scope.yml
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:implement-profile-drawer-applicator
---

## Cites register entries

- `register/boundary/scope-profile-applicator` — session creation calls `--create-for-session` in mode 2a (string mode) and prepends the result to chat-mode initial content. Mode 2b is for branch/agent buffer cases.
- `register/shape/drawer-text-block` — what session creation embeds.
- `register/invariant/scope-drawer-no-duplication` — the prepend-once flow is what guards the invariant at session-creation time. If chat-mode hooks could insert a second drawer, this is where it would surface first.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-profile-applicator.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`

## Files to modify
- `config/gptel/sessions/commands.org` (modify) — `jf/gptel-persistent-session` and any related session-creation entrypoint should compose the drawer text from the resolved profile and prepend it to the chat-mode initial content before `write-region`.
- `config/gptel/sessions/filesystem.org` (possibly modify) — if it owns the actual file write, update there.
- `config/gptel/sessions/activities-integration.org` (modify) — the activities flow also creates sessions; rewire the same way.
- Tangle each touched file.

## Implementation steps

1. Locate the session.org file-write site (currently writes `"#+begin_user\n\n#+end_user\n"` plus the `:GPTEL_PRESET:` drawer). Identify how `jf/gptel-scope-profile--create-for-session` is currently called (it writes `scope.yml` as a side effect).

2. Change the flow:
   - Resolve the profile via `jf/gptel-scope-profile--resolve` (or whatever the existing call path is) to get the merged scope plist.
   - Build the drawer text via `jf/gptel-scope-profile--render-drawer-text` with the preset name, parent-session-id (nil for top-level sessions; populated for branches/agents), and the scope plist.
   - Write `session.org` in one shot: `(write-region (concat drawer-text "* Session\n#+begin_user\n\n#+end_user\n") nil session-org-path)` (or whatever the chat-mode initial content is — confirm by reading the existing creation code; the `gptel-org-mode-sessions` change settled this content to `#+begin_user\n\n#+end_user\n` per `sessions-persistence.md`).

3. Remove the call to `jf/gptel-scope-profile--write-scope-yml` (which is being deleted in `implement-profile-drawer-applicator` if it hasn't been already; if it's been stubbed-with-error, replace the call entirely).

4. Remove the directory step that creates `scope.yml` (no scope.yml is written anywhere now).

5. Update `jf/gptel-branch-session` (if it exists) to copy the parent branch's drawer scope keys into the new branch's `session.org`. Use `jf/gptel-scope-profile--render-drawer-text` with the parent's scope-plist (which the loader can recover from the parent branch's drawer). This matches the current behavior of copying `scope.yml` to the new branch.

6. Update activities-integration's session-creation path the same way.

7. Tangle and run `./bin/run-tests.sh -d config/gptel` — `session-creation-spec.el` tests that fixture or assert on `scope.yml` will fail; those are migrated in `migrate-session-creation-tests`.

## Design rationale

Per design.md § Decision 5 and the migration plan step 5, session creation produces drawer text and embeds it in initial content. Doing this at write time (one `write-region` call) — instead of "write file with empty drawer, then open buffer, then `org-entry-put`, then save" — avoids the buffer-lifecycle dance and keeps creation a single I/O step.

The branch-creation flow currently copies `scope.yml` from the parent branch. The drawer-resident equivalent reads the parent branch's drawer (via the loader) and renders new drawer text for the child branch. This preserves "branch inherits scope from parent at creation time" semantics.

## Design pattern

Single-shot `write-region` for new files, not "open buffer + save." See how `:GPTEL_PRESET:` is already pre-populated in the existing session-creation code (in `commands.org` — search for `:PROPERTIES:` in the file content building).

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `grep -rn 'scope\.yml' config/gptel/sessions/` returns no results.
- After a fresh `M-x jf/gptel-persistent-session "test-drawer"` (manual smoke), the resulting `session.org` carries a `:PROPERTIES:` drawer with `:GPTEL_PRESET:`, the resolved scope keys, then `#+begin_user\n\n#+end_user\n`.
- The session directory does NOT contain a `scope.yml` file.
- `./bin/run-tests.sh -d config/gptel` runs (session-creation tests will fail until `migrate-session-creation-tests`; other tests pass).

## Context

design.md § Decision 5 (initial-content embedding)
design.md § Migration Plan steps 5, 6
architecture.md § Boundaries
specs/gptel/sessions-persistence/spec.md § MODIFIED Requirements / "Directory structure initialization", "Scope profile integration"
specs/gptel/sessions-persistence/spec.md § REMOVED Requirements / "scope.yml file format"
specs/gptel/scope-profiles/spec.md § MODIFIED Requirements / "Integration with session creation"
