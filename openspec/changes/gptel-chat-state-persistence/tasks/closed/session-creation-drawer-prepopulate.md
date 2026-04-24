---
name: session-creation-drawer-prepopulate
description: Pre-populate session.org initial content with a PROPERTIES drawer containing GPTEL_PRESET (and GPTEL_PARENT_SESSION_ID for agents), and stop writing metadata.yml during session creation.
change: gptel-chat-state-persistence
status: needs-review
relations:
  - "blocked-by:chat-drawer-overrides-overlay"
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — introduce `jf/gptel--initial-session-content`; use it as the default for `initial-content` in `jf/gptel--create-session-core`; delete the `metadata.yml` write block.
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (modify) — assert the pre-populated drawer; drop `metadata.yml` assertions.
- `config/gptel/sessions/test/activities/activity-session-chat-spec.el` (modify) — same pattern.

## Implementation steps

1. In `commands.org`, add a new helper `jf/gptel--initial-session-content (preset-name &optional parent-session-id)` that returns a string:
   ```
   :PROPERTIES:
   :GPTEL_PRESET: <name>
   [:GPTEL_PARENT_SESSION_ID: <id>]
   :END:
   #+begin_user

   #+end_user
   ```
   The `GPTEL_PARENT_SESSION_ID` line appears only when `parent-session-id` is a non-empty string. Use `format` or `concat`; prefer raw string concatenation over org's drawer writers since this is a fresh file on disk, not a live buffer.
2. In `jf/gptel--create-session-core`, replace the current default for `initial-content` (which is `"#+begin_user\n\n#+end_user\n"`) with:
   - `(or initial-content (jf/gptel--initial-session-content preset-name parent-session-id))`
   - Add a `parent-session-id` parameter to `jf/gptel--create-session-core` (defaults to nil). Pass-through from callers:
     - Agent creation callers pass the parent session id.
     - Branch/standalone creation callers leave it nil.
3. Delete the "Write metadata.yml" block (around `commands.el:363-371`) and any related `with-temp-file` / `jf/gptel--write-session-metadata` calls. Drop `(require 'gptel-session-metadata)`.
4. Update `jf/gptel-persistent-session` interactive command to not write metadata.yml; ensure the preset-name is threaded into `jf/gptel--create-session-core`.
5. Audit `activities-integration.org` callers and update the `jf/gptel--create-session-core` call site to pass any parent-session-id (likely nil for activities) and remove the post-creation `metadata.yml`-style writes if any.
6. Tangle all three: `./bin/tangle-org.sh config/gptel/sessions/commands.org`, same for any other org files touched.
7. Rewrite `session-org-creation-spec.el` specs:
   - Replace "writes metadata.yml" assertions with "session.org contains `:PROPERTIES:` drawer with `:GPTEL_PRESET: <name>`".
   - Assert `metadata.yml` does NOT exist in the branch directory after creation.
   - Assert agent-session session.org contains `GPTEL_PARENT_SESSION_ID`.
8. Rewrite `activity-session-chat-spec.el` similarly — replace metadata assertions with drawer assertions.
9. Run `./bin/run-tests.sh -d config/gptel/sessions/test/commands` and `-d config/gptel/sessions/test/activities`.

## Design rationale

Constructing the drawer as a string at creation time is the simplest single-pass write (design.md §Decision 4). The shape matches what the save hook would produce on first save, so behaviour converges deterministically.

Removing the metadata.yml write step is a direct consequence of the drawer becoming authoritative (design.md §Decision 6). Tests that previously asserted metadata.yml contents switch to asserting drawer contents — coverage preserved, source of truth updated.

Threading `parent-session-id` through `jf/gptel--create-session-core` keeps the helper pure: the caller decides whether a parent exists; the helper writes whatever it is told. This avoids having the helper read `metadata.yml` from a parent directory (which no longer exists).

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org`
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands`
- `./bin/run-tests.sh -d config/gptel/sessions/test/activities`
- `grep -n "metadata.yml\|jf/gptel--write-session-metadata\|jf/gptel--metadata-file-path" config/gptel/sessions/commands.el` — no matches.
- Manually: `M-x jf/gptel-persistent-session`, inspect `branches/main/session.org` — contains the drawer; no `metadata.yml` next to it.

## Context

- proposal.md §What Changes (BREAKING session creation, BREAKING metadata.yml removal)
- specs/gptel/sessions-persistence.md §"Session creation" (MODIFIED), §"session.org as authoritative session file" (ADDED)
- architecture.md §"`jf/gptel--create-session-core` (modified)"
- design.md §Decisions 4, 6
