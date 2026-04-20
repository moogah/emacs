---
name: sessions-persistent-create
description: jf/gptel-persistent-session writes session.org with chat-mode initial content
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:mode-definition
  - blocked-by:sessions-filesystem
---

## Files to modify
- `config/gptel/sessions/commands.org` (modify — `jf/gptel-persistent-session`
  creation path)
- `config/gptel/sessions/commands.el` (tangled)
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (new)

## Implementation steps
1. Update `jf/gptel-persistent-session` so the session-file it creates is
   `session.org` with chat-mode initial content:
   ```
   #+begin_user

   #+end_user
   ```
   (a single empty user block — matches Decision 9's new-chat initial
   content so a fresh session looks identical to a fresh standalone chat
   buffer).
2. Remove any code that:
   - Writes a markdown heading, `###`, or other markdown-specific
     initial content.
   - Invokes `gptel--save-state` during creation.
   - Writes a Local Variables block.
3. `metadata.yml` population is **unchanged** — same fields (`session_id`,
   `created`, `updated`, `preset`), same serialisation. Only the
   conversation-file contents change.
4. Scope generation from preset profile (`scope.yml`) is **unchanged**.
5. Tests (Buttercup):
   - Create a session with a default preset — assert `session.org` exists
     with exactly `#+begin_user\n\n#+end_user\n`, no Local Variables
     block, no markdown content.
   - `metadata.yml` contains `session_id`, `created`, `updated`, `preset`.
   - Opening the created buffer activates `gptel-chat-mode` (auto-init —
     depends on task `sessions-auto-init` being in place, but the
     creation test itself can use a simple `find-file` on the tempdir).

## Design rationale
Decision 9 (new-chat initial content) and Decision 18 (session.org format)
converge on the same empty-user-block template so chat-mode behaves
identically whether the buffer was created as a persistent session or as a
free-standing chat. Consistency here means users don't have to remember
"is this a session or a free-standing chat" when looking at an empty
buffer.

Decision 18 also eliminates the Local Variables round-trip: chat-mode's
block format is self-describing. Removing the `gptel--save-state` call
from creation is part of this — persistence is plain `save-buffer`.

## Design pattern
Session creation is a pure filesystem operation: make directories, write
template files, seed `metadata.yml`, generate `scope.yml`. No buffer
activation inside the creation helper — the caller (or a file-opening
workflow) triggers auto-init which activates chat-mode.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes
  session-org-creation suite.
- Scenario (sessions-persistence delta §"Session creation writes
  session.org"):
  - Create session with default preset → `session.org` exists with
    chat-mode initial content; `metadata.yml` populated; file opens in
    `gptel-chat-mode` with preset applied.

## Context
- design.md §Decision 9 (new-chat initialization)
- design.md §Decision 18 (session file format)
- specs/gptel/sessions-persistence.md §"Session creation writes
  session.org" (MODIFIED)
- specs/gptel/sessions-persistence.md §"Fresh session.org" scenario
- architecture.md §`sessions/commands` (modified)
