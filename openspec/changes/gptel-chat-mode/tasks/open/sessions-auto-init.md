---
name: sessions-auto-init
description: Rewrite auto-init hook for session.org and chat-mode activation
change: gptel-chat-mode
status: blocked
relations:
  - blocked-by:mode-definition
  - blocked-by:preset-wiring
  - blocked-by:sessions-filesystem
---

## Files to modify
- `config/gptel/sessions/commands.org` (modify — `jf/gptel--auto-init-session-buffer`
  and `jf/gptel--ensure-mode-once`)
- `config/gptel/sessions/commands.el` (tangled)
- `config/gptel/sessions/test/commands/auto-init-chat-mode-spec.el` (new)
- `config/gptel/sessions/test/commands/preset-application-spec.el` (new or
  modify — preset path unchanged but test needs updating to assert no
  `gptel-mode` call)

## Implementation steps
1. Rewrite `jf/gptel--auto-init-session-buffer`:
   - Change the path-regex match from `*/branches/<branch>/session.md` to
     `*/branches/<branch>/session.org` (and likewise for
     `*/agents/<agent>/session.org`).
   - On match:
     1. Extract `session-id` and `branch-name` from the path.
     2. Set the five buffer-local session variables
        (`jf/gptel--session-id`, `jf/gptel--session-dir`,
        `jf/gptel--branch-name`, `jf/gptel--branch-dir`,
        `jf/gptel--parent-session-id` when applicable) — unchanged from
        today.
     3. Register the buffer in `jf/gptel--session-registry` keyed
        `"<session-id>/<branch-name>"` — unchanged.
     4. Read `metadata.yml` from the branch directory and call
        `(gptel--apply-preset (intern (plist-get meta :preset)) (lambda
        (sym val) (set (make-local-variable sym) val)))`.
     5. Ensure `gptel-chat-mode` is active (no-op if already active,
        switch if not).
     6. Update the `current` symlink to point at this branch — unchanged.
2. Rewrite `jf/gptel--ensure-mode-once` to ensure `gptel-chat-mode` is
   active. It must NOT call `(gptel-mode 1)`.
3. **Remove** any invocations of `gptel--save-state` and
   `gptel--restore-state` from the session code path. The chat-mode file
   format is self-describing; no Local Variables round-trip is required
   or wanted.
4. **Precedence when both sources exist**: if a session file's
   `:PROPERTIES:` drawer declares a preset AND `metadata.yml` declares
   one, `metadata.yml` wins (sessions are the authoritative configuration
   for session files; Decision 16 point 2). Add a test covering this.
5. Tests:
   - Opening `~/.gptel/sessions/foo-20260420000000/branches/main/session.org`
     activates `gptel-chat-mode`, sets all five buffer-local vars,
     registers in registry, does NOT enable `gptel-mode`.
   - Agent file pattern (`agents/<name>/session.org`) works the same
     way.
   - Preset applied from `metadata.yml` via `gptel--apply-preset` with
     buffer-local setter (spy).
   - A `.org` file at an unrelated path (e.g. `~/notes/chat.org`) does
     NOT fire auto-init; no session vars are set; no registry entry is
     created; chat-mode still activates if the file declares
     `-*- gptel-chat -*-`.
   - `gptel--save-state` / `gptel--restore-state` are NOT called during
     auto-init (spies).

## Design rationale
Decision 16: session buffers use chat-mode exclusively. Running `gptel-mode`
and `gptel-chat-mode` together produces a mixed-format file that neither
parser can read cleanly. The three concrete wiring changes Decision 16
calls out are exactly:
1. Path regex matches `.org` not `.md`.
2. Mode switch goes to `gptel-chat-mode`, not `(gptel-mode 1)`.
3. No `gptel--save-state` / `gptel--restore-state` round-trip.

Decision 17 keeps the auto-init's existing **contract** (path-based
recognition, five buffer-local vars, registry entry, preset applied,
mode enabled) — the shape is unchanged, only the mode and format change.
This means activities-integration, branching, and any future session-
aware tool that depends on the buffer-local vars keeps working unchanged.

The `metadata.yml`-wins-over-property-drawer rule exists because sessions
are the source of truth for their own configuration: a user editing the
drawer in the buffer shouldn't silently override what the registry
believes about the session.

## Design pattern
Single-pass, idempotent function keyed on path — same shape as today's
auto-init. Extensions in the future (e.g. a different session layout for
sub-agents) should be one-line additions to the path-pattern recognizer,
not separate hook functions.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` passes the
  auto-init-chat-mode and preset-application suites.
- `grep -n "gptel--save-state\|gptel--restore-state\|gptel-mode\s*1" 
  config/gptel/sessions/commands.org` returns nothing in live code.
- Scenarios (sessions-persistence delta §"Auto-initialization enables
  gptel-chat-mode" and spec §"Session-file auto-initialization"):
  - Session file detection (branch path) → chat-mode active, session vars
    set.
  - Agent file detection → same with `branch-name = "main"`.
  - New session — preset from `metadata.yml`, chat-mode active,
    `gptel-mode` NOT enabled.
  - Existing session — no Local Variables round-trip; preset from
    `metadata.yml` only.
  - Path outside session layout — auto-init does not fire; buffer still
    usable as standalone chat.

## Context
- design.md §Decision 16 (sessions use gptel-chat-mode, never gptel-mode)
- design.md §Decision 17 (session auto-init contract under chat-mode)
- specs/gptel/sessions-persistence.md §"Auto-initialization enables
  gptel-chat-mode" (MODIFIED)
- specs/gptel-chat-mode/spec.md §"Session-file auto-initialization"
- architecture.md §`sessions/commands` (modified)
