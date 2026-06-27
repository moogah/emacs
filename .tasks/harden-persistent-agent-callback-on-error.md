---
name: harden-persistent-agent-callback-on-error
description: Wrap jf/gptel-persistent-agent--task body in condition-case so synchronous errors translate to main-cb failures instead of hanging the parent FSM. Defense-in-depth — no active reproduction path until chat-mode-tool-confirm-ui-missing is fixed.
status: ready
source: openspec/changes/gptel-scope-in-org-properties
relations:
  - discovered-from:final-verify-and-archive-prep
  - blocked-by:.tasks/chat-mode-tool-confirm-ui-missing.md
---

> Surfaced during cycle-4 smoke testing of `gptel-scope-in-org-properties` as the suspected root cause of a PersistentAgent FSM hang. Subsequent instrumentation revealed the actual hang is `chat-mode-tool-confirm-ui-missing` (separate, pre-existing chat-mode bug). This task therefore stands as defense-in-depth, not as the fix for an active regression. Externalised to `.tasks/` per cycle-4 user disposition (2026-04-30) since the chat-mode UI bug is the activation precondition; deferring this until that lands.

## The bug in one sentence

`jf/gptel-persistent-agent--task` is registered as `:async t` but has two synchronous-error paths (no parent session; unknown preset) that signal `user-error` without invoking the supplied callback, violating the `:async t` contract. When/if a dispatcher invokes it that does NOT own its own error translation, the parent FSM hangs in `TOOL` state with no `#+end_tool` block ever written.

## Why this is currently latent

`gptel-chat-mode` does not invoke `--task` at all when a `:confirm t` tool is requested — the chat-mode tool-confirm UI is missing entirely (see `chat-mode-tool-confirm-ui-missing.md`). So the active hang reported during cycle-4 smoke was that bug, not this one. Bug activation requires either:

1. The chat-mode tool-confirm UI bug to be fixed (per `.tasks/chat-mode-tool-confirm-ui-missing.md`) AND a synchronous error in `--task`. Without `condition-case` here, the FSM still hangs.
2. `--task` is invoked from a dispatch path that has its own UI but does not own error translation (programmatic invocation, a different mode wrapping the tool, future tooling).
3. A future maintainer adds a new synchronous-error path to `--task` (today there are two: parent-session check, preset validation; no `condition-case` at any level inside the function).

## Files to modify

- `config/gptel/tools/persistent-agent.org` — wrap the body of `jf/gptel-persistent-agent--task` in `condition-case`, mirroring the pattern in `config/gptel/scope/scope-tool-wrapper.org` lines 84-96.
- `config/gptel/tools/test/persistent-agent/` — add a `*-spec.el` covering the two known synchronous-error paths plus a negative control.

## Implementation steps

1. **Wrap the body.** In `jf/gptel-persistent-agent--task`, wrap the entire form following the parameter list in a `(condition-case err ... (error ...))`. The error arm calls `main-cb` with a JSON-encoded structured failure:

   ```elisp
   (defun jf/gptel-persistent-agent--task (main-cb preset description prompt
                                                    &optional allowed-paths)
     "..."
     (condition-case err
         (progn
           (unless jf/gptel--session-dir
             (user-error "PersistentAgent requires parent persistent session"))
           (let ((preset-sym (intern preset)))
             (unless (gptel-get-preset preset-sym)
               (user-error "Preset '%s' not found in gptel--known-presets" preset))
             ;; ... existing body ...
             ))
       (error
        (funcall main-cb
                 (json-serialize
                  (list :success nil
                        :error "agent_spawn_failed"
                        :message (format "PersistentAgent error: %s"
                                         (error-message-string err))))))))
   ```

   Tangle and confirm the `.el` is updated.

2. **Add regression specs.** In `config/gptel/tools/test/persistent-agent/` create (or extend) a buttercup spec exercising both synchronous-error paths:

   - **No parent session.** `cl-letf` `jf/gptel--session-dir` to nil; record `main-cb`'s argument; assert it's JSON-decodable to `(:success nil :error "agent_spawn_failed" :message <contains "PersistentAgent requires parent persistent session">)`.
   - **Unknown preset.** `cl-letf` `gptel-get-preset` to a stub returning nil; assert the structured failure carries `:message` containing "Preset '...' not found".
   - **Negative control.** Spy `main-cb`; under conditions where `--task` would dispatch successfully (stub `gptel-request` to no-op), assert `main-cb` is *not* invoked synchronously.

3. **Run the suite.**

   ```bash
   ./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent
   make test
   ```

## Design rationale

`PersistentAgent` is registered with `:async t`. The contract for an `:async` gptel tool is that the function eventually calls its first-arg callback exactly once. A synchronous `user-error` violates this: from the dispatcher's POV the tool was launched but no callback ever arrives, leaving the parent FSM stuck in `TOOL` state.

`scope-tool-wrapper.org` lines 84-96 already have the right pattern: `condition-case` translates any caught condition into a `:success nil :error <classifier> :message ...` payload routed through the supplied callback. PersistentAgent's `--task` was rewritten in the source change's lifecycle (cycle-1 `rebuild-persistent-agent-module`, cycle-2 `rewire-persistent-agent`) without that protection.

Wrapping the entire body is the structural fix; per-path defensive `if`s would not generalize to future synchronous-error paths.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` — all specs pass, including the three new ones.
- Manual reproduction (smoke, after chat-mode UI bug is fixed): from a parent persistent session, invoke PersistentAgent with a known-bad preset name. Confirm the tool block closes with an `:error "agent_spawn_failed"` payload and the parent FSM moves on.
- Negative control: invoke PersistentAgent with valid args; agent spawns normally; tool block closes with the agent's final text on DONE.

## Activation precondition

Do not pick this task up before `.tasks/chat-mode-tool-confirm-ui-missing.md` is dispositioned. Until that is fixed, `--task` is unreachable from `gptel-chat-mode`, and the regression specs cover the only paths that exercise this code today. Landing this task earlier is harmless but pointless — the protection is dormant.

## Context

- Active regression sibling (in-change, NOT externalised): `decouple-auto-init-state-from-preset-application` — addresses the auto-init bug actually introduced by the source change.
- Source change: `openspec/changes/gptel-scope-in-org-properties/` (after archival: `openspec/archive/gptel-scope-in-org-properties/`)
- Smoke transcript that surfaced this: `/Users/jefffarr/.gptel/sessions/smoke-drawer-20260430085658/branches/main/session.org`
- Activation precondition: `.tasks/chat-mode-tool-confirm-ui-missing.md`
