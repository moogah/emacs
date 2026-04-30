---
name: harden-persistent-agent-callback-on-error
description: Wrap jf/gptel-persistent-agent--task body in condition-case so synchronous errors translate to main-cb failures instead of hanging the parent FSM
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:final-verify-and-archive-prep
---

## Cites register entries

- Boundary: `scope-profile-applicator` (the agent path consumes `--render-drawer-text`; failures in that pipeline currently leak through `--task` without translation)

## Files to modify

- `config/gptel/tools/persistent-agent.org` — wrap the body of `jf/gptel-persistent-agent--task` in `condition-case`, mirroring the pattern in `config/gptel/scope/scope-tool-wrapper.org` lines 84-96.
- `config/gptel/tools/test/persistent-agent/` — add a `*-spec.el` covering the two known synchronous-error paths.

## Implementation steps

1. **Wrap the body.** In `jf/gptel-persistent-agent--task`, wrap the entire form following the parameter list (everything from `(unless jf/gptel--session-dir ...)` through the trailing `gptel-request` call) in a `(condition-case err ... (error ...))`. The error arm calls `main-cb` with a JSON-encoded structured failure:

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

2. **Add regression specs.** In `config/gptel/tools/test/persistent-agent/` create (or extend) a buttercup spec that exercises both synchronous-error paths:

   - **No parent session.** `cl-letf` `jf/gptel--session-dir` to nil; create a fake `main-cb` that records its single argument; invoke `--task` with valid-shaped args; assert the recorded callback arg is JSON-decodable to `(:success nil :error "agent_spawn_failed" :message <contains "PersistentAgent requires parent persistent session">)`.
   - **Unknown preset.** `cl-letf` `gptel-get-preset` to a stub returning nil; invoke with a preset name; assert the structured failure carries `:message` containing "Preset '...' not found".
   - **Negative control.** Spy `main-cb`; under conditions where `--task` would dispatch the agent successfully, assert `main-cb` is *not* invoked synchronously (the asynchronous DONE/ERRS/ABRT path remains the canonical exit). Stubbing `gptel-request` to a no-op is sufficient — we're not testing the agent flow here, only that the wrapper doesn't short-circuit in the success case.

3. **Run the suite.**

   ```bash
   ./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent
   make test
   ```

   Confirm new specs pass and no regression in the rest of the persistent-agent suite.

## Design rationale

`PersistentAgent` is registered with `:async t`. The contract for an `:async` gptel tool is that the function eventually calls its first-arg callback exactly once. A synchronous `user-error` violates this: from the dispatcher's POV the tool was launched but no callback ever arrives, leaving the parent FSM stuck in `TOOL` state with no `#+end_tool` block ever written.

`scope-tool-wrapper.org` already has the right pattern (lines 84-96): `condition-case` translates any caught condition into a `:success nil :error <classifier> :message ...` payload routed through the supplied callback. PersistentAgent's `--task` was rewritten in this change's lifecycle (cycle-1 `rebuild-persistent-agent-module`, cycle-2 `rewire-persistent-agent`) without that protection.

Two synchronous error paths exist today (line 432-433: parent session check; line 435-436: preset validation). Future paths added without `condition-case` would have the same hang signature. Wrapping the entire body is the structural fix; per-path defensive `if`s would not generalize.

## Design pattern

Mirror `scope-tool-wrapper.org` lines 84-96. Single `condition-case` at the top, structured-failure JSON via `main-cb`, do not attempt to recover state — once spawn fails, the agent did not run.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` — all specs pass, including the three new ones.
- Manual reproduction (smoke): from a parent persistent session, invoke PersistentAgent with a known-bad preset name. Confirm the tool block closes with an `:error "agent_spawn_failed"` payload and the parent FSM moves on. No hung state.
- Negative control: invoke PersistentAgent with valid args; agent spawns normally; tool block closes with the agent's final text on DONE.

## Context

Originally framed as the failure-mode-amplification layer of a smoke-test hang in `final-verify-and-archive-prep`. Subsequent diagnosis (instrumentation pass, 2026-04-30) revealed that the dispatcher in `gptel-chat-mode` **never invokes `--task`** when a `:confirm t` tool is requested by the LLM — the chat-mode tool-confirm UI is missing entirely. That is a separate, pre-existing chat-mode bug tracked in `.tasks/chat-mode-tool-confirm-ui-missing.md`. The smoke-test hang we observed was that bug, not Bug A.

This task therefore stands as **defense-in-depth**, not as the fix for any active regression. Bug A would manifest in the following scenarios:

1. The chat-mode tool-confirm UI bug is fixed (per `.tasks/chat-mode-tool-confirm-ui-missing.md`) AND a `:confirm t` tool's body has a synchronous error. Without `condition-case`, the FSM still hangs.
2. `--task` is invoked from a dispatch path that does have its own UI but does not own error translation (e.g. a programmatic invocation, a different mode wrapping the tool, future tooling).
3. A future maintainer adds a new synchronous-error path to `--task` (the existing two paths today are parent-session and preset checks; there are no `condition-case`'s at any level inside the function).

The `condition-case` wrapper is also the right structural choice because it mirrors `scope-tool-wrapper.org` lines 84-96, which is the established pattern for `:async t` tool wrappers in this repo. PersistentAgent is registered via `gptel-make-tool` (not `gptel-make-scoped-tool`), so it does not get that protection by default — this task lands the equivalent guard at the function level.

The complementary task `decouple-auto-init-state-from-preset-application` addresses an auto-init regression that *is* a real, current bug introduced by this change.

Both tasks block `final-verify-and-archive-prep`. Bug A is defense-in-depth (no active reproduction path until chat-mode UI is fixed); Bug B is an active regression. Severity for archive-readiness: Bug B is required; Bug A could in principle slip to a follow-up cycle if needed, but the fix is small and the pattern is established.
