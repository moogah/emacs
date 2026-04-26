---
name: verify-end-to-end
description: Run full gptel suite + manual smoke test of PersistentAgent against a real preset
change: persistent-agent-rebuild
status: blocked
relations:
  - blocked-by:add-chat-mode-public-api-tests
  - blocked-by:add-agent-creation-tests
  - blocked-by:add-agent-auto-init-reload-tests
  - blocked-by:add-agent-send-completion-tests
  - blocked-by:add-agent-error-handling-tests
  - blocked-by:scrub-denied-paths-references
---

## Files to modify

None (verification-only task).

## Implementation steps

1. **Full gptel-subsystem test run**:
   ```
   ./bin/run-tests.sh -d config/gptel
   ```
   Expect: the failure count matches the established baseline. As of the rebuild's start point (commit `4e641c7`), the baseline was 10 ERT failures (bash-parser pattern-flow / corpus / a sessions filesystem test) and 24 Buttercup failures (scope/expansion + parallel-tool-callback + run_bash_command), totalling 34 pre-existing failures unrelated to this change. The post-change run must show no NEW failures relative to that baseline — fail counts equal or lower, every still-failing test traceable to the baseline list. Any new failure indicates a rename ripple or an integration regression and must be fixed in the originating task (rename-chat-mode-internals, rebuild-persistent-agent-module, add-chat-mode-public-api-tests) before re-running.

2. **Buttercup-specific run** for the new specs (sanity check that they're discovered):
   ```
   ./bin/run-tests.sh -d config/gptel/chat/test/parser/public-api-spec.el
   ./bin/run-tests.sh -d config/gptel/chat/test/send/public-api-spec.el
   ./bin/run-tests.sh -d config/gptel/chat/test/stream/public-api-spec.el
   ./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent
   ```
   Each should report all `it` blocks passing.

3. **Lexical-binding header check**:
   ```
   ./bin/run-tests.sh -d config/core/test/test-lexical-binding-headers.el
   ```
   Confirms no tangled file lost its `lexical-binding: t` mode line during the rewrite (per project memory, this is a known footgun).

4. **Manual smoke test**: launch the isolated Emacs and exercise PersistentAgent against a real preset.

   ```
   ./bin/emacs-isolated.sh -nw
   ```

   In Emacs:
   1. `M-x jf/gptel--create-session` (or whatever the standard session-creation entry point is — verify with the user's normal workflow).
   2. Choose a preset that's available in your environment.
   3. From the parent session buffer, send a prompt that calls PersistentAgent. Example user message:
      ```
      Use PersistentAgent with preset=<your preset>, description="hello agent",
      prompt="Greet me in three sentences.", and allowed_paths=[].
      ```
   4. Observe:
      - The parent buffer shows an overlay with "<Preset> Task: hello agent" and a "Waiting…" indicator.
      - A new agent buffer appears in the buffer list (named `<session.org file>` or similar).
      - The overlay updates to "Calling Tools…" if the preset uses tools, else stays in "Waiting…" through to completion.
      - On completion, the overlay disappears and the parent buffer receives the agent's three-sentence greeting as a tool result.
   5. Find the agent buffer, kill it, then `C-x C-f <agent-dir>/session.org`.
      - The reopened buffer is in `gptel-chat-mode`.
      - `M-x describe-mode` confirms `gptel-chat-mode` is active.
      - `M-: gptel-backend` (in the reopened buffer) returns the preset's declared backend, not a stale parent value.

5. **Document results** of the smoke test in a brief note (verbal or in the change's `tasks/closed/` notes when this task completes). If any step fails, file a follow-up task or reopen the relevant earlier task.

6. **Validate openspec change**:
   ```
   openspec validate persistent-agent-rebuild
   ```
   No structural errors in proposal/specs/architecture/design/tasks.

7. **Spec scenario coverage audit** (light-touch). For each scenario in `specs/persistent-agent/spec.md` and `specs/chat-mode/spec.md` (deltas), confirm that at least one `it` block in the new test files maps to it via comment. The scenario-mapping comments make this a `grep` exercise:
   ```
   grep -rn 'Scenario:' config/gptel/tools/test/persistent-agent \
                       config/gptel/chat/test/parser/public-api-spec.el \
                       config/gptel/chat/test/send/public-api-spec.el \
                       config/gptel/chat/test/stream/public-api-spec.el
   ```
   Cross-reference against the scenarios listed in the deltas. Any uncovered scenario gets a follow-up `it` added (small, keep this task tight — don't expand scope).

## Design rationale

The previous tasks each verify one piece of the contract. This task is the integration check: do they actually compose into a working system?

Two distinct verifications matter here:
1. **Test-suite pass-through**: zero regressions in the broader gptel subsystem. Renames + rewrites are exactly the kind of change where an out-of-date caller, a missed import, or a load-order issue surfaces only in the full suite.
2. **Manual smoke test**: catches things the test suite can't easily — interactive UX (does the overlay actually look right?), real backend handshake (does the preset's backend authenticate?), real LLM response shape (does `extract-final-text` find the trailing text in a real response?).

The reload-as-interactive smoke step (step 4.5) is the key user-facing contract: after `:include t` returns the agent's text to the parent, the user might want to inspect or continue the agent's conversation. Reopening the saved file should "just work."

## Verification

- All test commands listed above succeed.
- Manual smoke test completes without errors.
- `openspec validate persistent-agent-rebuild` reports no errors.
- Every spec scenario has at least one `it` mapping (per the grep audit).

**Done means**: the change is verified end-to-end, ready for `/opsx-archive`.

If this task surfaces issues that fall outside the change's scope (e.g., a pre-existing test failure in an unrelated subsystem, or a new finding about gptel itself), park them in `.tasks/` per the cross-cutting follow-ups convention rather than expanding this change.

## Context

architecture.md § "Boundaries" → "In scope"
design.md § "Sequencing for Task Generation" — this is the final task in the graph
proposal.md § "Impact" — affected code, affected tests
