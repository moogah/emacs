---
name: add-agent-auto-init-reload-tests
description: Buttercup specs verifying agent buffer activates chat-mode and saved files reload as interactive sessions
change: persistent-agent-rebuild
status: blocked
relations:
  - blocked-by:add-persistent-agent-test-fixtures
  - blocked-by:rebuild-persistent-agent-module
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/auto-init-reload-spec.el` (new)

## Implementation steps

1. **File header**:
   ```elisp
   ;;; auto-init-reload-spec.el --- Persistent-agent auto-init + reload tests -*- lexical-binding: t; -*-

   (require 'buttercup)
   (require 'jf-persistent-agent-test-helpers)
   (require 'gptel-persistent-agent)
   (require 'gptel-chat-mode)
   ```

2. **Required `it` cases**:

   - `it "the agent buffer is in gptel-chat-mode after creation"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Agent session creation" → "Agent buffer auto-initializes via find-file-hook"
     - Setup: `with-mock-parent-session` + `with-mock-preset 'test-preset` + `with-mock-gptel-request 'captured`.
     - Call the task. Capture the agent buffer (via `gptel-request`'s captured args' `:buffer` keyword, or by walking the buffer list for `*-test-preset-*` named buffers).
     - Expect `(buffer-local-value 'major-mode agent-buffer)` is `gptel-chat-mode`.
     - Expect `(buffer-local-value 'jf/gptel--session-id agent-buffer)` is the agent's session id (not the parent's).
     - Expect `(buffer-local-value 'jf/gptel--branch-name agent-buffer)` is `"main"`.
     - Expect `(buffer-local-value 'jf/gptel-autosave-enabled agent-buffer)` is non-nil.

   - `it "the agent buffer registers in jf/gptel--session-registry"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Agent session creation" → "Agent buffer auto-initializes via find-file-hook" (registry assertion piece)
     - Same setup.
     - Expect the agent's `session-id` is a key in `jf/gptel--session-registry` (or whatever the lookup function is — verify with `grep -n register-session config/gptel/sessions/registry.el`).

   - `it "the saved session.org reloads as an interactive chat session"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Persistence and resumption" → "Saved agent session reloads as interactive"
     - Setup: `with-mock-parent-session` + `with-mock-preset 'test-preset` + `with-mock-gptel-request`.
     - Call the task. Capture the agent buffer.
     - Kill the agent buffer (`(kill-buffer agent-buffer)`).
     - Re-open the agent's session.org via `(find-file-noselect <agent-dir>/session.org)`.
     - Expect the reloaded buffer is in `gptel-chat-mode`.
     - Expect `gptel-backend` and `gptel-model` buffer-locals match the preset's declarations (i.e., the drawer-driven preset application fired on reload).
     - Expect `jf/gptel--branch-name` is `"main"` and `jf/gptel--session-id` matches.
     - Expect `jf/gptel--parent-session-id` (if exposed via the chat preset machinery) reflects the parent session id from the drawer.

   - `it "no agent-specific auto-save hook is installed"`
     > Scenario: specs/persistent-agent/spec.md (delta) § "Persistence and resumption" → "No agent-specific auto-save hook"
     - With the persistent-agent module loaded (any test setup is fine).
     - Inspect `(default-value 'gptel-post-response-functions)`.
     - Expect it does NOT contain `jf/gptel--auto-save-session-buffer` (or any function defined in `persistent-agent.el`).
     - Also inspect a freshly-created agent buffer: `(with-current-buffer agent-buffer (memq 'jf/gptel--auto-save-session-buffer gptel-post-response-functions))` returns nil.

3. **A note on the registry-lookup test**: the existing registry module exposes register/lookup functions — confirm exact names with `grep -n 'defun jf/gptel--' config/gptel/sessions/registry.el` while writing the test.

4. **Test isolation**: `find-file-hook` and the auto-init function fire as side effects of `find-file-noselect`. To avoid pollution between tests, each test's `before-each` / `after-each` should kill any agent buffers it creates and clean up the registry entries. The fixture macros' temp-dir cleanup handles the on-disk state; buffer/registry cleanup is per-test.

## Design rationale

This task pins the contract that makes "saved agent → reloadable interactive session" work. It's the load-bearing user-facing claim of the whole change. If the drawer isn't written correctly, or if `find-file-hook` doesn't fire, or if `gptel-chat-mode` doesn't activate, this test catches it.

The "no agent-specific auto-save hook" assertion is a structural correctness check: the rebuild explicitly delegates autosave to the chat-mode auto-init pipeline. If a regression accidentally re-installs the old hook, this test fails.

## Design pattern

Real `find-file-noselect` calls (no mocking of file I/O) — same pattern as the creation tests. This is required because we're asserting on the side effect of `find-file-hook`, which requires the actual hook chain to fire.

For asserting on buffer-local values: `buffer-local-value` (functional, no `with-current-buffer` needed). For asserting on hook contents: inspect the hook variable directly (`gptel-post-response-functions`), both default value and buffer-local value.

For the reload test, kill-and-reopen mirrors the user's experience: they close the agent buffer, later they open the file. The auto-init pipeline must re-establish the same state from on-disk content alone.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` includes the new file and all `it` blocks pass.
- The reload test specifically: after kill + reopen, the buffer's `gptel-backend` is the preset's declared backend (not whatever happened to be the default at reload time).
- Each `it` block has a leading scenario-mapping comment.

**Done means**: 4 `it` blocks, all green, reload-as-interactive contract pinned end-to-end.

## Context

specs/persistent-agent/spec.md (delta) § "Agent session creation" (auto-init scenario), "Persistence and resumption"
design.md § "Decision 3" (find-file-noselect choice and find-file-hook semantics)
architecture.md § "Boundaries" → "Out of scope" (migration of pre-existing files; this test confirms NEW files reload, not OLD files)
