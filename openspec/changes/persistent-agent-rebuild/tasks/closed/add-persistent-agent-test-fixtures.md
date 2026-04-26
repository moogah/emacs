---
name: add-persistent-agent-test-fixtures
description: Add shared Buttercup fixtures for persistent-agent tests (mock parent session, mock preset, mock gptel-request)
change: persistent-agent-rebuild
status: needs-review
relations: []
---

## Files to modify

- `config/gptel/tools/test/persistent-agent/helpers-spec.el` (new) — actually a test-helpers file (despite the `-spec.el` suffix, this is a `provide`-style fixture module loaded by sibling specs; matches the pattern of `config/gptel/scope/test/helpers-spec.el` in the existing tree)
- `config/gptel/tools/test/persistent-agent/` (new directory)

## Implementation steps

1. **Create the directory**:
   ```
   mkdir -p config/gptel/tools/test/persistent-agent
   ```

2. **Write `helpers-spec.el`**. File header:
   ```elisp
   ;;; helpers-spec.el --- Shared fixtures for persistent-agent tests -*- lexical-binding: t; -*-

   ;;; Commentary:
   ;; Shared test infrastructure for the persistent-agent suite.
   ;; Provides:
   ;;   `jf/persistent-agent-test--with-mock-parent-session'  — temp session-dir
   ;;                                                            with parent session
   ;;                                                            buffer-locals set
   ;;   `jf/persistent-agent-test--with-mock-preset'          — register an
   ;;                                                            ephemeral preset
   ;;   `jf/persistent-agent-test--with-mock-gptel-request'   — capture args + FSM
   ;;                                                            instead of network
   ;;
   ;; This file uses `provide' so sibling spec files may `require' it.

   ;;; Code:
   (require 'cl-lib)
   (require 'gptel)
   (require 'gptel-session-filesystem)
   ```

3. **`jf/persistent-agent-test--with-mock-parent-session`** macro. Sets up a temp session-dir matching `nested-agent-re` so `find-file-hook` auto-init recognizes child agent paths. Sketch:
   ```elisp
   (defmacro jf/persistent-agent-test--with-mock-parent-session (&rest body)
     "Run BODY inside a temp parent persistent session.
   The temp dir matches the layout `<root>/<session-id>/branches/main/`
   so that nested-agent paths under `agents/` are recognized by
   `jf/gptel--auto-init-session-buffer'.

   Inside BODY, the following buffer-local-style bindings are visible
   as `let'-scoped variables:
     `mock-session-dir' — `<root>/<session-id>/`
     `mock-branch-dir'  — `<root>/<session-id>/branches/main/`
     `mock-session-id'  — the session-id string

   On exit, the temp tree is removed."
     (declare (indent 0) (debug t))
     `(let* ((root (make-temp-file "pa-test-" t))
             (mock-session-id (format "test-session-%d" (random 1000000)))
             (mock-session-dir (expand-file-name mock-session-id root))
             (mock-branch-dir (expand-file-name "branches/main" mock-session-dir)))
        (unwind-protect
            (progn
              (make-directory mock-branch-dir t)
              ;; Write a minimal session.org so jf/gptel--valid-branch-directory-p
              ;; returns t when the agent layout is opened.
              (with-temp-file (expand-file-name "session.org" mock-branch-dir)
                (insert ":PROPERTIES:\n:GPTEL_PRESET: dummy\n:END:\n#+begin_user\n#+end_user\n"))
              (let ((jf/gptel--session-id mock-session-id)
                    (jf/gptel--session-dir mock-session-dir)
                    (jf/gptel--branch-name "main")
                    (jf/gptel--branch-dir mock-branch-dir))
                ,@body))
          (delete-directory root t)))))
   ```
   Note: `let`-binding the session vars is sufficient when the agent code does NOT depend on them being buffer-local in the parent. The agent reads them via `(boundp/symbol-value)` at task entry. If a test needs the parent buffer specifically, it should use `(setq-local …)` inside `with-temp-buffer`.

4. **`jf/persistent-agent-test--with-mock-preset`** macro. Registers a preset and ensures cleanup:
   ```elisp
   (defmacro jf/persistent-agent-test--with-mock-preset (name &rest body)
     "Register a minimal gptel preset NAME (a symbol) for the duration of BODY."
     (declare (indent 1) (debug t))
     `(let ((preset-name ,name))
        (unwind-protect
            (progn
              (gptel-make-preset preset-name
                :description "test preset"
                :backend gptel-backend
                :model gptel-model)
              ,@body)
          (setq gptel--known-presets
                (assq-delete-all preset-name gptel--known-presets)))))
   ```

5. **`jf/persistent-agent-test--with-mock-gptel-request`** macro. Captures args and FSM, returns immediately without network:
   ```elisp
   (defmacro jf/persistent-agent-test--with-mock-gptel-request (capture-var &rest body)
     "Stub `gptel-request' to push its args (as a plist) onto CAPTURE-VAR.
   CAPTURE-VAR is a symbol naming a list variable in the surrounding scope.
   Each captured invocation is a plist:
     (:prompt PROMPT :buffer BUF :callback CB :context CTX :fsm FSM ...)"
     (declare (indent 1) (debug t))
     `(cl-letf (((symbol-function 'gptel-request)
                 (lambda (prompt &rest args)
                   (push (cons :prompt (cons prompt args)) ,capture-var)
                   nil)))
        ,@body))
   ```

6. **`provide`** at end:
   ```elisp
   (provide 'jf-persistent-agent-test-helpers)
   ;;; helpers-spec.el ends here
   ```

7. **Smoke-test the fixtures** by writing a tiny `describe` at the bottom of the file (or in a sibling spec, your call) that exercises each macro once. This is just to confirm the file loads cleanly under Buttercup discovery — actual usage is covered by the consuming specs in tasks 5–8.

## Design rationale

The agent tests exercise file I/O paths (find-file → auto-init), preset application, and FSM composition. Centralizing the three repetitive setup patterns in one fixture file means each consuming spec stays focused on what it asserts, not on setup boilerplate.

The "use `let` not `setq-local` for session vars in the mock parent" decision: the agent's task entry point reads session vars via dynamic lookup. Letting them is sufficient and avoids having to construct + dispose of a fake parent buffer for every test. Tests that specifically care about parent-buffer behavior (e.g., overlay placement) construct a `with-temp-buffer` and `setq-local` inside the mock-parent-session block.

The mock `gptel-request` doesn't simulate FSM transitions — it just captures what the agent passed in. Send/completion tests that need to drive the FSM through state transitions use `gptel-make-fsm` directly with the captured handler list, then call handler entries in order. This keeps the mock simple and the FSM tests realistic (they exercise the actual handler composition, not a mocked simulation of it).

## Design pattern

The shape of `gptel-chat-test--with-buffer` (in `config/gptel/chat/test/test-helpers.el`) is the canonical fixture pattern for this codebase: a single macro that sets up + tears down, one feature per macro, `provide`-able. Match that shape.

The agent-specific fixtures parallel `config/gptel/scope/test/helpers-spec.el` (existing) — that file uses `*-spec.el` suffix for a fixtures module, so the parser/runner conventions accept that shape.

## Verification

- The file loads under Buttercup without errors:
  ```
  emacs --batch -l config/gptel/tools/test/persistent-agent/helpers-spec.el -f buttercup-run
  ```
- `(featurep 'jf-persistent-agent-test-helpers)` returns non-nil after load.
- The smoke-test `describe` (if added) passes.
- A consuming spec in the next tasks (creation-spec, etc.) can `(require 'jf-persistent-agent-test-helpers)` and use the macros without a stack trace.

**Done means**: three macros provided, file loads cleanly, no runtime side effects on load (each macro's body is gated behind invocation).

## Context

architecture.md § "Testing Approach" → "Test Patterns" (mock-pattern decisions)
design.md § "Test Strategy" → "Mock pattern for `gptel-request`"
existing fixture: `config/gptel/chat/test/test-helpers.el`
existing fixture: `config/gptel/scope/test/helpers-spec.el`
