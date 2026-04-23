---
name: menu-behavioral-test-real-infix
description: Strengthen the "Menu configuration works in chat-mode buffer" behavioral test to exercise an actual upstream menu infix against a real gptel variable, instead of using a synthetic probe symbol through gptel--set-with-scope
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:menu-send-coupled-options-scope
---

## Files to modify
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el` (replace the
  probe-symbol spec at `:243-268` with one that invokes a real upstream
  config infix path against a real gptel variable)

## Implementation steps

1. **Replace the probe-symbol spec** (`menu-send-rebind-spec.el:243-268`).
   The current spec calls `gptel--set-with-scope` against a synthesized
   `gptel-chat-menu-test--model-probe`. That proves Emacs primitives
   work in a `gptel-chat-mode` buffer but does not prove that an
   actual upstream menu infix (e.g. `gptel--infix-provider`) mutates
   the right gptel variable when invoked from a chat-mode buffer.

2. **Pick one upstream config infix** with a stable `:set-value
   gptel--set-with-scope` / `:variable gptel-<name>` shape. Candidates
   from `runtime/straight/repos/gptel/gptel-transient.el`:
   - `gptel--infix-provider` → mutates `gptel-model` (and
     `gptel-backend`)
   - `gptel--infix-max-tokens` → mutates `gptel-max-tokens`
   - `gptel--infix-temperature` → mutates `gptel-temperature`

   Pick one that exposes a clean, non-interactive path. `max-tokens`
   and `temperature` read a numeric value directly; `provider`
   requires a backend alist lookup. Prefer one of the numeric infixes
   for test stability.

3. **Write the test**:
   - In `with-temp-buffer` under `(gptel-chat-mode)`, baseline the
     chosen variable to a known default (either bind via `let` or
     assert the absence of a buffer-local binding first).
   - Invoke the infix's `:set-value` path with a sentinel value —
     either by calling `gptel--set-with-scope 'gptel-max-tokens <n> t`
     directly (the *named* variable, not a probe) OR by calling the
     infix's suffix function if it exposes one that doesn't require a
     prefix transient to be active.
   - Assert `(local-variable-p 'gptel-max-tokens)` is `t` and the
     buffer-local value equals the sentinel.
   - Assert the binding did not leak to a fresh buffer (same
     locality check the existing test uses).

4. **Keep the negative-leak assertion.** The second `with-temp-buffer`
   block at `:266-268` is correct scaffolding; carry it forward.

5. **Do not add new specs.** Replace the existing spec in place so the
   suite size stays stable. The existing spec's name
   ("mutates a buffer-local variable when a configuration path is
   invoked in a chat-mode buffer") is fine; the body needs
   tightening, not renaming.

## Design rationale

The reviewer of `menu-send-coupled-options-scope` observed that the
original task step 5 allowed either a real infix invocation or a
"direct mutation path" as an alternative — the author took the
alternative. In retrospect, the alternative waters down the specific
gap the task was closing: the scenario "configuration actions
(preset pick, model change, tool selection) mutate buffer-local
variables as upstream does" was supposed to move from transitive
verification (symbol membership in the prefix layout) to direct
behavioral verification. A probe symbol demonstrates that
`gptel--set-with-scope` is buffer-local-safe, which is already
guaranteed by Emacs; it does not demonstrate that the path upstream's
infixes actually take produces a mutation on a real gptel variable
inside a chat-mode buffer.

Using a named gptel variable (e.g. `gptel-max-tokens`) closes the
gap: if upstream ever changes its `:set-value` wiring such that a
buffer-local write no longer happens, the test fails. The probe
symbol is insensitive to that class of regression.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/menu.org` unaffected
  (implementation file not touched).
- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes;
  spec count stays at 53.
- `grep -n "gptel-chat-menu-test--model-probe" config/gptel/chat/test/menu/menu-send-rebind-spec.el`
  returns no matches (the probe symbol is gone).
- The replaced spec body references a named `gptel-` variable (grep:
  `grep -n "gptel-max-tokens\|gptel-temperature\|gptel-model" config/gptel/chat/test/menu/menu-send-rebind-spec.el`).

## Context

- Review of `menu-send-coupled-options-scope` (2026-04-23),
  Finding 1. Reviewer severity: Follow-up (non-blocking).
- `config/gptel/chat/test/menu/menu-send-rebind-spec.el:243-268` —
  current probe-symbol spec.
- `runtime/straight/repos/gptel/gptel-transient.el` — upstream infix
  definitions; source of truth for picking a representative
  config path.
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md
  §"Menu configuration works in chat-mode buffer"` — the scenario
  this test is the behavioral witness for.
