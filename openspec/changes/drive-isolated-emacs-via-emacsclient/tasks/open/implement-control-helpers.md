---
name: implement-control-helpers
description: Implement type-text/send-keys/run-command target-side helpers + unit specs
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "blocked-by:scaffold-target-support-module"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — add control helpers
- config/emacs-test-targets/test/unit/control-helpers-spec.el (new) — Buttercup specs

## Implementation steps
1. Write unit specs first (`control-helpers-spec.el`):
   - `jf/type-text` inserts a literal string into the current buffer exactly as given
     (no key interpretation); verify via buffer contents.
   - `jf/run-command` invokes a named interactive command via `call-interactively`;
     verify a side effect on buffer state.
   - `jf/send-keys` parses a `kbd`-notation string and executes it via
     `execute-kbd-macro`; verify resulting state in a buffer that does not require a
     graphical frame (keep the spec frame-independent; frame-dependent behavior is the
     behavioral-smoke / on-demand-frame concern).
2. Implement the three helpers in `target-support.org`:
   - `jf/type-text` (string) → `insert` literal.
   - `jf/send-keys` (kbd-string) → `(execute-kbd-macro (kbd …))`.
   - `jf/run-command` (command-name) → `(call-interactively (intern name))`.
   - Each returns a small status value usable over `emacsclient --eval`.
3. Tangle + validate; run `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
`eval-elisp` needs no helper — it is `emacsclient --eval` directly (design.md D5).
The literal/insert, kbd, and interactive-command helpers DO need code in the target,
so they live in the support module. `send-keys`/`run-command` may need a real frame
for keymap/redisplay context; per design.md D6 that is provided on demand
(emacsclient -c/-t), so the unit specs stay frame-independent and the frame path is
exercised separately.

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes the
  control-helper specs.
- Each spec `it` cites `specs/emacs-control-tools/spec.md § <Requirement>`.

## Context
- specs/emacs-control-tools/spec.md
- design.md § Decisions 'D5' and 'D6. Frames are attached on demand'
</content>
