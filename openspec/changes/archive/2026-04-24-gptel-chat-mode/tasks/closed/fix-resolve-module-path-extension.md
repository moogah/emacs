---
name: fix-resolve-module-path-extension
description: Simplify dead-if in jf/resolve-module-path, add tests, update CLAUDE.md
change: gptel-chat-mode
status: done
relations:
  - discovered-from:scaffold-chat-subsystem
---

## Files to modify
- `init.org` (modify — `jf/resolve-module-path` definition)
- `init.el` (tangled)
- `config/core/test/` (new test file or extend existing)
- `CLAUDE.md` (modify — §Path Resolution still says "handles both
  `core/defaults` and `transient` formats")

## Implementation steps
1. In `init.org`, simplify the `(if (string-match-p "/" module-path) ...)`
   in `jf/resolve-module-path`. Both arms of the `if` collapsed to the
   same expression after the multi-segment refactor — the `if` is dead
   code. Replace with the single expression:
   `(expand-file-name (concat "config/" module-path ".el") jf/emacs-dir)`.
2. Add a docstring noting that `module-path` may contain zero or more
   `/` separators (e.g. `"transient"`, `"core/defaults"`,
   `"gptel/chat/chat"`).
3. Add a Buttercup spec under `config/core/test/` that exercises all three
   path shapes against a known fixture path (or use `expect :to-equal`
   with the expected expanded path string).
4. Update `CLAUDE.md` §Path Resolution to document multi-segment paths
   alongside the existing examples.
5. Re-tangle, run the new test, run the full suite to confirm no
   regression.

## Design rationale
The scaffold-chat-subsystem task extended `jf/resolve-module-path` to
handle multi-segment paths like `"gptel/chat/chat"`, but did so with a
dead `if` (both arms identical) and without tests, docstring updates, or
mention in any change artifact. This is a real shift in the module-system
contract that needs to be documented and tested before the change is
archived. The dead `if` misleads readers into thinking there are two
cases handled differently.

Reviewer's preferred path is (a) simplify + add tests + document, rather
than (b) reverting the contract change and renaming the chat loader to
fit the old single-slash semantics.

## Verification
- `./bin/tangle-org.sh init.org` succeeds.
- New test passes: `./bin/run-tests.sh -d config/core/test` (or scoped
  pattern).
- `grep -n "core/defaults" CLAUDE.md` shows the docs now mention
  multi-segment paths.
- Full suite: `./bin/run-tests.sh` no new failures.

## Context
- Review of scaffold-chat-subsystem (orchestrator session 2026-04-20)
  Finding #2
- init.org:70-82 / init.el:44-56
- CLAUDE.md §Path Resolution
