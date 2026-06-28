---
name: behavioral-smoke-real-daemon
description: Guarded real-daemon Buttercup smoke — start, eval, frame-state, dispose
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "blocked-by:implement-frame-state-serializer"
  - "blocked-by:implement-control-helpers"
---

## Files to modify
- config/emacs-test-targets/test/behavioral/target-lifecycle-spec.el (new)

## Implementation steps
1. Write a Buttercup spec that, when a suitable `emacs` + `emacsclient` are available:
   - `before-each`/`it`: start ONE real bare target: `emacs -Q --daemon=<unique-name>`
     via `make-process`/`call-process`, then load the tangled `target-support.el` into
     it: `emacsclient -s <name> --eval '(load "…/target-support.el")'`.
   - eval round-trip: `emacsclient -s <name> --eval '(+ 1 2)'` → `3`.
   - observation: `emacsclient -s <name> --eval '(jf/frame-state)'` → JSON the spec
     parses and asserts on (current buffer present, etc.).
   - dispose: `emacsclient -s <name> --eval '(kill-emacs)'`; assert the socket is gone
     and a follow-up call fails.
2. Guard the suite: detect `emacsclient` and a daemon-capable `emacs`; if absent,
   `xit`/skip with a logged message so constrained CI still passes (design.md D7).
3. Generate a unique socket name per run (no `Date.now` analog needed; use the spec's
   process id or an incrementing counter) to avoid collisions.
4. Run: `./bin/run-tests.sh -d config/emacs-test-targets/test/behavioral -f buttercup`.

## Design rationale
Unit specs mock nothing structural but run in-process; the behavioral smoke proves the
real integration that the recipes depend on — that a daemon started, eval'd, observed,
and disposed end-to-end behaves as the specs claim (design.md D7). Crash containment
and isolation (specs/isolated-emacs-targets) are inherent to separate OS processes, so
the smoke focuses on the happy path plus dead-target detection. Kept minimal and
guarded because real daemons are slower/flakier.

## Verification
- With a daemon-capable Emacs available, the smoke passes start→eval→frame-state→dispose.
- With none available, the suite skips cleanly and the overall run is green.
- Asserts cite `specs/isolated-emacs-targets/spec.md` and
  `specs/frame-state-observation/spec.md`.

## Context
- design.md § Decisions 'D7. Testing approach'
- specs/isolated-emacs-targets/spec.md
