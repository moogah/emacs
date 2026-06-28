---
name: write-client-recipes-doc
description: Author the emacsclient client recipes doc (start/eval/observe/attach-frame/dispose)
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "blocked-by:implement-frame-state-serializer"
  - "blocked-by:implement-control-helpers"
---

## Files to modify
- config/emacs-test-targets/RECIPES.md (new) — agent-facing emacsclient recipes

## Implementation steps
1. Document the lifecycle as copy-pasteable `emacsclient` one-liners an agent runs via
   Bash:
   - **Start (full config, headless):** `bin/emacs-isolated.sh --daemon=<name>`
     (note: the script's arg pass-through already forwards `--daemon`).
   - **Start (bare):** `emacs -Q --daemon=<name>` then load helpers:
     `emacsclient -s <name> --eval '(load "config/emacs-test-targets/target-support.el")'`.
   - **Eval:** `emacsclient -s <name> --eval '(form)'` — runs in the loaded config.
   - **Observe:** `emacsclient -s <name> --eval '(jf/frame-state)'` → JSON.
   - **Control:** `jf/type-text` / `jf/send-keys` / `jf/run-command` via `--eval`.
   - **Attach a real frame (for keymap/redisplay tests):**
     `emacsclient -s <name> -c` (GUI) or `-t` (TTY).
   - **Enumerate targets:** `ls "$TMPDIR/emacs$(id -u)/"`.
   - **Dispose:** `emacsclient -s <name> --eval '(kill-emacs)'`.
2. Document the isolation boundary plainly: process/session/runtime isolation only,
   NOT a filesystem sandbox; local socket only; trusted clients only
   (specs/isolated-emacs-targets § Documented Isolation Boundary).
3. State the requirement clearly: control needs a named server socket
   (`server-start`); `--daemon=<name>` is the convenient default, not a hard
   requirement.

## Design rationale
For a shell-capable client, the documented recipes ARE the product — `emacsclient` is
the transport and Bash is the client (design.md D1). The doc encodes the verified
mechanism and the server-vs-daemon clarification (design.md D2) so an agent can drive
targets without rediscovering it.

## Verification
- Every recipe in the doc executes successfully against a live daemon (manual or via
  the behavioral smoke's invocations).
- The doc references real function names (`jf/frame-state`, etc.) as implemented.

## Context
- design.md § Decisions 'D1', 'D2', 'D6'
- specs/emacs-control-tools/spec.md
- specs/isolated-emacs-targets/spec.md § 'Documented Isolation Boundary'
