---
name: scaffold-target-support-module
description: Create the literate config/emacs-test-targets module scaffold and loader
change: drive-isolated-emacs-via-emacsclient
status: ready
relations: []
---

## Files to modify
- config/emacs-test-targets/emacs-test-targets.org (new) — module loader
- config/emacs-test-targets/target-support.org (new) — target-side helpers (skeleton)
- config/emacs-test-targets/test/ (new dir) — Buttercup specs land here later

## Implementation steps
1. Create `config/emacs-test-targets/`. Choose `emacs-test-targets` as the module
   name (per design.md Open Questions; finalize here).
2. Create `target-support.org` with the standard literate headers:
   - `#+title: Target Support`
   - `#+property: header-args:emacs-lisp :tangle target-support.el`
   - `#+auto_tangle: y`
   - First babel block uses `:comments no` and emits the lexical-binding mode line on
     line 1: `;;; target-support.el --- Display-free observation + control helpers -*- lexical-binding: t; -*-`
     (see MEMORY.md — lexical binding must be line 1).
   - `(require 'cl-lib)` and `(require 'json)` near the top.
   - Add empty placeholder sections for `jf/frame-state` (filled by
     implement-frame-state-serializer) and the control helpers (filled by
     implement-control-helpers).
3. Create `emacs-test-targets.org` loader that, when loaded, makes the helpers
   available. It MUST define only inert functions — no `server-start`, no hooks, no
   global side effects (design.md Risks: must be safe even if loaded into live Emacs).
   It is NOT added to `jf/enabled-modules`.
4. Tangle + validate both files: `./bin/tangle-org.sh config/emacs-test-targets/target-support.org`
   and the loader.

## Design rationale
The observation + control helpers must be *present in the target* (design.md D5).
Shipping them as an ordinary literate module lets a worktree target load them as part
of config and lets a `-Q` target load the tangled `.el` on demand. Keeping the module
inert (no server/hooks) makes accidental loading into the user's live Emacs harmless
(design.md Risks).

## Verification
- `./bin/tangle-org.sh config/emacs-test-targets/target-support.org` tangles and
  passes `check-parens`.
- The generated `target-support.el` has `lexical-binding: t` on line 1.
- Loading the module in a scratch Emacs defines no server and starts no process.

## Context
- design.md § Decisions 'D5. A small target-side support module provides observation + control helpers'
- design.md § Decisions 'D8. Module layout (literate)'
- MEMORY.md § 'Lexical Binding Mode Line Position'
</content>
