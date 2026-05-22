---
name: fold-config-drawer-on-open
description: The file-level config properties drawer in session.org is shown expanded when a chat session opens, adding visual clutter as it fills with snapshot and scope keys. Make gptel-chat-mode fold that drawer on activation so the file opens clean. The `* System Prompt` heading is folded separately via the VISIBILITY property emitted at creation.
change: gptel-drawer-as-source-of-truth
status: ready
relations: []
---

## Files to modify

- `config/gptel/chat/mode.org` (modify) — fold the file-level `:PROPERTIES:` drawer on `gptel-chat-mode` activation
- `config/gptel/chat/test/display/drawer-fold-spec.el` (add) — folding regression spec

## Why

design.md §Addendum Finding C (Decision C). The full-snapshot drawer now carries preset, model, backend, tools, temperature, and the `:GPTEL_SCOPE_*:` keys — a substantial block. Open `session.org` and that block is the first thing in view. Folding it on open keeps the file WYSIWYG-clean: the user sees a folded drawer plus the `* System Prompt` and `* Chat` headings, and expands the drawer only when they want to inspect configuration.

This task covers only the **file-level config drawer**. The `* System Prompt` heading folds via the `:VISIBILITY: folded` property emitted by `emit-system-prompt-and-chat-headings-at-creation` — no work here for that.

## Implementation steps

1. Investigate why the file-level drawer is not folded in `gptel-chat-mode` buffers today. `gptel-chat-mode` derives from `org-mode`; org's startup visibility (`org-set-visibility-according-to-property` / `org-cycle-set-startup-visibility`, which hides drawers) may not be reaching the pre-first-heading file-level drawer in the derived mode, or may run before content is present for programmatically created buffers.
2. Fold the file-level drawer on activation — fold the `:PROPERTIES: ... :END:` region at `point-min`. Prefer the org fold API available in this Emacs (`org-fold-hide-drawer-toggle` / `org-cycle-hide-drawers`, depending on org version — confirm against the runtime org). Do this from `gptel-chat-mode` body or `gptel-chat-mode-hook`, guarded so it is a no-op when there is no drawer (e.g. `gptel-chat-new` scratch buffers).
3. Ensure the fold happens for both entry paths: a session file opened via `find-file` (mode activates through `normal-mode`) and a buffer where `gptel-chat-mode` is invoked directly. If startup-visibility ordering is the root cause, running the fold from the mode hook after content load is the robust fix.
4. Re-tangle `config/gptel/chat/mode.org`.
5. Add a Buttercup spec under `config/gptel/chat/test/display/`: open/create a chat-mode buffer with a `:PROPERTIES:` drawer, assert the drawer region is folded (invisible) after activation, and assert turn content under `* Chat` remains visible. Add a no-drawer case asserting no error.
6. Re-run `./bin/run-tests.sh -d config/gptel/chat`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/mode.org
./bin/run-tests.sh -d config/gptel/chat
grep -n 'hide-drawer\|org-cycle-hide\|org-fold' config/gptel/chat/mode.el
```

Expect: the file-level `:PROPERTIES:` drawer is folded on open; chat content stays visible; no error on drawerless buffers.

## Context

design.md §Addendum Finding C (Decision C). `gptel-chat-mode` definition: `config/gptel/chat/mode.el` (`define-derived-mode ... org-mode`). The `* System Prompt` heading-fold is out of scope here — it is handled by the `:VISIBILITY: folded` property from `emit-system-prompt-and-chat-headings-at-creation`.

Cited register entry (cycle-7 plan): `interfaces.org#register-boundary-chat-mode-session-display` (`status: speculated`) — this task satisfies **override C** (file-level config `:PROPERTIES:` drawer folded on mode activation; drawerless scratch buffers are a no-op). The scaffolded failing contract-test at `openspec/changes/gptel-drawer-as-source-of-truth/scaffolding/boundaries/chat-mode-session-display.el` has a `describe` block for override C — make those `it` bodies pass, or revise the scaffold and explain in `## Discoveries`. Leave the override-A block (`fix-scope-drawer-value-emphasis`'s) untouched.
