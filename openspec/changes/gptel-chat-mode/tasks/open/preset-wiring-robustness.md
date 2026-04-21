---
name: preset-wiring-robustness
description: Scope preset's hack-local-variables-hook to the buffer and drop the org-entry-get dependency for drawer parsing
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:preset-wiring
---

## Files to modify
- `config/gptel/chat/menu.org` (hook registration + drawer parser)
- `config/gptel/chat/menu.el` (re-tangled)
- `config/gptel/chat/test/` (add specs for the new drawer path + the
  buffer-local hook)

## Implementation steps
1. Move `(add-hook 'hack-local-variables-hook #'...
   gptel-chat--apply-declared-preset-after-locals)` from module-level
   global registration to inside the mode body, with buffer-local
   scope:
   ```elisp
   (add-hook 'hack-local-variables-hook
             #'gptel-chat--apply-declared-preset-after-locals
             nil t)
   ```
   This ensures every-file-open in unrelated buffers does not pay the
   `derived-mode-p` hook cost.

2. Replace `org-entry-get nil "GPTEL_PRESET" 'selective` in the
   property-drawer path with a native
   `re-search-forward`-on-`^:GPTEL_PRESET:\\s-*\\(\\S-+\\)` scoped to
   the first `:PROPERTIES:` block at `point-min`. This removes the
   implicit dependency on `org-mode` being loaded and matches chat-mode
   buffers (which derive from `text-mode`, not `org-mode`) more
   cleanly.

3. Verify the ordering claim in the `apply-declared-preset-after-locals`
   docstring against Emacs 26+ `normal-mode`: the sequence is
   `hack-local-variables` → `set-auto-mode` → mode hooks, so once the
   hook is buffer-local inside the mode body, the second hook may be
   redundant for fresh file-open. Simplify if verification confirms.

4. Add specs:
   - Opening a non-chat file does not trigger `derived-mode-p` checks
     from the preset hook (spy on the function or assert the hook is
     not on the global list after module load).
   - A chat-mode buffer with a property drawer at `point-min`
     containing `:GPTEL_PRESET: coding` triggers preset application.
   - A chat-mode buffer WITHOUT org loaded succeeds without error
     (requires the native drawer parser).

## Design rationale
The `preset-wiring` review flagged two non-blocking concerns:

- **Finding 1**: the global hook imposes a per-file-open cost on every
  Emacs user who loads the chat module, even when they never open a
  chat buffer. Upstream gptel avoids this by calling
  `gptel--restore-state` from inside its mode body.
- **Finding 2**: `org-entry-get` on a non-Org buffer works, but only
  because it falls back to regex. Removing the dependency removes a
  silent coupling to `org-mode` being loaded and removes a surprise
  if org's drawer API ever changes.

Both are quality-of-life improvements, not correctness fixes. Neither
blocks downstream work.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/` passes with new
  specs.
- `grep -n "add-hook 'hack-local-variables-hook" config/gptel/chat/menu.el`
  is inside the mode body, not at module load.
- Module loads without `(require 'org)` side effects at module load
  time (verified by tracing or by a test that loads the module in a
  fresh Emacs without org preloaded).

## Context
- Review of `preset-wiring` (2026-04-21, orch-review-1776770835),
  Findings 1 and 2.
- Upstream reference: `runtime/straight/build/gptel/gptel.el:636-643`
  (`gptel--restore-state` from inside mode body).
