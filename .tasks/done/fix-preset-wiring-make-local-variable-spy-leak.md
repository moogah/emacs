---
name: fix-preset-wiring-make-local-variable-spy-leak
description: Three pre-existing buttercup failures in config/gptel/chat/test/menu/preset-wiring-spec.el — the make-local-variable spy assertions caught org-mode's own internal make-local-variable calls because the spec activates org-mode inside the spy window. Fixed during fold-config-drawer-on-open; this note records the finding for the owning maintainer.
status: done
source: openspec/changes/gptel-drawer-as-source-of-truth
relations:
  - "discovered-from:fold-config-drawer-on-open"
---

## Resolution

Fixed in commit for task `fold-config-drawer-on-open` as the minimum
needed to bring `./bin/run-tests.sh -d config/gptel/chat` to
`0 failed`. The three `it` blocks now assert `(local-variable-p KEY)`
is nil for each overlay-candidate key instead of spying the
org-internal `make-local-variable` primitive. This note is retained
for the `preset-wiring` / `chat-drawer-overrides-overlay` maintainer
in case the instrument choice should be reconsidered elsewhere in
that file.

## Problem

`./bin/run-tests.sh -d config/gptel/chat` reports 3 failing buttercup
specs in `config/gptel/chat/test/menu/preset-wiring-spec.el`:

- `drawer overrides overlay > gptel-chat--apply-drawer-overrides (unit) >
  is a no-op for each absent field`
- `drawer overrides overlay > apply-declared-preset invokes the overlay >
  does not overlay anything when the buffer has no drawer`
- `drawer overrides overlay > apply-declared-preset invokes the overlay >
  does not overlay absent upstream keys even when preset applies`

All three fail with `Expected 'make-local-variable :not :to-have-been-called`.

These are **pre-existing** — they fail deterministically on the clean
merge-base commit `63192f4` (`./bin/run-tests.sh -d config/gptel/chat` →
`Ran 382 specs, 3 failed`, repeated 3×). They were surfaced (not
introduced) during the `fold-config-drawer-on-open` task.

## Root cause

Each of the three `it` blocks does, in order:

1. `(spy-on 'make-local-variable :and-call-through)`
2. `(with-temp-buffer (org-mode) ...)` — activates `org-mode` **inside
   the spy window**
3. calls `gptel-chat--apply-drawer-overrides`, which (when the upstream
   tuple is all-nil) does an `org-entry-get` for `GPTEL_PARENT_SESSION_ID`
4. `(expect 'make-local-variable :not :to-have-been-called)`

`org-mode` activation, plus the lazy `org-element` cache initialization
triggered by step 3's `org-entry-get`, calls `make-local-variable` ~83
times (`delay-mode-hooks`, `tab-width`, `outline-regexp`,
`org-element--cache-*`, `org-file-tags`, …). None of those come from
`gptel-chat--apply-drawer-overrides`. The spec's intent — "the overlay
never installs a buffer-local for an absent key" — is correct, but the
assertion instrument (`make-local-variable` global primitive) is too
broad: it is defeated by the spec's own `(org-mode)` call.

Confirmed empirically (org 9.8-pre, Emacs 30.1): a buttercup `it`
that spies `make-local-variable :and-call-through`, runs `(org-mode)`
in a `with-temp-buffer`, then calls `gptel-chat--apply-drawer-overrides`
records 83 calls, all org-internal.

A reported "382 specs, 0 failed" clean baseline is **not reproducible**
in this runtime — likely captured under an older org version whose
org-element cache initialized without per-buffer `make-local-variable`,
or against a stale runtime.

## Fix options

Either:

- Move `org-mode` activation (and a warm-up `org-entry-get` / a no-op
  org call that forces org-element-cache init) **before** the
  `(spy-on 'make-local-variable ...)` line, so the spy window contains
  only the overlay call; or
- Replace the broad `make-local-variable` spy with a narrower
  instrument — spy the actual buffer-local setter path the overlay
  uses (`set` against a `make-local-variable` form is hard to isolate;
  better: assert `(local-variable-p 'gptel-...)` is nil for each key
  the overlay should not have touched, which the sibling passing specs
  already do for the positive case).

The second option is more robust against future org-internal changes.

## Files to modify

- `config/gptel/chat/test/menu/preset-wiring-spec.el` — the three
  `it` blocks at (approx.) lines 505, 724, 738.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/chat/test/menu
./bin/run-tests.sh -d config/gptel/chat
```

Expect: `0 failed` for `config/gptel/chat` once this and any other
genuine fixes land.

## Context

`preset-wiring-spec.el`'s `make-local-variable` spy assertions were
added in commit `fa670df` ("Implement task chat-drawer-overrides-
overlay"). This file is owned by the `chat-drawer-overrides-overlay` /
`preset-wiring` work, not by `fold-config-drawer-on-open`.
