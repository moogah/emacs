---
name: gptel-preset-mode-defaults-no-consumer
description: jf/gptel-preset--mode-defaults is populated on every preset registration but no code reads it; either wire the session subsystem consumer or drop the storage and keep only the :mode strip.
source: openspec/changes/gptel-fragment-presets (registration-rewrite task, cycle-1781885402)
status: externalised
discovered_from: registration-rewrite
discovered_by: architect
discovered_class: dead-branch
relations:
  - "discovered-from:registration-rewrite"
---

## Finding

`jf/gptel-preset--mode-defaults` (a defvar alist of preset-name symbol → `:mode`
string, default `"org-mode"`) is populated on every registration by
`jf/gptel-preset--extract-mode` (`config/gptel/presets/registration.el:663,716`),
and the module prose claims it is read "by the session subsystem when determining
file format (org-mode vs markdown-mode)." A grep across `config/gptel` (excluding the
module and its tests) finds **no reader**. Only the sibling
`jf/gptel-preset--scope-defaults` has a live consumer (`scope-profiles.el:96`).

Corroborated by two independent signals this cycle:
- registration-rewrite author-blind review, finding 1 (advisory, correctness).
- End-of-cycle Architect audit, arch-cycle-1781885402-4 (advisory, dead-branch).

## Why this is external (not in-change)

Verified consumer-less at base commit `842b3661` as well — this is a **pre-existing**
write-only table that registration-rewrite faithfully carried forward, not a defect it
introduced. The `:mode` key **strip** from the `gptel-make-preset` plist is live and
correct; only the side-table *storage* has no reader. Resolving it means touching the
**session subsystem** (file-format selection), which is outside the
`gptel-fragment-presets` change scope.

## Options

1. **Wire the consumer** — have the session subsystem read
   `jf/gptel-preset--mode-defaults` when choosing `org-mode` vs `markdown-mode` for a
   session's buffer/file, matching the documented intent.
2. **Drop the storage** — remove the `--mode-defaults` defvar + `--extract-mode`'s
   write, keep only the `:mode`-strip from the registration plist, and soften the
   org prose to stop claiming a consumer.

## Verification

- `grep -rn "gptel-preset--mode-defaults" config/gptel` — after fix, every write has a
  matching read (option 1) OR the symbol is gone (option 2).
- `./bin/run-tests.sh -d config/gptel/presets/test` and the session-subsystem suite green.

## Context pointers

- `config/gptel/presets/registration.org` (the side table + extract-mode).
- `config/gptel/presets/registration.org:74-88` (the prose overstating wiring).
- `.orchestrator/cycles/cycle-1781885402/findings/arch-cycle-1781885402-4.md`
