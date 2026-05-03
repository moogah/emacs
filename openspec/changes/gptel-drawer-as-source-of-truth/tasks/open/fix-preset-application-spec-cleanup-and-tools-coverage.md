---
name: fix-preset-application-spec-cleanup-and-tools-coverage
description: The three new describe blocks in `preset-application-spec.el` push onto a global registry-keys list inside their `it` bodies but their `after-each` hooks never drain it, leaking entries into `jf/gptel--session-registry` for the test process lifetime. Separately, the fresh-session scenario omits the `:GPTEL_TOOLS:` registration and assertion that the brief explicitly required — the most error-prone snapshot key (list, not scalar) goes uncovered.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:update-preset-application-spec-for-snapshot
---

## Files to modify

- `config/gptel/sessions/test/commands/preset-application-spec.el` — add registry drain to the three new describe blocks (or one outer-scope drain); add `:tools` registration + assertion in the fresh-session scenario

## Why

**Finding 1 (advisory).** The cleanup function `jf-gptel-preset-app-test--register-cleanup` pushes keys onto the global `jf-gptel-preset-app-test--registry-keys` defvar. The drain (`remhash` each key from `jf/gptel--session-registry`, then reset the list) lives in the `after-each` at line 88–91 of the original `"Drawer-driven auto-init …"` top-level describe. The three new describe blocks (around lines 343, 390, 437) each call the registration helper but their own `after-each` hooks do not include the drain. In separate-process CI runs this is invisible, but a single-process run of the sessions suite leaves stale registry entries that can produce false hits in registry-lookup assertions downstream.

**Finding 2 (advisory).** The fresh-session scenario (lines 339–361) registers the preset with only `:model` and `:temperature` and asserts only `:GPTEL_MODEL:` and `:GPTEL_TEMPERATURE:`. The brief required `:tools` in both the registration and the assertion; the register entry `drawer-text-block` lists `:GPTEL_TOOLS:` as one of the six snapshot keys. Tools is the most likely key to mis-handle (list vs. scalar serialization) and it is currently unexercised at the integration level.

## Implementation steps

1. In `config/gptel/sessions/test/commands/preset-application-spec.el`, add the registry drain to the three new describe blocks. Two viable shapes:
   - **Per-describe:** add `(after-each (dolist (key jf-gptel-preset-app-test--registry-keys) (remhash key jf/gptel--session-registry)) (setq jf-gptel-preset-app-test--registry-keys nil))` to each.
   - **Outer drain:** wrap the three describes in a single outer `describe "gptel-drawer-as-source-of-truth: full snapshot end-to-end"` with one shared `after-each` doing the drain.
2. In the fresh-session scenario (around lines 339 / 349), add `:tools '(toolA toolB)` to the `gptel-make-preset` call and `(expect content :to-match ":GPTEL_TOOLS: ")` to the assertion block. A presence-match is sufficient — exact list formatting is implementation detail and the scope-profile-applicator dedupe task (separate) will cover it more rigorously.
3. Re-run `./bin/run-tests.sh -d config/gptel/sessions` and confirm green.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/sessions
grep -nE "remhash|jf-gptel-preset-app-test--registry-keys" config/gptel/sessions/test/commands/preset-application-spec.el
grep -n "GPTEL_TOOLS" config/gptel/sessions/test/commands/preset-application-spec.el
```

Expect: drain calls present in every after-each that touches the helper; tools assertion present.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/update-preset-application-spec-for-snapshot.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-invariant-drawer-system-key-write-exclusion`.
