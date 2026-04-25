---
name: save-hook-require-gptel-org
description: gptel-chat--save-state calls gptel-org-set-properties without ensuring gptel-org is loaded; first save in a real session aborts with void-function. One-line fix plus a behavioral test that reproduces without test-only loading.
change: gptel-chat-state-persistence
status: needs-review
relations:
  - "discovered-from:regression-sweep-and-manual-smoke"
---

## Files to modify

- `config/gptel/chat/menu.org` (and the tangled `menu.el`)
- `config/gptel/chat/test/menu/save-state-spec.el` — add a fresh-load reproduction so the green-test / red-prod gap is closed.

## Implementation steps

1. Reproduce. In a fresh `./bin/emacs-isolated.sh` session: `M-x jf/gptel-persistent-session`, pick a preset, `M-x gptel-chat-menu` → toggle a tool, apply, `C-x C-s`. Observe `(void-function gptel-org-set-properties)` from `gptel-chat--save-state` → `gptel-org-set-properties (point-min) nil`. Confirms the call site at `config/gptel/chat/menu.el:360` (org source `menu.org:390`) fires without `gptel-org` loaded.
2. Fix. In `config/gptel/chat/menu.org`, inside `gptel-chat--save-state` body, add `(require 'gptel-org)` immediately before the `(save-excursion ...)` form (and after the `(when (derived-mode-p 'gptel-chat-mode) ...)` guard). Eager require at top of module is also acceptable, but inside-the-hook keeps menu.el's load surface minimal and matches the existing `(require 'gptel nil t)` soft-load pattern at line 35 (the soft-load there is fine because the menu file's other paths don't dereference upstream symbols at module load).
3. Tangle: `./bin/tangle-org.sh config/gptel/chat/menu.org`. Verify `menu.el` reflects the new `require`.
4. Add a behavioral regression test in `save-state-spec.el` that proves the production path. The current "Integration" describe block (lines ~155 onward) explicitly loads `gptel-org` via the `(unless (fboundp 'gptel-org-set-properties) ...)` guard near line 173 — that helper is exactly what masked the bug. The new test must:
   - Run in a separate `before-each` that does NOT pre-load `gptel-org` and does NOT spy on `gptel-org-set-properties`.
   - `(when (featurep 'gptel-org) (unload-feature 'gptel-org t))` to actively evict the cached load if a prior spec loaded it.
   - Then call `gptel-chat--save-state` in a chat-mode buffer and expect it to **succeed** (not signal `void-function`). The test's success demonstrates the require-on-call path works.
   - Tag/comment the test so a future maintainer doesn't "fix" it by adding the missing pre-load.
5. Run `./bin/run-tests.sh -d config/gptel/chat/test/menu` and confirm the new spec fails before step 2's fix and passes after.
6. Re-run the manual smoke for `regression-sweep-and-manual-smoke` step 6 (gptel-menu persistence) end-to-end after the fix lands.

## Design rationale

`chat-mode` derives from `org-mode` (`config/gptel/chat/mode.el:121`), so `org` is loaded by the time the save hook fires. `gptel-org` is a sibling library of `gptel` (`runtime/straight/repos/gptel/gptel-org.el:590` defines `gptel-org-set-properties`). Upstream gptel only `(require 'gptel-org)` from three call sites in `gptel.el` (lines 631, 685, 889 — all gated on org-mode entry into the gptel feature path). Our chat-mode bypasses those gates entirely — it activates org-mode but never goes through upstream's "we're working with an org buffer in gptel" doors. So `gptel-org` never gets pulled in, and the first save crashes.

The fix is to load `gptel-org` ourselves on the path that needs it. Doing it inside the save hook (lazy) is the minimal, safe choice: every buffer that triggers the hook will end up with `gptel-org` loaded (one require, idempotent), and a chat-mode buffer that is never saved doesn't pay the load cost.

The test gap is the more important finding. Both existing test paths — unit (spies on `gptel-org-set-properties`) and integration (explicitly loads `gptel-org` first) — sidestep the production path where `gptel-org` is *not* loaded and *not* spied. A behavioral spec that asserts on the cold-load path is the only way to keep this regression from recurring. This is exactly the failure mode the architecture's "Testing Approach" section warns against (mocks scoped only to the test, not used as a substitute for testing the real boundary).

## Verification

- `./bin/run-tests.sh -d config/gptel/chat/test/menu` passes.
- New spec in `save-state-spec.el` (cold-load reproduction) is present, fails before the menu.org fix, passes after.
- Manual: fresh `./bin/emacs-isolated.sh` → create persistent session → menu toggle tool → apply → `C-x C-s` succeeds without entering the debugger; the on-disk file's drawer contains `:GPTEL_TOOLS:` with the new tool listed.
- `grep -n "(require 'gptel-org" config/gptel/chat/menu.el` shows the require at the expected location.

## Context

- design.md §Decisions 1, 3, 10 (drawer schema, save hook delegation to upstream helper).
- architecture.md §"Testing Approach" — over-mocked unit tests must be backed by behavioral tests on the real boundary.
- Reproduction stack from 2026-04-25 manual smoke:
  ```
  Debugger entered--Lisp error: (void-function gptel-org-set-properties)
    (gptel-org-set-properties (point-min) nil)
    (org-with-wide-buffer (gptel-org-set-properties (point-min) nil) ...)
    gptel-chat--save-state ()
    run-hooks (before-save-hook)
    basic-save-buffer (t)
    save-buffer (1)
  ```
- Surfaced by: `regression-sweep-and-manual-smoke` step 6, immediately after `gptel-menu` toggle + `C-x C-s`.
