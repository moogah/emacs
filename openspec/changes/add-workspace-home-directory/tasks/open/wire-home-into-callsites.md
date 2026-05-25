---
name: wire-home-into-callsites
description: Update tabs.org's workspace-new and all test fixtures to pass HOME to workspace--make; close the 76-failure cascade from add-home-slot-to-data-model
change: add-workspace-home-directory
status: ready
relations:
  - blocked-by:add-home-slot-to-data-model
  - discovered-from:add-home-slot-to-data-model
---

## Why this task exists

`add-home-slot-to-data-model` changed `workspace--make`'s signature from
`(name)` to `(name home)`. Every existing callsite still passes one arg
and now fails with `wrong-number-of-arguments`. The cascade hits:

- **1 production callsite**: `config/workspaces/tabs.org` line ~145
  inside `workspace-new` — `(workspace--make name)`.
- **~11 test files** under `config/workspaces/test/` that construct
  test workspaces via `workspace--make` directly (fixtures) or
  indirectly via `workspace-new` (which routes through the production
  callsite).

Net effect on the integration branch:
`./bin/run-tests.sh -d config/workspaces` → 76 failed, 75 passed.

Cycle 1's plan deferred the full `workspace-new-default-path` task
(defcustom + scaffold pipeline + rewritten `workspace-new`) to cycle 3.
This task is the minimum cycle-1 work to keep the integration branch
green: a placeholder HOME synthesis in `workspace-new` plus mechanical
fixture updates. Cycle 3's `workspace-new-default-path` will replace
the placeholder with the proper `defcustom workspaces-default-parent-
directory` + scaffold pipeline.

## Files to modify

- `config/workspaces/tabs.org` (modify — `workspace-new` synthesizes a HOME)
- All test files under `config/workspaces/test/` that call
  `workspace--make` directly or invoke `workspace-new`. Discover with:
  ```bash
  grep -rln 'workspace--make\b\|workspace-new\b' config/workspaces/test/
  ```
  Expected list (verify by grep):
  `data-model-spec.el`, `home-spec.el`, `tabs-spec.el`,
  `persistence-spec.el`, `layouts-spec.el`, `buffer-membership-spec.el`,
  `anti-save-spec.el`, `save-restore-spec.el`,
  `buffer-reincarnation-spec.el`, `revert-spec.el`,
  `workspaces-mode-spec.el`.

  These are tangled outputs — find the corresponding `.org` source for
  each (if any) and edit there. Some test files may be hand-written
  `.el` (no `.org` source); edit those directly. Look for a top-of-file
  comment indicating tangle source, or check if a sibling `.org` exists.

## Implementation steps

### 1. In `config/workspaces/tabs.org`, update `workspace-new`

The current call is `(workspace--make name)`. Replace with:

```elisp
(workspace--make name
                 (wire-home-into-callsites--synthesize-home name))
```

…where the helper is a small local function defined in the same file
(or inline if you prefer — your call):

```elisp
;; Placeholder home synthesis. Cycle 3's workspace-new-default-path
;; task replaces this with a proper (expand-file-name NAME
;; workspaces-default-parent-directory) using the defcustom landed
;; alongside the scaffold pipeline. Until then, hardcode a sensible
;; default so workspace-new produces a valid :home and the branch
;; stays buildable.
(defun wire-home-into-callsites--synthesize-home (name)
  "Synthesize a placeholder home path for NAME.
This is a CYCLE-1 PLACEHOLDER replaced by cycle-3's
workspace-new-default-path task (which introduces the
workspaces-default-parent-directory defcustom + scaffold pipeline)."
  (expand-file-name
   name
   (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~"))))
```

You may inline the body of `wire-home-into-callsites--synthesize-home`
into `workspace-new` directly if that reads cleaner — the function
exists in this brief for clarity; the actual code can be inlined.
The important thing is the placeholder comment naming
`workspace-new-default-path` as the successor.

**Do NOT** create the directory on disk in this task. The scaffolding
pipeline is cycle 2's `scaffold-module` work. `workspace-new`'s job
in this task is just to compute a valid HOME path string and pass it
to `workspace--make`.

### 2. Update all test fixtures

For each test file in the list above, find every call to
`workspace--make NAME` and add a second argument. The simplest pattern
that satisfies the validator (basename(:home) == :name):

```elisp
;; Before:
(workspace--make "alpha")
;; After:
(workspace--make "alpha"
                 (file-name-as-directory
                  (expand-file-name "alpha"
                                    (or test-tmp-home "/tmp/ws-test/"))))
```

…where `test-tmp-home` is the per-test temp dir if the test file
already creates one (most do, via `(make-temp-file "ws-..." t)`). If
the test file doesn't create one, a hardcoded `/tmp/ws-test/<name>/`
is acceptable — the tests don't need the directory to exist on disk
(they're exercising the in-memory plist shape), and basename derivation
works on path strings regardless.

**For tests that invoke `workspace-new` rather than `workspace--make`
directly**: no fixture change needed in the test — once tabs.org's
`workspace-new` synthesizes HOME (step 1), those tests will pass
through cleanly. Spot-check one or two by running them in isolation.

**Patterns to watch for**:

- Some test fixtures may construct via `(puthash name (workspace--make
  name) workspace--registry)`. The fix is the same: add HOME as the
  second arg.
- A few specs may assert `:home` is nil on the constructed plist. If
  any such assertions exist, they're testing the OLD floating-workspace
  shape and need to be either deleted or rewritten — flag in
  `## Observations` either way.

### 3. Verify green

```bash
cd /Users/jefffarr/emacs-add-workspaces-package/.worktrees/<your-worktree>
./bin/run-tests.sh -d config/workspaces
```

Expected: ~165 specs, 0 failed. (Pre-cycle baseline 136 + 15 new from
data-model-home-spec.el + 14 new from home-org-spec.el = 165.)

If any specs fail after your fix, they're NOT cascade fallout — they
indicate either (a) a fixture pattern you missed (grep again and check),
or (b) a real interaction between the new shape and an existing test
expectation (record in `## Discoveries` with `class: spec-signal` and
let the integrate phase decide).

## Cited register entries

- `register/shape/workspace-plist-v3` (speculated, load_bearing: true)
  — your fixture changes create plists matching this shape; assertion:
  `name = basename(:home)` for every constructed test workspace.
- `register/invariant/home-required-no-floating-workspaces` (speculated,
  load_bearing: true) — this task is what makes the invariant
  practically achievable across the existing codebase; without these
  fixture updates, the constructor's required-HOME contract is true
  but unbearable.
- `register/invariant/registry-name-equals-basename` (speculated,
  load_bearing: true) — your synthesized HOME values have basenames
  matching the workspace NAME by construction.

## Notes for the implementor

- This is a follow-up task spawned mid-cycle in response to the
  `add-home-slot-to-data-model` implementor's `disc-add-home-slot-2`
  discovery (interface-drift / cascade visibility). The diff is
  mechanical; the architectural decisions were made by the parent
  task.
- The placeholder synthesis in `workspace-new` is intentional and
  short-lived. Cycle 3's `workspace-new-default-path` task replaces
  it with the proper defcustom + scaffold pipeline. Do NOT preempt
  that work here — keep your change small.
- The scaffolded vocabulary `workspace-broken-disposition.el` and
  scaffolded boundary `home-org-read-pipeline.el` are NOT in your
  scope; they belong to cycle-1 / cycle-2 work that has already
  landed (home-org) or is still ahead (data-model's cycle-1 deliverable
  is what you're fixing the fallout of).

## Verification commands

```bash
# Primary gate — must be 0 failed
./bin/run-tests.sh -d config/workspaces

# Find remaining unfixed callsites (should produce no offending matches
# once your fix is complete):
grep -rn 'workspace--make[[:space:]]\+"[^"]*"[[:space:]]*)' config/workspaces/

# Confirm placeholder comment is present so cycle 3 can find and replace:
grep -n 'wire-home-into-callsites\|CYCLE-1 PLACEHOLDER' config/workspaces/tabs.org config/workspaces/tabs.el
```
