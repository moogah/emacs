---
name: wire-home-into-callsites
description: Update tabs.org's workspace-new and all test fixtures to pass HOME to workspace--make; close the 76-failure cascade from add-home-slot-to-data-model
change: add-workspace-home-directory
status: done
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

## Observations

- The task body anticipated ~11 test files containing direct
  `workspace--make` calls. In reality, only **two** test files invoke
  `workspace--make` directly: `data-model-spec.el` (14 calls, all using
  the workspace name `"code"`) and `data-model-home-spec.el` (the new
  cycle-1 test, already passing HOME). All other test files
  (`tabs-spec.el`, `persistence-spec.el`, `layouts-spec.el`,
  `buffer-membership-spec.el`, `buffer-reincarnation-spec.el`,
  `anti-save-spec.el`, `save-restore-spec.el`, `revert-spec.el`,
  `home-spec.el`) construct workspaces exclusively via
  `workspace-new`, so they were fixed transitively by the single
  edit in `tabs.org`. The cascade was substantially narrower than
  the task brief's "~76 failing specs spread across ~11 fixture
  files" framing implied — most failures came from one production
  callsite multiplied by tab-creating tests.
- `workspaces-mode-spec.el` was listed in the expected file set but
  contains no calls to `workspace--make` or `workspace-new`. It was
  passing before and after with no edits required. The original
  `grep -rln` from the task body may have matched a different
  pattern, or the list was speculative.
- `data-model-home-spec.el` line 43 contains an intentional
  `(workspace--make "alpha")` (one-arg) inside an
  `:to-throw 'wrong-number-of-arguments` assertion. The verification
  grep `workspace--make[[:space:]]+"[^"]*"[[:space:]]*)` flags this
  as a remaining unfixed callsite, but it is the *intended*
  structural-enforcement test for
  `register/invariant/home-required-no-floating-workspaces`. Leaving
  this as-is; future readers of the verification grep should ignore
  matches inside `:to-throw` forms.
- The placeholder synthesis in `workspace-new` uses
  `(file-name-as-directory (expand-file-name name (expand-file-name
  "emacs-workspaces" (or (getenv "HOME") "~"))))`. This yields a
  trailing-slash absolute path like
  `/Users/<user>/emacs-workspaces/<name>/`, satisfying both the
  v3-shape validator's `file-name-absolute-p` and basename equality
  checks. No directory is created on disk — cycle-2's
  `scaffold-module` task owns that.
- Fixture HOMEs in `data-model-spec.el` use `/tmp/ws-test/code/` (a
  static, non-temp path). None of these data-model unit tests touch
  the filesystem; they exercise the in-memory plist shape only. If
  cycle-2 work adds shape validation that requires the directory to
  exist, these fixtures will need to migrate to `make-temp-file`.
  Flagged as deferred — not in this task's scope.

## Discoveries

- discovery_id: disc-wire-home-1
  class: spec-signal
  description: |
    The task body's expected file list overstated the cascade
    footprint. Of the 11 test files named, only `data-model-spec.el`
    required direct fixture edits — every other test file constructs
    workspaces via `workspace-new`, so a single production-callsite
    fix in `tabs.org` resolved them transitively. The actual
    "fixtures touched" count was 1 production org + 1 test file =
    2 files, not 12. This is useful pacing data for any future
    "wire-X-into-callsites" follow-up tasks: cascade scope is best
    estimated by grepping for the canonical constructor's *direct*
    callers, not by counting affected specs.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    No register change. This is a process observation, not an
    interface drift. The shape entry's `producers` field already
    correctly names `workspace--make` and
    `workspace--persistence-deserialize-workspace` as the only two
    canonical producers; `workspace-new` is correctly listed under
    `consumers` as a wrapper. Cycle 3's `workspace-new-default-path`
    will modify the wrapper (replace placeholder with real
    `defcustom` + scaffold call) without affecting the producer set.

- discovery_id: disc-wire-home-2
  class: invariant-gap
  description: |
    The placeholder synthesis in `workspace-new` produces a HOME
    path satisfying the `workspace-plist-v3` validator's structural
    checks (absolute path, basename = name), but it does NOT
    guarantee the directory exists on disk. Cycle 2's
    `scaffold-module` task will create the directory at
    `workspace-new` call time, but until that lands, calling
    `workspace-new` from an interactive session yields a workspace
    whose `:home` points to a non-existent path. The persistence
    deserializer will mark such workspaces `:broken t` on next
    Emacs restart per `register/invariant/broken-home-no-auto-recreate`.
    This is the intended cycle-1 → cycle-2 transition state, but
    worth flagging so the integrate phase doesn't read it as a
    defect.
  affected_register_entry: register/invariant/broken-home-no-auto-recreate
  recommendation: |
    No register change. The broken-home invariant correctly captures
    this transient state: "log and tag, never recreate". The
    placeholder synthesis is short-lived (cycle 3 replaces it with
    a real scaffold pipeline that creates the directory). If
    cycle-2's `scaffold-module` task slips beyond cycle 3, revisit
    whether the placeholder should also `make-directory` the path
    as a stop-gap — but not in this task.

- discovery_id: disc-wire-home-3
  class: vocabulary-mismatch
  description: |
    The helper function is named
    `wire-home-into-callsites--synthesize-home` — a name that
    encodes the task identifier directly. This is unusual for the
    codebase's naming conventions (helpers normally live under the
    module's `workspace--` prefix). The name was chosen
    intentionally to make cycle 3's find-and-replace mechanical:
    grep for `wire-home-into-callsites` and you find every line
    cycle 3 needs to remove or rewrite. The trade-off is a
    short-lived violation of the module's naming convention.
  affected_register_entry: register/shape/workspace-plist-v3
  recommendation: |
    No register change. This is a deliberate cycle-1 deviation
    that cycle 3's `workspace-new-default-path` task is responsible
    for cleaning up. The integrate phase should NOT flag the
    `wire-home-into-callsites--` prefix as a vocabulary leak —
    it's a load-bearing marker. Cycle 3's task body should
    explicitly call out: "delete
    `wire-home-into-callsites--synthesize-home` and its callsite;
    replace with `(workspace-scaffold name)` or equivalent
    scaffold-pipeline call."

