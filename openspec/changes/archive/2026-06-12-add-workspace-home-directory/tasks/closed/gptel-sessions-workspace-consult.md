---
name: gptel-sessions-workspace-consult
description: Add workspace-sessions-dir function; modify gptel sessions to consult via featurep; add prefix-arg force-global escape hatch
change: add-workspace-home-directory
status: done
relations:
  - blocked-by:add-home-slot-to-data-model
---

## Files to modify
- `config/workspaces/workspaces.org` (modify — export `workspace-sessions-dir` function)
- `config/gptel/sessions/*.org` (modify — the module containing the session-dir resolution / new-session entry point; locate during apply)
- `config/workspaces/test/gptel-integration-spec.el` (new — Buttercup; tests the workspaces side of the contract)
- `config/gptel/sessions/test/workspace-routing-spec.el` (new — Buttercup; tests the gptel side)

## Implementation steps

1. **Locate the gptel session-dir resolution.** Search
   `config/gptel/sessions/` for the function that computes where a new
   session file goes. Likely candidates: a `gptel-sessions-directory`
   defcustom referenced inside a `gptel-sessions-new` or
   `gptel-sessions-create` function. Identify:
   - The variable / function that resolves the target directory.
   - The user-facing command that creates a new session.

   Document the actual function names you find inline at the top of
   this task as you work — design.md flags this as Open Question Q1.

2. **In `config/workspaces/workspaces.org`**, add the exported
   workspace-consult function:

   ```elisp
   (defun workspace-sessions-dir ()
     "Return the sessions/ directory of the current workspace, or nil.
   Returns nil when no workspace is the current tab's owner, or when
   the workspace's :home is missing/broken."
     (when-let* ((name (workspace--current-name))
                 (ws   (gethash name workspace--registry))
                 ((not (workspace--broken-p ws)))
                 (home (workspace--home ws)))
       (let ((dir (expand-file-name "sessions" home)))
         (when (file-directory-p dir)
           dir))))
   ```

   Notes:
   - Returns nil rather than signalling — gptel needs a value-or-nil
     to fall through cleanly.
   - Broken-state check: do not route into a broken workspace.
   - `file-directory-p` check guards against the rare case where
     `sessions/` was deleted by the user after creation.

3. **In the located gptel sessions module**, modify the session-dir
   resolver to consult the workspace function. Pattern:

   ```elisp
   (defun gptel-sessions--target-dir (&optional force-global)
     "Return the directory where a new session file should be created.
   When a workspace is active and FORCE-GLOBAL is nil, returns the
   workspace's sessions/ dir. Otherwise returns the global default."
     (or (and (not force-global)
              (featurep 'workspaces)
              (fboundp 'workspace-sessions-dir)
              (workspace-sessions-dir))
         gptel-sessions-default-directory))   ;; or whatever the existing var is
   ```

   (Replace `gptel-sessions-default-directory` with the actual
   variable name discovered in step 1.)

4. **Add the prefix-arg escape hatch** to the user-facing
   session-creation command:

   ```elisp
   (defun gptel-sessions-new (&optional force-global)
     "Create a new gptel session file.
   With prefix arg FORCE-GLOBAL, write to the global session directory
   even when a workspace is active."
     (interactive "P")
     (let ((dir (gptel-sessions--target-dir force-global)))
       ;; ... existing creation logic, now using DIR
       ))
   ```

   Adapt to the actual existing command shape. The two changes:
   `interactive "P"` for the prefix arg, and routing through
   `gptel-sessions--target-dir`.

5. **Create `config/workspaces/test/gptel-integration-spec.el`** —
   tests for the workspaces side (no dependency on gptel being loaded):

   - `workspace-sessions-dir` returns nil when not on a workspace tab
     (stub `workspace--current-name` to return nil).
   - Returns nil when on a broken workspace.
   - Returns `<:home>/sessions/` for a healthy workspace whose
     sessions dir exists.
   - Returns nil when the workspace exists but `sessions/` was
     deleted (`file-directory-p` false).

6. **Create `config/gptel/sessions/test/workspace-routing-spec.el`** —
   tests the gptel side of the contract:

   - With `workspaces` loaded and a current workspace: new session
     file lands under `<:home>/sessions/`. Use a real tmp workspace
     created via `workspace-new` or build the registry/tab manually.
   - With prefix arg (`force-global=t`): new session file lands in
     `gptel-sessions-default-directory` even when on a workspace.
   - With `workspaces` NOT loaded (`unfeaturep`): new session file
     lands in the global default. Simulate by `cl-letf` on `featurep`
     to return nil for `'workspaces`.
   - On a tab not owned by a workspace: new session file lands in
     global default.

7. Tangle and test both sides:
   ```bash
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/tangle-org.sh config/gptel/sessions/<changed-file>.org
   ./bin/run-tests.sh -d config/workspaces -p gptel-integration-spec
   ./bin/run-tests.sh -d config/gptel/sessions -p workspace-routing-spec
   ```

## Design rationale

Soft dependency via `(featurep 'workspaces)` keeps the gptel sessions
module loadable in configurations without workspaces. The dependency
direction is one-way: gptel optionally consults workspaces; workspaces
never references gptel. This matches the project's general
loose-coupling pattern for cross-subsystem features.

Returning nil (rather than signalling) from `workspace-sessions-dir`
lets gptel use a clean `(or workspace-dir global-default)` fallback.
The function does its own validity checks (broken, missing sessions/)
so callers don't have to.

The prefix arg as the escape hatch matches Emacs convention — users
already understand `C-u M-x command` as "do the thing differently."
Naming it `force-global` in the function signature makes the call site
self-documenting.

## Design pattern

Cross-module integration via published function: workspaces exports
`workspace-sessions-dir` as a stable contract; gptel calls it. This
keeps the per-module interfaces narrow and visible.

`fboundp` check in addition to `featurep`: defensive for the case where
the feature is provided but the specific function was renamed in a
future workspaces version. Cheap and prevents nondescript "void
function" errors.

## Verification

- Tangle: both `.org` files.
- Specs: both new spec files pass.
- Full test runs: `./bin/run-tests.sh -d config/workspaces` and
  `./bin/run-tests.sh -d config/gptel/sessions` — no regressions.
- Manual:
  - On a workspace tab: `M-x gptel-sessions-new` → assert the new file
    lives under `<home>/sessions/`.
  - `C-u M-x gptel-sessions-new` → assert it lives in the global dir.
  - On a non-workspace tab (e.g., a tab created via `M-x tab-bar-new-tab`
    that is not registered as a workspace): `M-x gptel-sessions-new`
    → assert global dir.

## Context

design.md § Decisions / D3 — gptel integration: soft dependency via featurep + named function
design.md § Open Questions Q1 — locate exact gptel session-dir resolution function during apply
specs/workspaces/spec.md § ADDED "Workspace-aware gptel session creation"
specs/workspaces/spec.md § ADDED "Filesystem-authoritative session inventory"

## Cycle 2 plan stanza (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. The single blocker
  (`add-home-slot-to-data-model`) closed in cycle 1 at merge
  commit `7026d37`.

### Cited register entries

- `register/boundary/gptel-sessions-workspace-consult`: **speculated**
  (net-new this plan). The cross-subsystem boundary pinning three
  concerns: direction (gptel → workspaces; never reverse), softness
  (`featurep` + `fboundp` guards), and nil-discipline (the producer
  returns dir-or-nil and never signals). Your steps 2 and 3 ARE the
  canonical producer and consumer functions. Scaffold at
  `scaffolding/boundaries/gptel-sessions-workspace-consult.el` has
  10 `it` cases across three describe blocks (producer-side
  contract, consumer-side fallback chain, directionality lint).
  Lift the assertion shapes into the two new spec files this task
  produces; the directionality lint case is the one that catches a
  future refactor breaking the one-way contract — keep that one
  whether or not you migrate the other shapes.

- `register/shape/workspace-plist-v3`: **confirmed** (cycle 1). The
  producer (`workspace-sessions-dir`) reads `:home` and the broken
  tag off the workspace plist. The shape is cited for the
  `workspace--broken-p ws` and `workspace--home ws` accessor calls
  in step 2's code.

- `register/vocabulary/workspace-broken-disposition`: **confirmed**
  (cycle 1). The producer-side nil-on-broken behaviour you implement
  in step 2 IS the vocabulary's `:refused` disposition extended to
  the gptel routing concern. A broken workspace MUST NOT route
  session creation into a missing path — falling through to the
  global default is the correct behaviour.

- `register/invariant/registry-name-equals-basename`: **confirmed**
  (cycle 1). Cited as supporting context: the producer's
  `(workspace--current-name)` look-up assumes the current-tab
  name matches a registry key, which holds because of this
  invariant. If the invariant ever divergent's, the producer's
  nil-discipline still keeps the consumer safe (no broken-tag
  needed to fall through).

### Open Question Q1 resolution path

Design.md Q1 flags that the exact gptel function name being
modified is forward-pinned. Your step 1's "Locate the gptel
session-dir resolution" is the resolution step. The exact answer
lands in your `## Discoveries` (or appended to step 1 inline) and
the boundary entry's `consumer_side.file` field is updated at
cycle-2 integrate.

Suggested grep starting points (recorded in the register entry's
`consumer_side.note`):
```bash
grep -rn "session.*directory\\|sessions-dir\\|sessions-default" config/gptel/sessions/
grep -rn "defcustom" config/gptel/sessions/ | grep -iE "session.*dir|dir.*session"
```

### Cross-task interaction with scaffold-module

Both tasks land cycle 2 in parallel. The interaction surface is
the optional `gptel-sessions-create-empty-file` (or equivalent)
function — `scaffold-module`'s `workspace--scaffold-initial-session`
prefers calling it if `(featurep 'gptel-sessions)` and the function
is bound. If this task introduces such a function, scaffold's
preferred branch lights up automatically; if not, scaffold's
fallback writes a minimal `#+TITLE:` file. The two tasks do NOT
need to coordinate on naming — each can ship independently. If you
notice during apply that the gptel sessions module already
exports a public-shaped session-create function, name and
signature it in `## Discoveries` so scaffold-module's
implementor (or its reviewer) can wire it.

### Directionality enforcement

The directionality lint (scaffold's third `describe` block) is the
strictest part of the contract. After your changes:
```bash
grep -rn "gptel-sessions-\\|gptel-sessions/\\|require.*gptel-sessions" config/workspaces/
```
MUST return zero matches. If your implementation requires referencing
gptel from inside workspaces (e.g. for a hook), the contract has
divergent — flag it in `## Discoveries` rather than shipping a
silent contract violation.

## Observations

- **Prefix-arg conflict, resolved by adding a separate command.** The
  task body step 4 sketched `(interactive "P")` on
  `jf/gptel-persistent-session` with the prefix arg meaning
  "force-global". The existing command, however, already consumes
  `current-prefix-arg` for "prompt to select preset" — a UX that
  predates this work. Rebinding the prefix arg would silently break
  the preset-prompt flow. I added a second public command
  `jf/gptel-persistent-session-global` that wraps the existing one
  with `force-global=t`. The spec scenario "the user invokes the
  gptel session-creation command with a prefix arg → session is
  created in the global directory" is satisfied at the contract
  level (`(gptel-sessions--target-dir t)` returns the global), with
  the user-facing surface being `M-x jf/gptel-persistent-session-global`
  instead of `C-u M-x jf/gptel-persistent-session`. The task body's
  step 7 explicitly says "Adapt to the actual existing command
  shape" — this is that adaptation. The Verification section's
  "Manual: `C-u M-x gptel-sessions-new` → global dir" bullet
  becomes "Manual: `M-x jf/gptel-persistent-session-global` → global
  dir".
- **Insertion-point split.** I deliberately did NOT modify
  `jf/gptel--ensure-sessions-root`; it remains the *global* root
  resolver used by registry init, session-listing, and discovery
  code paths (all of which must scan every session regardless of
  the workspace it was created under). The new resolver
  `jf/gptel--target-sessions-root` only governs new-session
  creation. Splitting the two preserves the invariant that the
  existing inventory paths are unaffected by the introduction of
  workspaces. This is documented in the new section
  "Sessions Root Resolution" in `filesystem.org`.
- **Naming deviation from scaffold contract.** The scaffold names
  the resolver `gptel-sessions--target-dir`; I used
  `jf/gptel--target-sessions-root` to match the project's
  `jf/gptel-` prefix convention (every other gptel function in
  `config/gptel/sessions/` uses that prefix). The boundary register
  entry's `consumer_side.function` field should be updated at
  integrate to the actual function name. Docstring cross-references
  the scaffold's canonical name.
- **Test isolation pattern.** The consumer-side specs use
  `cl-letf` on `featurep` itself (capturing the original closure
  via `(let ((orig (symbol-function 'featurep))) ...)`) rather than
  loading the real workspaces package into the test image — the
  test process must not have `workspaces` as a feature, otherwise
  the "not loaded" case can't be exercised. This is a deliberate
  choice that mirrors the buffer-membership specs' bufferlo-stub
  pattern (don't require the cross-module dependency just to test
  the soft-dep guard).
- **Test load chain.** `gptel-integration-spec.el` loads
  `data-model.el` + `tabs.el` + `workspaces.el` in order.
  `workspaces.el` runs the submodule loader via `jf/load-module`,
  which re-loads data-model and tabs idempotently; the test then
  has access to the production `workspace-sessions-dir` function.
  No special test-time provide-stubs were needed beyond what
  existing workspaces specs already establish.

## Discoveries

- **class: deviation** — task-body step 4 sketched a prefix-arg
  rebind on `jf/gptel-persistent-session`; the existing command
  already uses `current-prefix-arg` for preset selection. Resolved
  by adding a separate public command
  `jf/gptel-persistent-session-global` that calls
  `jf/gptel-persistent-session` with `force-global=t`. The
  contract (`gptel-sessions--target-dir` accepts force-global and
  routes accordingly) is preserved. The user-visible binding for
  the escape hatch becomes `M-x jf/gptel-persistent-session-global`
  rather than `C-u M-x jf/gptel-persistent-session`.

- **class: interface-drift** — the scaffold contract pins the
  consumer-side function as `gptel-sessions--target-dir`; project
  naming convention (every other gptel-sessions function uses
  `jf/gptel-` prefix) led me to name it
  `jf/gptel--target-sessions-root` instead. Boundary register entry
  `register/boundary/gptel-sessions-workspace-consult` should be
  updated at cycle-2 integrate so its `consumer_side.function`
  field reflects the actual name. The docstring of the function
  cross-references the scaffold's canonical name. The producer
  side (`workspace-sessions-dir`) matches the scaffold name
  verbatim.

- **class: responsibility-leakage (avoided)** — I considered
  modifying `jf/gptel--ensure-sessions-root` to consult workspaces
  directly, which would have been the smallest diff. Doing so
  would have routed *every* call site through workspaces —
  including registry-init, session-listing, and discovery scans
  that need the global root to find every session regardless of
  the workspace it was created under. Split the responsibility:
  `jf/gptel--ensure-sessions-root` stays global-only;
  `jf/gptel--target-sessions-root` is the new workspace-aware
  resolver used only at new-session creation. This is documented
  in `filesystem.org` § "Sessions Root Resolution".

- **class: spec-signal** — sibling task `scaffold-module` will
  look for `(fboundp 'gptel-sessions-create-empty-file)` to
  decide whether to populate the initial session via gptel or
  fall back to a minimal `#+TITLE:` stub. **I did NOT introduce
  such a function** in this task. The existing public-shaped
  session-create command is
  `jf/gptel-persistent-session (session-name &optional backend
  model preset-name force-global)` — but it is interactive,
  prompts for project selection, and opens a buffer; it is not
  suitable as a non-interactive "create-empty-file" primitive
  callable from a scaffold pipeline. If scaffold-module wants to
  reach into gptel, the lowest-cost addition would be a new
  non-interactive helper like `jf/gptel-create-empty-session-file
  (dir basename)` that writes a minimal session.org skeleton; it
  could live in `commands.org` § "Core Session Creation" right
  next to `jf/gptel--create-session-core`. That is out of scope
  for *this* task — flagging here so the scaffold reviewer can
  pick it up if they want the preferred branch lit up.
