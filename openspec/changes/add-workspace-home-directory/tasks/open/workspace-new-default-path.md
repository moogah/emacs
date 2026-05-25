---
name: workspace-new-default-path
description: Wire scaffold into workspace-new (no prefix arg); add workspaces-default-parent-directory defcustom; rewrite default workspace-home-builder to find-file home.org
change: add-workspace-home-directory
status: ready
relations: []
---

## Files to modify
- `config/workspaces/workspaces.org` (modify — add defcustom, update default home builder)
- `config/workspaces/tabs.org` (modify — rewrite `workspace-new` default branch, update `workspace-default-home-builder`)
- `config/workspaces/test/workspace-new-default-spec.el` (new — Buttercup)

## Implementation steps

1. In `config/workspaces/workspaces.org`, add a new defcustom near the
   existing `workspace-home-builder` defcustom:

   ```elisp
   (defcustom workspaces-default-parent-directory
     (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~"))
     "Default parent directory under which `workspace-new' creates new workspaces.
   When `workspace-new NAME' is invoked without a prefix arg, the new
   workspace's home directory is `(expand-file-name NAME this-dir)'."
     :type 'directory
     :group 'workspaces)
   ```

2. In the same file, update the default value's docstring for
   `workspace-home-builder` to reflect the new default behavior
   (find-file home.org), but DO NOT change the default symbol — keep
   pointing at `workspace-default-home-builder` (the implementation
   change lives in `tabs.org`).

3. In `config/workspaces/tabs.org`, rewrite
   `workspace-default-home-builder` (currently opens `*scratch*`) to
   open `home.org` from the current workspace's `:home`:

   ```elisp
   (defun workspace-default-home-builder (workspace-name)
     "Default `workspace-home-builder': `find-file' the workspace's home.org.
   Looks up the workspace by WORKSPACE-NAME and opens
   `<:home>/home.org' in a single window. If the workspace has no
   `:home' (defensive — should never happen now that :home is
   required), falls back to *scratch*."
     (delete-other-windows)
     (let* ((ws (gethash workspace-name workspace--registry))
            (home (and ws (workspace--home ws))))
       (if home
           (find-file (expand-file-name "home.org" home))
         (switch-to-buffer (get-buffer-create "*scratch*")))))
   ```

   The fallback to `*scratch*` is purely defensive; with the
   data-model changes in task `add-home-slot-to-data-model`, every
   registered workspace has `:home`. If you discover any code path
   that could produce a no-`:home` workspace, that is a separate bug
   to fix.

4. Rewrite `workspace-new` in `tabs.org`. The new shape:

   ```elisp
   (defun workspace-new (name &optional anchor-existing)
     "Create a new workspace named NAME.

   With no prefix argument (ANCHOR-EXISTING is nil), scaffold a fresh
   directory at `(expand-file-name NAME workspaces-default-parent-directory)'.
   Signal `user-error' if that directory already exists.

   With a prefix argument (ANCHOR-EXISTING is non-nil), prompt for an
   existing directory to anchor — see Requirement: Anchoring an
   existing directory via prefix arg. (That branch is implemented in
   task `workspace-new-anchor-existing'; for this task, only the
   default-path scaffold flow is wired.)"
     (interactive "sWorkspace name: \nP")
     (if anchor-existing
         (workspace--new-anchor-existing)   ;; defined by sibling task
       (workspace--new-default-path name)))

   (defun workspace--new-default-path (name)
     "Default-path branch of `workspace-new'. See its docstring."
     (let* ((home (expand-file-name
                   name
                   workspaces-default-parent-directory)))
       (when (file-exists-p home)
         (user-error
          "Cannot scaffold %s: %s already exists. Use `C-u %s' to anchor an existing dir."
          name home (key-description (where-is-internal 'workspace-new nil t))))
       ;; Scaffold first; only register if scaffold succeeded.
       (workspace-scaffold home name :init-and-commit? t)
       (tab-bar-new-tab)
       (tab-bar-rename-tab name)
       (puthash name (workspace--make name home) workspace--registry)
       (when (and (boundp 'workspace-home-builder)
                  (functionp workspace-home-builder))
         (funcall workspace-home-builder name))
       (gethash name workspace--registry)))
   ```

   Notes:
   - The "create tab then register" order matters: scaffold is the
     fail-prone step; do it first; if it succeeds, register and create
     tab; the home builder runs last.
   - The `where-is-internal` lookup in the error message shows the
     user the actual key binding for the prefix-arg form. If this
     proves brittle, hard-code `C-u C-x w n`.

5. Create `test/workspace-new-default-spec.el`:

   - Stub `workspaces-default-parent-directory` to a tmp dir via
     `cl-letf` (or just `let`-bind it — it's a defcustom variable).
     Stub the tab-bar functions (`tab-bar-new-tab`,
     `tab-bar-rename-tab`) to spies that record calls.
   - **Happy path**: `(workspace-new "foo")`; assert
     `~/<tmp>/foo/` exists; assert `tab-bar-new-tab` was called
     once; assert `workspace--registry` contains `foo` with `:home`
     set to the tmp path.
   - **Collision**: pre-create the target dir; assert `user-error`
     is signalled; assert no tab was created; assert registry is
     unchanged.
   - **Scaffold failure leaves registry empty**: stub
     `workspace-scaffold` to signal `user-error`; assert
     `workspace-new` propagates the error; assert no tab was created;
     assert registry has no `foo` entry.
   - **Default home builder opens home.org**: with the workspace
     created, assert the buffer for `<home>/home.org` is current.
     (May need to defer this assertion or do it post-builder.)

6. Tangle and test:
   ```bash
   ./bin/tangle-org.sh config/workspaces/workspaces.org
   ./bin/tangle-org.sh config/workspaces/tabs.org
   ./bin/run-tests.sh -d config/workspaces -p workspace-new-default-spec
   ```

## Design rationale

The scaffold-then-register order is deliberate: scaffold is the
fail-prone step (git, filesystem). If it fails, we don't want a tab
or registry entry pointing at half-formed state. The previous v2
flow registered first and ran the home builder second; the new flow
inserts scaffold as a pre-step.

`workspaces-default-parent-directory` as a defcustom (rather than a
hardcoded constant) lets users override without forking the function.
Default value uses `getenv "HOME"` rather than `~` literal so the
expansion happens at defcustom-load time, not at every
`expand-file-name` call.

`workspace-default-home-builder` reading from `:home` via the registry
(rather than receiving `:home` as a parameter) preserves the existing
`workspace-home-builder` function signature `(NAME)`. Custom builders
the user has written continue to work; if they want the home dir they
can do the same registry lookup. A signature change would be a
breaking API surface for a small ergonomic win.

The "collision is a hard error" choice (design D2 / spec scenario
"Default-path collision is rejected"): users have a clear remedy
(prefix arg to anchor) and we never silently reuse a stale dir.

## Design pattern

`interactive "sWorkspace name: \nP"` — the `\nP` adds prefix-arg
binding without prompting for it. See existing `workspace-` commands
for the established style.

`cl-letf` over `let` for tab-bar functions in tests so the binding
applies even if tab-bar resolves the symbol via `symbol-function`.

## Verification

- Tangle: both `workspaces.org` and `tabs.org`.
- Spec: `./bin/run-tests.sh -d config/workspaces -p workspace-new-default-spec`
- Full workspaces test run: `./bin/run-tests.sh -d config/workspaces`
- Manual: in a fresh isolated Emacs, `M-x workspace-new foo`; check
  `~/emacs-workspaces/foo/` exists, has `.git/`, has `home.org` (with
  `#+TITLE: foo`), has `sessions/<date>-initial.org`, has one git
  commit, and the tab shows `home.org`.

## Context

design.md § Decisions / D1 — module layout (tabs.org changes)
design.md § Decisions / D2 — pipeline error handling (scaffold-then-register order)
design.md § Goals — default path produces a fully scaffolded, git-tracked workspace
specs/workspaces/spec.md § ADDED "workspace-new default scaffolding"
specs/workspaces/spec.md § MODIFIED "Per-workspace home layout" (default builder opens home.org)

## Cycle 1 updates (cycle-20260525-200459)

### Cited register entries

- `register/shape/workspace-plist-v3`: speculated → **confirmed**.
  The data-model task introduced `workspace--make(name home)`. Your
  task's call to `workspace--make` MUST pass HOME (this task replaces
  the cycle-1 placeholder synthesis with proper defcustom-driven
  HOME — see "Already-shipped placeholder" below).
- `register/boundary/home-org-read-pipeline`: speculated →
  **reconciled**. Producers list now records that the new
  `home-org.org` module is wired via `config/workspaces/workspaces.org`'s
  submodule loader (not `init.org`'s `jf/enabled-modules`); apply the
  same wiring convention when the cycle-2 `scaffold-module` task lands
  (which you depend on).

### Already-shipped placeholder (REPLACE, don't preserve)

Cycle 1's mid-cycle `wire-home-into-callsites` task added a
**placeholder** HOME synthesis in `config/workspaces/tabs.org`:

```elisp
;; CYCLE-1 PLACEHOLDER replaced by cycle-3's
;; workspace-new-default-path task ...
(defun wire-home-into-callsites--synthesize-home (name)
  (expand-file-name
   name
   (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~"))))
```

…and threaded it into `workspace-new`'s sole `workspace--make` call.
**Your task's job includes removing this helper** and replacing it
with the proper defcustom-driven synthesis. Search for the literal
`wire-home-into-callsites--synthesize-home` (the deliberately-verbose
name was chosen so you can grep for it).

The placeholder's synthesis logic is correct in shape (basename ==
name, absolute path); your task replaces it with one that honours
the new `workspaces-default-parent-directory` defcustom AND dispatches
to the cycle-2 scaffold pipeline for the actual directory creation.
Don't preserve the helper as a fallback — the scaffold pipeline is
the canonical path post-cycle-2.

Self-audit before deleting: grep for any other callers of
`wire-home-into-callsites--synthesize-home`. Cycle 1 introduced exactly
one (in `workspace-new`); if cycle 2 added more, address each.

### Already-shipped infrastructure

- `config/workspaces/data-model.el`: `workspace--home`,
  `workspace--set-home`, `workspace--broken-p` accessors are
  available (cycle 1).
- `config/workspaces/home-org.el`: `workspace-home-org-path`,
  `workspace-home-org-exists-p`, `workspace-home-org-title`
  available for your `workspace-default-home-builder` to use directly
  (cycle 1).

### Open ask carried from cycle 1

- `ask-cycle-20260525-200459-1`: design.md §D5/§D6 names the predicate
  `workspace--home-broken-p`, but the register + implementation use
  `workspace--broken-p`. Not directly in your scope (you don't touch
  the broken-state guards), but worth knowing if you read design.md.

## Cycle 2 updates (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. All three blockers closed:
  - `add-home-slot-to-data-model` (cycle 1, merge `7026d37`)
  - `persistence-schema-v3` (cycle 2, merge `f21f592`)
  - `scaffold-module` (cycle 2, merge `5be66c7`)

### Cycle-2 register-diff hits relevant to this task

- `register/boundary/workspace-scaffold-pipeline`: speculated →
  **reconciled**. The canonical signature for the function this task
  calls is `(cl-defun workspace-scaffold (home name &key
  init-and-commit?) ...)`. **For the default-path branch (no prefix
  arg), invoke with `:init-and-commit? t`.** The scaffold returns
  HOME on success; signals `user-error` with the path on failure.
  Per invariant `scaffold-leave-partial-on-failure` (cycle-2
  confirmed), DO NOT auto-cleanup on scaffold failure — surface the
  error to the user verbatim, and DO NOT register the workspace.
  See
  `.orchestrator/cycles/cycle-20260525-213500/reconciliations/boundary-workspace-scaffold-pipeline.md`.

- `register/shape/workspace-plist-v3`: re-confirmed cycle 2. Your
  call sequence (scaffold returns HOME; you construct the plist with
  `:home HOME`; persist; register) matches the shape's required-keys
  contract. Persistence-side `:home` round-trip is now live and
  tested (12 specs at `persistence-v3-spec.el` + `broken-home-load-spec.el`).

### Initial-session producer (per design D7)

The scaffold pipeline's stage 5 currently writes a minimal
`#+TITLE: <date> initial session` file (no gptel-sessions
integration this cycle — the orchestrator removed scaffold-module's
speculative `(featurep 'gptel-sessions)` branch at inline-fix
commit `63d60ec` because no non-interactive
`gptel-sessions-create-empty-file` exists and the branch was dead
AND would have failed gptel-sessions's directionality lint). Your
caller code does NOT need to do anything additional with the
initial session file; it lives at `<HOME>/sessions/<date>-initial.org`
and is committed by the scaffold pipeline's stage 6.

A future cycle may add a gptel-sessions hook for richer initial-
session content; the integration belongs on the gptel side, not
here.

### Cross-cycle dependency: anchor flows

Tasks `workspace-new-anchor-existing` (prefix-arg branch) and
`workspace-delete-and-purge` are blocked on THIS task. Your
implementation establishes the call-site shape both consume — keep
the workspace-construction logic factored such that the prefix-arg
branch (cycle 3+) can call it with the same HOME but
`:init-and-commit? nil`.
