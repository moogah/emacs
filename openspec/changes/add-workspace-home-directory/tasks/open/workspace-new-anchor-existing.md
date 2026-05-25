---
name: workspace-new-anchor-existing
description: Prefix-arg branch — anchor an existing directory with three sub-cases (repo+home.org, repo no home.org, non-repo); double-registration guard
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:workspace-new-default-path
---

## Files to modify
- `config/workspaces/tabs.org` (modify — add `workspace--new-anchor-existing`)
- `config/workspaces/test/workspace-new-anchor-spec.el` (new — Buttercup)

## Implementation steps

1. In `tabs.org`, implement `workspace--new-anchor-existing` (already
   referenced from `workspace-new` in the sibling task):

   ```elisp
   (defun workspace--new-anchor-existing ()
     "Anchor an existing directory as a workspace.
   See `workspace-new' docstring for the prefix-arg behavior.

   Three sub-cases by directory state:
     1. Git repo AND has home.org  → register only; no scaffolding.
     2. Git repo, no home.org      → scaffold files; NO git ops
                                     (the user owns this repo).
     3. Not a git repo             → full scaffold (mkdir, git init,
                                     files, initial commit)."
     (let* ((home (file-name-as-directory
                   (read-directory-name "Anchor workspace at: " nil nil t)))
            (name (file-name-nondirectory
                   (directory-file-name home))))
       (when (workspace--registered-for-home-p home)
         (user-error "Already a registered workspace: %s" home))
       (let ((is-repo? (file-directory-p (expand-file-name ".git" home)))
             (has-home-org? (workspace-home-org-exists-p home)))
         (cond
          ;; Case 1: repo + home.org → register only.
          ((and is-repo? has-home-org?)
           ;; No scaffolding; just register.
           )
          ;; Case 2: repo, no home.org → scaffold files, no git ops.
          (is-repo?
           (workspace-scaffold home name :init-and-commit? nil))
          ;; Case 3: non-repo → full scaffold including git init + commit.
          (t
           (workspace-scaffold home name :init-and-commit? t)))
         (tab-bar-new-tab)
         (tab-bar-rename-tab name)
         (puthash name (workspace--make name home) workspace--registry)
         (when (and (boundp 'workspace-home-builder)
                    (functionp workspace-home-builder))
           (funcall workspace-home-builder name))
         (gethash name workspace--registry))))
   ```

2. Add the helper for the double-registration guard:

   ```elisp
   (defun workspace--registered-for-home-p (home)
     "Return non-nil if any workspace in the registry has :home equal to HOME.
   Path comparison is by `file-equal-p' so trailing-slash differences
   do not produce false negatives."
     (let ((target (file-name-as-directory (expand-file-name home))))
       (catch 'found
         (maphash
          (lambda (_name ws)
            (let ((ws-home (workspace--home ws)))
              (when (and ws-home
                         (file-equal-p (file-name-as-directory
                                        (expand-file-name ws-home))
                                       target))
                (throw 'found t))))
          workspace--registry)
         nil)))
   ```

3. Create `test/workspace-new-anchor-spec.el` with one `describe` per
   sub-case:

   - **Case 1 (repo + home.org)**: pre-create a tmp dir, `git init`
     it manually, write a `home.org`. Stub `read-directory-name` to
     return the tmp path. Invoke `(workspace-new "ignored" t)`.
     Assert: no new files were created (file mtimes unchanged);
     `git log` is empty (no commits); registry contains a workspace
     with `:home` set to the tmp path and registry name equal to the
     tmp basename.

   - **Case 2 (repo, no home.org)**: tmp dir + `git init`; do NOT
     write home.org. Stub `read-directory-name`. Invoke. Assert:
     `home.org` and `sessions/<date>-initial.org` exist; `git
     status --porcelain` shows them as untracked (no commit was made);
     registry entry exists.

   - **Case 3 (non-repo)**: tmp dir, nothing in it (no `.git`, no
     home.org). Stub `read-directory-name`. Invoke. Assert: `.git/`
     exists; `home.org` exists; `sessions/<date>-initial.org` exists;
     `git log --oneline` shows one commit; registry entry exists.

   - **Double-registration guard**: register a workspace for tmp dir
     X. Invoke anchor flow targeting X again. Assert `user-error` is
     signalled and registry is unchanged.

   - **Trailing-slash equivalence**: register workspace for
     `/tmp/foo/`. Invoke anchor flow targeting `/tmp/foo` (no
     trailing slash). Assert the double-registration guard fires.

4. Tangle and test:
   ```bash
   ./bin/tangle-org.sh config/workspaces/tabs.org
   ./bin/run-tests.sh -d config/workspaces -p workspace-new-anchor-spec
   ```

## Design rationale

The three sub-cases are the contract from the spec; the `cond` keeps
each branch's behavior visible at the call site. "We never touch a
repo we did not initialize" (case 2 omitting git ops) is the safety
invariant — the user owns external repos, and we don't stage their
changes for them.

The double-registration guard prevents the easy mistake of pointing
two workspaces at the same dir (they would fight over `home.org` reads,
session routing, etc.). Using `file-equal-p` instead of `string=` handles
the trailing-slash / `~` expansion edge cases.

`workspace-scaffold` with `init-and-commit? nil` (case 2) is the right
factoring: same pipeline body, fewer steps. The git-init-and-commit
behavior is purely a flag.

## Design pattern

`read-directory-name` with `mustmatch=t` (the fourth `t`) ensures the
user picks an existing directory — they cannot type a path that
doesn't exist. That matches the "anchor an EXISTING directory" spec
contract.

`file-name-as-directory` + `expand-file-name` is the canonical form
for path comparison in Emacs. Do it on both sides of `file-equal-p`.

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/tabs.org`
- Spec: `./bin/run-tests.sh -d config/workspaces -p workspace-new-anchor-spec`
- Manual:
  - `mkdir /tmp/test-anchor && cd /tmp/test-anchor && git init`; in
    Emacs `C-u M-x workspace-new`; choose `/tmp/test-anchor`; verify
    home.org and sessions/ appear but no commit was made.
  - Pre-create `/tmp/test-anchor-2` (empty dir); anchor it; verify
    full scaffold + initial commit.

## Context

design.md § Decisions / D1 — module layout
specs/workspaces/spec.md § ADDED "Anchoring an existing directory via prefix arg" (all three sub-cases + double-registration guard)


## Cycle 2 updates (cycle-20260525-213500)

### Cycle-2 register-diff hits relevant to this task

- `register/boundary/workspace-scaffold-pipeline`: speculated →
  **reconciled** cycle 2. The canonical signature is now
  `(cl-defun workspace-scaffold (home name &key init-and-commit?) ...)`.
  **For the prefix-arg anchor branch, invoke with
  `:init-and-commit? nil`** — this skips stages 2 (git init) and 6
  (git add + commit), which is the contract that lets you safely
  scaffold over an existing user-owned git repo without touching its
  git state. The two non-default-path sub-cases of `workspace-new
  NAME C-u`:
  - **Already a git repo with `home.org`**: register only; no
    scaffold call at all.
  - **Already a git repo without `home.org`**: call
    `(workspace-scaffold home name :init-and-commit? nil)` — only
    stages 1, 3, 4, 5 run (mkdir HOME is idempotent on an existing
    dir; home.org write is guarded by `file-exists-p`; sessions/
    + initial session land safely).
  - **Not a git repo**: call
    `(workspace-scaffold home name :init-and-commit? t)` — same as
    default-path branch (gates 2 + 6 run).

- `register/shape/workspace-plist-v3`: re-confirmed cycle 2. Same
  caller-side contract as the default-path task — construct the
  plist with `:home HOME` (where HOME is what the user picked, not
  what scaffold returns).

### Cycle-2 inline-fix hits

The orchestrator's inline fix at `63d60ec` removed scaffold-module's
speculative `(featurep 'gptel-sessions)` branch from
`workspace--scaffold-initial-session`. The scaffold's initial-
session creator now ALWAYS writes a minimal `#+TITLE:` stub. For the
"already a git repo with `home.org`" branch you don't trigger this
code path at all. For the other two branches the stub file lands;
the user may delete it if they have their own session-management
preferences.

### Cross-task interaction with broken-home-tolerance

The "double-registration guard" requirement of this task — refuse
to register a workspace whose `:home` matches an already-registered
entry — should also work against broken entries (the broken status
doesn't change the registry-key uniqueness invariant).
