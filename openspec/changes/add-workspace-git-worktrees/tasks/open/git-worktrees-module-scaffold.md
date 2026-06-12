---
name: git-worktrees-module-scaffold
description: New config/workspaces/git-worktrees.org module — header, soft repo-source helper, magit-gated registration skeleton, enabled-modules entry
change: add-workspace-git-worktrees
status: ready
relations:
  - enables:add-worktree-command
  - enables:on-purge-teardown-handler
---

## Files to modify
- config/workspaces/git-worktrees.org (new) — tangles to git-worktrees.el
- config/workspaces/workspaces.org (modify) — add a `jf/load-module` line for
  git-worktrees.el in the loader block, OR add to init.org enabled-modules (see
  step 5; follow whichever the repo uses for workspace submodules)
- init.org (modify) — `jf/enabled-modules` entry if workspaces submodules are
  registered there
- config/workspaces/test/git-worktrees-spec.el (new)

## Implementation steps
1. Create `config/workspaces/git-worktrees.org` with the literate headers:
   ```org
   #+title: Workspaces — Git Worktrees Integration
   #+property: header-args:emacs-lisp :tangle git-worktrees.el
   #+auto_tangle: y
   ```
   First src block (`:comments no`) emits the lexical-binding cookie on line 1:
   `;;; git-worktrees.el --- Git worktree workspace integration -*- lexical-binding: t; -*-`.
   `(require 'cl-lib)`. Do NOT `(require 'magit)` at top level — gate on it (see
   step 4). End with `(provide 'workspace-git-worktrees)`.
2. Implement the soft repo-candidate helper
   `jf/workspace--worktree-repo-candidates`:
   - Collect, guarded by `(boundp/fboundp ...)` / `featurep`, from
     `projectile-known-projects` (when projectile loaded),
     `(magit-list-repos)` (when magit loaded), and
     `project-known-project-roots` (when project.el available).
   - Merge and de-dupe (`delete-dups` on expanded absolute paths).
   - Filter to actual git repos (`(locate-dominating-file dir ".git")` or
     `(magit-toplevel dir)`).
   - Return the list; callers add a `read-directory-name` floor at the prompt.
   Keep it pure/list-returning so it is unit-testable without prompting.
3. Add a small repo-selection wrapper `jf/workspace--worktree-read-repo` that
   `completing-read`s over the candidates and always allows a free-form path
   (so a repo in no list is still reachable). This is the only place that
   prompts.
4. Add the registration skeleton, gated so absent magit means NO integration
   (not an error):
   ```elisp
   (with-eval-after-load 'workspaces
     (with-eval-after-load 'magit
       (workspace-register-integration 'git-worktree
         :label "git worktree"
         :menu (cons "w" #'jf/workspace--add-worktree)      ; from add-worktree-command task
         :on-purge #'jf/workspace--worktree-on-purge)))     ; from on-purge task
   ```
   The two handler symbols are implemented in the dependent tasks; reference
   them here (forward declaration is fine — registration only runs after load).
5. Register the module so it loads: add a `jf/load-module` line for
   `config/workspaces/git-worktrees.el` in the workspaces loader block (after
   integrations.el) and/or an `init.org` `jf/enabled-modules` entry — match how
   existing workspace submodules are wired. Confirm removing this one entry
   leaves workspaces fully functional.
6. Tangle + validate: `./bin/tangle-org.sh config/workspaces/git-worktrees.org`.
7. Create `git-worktrees-spec.el` covering the repo-candidate helper: merges
   distinct repos from multiple loaded sources, de-dupes cross-source
   duplicates, excludes non-git dirs, and the floor prompt path. Spy
   projectile/magit/project.el list functions; do not require them really
   present.

## Design rationale
This is the user's "leverage where valuable without a hard dependency" seam:
we consume only candidate LISTS from projectile/magit/project.el, never their
workflows, with a `read-directory-name` floor so none is required. The module
hard-depends on magit for the actual worktree operations, but registering under
`with-eval-after-load 'magit` keeps it honestly droppable — its absence simply
means no git integration. The module attaches solely through
`workspace-register-integration`, exactly as gptel does, so workspaces core
names no consumer.

## Design pattern
Model the module on config/gptel/sessions/workspace-integration.org: literate
headers, `with-eval-after-load 'workspaces` registration, no mutation of
workspace core. Model registration shape on the existing gptel registration
block.

## Verification
- `./bin/tangle-org.sh config/workspaces/git-worktrees.org` validates
- `./bin/run-tests.sh -d config/workspaces` — repo-source specs pass
- Acceptance (spec scenarios): candidates merge from loaded sources; non-git
  excluded; floor prompt available; absent magit ⇒ integration not registered;
  removing the module leaves workspaces working.

## Context
design.md § Decisions 'D1 — Git operations go through magit' and 'D2 — Repo-source selection stays soft and list-only'
specs/workspace-git-worktrees/spec.md (Requirement: Git non-mandatory …;
Requirement: Soft repo-source selection)
