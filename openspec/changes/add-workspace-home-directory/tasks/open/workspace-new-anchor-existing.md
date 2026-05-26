---
name: workspace-new-anchor-existing
description: Prefix-arg branch — anchor an existing directory with three sub-cases (repo+home.org, repo no home.org, non-repo); double-registration guard
change: add-workspace-home-directory
status: needs-review
merge_commit: 9cbba3e
relations: []
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

## Cycle 3 updates (cycle-20260526-171719)

### Status

- `status: blocked` → `status: ready`. Blocker `workspace-new-default-path`
  closed at merge `cde20af` (cycle-3). The `workspace--new-anchor-existing`
  stub is live at `config/workspaces/tabs.el` signalling `user-error`
  ("Anchor-existing not yet implemented (cycle 4)"); this task replaces
  the stub with the real implementation.

### Cycle-3 register-diff hits relevant to this task

- `register/shape/workspace-plist-v3`: confirmed → **reconciled**. Cycle-3
  inline-fix `c6c1b22` added `workspace--set-name` as a sibling setter to
  `workspace--set-home`. Your task does NOT touch the rename code path
  (re-anchor's rename branch is in `workspace-re-anchor`, separate
  command), but the lockstep convention is now part of the entry's
  contract — keep `:name` and `:home` consistent in every code path
  that mutates either.

- `register/invariant/registry-name-equals-basename`: confirmed →
  **reconciled**. Same inline-fix; same convention. Your task's
  "double-registration guard" upholds the registry-key uniqueness
  invariant which is sibling to (but not the same as) name-equals-
  basename. Construct the new plist via `workspace--make name home`;
  basename(home) == name by construction at the canonical caller.

- `register/invariant/home-required-no-floating-workspaces`: confirmed →
  **reconciled**. Cycle-3 added the absolute-path skip arm to the
  persistence deserializer. Your task ingests an existing directory
  via `read-directory-name`; the user-supplied path will be absolute
  by virtue of the prompt's interactive completion. No additional
  guard required here, but the broader invariant remains the load-
  bearing contract.

### Architect findings relevant to this task

- `arch-cycle-20260526-171719-01`: scaffold.el inlines
  `(expand-file-name "home.org" home)` duplicating the
  `workspace-home-org-path` helper. Not in your scope — `scaffold` is
  not on the anchor-existing path (which uses an *existing* directory).
  Flagged for follow-up task `route-scaffold-writer-through-home-org-helper`.

### Open asks (do not block this task)

Two cycle-3 user asks (writer-lint heuristic; *scratch* fallback
reachability) are non-blocking on your work. The fallback ask
(`ask-cycle-20260526-171719-2`) is *adjacent* — if option A is
chosen, your task may need to incorporate a `cl-check-type` guard
in `workspace--make`. Watch the disposition.

### Notes on the anchor-existing call sequence

The cycle-3 default-path branch (which you'll dispatch from) does:

```
workspace-scaffold → puthash → tab-bar-new-tab → workspace-home-builder
```

The anchor-existing branch should do (per design):

```
validate-existing-dir → puthash (workspace--make name home) → tab-bar-new-tab → workspace-home-builder
```

i.e. NO `workspace-scaffold` call (the directory already exists; you
do not init-and-commit). Use the `workspace-scaffold` boundary's
`:init-and-commit? nil` keyword for the case-2 (repo, no home.org)
sub-flow if you want to share the home.org seeding logic; otherwise
write the home.org directly.

## Observations

- Implementation matches the task body verbatim: three sub-cases by `cond`
  on `(is-repo? has-home-org?)`; double-registration guard via
  `workspace--registered-for-home-p` using `file-equal-p` on
  directory-normalized, `expand-file-name`-d paths; plist construction
  via `workspace--make name home`; `tab-bar-new-tab` → `tab-bar-rename-tab`
  → `puthash` → `workspace-home-builder` order matches the cycle-3
  default-path branch.
- Updated `workspace-new`'s docstring: the prefix-arg branch ignores
  the interactive NAME argument (the workspace name is derived from
  the chosen directory's basename per
  `register/invariant/registry-name-equals-basename`). The cycle-3
  docstring still pointed at "task `workspace-new-anchor-existing`"
  as a future cycle.
- Added `(require 'workspace-home-org)` to `tabs.el` because
  `workspace-home-org-exists-p` is now invoked here. The transitive
  require chain through `workspace-scaffold` does NOT pull in
  `workspace-home-org` (scaffold writes home.org directly without
  consulting the reader), so the new require is load-bearing.
- Spec file structure mirrors `workspace-new-default-spec.el` (cycle-3
  reference): per-describe `before-each` resets registry/tab-bar/tmpdir
  and re-installs spies; `after-each` deletes the tmpdir tree AND
  restores `workspaces-default-parent-directory` to its pre-spec value
  via a `wna-spec--saved-default-parent-directory` sentinel. (This
  spec does not actually mutate that defcustom — anchor-existing
  uses the user-picked path, not the parent dir — but the
  restoration is in place defensively in case a future test does
  set it.)
- Per task body, used `workspace-scaffold home name :init-and-commit? nil`
  for case 2 rather than writing home.org directly. The home.org
  writer-lint passes because the only literal "home.org" file-write
  primitives in `config/workspaces/*.el` remain inside
  `workspace--scaffold-write-home-org` in `scaffold.el`.
- The case-1 spec asserts that the user-authored `home.org` content
  is preserved byte-for-byte after the anchor call. The
  scaffold-write helper is guarded by `(unless (file-exists-p path)
  ...)`, so even if a future regression accidentally invoked
  `workspace-scaffold` in case 1, the user content would survive —
  but the current case-1 path does not call `workspace-scaffold` at
  all (asserted via `:not :to-have-been-called`).
- The case-1 spec also asserts that `sessions/` was NOT created. This
  is an additional regression guard beyond what the task body
  prescribed — case 1 must be PURE registration with zero filesystem
  side-effects on the user's repo.
- Trailing-slash spec: I documented in the test comment that
  `read-directory-name` with mustmatch=t WILL canonicalize the input
  in practice (the user-typed path is resolved against existing
  files). The test deliberately stubs the prompt to return the
  un-canonicalized form so the `file-equal-p`-based guard is
  exercised against the worst-case input. This is a defense-in-depth
  property — even if a future Emacs version changes
  `read-directory-name` behavior, the guard still holds.
- The `wna-spec--init-repo` fixture uses local `git config user.email`
  and `user.name` so the spec does not depend on the runner's global
  git identity. The pre-existing `scaffold-spec.el` relies on the
  runner having a global identity configured (it does `git commit`
  via the production code path); the new spec is more hermetic on
  this point.
- Adjacent observation (NOT in scope): pre-existing spec files in
  `config/workspaces/test/` are missing the `after-each` cleanup
  convention. The deferred task `cleanup-workspaces-test-isolation`
  (cycle 5) is the right place to address this. The new spec
  follows the cycle-3 convention from `workspace-new-default-spec.el`.
- No push-backs against cited register entries. All four cited
  entries (`register/boundary/workspace-scaffold-pipeline`,
  `register/shape/workspace-plist-v3`,
  `register/invariant/registry-name-equals-basename`,
  `register/invariant/home-required-no-floating-workspaces`) match
  the implementation exactly. The `workspace--set-name` /
  `workspace--set-home` lockstep convention (cycle-3 inline-fix)
  does NOT come into play here — anchor-existing constructs a fresh
  plist via `workspace--make`; the rename branch is owned by
  `workspace-re-anchor`.

## Discoveries

- discovery_id: disc-workspace-new-anchor-existing-1
  class: invariant-gap
  description: |
    `workspace--registered-for-home-p` does NOT check the `:broken`
    flag — a broken workspace's `:home` is still treated as a
    registered home, which is the correct uniqueness semantics
    (re-anchoring a broken entry is the dedicated path; you don't
    create a SECOND workspace pointing at the same dir just because
    the first one is broken). The task body's cycle-2 note flagged
    this same property; the implementation matches. No new register
    entry needed — the existing
    `register/invariant/registry-name-equals-basename` and the
    broken-state vocabulary entries cover the semantics; this is
    a noteworthy interaction worth pinning prose around if a future
    spec ever encodes "registry key uniqueness" as a distinct
    invariant.
  affected_register_entry: register/invariant/registry-name-equals-basename
  recommendation: |
    Consider promoting "registry key uniqueness across all entries
    regardless of broken state" to its own register/invariant entry
    in a future cycle. The current invariant family covers the name
    derivation but not the uniqueness composite.

- discovery_id: disc-workspace-new-anchor-existing-2
  class: spec-signal
  description: |
    The interactive NAME argument to `workspace-new` is IGNORED on
    the prefix-arg path (the name is derived from the chosen
    directory's basename). The cycle-3 docstring still claimed the
    prefix-arg branch was unimplemented; I updated it to note the
    NAME-ignoring semantics. The interactive form
    `(interactive "sWorkspace name: \nP")` still prompts for a name
    on the prefix-arg path — a UX wart: the user types a name that
    is then discarded. A cleaner UX would short-circuit the name
    prompt when the prefix arg is non-nil, but this requires
    splitting the interactive spec which is more invasive than this
    task's contract permits.
  affected_register_entry: register/invariant/registry-name-equals-basename
  recommendation: |
    Future task / cycle could refactor `workspace-new`'s interactive
    form to skip the name prompt under prefix-arg (e.g. by
    dispatching the interactive form by `current-prefix-arg`). Not
    in scope for this task; left as a UX polish item.

- discovery_id: disc-workspace-new-anchor-existing-3
  class: interface-drift
  description: |
    The case-2 sub-flow (repo, no home.org) calls
    `workspace-scaffold home name :init-and-commit? nil`. The
    scaffold pipeline stages 4 (`make-directory sessions/`) and 5
    (`workspace--scaffold-initial-session`) ALWAYS run regardless of
    `:init-and-commit?`. This means anchoring an existing user-owned
    repo will CREATE a `sessions/` subdirectory and a
    `sessions/<date>-initial.org` file even if the user did not want
    them. The task body explicitly accepts this: "the stub file
    lands; the user may delete it if they have their own session-
    management preferences" (cycle-2 update). Worth noting as a
    visible side-effect on case 2.
  affected_register_entry: register/boundary/workspace-scaffold-pipeline
  recommendation: |
    No action required. The behavior is documented in the scaffold
    pipeline's stage gating and was explicitly accepted in the
    cycle-2 inline-fix that removed the gptel-sessions
    speculative branch. If a future cycle wants finer-grained
    control (e.g. a `:create-sessions? nil` keyword), the
    cl-defun-keyword shape already accommodates it.
