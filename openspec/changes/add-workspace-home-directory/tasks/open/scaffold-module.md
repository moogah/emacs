---
name: scaffold-module
description: New scaffold.org — directory, git init, home.org skeleton, sessions/, initial gptel session, initial commit
change: add-workspace-home-directory
status: blocked
relations:
  - blocked-by:add-home-slot-to-data-model
  - blocked-by:home-org-reader-module
---

## Files to modify
- `config/workspaces/scaffold.org` (new — literate source)
- `config/workspaces/scaffold.el` (new — tangled)
- `config/workspaces/test/scaffold-spec.el` (new — Buttercup)
- `init.org` (modify — register `workspaces/scaffold` in `jf/enabled-modules`)

## Implementation steps

1. Create `config/workspaces/scaffold.org` with the standard literate
   header and lexical-binding declaration:

   ```org
   #+title: Workspaces — Directory Scaffolding
   #+author: Jeff Farr
   #+property: header-args:emacs-lisp :tangle scaffold.el
   #+auto_tangle: y
   ```

   ```elisp
   ;;; scaffold.el --- Workspaces directory scaffolder -*- lexical-binding: t; -*-
   (require 'cl-lib)
   ```

2. Define a small subprocess helper that runs git and signals on
   non-zero exit:

   ```elisp
   (defun workspace--scaffold-git (home &rest args)
     "Run `git ARGS' with `default-directory' bound to HOME.
   Signal `user-error' with stderr on non-zero exit."
     (let* ((default-directory (file-name-as-directory home))
            (out-buf (generate-new-buffer " *workspace-scaffold-git*"))
            (status (apply #'call-process "git" nil out-buf nil args)))
       (unwind-protect
           (unless (zerop status)
             (user-error "git %s failed in %s: %s"
                         (mapconcat #'identity args " ")
                         home
                         (with-current-buffer out-buf
                           (string-trim (buffer-string)))))
         (kill-buffer out-buf))))
   ```

3. Define the home.org skeleton writer:

   ```elisp
   (defun workspace--scaffold-write-home-org (home name)
     "Write the initial `home.org' skeleton into HOME, using NAME for #+TITLE:.
   Does nothing if `home.org' already exists.
   The package writes home.org exactly once (at scaffold time); never again
   after that."
     (let ((path (expand-file-name "home.org" home)))
       (unless (file-exists-p path)
         (with-temp-file path
           (insert (format "#+TITLE: %s\n\n" name))
           (insert "* Description\n\n")
           (insert "* Notes\n")))))
   ```

   Note: do NOT write a `* Sessions` heading. Spec is explicit that the
   package never auto-renders/syncs session lists into home.org.

4. Define the initial gptel session creator. Prefer calling into the
   gptel sessions module if loaded; fall back to writing an empty file
   if not:

   ```elisp
   (defun workspace--scaffold-initial-session (home)
     "Create the initial gptel session file under HOME/sessions/.
   Returns the absolute path of the session file."
     (let* ((sessions-dir (expand-file-name "sessions" home))
            (filename (format "%s-initial.org" (format-time-string "%Y-%m-%d")))
            (path (expand-file-name filename sessions-dir)))
       (make-directory sessions-dir t)
       (cond
        ((and (featurep 'gptel-sessions)
              (fboundp 'gptel-sessions-create-empty-file))
         ;; Prefer the gptel module's own session-file creator so the
         ;; schema stays current. The actual function name needs to be
         ;; located during apply — see Open Question Q1 in design.md.
         (gptel-sessions-create-empty-file path))
        (t
         (with-temp-file path
           (insert (format "#+TITLE: %s initial session\n" (format-time-string "%Y-%m-%d"))))))
       path))
   ```

   **During apply**: locate the actual gptel session-file creator in
   `config/gptel/sessions/`. If no suitable entry point exists, this
   task's fallback (write a minimal `#+TITLE:` file) is sufficient;
   any further coupling is owned by task
   `gptel-sessions-workspace-consult`.

5. Define the top-level pipeline function. It accepts the home path
   and a boolean `do-init-and-commit?` (true for default-path and
   non-repo anchor branches; false for "anchor existing repo"):

   ```elisp
   (defun workspace-scaffold (home name &key init-and-commit?)
     "Scaffold a workspace directory at HOME with display NAME.

   When INIT-AND-COMMIT? is non-nil, run `git init' and an initial
   `git commit'. When nil (anchoring an existing repo without
   home.org), do file writes only — never touch the user's git state.

   Steps (default-path / non-repo branches):
     1. make-directory HOME
     2. git init HOME              (skipped when init-and-commit? is nil)
     3. write HOME/home.org skeleton
     4. make-directory HOME/sessions/
     5. create HOME/sessions/<date>-initial.org
     6. git add . && git commit    (skipped when init-and-commit? is nil)

   On any failure, signal `user-error'. Partially-created files are
   LEFT IN PLACE — silent rm -rf is too risky (design D2). The error
   message names the path so the user can inspect/remediate manually.

   This function does NOT register the workspace or create a tab —
   that is the caller's responsibility (tabs.org workspace-new)."
     (make-directory home t)
     (when init-and-commit?
       (workspace--scaffold-git home "init"))
     (workspace--scaffold-write-home-org home name)
     (make-directory (expand-file-name "sessions" home) t)
     (workspace--scaffold-initial-session home)
     (when init-and-commit?
       (workspace--scaffold-git home "add" ".")
       (workspace--scaffold-git home "commit" "-m" "Initial workspace"))
     home)
   ```

6. End with `(provide 'workspace-scaffold)`.

7. Register the module: add `"workspaces/scaffold"` to
   `jf/enabled-modules` in `init.org`, placed after `"workspaces/home-org"`
   and before `"workspaces"` (the top-level loader).

8. Create `test/scaffold-spec.el`. Use real `git` against tmpdirs (per
   design D9 — no mocking subprocesses):

   - **default-path scaffold (init-and-commit? t)**: invoke
     `(workspace-scaffold tmp "foo" :init-and-commit? t)`; assert:
     `.git/` exists; `home.org` exists with `#+TITLE: foo`;
     `sessions/` exists; one `*-initial.org` file in `sessions/`;
     `git log --oneline` shows exactly one commit named `Initial
     workspace`.
   - **anchor-without-commit (init-and-commit? nil)**: invoke against
     an existing `git init`'d tmpdir; assert: `home.org` and
     `sessions/<date>-initial.org` exist; `git status --porcelain`
     shows them as untracked (no commit made).
   - **idempotent home.org write**: pre-create `home.org` with custom
     content; invoke scaffold; assert the file content is unchanged.
   - **mid-pipeline failure**: stub `workspace--scaffold-git` via
     `cl-letf` to signal on the `commit` arg; assert the
     `user-error` is raised; assert that the directory, `home.org`,
     and `sessions/initial.org` ARE all left in place on disk (no
     auto-cleanup).
   - **git missing on PATH**: stub `call-process` to return a non-zero
     exit; assert `user-error` is raised mentioning git.

   Helper macro for tmpdirs (consider lifting into a shared
   `test/helpers.el` if not present):

   ```elisp
   (defmacro workspace-test--with-tmp-home (binding &rest body)
     "Bind BINDING to a fresh tmp dir; delete on exit."
     (declare (indent 1))
     `(let ((,(car binding) (make-temp-file "ws-test-" t)))
        (unwind-protect (progn ,@body)
          (delete-directory ,(car binding) t))))
   ```

9. Tangle and run:
   ```bash
   ./bin/tangle-org.sh config/workspaces/scaffold.org
   ./bin/run-tests.sh -d config/workspaces -p scaffold-spec
   ```

## Design rationale

The scaffold pipeline is a coherent concern (filesystem + git + skeleton
files + initial session) and gets its own module rather than inflating
`tabs.org`. The pipeline is synchronous because users expect
`workspace-new` to return with a ready-to-use workspace (design D2 —
async via `make-process` rejected).

Real git in tmpdirs (no mocking subprocess output): the four git
invocations are small and deterministic; mocking is brittle (newline
handling, exit-code paths) and would miss real-git behavior.

"Leave partial scaffold in place on failure" is a deliberate safety
choice. If the user typo'd the path and we silently `rm -rf` it, that's
catastrophic. The cost is an awkward manual cleanup if scaffold fails
mid-pipeline; the alternative is unacceptable.

The optional `init-and-commit?` parameter is the right factoring for
the prefix-arg anchor flow (task `workspace-new-anchor-existing`),
where we never touch a user-owned git repo. Same function, different
flag — the four git calls are guarded by one boolean.

## Design pattern

Subprocess error handling pattern: capture stderr into a buffer, check
exit code, signal with the captured text. `unwind-protect` to
guarantee buffer cleanup. See `config/bash-parser/` for similar
patterns.

`with-temp-file` for atomic-write-or-fail file creation. Do not use
`write-region` directly — `with-temp-file` handles encoding and atomic
rename for us.

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/scaffold.org`
- Spec: `./bin/run-tests.sh -d config/workspaces -p scaffold-spec`
- Manual: in a scratch session,
  `(workspace-scaffold (make-temp-file "ws-" t) "manualtest" :init-and-commit? t)`
  and inspect the result.

## Context

design.md § Decisions / D1 — module layout (scaffold.org as new module)
design.md § Decisions / D2 — git invocation: call-process, abort-on-failure, no auto-cleanup
design.md § Decisions / D7 — initial-session file format (gptel module call + fallback)
design.md § Decisions / D9 — testing approach (real git in tmpdirs)
specs/workspaces/spec.md § ADDED "workspace-new default scaffolding" (pipeline contract)
specs/workspaces/spec.md § ADDED "Anchoring an existing directory via prefix arg" (init-and-commit? guard)
