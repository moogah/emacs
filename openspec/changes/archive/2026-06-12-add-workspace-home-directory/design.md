## Context

The `workspaces` package today represents workspaces as pure in-Emacs
constructs: a `:name`, a list of `:buffer-files`, a list of
`:layout-groups`, a `:recent-layout-group`. Identity lives in the
per-machine persistence file at `~/.emacs.d/state/workspaces-<role>.eld`.
No filesystem anchor exists.

This change anchors every workspace to an absolute filesystem path
(`:home`). The home directory becomes the carrier for cross-machine,
user-authored content: `home.org` (dashboard / display-name source) and
`sessions/` (gptel session files). The persistence file remains the
authoritative store of Emacs-specific state (layouts, window-state,
buffer-files); `home.org` and `sessions/` are mutually-exclusive with
that — neither side persists what the other owns.

The package is pre-alpha and breaking-change-friendly: no migration code
ships for floating workspaces. The persistence schema bumps from v2 to
v3 with the same rejection-and-notice pattern the v1→v2 cutover used.

Stakeholders: the `workspaces` module owners (us) and the `gptel/sessions`
module, which gains a one-way soft-dependency consult so new sessions
can be filed under `<HOME>/sessions/` when a workspace is active.

## Goals / Non-Goals

**Goals:**
- Every workspace has an absolute `:home` path; identity (registry name)
  is `basename(:home)`.
- Default `workspace-new NAME` produces a fully scaffolded, git-tracked
  workspace directory at `~/emacs-workspaces/NAME/` with zero further
  user input.
- `C-u workspace-new` anchors an existing directory with three crisply
  defined sub-behaviors (repo+home.org, repo without home.org, non-repo).
- gptel sessions module routes new sessions under `<HOME>/sessions/` when
  a workspace is active, with a `C-u` escape hatch and a `featurep`
  guard.
- `workspace-delete` is unregister-only; a separate `workspace-purge`
  command does destructive deletion with a `yes-or-no-p` confirm and a
  scope safeguard.
- Broken home directories (`:home` missing on load) yield a registry
  entry the user can re-anchor or purge — never silent auto-recreate.
- Persistence schema v3 carries `:home`; v2 files are rejected with a
  `*Messages*` notice.

**Non-Goals:**
- Aggregator vs single-repo type detection. All workspaces are treated as
  single git repositories. The directory MAY contain worktrees or
  sub-projects; the package does not enumerate them.
- Worktree integration / sibling-vs-aggregator conventions.
- Discovery commands beyond the `C-u` anchor flow (no
  `workspace-discover`, no scan-of-`~/emacs-workspaces/`).
- Binding `default-directory` / shell `cwd` to `:home`.
- `project.el` integration.
- Workspace-aware completion ranking for files under `:home`.
- A `workspace-rename` command. Renames are out-of-band (`mv` the dir,
  restart, workspace re-appears under the new basename).
- Any auto-rendering of session inventory into `home.org`. The package
  writes `home.org` exactly once (at scaffold time) and reads it
  thereafter.

## Decisions

### D1. Module layout: one new `scaffold` module, plus edits to existing modules

The scaffolding pipeline (mkdir, git init, write home.org skeleton, mkdir
sessions, create initial session, git add + commit) is a coherent
concern with non-trivial error handling. It lands in a new module
`config/workspaces/scaffold.org` → `scaffold.el`. A second new module
`config/workspaces/home-org.org` → `home-org.el` houses the small set
of `home.org` reader helpers (title lookup, file-existence check). The
`home-org` module is intentionally separate from `data-model.org` because
it touches I/O — `data-model.org` is the side-effect-free layer and we
want to preserve that property.

Existing modules absorb the rest:
- `data-model.org` — add `:home` to `workspace--make`, `workspace--home`
  accessor, `workspace--broken-p` predicate (runtime tag).
- `tabs.org` — `workspace-new` dispatches to `scaffold` for the default
  path and the prefix-arg branches; default `workspace-home-builder` is
  rewritten to `find-file <home>/home.org`.
- `persistence.org` — schema constant bumps to `3`; reader rejects
  non-`3` files; per-workspace deserializer skips entries lacking
  `:home`; loaded entries whose `:home` is missing get a `:broken` tag
  and a `*Messages*` notice.
- `workspaces.org` — `defcustom workspaces-default-parent-directory`
  (default `~/emacs-workspaces/`), new commands `workspace-purge` and
  `workspace-re-anchor`, updated key bindings (`C-x w D` for delete,
  `C-x w P` for purge — uppercase to distinguish from buffer-removal
  commands).

**Alternatives considered:** Folding scaffolding into `tabs.org`. Rejected
because `tabs.org` is currently focused on tab-bar and workspace-new
control flow; the scaffold pipeline (git subprocess, file I/O, rollback
semantics) is a different concern and inflates that module unhelpfully.

### D2. Git invocation: `call-process`, abort-on-failure, no auto-cleanup

`scaffold.el` runs git via `call-process "git" nil OUTPUT-BUFFER nil
"init" / "add" / "commit"`. Synchronous. Non-zero exit signals
`user-error` carrying the git stderr.

If any pipeline step (steps 2–7 in the spec) fails:
1. The pipeline aborts; subsequent steps do not run.
2. No registry mutation; no tab is created.
3. The partially-scaffolded directory is **left in place** on disk.

The "leave it" choice: silent `rm -rf` on partial failure is too risky
(if the user pointed at the wrong path because of a typo, we don't want
to delete files we didn't create). The error message names the path and
suggests `M-x workspace-purge` (which, post-creation, requires a registry
entry — so the user inspects manually). Acceptable cost: the user may
need a manual `rm -rf` to retry.

**Alternatives considered:**
- Background async git via `make-process` — rejected; users expect
  `workspace-new` to return synchronously with a ready-to-use workspace.
- Library wrapper (e.g., `magit-call-git`) — rejected; introduces a
  hard runtime dependency on magit just for scaffolding. The four git
  invocations don't justify it.
- Auto-cleanup-on-failure — rejected; see above.

### D3. gptel integration: soft dependency via `featurep` + named function

Workspaces module defines `workspace-sessions-dir` (function):

```elisp
(defun workspace-sessions-dir ()
  "Return the sessions/ dir of the current workspace, or nil if off-workspace."
  (when-let* ((ws (workspace--current))
              (home (workspace--home ws)))
    (expand-file-name "sessions" home)))
```

`gptel/sessions/` module's session-creation entry point gains:

```elisp
(defun gptel-sessions--target-dir (&optional force-global)
  (or (and (not force-global)
           (featurep 'workspaces)
           (fboundp 'workspace-sessions-dir)
           (workspace-sessions-dir))
      gptel-sessions-default-directory))
```

The `force-global` escape hatch is exposed as a SEPARATE
`-global`-suffixed command (e.g. `jf/gptel-persistent-session-global`)
alongside the workspace-aware command (`jf/gptel-persistent-session`),
NOT as a prefix-arg overload on the workspace-aware command. The
existing workspace-aware command already binds `current-prefix-arg`
to an orthogonal concern (preset selection); overloading the prefix
slot would break that pre-existing affordance. Both commands surface
in `M-x` completion, making the escape hatch discoverable.

The dependency direction is one-way: `gptel/sessions/` optionally
consults `workspaces`; `workspaces` never references
`gptel/sessions/`. Soft via `featurep` so the gptel module continues
to load and function in configurations where `workspaces` is disabled.

**Alternatives considered:**
- A defcustom `gptel-sessions-dir-function` that workspaces overrides on
  load. Rejected: gives the user one more configuration surface to
  understand without any flexibility they couldn't get by directly
  overriding `gptel-sessions--target-dir` via `cl-letf` in their config.
- Workspaces module advising the gptel session creator. Rejected:
  advice is harder to reason about than the explicit consult at the
  call site, and it reverses the dependency direction.

### D4. `home.org` reader: regex scan, no `org-mode` activation

`workspace-home-org-title <home>` reads the file with
`insert-file-contents` into a temp buffer and runs a regex for `^#\+TITLE:
\(.*\)$` (case-insensitive). Returns the trimmed match or nil. No
`org-mode` invocation; no parser; no caching. This keeps the read cheap
enough that the spec's "live read on every call" promise is genuinely
free.

**Why not `org-collect-keywords`?** It works, but requires loading
`org-mode` for the buffer; the regex is ~10× faster and our usage
(display name lookup, polled lazily during prompts) does not benefit
from a full parser.

### D5. Persistence schema v3 — slot addition, same file path

Schema constant `workspace--state-version` (the generic version
constant carried from the v1→v2 cutover) bumps from `2` to `3`.
The on-disk filename is unchanged
(`~/.emacs.d/state/workspaces-<role>.eld`) — the schema integer
discriminates. v1 and v2 readers were already a single `(unless (= ver
EXPECTED) (notice-and-return-nil))` gate; v3 reuses the gate with the
new expected value.

Serialized workspace plist gains `:home <absolute-path>`. The
deserializer:
- Skips entries lacking `:home` (notice + continue).
- For entries with `:home`, calls `(file-directory-p :home)`. If false,
  marks the in-memory plist with `:broken t` and emits a notice. The
  `:broken` tag is **runtime-only** — it is never serialized back to
  disk; on next save the workspace is written with its `:home` intact and
  the broken tag implicitly dropped (since the next-restart check is
  fresh).

### D6. Broken-state semantics in commands

`workspace-switch` and `workspace-restore` consult
`workspace--broken-p` before doing anything else and signal
`user-error` if true. `workspace-purge` is permitted on broken
workspaces (registry-only cleanup; no filesystem deletion to perform).
`workspace-re-anchor NAME PATH` clears the broken tag, updates `:home`,
and renames the registry key if the new basename differs from the old.

### D7. Initial-session file format

`<HOME>/sessions/<ISO-date>-initial.org` contains a minimal gptel session
template. The exact format will be cribbed from
`config/gptel/sessions/`'s own current new-session function (the cleanest
path is for `scaffold.el` to call into the gptel sessions module to
generate the file — same code path as user-driven creation, just routed
to a specific path). If the gptel sessions module is not loaded at
workspace-new time, `scaffold.el` falls back to writing an empty file
with `#+TITLE: <date> initial`. This means the scaffolder has an
*optional* runtime dependency on gptel sessions — `featurep` check.

**Alternative considered:** hard-code the template. Rejected because the
gptel session schema is likely to evolve, and hard-coding here creates a
drift point.

### D8. `workspace-purge` safeguard implementation

```elisp
(unless (or current-prefix-arg
            (file-in-directory-p home workspaces-default-parent-directory))
  (user-error "Refusing to purge %s; outside default parent (%s). Use C-u to override."
              home workspaces-default-parent-directory))
```

The check happens **before** the `yes-or-no-p` confirm. Two confirmations
are required to delete a workspace under `~/code/`: the prefix arg, and
the explicit yes-or-no.

### D9. Testing approach

**Framework:** Buttercup. New tests live at
`config/workspaces/test/scaffold-spec.el`,
`config/workspaces/test/home-org-spec.el`,
`config/workspaces/test/data-model-home-spec.el`,
`config/workspaces/test/persistence-v3-spec.el`,
`config/workspaces/test/broken-home-spec.el`. Existing
`workspaces-mode-spec.el` stays; tests for command-level changes
(`workspace-new`, `workspace-delete`, `workspace-purge`) live alongside
the relevant module spec.

**Running:**
- All workspaces tests: `./bin/run-tests.sh -d config/workspaces`
- One spec file: `./bin/run-tests.sh -d config/workspaces -p
  scaffold-spec`
- Snapshot for regression tracking: `./bin/run-tests.sh -d
  config/workspaces --snapshot`

**Naming:**
- File: `<module>-spec.el`
- Top-level: `(describe "module-name" ...)` matching the production
  module name
- Cases: `(it "does X" ...)` in user-facing prose

**Git subprocess strategy: real git, never mocked.** The scaffolder
shells out via `call-process`. Tests run real `git` against tmpdirs
created with `make-temp-file (dir) t`. Rationale:
- The CI already requires git; no new dependency.
- Mocking subprocess I/O is brittle (newline-handling, exit-code paths).
- Real git catches real behavior — including the things you'd miss with a
  mock (commit hooks if any user accidentally adds them, init template
  oddities, etc.).
- Performance: 4 git ops × ~50ms ≈ 200ms per scaffold test. Acceptable
  for the ~10 scaffold-pipeline cases.

Test helper `workspace-test--with-tmp-home`:
```elisp
(defmacro workspace-test--with-tmp-home (binding &rest body)
  "Bind BINDING to a fresh tmp dir, execute BODY, then delete the dir."
  (declare (indent 1))
  `(let ((,(car binding) (make-temp-file "ws-test-" t)))
     (unwind-protect (progn ,@body)
       (delete-directory ,(car binding) t))))
```

**Mocking pattern:** scoped `cl-letf` for the boundary between our code
and outside primitives only (e.g., stub `yes-or-no-p` to return `t` for
purge-confirmation tests; stub `read-directory-name` for prefix-arg
anchoring tests). Never advise globals.

**Buffer reincarnation tests** (Requirement: Buffer reincarnation across
restart) are unchanged; existing coverage in `persistence`-related specs
covers them. Schema-v3 specs only re-verify the `:home` round-trip and
the broken-state handling.

**Scenario → test mapping:** each `#### Scenario:` in
`specs/workspaces/spec.md` maps to one `it` clause in the corresponding
spec file. We aim for 1:1.

### D10. Key bindings

- `C-x w n` — `workspace-new` (existing; gains scaffolding behavior and
  prefix-arg branch).
- `C-x w D` — `workspace-delete` (NEW binding; uppercase D).
- `C-x w P` — `workspace-purge` (NEW; uppercase P).
- `C-x w R` — `workspace-re-anchor` (NEW; uppercase R, only useful when
  a broken-state workspace is selected).

Uppercase deliberately chosen so the lowercase prefix-space stays
available for future buffer-membership commands and avoids confusion
with `kill-buffer`-style verbs.

## Risks / Trade-offs

**[R1] Sessions inventory drift between `home.org` and `sessions/`.**
The spec is explicit that the filesystem wins and `home.org`'s
hand-curated session list is decorative. Users may still be confused
when their `* Sessions` heading and the actual session inventory
disagree. → **Mitigation:** the package never *writes* a `* Sessions`
heading at all (not even in the skeleton). If the user adds one, that's
their choice. Document this in the README.

**[R2] Default-path collision is a hard error.** If a user runs
`workspace-new myproj` twice in a row (the first failed mid-pipeline),
the second invocation fails the existence check and signals the user.
→ **Mitigation:** the error message names the path and the (manual)
remedy. We treat partial-scaffold cleanup as a known support cost vs.
the alternative of silently deleting user data.

**[R3] gptel module loading order.** The integration relies on
`(featurep 'workspaces)` from inside gptel sessions code. If gptel
sessions loads before workspaces, the initial session-creation routing
falls through to global until workspaces is loaded. → **Mitigation:**
this is the desired soft-dependency behavior; module load order in
`init.org` already places workspaces early in initialization, and the
edge case (user creates a session in the brief window between gptel
sessions loading and workspaces loading at startup) is benign.

**[R4] Git availability.** `scaffold.el` requires `git` on `PATH`. If
absent, `workspace-new` fails. → **Mitigation:** explicit error message
("workspace-new requires git; not found on exec-path"). The repo already
requires git for the configuration's own update workflow.

**[R5] Broken-state UX.** A workspace whose directory has been deleted
externally shows up in completion lists but refuses activation. Users
will need to know about `workspace-re-anchor` or `workspace-purge`. →
**Mitigation:** the `*Messages*` notice at load time names the
workspaces and the missing paths and includes the remedy commands. The
broken-state error from `workspace-switch` repeats the remedy.

**[R6] `home.org` is single-write.** If a user wants the package to
re-render their dashboard, there is no built-in command. → **Mitigation:**
this is a deliberate scope choice. A `workspace-rebuild-home-org`
command can land in a follow-up change if user demand emerges.

**[R7] gptel session initial-template coupling.** Cribbing the new-session
file format from `gptel/sessions/` creates a soft coupling: if that
module changes its template, we need to refresh ours. → **Mitigation:**
D7's preferred approach is calling into the gptel module to *generate*
the initial session, not re-implementing the template. The hard-coded
fallback is only for the case where gptel isn't loaded at workspace-new
time.

## Migration Plan

This is a pre-alpha package; no production users; no migration code.

1. Bump `workspace--state-version` from 2 to 3.
2. Reader rejects v2 files with a `*Messages*` notice (same code path
   that v1→v2 used).
3. Users with v2 persistence files on disk: delete the file before
   restarting Emacs into v3 code. The notice tells them where.
4. No code path attempts to read or transform v2 contents.

Rollback: revert the change set. The v2 reader returns; any v3 files on
disk become unreadable (notice + start fresh). Symmetric to forward
migration.

## Open Questions

None that block implementation. Two items worth flagging for review
during apply:

- **Q1:** Where exactly does `gptel-sessions--target-dir` live in the
  current gptel sessions module structure? We'll locate the precise
  function in `config/gptel/sessions/` during apply and wire the
  workspace consult at that point.
- **Q2:** The initial gptel session — single empty file with
  `#+TITLE:`, or do we want it pre-populated with a "welcome" prompt
  pointing back at `home.org`? Default to empty unless someone speaks
  up.
