---
name: home-org-reader-module
description: New home-org.org module with regex-based #+TITLE: reader (no org-mode parser)
change: add-workspace-home-directory
status: done
relations: []
---

## Files to modify
- `config/workspaces/home-org.org` (new — literate source)
- `config/workspaces/home-org.el` (new — tangled)
- `config/workspaces/test/home-org-spec.el` (new — Buttercup)
- `init.org` (modify — register `workspaces/home-org` in `jf/enabled-modules`)

## Implementation steps

1. Create `config/workspaces/home-org.org` with the standard literate
   header:

   ```org
   #+title: Workspaces — home.org Reader
   #+author: Jeff Farr
   #+property: header-args:emacs-lisp :tangle home-org.el
   #+auto_tangle: y
   ```

2. Add a top-level lexical-binding declaration:

   ```elisp
   ;;; home-org.el --- Workspaces home.org reader -*- lexical-binding: t; -*-
   ```

3. Define the file-path helper:

   ```elisp
   (defun workspace-home-org-path (home)
     "Return the absolute path to `home.org' inside HOME directory."
     (expand-file-name "home.org" home))
   ```

4. Define the title reader using regex (NOT `org-collect-keywords` —
   this avoids loading `org-mode` and is ~10× faster per design D4):

   ```elisp
   (defun workspace-home-org-title (home)
     "Return the trimmed #+TITLE: keyword value from HOME/home.org, or nil.
   Returns nil if the file is missing, unreadable, or has no #+TITLE:
   keyword. Live read on every call — no caching."
     (let ((path (workspace-home-org-path home)))
       (when (file-readable-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.*\\)$" nil t)
             (let ((title (string-trim (match-string 1))))
               (and (not (string-empty-p title)) title)))))))
   ```

5. Define a tiny existence helper used by anchoring logic:

   ```elisp
   (defun workspace-home-org-exists-p (home)
     "Return non-nil if HOME contains a `home.org' file."
     (file-readable-p (workspace-home-org-path home)))
   ```

6. End with `(provide 'workspace-home-org)`.

7. Create `test/home-org-spec.el` with cases (use a temp-dir helper —
   the same `workspace-test--with-tmp-home` pattern other tasks
   establish; if not yet present, inline a let-bound `make-temp-file
   (dir) t` and an `unwind-protect` `delete-directory`):

   - `workspace-home-org-title` on a missing file returns nil.
   - On a file with no `#+TITLE:` returns nil.
   - On a file with `#+TITLE: Foo` returns `"Foo"`.
   - On `#+TITLE:   spaces around   ` returns trimmed `"spaces around"`.
   - On `#+title: Lowercase` returns `"Lowercase"` (case-insensitive
     regex — verify your regex actually matches; if it does not by
     default, switch to a `case-fold-search` `let`).
   - On `#+TITLE:` (empty) returns nil.
   - On a file where `#+TITLE:` appears NOT on the first line (e.g.,
     comments above) the title is still found.
   - `workspace-home-org-exists-p` returns nil for nonexistent file, t
     for existing.

8. Register the module: add `"workspaces/home-org"` to
   `jf/enabled-modules` in `init.org` (place it after `"workspaces"` /
   before `"workspaces-mode"`).

9. Tangle and validate:
   ```bash
   ./bin/tangle-org.sh config/workspaces/home-org.org
   ```

## Design rationale

`home.org` is read lazily on every query (no caching) per the spec's
"live read" requirement. The regex approach (D4 in design.md) is chosen
over `org-collect-keywords` because:

- Reads happen during `completing-read` prompts and mode-line refreshes
  — they are user-perceptible.
- We only need one keyword (`#+TITLE:`), not a full parser.
- Loading `org-mode` for the buffer is heavy and pulls in transitive
  initialization (`org-load-hook`, etc.).
- Regex on a temp buffer is order-of-magnitude faster and has no side
  effects.

The module is intentionally kept separate from `data-model.org` (which
is the side-effect-free layer) because it does file I/O. Other modules
that need to display the workspace's name read `(or
(workspace-home-org-title home) basename)`.

## Design pattern

`insert-file-contents` into `with-temp-buffer` is the standard
side-effect-free file read in Emacs Lisp. Pattern:

```elisp
(with-temp-buffer
  (insert-file-contents path)
  ;; ...scan...
  )
```

`file-readable-p` before reading: belt-and-suspenders for the empty/
missing case. Returning nil on any read failure (not signalling) lets
callers do `(or title fallback)` cleanly.

## Verification

- Tangle: `./bin/tangle-org.sh config/workspaces/home-org.org`
- Spec passes: `./bin/run-tests.sh -d config/workspaces -p home-org-spec`
- Module loads cleanly on Emacs startup (no errors in `*Messages*`
  after `./bin/emacs-isolated.sh -nw` and `M-x message-buffer`).

## Context

design.md § Decisions / D4 — home.org reader: regex scan, no `org-mode` activation
specs/workspaces/spec.md § ADDED "home.org is user-authored after creation"
specs/workspaces/spec.md § ADDED "Required home directory and identity coupling" (scenarios on #+TITLE: override)

## Observations

- Task body step 8 instructs registering `"workspaces/home-org"` in
  `jf/enabled-modules` in `init.org`, but the existing workspaces
  subsystem (data-model, tabs, buffer-membership, layouts, persistence,
  workspaces-mode) does not use that pattern — those submodules are
  loaded from inside `config/workspaces/workspaces.org`'s "Submodules"
  section via `jf/load-module`, and only the top-level `workspaces/
  workspaces` appears in `init.org`. Following the established
  convention, I registered `home-org` in `workspaces.org`'s submodule
  load list (after `data-model.el`, before `tabs.el`) rather than in
  `init.org`. The task body's referenced anchor `"workspaces-mode"`
  doesn't exist in `init.org` either, which is further evidence the
  step was written without checking the actual current loader shape.
  Outcome: zero changes to `init.org`; one new line in
  `workspaces.org`. Recording as a deviation; the loader contract is
  satisfied either way (the module loads at startup).

- The task body's step-7 enumeration of test cases includes "On
  `#+TITLE:` (empty) returns nil." This case is ambiguous: the regex
  pattern `^#\+TITLE:[ \t]+\(.*\)$` requires at least one space/tab
  AFTER the colon, so a bare `#+TITLE:` with no trailing whitespace
  short-circuits at stage 4 (no regex match), not stage 5
  (trim-and-emptiness). I wrote the spec case for `#+TITLE:    \n`
  (trailing whitespace only) to exercise the stage-5 short-circuit
  explicitly, and documented the distinction in a comment. The
  alternative — relaxing the regex to `[ \t]*` — would change the
  observable behavior (now an unwritten title silently falls back via
  stage 4 vs. stage 5), so I kept the speculated regex unchanged. No
  behavioral difference at the boundary (both return nil).

- The scaffolded invariant `home-org-user-authored-after-creation.el`
  cycle-1 assertion (the 2nd `it`) is implemented in spirit inside the
  new spec file as "Invariant: reader module is read-only". I did NOT
  copy/migrate the scaffold file itself into `config/workspaces/test/`
  — the scaffold-file-with-shared-helpers shape doesn't match the
  per-spec-file convention in this test tree. The narrower assertion
  is in `home-org-spec.el`; the broader lint (1st `it`, applies to
  the full workspaces module set) and the cycle-2 byte-for-byte
  snapshot (3rd `it`) remain in the scaffolding directory for their
  owning cycles to pick up.

- The reader-module read-only assertion uses a literal-token grep
  scoped to function-call form (`(write-region\_>`, etc.) to avoid
  false matches against the strings inside docstrings (the org file's
  prose mentions the names of write primitives by way of explaining
  the invariant). The tangled `.el` strips org comments, so the only
  way `write-region` could appear in the tangled file is if a future
  edit actually adds it — exactly the failure mode the lint catches.

## Discoveries

- discovery_id: disc-home-org-1
  class: vocabulary-mismatch
  description: |
    The task body refers to wiring the new module via
    `jf/enabled-modules` in `init.org`, but the workspaces subsystem
    uses a top-level loader (`workspaces.org`) that owns its own
    submodule load order via embedded `jf/load-module` calls. The
    project implementor overlay (`.claude/orchestrator/roles/
    implementor.md` § "Module loading") describes the general
    `jf/enabled-modules` pattern, but the workspaces subsystem is a
    structural exception that pre-dates this change. Future task
    bodies for cycle-2+ workspaces modules (scaffold, etc.) should
    state "register in `workspaces.org`'s Submodules section" rather
    than "register in `jf/enabled-modules`" to avoid the misdirection.
  affected_register_entry: register/boundary/home-org-read-pipeline
  recommendation: |
    Integrate phase: when registering this entry as reconciled, add a
    note to the entry's `producers:` block (or a sibling note) that
    the load wiring goes through `workspaces.org`'s submodule loader,
    not `init.org`'s `jf/enabled-modules`. Equally, update the
    cycle-2 scaffold-module task body (when authored) to reference
    the correct wiring path.

- discovery_id: disc-home-org-2
  class: spec-signal
  description: |
    The task-body spec case "On `#+TITLE:` (empty) returns nil" does
    not distinguish between two structurally-different stage
    short-circuits: stage 4 (regex no-match because the regex
    requires `[ \t]+` after the colon) vs. stage 5 (regex matches
    but trimmed value is empty). The speculated regex in the
    register entry pins `[ \t]+`, so a bare `#+TITLE:` line is a
    stage-4 case, not stage-5. I split this into two spec cases:
    `#+TITLE:    \n` (trailing whitespace only) for stage 5, and the
    existing "no #+TITLE: keyword" case covers stage 4. The bare
    `#+TITLE:` form remains untested — it would also return nil but
    via a path that's not interesting to pin behaviorally.
  affected_register_entry: register/boundary/home-org-read-pipeline
  recommendation: |
    No register change required — the staged short-circuit policies
    already distinguish the two paths correctly. But the next
    architect-audit pass could tighten the register entry's stage-5
    notes by adding an explicit example string ("e.g. `#+TITLE:    `
    → stage 5") to disambiguate for future implementors.
