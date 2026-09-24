---
name: stamp-seam
description: Stamp identity and repo-relative path onto new bookmark records via bookmark-make-record advice, Tier 1 only
change: workspace-portable-bookmarks
status: ready
task_class: feature
on_critical_path: true
cycle: cycle-1789935943
cites_register_entries:
  - register/boundary/bookmark-advice-seams
  - register/shape/portable-bookmark-coordinates
relations:
  - blocked-by:repo-identity
---

## Files to modify
- `config/workspaces/portable-bookmarks.org` (modify)
- `config/workspaces/portable-bookmarks.el` (tangled)
- `config/workspaces/test/portable-bookmarks-stamp-spec.el` (new)

## Implementation steps

1. Add `workspace-bookmark--record-location (record)` returning the record's
   single location, or nil. It must answer "is this even a path?" before treating
   a value as one:
   - read the record's `filename` property via `bookmark-get-filename` /
     `bookmark-prop-get`;
   - return nil unless the value is a string that looks like a path AND names an
     existing file or directory (`file-exists-p` after `expand-file-name`);
   - return nil when the record contains a nested sub-record structure or a
     window tree (Tier 2 territory) — detect by checking for a `buffer` or
     `window` property whose value is not a string.

   The real bookmark file on this machine contains four `tab-bookmark` records
   whose `filename` is the label `"3 buffers"`. Those MUST come back nil.

2. Add `workspace-bookmark--coordinates (location)` returning
   `(identity . relpath)` or nil:
   - repo root for LOCATION (from `repo-identity`); nil → no coordinates;
   - identity for that root; nil (no remote) → no coordinates;
   - relpath via `file-relative-name` of LOCATION against the repo root;
   - reject and return nil if the relpath escapes the root (starts with `..`).

3. Add the stamping function and install it as `:filter-return` advice on
   `bookmark-make-record`:
   - take the record produced by Emacs;
   - if `--record-location` is nil, return the record **unchanged**;
   - otherwise `bookmark-prop-set` the identity and relpath properties and
     return the record;
   - wrap the whole body in `condition-case` so any error returns the original
     record untouched. A failure to stamp must never prevent a bookmark from
     being created.
   - do not prompt, do not message, do not alter the bookmark name.

4. Confirm the absolute `filename` is left in place — it is the fallback that
   every degraded outcome relies on, and it is what keeps the file readable by a
   stock Emacs.

5. Write `portable-bookmarks-stamp-spec.el`:
   - **Behavioral** (real git + real bookmark API, temp `bookmark-default-file`):
     visit a file inside a temp repo with a remote, `bookmark-set`, and assert
     the stored record carries the expected identity and relpath and an unchanged
     absolute `filename`.
   - **Behavioral**: a file in no repository, and a file in a repo with no
     remote, both produce records with no coordinates.
   - **Behavioral round trip**: `bookmark-write-file` then reload with
     `bookmark-load` and assert the coordinates survive — this is the property
     that makes the whole design additive.
   - **Unit**: a synthetic record whose `filename` is the literal string
     `"3 buffers"` is not stamped; a record with a `buffer` property holding a
     list is not stamped; a record whose relpath would escape the root is not
     stamped.
   - **Unit**: force an error inside the coordinates helper with `cl-letf` and
     assert `bookmark-set` still succeeds and the record is unstamped.

6. Tangle and validate.

## Design rationale

A bookmark record is a name plus an *open bag* of properties; `filename` and
`position` are conventions the built-in handler uses, not a schema. Verified in
`emacs -Q --batch`: extra properties can be added, stock Emacs writes them to the
file, and they survive a save/reload round trip with no custom handler required.
That is what makes stamping additive rather than a format change — no migration,
no flag day, and the file still loads in an Emacs without this module.

Tier 1 (one location per record) is the deliberate scope boundary. The real
bookmark file shows why: `tab-bookmark` records store a human label in
`filename`, nest complete bookmark records inside a `buffer` property, embed
window trees, and use `#N=`/`#N#` shared structure that is in places genuinely
circular. Rewriting those naively would corrupt them and a recursive walk would
need cycle handling — deferred to Tier 2, which arrives with portable layouts
where trees are unavoidable anyway.

`:filter-return` on `bookmark-make-record` is the single creation choke point:
every handler's record passes through it, whether the buffer is a file, a
directory, or something a third-party mode defines.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` — stamp spec passes.
- `grep -n "bookmark-make-record" config/workspaces/portable-bookmarks.el`
  shows exactly one `advice-add` and its matching `advice-remove`.
- Manual: `C-x r m` on a file in this worktree, then inspect the record —
  identity `github.com/moogah/emacs`, relpath relative to the worktree root, and
  the original absolute `filename` intact.

## Context
- specs/portable-bookmarks/spec.md § 'Portable coordinates stamped at bookmark creation'
- design.md § D1 'Two advice seams, nothing else'
- design.md § D4 'Tier 1 records only'

## Amendments (2026-09-24, user decisions DD-7, DD-8, DD-9)

These SUPERSEDE the steps above where they conflict. Full text:
`.orchestrator/state.json` → `design_decisions` (DD-7, DD-8, DD-9).

A. **Relpath comes from git, not string arithmetic (DD-8).** Step 2's
   `file-relative-name` + "reject if it starts with `..`" is replaced by asking
   git: `git ls-files --full-name` for the location. A location outside the repo
   yields nothing, so the `..` guard is no longer needed; drop it rather than keep
   it as belt-and-braces. Step 5's unit case "a record whose relpath would escape
   the root is not stamped" becomes "a location git does not report is not
   stamped".

B. **Untracked locations are not stamped (DD-9 / OD-1).** When git reports no
   tracked path for the location, the record is returned unchanged:
   non-portable, absolute `filename`, exactly today's behaviour. No fallback
   path computation is introduced. Add a behavioral spec: bookmark an untracked
   file inside a repo with a remote → record carries no coordinates.

C. **Property names are `defconst`, not `defcustom` (DD-9 / OD-2).** Convert
   `workspace-bookmark-repo-property` and `workspace-bookmark-relpath-property`
   to `defconst`. They are an on-disk format, not a preference. Pin 2 (named in
   one place) still holds.

D. **Stamp-format version property (DD-9 / OD-3).** Add a third stamped
   property, named by a `defconst` alongside the other two (e.g.
   `workspace-bookmark-format-property` → `workspace-bookmark-format`), whose
   value is the integer format version, `1`, itself held in a `defconst`.
   "Both or neither" becomes **all three or none**: stamping writes all three
   or none. Extend the behavioral stamp and round-trip specs to assert the
   version survives `bookmark-write-file` / `bookmark-load`.

E. **Memoise repo identity per repository root (DD-7).** Stamping runs on
   `bookmark-make-record`, which workspaces' own layout save calls for EVERY
   live buffer on EVERY save, including the idle-tick autosave. Identity must
   be computed at most once per repository root per session; only the
   per-location git calls may repeat. Add a spec showing a second stamp in the
   same repo does not re-query the remote.

F. **Workspaces' layout records get stamped too, deliberately.** `layouts.el`
   calls `bookmark-make-record` for every buffer. Those records are stamped the
   same as a user's bookmarks. Do not try to detect or exclude the caller
   (user decision on ask-cycle-1789935943-4).
