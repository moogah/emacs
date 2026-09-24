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

## Observations

Pressure-test items from the brief, each settled by a spec or a recorded probe
(git 2.49.0, GNU Emacs 30.2, macOS/APFS):

- **Directory bookmarks (item 1).** `git ls-files -- DIR` lists the files under
  DIR, which is the wrong answer for DIR's own path. Directories take their
  relpath from `git rev-parse --show-prefix` run inside them (`"lisp/"`, with
  the trailing slash git prints). "Tracked" for a directory means the same as
  for a file in spirit: `ls-files` reports **at least one** file under it. An
  empty or wholly untracked directory is unstamped, and so is `.git/` itself,
  because `rev-parse --show-toplevel` fails inside it. Specs: "stamps a tracked
  subdirectory with git's prefix", "leaves a directory holding only untracked
  files unstamped", "leaves the .git directory unstamped". Probe: under
  `emacs -Q` a Dired buffer's `bookmark-make-record` is the default record (the
  directory with a trailing slash in `filename`, and no handler), so the gate
  needs no Dired-specific code. Cost: `ls-files` has no way to stop after the
  first match, so at the repo root it prints the whole index. Measured at 14 ms
  / 95 KB for this 1390-file repo. The cost is bounded by the subtree and the
  output is thrown away, but it grows with repo size.
- **Repository root (item 2).** Stored as `"./"`, not `""`. Reasons: (a) every
  emptiness check (`string-empty-p`, `seq-empty-p`) reads `""` as missing,
  which would silently break all-three-or-none; (b) every directory relpath
  then ends in `/`, so a relpath's spelling tells you whether it is a file or
  a directory; (c) it re-anchors like any other relpath:
  `(expand-file-name "./" root)` gives `root`, with its slash. The spec
  asserts this. The runner's empty-to-nil collapse is avoided by asking
  `--show-toplevel --git-common-dir --show-prefix` in ONE `rev-parse`. At the
  root the trimmed output has two lines instead of three, and two lines is
  parsed as an empty prefix.
- **Unusual filenames (item 3).** Measured: without `-z`, git prints
  `"dir with spaces/caf\303\251 \"q\" \\b.txt"`, C-quoted with the quotes
  included. `-z` prints the raw bytes. A second hazard the brief did not name:
  **pathspecs are globs by default**. In a repo tracking both `glob[1].txt` and
  `glob1.txt`, `git ls-files -- glob[1].txt` returned BOTH, so the file call
  uses `--literal-pathspecs`. A file named `-v` needs `--`. Output and args are
  forced to UTF-8 (`coding-system-for-read/write`), because a GUI Emacs can
  start with a non-UTF-8 `default-process-coding-system`. A spec binds it to
  latin-1 and still gets `naïve/résumé.txt`. Mutation-tested: removing `-z`,
  `--literal-pathspecs` or the coding binding each fails the matching spec.
- **Basename vs absolute (item 4).** Probed both: with git 2.49 an absolute
  `/var/folders/...` name (the symlinked spelling) and the basename both return
  `s/b.txt`, and so does `/tmp/...` → `/private/tmp`. An absolute name outside
  the repo is `fatal: ... outside repository` (exit 128), which the runner
  turns into nil. I chose the basename because the runner has already put
  `default-directory` in the file's directory and it depends on nothing else.
  Spec "stamps a file reached through a symlinked, unresolved repository path"
  covers visiting through a symlink.
- **Subprocess count (item 5, DD-7).** These counts are asserted with spies on
  `workspace-bookmark--git`:
  - 0 calls for anything the gate rejects (no `filename`, the `"3 buffers"`
    label, nested `buffer`/`window`, TRAMP). The TRAMP spec also asserts that
    `file-exists-p` is never called.
  - 1 call for a file in no repo.
  - 2 calls for a tracked file or directory once identity is memoised:
    `rev-parse` (root, common dir and prefix in one call) plus `ls-files`.
  - 2 calls for an untracked file. It never reaches the remote lookup.
  - The first stamp in a repo adds the 1–3 `git remote` calls.

  **Memo invalidation: yes, it needs one, and it costs no subprocess.** Each
  entry carries the mtime of the repo's shared `config`. Its path comes from
  `--git-common-dir` in the same `rev-parse`, so the check is one `stat`. That
  covers `git remote add/set-url/remove`. Without it, re-pointing `origin`
  mid-session would keep stamping the OLD identity into new persisted records
  (spec "re-reads identity after origin is re-pointed mid-session"). A nil
  identity (no remote) is memoised too, because it is the common case (9 of 51
  repos) and would otherwise re-query on every autosave. The memo does NOT see
  `url.*.insteadOf` changes in a global or included gitconfig; the fix for
  that is `M-x workspace-bookmark-clear-identity-cache`.
- **Hot-path cost is still real (worth a look before jump-seam ships).**
  Measured in batch Emacs with the identity already memoised: about 18.5 ms
  per stamped file record on an idle machine (about 21 ms with the full suite
  running at the same time), i.e. two git process spawns.
  Amendment F means `layouts.el` calls this for every live buffer on every
  save, including the idle autosave, so a session with 30 file buffers pays
  about 0.55 s of blocking git per autosave. DD-7's budget is met ("only the
  per-location git calls may repeat"), but the budget itself may be too
  generous. A per-location memo keyed on the index mtime would bring the
  steady state to zero calls. Not done here, because it is outside the
  amendments.
- **Departure: a fresh record instead of `bookmark-prop-set` (step 3).** The
  stamped record is built as `(NAME . original-props ++ three-new-props)`, and
  the original is not mutated. Two reasons: all-three-or-none holds by
  construction, because every fallible step finishes before anything is
  built; and the error arm really returns the ORIGINAL, unmutated record. It
  also never writes through a record a handler shared or returned as a
  literal. Stale copies of the three properties are dropped before appending,
  so a property can never appear twice. Specs: "does not modify the record it
  was given", "returns the original record, unmutated, when building the
  stamp signals", "replaces, rather than duplicates, stale stamped
  properties".
- **Addition: a re-anchor check, not a `..` guard.** Amendment A says to drop
  the `..` guard, and it is gone; the relpath is taken from git verbatim. What
  I added instead: the relpath git reports is accepted only if
  `(file-equal-p (expand-file-name relpath root) location)`. The reason is
  that the shared runner `string-trim`s its output, so a filename with a
  leading or trailing blank could come back altered. This check turns that,
  or any git answer I misparsed, into "unstamped" rather than a persisted
  wrong path. `file-equal-p` compares files rather than strings, so `/var` vs
  `/private/var` and symlinks are handled. Mutation-tested.
- **The runner's `string-trim` is a latent hazard for other callers.** Any
  future caller that parses PATHS out of `workspace-bookmark--git` (for
  example checkout-discovery's `worktree list --porcelain`) loses leading and
  trailing whitespace, and with a single-line answer it loses a trailing
  newline-significant empty line. The runner is not mine to change (and is
  shared), so I worked around it here.
- The memo lives beside `--identity`, not inside it. `--identity` is now
  `repo-root` + `--root-identity` and is still unmemoised, so the identity
  specs' mocks and checkout-discovery's callers see no behaviour change. The
  resolver may want `--memoised-identity` too. It needs a root and a git
  common dir, which `--checkout-position` gives in one call.
- `bookmark-make-record` puts `(last-modified . <current-time>)` into every
  record, so two calls in a row never compare `equal`. The additive spec
  compares the two records with `last-modified` removed.
- The `workspace-bookmark` defgroup now has no members, because the only two
  defcustoms became defconsts. I left it alone: `* Custom Group` is outside
  my ownership, and a later option may use it.
- `condition-case` catches `error`, not `quit`. A `C-g` during the git calls
  still aborts bookmark creation, which is the user's intent.
- Not covered by a spec: a case-insensitive mismatch between the visited name
  and git's (`readme.md` vs a tracked `README.md`). `file-equal-p` would accept
  git's spelling; whether `ls-files` matches depends on `core.ignorecase`.
- A tracked symlink inside the repo that points elsewhere is stamped with the
  LINK's relpath, because `file-equal-p` resolves both sides. Re-anchoring
  then follows the link in the other checkout. I think this is correct, but it
  has no spec.
- Amendment F (layout records stamped): no code detects the caller. There is
  no layouts-level spec, because `layouts.el` is outside this task's files.
  `workspace--scrub-bookmark-record` mutates the record it gets in place, and
  that is now the fresh stamped record, which is harmless.
- The "Manual" verification step (`C-x r m` on a real file) was skipped as the
  brief directs; it is the user's step and would touch the real bookmark
  file.

## Discoveries

- discovery_id: disc-stamp-seam-1
  class: interface-drift
  description: |
    The persisted shape has THREE stamped fields, not two. The third,
    `workspace-bookmark-format`, holds the integer format version (1, from
    `workspace-bookmark-format-version`). Pin 3 is now "all three or none".
    Stamping builds a fresh record, so a partial stamp cannot be produced.
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Add field `<workspace-bookmark-format> : integer, required` and rewrite
    pin 3 as ALL THREE OR NONE. The resolver must treat an unknown version,
    or any subset of the three, as non-portable. Update the title ("the three
    properties") and validator 3 (the round trip asserts all three survive).

- discovery_id: disc-stamp-seam-2
  class: interface-drift
  description: |
    The property names are `defconst`, not defvar/defcustom (DD-9 / OD-2):
    `workspace-bookmark-repo-property`, `-relpath-property`,
    `-format-property`, plus `workspace-bookmark-format-version`. Validator 1
    still says "the defvar/defcustom itself".
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Pin 2 and validator 1: "defined by defconst". Add the check that
    `(custom-variable-p VAR)` is nil for all four (asserted in
    portable-bookmarks-spec.el).

- discovery_id: disc-stamp-seam-3
  class: interface-drift
  description: |
    The relpath field's description is out of date. It is not
    `file-relative-name` against the repo root, and there is no `..`
    rejection. Files: `git --literal-pathspecs ls-files -z --full-name --
    BASENAME`, run in the file's directory. Directories:
    `rev-parse --show-prefix` inside them (trailing `/`), and only when
    `ls-files` reports at least one file under them. Repository root: "./".
    UNTRACKED locations are unstamped. Every relpath must re-anchor
    (`file-equal-p`) to the location, or the record is unstamped.
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Rewrite the relpath field: "git's repo-relative spelling; directories end
    in '/'; the root is './'; re-anchor with (expand-file-name RELPATH
    CHECKOUT-ROOT)". Drop the `..` sentence. Add "untracked -> unstamped" to
    tier_1_boundary.

- discovery_id: disc-stamp-seam-4
  class: deviation
  description: |
    The entry's canonical_constructor note says "The stamp advice writes it
    with bookmark-prop-set". It does not. The seam returns a NEW record
    (name + original props in order + the three stamped props, with stale
    copies removed) and never mutates the record Emacs handed it. An
    unstamped record is still returned `eq`.
    `workspace-bookmark--coordinates` is still the sole producer of
    (identity . relpath).
  affected_register_entry: register/shape/portable-bookmark-coordinates
  recommendation: |
    Replace "writes it with bookmark-prop-set" with "builds a fresh stamped
    record (workspace-bookmark--with-coordinates); the input record is never
    mutated". This also strengthens validator 2 (no prop-set on 'filename).

- discovery_id: disc-stamp-seam-5
  class: spec-signal
  description: |
    Seam 1 now has a measured subprocess budget, and the entry should carry it
    as part of the seam's contract. 0 git calls for gate-rejected records
    (non-file, labels, nested/window, TRAMP), 1 for a file in no repo, 2 for
    a tracked file or directory once identity is memoised, and 2 for an
    untracked one (no identity lookup). Validator 3 (a condition-case
    returning the original) holds for the seam-1 body. Measured cost is about
    18.5 ms per stamped record, and layouts call the seam for every buffer on
    every autosave (Amendment F).
  affected_register_entry: register/boundary/bookmark-advice-seams
  recommendation: |
    Add a `cost_contract` to seam 1 with the budget above. Consider
    escalating whether the per-autosave cost (about 2 spawns per file buffer)
    is acceptable, or whether a per-location memo keyed on the index mtime is
    wanted.

- discovery_id: disc-stamp-seam-6
  class: spec-signal
  description: |
    Repo identity is memoised per checkout root for the stamp path
    (`workspace-bookmark--memoised-identity`, `workspace-bookmark--identity-memo`,
    `M-x workspace-bookmark-clear-identity-cache`). Each entry is validated
    against the mtime of the shared git `config` (path from
    `rev-parse --git-common-dir`), and nil identities are memoised.
    `workspace-bookmark--identity` itself is unmemoised and was split into
    `--repo-root` + `--root-identity`.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Record the memo and its invalidation rule (config mtime; global
    insteadOf is not seen, so there is a clear command) wherever the entry
    describes identity lookup, so the resolver reuses it rather than adding a
    second cache.

- discovery_id: disc-stamp-seam-7
  class: invariant-gap
  description: |
    `workspace-bookmark--git` string-trims stdout. That is safe for URLs and
    refs but not for paths: leading or trailing blanks in a path are lost,
    and a trailing empty line (git's empty `--show-prefix`) disappears. The
    stamp path defends against this (the relpath must re-anchor to the same
    file; two-vs-three-line parse). Other path-parsing callers have no such
    defence.
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    Add to the subprocess_contract: "output is trimmed; callers parsing paths
    must use -z and verify, or the runner grows an untrimmed mode".
