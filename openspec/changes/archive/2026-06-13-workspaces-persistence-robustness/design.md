# Design — workspaces-persistence-robustness

## Context

Persistence/restore lives in `config/workspaces/persistence.el` (tangled
from `.org`); window-state capture/restore + the `workspace-buffer`
reincarnation struct live in `config/workspaces/layouts.el`. The
data-model + struct defs live in `config/workspaces/data-model.el`.

The corruption was a `#<killed buffer>` inside a `window-preserved-size`
window parameter, written to `workspaces.eld` and then unreadable on the
next startup, after which an autosave overwrote the file with an empty
registry. Full research (verified against Emacs source + peer packages)
is in `research/` — `findings-serialization-and-corruption-safety.md` and
`findings-activities-persistence-deep-dive.md`. Key verified facts the
design rests on:

- `window--state-get-1` (window.el) emits a `writable`-marked parameter's
  value **verbatim** — no `bufferp`/`readablep` guard, no recursion into
  the value. The `writable` flag does not make
  `window-preserved-size`'s `(BUFFER W H)` readable.
- **`frameset` is the wrong altitude** — `frameset-filter-alist` filters
  *frame* parameters only; window parameters flow through a separate
  `window-state-get`. So adopting frameset would not fix this.
- **`burly.el` and `activities.el` both solve it the same way**: a
  per-parameter `serialize`/`deserialize` translator table run over the
  window-state leaf tree. `window-preserved-size`: buffer → `buffer-name`
  on serialize, name → `get-buffer` on deserialize.
- **`bookmark.el` has no write-side guard** against unreadable objects;
  burly added a `bufferp`→nil scrub (bug#56643). We have a restore-side
  workaround only.
- **`persist.el` is not safer**: `write-region` direct (no atomic rename),
  no backups, `persist-load` does `read` with no error handling, and
  `persist-save` *deletes* the file when in-memory state equals the
  default. `activities.el`'s `with-demoted-errors`-then-save has the
  identical clobber-on-corrupt bug we do. → keep our own writer, harden it.

## Goals / Non-Goals

**Goals:**
- No live Emacs object is ever written to `workspaces.eld` (readable by
  construction; verified by a write-time assertion).
- `window-preserved-size` round-trips faithfully across restart
  (translated, not dropped) — D1.
- A present-but-unreadable file is preserved (backed up) and never
  overwritten by an autosave of empty/partial state — D2.
- Comprehensive round-trip + corruption-injection test coverage.

**Non-Goals:**
- No v3 schema change; no migration tooling.
- No change to autosave *triggers* (only their gating when persistence is
  blocked).
- Not adopting `persist.el` or `frameset` wholesale.
- No change to `workspace-switch`/`-restore`/`-save` user semantics
  beyond the corruption-safety guarantees.

## Decisions

### D1 — Window-parameter translators + readable-before-write assert (confirmed)

`config/workspaces/layouts.el`:

1. Add `workspace-window-parameter-translators` — an alist
   `(PARAM . ((serialize . FN) (deserialize . FN)))`, modelled on
   `activities-window-parameters-translators` / `burly-window-parameters-translators`:

   ```elisp
   (defvar workspace-window-parameter-translators
     `((window-preserved-size
        (serialize . ,(pcase-lambda (`(,buffer ,dir ,size))
                        (list (and (bufferp buffer) (buffer-name buffer)) dir size)))
        (deserialize . ,(pcase-lambda (`(,name ,dir ,size))
                          (list (and name (get-buffer name)) dir size))))))
   ```

2. In `workspace--window-state-serialize` (already walks leaves and
   touches each leaf's `parameters` map), after setting the
   `workspace-buffer` struct, loop the translators: for each `(param
   . translators)` present in `parameters`, replace its value with
   `(funcall serialize value)`. In `workspace--window-state-deserialize`,
   apply the `deserialize` fn (wrapped in `condition-case-unless-debug`,
   dropping the parameter on failure — mirrors activities' restore
   robustness).

3. **Bookmark-record scrub** in `workspace--serialize-buffer`: after
   `(bookmark-make-record)`, walk the record's prop alist and replace any
   value failing a readability test (a `bufferp`, `markerp`, `overlayp`,
   `framep`, `windowp`, or non-symbol `functionp`) with nil. Keep it a
   single shared predicate, e.g. `workspace--unreadable-object-p`, reused
   by the write-assert.

4. **Readable-before-write assert** — see D2 step 2 (lives at the write
   boundary in persistence.el but uses the shared predicate / a
   `prin1`→`read` round-trip).

*Alternative considered (rejected):* drop `window-preserved-size` from
the persistent set. Simpler but loses preserved-size-across-restart; the
translator is the faithful fix and is cheap. *Alternative (rejected):*
adopt `frameset` — wrong altitude (frame params only).

### D2 — Corruption-safe persistence I/O (full hardening, confirmed)

`config/workspaces/persistence.el`:

1. **Atomic write** — `workspace--write-state` writes to a temp file in
   `(workspace--state-directory)` (same filesystem) then `rename-file`
   over the target with OK-IF-ALREADY-EXISTS. Create dir first.

2. **Write-time readable assert** — before the rename, round-trip the
   serialized text: `(condition-case _ (read (prin1-to-string form)) …)`
   (or `readablep` on Emacs ≥28 — check repo min version; fall back to
   round-trip). If it throws `invalid-read-syntax` / contains an
   unreadable object, **abort the write**, leave the prior good file
   intact, and `display-warning` loudly. With D1 in place this is a
   backstop that should never fire.

3. **Backup-on-corrupt read + absent-vs-unreadable signal** —
   `workspace--read-state`: keep the "missing file → nil" path, but on a
   `read`/parse error of an *existing* file, (a) `rename-file` it to
   `workspaces.eld.corrupt-<ts>` (ts from `format-time-string`), (b)
   `display-warning`, (c) set `workspace--persistence-blocked` to `t`,
   and (d) return a distinct sentinel (e.g. the symbol
   `workspace--unreadable`) — NOT bare nil. `workspace--restore` treats
   the sentinel as "do nothing, stay blocked" (registry empty but no
   save permitted) and bare-nil as "fresh start" (save permitted).

4. **Autosave gate** — add `workspace--persistence-blocked` (defvar,
   nil). Every writer short-circuits with a one-time warning while it is
   set: `workspace--write-state` (the choke point — guard here covers
   `workspace--flush-state` and `workspace-save-state` which route
   through it) and explicitly `workspace--kill-emacs-flush`. The flag is
   session-scoped; it clears only by a clean load (e.g. the user removes/
   fixes the file and reloads) — we do not auto-clear within a session,
   because the in-memory registry after a failed load is untrustworthy.

*Alternative considered (rejected):* thread a sentinel return through
every caller instead of a module flag. The flag gates all four writers
with far fewer call-site changes and directly protects the
`kill-emacs-hook` path. We still use a read-side sentinel for the
absent-vs-unreadable *decision*, but the flag is the write-side guard.

### D3 — Shared readability predicate

One helper, `workspace--unreadable-object-p` (data-model.el or layouts.el),
returns non-nil for objects `read` cannot reconstruct (buffer, marker,
overlay, frame, window, process, hash-table?, non-symbol subr/closure).
Used by the bookmark scrub (D1.3) and conceptually by the write-assert
(D2.2 uses the cheaper round-trip but may early-scan with this). Keeping
one predicate avoids divergent notions of "unreadable."

## Risks / Trade-offs

- **[Test blast radius]** Specs asserting the old read-failure → nil →
  (over)write behavior, or the untranslated `window-preserved-size`
  shape, must change: `persistence-spec`, `persistence-v3-spec`,
  `layouts-spec`, `buffer-reincarnation-spec`. Mitigation: enumerate per
  task; full `config/workspaces` suite is the gate.
- **[Write-assert cost]** A `prin1`→`read` round-trip doubles
  serialization cost. Acceptable: saves are debounced/human-paced. Use
  `readablep` if the min Emacs version allows (cheaper).
- **[Flag stuck on]** If `workspace--persistence-blocked` is set, the
  session won't autosave until reload. This is intentional (better than
  clobbering) but must be surfaced loudly (warning + the `.corrupt-<ts>`
  path) so the user knows to act. Document in the warning text.
- **[Translator completeness]** D1 is a whitelist; an unknown future
  buffer-bearing parameter would slip past the translators — which is
  exactly why D2.2's assert exists as the backstop (abort, don't corrupt).

## Migration Plan

Pure code change; v3 format unchanged. Existing readable `.eld` files
load unchanged. A pre-existing corrupt file now gets backed up to
`*.corrupt-<ts>` on first load instead of being silently overwritten.
Tangle `layouts.org` + `persistence.org` (and `data-model.org` if the
predicate/flag lands there); run `./bin/run-tests.sh -d config/workspaces`.

## Open Questions

None blocking. `readablep`-vs-round-trip is settled by the repo's min
Emacs version (the implementor checks; round-trip is the safe fallback).
