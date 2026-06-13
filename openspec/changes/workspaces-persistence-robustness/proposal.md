## Why

A workspace persistence file (`workspaces.eld`) was found to contain an
unreadable Emacs object — `#<killed buffer>` — embedded in a
`window-preserved-size` window parameter. The cascade that followed is a
**silent data-loss bug**:

1. The layout serializer captures window-state via
   `window-state-get … 'writable`. The `writable` flag selects which
   window parameters to include but does **not** sanitize their *values*;
   `window-preserved-size`'s value is `(BUFFER WIDTH HEIGHT)` — a raw
   buffer object. Once that buffer is killed it prints as
   `#<killed buffer>`, which `read` cannot parse.
2. On startup `workspace--read-state` calls `read`, which throws
   `invalid-read-syntax`; the `condition-case` swallows it and returns
   `nil` — **indistinguishable from "no file exists."**
3. The registry hydrates empty; `workspace-restore` reports "No saved
   workspaces."
4. An autosave/idle/`kill-emacs` flush then serializes the **empty
   registry over the file**, destroying the user's data:
   `(:version 3 :workspaces nil)`.

Research into Emacs core (`window-state-get`, `frameset`/`desktop.el`,
`bookmark.el`) and peer packages (`activities.el`, `burly.el`) shows two
root problems, both of which we were naive about:

- **Serialization is not guaranteed readable.** We copied
  `activities.el`'s struct-based buffer reincarnation but **not** its
  window-parameter *translators*. `activities.el` and `burly.el` both
  translate `window-preserved-size`'s buffer object to a buffer-name
  string before persisting (and back on restore). We mark the parameter
  `writable` and write the raw object.
- **Persistence is not corruption-safe.** `workspace--write-state` writes
  directly to the live file (no atomic rename, no backup), and a failed
  read is treated as "absent" and then overwritten. `activities.el` /
  `persist.el` share this clobber-on-corrupt failure mode; it is not a
  pattern to copy.

## What Changes

Two coherent layers (decisions D1–D2 confirmed with the user):

- **Layer A — guarantee readable-before-write** (`config/workspaces/layouts.el`):
  - Add a `workspace-window-parameter-translators` alist with
    `serialize` / `deserialize` functions per buffer/marker-bearing
    window parameter, starting with `window-preserved-size`
    (buffer → `buffer-name` on serialize; name → `get-buffer` on
    deserialize). Run the translators inside the existing leaf-walkers
    (`workspace--window-state-serialize` / `-deserialize`).
  - Scrub the `bookmark-make-record` result in
    `workspace--serialize-buffer` of any unreadable value (buffers,
    markers, non-symbol function objects) — closing the second source of
    the same class (bug#56643: a major mode embedding an unreadable
    object in its bookmark record).
  - Add a **readable-before-write assertion** as a safety net: a
    `prin1`→`read` round-trip (or `readablep`) check that aborts the
    write — preserving the prior good file — if any unknown future leak
    produces an unreadable form. With the translators in place this
    should never fire; it is the backstop, not the primary fix.

- **Layer B — corruption-safe persistence I/O** (`config/workspaces/persistence.el`):
  - **Atomic write**: serialize to a temp file in the same directory and
    `rename-file` over the target, so a crash mid-write can never
    truncate the live file.
  - **Never clobber an unreadable file**: on `read`/parse failure of an
    *existing* file, rename it to `workspaces.eld.corrupt-<timestamp>`,
    warn loudly, and set a session `workspace--persistence-blocked` flag.
  - **Distinguish absent from unreadable**: `workspace--read-state`
    returns a distinct signal for "present-but-unreadable" so the caller
    no longer treats it as "fresh start."
  - **Autosave gate**: every writer (`workspace--write-state`,
    `workspace--flush-state`, `workspace-save-state`,
    `workspace--kill-emacs-flush`) no-ops with a one-time warning while
    `workspace--persistence-blocked` is set — this is what stops the
    empty-registry clobber.

We **keep our own `.eld` writer** rather than adopt `persist.el` (which
has no atomic write, no backup, and `read` with no error handling — and
whose default-value short-circuit *deletes* the file when in-memory
state collapses after a failed load).

## Capabilities

### New Capabilities
_None._ This hardens an existing capability's persistence path; it adds
no user-facing capability.

### Modified Capabilities
- `workspaces`: *Per-machine persistence and restoration* gains two
  guarantees — (1) the serialized form SHALL be readable (`read`-able)
  by construction: no live Emacs object is ever written to the
  persistence file; (2) a persistence file that is present but
  unreadable SHALL be preserved (backed up) and SHALL NOT be overwritten
  — the package SHALL NOT lose data by reserializing an empty registry
  over a file it failed to parse.

## Impact

- **Code:** `config/workspaces/layouts.el` (window-parameter translators,
  bookmark-record scrub, capture/restore passes) and
  `config/workspaces/persistence.el` (atomic write, write-time readable
  assertion, backup-on-corrupt read, absent-vs-unreadable signal,
  autosave gate). Possibly `config/workspaces/data-model.el` if a flag
  defvar lands there.
- **Tests:** new `config/workspaces/test/serialization-robustness-spec.el`
  (round-trip readability incl. the killed-buffer regression;
  corruption-injection no-clobber; write-assert refuses unreadable forms;
  autosave-gate after failed load). Updates to `layouts-spec.el`,
  `persistence-spec.el`, `persistence-v3-spec.el`,
  `buffer-reincarnation-spec.el` where they assert the old
  read-failure → nil → (over)write behavior or the untranslated
  `window-preserved-size` shape.
- **Spec:** delta under
  `openspec/changes/workspaces-persistence-robustness/specs/workspaces/spec.md`.
- **No change** to the v3 schema, the registry data model, the autosave
  *triggers* (only their gating), or the gptel/worktree integrations.
- **Relationship to `workspaces-startup-no-auto-tab-restore`:** that
  change (closed, not yet archived) made startup hydrate-only; this
  change hardens the persistence path the user exercised when the bug
  surfaced. Independent code surfaces; can land and archive separately.
