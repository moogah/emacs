## MODIFIED Requirements

### Requirement: Per-machine persistence and restoration

Workspaces and their layouts SHALL persist to disk under a per-machine
path keyed by `jf/machine-role`, using **schema version 3** (unchanged by
this change). In addition to the existing serialization, hydration, and
flush-trigger guarantees, persistence SHALL be **readable by
construction** and **corruption-safe**:

**Readable-by-construction.** The serialized persistence form SHALL NOT
contain any live Emacs object that `read` cannot reconstruct (a buffer,
marker, overlay, frame, window, process, or non-symbol function object).

- Window-state captured for a layout SHALL pass every persistent window
  parameter whose value embeds such an object through a
  *serialize/deserialize translator* that converts it to a readable form
  and back. In particular, the `window-preserved-size` parameter's buffer
  object SHALL be serialized as the buffer's name and restored via
  buffer-name lookup, so a preserved size survives restart without
  writing an unreadable object.
- The buffer-reincarnation record captured per window leaf (its
  `bookmark-make-record` result) SHALL be scrubbed of any unreadable
  value before persistence.
- Before committing a write, the package SHALL verify the serialized form
  round-trips through `read`. If it does not, the package SHALL abort the
  write, SHALL leave the previously persisted file intact, and SHALL emit
  a warning — it SHALL NOT write an unreadable file.

**Corruption-safe I/O.** Writing the persistence file SHALL be atomic
with respect to the live file: a write SHALL NOT truncate or partially
overwrite the existing file if it is interrupted (e.g. serialize to a
sibling temp file and rename over the target).

A persistence file that exists but cannot be read (parse/`read` failure)
is **present-but-unreadable**, which the package SHALL distinguish from
**absent**:

- On a present-but-unreadable file, the package SHALL preserve the file by
  renaming it to a timestamped sibling (`workspaces.eld.corrupt-<ts>`),
  SHALL emit a warning naming the backup path, and SHALL NOT overwrite the
  original path with the (empty) in-memory registry.
- After a present-but-unreadable load, the package SHALL suppress all
  persistence writes for the session (every flush trigger no-ops with a
  one-time warning) — so an autosave, idle flush, or `kill-emacs` flush
  cannot destroy data by reserializing an empty or partial registry over
  the path.
- On an absent file, the package SHALL start fresh and persistence SHALL
  proceed normally (writes permitted).

The existing flush triggers (explicit save, context switch, layout
switch, idle timer, `kill-emacs-hook`) are unchanged except that each is
gated by the suppression rule above when the prior load was unreadable.

#### Scenario: A live Emacs object is never written to disk
- **WHEN** a workspace layout is captured whose window tree carries a
  `window-preserved-size` parameter referencing a buffer (live or killed)
- **AND** the layout is serialized for persistence
- **THEN** the serialized form contains no `#<…>` token and is readable
  by `read`
- **AND** the `window-preserved-size` value is encoded by buffer name

#### Scenario: Preserved window size round-trips across restart
- **WHEN** a workspace with a preserved-size window is saved and the
  persistence file is reloaded in a fresh session
- **THEN** the persisted form deserializes without error
- **AND** the preserved-size parameter is rehydrated against the live
  (reincarnated) buffer by name

#### Scenario: An unreadable persistence file is preserved, not overwritten
- **WHEN** the persistence file exists but contains an unreadable object
  (e.g. `#<killed buffer>`) so `read` fails
- **AND** Emacs starts up
- **THEN** the file is renamed to `workspaces.eld.corrupt-<timestamp>`
  and a warning names that path
- **AND** the in-memory registry is empty (no workspaces hydrated)
- **AND** the original path is NOT overwritten with `(:version 3
  :workspaces nil)`

#### Scenario: Autosave is suppressed after a failed load
- **WHEN** a startup load was present-but-unreadable (persistence blocked
  for the session)
- **AND** an autosave, idle flush, or `kill-emacs` flush subsequently fires
- **THEN** no write to the persistence path occurs (the flush no-ops with
  a one-time warning)

#### Scenario: A write that would be unreadable is aborted
- **WHEN** a persistence write is attempted whose serialized form fails the
  pre-write `read` round-trip (an unknown unreadable value slipped past the
  translators)
- **THEN** the write is aborted and a warning is emitted
- **AND** the previously persisted file is left intact (not truncated or
  replaced)

#### Scenario: An absent file starts fresh and saves normally
- **WHEN** no persistence file exists
- **AND** Emacs starts up and the user creates and saves a workspace
- **THEN** the registry starts empty, persistence is not blocked, and the
  save writes the file normally
