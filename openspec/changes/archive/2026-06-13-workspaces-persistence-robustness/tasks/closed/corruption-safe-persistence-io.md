---
name: corruption-safe-persistence-io
description: Atomic write + write-time readable assert + never-clobber-on-unreadable (backup + autosave gate) so a corrupt file can never cause data loss
change: workspaces-persistence-robustness
status: done
relations:
  - "blocked-by:serialize-window-params-readable"
---

## Files to modify

- config/workspaces/persistence.org (modify) → tangle to persistence.el
- config/workspaces/test/serialization-robustness-spec.el (extend — shared with the other task; the other task creates it first)
- config/workspaces/test/persistence-spec.el (modify)
- config/workspaces/test/persistence-v3-spec.el (modify, if it asserts read-failure → nil)

## Implementation steps

1. **persistence.org — session block flag.** Add
   `workspace--persistence-blocked` (defvar, nil): "Non-nil when the
   startup load found a present-but-unreadable file; suppresses all
   persistence writes for the session to avoid clobbering the backed-up
   original."

2. **persistence.org — atomic write + write-time readable assert** in
   `workspace--write-state`:
   - First, the **gate**: if `workspace--persistence-blocked`, no-op with a
     one-time `display-warning` (use a separate `workspace--persistence-
     blocked-warned` flag so it warns once, not every debounce tick).
   - **Readable assert**: before touching disk, round-trip the form:
     `(condition-case _ (read (prin1-to-string FORM)) (error → abort))`.
     (Prefer `readablep` if the repo's min Emacs version is ≥28 — check
     and note in ## Observations; round-trip is the safe fallback.) On
     failure: `display-warning` loudly and RETURN without writing —
     leaving any prior good file intact. Do not signal.
   - **Atomic write**: `make-directory` the state dir; write the form to a
     temp file in the SAME directory (`make-temp-file` with the state dir
     as DIR, or a `workspaces.eld.tmp-<pid>` sibling); then
     `(rename-file TMP (workspace--state-file) t)`. Use the same
     `print-length`/`print-level` nil binding as today.

3. **persistence.org — backup-on-corrupt + absent-vs-unreadable** in
   `workspace--read-state`:
   - Keep "file missing → nil" (absent → fresh start).
   - On a `read`/parse error of an EXISTING file (the `condition-case`
     error arm): (a) rename the file to
     `(concat file ".corrupt-" (format-time-string "%Y%m%d-%H%M%S"))`
     (guard the rename itself in `ignore-errors`); (b) `display-warning`
     naming both the original and the backup path and telling the user
     persistence is suppressed this session; (c) set
     `workspace--persistence-blocked` to t; (d) return the sentinel symbol
     `workspace--unreadable` (NOT nil).
   - The version-mismatch arm keeps returning nil (that's a recognized
     "ignore + start fresh", not corruption) — do not back up / block on
     version mismatch.

4. **persistence.org — `workspace--restore` handles the sentinel.** It
   currently does `(let ((state (workspace--read-state))) (when state
   (workspace--deserialize-state state)))`. Update so:
   - `nil` (absent) → no-op, persistence NOT blocked (fresh start saves OK).
   - `workspace--unreadable` sentinel → no-op deserialize (registry stays
     empty), persistence already blocked by read-state. Do NOT deserialize
     the sentinel.
   - a real state plist → deserialize as today.

5. **persistence.org — gate the other writers.** Ensure
   `workspace--flush-state`, `workspace-save-state` (debounced), and
   `workspace--kill-emacs-flush` all route through / respect
   `workspace--persistence-blocked`. The cleanest is to gate at the
   `workspace--write-state` choke point (covers flush + save-state which
   call it); ALSO add an explicit early-return in `workspace--kill-emacs-flush`
   (and in `workspace-save-state` so it doesn't even arm the timer) for
   clarity and to avoid arming useless timers. Verify every disk-writing
   entry point is covered.

6. **Tangle**: `./bin/tangle-org.sh config/workspaces/persistence.org`
   (validates). `git diff config/workspaces/persistence.el` to confirm the
   expected changes landed (tangle leading-`*` hazard).

7. **Tests — serialization-robustness-spec.el** (extend the file created
   by the sibling task; use `workspace-state-directory-override` to
   sandbox every test — set it in `before-each`, clean in `after-each`):
   - **Corruption-injection / no-clobber**: write a state file whose
     contents literally contain an unreadable token, e.g.
     `"(:version 3 :workspaces (#<killed buffer>))"`; call
     `workspace--read-state` (or `workspace--restore`); assert: (a) the
     original path no longer holds the corrupt bytes AND a
     `workspaces.eld.corrupt-*` sibling exists with them; (b)
     `workspace--persistence-blocked` is t; (c) `workspace--read-state`
     returned the `workspace--unreadable` sentinel; (d) the registry is
     empty after restore.
   - **Autosave-gate**: with `workspace--persistence-blocked` t, call
     `workspace--flush-state` and simulate `workspace--kill-emacs-flush`;
     assert NO write occurred (the override dir's file is unchanged / not
     created as `(:version 3 :workspaces nil)`). Reset the once-warned
     flag between assertions if needed.
   - **Write-assert**: stub `workspace--serialize-registry` (or pass a
     form) containing a raw buffer object; call `workspace--write-state`;
     assert it does NOT write and any prior file is intact.
   - **Atomic write happy-path**: a normal write produces the file via the
     temp+rename path and leaves no `*.tmp-*` sibling behind; the written
     file `read`s back equal.
   - **Absent → fresh**: no file present; `workspace--read-state` returns
     nil, `workspace--persistence-blocked` stays nil, a subsequent save
     writes normally.

8. **Tests — persistence-spec.el / persistence-v3-spec.el**: update any
   spec asserting the OLD read-failure behavior (silently returns nil /
   then writes). The read-failure contract is now backup + sentinel +
   block. The version-mismatch → nil specs are UNCHANGED (still ignored,
   not backed up).

## Design rationale

A failed read was indistinguishable from "no file", and nothing stopped
an autosave from overwriting the unreadable file with an empty registry —
silent data loss. Distinguishing absent from present-but-unreadable,
preserving the corrupt file, and gating all writers on a session flag
removes the clobber. Atomic temp+rename removes mid-write truncation. The
write-time readable assert is the backstop for any unreadable value that
slips past the serializer's translators (other task). We keep our own
writer rather than persist.el (which is no safer; see research). See
design.md §D2, proposal "Layer B".

## Verification

- `./bin/tangle-org.sh config/workspaces/persistence.org` validates.
- `grep -n 'persistence-blocked\|rename-file\|corrupt-\|workspace--unreadable\|readablep\|prin1-to-string' config/workspaces/persistence.el`
  shows atomic write, backup, sentinel, gate, assert.
- `./bin/run-tests.sh -d config/workspaces` fully green.
- **End-to-end regression**: a spec (or manual note) proving the original
  cascade is dead — write a `#<…>` file, `workspace--restore`, then fire a
  flush, and confirm the file was NOT reduced to `(:version 3 :workspaces nil)`.

## Context

design.md §D2 (atomic write, assert, backup, gate); proposal "Layer B";
specs/workspaces/spec.md scenarios "An unreadable persistence file is
preserved, not overwritten", "Autosave is suppressed after a failed load",
"A write that would be unreadable is aborted", "An absent file starts
fresh"; research/findings-serialization-and-corruption-safety.md (§C),
research/findings-activities-persistence-deep-dive.md (why not persist.el).
Depends on serialize-window-params-readable (the readable-by-construction
serializer + the shared `workspace--unreadable-object-p` predicate this
task's write-assert may reuse).

## Observations

- **readablep vs round-trip.** Implemented the write-time readable
  assert as a `prin1-to-string` → `read` round-trip
  (`workspace--state-readable-p`), per the task/design fallback. The
  runtime Emacs in this environment is 30.2 (so `readablep`, available
  since Emacs 28, would work), but the repo declares no minimum Emacs
  version anywhere (no `Package-Requires`, no `emacs-min-version`
  constant; `grep` found none), so the version-safe round-trip is the
  correct choice and its cost is negligible against the debounced,
  human-paced write cadence. The round-trip catches the exact failure
  mode of concern — a `#<…>` token making the printed form unreadable —
  by construction.

- **make-temp-file ordering.** `make-directory` must precede
  `make-temp-file` (which fails if the DIR prefix's directory is
  absent). Sequenced via a `let*` `(_ (make-directory dir t))` binding
  before the `tmp` binding.

- **Tier precedence under noninteractive.** The new specs sandbox via
  `workspace-state-directory-override` (tier 1), which takes precedence
  over the batch-noninteractive sandbox (tier 2). Confirmed: the override
  dir is where all I/O lands during the specs.

- **End-to-end regression result.** Encoded as the spec "end-to-end
  clobber-cascade regression": write a `#<window 1>` state file →
  `workspace--restore` → fire `workspace--flush-state`,
  `workspace--kill-emacs-flush`, and `workspace-save-state`. Asserts the
  live state file is NOT (re)written (so never reduced to
  `(:version 3 :workspaces nil)`), the corrupt original survives
  verbatim in a `workspaces.eld.corrupt-*` backup, and no useless idle
  timer was armed. The original clobber cascade is dead.

- **No spec rewrites needed for old read-failure behavior.** Neither
  `persistence-spec.el` nor `persistence-v3-spec.el` asserted the OLD
  "read-failure → nil → overwrite" contract — their only failure-arm
  coverage is version-mismatch → nil (unchanged: still ignored, no
  backup, no block) and absent → nil (unchanged). So those files needed
  no edits; the new read-failure contract (backup + sentinel + block) is
  covered entirely by the new Layer B blocks in
  `serialization-robustness-spec.el`.

- **Suite:** 352 specs / 0 failed (was 345/0; +7 Layer B specs). No
  regression.

## Discoveries

- class: interface-drift
  affected_register_entry: register/boundary/autosave-guard-pipeline
  summary: |
    Added a NEW suppression condition — `workspace--persistence-blocked`
    — to the autosave boundary, ADDITIVE to the existing stage-1
    anti-save-predicate gate. It is NOT a replacement: the four stage-1
    wrap call sites (before-tab-switch on tab-bar-select-tab AND
    tab-bar-switch-to-tab, kill-emacs-flush, idle-tick) and the
    explicit-save stage-2 entry all keep working unchanged. The new gate
    sits at stage 4 (flush) via the `workspace--write-state` choke point
    (covering flush-state, save-state, the idle→autosave→save-state path,
    and kill-emacs-flush), plus explicit early-returns in
    `workspace--kill-emacs-flush` and `workspace-save-state` (the latter
    so a blocked session never arms a useless idle timer). Recommend the
    integrate-phase architect record this boundary extension on the
    entry: stage 4 now has a session-scoped suppression precondition
    (`workspace--persistence-blocked`) in addition to its per-trigger
    debounce/synchronous routing.

- class: shape-confirmation
  affected_register_entry: register/shape/workspace-plist-v3
  summary: |
    The atomic temp+rename write produces the identical on-disk readable
    form `(:version 3 :workspaces (...))` — the temp file is written with
    the same `print-length`/`print-level` nil binding and `prin1` as
    before; only the destination path changes (sibling temp →
    rename-file over target). Confirmed by the "atomic write happy path"
    spec, which reads the file back and asserts `:to-equal` the exact
    input form. No drift to the v3 shape.

- class: new-boundary
  affected_register_entry: (none yet — recommend creating one at integrate)
  summary: |
    This task establishes a NEW persistence-robustness boundary with four
    coupled invariants: (1) atomic-write — every write goes temp-in-same-
    dir then rename-file, never an in-place truncating write; (2) never-
    clobber-unreadable — a present-but-corrupt file is renamed to
    `*.corrupt-<ts>` and never overwritten; (3) absent-vs-unreadable —
    `workspace--read-state` returns nil for absent (fresh start, saving
    permitted) but the `workspace--unreadable` sentinel for corrupt
    (registry empty, saving suppressed), and `workspace--restore` honours
    the distinction; (4) autosave-gate — `workspace--persistence-blocked`
    is a one-way session flag set by a failed load that suppresses ALL
    writers, never auto-cleared. Recommend the integrate-phase architect
    create a `register/boundary/persistence-robustness-io` (or similar)
    entry capturing these four invariants and their producer
    (config/workspaces/persistence.org) and consumers
    (workspace--restore, all writers).
