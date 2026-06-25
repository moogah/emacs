## Why

The workspaces package debounces its disk flush (a 2-second `run-with-idle-timer`)
on *every* persistence trigger via a single blanket advice on
`workspace--autosave-current-layout`. This makes deliberate user actions
(`workspace-new`, `workspace-save-layout`, `workspace-switch-layout`) appear
unsaved for up to two seconds and lost outright on a crash, and it produced a
confusing "nothing ever autosaves" experience during testing. The debounce
works exactly as specified — but it is premature infrastructure: it exists to
coalesce bursts, yet every trigger in the package today is a discrete,
quiescent, low-frequency event with no burst to coalesce. The one event source
that *would* fire in bursts — autosave on window-configuration change — is not
implemented. The debounce therefore has no remaining customer; it only adds a
flush-latency window and conceptual noise.

## What Changes

- Make **every existing persistence trigger flush synchronously**:
  - `workspace-save-layout` — debounced → synchronous.
  - `workspace-switch-layout` — debounced → synchronous.
  - `workspace-new` home stamp (via `workspace--capture-home-layout`) —
    debounced → synchronous.
  - Workspace context switch / tab switch (writes outgoing `:working-state`) —
    debounced → synchronous.
  - `workspaces-mode` idle timer (writes `:working-state`) — debounced →
    synchronous. (Debouncing it gained nothing and actively hit the
    `run-with-idle-timer` already-idle trap, deferring the disk write to a
    future idle period and defeating the idle save's crash-safety purpose.)
  - `workspace-save` and `kill-emacs-hook` are already synchronous — unchanged.
- **Remove the debounce machinery**: `workspace-save-state`,
  `workspace--save-timer`, the `workspace-save-idle-delay` defcustom,
  `workspace--persistence-after-autosave`, and the blanket
  `advice-add` on `workspace--autosave-current-layout`. **BREAKING** for any
  user who set `workspace-save-idle-delay` (the option is removed; it no longer
  has any effect).
- Route each call site through the existing synchronous `workspace--flush-state`.
- The behavioral invariants are preserved: autosave still writes only
  `:working-state`, explicit save still clears `:working-state`,
  working-over-saved restore precedence is unchanged, `workspace--persistence-blocked`
  still suppresses all writes for the session, and the readable-by-construction
  write-time assert still gates every write. **Only the flush *timing* changes**
  (debounced → synchronous); *what* each trigger captures is unchanged.

Explicitly **out of scope**: autosave on window-configuration change. That is
the only future trigger that would genuinely warrant re-introducing a debounce
(window changes fire in bursts), and it carries its own design questions
(transient-popup filtering, capture cost, default-on). It is deferred to a
separate change.

## Capabilities

### New Capabilities
<!-- none -->

### Modified Capabilities
- `workspaces`: The `Per-machine persistence and restoration` requirement's
  flush-trigger list changes the "(debounced flush)" annotations on workspace
  context switch, intra-workspace layout switch, and the `workspaces-mode` idle
  timer to "(synchronous flush)". The `Auto-save layout on context switch`
  requirement is clarified to state that the context-switch and layout-switch
  flushes are synchronous (the working-state *capture* is unchanged; only the
  flush timing changes). The `Idle save mode` requirement's reference to the
  shared debounce is updated to a synchronous flush.

## Impact

- **Code (literate; edit `.org`, tangle to `.el`, commit both):**
  - `config/workspaces/persistence.org` — remove debounce machinery, blanket
    advice; tab-switch advice and kill-emacs flush paths.
  - `config/workspaces/layouts.org` — `workspace-save-layout`,
    `workspace-switch-layout`, `workspace--capture-home-layout`.
  - `config/workspaces/workspaces-mode.org` — idle-tick body.
- **Customization:** `workspace-save-idle-delay` defcustom removed (BREAKING for
  anyone who set it).
- **Tests:** existing Buttercup specs under `config/workspaces/test/`; any spec
  asserting debounced/timer behavior is updated to assert synchronous flush.
  Run with `./bin/run-tests.sh -d config/workspaces`.
- **No change** to on-disk schema (still `:version 3`), restore semantics, or
  corruption-safety behavior.
