---
name: remove-debounce-wire-synchronous-flush
description: Remove the debounce machinery and make every persistence trigger flush synchronously.
change: workspaces-synchronous-persistence-flush
status: done
relations: []
---

# Remove debounce machinery; wire synchronous flush at every call site

Make every existing workspaces persistence trigger flush to disk
**synchronously**, and delete the now-unused debounce machinery. This is a
single atomic task across three `.org` files because removing the blanket
advice (in `persistence.org`) stops the layouts and idle-tick triggers from
flushing at all until they are rewired — splitting by file would create a
broken intermediate state.

This change alters only *when* the write happens (synchronously vs after a 2s
idle debounce) plus deletes orphaned timer code. It does NOT change what each
trigger captures, the slot routing, restore precedence, anti-save predicates,
or the corruption-safety path.

## Files to modify

Literate config — edit the `.org` source, then tangle to `.el` with
`./bin/tangle-org.sh`, and commit both:

- `config/workspaces/persistence.org` (→ `persistence.el`)
- `config/workspaces/layouts.org` (→ `layouts.el`)
- `config/workspaces/workspaces-mode.org` (→ `workspaces-mode.el`)

## Implementation steps

### persistence.org

1. **Delete the debounce machinery** entirely:
   - `workspace-save-idle-delay` (defcustom).
   - `workspace--save-timer` (defvar).
   - `workspace-save-state` (the debounced scheduler).
   - `workspace--persistence-after-autosave` (the advice body).
   - The blanket advice:
     `(advice-add 'workspace--autosave-current-layout :after #'workspace--persistence-after-autosave)`.
2. **Simplify `workspace--flush-state`** — it no longer needs the
   timer-cancel preamble (the timer var is gone). It should reduce to writing
   the serialized registry:
   `(workspace--write-state (workspace--serialize-registry))`.
   Do NOT change `workspace--write-state` — the block check
   (`workspace--persistence-blocked`), the readable-by-construction assert
   (`workspace--state-readable-p`), and the atomic temp-file + rename stay
   exactly as they are.
3. **Tab-switch autosave flushes synchronously.** In
   `workspace--persistence-before-tab-switch`, after the
   `(workspace--autosave-current-layout :working-state)` capture (still gated by
   `workspace-anti-save-predicates`), call `(workspace--flush-state)`. Keep the
   anti-save-predicate guard wrapping both the capture and the flush so a
   suppressed autosave does not write.
4. Leave `workspace-save`, `workspace-revert`, and `workspace--kill-emacs-flush`
   as they are — they already call `workspace--flush-state` /
   `workspace--write-state` synchronously.

### layouts.org

5. In each of these three commands, after the existing
   `workspace--autosave-current-layout` capture (and any `:working-state`-clear
   step), add a synchronous `(workspace--flush-state)`:
   - `workspace-save-layout` (`:saved-state`) — flush after the working-state
     clear step.
   - `workspace-switch-layout` (`:working-state`) — flush after the capture of
     the outgoing layout (before or after restoring the destination layout is
     fine; the captured outgoing state is what must persist).
   - `workspace--capture-home-layout` (`:saved-state`; runs as the
     `workspace-new` `:after` advice) — flush after the stamp.
   Note `persistence.el` `require`s `workspace-layouts`, not the reverse, so
   `workspace--flush-state` must be reachable from `layouts.el` at call time.
   It is defined in `persistence.el`. Since these are interactive commands
   invoked well after load, a forward reference is safe; if byte-compile warns
   about an undefined function, add a `(declare-function workspace--flush-state
   "workspace-persistence")` near the top of `layouts.org` rather than adding a
   `require` cycle.

### workspaces-mode.org

6. In `workspaces-mode--idle-tick`, after the
   `(workspace--autosave-current-layout :working-state)` capture (still gated by
   `workspace-anti-save-predicates`), call `(workspace--flush-state)` so the
   idle save lands on disk immediately at the quiescent tick moment. This fixes
   the `run-with-idle-timer` already-idle trap (a debounced write armed from
   within a ≥60s idle period was deferred to a future idle period). Add a
   `declare-function` for `workspace--flush-state` if byte-compile warns.

### Tangle + sanity

7. Tangle each edited file: `./bin/tangle-org.sh config/workspaces/persistence.org`
   (and likewise for `layouts.org`, `workspaces-mode.org`). The script
   auto-validates parens.
8. Grep the tangled `.el` files to confirm the debounce symbols are gone:
   `grep -n "workspace-save-state\|workspace--save-timer\|workspace-save-idle-delay\|workspace--persistence-after-autosave" config/workspaces/persistence.el`
   should return nothing.

## Design rationale

- "Remove, not park": every trigger becomes synchronous, so the debounce has
  zero customers. Leaving it dormant keeps dead, misleading code (it is what
  made saves look broken during testing). See design.md D1.
- Per-call-site synchronous flush via the existing `workspace--flush-state`
  keeps the write path (block check, readable assert, atomic rename)
  untouched. See design.md D2.
- Idle tick must flush synchronously to actually provide crash-safety; the
  prior debounce hit the already-idle trap. See design.md D3.
- `workspace-save-idle-delay` removal is the one BREAKING change (pre-alpha; no
  shim). See design.md D4.

## Invariants to preserve (do not regress)

- `autosave-never-writes-saved-state` — autosave call sites still pass
  `:working-state`.
- `explicit-save-clears-working-state` — `workspace-save` / `workspace-save-layout`
  still clear `:working-state`.
- `restore-precedence-working-over-saved` — restore path untouched.
- `workspace--persistence-blocked` still suppresses ALL writes for the session.
- Readable-by-construction write-time assert still gates every write.

## Verification

```bash
./bin/tangle-org.sh config/workspaces/persistence.org
./bin/tangle-org.sh config/workspaces/layouts.org
./bin/tangle-org.sh config/workspaces/workspaces-mode.org
# debounce symbols fully removed:
grep -n "workspace-save-state\|workspace--save-timer\|workspace-save-idle-delay\|workspace--persistence-after-autosave\|run-with-idle-timer" \
  config/workspaces/persistence.el config/workspaces/layouts.el
# expect: no matches in persistence.el/layouts.el for the debounce symbols
# (workspaces-mode.el legitimately keeps run-with-idle-timer for the MODE timer,
#  not for the flush debounce)
# synchronous flush wired at the rewired call sites:
grep -n "workspace--flush-state" config/workspaces/layouts.el config/workspaces/persistence.el config/workspaces/workspaces-mode.el
```

Tests are handled by the dependent task `update-tests-for-synchronous-flush`.

## Context pointers

- proposal.md — motivation, BREAKING note on `workspace-save-idle-delay`.
- design.md — D1–D5, the call-site table, risks.
- specs/workspaces/spec.md — MODIFIED `Auto-save layout on context switch`,
  `Per-machine persistence and restoration`, `Idle save mode`.
