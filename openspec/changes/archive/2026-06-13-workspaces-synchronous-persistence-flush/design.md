## Context

Today every workspaces persistence trigger funnels its disk flush through a
single blanket advice:

```elisp
(advice-add 'workspace--autosave-current-layout :after
            #'workspace--persistence-after-autosave)  ; → workspace-save-state (debounced 2s)
```

`workspace-save-state` arms a `run-with-idle-timer` of `workspace-save-idle-delay`
(2s); the actual write (`workspace--flush-state` → `workspace--write-state`)
runs only after Emacs has been idle that long. Two commands bypass the debounce
and flush synchronously by calling `workspace--flush-state` directly:
`workspace-save` and `workspace-revert`. The `kill-emacs-hook` flush
(`workspace--kill-emacs-flush`) also writes synchronously.

The capture/flush call sites are:

| Call site | Module | Slot | Flush today |
|---|---|---|---|
| `workspace-save` | persistence | `:saved-state` | sync (direct) |
| `workspace-revert` | persistence | — (clears) | sync (direct) |
| `workspace--kill-emacs-flush` | persistence | `:working-state` | sync (direct) |
| `workspace-save-layout` | layouts | `:saved-state` | debounced (advice) |
| `workspace-switch-layout` | layouts | `:working-state` | debounced (advice) |
| `workspace--capture-home-layout` (via `workspace-new` advice) | layouts | `:saved-state` | debounced (advice) |
| `workspace--persistence-before-tab-switch` | persistence | `:working-state` | debounced (advice) |
| `workspaces-mode--idle-tick` | workspaces-mode | `:working-state` | debounced (advice) |

The investigation in the originating explore session confirmed the debounce
behaves exactly as specified — it is not buggy. But it is premature: it exists
to coalesce burst-fire writes, and no current trigger fires in bursts. The only
trigger that *would* (autosave on window-configuration change) is unimplemented
and out of scope here.

This is a literate Emacs config: edit `.org`, run `./bin/tangle-org.sh`, commit
both `.org` and generated `.el`. Existing behavioral invariants must be
preserved (see Non-Goals).

## Goals / Non-Goals

**Goals:**
- Every existing persistence trigger flushes synchronously (no idle/debounce lag).
- Remove the debounce machinery entirely rather than leaving it dormant
  (`workspace-save-state`, `workspace--save-timer`, `workspace-save-idle-delay`,
  `workspace--persistence-after-autosave`, the blanket advice).
- Keep all capture semantics, slot routing, anti-save predicates,
  corruption-safety, and restore precedence exactly as they are.

**Non-Goals:**
- Autosave on window-configuration change (the one real burst source). Deferred
  to a separate change; that is where a *scoped* debounce would legitimately
  return, along with transient-popup filtering and default-on questions.
- Any change to the on-disk schema (stays `:version 3`), restore logic, buffer
  reincarnation, or the `workspace--persistence-blocked` corruption path.
- Touching `workspace-save` / `workspace-revert` flush behavior (already sync).

## Decisions

### D1 — Remove the blanket advice; flush per call site ("remove, not park")

Drop `(advice-add 'workspace--autosave-current-layout :after …)` and have each
call site invoke `workspace--flush-state` (synchronous) itself after its
`workspace--autosave-current-layout` capture. `workspace--flush-state` already:
cancels any pending timer (will be a no-op once the timer var is gone),
serializes the whole registry, runs the readable-by-construction assert, honors
`workspace--persistence-blocked`, and writes atomically via temp-file + rename.

**Alternative considered (rejected): keep the advice, append a sync flush to the
three deliberate commands.** Smaller diff, but it leaves the deliberate path
*arming an idle timer then immediately cancelling it* — exactly the vestigial
"why is this here?" machinery that made saves look broken during testing. Since
*every* trigger becomes synchronous, the debounce has zero customers; parking it
keeps dead, misleading code. Remove it.

### D2 — `workspace--flush-state` becomes the single synchronous write entry point

After removing `workspace-save-state` and `workspace--save-timer`,
`workspace--flush-state` no longer needs its timer-cancel preamble. It reduces
to "serialize the registry and write it" (still gated inside
`workspace--write-state` by the block + readable asserts). All eight non-block
call sites use it. `workspace--write-state` is unchanged.

### D3 — Idle tick flushes synchronously, fixing the already-idle trap

`workspaces-mode--idle-tick` currently relies on the advice to schedule a
debounced write. Because the tick fires from *within* an already-long idle
period (≥ `workspaces-mode-idle-frequency`, default 60s), the 2s idle-delay
write it armed would not fire in the current idle period — it deferred to a
future one, so the idle save's crash-safety value was largely illusory (it
effectively only landed via the kill-emacs flush). Calling
`workspace--flush-state` directly in the tick body fixes this: the capture and
the write both happen at the quiescent moment the tick fires. The tick still
respects `workspace-anti-save-predicates` (unchanged).

### D4 — `workspace-save-idle-delay` defcustom is removed (BREAKING)

It only parameterized the debounce. With no debounce, it has no effect.
Removing it (rather than leaving an inert defcustom) keeps the customization
surface honest. This is the single user-visible breaking change; documented in
the proposal. No migration shim — the package is pre-alpha.

### D5 — Preserve invariants exactly

The capture helper `workspace--autosave-current-layout` and its slot routing are
untouched, so these hold by construction:
- *autosave-never-writes-saved-state* — autosave call sites still pass
  `:working-state`.
- *explicit-save-clears-working-state* — `workspace-save` /
  `workspace-save-layout` still clear `:working-state` as their own step.
- *restore-precedence-working-over-saved* — restore path untouched.
- corruption-safety + readable-by-construction — `workspace--write-state`
  untouched; `workspace--persistence-blocked` still short-circuits every write.

The change is purely *when* the write happens (synchronously vs after idle),
plus deleting the now-orphaned timer code.

## Risks / Trade-offs

- **[Rapid tab cycling triggers several synchronous full-registry writes in
  quick succession]** → Each write is a small registry serialize + read-assert +
  atomic rename (sub-millisecond to a few ms for a handful of workspaces). Even
  cycling 5 tabs in a second is a few writes totaling tens of ms — imperceptible,
  and each write is legitimate (it persists a distinct outgoing working-state).
  Accepted deliberately (user chose sync for tab-switch).
- **[A future high-frequency trigger could reintroduce burst writes]** → That is
  exactly the deferred window-change-autosave change, which will re-introduce a
  debounce *scoped to that trigger* rather than blanket-applied. Documented as a
  non-goal so the next implementer doesn't resurrect the blanket advice.
- **[Removing `workspace-save-idle-delay` breaks user configs that set it]** →
  Pre-alpha package; called out as BREAKING in the proposal. The variable simply
  ceases to exist; setting it becomes a no-op `setq` (harmless) or a
  void-variable only if referenced in code (none remains).
- **[Tests asserting timer/debounce behavior fail]** → Update those specs to
  assert synchronous flush (file written immediately after the command returns)
  instead of timer arming. Identify them during implementation by grepping the
  test tree for `workspace-save-state`, `workspace--save-timer`,
  `workspace-save-idle-delay`, and `run-with-idle-timer`.

## Migration Plan

1. Edit the three `.org` files (persistence, layouts, workspaces-mode); tangle
   each with `./bin/tangle-org.sh` (auto-validates parens).
2. Remove the debounce machinery and rewire call sites per D1–D4.
3. Update/curate the spec deltas (already authored) — no code dependency.
4. Run `./bin/run-tests.sh -d config/workspaces`; fix any debounce-asserting
   specs to assert synchronous flush.
5. Commit `.org` + `.el` together.

Rollback: revert the commit; the prior debounced behavior is fully restored
(no schema or on-disk format change to unwind).

## Open Questions

None. The two design forks (remove-vs-park; tab-switch bucket) were resolved in
the originating explore session: remove the machinery; tab-switch is synchronous.
