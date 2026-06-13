# Design — workspaces-startup-no-auto-tab-restore

## Context

All persistence/restore logic lives in `config/workspaces/persistence.org`
(tangles to `persistence.el`); the `:restore-pending` flag helpers live in
`config/workspaces/data-model.org`. The current startup path is:

```
workspace--restore
  ├─ workspace--read-state          read .eld
  ├─ workspace--deserialize-state   clrhash + rehydrate registry;
  │                                 marks each entry :restore-pending
  └─ workspace--restore-tabs        ← creates ONE TAB PER WORKSPACE
```

Layout for those startup tabs is applied lazily, on first selection, via:

```
advice :after tab-bar-select-tab / tab-bar-switch-to-tab
  → workspace--persistence-after-tab-switch
      → workspace--activate-pending-workspace   (if :restore-pending)
          → workspace--apply-saved-layout        (clears :restore-pending)
```

Two **explicit** paths apply layout synchronously and are known-good:
`workspace-restore` and `workspace-switch-layout`. The lazy path is the
only unreliable one. The registry is hydrated from the `.eld` and the
whole registry is re-serialized on every flush (incl. `kill-emacs`), so
**unmaterialized workspaces round-trip losslessly** — nothing about
dropping startup tabs risks their persisted layout data.

The recently-landed test-isolation fix (`workspace-state-directory-override`
+ batch sandbox) is already on the branch; this change builds on it.

## Goals / Non-Goals

**Goals:**
- Startup hydrates the registry only; creates no tabs.
- Remove the lazy-restore machinery entirely (decision: full removal).
- `workspace-restore` opens `home.org` for a workspace with no saved
  layout instead of leaving a bare tab.
- Keep the persistence file format (v3) and all autosave/flush triggers
  unchanged.

**Non-Goals:**
- No disk-scan / reconciliation of on-disk dirs into the registry — the
  registry stays the authoritative, disk-decoupled source of truth (this
  is what enables implicit archive; out of scope here).
- No change to `workspace-switch`, `workspace-switch-layout`,
  `workspace-save`, `workspace-revert`, buffer reincarnation, or the
  gptel/worktree integrations.
- No migration tooling for the file format (still v3).

## Decisions

### D1 — Drop the `workspace--restore-tabs` call; delete the function

`workspace--restore` becomes read-state → deserialize, and stops there.
`workspace--restore-tabs` is deleted (no other caller). Registry
hydration already makes every workspace a `workspace-restore` candidate
(it completes over `workspace--registered-names`), so reachability is
preserved without tabs.

*Alternative considered:* keep `workspace--restore-tabs` behind a defcustom
(`workspace-restore-tabs-on-startup`, default nil). Rejected — the lazy
layout path it depends on is being removed anyway, so a re-enabled flag
would resurrect the exact bug we're killing. A clean removal is honest;
users who want tabs back use `workspace-restore`.

### D2 — Remove the lazy-activation machinery in full

Delete:
- `workspace--activate-pending-workspace` (persistence.org).
- `workspace--persistence-after-tab-switch` and its two `advice-add`
  forms on `tab-bar-select-tab` / `tab-bar-switch-to-tab`
  (persistence.org). This advice does **only** lazy activation.
- The `:restore-pending` data-model helpers
  (`workspace--restore-pending-p`, `workspace--mark-restore-pending`,
  `workspace--clear-restore-pending`) in data-model.org, now dead.

Edit:
- `workspace--deserialize-state`: stop wrapping entries in
  `workspace--mark-restore-pending`; `puthash` the (possibly
  broken-tagged) workspace directly.
- `workspace--apply-saved-layout`: drop the trailing block that clears
  `:restore-pending`; it now only applies the effective layout. Still
  used by `workspace-restore` / `workspace-revert` / `workspace-switch-
  layout`, so the function stays.
- `workspace--persistence-serialize-workspace`: it already whitelists
  slots (`:name :home :recent-layout-group :buffer-files :layout-groups`),
  so it inherently drops runtime tags — only the docstring's mention of
  `:restore-pending` needs updating.

The `:before` autosave advice (`workspace--persistence-before-tab-switch`,
captures the outgoing workspace's `:working-state`) is **unrelated and
retained**.

*Alternative considered:* leave the three pure data-model helpers as dead
code (smaller diff, fewer touched specs). Rejected — "remove fully" was
the explicit decision; orphan helpers + a `:restore-pending` keyword that
nothing sets is exactly the misleading machinery we're clearing.

### D3 — `workspace-restore` home-builder fallback

In the "no live tab" branch of `workspace-restore`, after
`tab-bar-new-tab` + `tab-bar-rename-tab`, branch on whether a saved layout
exists (recent group → recent layout → `workspace--layout-effective-state`
non-nil):
- saved layout present → `workspace--apply-saved-layout` (unchanged).
- no saved layout → `funcall workspace-home-builder` with the name
  (same call shape `workspace-new` uses), opening `<home>/home.org`.

The broken-state guard (signals `user-error` before creating a tab) and
the existing-tab switch branch are unchanged.

*Alternative considered:* always run the home-builder, then overlay the
layout. Rejected — layouts already encode their own buffers/windows;
running the builder first would flash an extra `home.org` window and
fight `window-state-put`.

## Risks / Trade-offs

- **[Behavioral surprise: tabs no longer reappear on launch]** → Documented
  in the proposal; `workspace-restore` (`C-x w o`) is the deliberate
  one-keystroke path. Acceptable per the originating QA request.
- **[Wide test blast radius]** → Specs asserting `restore-tabs` /
  `:restore-pending` / `activate-pending` must be reworked, not just the
  two reframed scenarios: `tabs-spec`, `save-restore-spec`,
  `broken-home-load-spec`, `broken-home-runtime-spec`,
  `buffer-reincarnation-spec`, `persistence-v3-spec`, `persistence-spec`,
  `workspace-delete-purge-spec`, and `data-model-spec` (the removed
  helpers). Mitigation: the tasks phase enumerates each file; run the full
  `config/workspaces` suite green before close.
- **[A broken workspace can no longer be "seen" as a startup tab]** →
  It's still hydrated into the registry and listed by `workspace-restore`,
  which surfaces the broken-state `user-error` with re-anchor/purge
  guidance — same recovery affordance, just not auto-surfaced as a tab.

## Migration Plan

Pure code change, no data migration (v3 format unchanged). Existing `.eld`
files load unchanged; the only difference is no tabs spawn at startup.
Rollback is reverting the commit. Tangle `persistence.org` and
`data-model.org` after editing; run `./bin/run-tests.sh -d config/workspaces`.

## Open Questions

None. The two design choices that needed input (full removal vs. dormant;
home.org fallback vs. bare tab) were resolved before drafting.
