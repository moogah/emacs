---
name: layout-commands
description: Implement layout save/switch/delete commands, recent-layout pointer maintenance, and auto-save on layout/workspace context switch. Buttercup-tested behaviorally against spec scenarios.
change: add-workspaces-package
status: done
relations:
  - "blocked-by:tab-integration"
---

## Files to modify

- `config/workspaces/layouts.org` (new) — layout commands + auto-save plumbing.
- `config/workspaces/workspaces.org` (modify) — load `layouts.el`.
- `config/workspaces/test/layouts-spec.el` (new) — Buttercup behavioral tests.
- `config/workspaces/test/home-spec.el` (modify) — un-`xit` the scenarios that this task unblocks.

## Implementation steps

1. In `layouts.org`, implement the user-facing commands. Each works against the *current* workspace (`workspace--current-name`); error if not on a workspace-tagged tab.
   - `workspace-save-layout (NAME)` — prompts for NAME (history-aware), serializes current window config via `frameset-save` (single-frame, no buffer-state for non-file buffers), wraps in a `workspace--layout-make`, `workspace--upsert-group`s it into the current workspace's groups, sets recent-group to NAME, persists registry to memory only (disk write lands in `persistence` task).
   - `workspace-switch-layout (NAME)` — `completing-read` over the current workspace's group names; first calls the internal auto-save helper (step 2) to snapshot the *outgoing* group's layout, then `frameset-restore`s the chosen group's most recent layout into the selected frame, then sets recent-group to NAME.
   - `workspace-delete-layout (NAME)` — refuses if `workspace--group-name-reserved-p NAME`. Otherwise removes the group; if the deleted group was the recent pointer, reassign to the next most recently used remaining group (track order in a list; if empty, fall back to `"home"`).
   - `workspace-switch-to-recent-layout` — alias to `workspace-switch-layout` of the current workspace's recent-group.
2. Implement `workspace--autosave-current-layout`: pure-internal. Reads `tab-bar--current-tab`'s `:workspace-name` and recent-group; serializes the current frame's window config via `frameset-save`; replaces the most recent layout in the recent group (does NOT append a new revision — MVP keeps one layout per group, per design.md §D5).
3. Hook auto-save:
   - At the **start** of `workspace-switch-layout` (handles within-workspace switch).
   - On the existing `tab-bar-switch-to-tab` advice (from `tab-integration`): when switching FROM a workspace tab, call `workspace--autosave-current-layout` *for the outgoing tab* (capture the parameters before the switch fires). The cleanest hook is `tab-bar-tab-pre-select-functions` if available in the running Emacs version; otherwise use `:before` advice on `tab-bar-select-tab`.
4. Update `workspace-new` (in `tabs.org`) so that after running the home builder, it captures the resulting window config and stores it as the home layout via the same path (`workspace-save-layout "home"`), establishing the invariant that *every workspace has at least one stored layout* before any switch happens.
5. In `layouts-spec.el`, write behavioral tests mapping 1:1 to spec scenarios:
   - **Per-workspace named layouts**: Saving and switching layouts within a workspace; Layout names are scoped to their workspace; Deleting a layout.
   - **Recent-layout pointer**: Recent-layout updates on switch; Recent-layout survives workspace switching.
   - **Auto-save on context switch**: Switching workspaces snapshots the outgoing layout; Switching layouts within a workspace snapshots the outgoing layout.
   - **Home reserved-name**: Home layout cannot be deleted; Re-saving home overwrites without invoking the builder.
6. In `home-spec.el`, un-`xit` the previously-pending scenarios now that the commands exist.
7. Smoke test in isolated Emacs:
   ```
   ./bin/emacs-isolated.sh -nw --eval "(progn
     (workspace-new \"demo\")
     (split-window-right)
     (workspace-save-layout \"split\")
     (workspace-switch-layout \"home\")
     (message \"recent: %s windows: %d\"
              (workspace--group-name (workspace--find-group ... )) (length (window-list)))
     (kill-emacs))"
   ```

## Design rationale

Auto-save on every context switch eliminates the "I forgot to save my layout" friction that activities.el's default+last model invited. The cost is one `frameset-save` per switch — fast enough in practice not to need debouncing for the in-memory write; the on-disk write is debounced in the persistence task (design.md §D7).

Snapshotting only the *active* layout (the recent-group's most recent layout) keeps MVP behavior aligned with the user-facing "one layout per group" expectation while leaving the door open for the deferred history feature (which appends a new layout to the group's `:layouts` list instead of replacing in place — pure data-model addition, no command surface change, per design.md §D5).

The reserved-name policy is enforced at the command layer via `workspace--group-name-reserved-p` (defined in the pure data layer) so the same check is unit-tested in `data-model-spec.el` and exercised end-to-end in `home-spec.el` (design.md §D6).

## Verification

- `./bin/tangle-org.sh config/workspaces/layouts.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/tangle-org.sh config/workspaces/tabs.org` (only if `workspace-new` was modified)
- `./bin/run-tests.sh -d config/workspaces` — all `layouts-spec.el`, `home-spec.el`, and previously-passing specs pass.
- `grep -n "workspace--autosave-current-layout" config/workspaces/layouts.el config/workspaces/tabs.el` shows hook sites in both places.
- Smoke command in step 7 exits 0 and reports recent group is `"home"` after the final switch.

## Context

- design.md §D5 "Persistence schema is forward-compatible by design"
- design.md §D6 "Reserve `home` as a layout-group name; configurable builder"
- design.md §D7 "Auto-save on context switch; explicit save still available"
- specs/workspaces/spec.md Requirements: Per-workspace named layouts, Per-workspace home layout, Recent-layout pointer, Auto-save layout on context switch
