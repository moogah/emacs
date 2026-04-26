---
name: spike-eval-checklist
description: Author the spike evaluation runbook so the user can systematically validate the layered system over the ~1-week eval window before deciding the long-term shape.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:wire-into-init"
---

## Files to modify

- `openspec/changes/org-graph-spike/eval-checklist.md` (new) — manual validation runbook lived inside the change folder, NOT under `config/`. (Per CLAUDE.md "never create *.md files" guardrail, this is permitted because it's a change-scoped artifact, not a permanent doc; it gets archived alongside the change.)

## Implementation steps

1. Create `openspec/changes/org-graph-spike/eval-checklist.md` with the following sections:

   - **Setup once**: commands to run after the spike module is wired in.
     - `M-x org-graph/eager-discover` once for `~/org/roam/` and `~/work/`.
     - Confirm vulpea sync completes (check `*Messages*`).
     - Add 3–5 typed-edge properties (`:IMPLEMENTS:`, `:RELATES_TO:`) to existing concept notes.

   - **Daily-use checklist** (run for ~1 week):
     - `org-graph/find-topic` returns the curated topic list quickly.
     - Save latency on a 1000+ heading file is imperceptible (< 50ms target — measure with `(benchmark 1 (save-buffer))`).
     - External writes (e.g. `git pull`, agent tool write) are reflected in queries within ~5s without manual sync.
     - `org-graph-query/outgoing <id>` returns the expected typed edges.
     - Project-co-located note created at `~/work/<ticket>/notes.org` is reachable via `org-graph/find-project` after eager discover.
     - org-roam continues to work: `org-roam-node-find`, `org-roam-buffer-toggle`, dailies all functional.

   - **Agent-tool smoke test**:
     - Through gptel, ask the agent to `org-graph-query-notes` for a tag — verify response shape.
     - Ask the agent to `org-graph-write-node` — verify file lands under `~/org/roam/`, carries `:agent-draft:`, has the requested typed edges in PROPERTIES.
     - Trigger two agent writes to the same target file (artificially) — verify no corruption.

   - **Open-question resolution journal**: a section per design.md Open Question. Track findings in line; resolved questions move to "Decided" for the follow-up change.

   - **Watcher-load measurement**: capture `(length (file-notify--descriptors))` before and after enabling. If it grows unreasonably, narrow `org-graph-watched-roots`.

   - **Decision criteria for follow-up**: a small rubric — keep / refine / drop. Each criterion mapped to a measurable observation from the week.

2. Cross-link the checklist from `proposal.md` (Impact section) so the change-archive carries the eval evidence.

## Design rationale

A spike is only as good as the evidence collected during it. Without a written checklist, week-of-use observations stay in the user's head and the follow-up change has to re-derive them. Putting the checklist in the change folder (not in `config/`) keeps it scoped to this decision and archived with the change — preserving the reasoning for future maintainers without polluting the live config tree.

The watcher-load measurement is the single most likely surprise (design.md §Risks: inotify/fsevents limits) — capturing it explicitly avoids the trap of "everything seems fine until one day Emacs hangs on save".

## Verification

- `ls openspec/changes/org-graph-spike/eval-checklist.md` — exists.
- `grep -nE "^#+ (Setup|Daily|Agent|Watcher|Decision)" openspec/changes/org-graph-spike/eval-checklist.md` — at least 5 matches (one per major section).
- `grep -n "eval-checklist.md" openspec/changes/org-graph-spike/proposal.md` — at least one match (the cross-link).

## Context

- design.md §Risks (especially the inotify/fsevents and two-indices items)
- design.md §Open Questions (all five)
- architecture.md §Testing Approach §Scenario Mapping (the rows marked manually-validated)
