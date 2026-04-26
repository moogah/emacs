---
name: implement-discovery
description: Implement org-graph/eager-discover so project-co-located notes under ~/work/ become reachable via org-id without per-directory configuration.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:install-packages"
---

## Files to modify

- `config/org-graph/org-graph.org` (modify) — fill the `Discovery` subtree.

## Implementation steps

1. Inside the `Discovery` subtree, define `org-graph/eager-discover` as an interactive command:
   - Iterate `org-graph-watched-roots`.
   - For each root, expand-file-name and `directory-files-recursively ROOT "\\.org\\'"` (skip non-existent roots with a warning).
   - Pass the accumulated file list to `org-id-update-id-locations`.
   - Print a message reporting count scanned, count with IDs registered.

2. Define `org-graph-discovery--id-files (root)` as a private helper returning org files under one root. Keep `eager-discover` thin enough to read in one screenful.

3. Make the command idempotent — running twice does not duplicate entries (`org-id-update-id-locations` handles this internally; verify by re-reading the function's docstring).

4. Do NOT add a startup hook that runs `eager-discover`. Per design.md §D6 it is manual-only; the implicit cost on `~/work` is too high for every Emacs launch.

5. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

org-node's discovery model is "whatever org-id knows about" (research findings, design.md §D1). Eager discovery is just the bridge that makes `~/work` participate. We don't wrap org-node here — we just feed `org-id-locations`, which is what org-node consumes. Keeping `eager-discover` interactive-only is a deliberate guardrail: the cost of a full `~/work` walk is paid only when the user asks for it, never on startup (design.md §D6).

## Verification

- `./bin/tangle-org.sh config/org-graph/org-graph.org` — exits 0.
- Manual: `M-x org-graph/eager-discover` reports a non-zero count when run with `~/org/roam/` populated.
- Manual: create `/tmp/test-discovery.org` with an `:ID:` property, add `/tmp/` to `org-graph-watched-roots`, run `eager-discover`, confirm `(org-id-find "<that-id>" t)` returns the file.
- `grep -n "defun org-graph/eager-discover" config/org-graph/org-graph.el` — matches.

## Context

- design.md §D1, §D6
- specs/org-graph/spec.md §Distributed Note Discovery
