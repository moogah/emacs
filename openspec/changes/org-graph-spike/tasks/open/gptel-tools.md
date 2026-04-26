---
name: gptel-tools
description: Register three gptel tools (query-notes, typed-edges, write-node) so AI agents can read the graph and write coordinator-mediated notes.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:typed-edge-query"
  - "blocked-by:coordinator-lock"
---

## Files to modify

- `config/org-graph/org-graph.org` (modify) — fill the `Tools` subtree.

## Implementation steps

1. In the `Tools` subtree, register three gptel tools using `gptel-make-tool` (or whatever the existing repo pattern is — check `config/gptel/tools/` for the canonical form):

   - `org-graph-query-notes` (read) — args: optional `filetags` list, optional `title-match` regex. Returns a list of `{:id :title :tags :file}` plists. Implementation calls `vulpea-db-query` with predicates derived from args.

   - `org-graph-typed-edges` (read) — args: `note-id`, optional `direction` (`outgoing` / `incoming` / `both` — defaults to `both`), optional `rel-type`. Delegates to `org-graph-query/outgoing` etc. Returns the plists from the query layer.

   - `org-graph-write-node` (write) — args: `title`, `filetags` list, optional `body`, optional `typed-edges` (list of `{rel-type, to-id}` maps). Behavior:
     1. Generate a slug from `title` and a deterministic filename `topic-<slug>-<yyyymmddhhmmss>.org` under `org-graph-typed-graph-root`.
     2. Generate a fresh UUID via `org-id-new`.
     3. Wrap the file write in `org-graph-coordinator/with-file-lock`. Inside: write a node with `:ID:`, `:PROPERTIES:` containing the typed-edge entries, `#+filetags:` line carrying the requested tags PLUS `:agent-draft:`.
     4. Call `org-id-add-location` to register the new ID.
     5. Return the new note's id and absolute path.

2. The write tool MUST stamp `:agent-draft:` unconditionally unless the args explicitly include `(:no-draft-tag t)`. Per spec scenario: agent-authored notes carry the draft tag by default.

3. Register the tools under a category like `'org-graph` (or whatever your existing gptel registry conventions use). Document the tool descriptions in the registration's `:description` field with a one-line summary so the model knows when to call them.

4. Add a module-load assertion: `(cl-assert (functionp 'org-graph-coordinator/with-file-lock))` before the tool registration. If the coordinator isn't loaded, fail loudly rather than register a tool that bypasses the lock.

5. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

The three-tool surface is the minimum viable agent contract: one read for "find me notes by tag/title", one read for "what does this note connect to", one write for "create a new node and link it". Anything more is YAGNI for the spike (design.md §Goals).

Routing the write through the coordinator (design.md §D5) is the only thing standing between the user's vault and concurrent-write corruption. The cl-assert guards the contract — if a future change accidentally drops the wrap, the assertion catches it at module-load, not at the moment two agents collide on a file.

The mandatory `:agent-draft:` stamping (design.md §Goals, spec scenarios) is the curation hygiene gate — it lets the user trust that nothing the agents write pollutes the topic finder until reviewed.

## Verification

- `./bin/tangle-org.sh config/org-graph/org-graph.org` — exits 0.
- `grep -nE "gptel-make-tool.*org-graph-(query-notes|typed-edges|write-node)" config/org-graph/org-graph.el` — 3 matches.
- `grep -n "with-file-lock" config/org-graph/org-graph.el | grep -i tools` — at least one match (write tool wraps the coordinator).
- `grep -n "agent-draft" config/org-graph/org-graph.el` — at least one match in the Tools subtree (mandatory stamping).

## Context

- design.md §D5
- specs/org-graph/spec.md §Agent-Facing Graph Tools (all four scenarios)
- architecture.md §Components §org-graph-tools
