---
name: finders-and-filters
description: Implement per-taxonomy finder commands (find-topic, find-debug, etc.) backed by org-node-find with filetag predicates, test-first.
change: org-graph-spike
status: ready
relations:
  - "blocked-by:test-helpers"
  - "blocked-by:install-packages"
---

## Files to modify

- `config/org-graph/test/finders/filetag-filter-spec.el` (new) — Buttercup spec, written first.
- `config/org-graph/org-graph.org` (modify) — fill the `Finders` subtree.

## Implementation steps

1. Write the spec first. `describe "org-graph-finders--filetag-predicate"` with these `it` blocks:
   - returns t for a candidate carrying the requested filetag.
   - returns nil for a candidate without the requested filetag.
   - returns nil for a candidate with no filetags at all.
   - returns t for a candidate carrying multiple filetags one of which matches.

   The candidate object should be constructed via a lightweight fixture (a plist or struct with a `:tags` slot — match whatever org-node's candidate shape is; check `org-node-find` source if uncertain).

   Add a `describe "org-graph/find-*"` outer block with one `it` per command verifying it calls `org-node-find` with the correct filetag predicate. Mock `org-node-find` via `cl-letf` and capture the predicate argument; assert behavior on the captured predicate.

2. Implement `org-graph-finders--filetag-predicate (tag)` in the `Finders` subtree:
   - Returns a `(lambda (candidate) ...)` that checks whether `tag` (symbol) is in the candidate's filetags.

3. Implement the seven interactive commands:
   - `org-graph/find-topic` — predicate filters on `topic`.
   - `org-graph/find-debug` — `debug`.
   - `org-graph/find-log` — `log`.
   - `org-graph/find-reference` — `reference`.
   - `org-graph/find-project` — `project`.
   - `org-graph/find-any` — no filter (delegates to plain `org-node-find`).
   - `org-graph/find-agent-drafts` — `agent-draft`.

   Each is two lines: an interactive declaration and a call to `(org-node-find :predicate (org-graph-finders--filetag-predicate '<tag>))`. If `org-node-find`'s actual API uses a different keyword, adapt at implementation time.

4. By default, the topic / reference finders should EXCLUDE `agent-draft` candidates. Implement this by composing predicates: `(and (has-tag <type>) (not (has-tag agent-draft)))`. Make the exclusion a defcustom: `org-graph-exclude-drafts-from` defaulting to `'(topic reference)`.

5. Run tests until green: `./bin/run-tests.sh -d config/org-graph/test/finders`.

6. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org`.

## Design rationale

Each finder is intentionally trivial — a one-line predicate plus an `org-node-find` call. Building per-type finders rather than one parameterized command makes them keybind-friendly and gives users muscle memory like `M-x org-graph/find-topic`. The `agent-draft` exclusion default reflects spec scenario "Agent-authored note carries draft tag" — users invoke `find-topic` and don't want noise from drafts; they review drafts via `find-agent-drafts` explicitly.

## Verification

- `./bin/run-tests.sh -d config/org-graph/test/finders` — green.
- `grep -nE "defun org-graph/find-(topic|debug|log|reference|project|any|agent-drafts)" config/org-graph/org-graph.el` — 7 matches.
- `grep -n "defcustom org-graph-exclude-drafts-from" config/org-graph/org-graph.el` — 1 match.

## Context

- specs/org-graph/spec.md §Note-Type Taxonomy and Finders
- specs/org-graph/spec.md §Agent-Facing Graph Tools (the agent-draft exclusion behavior)
- architecture.md §Components §org-graph-finders
