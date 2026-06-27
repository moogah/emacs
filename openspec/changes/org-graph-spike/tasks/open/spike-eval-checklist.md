---
name: spike-eval-checklist
description: Author the spike evaluation runbook so the user can systematically validate the layered system over the eval window before deciding the long-term shape.
change: org-graph-spike
status: blocked
relations:
  - blocked-by:wire-into-init
---

## Files to modify
- `config/org-graph/docs/spike-eval.org` (new) — the evaluation runbook

## Implementation steps
1. Write a checklist the user works through during the ~1-week eval window,
   organized by the re-centered capabilities:
   - **Discovery (vulpea-only):** create a workspace; confirm its `:home`
     gets watched and indexed via the `:on-create` handler; confirm `id:`
     links resolve cross-workspace on a fresh session (the startup seed);
     confirm `~/work` is NOT being walked wholesale.
   - **Taxonomy (vulpea-schema):** tag notes as each type; run the per-type
     finders; run `org-graph/validate-all-of-type` and eyeball violations —
     is the schema field set right, too strict, too loose?
   - **Typed edges:** add `:IMPLEMENTS:`/`:CONTRADICTS:`/`:SUPERSEDES:`/
     `:RELATES_TO:` to 3-5 roam concept notes; verify outgoing/incoming/
     connected queries; confirm project notes do NOT get typed edges.
   - **Agent surface:** from a workspace assistant, exercise `org-graph-query`,
     `org-graph-typed-edges`, and a coordinator-mediated `org-graph-write-node`;
     confirm `:agent-draft:` stamping and no file corruption under rapid calls.
   - **Coexistence:** confirm org-roam UX is unchanged throughout.
2. Capture the open questions to answer empirically: is `RELATES_TO` too broad
   (OQ4); does `vulpea-journal` add value (OQ5); is the parser-epoch re-run
   actually repopulating `typed_edges`; do un-ID'd notes still slip through.
3. End with the decision prompt: keep both, retire org-roam, fold org-graph
   into workspaces, or abandon — and what evidence would point each way.

## Design rationale
This is a spike: the deliverable is a system the user evaluates day-to-day,
and findings inform a follow-up change that decides the long-term shape
(proposal.md). A written runbook makes the evaluation systematic instead of
ad-hoc and ensures the resolved decisions (RE-1..RE-6, RE-2a) actually get
exercised and stress-tested.

## Design pattern
Plain org checklist with `[ ]` items grouped by capability, plus a
"Findings" section to fill in. Reference the open questions in design.md so
nothing gets evaluated in a vacuum.

## Verification
- The runbook exists, is tangle-free prose (no code to validate), and covers
  every resolved decision and open question.
- A dry read-through confirms each item is concretely checkable by the user
  without further explanation.

## Context
proposal.md (spike framing, findings inform follow-up);
design.md § Open Questions; design.md § Re-evaluation.
</content>

## Cycle 1782564058 updates (cycle-1782564058)
> Still blocked on `wire-into-init`. Context update only — no prose invalidated.

- The **Agent surface** eval section is now concretely backed: the three tools
  exist (`org_graph_query`, `org_graph_typed_edges`, `org_graph_write_node`) with
  coordinator-mediated, `:agent-draft:`-stamping `write-node`. When you write the
  runbook, name the snake_case tool identifiers the assistant will expose, and
  point the "no file corruption under rapid calls" check at
  `org-graph-coordinator/with-file-lock` (the confirmed unconditional lock).
