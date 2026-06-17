---
name: canonicalize-project-root-at-source
description: expand-file-name project-root ONCE at create-session-core entry so both scope patterns and GPTEL_WORK_ROOT are absolute (and agree)
change: gptel-work-root-default-directory
status: ready
relations:
  - discovered-from:session-write-work-root
---

## Provenance
- discovered_from: session-write-work-root (author-blind review, cycle-1781716077, finding 3 spec-signal)
- discovered_by: reviewer
- discovered_class: spec-signal
- user_decision: "Canonicalize at source" (AskUserQuestion, cycle-1781716077 integrate close)

## Why
`jf/gptel-scope-profile--expand-string` substitutes `${project_root}` VERBATIM
(`replace-regexp-in-string … project-root … t t`; scope-profiles.org §expand-string),
NOT absolute-expanded. Cycle-1 made `GPTEL_WORK_ROOT` and the scope keys *agree* by
both being verbatim, so `register/invariant/cwd-scope-agreement` holds. But for a
relative / trailing-slash / `~` project-root the scope patterns are non-absolute and
won't match the absolute paths that relative tool-path resolution produces — latent
today only because real callers pass absolute roots. The user chose to canonicalize
at the source so the agreement is *absolute*-and-agreeing for ALL inputs.

## Files to modify
- `config/gptel/sessions/commands.org` — `jf/gptel--create-session-core`
- `config/gptel/sessions/test/commands/work-root-emission-spec.el` — extend
- `openspec/changes/gptel-work-root-default-directory/design.md` — correct D3 wording

## Implementation steps
1. In `jf/gptel--create-session-core`, canonicalize the incoming `project-root`
   ONCE at entry: `(setq project-root (and project-root (expand-file-name project-root)))`
   (or bind a `canonical-root` let and use it everywhere `project-root` currently flows).
   Feed THE SAME canonical value into BOTH `jf/gptel-scope-profile--create-for-session`
   (scope expansion) AND the `GPTEL_WORK_ROOT` drawer write. One canonical input → two
   absolute, byte-identical outputs ⇒ agreement is preserved AND both are absolute.
2. Keep the `GPTEL_WORK_ROOT` write itself VERBATIM relative to that canonical value
   (do NOT double-expand) — the agreement guarantee from cycle-1 stays intact because
   both sides consume the one canonicalized string.
3. Confirm nil handling unchanged (nil project-root → keyless, no `GPTEL_WORK_ROOT`,
   no scope `${project_root}` patterns).
4. Update `work-root-emission-spec.el`: the agreement test should now pass a
   NON-canonical input (e.g. relative `"proj"` or trailing-slash `"/x/proj/"`) and
   assert BOTH `GPTEL_WORK_ROOT` and the scope keys carry the ABSOLUTE canonical form,
   byte-identical to each other (strengthens the cycle-1 verbatim-agreement test to an
   absolute-agreement test).
5. Correct `design.md` D3: `${project_root}` is a verbatim string substitution, not an
   absolute expansion; the canonical-absolute form is now established by source-side
   `expand-file-name`, not by the renderer.
6. Tangle + validate: `./bin/tangle-org.sh config/gptel/sessions/commands.org`.

## Design rationale
Resolves the cycle-1 spec-signal. Canonicalizing once at the source is the most robust
fix: it upholds `register/invariant/cwd-scope-agreement` (one input → two outputs) AND
makes the scope patterns absolute so relative-path resolution lands in scope for any
input form — closing the latent gap without reopening the agreement obligation.

## Verification
- `./bin/run-tests.sh -d config/gptel/sessions` green; the strengthened agreement spec
  passes with a non-canonical input and would fail if either output were left non-absolute
  or the two diverged.
- Re-confirm `register/invariant/cwd-scope-agreement` (still confirmed; now absolute-and-agreeing).

## Context
register/invariant/cwd-scope-agreement; design.md D1/D3;
.orchestrator/cycles/cycle-1781716077/reconciliations/invariant-cwd-scope-agreement.md;
.orchestrator/cycles/cycle-1781716077/reviews/session-write-work-root.md (finding 3).
