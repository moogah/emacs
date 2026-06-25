---
name: canonicalize-project-root-at-source
description: expand-file-name project-root ONCE at create-session-core entry so both scope patterns and GPTEL_WORK_ROOT are absolute (and agree)
change: gptel-work-root-default-directory
status: done
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

## Observations

- DEPARTURE (none material): implemented the canonicalization as a guarded
  `(when project-root (setq project-root (expand-file-name project-root)))`
  immediately BEFORE the `let*` (rather than as the first `let*` binding). This
  mutates the parameter binding once at entry, so every downstream reference
  (`jf/gptel-scope-profile--create-for-session` and the `GPTEL_WORK_ROOT`
  append) reads the one canonical string with no further edits. Equivalent to
  the task's suggested `canonical-root` let, but touches fewer lines and keeps
  the `GPTEL_WORK_ROOT` write byte-for-byte verbatim relative to the canonical
  value (no double-expand). nil stays nil (keyless path unchanged).

- PRE-EXISTING FAILURE (NOT introduced by this task, NOT in scope): the sessions
  suite has ONE failing buttercup spec —
  `config/gptel/sessions/test/commands/identity-keys-emission-spec.el`
  ("jf/gptel-persistent-agent--task ... writes the agent's OWN
  :GPTEL_SESSION_ID: ..."). Root cause is an arity mismatch in an UNRELATED
  module: `jf/gptel-persistent-agent--build-scope-plist` was split to take
  `(read-paths write-paths)` (commit ff98330e, task agent-build-scope-plist-
  split) but its caller at `config/gptel/tools/persistent-agent.org` §567 /
  `.el:337` still passes a single `allowed-paths-list` arg →
  `(wrong-number-of-arguments ... 1)`. My changes touch only
  `config/gptel/sessions/commands.{org,el}` and the work-root spec; this failure
  reproduces independent of them. Filed as a cross-cutting follow-up in
  `.tasks/` (fix-persistent-agent-build-scope-plist-arity). All THREE work-root
  emission specs (including the new strengthened absolute-agreement case) PASS.

- TEST-QUALITY: the strengthened Case 2 drives the REAL verbatim-substitution
  contract (the renderer mock does a raw `replace-regexp-in-string`-style splice
  with NO `expand-file-name`), and iterates over a relative input (`"proj"`) and
  a trailing-slash input (`"/Users/x/proj/"`). It asserts the renderer RECEIVED
  the absolute `(expand-file-name input)` AND that both `:GPTEL_WORK_ROOT:` and
  the scope key carry that byte-identical absolute string. The mock cannot
  fabricate agreement because it never canonicalizes — only the production code
  can make the scope key absolute. A regression that left WORK_ROOT verbatim, or
  canonicalized only one side, fails visibly.

## Discoveries

- discovery_id: disc-canonicalize-project-root-at-source-1
  class: spec-signal
  description: |
    Resolved as intended. `register/invariant/cwd-scope-agreement` is now
    ABSOLUTE-and-agreeing rather than merely verbatim-agreeing: a single
    source-side `expand-file-name` canonicalizes `project-root` once at the
    entry of `jf/gptel--create-session-core`, and that one canonical-absolute
    string fans out to BOTH the `${project_root}` scope expansion and the
    `:GPTEL_WORK_ROOT:` write. The renderer
    (`jf/gptel-scope-profile--expand-string`) still substitutes VERBATIM, so the
    absolute form is established at the source, not the renderer — closing the
    latent gap where relative/trailing-slash/`~` roots produced non-absolute
    scope patterns that would not match the absolute paths relative-tool-path
    resolution produces. The agreement obligation did NOT reopen: the two
    outputs derive from one mutated binding, so they are byte-identical by
    construction.
  affected_register_entry: register/invariant/cwd-scope-agreement
  recommendation: |
    Reconfirm the entry with strengthened wording: "...the GPTEL_WORK_ROOT
    drawer value MUST equal — and now is byte-identical-and-ABSOLUTE to — the
    project-root used to expand GPTEL_SCOPE_*; a single source-side
    expand-file-name canonicalizes the one input before fan-out." Note in
    HISTORY that the renderer remains a verbatim substituter (NOT expand-file-
    name) and the absolute guarantee lives at the source, so any future renderer
    change cannot silently break absoluteness.

- discovery_id: disc-canonicalize-project-root-at-source-2
  class: invariant-gap
  description: |
    Out-of-scope but surfaced while running the sessions suite for verification:
    `jf/gptel-persistent-agent--build-scope-plist` (signature
    `(read-paths write-paths)`, change agent-build-scope-plist-split) has a
    caller in `config/gptel/tools/persistent-agent.org` §567 that still passes a
    single positional arg (`allowed-paths-list`), so the agent
    session-creation path errors with `wrong-number-of-arguments ... 1`. This
    fails `identity-keys-emission-spec.el` in the sessions suite. It is a
    pre-existing defect in the persistent-agent (tools) subsystem, unrelated to
    the work-root/project-root canonicalization, and was NOT introduced here.
  affected_register_entry: register/shape/scope-config-plist
  recommendation: |
    Fix the §567 caller to supply the read/write split the split function now
    expects (filed as .tasks/fix-persistent-agent-build-scope-plist-arity.md).
    Owned by the persistent-agent / agent-build-scope-plist-split change, not by
    this task.
