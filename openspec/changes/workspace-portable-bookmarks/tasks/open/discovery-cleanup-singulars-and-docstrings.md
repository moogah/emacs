---
name: discovery-cleanup-singulars-and-docstrings
description: Delete the two test-only singular discovery functions, move their specs onto the plurals, and correct two anchoring docstrings that overstate "never where a link points"
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/boundary/git-derived-checkout-discovery
  - register/vocabulary/bookmark-resolution-outcome
relations:
  - discovered-from:arch-cycle-1789935943-eoc-02
  - discovered-from:arch-cycle-1789935943-eoc-07
  - relates-to:missing-repo-choice
discovered_from: [arch-cycle-1789935943-eoc-02, arch-cycle-1789935943-eoc-07]
discovered_by: architect
discovered_class: dead-branch
created_at: 2026-09-26T14:00:00-04:00
---

## Summary

After the plural discovery functions landed (discovery-refinements) and the resolver was
written against them (resolver-outcomes, Amendment B), `workspace-bookmark--workspace-checkout`
and `workspace-bookmark--canonical-for-identity` have no production caller; the uniqueness
test they encode is inlined in `--resolve-coordinates`. Delete them and re-home their specs.
While in the resolver section, fix two docstrings: anchoring decides LOCATION by spelling,
but presence and kind are asked of the filesystem, which follows links.

## Cited register entries

- `register/boundary/git-derived-checkout-discovery` (reconciled this cycle): its
  `functions` list must drop the two singulars once they are gone. Pressure-test: is a
  one-arm "the unique checkout" helper wanted by `missing-repo-choice`? If so, add it WITH a
  caller in that task, not here.
- `register/vocabulary/bookmark-resolution-outcome` (reconciled): consumers compare `:path`
  / `:checkout` by file identity, never by string (workspace spelling vs canonical truename).

## Implementation steps

1. In `config/workspaces/portable-bookmarks.org` § "A workspace's checkouts" and § "The
   safety rule, and ambiguity", remove `--workspace-checkout` and `--canonical-for-identity`
   (`.el:718-730`, `:800-810` at 20944160) and their prose.
2. In `portable-bookmarks-discovery-spec.el` (~213-249, 251-290, 1017-1035, 1466-1530), rewrite
   the singular specs against `--workspace-checkouts` / `--canonical-checkouts-for-identity`:
   "is nil when none / several" becomes "lists both, and neither is unique".
3. Docstrings (`.el:872-873`, `:922-924`): "PATH is the answer by its spelling; whether
   something of RELPATH's kind exists there is asked of the filesystem, which follows links,
   so a dangling link is absent."
4. `grep -rn "workspace-bookmark--workspace-checkout\b\|workspace-bookmark--canonical-for-identity" config/` returns nothing.
5. Tangle and validate; all specs green.

## Verification
- `./bin/tangle-org.sh config/workspaces/portable-bookmarks.org` exits 0.
- `./bin/run-tests.sh -d config/workspaces` all green; full suite = baseline set.

## Out-of-scope
The identity memo / cost contract (`discovery-identity-memo-and-cost-contract`). Any change
to outcome semantics.

## Context
- `.orchestrator/cycles/cycle-1789935943/findings/end-of-cycle-1789935943.md` § Findings 2 and 7
- meta-discovery M4 (discovery returns lists; uniqueness is the consumer's decision)

## Observations

- The task body cites four spec ranges (~213-249, 251-290, 1017-1035, 1466-1530 at
  20944160) for the singular specs; the spec file actually had 44 singular call
  sites at that commit and at the base (e6974126), spread over 27 `it` blocks in
  ten `describe`s. All 44 were re-homed. Where a singular expectation sat next to
  the plural asserting the same list, the singular was dropped rather than
  duplicated (two whole specs became pure duplicates and were removed: "finds the
  checkout inside the workspace's home" folded into "lists exactly the one
  checkout ...", and the unit "canonical-for-identity is nil for a nil identity",
  whose plural twin already existed). Directory suite: 655 -> 653 specs, 0 failed.
- Deviation, recorded here: the resolver's "The decision" prose (org § "The
  decision", right under the outcome table) said the plurals are asked "never
  the singulars, which answer nil for both ...". With the singulars gone that
  sentence pointed at nothing, so it was reworded to say uniqueness is read off
  the list in the resolver and why no single-valued helper exists. This is
  outside the two discovery sections and two docstrings the brief named, but the
  parallel task does not touch that section, so the merge-conflict rationale for
  the restriction does not apply. No code changed there.
- Pressure test on `register/boundary/git-derived-checkout-discovery`: no one-arm
  "the unique checkout" helper is wanted by `missing-repo-choice`. Its task body
  works from the resolution plist's `:checkout` and says "never re-run discovery
  here" (steps 2 and 6), and `--resolve-coordinates` already inlines the
  `(and xs (null (cdr xs)))` test for both plurals. Nothing was added.
- `register/vocabulary/bookmark-resolution-outcome` file-identity tag: the
  re-homed specs assert discovery output directly (not `:path` / `:checkout` of a
  resolution plist) and keep this file's existing convention of `:to-equal` on
  fixture paths that `workspace-bookmark-fixtures-temp-dir` already truenamed;
  the one helper that reasons about "same checkout" across spellings
  (`--expect-offered-main-only`) still uses `workspace-bookmark--same-file-p`.
  No string comparison of a resolution's `:path` / `:checkout` was introduced.
- The "several main clones from a third workspace" spec previously asserted only
  `(length ...) = 2`; it now asserts the sorted member set `(x1 x2)`, since
  registry walk order for non-current workspaces is not something this spec
  should pin.
- Adjacent, out of scope: three cycle-1 task files under `tasks/open/`
  (`checkout-discovery.md` steps 4-5, `resolver-outcomes.md` :379) still describe
  the singulars as things to add or call. They are historical implementation
  steps of closed-in-spirit tasks, not live instructions; left untouched.

## Discoveries

- discovery_id: disc-discovery-cleanup-singulars-and-docstrings-1
  class: dead-branch
  description: |
    `workspace-bookmark--workspace-checkout` and
    `workspace-bookmark--canonical-for-identity` are deleted from
    portable-bookmarks.org (§ "A workspace's checkouts", § "The safety rule,
    and ambiguity") and the tangled .el. Confirmed before deletion that their
    only callers in config/ were their own 44 spec sites; production
    (`--resolve-coordinates`) calls the plurals and inlines the uniqueness
    test. The register's `functions` list still names both singulars with the
    note "delete in a follow-up".
  affected_register_entry: register/boundary/git-derived-checkout-discovery
  recommendation: |
    In `functions`: remove the two sentences "The singulars --workspace-checkout
    (:718) and --canonical-for-identity (:800) have no production caller
    (end-of-cycle finding 2: delete in a follow-up)." and the owner comment
    beneath them. The line numbers cited elsewhere in the entry shift by -14
    after :812 and a further -12 after :894 (against e6974126); the parallel
    task's memo edits will move them again, so re-anchor once both merge. No
    one-arm helper was added (missing-repo-choice does not want one; see
    Observations), so the list gains nothing.

- discovery_id: disc-discovery-cleanup-singulars-and-docstrings-2
  class: spec-signal
  description: |
    The two anchoring docstrings (`--confined-relpath-p`, `--anchor`) now say
    what the register's member text already said: location is decided by
    spelling; presence and kind are asked of the filesystem, which follows
    links, so a link leaving the checkout still answers the in-checkout
    spelling and a dangling link is absent. Behaviour unchanged. The register
    entry's status_note anticipated this and said no register change follows.
  affected_register_entry: register/vocabulary/bookmark-resolution-outcome
  recommendation: |
    None required. The status_note sentence naming this task's docstring
    rewrite as pending can be marked done at reconcile.
