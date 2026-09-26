---
name: discovery-cleanup-singulars-and-docstrings
description: Delete the two test-only singular discovery functions, move their specs onto the plurals, and correct two anchoring docstrings that overstate "never where a link points"
change: workspace-portable-bookmarks
status: ready
task_class: refactor
on_critical_path: false
cycle: cycle-2
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
