---
name: docs-and-directionality
description: Update workspaces docs/prose for the menu + integrations + stub removal; confirm the directionality lint still passes
change: workspace-integration-menu
status: blocked
relations:
  - blocked-by:wire-create-dispatch
  - blocked-by:workspaces-transient-menu
  - blocked-by:gptel-session-integration
---

## Files to modify
- `config/workspaces/docs/README.org` (modify — document the menu + integrations + delete stale `*-initial.org` mentions)
- `config/workspaces/test/gptel-integration-spec.el` (modify — extend directionality grep to new files if it enumerates files explicitly)

## Implementation steps

1. **README.org**: add a section describing the `C-x w` transient menu (three
   states), the integration registry concept (how a subsystem registers
   `:on-create` / `:menu`), and the gptel-session integration as the first
   client. Remove/replace any description of the old eager
   `<date>-initial.org` scaffold session — it no longer exists; `sessions/` is
   created empty and populated by integrations on demand or at birth.

2. **Directionality lint**: confirm the existing grep test (the one asserting
   no `gptel-sessions-*` symbol appears under `config/workspaces/`) still
   passes against the NEW files (`integrations.el`, `workspaces-transient.el`).
   If the test enumerates files explicitly rather than globbing the directory,
   add the new tangled files to its list. The new workspaces code must name no
   gptel symbol (the gptel handler lives under `config/gptel/`, attached via
   `workspace-register-integration`).

3. **Sweep stale references**: `grep -rn "initial.org\|scaffold-initial-session"
   config/workspaces/` — fix any lingering prose in `scaffold.org`,
   `tabs.org`, `data-model.org`, `docs/README.org` left from the removal.

## Design rationale
The change removes a user-visible artifact (the eager initial session file) and
adds a user-visible surface (the menu + integrations), so the package docs must
track both. The directionality lint is the structural guarantee that the
registry — not a direct call — remains the only bridge between workspaces and
gptel; this task confirms the new files honor it.

## Verification
- `./bin/tangle-org.sh config/workspaces/docs/README.org` (if it tangles) or confirm it is prose-only
- `grep -rn "gptel-sessions-" config/workspaces/` returns nothing
- `grep -rn "initial.org\|scaffold-initial-session" config/workspaces/` returns only intentional historical notes (ideally nothing)
- `./bin/run-tests.sh -d config/workspaces`
- Done when: docs describe the menu + integrations; no stale stub references; directionality lint green.

## Context
design.md § Decision 6 and § Risks; spec `workspace-integrations` (Registry is
the published boundary); proposal § Impact.

## Observations

- README.org is documentation prose-only: its header carries no `:tangle`
  property and no `#+auto_tangle`, so no tangle step was required (confirmed by
  reading the header).
- data-model.org edit was confined to PROSE outside the babel block
  (the "Sessions directory helper" descriptive paragraph, above the
  `#+begin_src`). `git diff config/workspaces/data-model.el` is empty —
  the generated `.el` is unaffected, so no tangle/commit of the `.el` was
  needed.
- Directionality lint already globs: the test at
  `config/workspaces/test/gptel-integration-spec.el` uses
  `directory-files-recursively` over `config/workspaces/` (filtering out
  `/test/`), so it ALREADY covers the new `integrations.el` and
  `workspaces-transient.el` without enumerating files. No test edit was
  needed; left unchanged.
- README inline mnemonic references (e.g. `(=C-x w S=)`) in the body prose
  were left as-is. The new "The =C-x w= menu" section explicitly states the
  individual chords are retired and that the old mnemonics survive as the
  in-menu keys; the Command-reference table was relabelled from "Binding"
  (=C-x w n=) to "Menu key" (=n=) so the table no longer asserts a stale
  global chord. Rewriting every inline `(=C-x w X=)` was out of scope for a
  docs-touch task and risked churn; the menu section frames them correctly.
- The single remaining `initial.org` grep hit (README:66) is an intentional
  past-tense note ("`sessions/` is no longer seeded with an eager
  `<date>-initial.org` file"), describing the removed artifact — permitted by
  the task ("only an intentional historical note clearly marked as
  past-tense/removed").
- All `gptel-sessions-` matches under `config/workspaces/**/*.el` are in test
  comments only (the lint itself + scaffold-spec comment); zero symbol
  references in production source.
- Test suite green at 283 specs / 0 failed (baseline parity), the
  directionality lint among them.

## Discoveries

None
