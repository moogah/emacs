---
name: disposition-ask-scratch-fallback-reachability
description: User disposition for ask-cycle-20260526-171719-2 — workspace-default-home-builder's *scratch* fallback is unreachable per the home-required-no-floating-workspaces invariant, yet exists and is tested; decide whether to drop or harden
change: add-workspace-home-directory
status: blocked
relations:
  - discovered-from:workspace-new-default-path
  - blocked-by:user-decision
---

<!-- Provenance fields (orchestrator schema):
     discovered_from: workspace-new-default-path
     discovered_by: reviewer
     discovered_class: spec-signal
     ask_id: ask-cycle-20260526-171719-2
     review_file: .orchestrator/cycles/cycle-20260526-171719/reviews/workspace-new-default-path.md
     status: blocked (awaiting user disposition; closes when next cycle's
       handshake.asks_for_user_resolved records the decision) -->

## The ask

`workspace-default-home-builder` in `config/workspaces/tabs.el`
contains a defensive `*scratch*` fallback:

```elisp
(defun workspace-default-home-builder (workspace-name)
  ...
  (let* ((ws (gethash workspace-name workspace--registry))
         (home (and ws (workspace--home ws))))
    (if home
        (find-file (workspace-home-org-path home))
      (switch-to-buffer (get-buffer-create "*scratch*")))))
```

The fallback is reachable **only** if a registry entry exists whose
`:home` slot is nil. The cited register entry
`register/invariant/home-required-no-floating-workspaces`
(load_bearing: true, status: reconciled cycle-3) claims structural
enforcement at two points:

- `workspace--make`: signature requires `(name home)` — non-floating
  by construction.
- `workspace--deserialize-state`: skips entries lacking `:home`
  (or with relative `:home` per cycle-3 absolute-path arm).

If both producers are the only ways to land an entry in the registry,
the fallback is dead code.

The new test in `workspace-new-default-spec.el` exercises the
fallback by **bypassing both producers** via direct `puthash` of a
plist constructed without `workspace--make`:

```elisp
(puthash "bare" (list :name "bare") workspace--registry)
```

The test passes — the fallback fires — but it does so by violating
the invariant the entry claims to enforce. **Either the invariant is
incomplete (third producer exists), or the test is exercising dead
code.**

## Why this requires user disposition (not auto-fix)

The two dispositions have opposite design implications, and the
choice cascades into how the orchestrator handles load-bearing
invariants going forward (see related architect finding
`arch-cycle-20260526-171719-02`).

## Options for the user

**Option A — Treat the invariant as load-bearing-and-complete: drop
the fallback and its test**

If `workspace--make` and the deserializer are genuinely the only
producers, the fallback is dead. Remove it. Drop the
"falls back to *scratch* when the workspace has no :home" `it` clause
from `workspace-new-default-spec.el`. Add a `cl-check-type` on
`workspace--make`'s `home` parameter so programmatic violation
signals at the constructor (closes the open question carried in the
reconciliation note for `home-required-no-floating-workspaces`).

Cost: small (one defun simplification, one test removal, one
constructor guard). Benefit: the invariant's enforcement section
matches reality. The orchestrator's load-bearing-invariant audit
(architect finding -02) becomes mechanically tractable for this
entry.

**Option B — Treat the invariant as documenting a goal, not a
contract: keep the fallback and add the structural lint**

Acknowledge that third producers may exist (third-party code; future
features; programmatic test setups) and that the fallback is a
genuine safety net. Schedule a follow-up architect-tier structural
audit that walks every `puthash workspace--registry` site in the
codebase and asserts the value is the result of `workspace--make`
or the deserializer (e.g. a regression-style lint similar to
`home-org-writer-lint-spec.el` but for registry-mutation sites).

Cost: medium (new structural lint module; verification cadence).
Benefit: load-bearing claim survives the audit; the fallback is
exercised by realistic-enough setups.

## Recommendation

The reviewer routed this as `spec-signal` because it's a load-bearing
invariant disposition, not an implementation defect. **Recommendation:
Option A** unless the user can name a concrete third producer.
"Defensive code for an unreachable branch" is an anti-pattern when
the unreachability is supposed to be load-bearing.

## Files to modify (per chosen option)

- Option A:
  - `config/workspaces/tabs.org` + `config/workspaces/tabs.el`:
    remove the `*scratch*` fallback from `workspace-default-home-builder`.
  - `config/workspaces/data-model.org` +
    `config/workspaces/data-model.el`: add
    `(cl-check-type home string)` (or richer guard) in `workspace--make`.
  - `config/workspaces/test/workspace-new-default-spec.el`: drop
    the "falls back to *scratch*" `it` clause.
- Option B:
  - New module `config/workspaces/test/registry-mutation-lint-spec.el`
    (or similar): structural audit over `puthash workspace--registry`
    call sites.
  - Update `interfaces.org` entry
    `register/invariant/home-required-no-floating-workspaces`'s
    enforcement_mechanism section to enumerate the new lint as a
    third structural-audit site.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

Option A: all 209 (or 208 after removal) specs pass; constructor
guard fires on a malformed call.
Option B: all 209 (or 210) specs pass; the new lint passes against
the current registry-mutation-site inventory.

## Cited register entries

- `register/invariant/home-required-no-floating-workspaces`
  (load_bearing: true) — the invariant whose enforcement the
  disposition resolves.
- `register/shape/workspace-plist-v3` — the shape whose constructor
  may gain a `cl-check-type` guard under Option A.

## Notes for the implementor

This task is **blocked on user disposition**. When the user resolves
`ask-cycle-20260526-171719-2` in a future cycle's handshake
(`asks_for_user_resolved`), the orchestrator will update this task's
`status: blocked → ready` and the chosen option becomes the
implementation contract.

Until then: do not implement.

## Context

- Reviewer file: `.orchestrator/cycles/cycle-20260526-171719/reviews/workspace-new-default-path.md`
- Reconciliation note: `.orchestrator/cycles/cycle-20260526-171719/reconciliations/invariant-home-required-no-floating-workspaces.md`
- Related architect finding: `arch-cycle-20260526-171719-02` (load-bearing-invariant enforcement-completeness audit)
- PM digest (this cycle): `.orchestrator/cycles/cycle-20260526-171719/pm-digest.md` § Asks for the user
