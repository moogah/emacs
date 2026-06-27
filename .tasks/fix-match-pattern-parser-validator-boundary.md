---
name: fix-match-pattern-parser-validator-boundary
description: Bash-parser emits redundant :match-pattern operations whose :search-scope the validator ignores; resolve at the parser/validator boundary so find/ls/grep -l add-to-scope works end-to-end.
status: ready
source: openspec/changes/gptel-scope-in-org-properties
relations:
  - discovered-from:harden-add-to-scope-action-handler
  - discovered-from:ask-arch-cycle-1777478129-1
---

> Surfaced during cycle-4 plan of `gptel-scope-in-org-properties` while dispositioning cycle-3 user ask 1 (Stage 2 `:match-pattern` redirect-vs-refusal). Recording here so the next maintainer of bash-parser + scope-validation can pick it up; out of scope for the drawer-migration change.

## The bug in one sentence

For `find /home -name "*.txt"`, the bash-parser emits two operations (`:read-directory /home` and `:match-pattern "*.txt" :search-scope "/home"`); the validator ignores the `:search-scope` and resolves `*.txt` against the cwd, so even after the user adds `/home` to scope, the second op still denies — the find command remains broken end-to-end.

## Evidence

`config/bash-parser/commands/find.el:60-68` — parser correctly attaches `:search-scope`:
```elisp
(when name-pattern
  (push (list :file name-pattern
              :operation :match-pattern
              :confidence :high
              :source :flag-arg
              :pattern t
              :search-scope search-dir
              :command command)
        operations))
```

`config/gptel/scope/scope-validation.el:397-402` — validator ignores `:search-scope`:
```elisp
(let* ((operation (plist-get file-op :operation))
       (path (plist-get file-op :file))
       (resolved-path (when path
                        (if (file-name-absolute-p path)
                            (expand-file-name path)
                          (expand-file-name path directory)))))
  ...)
```

For `:match-pattern "*.txt"` from `find /home -name "*.txt"`, `resolved-path` becomes `<cwd>/*.txt`, never `/home/*.txt`. The `:search-scope` carried through from the parser is never consulted.

## Producers of `:match-pattern`

Three command handlers emit `:match-pattern`:

- `config/bash-parser/commands/find.el` — explicit `:search-scope` on each emission.
- `config/bash-parser/commands/ls.el` — implicit (cwd-relative); no `:search-scope`.
- `config/bash-parser/commands/grep.el` — implicit (cwd-relative); no `:search-scope`.

Plus a flow path in `config/bash-parser/analysis/bash-parser-orchestrator.el:749+` that links `:match-pattern` from command substitutions (e.g. `cat $(find /tmp -name "*.txt")`) to outer commands.

## Two viable fixes (pick one and design)

**Option C1 (parser-side, cleanest):** Drop `:match-pattern` as a separate operation in `find.el`. Emit a single op against the search root with the pattern attached as metadata (e.g. `:read-directory /home :filter "*.txt"`). Eliminates the cluster ambiguity. `ls *.txt` and `grep -l pat *.txt` need a separate decision (their pattern is the path, not a filter on a search root).

**Option C2 (validator-side, smaller surface):** When `:operation == :match-pattern` and `:search-scope` is present, use `:search-scope` as the resolution directory instead of `directory`. One conditional in `validate-file-operation`. Keeps the parser op duality. Doesn't help for `ls`/`grep -l` (no `:search-scope`).

## Why this matters

Two register entries in `interfaces.org` (`register/vocabulary/operation-to-drawer-key` `:match-pattern` member; `register/boundary/scope-expansion-action-handler` Stage 2) currently encode an action-handler "smart redirect" that tried to paper over this without the architect realising the validator itself was discarding the parse-time information needed to make `:match-pattern` resolvable. Cycle-4 of `gptel-scope-in-org-properties` records refuse-with-guidance as the as-shipped Stage 2 contract, citing first-denial-wins as the binding constraint — but the underlying defect is one layer deeper than first-denial-wins: even if the action handler saw the cluster, the validator would still resolve the pattern wrong.

## Recommended next step

Open a small openspec change under `gptel-scope-validation-pattern-resolution` (or similar name). Decide between C1 and C2 in the proposal. Both are bounded; C1 is one-handler surgery; C2 is a one-line conditional plus tests covering the `:search-scope`-present and `:search-scope`-absent paths.

When this lands, the cycle-3 architect finding `arch-cycle-1777478129-4` (and the as-shipped refuse-with-guidance Stage 2) can be reconsidered: with `:match-pattern` resolved correctly, the action handler's Stage 2 may simplify or disappear entirely.

## Context

- Architect finding: `.orchestrator/cycles/cycle-1777478129/findings/arch-cycle-1777478129-4.md`
- Reconciliation note: `.orchestrator/cycles/cycle-1777478129/reconciliations/boundary-scope-expansion-action-handler.md`
- User ask: `ask-arch-cycle-1777478129-1` (resolved Option B + externalize, cycle-4 plan, 2026-04-29)
- Source change archive (after archival): `openspec/archive/gptel-scope-in-org-properties/`
