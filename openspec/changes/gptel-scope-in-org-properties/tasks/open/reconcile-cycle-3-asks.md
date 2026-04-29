---
name: reconcile-cycle-3-asks
description: Apply cycle-3 user dispositions on Asks 1, 2, 3 — register edits, cloud-provider loader-side normalizer with round-trip test, and drawer-emission spec/fixture rewrite.
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:ask-arch-cycle-1777478129-1
  - discovered-from:ask-arch-cycle-1777478129-2
  - discovered-from:ask-arch-cycle-1777478129-3
---

> Cycle-4 plan resolved the three cycle-3 spec-signal asks as: **Ask 1 → Option B + externalize** (register edits, parser/validator bug filed under `.tasks/`); **Ask 2 → Option B** (keywords + loader-side normalizer); **Ask 3 → Option A** (implementation-as-canonical: rewrite spec scenarios + fixture helper to match the writer's single-line emission).

## Cites register entries

- `register/vocabulary/operation-to-drawer-key` — Ask 1 edits the `:match-pattern` member: drop `redirect_target`; rewrite `decision_note` to record refuse-and-direct as the cycle-3-as-shipped contract; cite `--validate-file-operations` first-denial-wins as the binding constraint. Status flips `confirmed → reconciled`.
- `register/boundary/scope-expansion-action-handler` — Ask 1 renames Stage 2 to "match-pattern refuse-with-guidance"; documents the validator constraint. Status flips `divergent → reconciled`.
- `register/shape/scope-config-plist` — Ask 2 narrows `:cloud.allowed-providers` to "list of keywords; loader normalizes string drawer values to keywords at parse time". Status remains `confirmed` with sub-shape narrowed.
- `register/boundary/scope-config-loader` — Ask 2 adds the keyword normalizer at the parse boundary. Status remains `reconciled` (cycle-3 Option B applied).

## Files to modify

**Ask 1 (register-only, no code):**
- `interfaces.org:480-492` — `register/vocabulary/operation-to-drawer-key` `:match-pattern` member: drop `redirect_target: :read-directory`; rewrite `decision_note` to record refuse-and-direct disposition.
- `interfaces.org:806+` — `register/boundary/scope-expansion-action-handler`: rename Stage 2 from "match-pattern redirect" to "match-pattern refuse-with-guidance"; rewrite `notes:` to cite first-denial-wins; flip `status: speculated → reconciled` (or `divergent → reconciled` per the integrate-phase reconciliation note).

**Ask 2 (production code + register + test):**
- `interfaces.org` (register/shape/scope-config-plist `:cloud.allowed-providers`) — narrow to keywords with loader-side normalization.
- `config/gptel/scope/scope-validation.org` (and tangle-emitted `.el`) — at `--load-from-buffer` lines 530-532, wrap the `org-entry-get-multivalued-property` call with a string→keyword normalizer:
  ```elisp
  :allowed-providers
  (mapcar (lambda (s)
            (intern (concat ":" (string-remove-prefix ":" s))))
          (org-entry-get-multivalued-property
           (point) "GPTEL_SCOPE_CLOUD_PROVIDERS"))
  ```
  (or extract a tiny `--normalize-provider-keyword` helper if cleaner).
- `config/gptel/scope/test/validation/cloud-auth-spec.el` — add a round-trip test: drawer text `:GPTEL_SCOPE_CLOUD_PROVIDERS: aws gcp` → `--load-from-buffer` → assert `:allowed-providers` is `(:aws :gcp)` (keywords).
- `config/gptel/scope/test/validation/path-validation-spec.el` (or alongside the existing cycle-3 `loader empty-drawer behaviour` describe block) — extend the loader shape test to assert `:allowed-providers` is a keyword list when present.

**Ask 3 (spec + fixture, no code):**
- `openspec/changes/gptel-scope-in-org-properties/specs/gptel/scope-expansion/spec.md:103-116` — rewrite the two scenarios. Drop the `:GPTEL_SCOPE_<KEY>+:` continuation form; describe single-line space-separated emission (`":GPTEL_SCOPE_<KEY>: v1 v2 v3"`).
- `config/gptel/scope/test/helpers-spec.el:564-575` — rewrite `jf/gptel-test--render-drawer`: drop the `(if first "" "+")` branch; emit one line per key with values space-joined. Mirror what `org-entry-put-multivalued-property` emits.

## Implementation steps

### Step 1 — Ask 1 register edits (~15 min)

1. Open `interfaces.org`. Edit the `register/vocabulary/operation-to-drawer-key` `:match-pattern` member:
   - Remove `redirect_target: :read-directory`.
   - Replace the `decision_note:` with text along the lines of: "Cycle-2 ask-10B resolved 'smart redirect at the action handler'; cycle-3 implementation discovered that `--validate-file-operations` (`scope-validation.el:412`) throws on first denial via `(catch 'error-found ...)`, so the action handler never receives a violation cluster. Cycle-4 plan resolved Ask 1 as Option B: action handler refuses with directing user-error ('use c for custom pattern, e for manual edit'). The reify-the-cluster route is externalized to `.tasks/fix-match-pattern-parser-validator-boundary.md`."
2. Edit `register/boundary/scope-expansion-action-handler` Stage 2:
   - `name: match-pattern refuse-with-guidance` (was `match-pattern redirect`).
   - Rewrite `notes:` to describe the as-shipped refusal flow citing the validator constraint.
   - Flip `status: divergent → reconciled` if not already done at cycle-3 integrate.
3. Re-tangle if `interfaces.org` has tangle headers (`./bin/tangle-org.sh interfaces.org`).

### Step 2 — Ask 3 spec + fixture rewrite (~20 min)

1. Edit `openspec/changes/gptel-scope-in-org-properties/specs/gptel/scope-expansion/spec.md:103-116`. Replace the two `+:` continuation scenarios with a single scenario describing single-line space-separated emission. Keep the round-trip-via-`org-entry-get-multivalued-property` invariant scenario unchanged.
2. Edit `config/gptel/scope/test/helpers-spec.el:564-575`. Rewrite `jf/gptel-test--render-drawer` so it emits one line per key:
   ```elisp
   (push (format "%s: %s" key (mapconcat #'identity (mapcar #'format-val val) " ")) lines)
   ```
   (where `format-val` is whatever value-stringification you already use; verify there isn't already a helper).
3. Run the expansion suite to confirm no test regressed: `./bin/run-tests.sh -d config/gptel/scope/test/expansion`.

### Step 3 — Ask 2 register edit (~5 min)

Edit `interfaces.org` `register/shape/scope-config-plist`. In the `:cloud.allowed-providers` description, narrow to "list of keywords (e.g. `:aws`, `:gcp`); the loader at `--load-from-buffer` normalizes string drawer values (`GPTEL_SCOPE_CLOUD_PROVIDERS`) to keywords at parse time."

### Step 4 — Ask 2 loader normalizer (~30 min)

1. Open `config/gptel/scope/scope-validation.org`. Locate the `--load-from-buffer` block (around lines 530-532 in the tangled `.el`).
2. Add the string→keyword normalizer at the `:allowed-providers` emission. A small inline `mapcar` is fine:
   ```elisp
   :allowed-providers
   (mapcar (lambda (s)
             (intern (concat ":" (string-remove-prefix ":" s))))
           (org-entry-get-multivalued-property
            (point) "GPTEL_SCOPE_CLOUD_PROVIDERS"))
   ```
3. Tangle and validate: `./bin/tangle-org.sh config/gptel/scope/scope-validation.org`.
4. Confirm no existing test regresses on the loader output shape: `./bin/run-tests.sh -d config/gptel/scope/test/validation`.

### Step 5 — Ask 2 round-trip test (~25 min)

1. Open `config/gptel/scope/test/validation/cloud-auth-spec.el`. Add a `describe` block "loader normalizes provider names to keywords" near the existing cycle-2 drawer-fixture block (~lines 84-103).
2. Inside, an `it` block fixtures a session.org with `:GPTEL_SCOPE_CLOUD_PROVIDERS: aws gcp`, calls `jf/gptel-scope--load-from-buffer`, and asserts `(plist-get config :cloud)` `:allowed-providers` equals `'(:aws :gcp)` (keyword list, not string list).
3. A second `it` block: empty `:GPTEL_SCOPE_CLOUD_PROVIDERS` → `:allowed-providers` is `nil` (no list of empty keywords).
4. Run: `./bin/run-tests.sh -d config/gptel/scope/test/validation`. Both new tests pass; no existing test regressed.

### Step 6 — Full scope suite check (~5 min)

```bash
./bin/run-tests.sh -d config/gptel/scope --report
```

Expected delta vs cycle-3 baseline (1631 specs, 80 failed): 1631+2 specs (or +3 if you split the round-trip test), 80 failed unchanged or 79 (if a previously-failing related test now passes). Any new failure is a regression — fix before review.

## Verification

- `grep -n 'redirect_target' interfaces.org` does not include the `:match-pattern` member.
- `grep -n 'match-pattern redirect' interfaces.org` returns nothing (the Stage 2 name is now "match-pattern refuse-with-guidance").
- `grep -n ':GPTEL_SCOPE_READ+:' openspec/changes/gptel-scope-in-org-properties/specs/gptel/scope-expansion/spec.md` returns nothing.
- `grep -n '(if first "" "+")' config/gptel/scope/test/helpers-spec.el` returns nothing.
- `grep -n 'allowed-providers' config/gptel/scope/scope-validation.el` shows the normalizer at the loader, and `(member provider allowed-providers)` at the validator now compares keywords against keywords.
- `./bin/run-tests.sh -d config/gptel/scope/test/validation` passes (180+2 ran, with the two new round-trip tests green).
- `./bin/run-tests.sh -d config/gptel/scope/test/expansion` passes (no regression from the fixture-helper rewrite).

## Design rationale

Three orthogonal cycle-3 spec-signal findings, each with a user disposition, all touching reconciliation surfaces in the same change. Bundling into one task keeps the cycle-4 batch tight; the steps are independent enough that they can be implemented in any order, and any single-step regression is recoverable without unwinding the others.

The Ask 2 normalizer is the only production-code surface in this task — review attention should focus there. Asks 1 and 3 are pure register/spec/fixture documentation edits; the test suite verifies they don't inadvertently break round-trip.

## Context

- Cycle-3 PM digest § "Asks for the user": `.orchestrator/cycles/cycle-1777478129/pm-digest.md`
- Architect finding for Ask 1: `.orchestrator/cycles/cycle-1777478129/findings/arch-cycle-1777478129-4.md`
- Architect finding for Ask 3: `.orchestrator/cycles/cycle-1777478129/findings/arch-cycle-1777478129-5.md`
- Reviewer finding for Ask 2: `.orchestrator/cycles/cycle-1777478129/reviews/migrate-validation-tests.md` § F2
- Externalized follow-up for Ask 1's deeper parser/validator bug: `.tasks/fix-match-pattern-parser-validator-boundary.md`
- Cycle-4 plan dispositions recorded in: `.orchestrator/state.json` § `tasks[].discovered_from`
