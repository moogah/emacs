---
name: harden-add-to-scope-action-handler
description: "Upstream guard at the --add-to-scope action handler — refuse on nil-operation bash violations (cloud-auth, parse-incomplete), redirect :match-pattern to the sibling :read-directory operation, and only delegate to the writer for the writer's ten-member operation domain."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:rewire-expansion-writer
  - discovered-from:implement-drawer-writer
  - discovered-from:disposition-match-pattern-handling
---

## Cites register entries

- `register/boundary/scope-expansion-action-handler` — NEW cycle-2-plan entry; this task is the implementation of that entry's three stages.
- `register/vocabulary/operation-to-drawer-key` — `unmapped_policy: error` documents that the writer rejects `nil` and `:match-pattern`; the upstream fix lives at the action layer (this task).
- `register/shape/violation-info` — the `:operation` field is nil for cloud-auth and parse-incomplete violations.

## Background

Two cycle-1-discovered failure modes converge at the same code layer
(the `--add-to-scope` action handler in `scope-expansion.org`), so
this task addresses them together:

**Failure mode 1 — nil :operation (finding-9)**: the writer's strict-
error fallback now errors when `:operation` is nil (cloud-auth or
parse-incomplete violations). Correct at the writer layer, but
user-hostile — surfaces as a generic Lisp error inside the transient
menu's callback.

**Failure mode 2 — :match-pattern (finding-10B, ask 10B disposition
2026-04-29)**: `find /home -name "*.txt"` emits both a
`:read-directory /home` violation and a `:match-pattern '*.txt'`
violation. Pressing `a` on the latter previously routed `*.txt` into
`:GPTEL_SCOPE_READ:`, granting fs-wide read on the pattern. User
disposition: smart-redirect to the sibling `:read-directory`.

## Implementation

In `config/gptel/scope/scope-expansion.org`:

### Stage 1: nil-operation refuse

At the top of `--add-to-scope` (and `--add-path-to-scope`,
`--add-bash-to-scope` if they're entered directly), check
`(plist-get violation :operation)`. If nil, branch on
`(plist-get violation :validation-type)`:

- `:cloud-auth` → prompt "add provider `<provider>` to allow-list?"
  The action writes to `:GPTEL_SCOPE_CLOUD_PROVIDERS:` rather than a
  path bucket. (`:provider` is on the violation plist.)
- `:parse-incomplete` → user-error: "this command could not be parsed;
  review and edit it manually". Surface the bash command for
  inspection. No drawer write.
- any other `:validation-type` with nil operation → user-error: "no
  operation associated with this violation; cannot add to scope".
  Suggest allow-once.

### Stage 2: :match-pattern redirect

When `(plist-get violation :operation)` is `:match-pattern`:

1. Scan the current violation cluster (the list of violations the UI
   surfaced together, accessible from the expansion-menu state) for a
   sibling violation with `:operation :read-directory` whose
   `:resource` is the search root (a directory path the pattern was
   evaluated against).
2. If a sibling is found, redirect the add-to-scope action to use the
   sibling's `:resource` (path) — so a `find /home -name '*.txt'`
   add-to-scope on the pattern adds `/home` (or its glob-form, e.g.
   `/home/**`) to `:GPTEL_SCOPE_READ:` rather than `*.txt`.
3. If no sibling `:read-directory` is in the cluster (rare, e.g. a
   top-level `find / -name X`), surface a user-error: "this pattern
   was evaluated against an unknown root; scope the search root
   explicitly via `--add-path-to-scope` instead".

The writer must not see `:match-pattern`; the writer's
`canonical_mapping_function` (per cycle-2 register update) errors
loudly if it does.

### Stage 3: transient menu re-labelling

`jf/gptel-scope-expansion-menu` should hide / relabel `a`:

- Hide when `:operation` is nil and `:validation-type` is
  `:parse-incomplete`.
- Relabel as "add provider to allow-list" when
  `:validation-type` is `:cloud-auth` and `:operation` is nil.
- Relabel as "add /home (search root) to scope" when `:operation` is
  `:match-pattern` AND a sibling `:read-directory` exists in the
  cluster.

### Stage 4: Decision 7 Bug 2 — thread no-op signal back to callback

**Discovered-from**: cycle-2 reviewer finding on `rewire-expansion-writer`
(`.orchestrator/cycles/cycle-1777470320/reviews/rewire-expansion-writer.md`,
Finding 2, advisory). design.md Decision 7 originally scoped this fold-in
to `rewire-expansion-writer`; integrate-phase deferred it here because
it's an action-layer guard, not a writer-layer change.

The outer action handlers (`--add-to-scope`, `--add-wildcard-to-scope`,
`--add-custom-to-scope`) currently fire the callback unconditionally with
`:success t :patterns_added (vconcat patterns)` after delegating to
`--write-pattern-to-drawer`. This is wrong when the writer was a no-op:

- Bare-command branch in `--add-bash-to-scope` returns `nil` without
  writing anything, but the outer handler still claims `:success t :patterns_added [<pattern>]`.
- Dedup branch in `--write-pattern-to-drawer` returns `nil` (pattern
  already present), but the outer handler also claims `:success t :patterns_added [<pattern>]`.

Both produce a false-success the LLM acts on (retry-and-refail or
trust-and-proceed). Fix: inspect the writer's return value
(non-nil → wrote; nil → no-op) and emit either:

- `:success t :patterns_added [<pattern>]` when the writer wrote, or
- `:success nil :reason "command-level expansion not supported"` (bare
  command branch) or
- `:success t :patterns_added []` `:message "Pattern already in scope"`
  (dedup branch).

This is a single-site fix at the outer action handlers; the writer's
return contract (non-nil pattern on write, nil on no-op) is already
correct as of cycle-2.

### Tests

Add a buttercup spec asserting:
- `:cloud-auth` violation → `add-to-scope` writes to
  `:GPTEL_SCOPE_CLOUD_PROVIDERS:`, not to a path bucket.
- `:parse-incomplete` violation → `add-to-scope` errors with a
  useful message that does not reach the writer.
- `:bash` violation with `:operation nil` (defensive, shouldn't
  happen but worth pinning) → user-error.
- `:match-pattern` violation with sibling `:read-directory` →
  `add-to-scope` writes the sibling's `:resource` to
  `:GPTEL_SCOPE_READ:`, not the pattern.
- `:match-pattern` violation without sibling `:read-directory` →
  user-error; writer not invoked.
- `:match-pattern` violation reaching the writer (defect contract) →
  writer signals the cycle-2 strict-error message.
- (Stage 4) Bare-command bash violation → action handler emits
  `:success nil :reason "command-level expansion not supported"`,
  not a false-success.
- (Stage 4) Pattern already in scope (writer dedup short-circuit) →
  action handler emits `:success t :patterns_added [] :message
  "Pattern already in scope"`, not a phantom-add.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/expansion
```

The new spec runs alongside `test-expansion-menu-spec.el` (or wherever
the existing menu specs live).
