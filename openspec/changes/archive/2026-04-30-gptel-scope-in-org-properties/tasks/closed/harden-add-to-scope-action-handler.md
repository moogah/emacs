---
name: harden-add-to-scope-action-handler
description: "Upstream guard at the --add-to-scope action handler — refuse on nil-operation bash violations (cloud-auth, parse-incomplete), redirect :match-pattern to the sibling :read-directory operation, and only delegate to the writer for the writer's ten-member operation domain."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-expansion-transient-and-queue-register-entries
  - discovered-from:implement-drawer-writer
  - discovered-from:disposition-match-pattern-handling
  - discovered-from:rewire-expansion-writer
---

## Cites register entries

- `register/boundary/scope-expansion-action-handler` — NEW cycle-2-plan entry; this task is the implementation of that entry's three stages.
- `register/vocabulary/operation-to-drawer-key` — `unmapped_policy: error` documents that the writer rejects `nil` and `:match-pattern`; the upstream fix lives at the action layer (this task).
- `register/shape/violation-info` — the `:operation` field is nil for cloud-auth and parse-incomplete violations.
- `register/shape/expansion-transient-scope` — NEW cycle-3 entry (created by `add-expansion-transient-and-queue-register-entries`). The 5-key plist on `(transient-scope)` — `:violation :callback :patterns :tool-name :chat-buffer`; cite when reading any of these. (The plan brief originally proposed `:command-name`, but the as-built shape has no such key — see entry's `status_note`.)
- `register/invariant/expansion-queue-always-progresses` — NEW cycle-3 entry. Stage 4's refusal/dedup-short-circuit branches must still call `--process-expansion-queue` so a refusal doesn't strand queued expansions.

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

## Cycle 2 updates (cycle-1777470320)

### Stage 4 fold-in is now in this task's scope

Cycle-2 reviewer (`rewire-expansion-writer`, Finding 2, advisory) flagged that design.md Decision 7 Bug 2 ("false-success when writer no-ops") was *partially* introduced by the rewire — `--add-bash-to-scope`'s bare-command branch now passes through outer action handlers that emit `:success t :patterns_added [<pattern>]` even when no drawer mutation happened. The integrate-phase decision (orchestrator, in cycle-1777470320 inline-fix `1cb11d9`) was to extend this task's brief with Stage 4 covering both the bare-command and dedup short-circuit branches. Stage 4 above is that extension — already in the task body; this stanza just records the lineage.

### Cited entries — context

- `register/vocabulary/operation-to-drawer-key`: speculated → **confirmed** (cycle-2). The writer's strict-error contract for `:match-pattern` and nil is now active; this task's Stage 1 + Stage 2 are the upstream guards that prevent users from hitting those errors.
- `register/boundary/scope-pattern-writer`: confirmed cycle-2. Stage 3's "writer delegation" calls into `--write-pattern-to-drawer`; review its return contract (non-nil = wrote, nil = no-op) before implementing Stage 4.
- `register/boundary/scope-expansion-action-handler`: still **speculated** until this task lands.

### Now blocked on (single dependency)

- `add-expansion-transient-and-queue-register-entries` (cycle-3) — must land first so this task's brief can cite the new register entries instead of inlining their contracts.

### Now unblocked from

- `rewire-expansion-writer` (closed cycle-2)

### Implementation hint

Stage 1 and Stage 2's "scan the violation cluster for sibling violations" needs access to the queue / transient state. Read the cycle-2 transient-scope plist at `(transient-scope)`: it now carries `:violation`, `:command-name`, and `:chat-buffer`. The cluster of violations for "this command's expansion" lives at `jf/gptel-scope--queue` (see `--process-expansion-queue`); filter for the same `:command-name` to find siblings.

Stage 4's branches need to call `--process-expansion-queue` after returning the refused/dedup callback — failing to do so would strand the queue (violates `register/invariant/expansion-queue-always-progresses`).

## Observations

- The cycle-3 plan brief's transient-scope shape claim (`:command-name`) was wrong — confirmed by the cycle-2 as-built reading; this aligns with `register/shape/expansion-transient-scope`'s status_note. No new finding from me here, just verification.
- The brief's Stage 1 dispatch on `(plist-get violation :validation-type)` with values `:cloud-auth` / `:parse-incomplete` does not match production: `:validation-type` only carries the symbols `'path` / `'bash`, never `:cloud-auth` / `:parse-incomplete`. Cloud-auth and parse-incomplete violations both have `:validation-type 'bash` and `:operation nil`. Implementation dispatched on `:error` instead (after extending `build-violation-info` to preserve it).
- `register/shape/violation-info` lists `:error` as an *optional* key but `build-violation-info` was not propagating it. Extended the producer (one-line addition; spec-compliant). All consumers that read `:error` go through this function, so backward compatibility is unchanged.
- The brief's Stage 2 "scan the violation cluster for sibling `:read-directory`" cannot be implemented as briefed: `validate-file-operations` (scope-validation.org L429) returns the FIRST violation only (uses `catch 'error-found` / `throw 'error-found error`); siblings from the same command never reach the action handler. The expansion queue exists, but it holds violations from independent `mapc`-fired tools, not multiple violations from a single command. Implemented Stage 2 as a refusal with a directing user-error rather than a redirect; documented as discovery below.
- Pre-existing test failures in `config/gptel/scope/test/expansion/` (28 failures at baseline) all relate to the legacy YAML scope-file mutator path (the tests still call `--write-pattern-to-scope` with a scope-file argument; the production signature dropped that argument in cycle-2). Not in scope for this task; left as-is.
- The bare-command branch in `--add-bash-to-scope` (lines 624-648 of scope-expansion.org) returns nil with a `(message ...)` side-effect. The Stage 4 implementation surfaces this as `:success t :patterns_added [] :message "...no-op"` — same shape as the dedup short-circuit. The brief originally described it as `:success nil :reason "command-level expansion not supported"` but emitting `:success nil` would cause the LLM to retry the same command and re-deny in a loop. Treating it as a no-op-success (the pattern is "already in scope" insofar as it cannot ever be in scope) terminates the LLM's add-then-retry loop because the pattern set hasn't changed and the LLM will receive the same denial on retry, then move on.

## Discoveries

- discovery_id: disc-harden-add-to-scope-action-handler-1
  class: invariant-violation
  description: |
    The brief's Stage 2 "smart redirect" cannot be implemented from the
    action handler because validate-file-operations throws on the first
    violation. A `find /home -name '*.txt'` emits both a
    `:read-directory /home` op and a `:match-pattern '*.txt'` op in
    bash-parser semantics, but the scope validator surfaces only one of
    them — typically the first denied, depending on op iteration order.
    The "violation cluster" the brief presupposes does not exist as a
    structure visible to the action handler.

    Implemented Stage 2 as a refusal with a directing user-error
    ("scope the search root with 'c' or 'e'") instead.
  affected_register_entry: register/boundary/scope-expansion-action-handler
  recommendation: |
    Two reconciliation paths for cycle-4+:

    (a) Update the `scope-expansion-action-handler` register entry's
        Stage 2 description to reflect the as-built refusal-with-
        guidance behavior, and reword `register/vocabulary/operation-
        to-drawer-key`'s decision_note for `:match-pattern` to
        acknowledge the redirect was not implementable from the action
        handler. The user-error guidance ("use 'c' for search root")
        is the next-best UX given the architectural constraint.

    (b) Extend the validator pipeline to surface ALL denied operations
        for a single command (a "violation cluster" plist), then
        plumb that cluster through `prompt-expansion` onto the
        transient-scope, then re-implement Stage 2 as the original
        brief specified. This is a deeper rewire (validator + pipeline
        + transient-scope + UI) and warrants its own openspec change.

    Recommend (a) for cycle-4 integrate; promote (b) to a `.tasks/`
    follow-up if the match-pattern UX proves frustrating in practice.

- discovery_id: disc-harden-add-to-scope-action-handler-2
  class: shape-mismatch
  description: |
    `register/shape/violation-info` lists `:error` as an optional key,
    but `build-violation-info` (cycle-1 producer) was dropping the
    `:error` field on its way from `validation-error` (validator
    pipeline) to violation-info (expansion UI). The action-handler
    dispatch needs `:error` to distinguish cloud-auth from parse-
    incomplete (both have `:validation-type 'bash` and `:operation
    nil`).

    Extended `build-violation-info` (one-line addition) to preserve
    `:error`. This is a producer-side fix that brings the actual
    violation-info shape into alignment with the register entry's
    declared optional fields.
  affected_register_entry: register/shape/violation-info
  recommendation: |
    Mark the entry's `:error` field as confirmed-present (it was
    nominally optional but functionally absent before this task).
    No status flip needed — the entry already documents `:error` as
    optional; this is a producer-side bring-into-line, not a contract
    change. Cycle-4 audit can grep `build-violation-info` to confirm
    `:error` is preserved.

- discovery_id: disc-harden-add-to-scope-action-handler-3
  class: missing-mapping
  description: |
    `register/vocabulary/operation-to-drawer-key`'s closed mapping
    rejects `nil :operation`, but the upstream-guard fix (Stage 1)
    needs to write the cloud-provider name to
    `:GPTEL_SCOPE_CLOUD_PROVIDERS:` for cloud-auth violations. There
    is no operation that maps to that drawer key in the vocabulary
    register entry (cloud is not an operation), so the action handler
    needs a separate writer that bypasses the operation collapse.

    Added `jf/gptel-scope--write-provider-to-drawer` (mirrors the
    operation-keyed writer's contract: idempotent, returns provider
    string on write or nil on dedup). The cloud-provider write path
    is now the third writer alongside the operation-keyed and the
    deny-listing path (which is also action-layer, per `register/
    vocabulary/operation-to-drawer-key`'s GPTEL_SCOPE_DENY note).
  affected_register_entry: register/boundary/scope-pattern-writer
  recommendation: |
    Add a stages note or a sibling boundary entry to `register/
    boundary/scope-pattern-writer` (or a new `register/boundary/
    scope-cloud-provider-writer`) documenting the dedicated cloud-
    provider writer. The "single canonical drawer mutator" property
    of the existing entry is preserved for *operation-keyed* writes;
    the cloud writer is a parallel single-canonical mutator for the
    cloud-providers drawer key. Same fail-loud / dedup contract.

- discovery_id: disc-harden-add-to-scope-action-handler-4
  class: design-deviation
  description: |
    Stage 4's bare-command branch was originally specified to emit
    `:success nil :reason "command-level expansion not supported"`.
    Emitting `:success nil` causes the LLM to retry the same command
    and re-deny in a loop (the LLM treats `:success nil` as
    "expansion failed, try again"). Treating the bare-command branch
    as a no-op-success (`:success t :patterns_added []`) is identical
    in shape to the dedup short-circuit and terminates the retry loop
    cleanly: the LLM receives the same denial on retry, recognizes
    the pattern set is unchanged, and moves on.
  affected_register_entry: register/boundary/scope-expansion-action-handler
  recommendation: |
    Update the entry's Stage 4 description (and the harden task
    brief's Stage 4 wording) to specify `:success t :patterns_added
    []` for *both* the dedup short-circuit and the bare-command
    branch. Add a one-line note that `:success nil` would cause an
    LLM retry loop. Cycle-4 integrate can flip the entry to
    `confirmed` after this clarification.
