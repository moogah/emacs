---
name: rewire-session-creation
description: Update session creation to embed drawer text in initial session.org; stop writing scope.yml
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

- `register/boundary/scope-profile-applicator` — session creation calls `--create-for-session` in mode 2a (string mode) and prepends the result to chat-mode initial content. Mode 2b is for branch/agent buffer cases.
- `register/shape/drawer-text-block` — what session creation embeds.
- `register/invariant/scope-drawer-no-duplication` — the prepend-once flow is what guards the invariant at session-creation time. If chat-mode hooks could insert a second drawer, this is where it would surface first.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-profile-applicator.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`

## Files to modify
- `config/gptel/sessions/commands.org` (modify) — `jf/gptel-persistent-session` and any related session-creation entrypoint should compose the drawer text from the resolved profile and prepend it to the chat-mode initial content before `write-region`.
- `config/gptel/sessions/filesystem.org` (possibly modify) — if it owns the actual file write, update there.
- `config/gptel/sessions/activities-integration.org` (modify) — the activities flow also creates sessions; rewire the same way.
- Tangle each touched file.

## Implementation steps

1. Locate the session.org file-write site (currently writes `"#+begin_user\n\n#+end_user\n"` plus the `:GPTEL_PRESET:` drawer). Identify how `jf/gptel-scope-profile--create-for-session` is currently called (it writes `scope.yml` as a side effect).

2. Change the flow:
   - Resolve the profile via `jf/gptel-scope-profile--resolve` (or whatever the existing call path is) to get the merged scope plist.
   - Build the drawer text via `jf/gptel-scope-profile--render-drawer-text` with the preset name, parent-session-id (nil for top-level sessions; populated for branches/agents), and the scope plist.
   - Write `session.org` in one shot: `(write-region (concat drawer-text "* Session\n#+begin_user\n\n#+end_user\n") nil session-org-path)` (or whatever the chat-mode initial content is — confirm by reading the existing creation code; the `gptel-org-mode-sessions` change settled this content to `#+begin_user\n\n#+end_user\n` per `sessions-persistence.md`).

3. Remove the call to `jf/gptel-scope-profile--write-scope-yml` (which is being deleted in `implement-profile-drawer-applicator` if it hasn't been already; if it's been stubbed-with-error, replace the call entirely).

4. Remove the directory step that creates `scope.yml` (no scope.yml is written anywhere now).

5. Update `jf/gptel-branch-session` (if it exists) to copy the parent branch's drawer scope keys into the new branch's `session.org`. Use `jf/gptel-scope-profile--render-drawer-text` with the parent's scope-plist (which the loader can recover from the parent branch's drawer). This matches the current behavior of copying `scope.yml` to the new branch.

6. Update activities-integration's session-creation path the same way.

7. Tangle and run `./bin/run-tests.sh -d config/gptel` — `session-creation-spec.el` tests that fixture or assert on `scope.yml` will fail; those are migrated in `migrate-session-creation-tests`.

## Design rationale

Per design.md § Decision 5 and the migration plan step 5, session creation produces drawer text and embeds it in initial content. Doing this at write time (one `write-region` call) — instead of "write file with empty drawer, then open buffer, then `org-entry-put`, then save" — avoids the buffer-lifecycle dance and keeps creation a single I/O step.

The branch-creation flow currently copies `scope.yml` from the parent branch. The drawer-resident equivalent reads the parent branch's drawer (via the loader) and renders new drawer text for the child branch. This preserves "branch inherits scope from parent at creation time" semantics.

## Design pattern

Single-shot `write-region` for new files, not "open buffer + save." See how `:GPTEL_PRESET:` is already pre-populated in the existing session-creation code (in `commands.org` — search for `:PROPERTIES:` in the file content building).

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `grep -rn 'scope\.yml' config/gptel/sessions/` returns no results.
- After a fresh `M-x jf/gptel-persistent-session "test-drawer"` (manual smoke), the resulting `session.org` carries a `:PROPERTIES:` drawer with `:GPTEL_PRESET:`, the resolved scope keys, then `#+begin_user\n\n#+end_user\n`.
- The session directory does NOT contain a `scope.yml` file.
- `./bin/run-tests.sh -d config/gptel` runs (session-creation tests will fail until `migrate-session-creation-tests`; other tests pass).

## Context

design.md § Decision 5 (initial-content embedding)
design.md § Migration Plan steps 5, 6
architecture.md § Boundaries
specs/gptel/sessions-persistence/spec.md § MODIFIED Requirements / "Directory structure initialization", "Scope profile integration"
specs/gptel/sessions-persistence/spec.md § REMOVED Requirements / "scope.yml file format"
specs/gptel/scope-profiles/spec.md § MODIFIED Requirements / "Integration with session creation"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/boundary/scope-profile-applicator`: speculated → reconciled. Three reconciliations: (1) multi-value encoding is single-line space-separated form `:KEY: v0 v1 v2` (Org wire format) for cross-mode idempotency; (2) NO `derived-mode-p 'org-mode'` precondition guard in mode 2b — non-org buffer surfaces a generic org-mode error; (3) default-cloud-auth-beacon clause was dropped — empty-paths profiles produce drawers with zero `:GPTEL_SCOPE_*` lines. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-profile-applicator.md`.
- `register/shape/drawer-text-block`: speculated → reconciled. Split into Shape A (drawer-text-block-complete: production renderer output, requires `:GPTEL_PRESET:`) and Shape B (drawer-text-block-fragment: test helper output, no `:GPTEL_PRESET:`). Production code emits Shape A. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-drawer-text-block.md`.
- `register/invariant/scope-drawer-no-duplication`: speculated → confirmed. Both producers (`--write-pattern-to-drawer`, `--apply-to-drawer`) operate on the existing drawer at point-min; renderer emits exactly one `:PROPERTIES:`/`:END:`. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-drawer-no-duplication.md`.

### User-resolved decisions
- `ask-arch-cycle-1777460733-2`: empty-paths beacon — user chose option (b): empty drawer = valid empty scope = deny-all defaults. `applied_via: deferred-to-cycle-2`, with this task explicitly named as a co-owner (alongside `rewire-validator-config-load`). **Implication for this task**: when this task generates initial drawer text via `--render-drawer-text`, the produced drawer for a profile with empty paths will have zero `:GPTEL_SCOPE_*` lines — the loader (revised by `rewire-validator-config-load`) is responsible for composing deny-all defaults around it. Verify the renderer-loader pair behaves correctly end-to-end.

### Meta-discoveries
- `shape-fragmentation-cluster/fragment-vs-complete-shape-ambiguity`: drawer-text-block now has two register entries (complete vs fragment). **Implication for this task**: production code MUST emit Shape A (complete with `:GPTEL_PRESET:`); test fixtures use Shape B via the helper.

### Already-shipped inline fixes
- `arch-cycle-1777460733-11`: write-side cloud-auth validation now active in `scope-profiles.el` (`--render-drawer-text` and `--apply-to-drawer` both call `--validate-cloud-auth`). **Implication for this task**: when this task calls `--render-drawer-text`, the producer signals an error if the resolved profile carries an invalid `:auth-detection` value. Profile-loading code must produce only `"allow"`/`"warn"`/`"deny"` for that field.

### Open follow-up: expected test failures owned by this task
> Cycle 1 noted in `state.json::implement-profile-drawer-applicator.execute.expected_test_failures_owner` that this task and `delete-yaml-and-security-residue` jointly own 13 expected test failures introduced by the cycle-1 profile-applicator merge:
> - 5 session-creation specs (`Scope profile integration writes scope.yml ...`)
> - 8 YAML Boolean Normalization specs
>
> The 5 session-creation specs become passing/obsolete once this task lands the drawer-resident replacement; the 8 YAML specs become obsolete once the YAML module is deleted (`delete-yaml-and-security-residue`).

## Observations

- `jf/gptel--create-session-core` now captures the drawer-text returned
  by `jf/gptel-scope-profile--create-for-session` (Mode 2a) and composes
  the new `session.org` content as `(concat drawer-text body)`. The
  helper writes the file in a single `with-temp-file` call. No
  `scope.yml` is written anywhere in the session-creation paths.
- Body composition was extracted into a new helper
  `jf/gptel--initial-session-body` that returns just the chat-mode empty
  user block (`#+begin_user\n\n#+end_user\n`). The legacy
  `jf/gptel--initial-session-content` (drawer + body, no scope keys) is
  retained for backward compatibility because four direct helper-level
  buttercup specs in
  `config/gptel/sessions/test/commands/session-org-creation-spec.el`
  pin its standalone shape contract. `--create-session-core` no longer
  routes through it.
- A caller-provided `initial-content` argument is still honoured
  verbatim (existing test contract preserved). The drawer-prepend only
  happens when the caller passes nil for `initial-content`.
- `jf/gptel-branch-session` no longer copies `scope.yml` from parent to
  child. The drawer-resident scope rides along automatically because
  `--copy-truncated-context` copies the parent's `session.org` bytes
  starting at point-min, which includes the parent's `:PROPERTIES:`
  drawer (preset + `:GPTEL_SCOPE_*:` keys + `:END:`). No call to
  `--render-drawer-text` is needed in the branching path — the bytes
  ARE the drawer text. This makes the branching rewire structurally
  simpler than the task brief anticipated.
- The activities-integration flow goes through `--create-session-core`
  for the file write, so the rewire carries through automatically. The
  worktree-paths plist that activities builds is now visible in the
  rendered drawer (as `:GPTEL_SCOPE_READ:`/`:GPTEL_SCOPE_WRITE:`/
  `:GPTEL_SCOPE_DENY:` lines) — previously these went into a separate
  `scope.yml` file. The worktree-comment annotation appended after the
  initial write (the `<!-- gptel-activity-worktrees: ... -->` line) is
  unchanged.
- One activities behavioral test
  (`activity-session-chat-spec.el` "appends the worktree comment when
  PROJECT_WORKTREE paths are declared") had a regex pinned to the
  scope-key-free drawer shape. Updated it to assert the relaxed
  invariants (drawer carries `:GPTEL_PRESET:` and the resolved
  `:GPTEL_SCOPE_*:` keys for the worktree paths; drawer is closed by
  `:END:`; body is the chat-mode template; worktree comment is still
  appended). All other activities tests passed unchanged because they
  don't seed worktree paths and the `'executor` preset has no
  registered scope profile in the test environment, so the rendered
  drawer collapses to `:GPTEL_PRESET:` only.
- `mock signature drift` hygiene fix: the pre-existing failing ERT
  test `test-directory-creation-org-session-structure`
  (`config/gptel/sessions/filesystem-test.el:43`) had an arity-4 stub
  for `--create-for-session` that no longer matched the cycle-1 arity-5
  signature. Updated the lambda to accept the fifth `_parent` arg so the
  test's actual failure mode (asserting on `metadata.yml` existence —
  removed in cycle-1) is preserved. The metadata.yml assertion itself
  remains broken; that's outside this task's scope (older
  metadata-removal task or `delete-yaml-and-security-residue` cleanup).
- The 5 expected scope.yml session-creation-spec failures in
  `config/gptel/test/session-creation-spec.el` are now reproducible
  obsolete tests (no scope.yml is written, so `captured-file-content`
  returns nil). `migrate-session-creation-tests` is the named owner.

## Discoveries

- discovery_id: disc-rewire-session-creation-1
  class: dead-branch
  description: |
    `jf/gptel-branch-session` does not need to call `--render-drawer-text`
    at all. The truncated-context copy already preserves the parent's
    drawer (which now carries `:GPTEL_SCOPE_*:` keys end-to-end) byte-
    for-byte. The task brief contemplated reading the parent's drawer
    via the validator's loader and re-rendering for the child; that's
    redundant work because the loader→renderer round-trip is, by the
    cross-mode idempotency invariant in
    `register/boundary/scope-profile-applicator`, a no-op against the
    parent's existing drawer text.
  affected_register_entry: register/boundary/scope-profile-applicator
  recommendation: |
    Integrate phase: note this as a confirmation of the cross-mode
    idempotency invariant — it pays off in the branching path by making
    "branch inherits parent's scope" a structural property of the byte
    copy rather than a dedicated rewire step. No follow-up task needed.

- discovery_id: disc-rewire-session-creation-2
  class: interface-drift
  description: |
    The `target-dir` parameter on
    `jf/gptel-scope-profile--create-for-session` is now unused (cycle-1
    removed the side effect that needed it; the renderer returns a
    string and callers write the file themselves). The argument is
    still in the signature for compatibility. `--create-session-core`
    passes `main-branch-dir` as the value for documentation/grep value;
    nothing reads it.
  affected_register_entry: register/boundary/scope-profile-applicator
  recommendation: |
    Future cleanup task (post-`delete-yaml-and-security-residue` once
    all callers are confirmed migrated): drop `target-dir` from the
    signature; or keep it documented as reserved for a future Mode-2b
    dispatch variant. Not blocking this change.

- discovery_id: disc-rewire-session-creation-3
  class: spec-signal
  description: |
    The activities-integration flow's worktree-paths plist now flows
    end-to-end into the rendered drawer (`:GPTEL_SCOPE_READ:`,
    `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:` for the bundled
    deny-list `("**/.git/**" "**/runtime/**" "**/.env"
    "**/node_modules/**")`). Previously these landed in a separate
    `scope.yml` file. This is a user-observable shape change for
    activity-backed sessions: their `session.org` drawer is now
    materially larger (5–6 extra lines) on first creation.
  affected_register_entry: register/shape/drawer-text-block
  recommendation: |
    Integrate phase: confirm with the architect that the
    `register/shape/drawer-text-block` cardinality bound (Shape A
    constraint: at least `:GPTEL_PRESET:`) is preserved — it is — and
    that an upper bound on key count is not part of the contract. If a
    spec wants to pin "no more than N `:GPTEL_SCOPE_*:` keys" that
    needs a new register entry; current contract is unconstrained.

- discovery_id: disc-rewire-session-creation-4
  class: invariant-gap
  description: |
    `register/invariant/scope-drawer-no-duplication` says the buffer
    has exactly ONE `:PROPERTIES:` and `:END:` line for the
    point-min drawer. The new `--create-session-core` path composes
    the file content as `(concat drawer-text body)` where `drawer-text`
    has its own `:PROPERTIES:`/`:END:` and `body` is just the user
    block. The invariant holds AT FILE-CREATION TIME because
    `--initial-session-body` does NOT include any `:PROPERTIES:`
    string. If a future caller ever passes a non-nil `initial-content`
    that DOES embed a drawer, AND the `drawer-text` from
    `--render-drawer-text` is also prepended, the invariant breaks.
    The current `(or initial-content (concat drawer-text body))`
    code path makes that impossible because the override is exclusive
    — but the safety property is brittle to future refactors.
  affected_register_entry: register/invariant/scope-drawer-no-duplication
  recommendation: |
    Integrate phase: consider adding a structural assertion (or a
    comment-as-contract) at the call site stating "if INITIAL-CONTENT
    is provided, the caller is responsible for embedding any drawer
    they want; the helper does NOT add one." A future
    behavioral test that round-trips a custom `initial-content`
    through the loader to confirm a single `:PROPERTIES:` exists
    would also pin the invariant.
