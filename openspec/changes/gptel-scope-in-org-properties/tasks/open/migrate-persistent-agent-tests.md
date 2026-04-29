---
name: migrate-persistent-agent-tests
description: Update persistent-agent creation tests to assert on agent session.org drawer instead of scope.yml
change: gptel-scope-in-org-properties
status: ready
relations: []
---

## Cites register entries

- `register/shape/drawer-text-block` — drawer-text assertions on the agent's `session.org`.
- `register/boundary/scope-profile-applicator` — the producer the agent-creation path uses; the negative `(file-exists-p (... "scope.yml"))` assertion is the structural form of "no other drawer-state route".

## Files to modify
- `config/gptel/tools/test/persistent-agent/creation-spec.el` (modify) — replace `scope.yml`-on-disk assertions with agent `session.org` drawer assertions; add a negative "no scope.yml in agent dir" assertion.
- `config/gptel/tools/test/persistent-agent/helpers-spec.el` (modify if needed) — update any fixture or matcher that depends on `scope.yml`.

## Implementation steps

1. Read existing tests. The relevant `it` blocks are around `creation-spec.el:95` (`writes scope.yml with allowed paths`) and `:125` (`writes scope.yml with empty read paths when allowed-paths is omitted`).

2. Rewrite each `it`:

   ```elisp
   ;; Before
   (it "writes scope.yml with allowed paths"
     ...
     (let ((scope-yml (expand-file-name "scope.yml" agent-dir)))
       (expect (file-exists-p scope-yml) :to-be t)
       (with-temp-buffer
         (insert-file-contents scope-yml)
         (expect (buffer-string) :to-match "/path/to/project"))))

   ;; After
   (it "writes session.org with scope drawer reflecting allowed paths"
     ...
     (let ((agent-session-org (expand-file-name "session.org" agent-dir)))
       (expect (file-exists-p agent-session-org) :to-be t)
       (with-temp-buffer
         (insert-file-contents agent-session-org)
         (org-mode)
         (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
                 :to-equal '("/path/to/project/**")))
       ;; And: no scope.yml is produced
       (expect (file-exists-p (expand-file-name "scope.yml" agent-dir))
               :to-be nil)))
   ```

3. The "empty read paths when allowed-paths is omitted" test becomes: agent's `session.org` drawer has no `:GPTEL_SCOPE_READ:` line (or `:GPTEL_SCOPE_READ:` is absent), but does have `:GPTEL_SCOPE_WRITE:` with `/tmp/**` and the standard `:GPTEL_SCOPE_DENY:` set.

4. Confirm the parent-session-id is captured in the agent's drawer (`:GPTEL_PARENT_SESSION_ID:`) — that property already exists pre-change but it's worth a positive assertion since this task tightens the agent's drawer contract.

5. Update `helpers-spec.el` if it carries a `mock-parent-session` fixture that previously wrote a parent `scope.yml`. The parent's drawer still needs a `:GPTEL_PRESET:` and (for some tests) a `:GPTEL_SCOPE_READ:`, but no `scope.yml`. Use `jf/gptel-test--render-drawer` from the shared helpers.

6. Run `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent`.

## Design rationale

Per spec delta `persistent-agent.md` § "No scope.yml written" scenario, agent directories must contain only `session.org`, `metadata.yml`, and (later) `tools.org`. The negative `scope.yml` assertion catches regressions where the YAML write step is accidentally re-introduced.

The "zero inheritance" contract is unchanged behaviorally — only the storage medium changes. Tests that assert "agent doesn't see parent's paths" stay structurally similar; the parent fixture changes from `scope.yml` to a drawer.

## Design pattern

Tmpdir + assert file content. Same as `migrate-session-creation-tests`. The agent tests already use this pattern; only the assertion targets change.

## Verification

- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` passes.
- `grep -n 'scope.yml' config/gptel/tools/test/persistent-agent/*-spec.el` returns only negative assertions.
- At least one test asserts on the agent's drawer via `org-entry-get-multivalued-property`.
- At least one test asserts `(not (file-exists-p ...agent-dir/scope.yml))`.

## Context

architecture.md § Testing Approach
specs/gptel/persistent-agent/spec.md § MODIFIED Requirements / "Agent session creation", "Configuration isolation (zero inheritance)"
specs/gptel/persistent-agent/spec.md § REMOVED Requirements / "scope.yml in agent directory"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/shape/drawer-text-block`: speculated → reconciled. Agent's `session.org` carries Shape A (complete, with `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:`); test assertions should use `org-entry-get` / `org-entry-get-multivalued-property` against the agent file. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-drawer-text-block.md`.
- `register/boundary/scope-profile-applicator`: speculated → reconciled. Multi-value encoding is single-line space-separated form. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-profile-applicator.md`.

### User-resolved decisions
- `ask-arch-cycle-1777460733-2` (related): empty drawer = valid empty scope = deny-all defaults.
  > Cycle 2: superseded — see § Cycle 2 updates / "empty-drawer behaviour pending user disposition". The cycle-1 description below is *not* what the merged loader does; the actual behaviour is the inverse (empty drawer → nil → no_scope_config). Defer empty-drawer-specific test pins until `disposition-empty-drawer-collapse` resolves.

  **Implication for this task**: the `:125` test ("writes scope.yml with empty read paths when allowed-paths is omitted") becomes "agent's drawer has only `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, and the standard `:GPTEL_SCOPE_DENY:`" — assert via positive present-key checks. **Do not** pin validator behaviour on the empty-allowed-paths case until the disposition resolves.

### Meta-discoveries
- `shape-fragmentation-cluster/fragment-vs-complete-shape-ambiguity`: agent drawer assertions must match Shape A.

### Already-shipped inline fixes
- `arch-cycle-1777460733-11`: cloud-auth write-side validation now active. **Implication**: agent profile tests must use valid `:auth-detection` values.

## Cycle 2 updates (cycle-1777470320)

### Cited register entries — disposition flips

- `register/shape/drawer-text-block`: speculated → **confirmed**. Agent `session.org` carries Shape A. See `.orchestrator/cycles/cycle-1777470320/reconciliations/shape-drawer-text-block.md`.
- `register/boundary/scope-profile-applicator`: speculated → **confirmed**. Mode 2a is the sole route in both consumer paths. See `.orchestrator/cycles/cycle-1777470320/reconciliations/boundary-scope-profile-applicator.md`.
- `register/invariant/scope-drawer-no-duplication`: speculated → **confirmed**. Single drawer in agent file. See `.orchestrator/cycles/cycle-1777470320/reconciliations/invariant-scope-drawer-no-duplication.md`.

### Empty-drawer behaviour pending user disposition

- `register/boundary/scope-config-loader`: **divergent**. Agent creation with `(allowed-paths nil)` produces an `:GPTEL_SCOPE_READ:`-omitted drawer; the loader's empty-drawer treatment is what's contested. The `:125` test is one of the test cases that pins this behaviour. **Implication for this task**: write the agent-side drawer assertions (these are unaffected — the renderer's behaviour is uncontested), but `xit` (skip with marker) any test that pins downstream validator behaviour for the empty-allowed-paths case until `disposition-empty-drawer-collapse` resolves.

### Cycle-2 implementation details to honour

- The implementor (commit `486d09f`) retained `--initial-content` legacy helper for production-dead/test-only callers. The org-tree heading explicitly marks it "legacy, test-only". **Implication**: helper-level tests pinning `--initial-content`'s shape can stay; just update them to assert on drawer-text-block invariants.
- Standard deny set is now hoisted to a defconst in `persistent-agent.org`. **Implication**: tests asserting on the standard deny patterns should reference the defconst by name (so tests don't double-pin the literal pattern strings).
- Agent description-string has been updated to direct the LLM at `session.org` and `:GPTEL_SCOPE_*` keys. **Implication**: any test asserting on the agent's `:description` string should be updated.

### Test-suite delta this cycle

State.json::tasks[3].test_output_tail says:
- 3 expected migration-bucket failures in `creation-spec.el`:
  - `PersistentAgent_creation_writes_session_org_with_drawer` (asserts old drawer shape)
  - `PersistentAgent_creation_writes_scope_yml_with_allowed_paths`
  - `PersistentAgent_creation_writes_scope_yml_empty_read_when_omitted`

These three are this task's primary work.

### Unblocked

- `add-test-helper-with-scope-drawer` (closed cycle-1).
- `rewire-persistent-agent` (closed cycle-2).

## Observations

- **Three tests, not two, were the migration bucket.** The brief listed
  the `:95` and `:125` `it` blocks as the primary work. In practice
  the `:70` test (`writes session.org with a self-describing
  :PROPERTIES: drawer`) was also failing because cycle-2's
  `rewire-persistent-agent` widened the agent's drawer to carry
  `:GPTEL_SCOPE_*:` keys (standard write `/tmp/**` + standard deny
  set), but the test still asserted on the pre-cycle-2 drawer shape
  (`:GPTEL_PRESET:` + `:GPTEL_PARENT_SESSION_ID:` only). The state.json
  test_output_tail cited in cycle-2 updates correctly named all three;
  the body's `:95`/`:125` framing under-counted by one. This task
  migrated all three.
- **No parent `scope.yml` to retire in `helpers-spec.el`.** The
  `with-mock-parent-session` macro was already drawer-only — it
  writes a parent `session.org` with `:GPTEL_PRESET: dummy` and never
  produced a parent `scope.yml`. The brief's step 5 anticipated a
  fixture migration that wasn't needed. `helpers-spec.el` was not
  modified.
- **Standard deny set referenced via defconst.** Per cycle-2 commit
  `486d09f`, `jf/gptel-persistent-agent--standard-deny-paths` is the
  hoisted defconst. All three migrated tests reference the defconst
  directly in their `:GPTEL_SCOPE_DENY:` assertions, so a future
  expansion of the deny set will not require this test file to change
  (single source of truth — `register/shape/drawer-text-block`
  invariant on the deny key's contents).
- **Drawer queries route through `org-entry-get-multivalued-property`.**
  Added a small `jf/persistent-agent-test--with-agent-session-org`
  macro local to this spec file that opens the agent's `session.org`
  in `org-mode` and runs body inside it. Drawer queries then go
  through the same parser the production loader uses
  (`register/boundary/scope-profile-applicator`, Mode 2a). No use of
  `:to-match` against raw drawer text — the `:to-equal` assertions on
  parsed multi-value lists are stronger and survive whitespace /
  ordering changes in the renderer.
- **`#+begin_user` body still asserted.** The `:70` migration retains a
  `:to-match "#\\+begin_user\nDO THE THING\n#\\+end_user"` assertion
  on the buffer string. This documents that the user prompt becomes
  the body of the first `#+begin_user` block — a contract that was
  implicit in the legacy literal-string `:to-equal` assertion and is
  worth preserving.
- **Empty-drawer disposition is not pinned here.** The third test
  (no allowed-paths) asserts only on the agent-side renderer's
  output (a `:GPTEL_SCOPE_READ:`-omitted drawer). It does not touch
  validator behaviour for the empty-allowed-paths case, per the
  cycle-2 update on `disposition-empty-drawer-collapse`.
- **All 36 specs in the persistent-agent suite pass.** The 3
  migration-bucket failures resolve as expected; no other tests
  regress.

## Discoveries

(none — the brief's pressure-test prompt covered the +1 test
discrepancy, and no register entries needed expansion. The cycle-2
defconst hoist and updated description string both held under test
without further follow-up.)
