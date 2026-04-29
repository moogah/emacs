---
name: migrate-persistent-agent-tests
description: Update persistent-agent creation tests to assert on agent session.org drawer instead of scope.yml
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-test-helper-with-scope-drawer
  - blocked-by:rewire-persistent-agent
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
- `ask-arch-cycle-1777460733-2` (related): empty drawer = valid empty scope = deny-all defaults. **Implication for this task**: the `:125` test ("writes scope.yml with empty read paths when allowed-paths is omitted") becomes "agent's drawer has only `:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, and the standard `:GPTEL_SCOPE_DENY:`" — assert via positive present-key checks; the loader composes deny-all behaviour around the rest.

### Meta-discoveries
- `shape-fragmentation-cluster/fragment-vs-complete-shape-ambiguity`: agent drawer assertions must match Shape A.

### Already-shipped inline fixes
- `arch-cycle-1777460733-11`: cloud-auth write-side validation now active. **Implication**: agent profile tests must use valid `:auth-detection` values.
