---
name: rewire-persistent-agent
description: Update persistent-agent creation to embed scope drawer in initial session.org; stop writing scope.yml for agents
change: gptel-scope-in-org-properties
status: done
relations: []
---

## Cites register entries

- `register/boundary/scope-profile-applicator` — agent creation also dispatches through `--create-for-session` (or the `--render-drawer-text` helper directly, for the file-write-in-one-shot path described in the architecture).
- `register/shape/drawer-text-block` — what agent creation embeds in the agent's initial `session.org`.
- `register/invariant/scope-drawer-no-duplication` — exactly one drawer in the agent's `session.org`.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-profile-applicator.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify) — replace the `scope.yml` write step with drawer-text embedding in the agent's initial `session.org` content.
- Tangle: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.

## Implementation steps

1. Locate the agent-creation function in `persistent-agent.org` (today it's around `persistent-agent.el:111` — `Write SESSION-DIR/scope.yml with read paths from ALLOWED-PATHS`). It currently:
   - Builds a scope plist from `allowed-paths`, denied-paths, and a hardcoded set of standard denies (`**/.git/**`, `**/runtime/**`, `**/.env`, `**/node_modules/**`).
   - Writes `scope.yml` to the agent dir.
   - Writes `session.org` (currently with a `:PROPERTIES:` drawer containing `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:`) — see `persistent-agent.el:139`.

2. Build the scope plist the same way (read paths from `allowed-paths`, write `/tmp/**`, deny standard set).

3. Render drawer text via `jf/gptel-scope-profile--render-drawer-text` with the agent's preset name, the parent session ID, and the scope plist. Confirm `--render-drawer-text` emits `:GPTEL_PARENT_SESSION_ID:` correctly (it should, per `implement-profile-drawer-applicator`).

4. Write the agent's `session.org` in one shot using the rendered drawer text + chat-mode initial content. Replace the existing `(format ":PROPERTIES:\n:GPTEL_PRESET: %s\n:GPTEL_PARENT_SESSION_ID: %s\n:END:\n#+begin_user\n%s\n#+end_user\n" ...)` literal with the call to `--render-drawer-text` plus the chat-mode initial content.

5. Delete the `scope.yml` write step entirely. Verify no `scope.yml` is created in the agent directory anywhere in the lifecycle.

6. Update any messages or docstrings that reference `scope.yml` to reference the agent's drawer instead. There's at least one user-facing message at `persistent-agent.el:283` ("Use the read_file_in_scope tool on scope.yml to get your current allowed...") — rewrite to point at the agent's `session.org` drawer.

7. Tangle. Run `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` — tests that fixture or assert on `scope.yml` will fail; those are migrated in `migrate-persistent-agent-tests`.

## Design rationale

Per Decision 5 in design.md, agent creation embeds drawer text in initial content rather than opening a headless buffer. This preserves the existing single-shot file-write pattern (already used for the `:GPTEL_PRESET:` / `:GPTEL_PARENT_SESSION_ID:` drawer) and avoids the buffer-lifecycle dance during creation.

Configuration isolation (zero inheritance) is unchanged — the agent's drawer is built from the explicit `allowed-paths` parameter, never from the parent's drawer. The spec scenario "Path configuration never inherited" carries forward verbatim, just with "drawer" substituted for "scope.yml".

## Design pattern

Single-shot file write for new agent files (the existing pattern). See `persistent-agent.el:139` for the current literal `(format ":PROPERTIES: ... :END:\n#+begin_user...")` shape — keep that structure, but build the drawer block via `--render-drawer-text` instead of the inline `format`.

## Verification

- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` succeeds.
- `grep -n 'scope\.yml' config/gptel/tools/persistent-agent.el` returns no results (or only in deprecated docstring text scheduled for cleanup).
- After a manual smoke (create a parent session, invoke PersistentAgent with `allowed_paths`), the agent's directory contains `session.org` with a populated drawer (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, `:GPTEL_SCOPE_READ:`, `:GPTEL_SCOPE_WRITE:`, `:GPTEL_SCOPE_DENY:` ×4) — and no `scope.yml`.
- `./bin/run-tests.sh -d config/gptel/tools/test/persistent-agent` runs (tests will fail until `migrate-persistent-agent-tests`; the tangling and macro behavior pass).

## Context

design.md § Decision 5 (Persistent-agent embeds drawer in initial content)
design.md § Migration Plan step 7
specs/gptel/persistent-agent/spec.md § MODIFIED Requirements / "Agent session creation", "Configuration isolation (zero inheritance)"
specs/gptel/persistent-agent/spec.md § REMOVED Requirements / "scope.yml in agent directory"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/boundary/scope-profile-applicator`: speculated → reconciled. Multi-value encoding is single-line space-separated form (`:KEY: v0 v1 v2`); the brief's default-cloud-auth-beacon clause for empty-paths profiles was dropped (drawer has zero `:GPTEL_SCOPE_*` lines for empty-paths). See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-profile-applicator.md`.
- `register/shape/drawer-text-block`: speculated → reconciled. Split into Shape A (complete, with `:GPTEL_PRESET:`) and Shape B (fragment, fixture-only). Agent's `session.org` must carry Shape A. See `.orchestrator/cycles/cycle-1777460733/reconciliations/shape-drawer-text-block.md`.
- `register/invariant/scope-drawer-no-duplication`: speculated → confirmed. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-drawer-no-duplication.md`.

### User-resolved decisions
- `ask-arch-cycle-1777460733-2` (related): user chose "empty drawer = valid empty scope = deny-all defaults" for the loader. **Implication for this task**: agents typically have explicit `allowed-paths`, so the empty-drawer case is rare; but if `allowed-paths` is omitted, the agent's drawer will likewise carry zero `:GPTEL_SCOPE_*` lines, and the loader's deny-all default semantics apply.

### Meta-discoveries
- `shape-fragmentation-cluster/fragment-vs-complete-shape-ambiguity`: agent code must emit Shape A (complete with `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:`).

### Already-shipped inline fixes
- `arch-cycle-1777460733-11`: write-side cloud-auth validation now active in `--render-drawer-text` / `--apply-to-drawer`. **Implication for this task**: the agent's profile must produce a valid `:auth-detection` (one of `"allow"`/`"warn"`/`"deny"`) or `--render-drawer-text` will signal an error.

## Observations

- **Two helpers introduced, one retained**: split body composition into
  `jf/gptel-persistent-agent--initial-body (prompt)` (production) while
  retaining the legacy `jf/gptel-persistent-agent--initial-content
  (preset-sym parent-id prompt)` (4-arg, drawer + body, no scope keys).
  The legacy function is no longer called from production
  (`--task` composes drawer-text + body directly via the renderer);
  it is kept solely to avoid churning helper-level test fixtures
  outside this task's scope. `migrate-persistent-agent-tests` is the
  natural home for any final cleanup.
- **Scope-plist construction extracted**: introduced
  `jf/gptel-persistent-agent--build-scope-plist (allowed-paths)` that
  returns a `register/shape/scope-config-plist` with the standard
  deny set. The deny set itself is hoisted to a defconst,
  `jf/gptel-persistent-agent--standard-deny-paths`, both for
  test-introspection and to make the deny vocabulary discoverable.
- **No `:cloud` sub-plist constructed**: per the cycle-1 reconciliation
  of `register/boundary/scope-profile-applicator`, omitting `:cloud`
  means the rendered drawer carries zero `:GPTEL_SCOPE_CLOUD_*` keys
  (validator nil-guard accepts a nil `:auth-detection`). This matches
  typical agent profiles that do not declare cloud-auth. If a future
  agent needs cloud-auth, the caller can pass an explicit `:cloud`
  sub-plist; no scaffolding required.
- **`--write-scope-file` deleted; `jf/gptel-session--scope-file`
  constant unreferenced from this module** (still defined in
  `gptel-session-constants` and removed by cycle-3
  `delete-yaml-and-security-residue`).
- **User-facing tool description rewritten**: the `gptel-make-tool`
  `:description` now points the LLM at the parent's `session.org`
  drawer (`:GPTEL_SCOPE_READ:` / `:GPTEL_SCOPE_WRITE:`) instead of
  the obsolete `scope.yml` artifact.
- **Overview docstring updated** to describe the drawer as the
  carrier of both parent-link AND scope policy
  (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:`, `:GPTEL_SCOPE_*:`),
  and to call out that no `scope.yml` sidecar is written.

## Discoveries

- finding: register/shape/scope-config-plist
  status: confirmed
  load_bearing: true
  description: |
    The persistent-agent's directly-constructed scope-plist
    (no `:cloud` sub-plist; `:paths` with `:read`/`:write`/`:deny`
    only) is accepted by `--render-drawer-text` and produces a valid
    `register/shape/drawer-text-block` Shape A. The omission of
    `:modify` and `:execute` keys also collapses cleanly (the
    renderer's `dolist` over op→key pairs only emits keys whose
    value list is non-empty). Confirms the renderer's tolerance for
    sparse plists.
- finding: register/boundary/scope-profile-applicator (Mode 2a, direct call)
  status: confirmed
  load_bearing: true
  description: |
    `--render-drawer-text` is callable directly from a producer that
    bypasses `--create-for-session` (which is the preset-resolving
    path). The persistent-agent constructs its own scope-plist from
    explicit per-call `allowed-paths` rather than resolving it from
    a registered preset profile, so it routes to `--render-drawer-text`
    directly. This is consistent with cycle-1's two-mode
    documentation: the producer *may* skip `--create-for-session`
    when its scope-plist is constructed elsewhere.
- finding: persistent-agent test-fixture migration
  status: expected_failures_documented
  load_bearing: false
  description: |
    Three pre-existing tests in `creation-spec.el` fail post-rewire
    (one drawer-shape strict-equality assertion; two `scope.yml`
    presence assertions). All three are documented in the
    `migrate-persistent-agent-tests` task per the cycle-2 plan. 33 of
    36 specs continue to pass — including the directory-creation,
    preset-validation, parent-session-requirement, FSM-handler-
    composition, and final-text-extraction suites.
- finding: residual `scope.yml` reference in tangled .el
  status: confirmed_acceptable
  load_bearing: false
  description: |
    A single occurrence of "scope.yml" remains in the tangled .el at
    line 276 — it is an inline comment in `--task` that explicitly
    states "No scope.yml is written — drawer-resident scope". This is
    the cycle-2 transitional documentation pattern matched by the
    rewire-session-creation precedent. Not a regression.
- finding: register/invariant/scope-drawer-no-duplication
  status: confirmed
  load_bearing: true
  description: |
    Composition is `(concat drawer-text body)` where drawer-text
    contains exactly one `:PROPERTIES:` / `:END:` pair (renderer
    contract) and body
    (`jf/gptel-persistent-agent--initial-body`) contains zero. The
    invariant holds by construction.
