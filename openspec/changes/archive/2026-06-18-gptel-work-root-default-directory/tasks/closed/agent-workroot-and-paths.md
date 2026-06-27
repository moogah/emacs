---
name: agent-workroot-and-paths
description: PersistentAgent passes work_root + read_paths/write_paths; drawer GPTEL_WORK_ROOT; guardrail
change: gptel-work-root-default-directory
status: done
relations:
  - blocked-by:binder-default-directory
  - blocked-by:agent-build-scope-plist-split
---

## Files to modify
- `config/gptel/tools/persistent-agent.org` (modify) — `--task` (≈`:514`), drawer build
  (≈`:560-620`), tool `:args` registration (≈`:681`)

## Implementation steps
1. **Tool schema (`gptel-make-tool` `:args`, ≈`:681`)** — BREAKING change to the tool
   surface. Replace the single `allowed_paths` array param with three params, in this
   order (the tool is `:async t`, so gptel prepends `main-cb`; `:args` order maps to
   positional params AFTER it):
   - `work_root` (string) — absolute dir the agent works in; default = parent's work root.
   - `read_paths` (array of glob strings) — agent read scope (replaces `allowed_paths`).
   - `write_paths` (array of glob strings) — agent write scope (`/tmp/**` auto-added).
   Update the `:description` to guide the model: pass `read_paths`/`write_paths` from the
   parent's `:GPTEL_SCOPE_*` drawer keys; pass `work_root` (typically a worktree); omit
   `work_root` to default to the parent's. Remove the `allowed_paths` text.
2. **`--task` signature (`:514`)** — add the new params after `prompt`
   (`preset description prompt work-root read-paths write-paths`), normalizing arrays
   from vectors as the existing `allowed-paths-list` code does.
3. **Resolve work_root + default** — when `work-root` is nil/empty, default to the
   parent buffer's work root: `(or work-root default-directory)` evaluated in the parent
   buffer context (the `--task` runs there), normalized with `expand-file-name`. Freeze
   the resolved value (it goes into the agent's own drawer — not a live link).
4. **build-scope-plist call** — call the new `(read-paths write-paths)` signature from
   task `agent-build-scope-plist-split`.
5. **Drawer write (`:560-620`)** — splice `:GPTEL_WORK_ROOT:` into the agent drawer via
   `jf/gptel--append-drawer-property` (same helper used for `GPTEL_SESSION_ID` /
   `GPTEL_BRANCH`), value = resolved absolute work root. On activation the binder
   (`binder-default-directory` task) sets the agent buffer's `default-directory` from it.
6. **Guardrail (design D7)** — after resolving work_root and read-paths, if the resolved
   work_root is not matched by any read pattern (reuse
   `jf/gptel-scope--path-matches-any-pattern-p` if available, else a glob check), emit a
   `display-warning` / `jf/gptel--log 'warn` noting the work root is outside read scope.
   Do NOT abort creation.
7. Tangle + validate: `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.

## Design rationale
Agents have ZERO inheritance from the parent (existing critical invariant). The parent
*passes* the agent's work root and read/write paths explicitly; the values are frozen in
the agent's own drawer at spawn, so a parent-supplied default is not runtime inheritance
(design D5). This also fixes the accidental `/tmp`-only write (via the split helper) and
gives the parent flexibility to assign each agent to a specific worktree. The guardrail
warns (not errors) because a parent may legitimately grant only absolute paths; warning
catches the silent-DENY trap without forbidding that case (design D7).

## Design pattern
- Drawer splice: copy the existing `(drawer-text (jf/gptel--append-drawer-property
  drawer-text "GPTEL_SESSION_ID" agent-session-id))` pattern in the same `--task` let*.
- Array normalization: copy the existing `allowed-paths-list` vector→list coercion.

## Verification
- `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org` passes.
- Buttercup specs under `config/gptel/tools/test/persistent-agent/`:
  - Tool `:args` lists exactly `preset, description, prompt, work_root, read_paths,
    write_paths` and excludes `allowed_paths` / `denied_paths`.
  - WHEN created with explicit `work_root` THEN agent drawer has `:GPTEL_WORK_ROOT: <abs>`;
    on activation the agent buffer's `default-directory` is that dir.
  - WHEN `work_root` omitted THEN drawer records the parent's work root (frozen).
  - WHEN created with `read_paths`/`write_paths` THEN drawer `:GPTEL_SCOPE_READ:` /
    `:GPTEL_SCOPE_WRITE:` carry them and `/tmp/**` appears in write as scratch.
  - WHEN `work_root` is outside `read_paths` THEN a warning is emitted AND the agent is
    still created.
- Run: `./bin/run-tests.sh -d config/gptel/tools`.

## Context
design.md § Decisions 'D5 — Agent: parent passes work_root', 'D6', 'D7 — Consistency guardrail = warn'
specs/persistent-agent/spec.md § 'Requirement: Agent working directory (parent-supplied)',
  'Requirement: Tool invocation and validation', 'Requirement: Agent session creation',
  'Requirement: Configuration isolation (zero inheritance)'

## Observations

- **BREAKING change forced updates to existing tests (in-scope).** The
  `allowed_paths` → `read_paths`/`write_paths` rename and the new `work_root`
  param changed the positional signature of `jf/gptel-persistent-agent--task`
  and the tool `:args` set. Two existing test files had to be updated to keep
  the suite green: `creation-spec.el` (the `read_paths` call now sits at the
  6th positional after `work_root`; the args-schema test now asserts the exact
  closed six-param set) and `agent-buffer-activation-reload-spec.el` (the agent
  buffer's `default-directory` is now sourced from `:GPTEL_WORK_ROOT:`, not
  from `find-file`'s file-dir default, so that test's `default-directory`
  assertion changed and its parent-buffer fixture now pins a known work root).
  These are direct consequences of the change's contract, not scope creep.

- **Adjacent latent bug: `persistent-agent-trace.el` around-advice is now
  arity-stale.** `jf/gptel-pa-trace--around-task` (config/gptel/tools/
  persistent-agent-trace.el:87) wraps `--task` with the OLD signature
  `(orig main-cb preset description prompt &optional allowed-paths)` and
  re-invokes `orig` with only those 5 positionals (line 105). With the new
  signature the trace advice would silently DROP `work_root`, `read_paths`,
  and `write_paths` whenever tracing is enabled — every agent spawned under a
  trace session would lose its work root and scope. This module is loaded at
  startup but the advice is opt-in (`M-x jf/gptel-pa-trace-start`), so it does
  not affect the default path or the test suite. It is outside this task's
  declared files-to-modify, so I did NOT edit it; filed as a `.tasks/`
  follow-up (`pa-trace-task-advice-arity-drift`).

- **Guardrail glob semantics are sharper than the prose implies.** Under this
  repo's glob engine (`jf/gptel-scope--glob-to-regex`), a `<root>/**` pattern
  compiles to `<root>/.*$`, which requires a trailing path component and so
  does NOT match the work-root directory `<root>` itself. Consequently the
  common "I gave the agent `work_root /proj` and `read_paths ["/proj/**"]`"
  case WILL trip the D7 warning, because `/proj` is not matched by `/proj/**`.
  This is harmless (warn, not error) and arguably correct (relative reads
  resolve to files *under* the root, which the pattern does cover), but the
  D7 prose ("work_root falls outside read scope") reads as if it would only
  fire for genuinely-disjoint roots. The "does not warn" test was written
  with an ancestor glob to reflect the actual semantics. No code change made;
  recorded as a Discovery for register reconciliation.

- **`jf-gptel-scope-validation` was not previously required by
  persistent-agent.org.** The D7 guardrail uses
  `jf/gptel-scope--path-matches-any-pattern-p`, which lives in the
  `jf-gptel-scope-validation` feature. persistent-agent only required
  `gptel-scope-profiles` (which does not transitively pull in validation), so
  I added an explicit `(require 'jf-gptel-scope-validation)` alongside the
  existing requires. Matches the convention in the scope subsystem's own
  modules (scope-expansion/scope-shell-tools/scope-tool-wrapper).

## Discoveries

- discovery_id: disc-agent-workroot-and-paths-1
  class: vocabulary-mismatch
  description: |
    The speculated closed param set in
    register/vocabulary/agent-path-params is CONFIRMED by implementation.
    The PersistentAgent tool `:args` now lists exactly
    {preset, description, prompt, work_root, read_paths, write_paths}, in
    that order, and `allowed_paths`/`denied_paths` are absent from the tool
    surface. `work_root` is a scalar written to `:GPTEL_WORK_ROOT:`
    (defaulting to the parent buffer's frozen `default-directory`);
    `read_paths` feeds build-scope-plist `:read` verbatim; `write_paths`
    feeds `:write` with `/tmp/**` appended last and serializes via the
    existing `+`-multivalue drawer convention (`:GPTEL_SCOPE_WRITE+: /tmp/**`).
    The build-scope-plist shape (register/shape/scope-config-plist) was
    consumed unchanged. All six-param assertions and the full tools suite
    pass green (81 specs, 0 failed).
  affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    Promote register/vocabulary/agent-path-params from SPECULATED to
    CONFIRMED. The user-approved closed set (D5/D6/D7) is exactly what the
    code advertises; no divergence found. The confirm gate (tool `:args`
    asserts the six-param set AND suite green) is satisfied.

- discovery_id: disc-agent-workroot-and-paths-2
  class: interface-drift
  description: |
    `jf/gptel-pa-trace--around-task` in
    config/gptel/tools/persistent-agent-trace.el is :around advice on
    `jf/gptel-persistent-agent--task`. Its parameter list and its `funcall
    orig` both still use the pre-change single-`allowed-paths` signature, so
    while tracing is active it drops the new work_root/read_paths/write_paths
    positionals — agents spawned under a trace session would lose their work
    root and scope. The trace module is not in this task's files-to-modify
    and tracing is opt-in, so it was left untouched.
  affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    Update the trace around-advice to mirror the new
    `(orig main-cb preset description prompt &optional work-root read-paths
    write-paths)` signature and pass all positionals through to `orig`.
    Tracked in .tasks/pa-trace-task-advice-arity-drift.md.

- discovery_id: disc-agent-workroot-and-paths-3
  class: spec-signal
  description: |
    D7's "work_root falls outside read scope" wording undersells how often
    the guardrail fires. Because the repo glob engine compiles `<root>/**`
    to `<root>/.*$` (requiring a trailing component), a work_root equal to
    the directory root of its own `<root>/**` read pattern is NOT matched
    and therefore WARNS. The warning is benign (relative reads still resolve
    to in-scope files under the root), but operators passing the natural
    `work_root=/p` + `read_paths=["/p/**"]` pairing will see a warning every
    time.
  affected_register_entry: register/vocabulary/agent-path-params
  recommendation: |
    Either (a) document in the D7/spec prose that a `<root>/**` pattern does
    not cover the root directory itself (so the warning is expected for the
    canonical pairing), or (b) soften the guardrail to also accept a read
    pattern whose directory prefix equals the work root. Option (a) is the
    lower-risk reconciliation; defer (b) to a follow-up if the warning noise
    proves annoying in practice.
