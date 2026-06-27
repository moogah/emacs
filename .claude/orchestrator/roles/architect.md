# Project-specific architect extensions (elisp / literate-org / gptel)

The core architect brief (eight signal classes, three triggers, register as
authority for shapes / vocabularies / boundaries / invariants) applies
unchanged. The notes below name this repo's drift hot-spots so the
forward-mode register population at plan and the on-touch / end-of-cycle
audits know where to look first.

## Interfaces register

This project's register lives at `interfaces.org` at the repo root,
hand-tangled (no `auto_tangle`). It is the authoritative catalogue of:

- **shape**: plist / alist shapes that flow across modules.
- **vocabulary**: enumerations / symbol sets that span boundaries (e.g. the
  bash-parser semantic-op set vs. scope's section keys).
- **boundary**: canonical mapping functions between subsystems.
- **invariant**: rules that must hold runtime-and-test (load order,
  parse-completeness, auth gating).

New entries land as `status: speculated`. Integrate-phase reconciliation
(driven by Implementor `## Discoveries`) advances them to `confirmed`,
`divergent`, or `reconciled`. Mark `load_bearing: true` for entries whose
violation has already cost real time (gptel callback closure, bash-parser
chain decomposition, drawer corruption).

## Drift hot-spots in this repo

The following are the patterns this codebase has already paid for. Bias
forward-mode register entries and on-touch audits toward them.

### 1. Literate-vs-tangled drift

The `.el` files are **generated** from `.org` files by `./bin/tangle-org.sh`.
A diff that touches `.el` without a matching `.org` (or vice versa) is
drift waiting to happen — the next tangle overwrites whichever side was
ahead.

- **What to scan**: every diff in the batch — paired `.org`/`.el` updates
  must move together.
- **Severity**: blocking when unpaired; advisory when the `.el` is
  hand-written (rare, look for "no auto_tangle" / hand-maintained
  marker in the file).
- **Maps to**: `class: invariant-gap` (the invariant is "tangled output
  matches source").

### 2. Lexical-binding header on line 1

The first elisp babel block of every `.org` file must use `:comments no`
so `lexical-binding: t` lands on line 1 of the tangled `.el`. Otherwise
Emacs silently uses dynamic binding, breaking closures.

- **What to scan**: any new `.org` file in the batch — verify first
  babel block has `:comments no`.
- **Severity**: blocking.
- **Maps to**: `class: invariant-gap`. Worth a register `invariant`
  entry: "every tangled .el begins with `;;; <name>.el --- ... -*-
  lexical-binding: t; -*-` on line 1."

### 3. Module-system contract

`jf/load-module` / `jf/resolve-module-path` is the canonical loader; the
contract is at `openspec/specs/core/module-system.md`.

- **What to scan**: any new module-loading code; any change to
  `jf/enabled-modules` in `init.org`; any cross-subsystem load helper
  that bypasses `jf/resolve-module-path`.
- **Load-order invariants** (encode as register `invariant` entries):
  - `transient` before `language-modes`, before `major-modes/magit`.
  - `major-modes/magit` before `major-modes/org`.
  - gptel internal: `skills` → `gptel-agent` → tool defs → sessions
    chain (constants → logging → filesystem → registry → metadata →
    subagent → commands) → `activities-integration`.
- **Severity**: blocking on violation.

### 4. gptel scope subsystem boundary (load-bearing)

The scope subsystem (`config/gptel/scope/`) is the most architecturally
load-bearing area of the repo — seven-stage operation-first validator
pipeline, bash-parser handler-shape contract, profile templates, expansion
UI. The boundary register entries are:

- **shape**: scope config plist (read/write/modify/execute/deny/cloud-auth/
  cloud-providers); validator result plist (`:reason`, `:tool`, `:resource`,
  `:command`); bash-parser handler output (`:op`, `:path`, `:confidence`,
  ...).
- **vocabulary**: bash-parser semantic op set; scope section keys; cloud
  provider names.
- **boundary**: `scope.yml` / drawer ↔ scope plist (currently in flight —
  see `gptel-scope-in-org-properties`); validator-result ↔ user-message;
  bash-parser ↔ scope categorisation `pcase`.
- **invariant**: `enforce-parse-complete` is `t`; `max-coverage-threshold`
  is `1.0`; allow-once is single-shot.

Treat **every** entry in this subsystem as `load_bearing: true` until proven
otherwise. The on-touch trigger should be aggressive here.

### 5. gptel sessions org-mode native (in flight)

Sessions migrated from JSON to `session.org` (chat-mode buffer). The
in-flight `gptel-scope-in-org-properties` change finishes the migration
by folding `scope.yml` into the same property drawer.

- **What to scan**: any code that reads/writes session files. The
  `:GPTEL_PRESET:` drawer pattern is the precedent; the new
  `:GPTEL_SCOPE_*` properties follow it.
- **Drift pattern**: a code path that still expects `scope.yml` after
  this change lands. Will manifest as "scope drawer not found" or
  silent fallback to default scope.
- **Maps to**: `class: dead-branch` (post-migration); `class:
  shape-fragmentation` (during migration if multiple readers diverge).

### 6. bash-parser ↔ scope handler-shape contract

`config/bash-parser/` produces semantic-op plists that
`scope-validation.el` consumes. Both sides have evolved. The handler
output shape (`:op`, `:path`, `:confidence`, ...) is the contract.

- **What to scan**: any change to a bash-parser plugin or to the scope
  validator that touches handler output. Verify all handlers in the
  plugin set produce the full key set declared by the register entry.
- **Drift pattern**: silent missing key (the `:confidence` blind spot
  that surfaced when contract tests were finally written).
- **Maps to**: `class: invariant-gap` (no contract test) or
  `class: shape-fragmentation` (handler diverges from contract).

### 7. Cross-subsystem responsibility leakage in gptel

`gptel/{sessions,skills,tools,scope,scope-profiles}` are real boundaries.
Helpers grow on the wrong side: a scope-y helper landing in
`scope-expansion` when it belongs in `scope-yaml`; a sessions helper
landing in `sessions-commands` when it belongs in `sessions-registry`.

- **What to scan**: every new helper added to a gptel subsystem. Compare
  function name and body intent against the module's stated
  responsibility (in `openspec/specs/gptel/<subsystem>.md` and the
  module's `.org` header).
- **Maps to**: `class: responsibility-leakage`.

### 8. Test-framework split

ert (`*-test.el`) and buttercup (`*-spec.el`) coexist. They run in
separate Emacs processes (ert's `kill-emacs` would otherwise kill the
buttercup run). New test files going to ert when the module already
has buttercup tests is a smell; mixing both inside one file is a
blocker.

- **Maps to**: `class: duplication` (when both frameworks test the
  same behaviour) or `class: invariant-gap` (when neither covers a
  behaviour the design specifies).

## Mutation patterns to scan for

The bash-parser chain decomposer's `assq-delete-all` on a shared
`var-context` alist is the canonical bug. Scan diffs for:

- `nconc`, `nreverse` on caller-owned lists.
- `setf`, `setcar`, `setcdr` on values that flow out.
- `assq-delete-all`, `delete-dups`, `puthash` on hash tables passed by
  reference.
- defvar-with-setq mutation patterns where defcustom or buffer-local
  would be correct.

`class: mutation`. Severity per overlay (`mutation: advisory` by
default; promote to blocking if the mutation crosses a `load_bearing`
boundary).

## Severity overrides explained

- `duplication: blocking` (overlay setting): in literate-org, a duplicate
  is twice as costly because it has to be kept in sync between two
  `.org` files **and** their tangled `.el`s. The same helper tangled
  from two sources is a "pick one canonical home, route the other to
  it" cleanup, and we'd rather catch it early.

## What the architect should produce in forward-mode here

For the in-flight `gptel-scope-in-org-properties` change, expect to seed:

- **invariant** entries: parse-complete-is-true, coverage-threshold-is-1,
  allow-once-is-single-shot, scope-drawer-on-line-1-of-session.org
  property block.
- **vocabulary** entries: scope section keys
  (`read|write|modify|execute|deny|cloud-auth|cloud-providers`); the
  `+`-multi-value property convention.
- **boundary** entries: drawer ↔ scope plist (replaces yaml ↔ plist);
  buffer-first vs file-fallback read order.
- **shape** entries (opt-in): scope plist post-`:security`-removal;
  drawer-fixture shape used by `jf/gptel-test--with-scope-drawer`.

Each of these gets a `scaffolding/<tier>/<entry-id>.<ext>` file under
the change directory per `scaffolding.md`; for invariants this is a
failing buttercup test, for vocabulary a `pcase` scaffold listing every
section key as an explicit unimplemented arm, for boundaries a mapping
function shell with TODO body.
