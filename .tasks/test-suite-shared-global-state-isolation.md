---
name: test-suite-shared-global-state-isolation
description: "Buttercup specs share one Emacs process and mutate global production state (notably the external gptel--known-tools tool registry) with no teardown, so one spec file silently contaminates later, unrelated specs. Tests pass in isolation but fail in full-suite runs. Root cause is established; this task is for a dedicated deep dive on the isolation architecture."
source: scope-deny-symbolp-crash-parallel-tools (test-isolation root cause surfaced while fixing that crash)
status: ready
relations:
  - discovered-from:scope-deny-symbolp-crash-parallel-tools
  - related-to:gptel-preexisting-async-scope-test-failures
---

## Problem

Our buttercup test suite has **no isolation of global mutable state between
specs**. Specs that mutate a global and don't restore it silently change the
environment for every later spec in the run. Because the suite runs as a single
process that loads every spec file and then runs them in one pass, the result is
**cross-file, load-order-dependent contamination**: a spec is correct in
isolation (or per-directory) but fails in a full-suite run, depending entirely
on what some unrelated earlier file did.

This is the underlying cause of a large share of the "flaky in batch / async
callback never fired" failures previously parked in
[[gptel-preexisting-async-scope-test-failures]] (~18 of those 21 failures were
this single class — see Impact).

## Mechanism (root cause — established)

Three facts combine:

1. **Single-process, shared-state runner.**
   `jf/test-run-buttercup-directory-batch` / `jf/test-run-all-buttercup-batch`
   (`config/core/testing.el`) `load-file` *every* `*-spec.el` into one Emacs
   process, then call `buttercup-run` once. All specs across `chat/`, `scope/`,
   `tools/`, `sessions/`, … share the same global environment. Buttercup
   auto-restores **spies** after each spec, but nothing else (defvars, registries,
   advice).

2. **A single global mutable registry, owned by an external package, used as
   both production state and a test sink.** `gptel--known-tools`
   (`runtime/straight/repos/gptel/gptel-request.el:1359`) is a global alist in
   the *vendored* gptel package. `gptel-make-tool` (same file, ~1405) is a
   *registering* constructor: it builds the struct and then `setf`s it into
   `gptel--known-tools` keyed by name+category (~1482–1488), overwriting any
   existing entry, with no unregister and no scoping. So calling `gptel-make-tool`
   anywhere — including a test fixture — is a global side effect that persists
   for the rest of the process.

3. **Integration tests read that same global to exercise the real tool.** Helpers
   like `parallel--find-tool` (and siblings) walk `gptel--known-tools` to pull the
   *real* production tool and drive it through the real wrapper. They therefore
   consume whatever the last writer left in the registry.

Put together: a `chat/` stream test built a stub tool with
`gptel-make-tool :name "run_bash_command" :function #'ignore` merely to obtain a
struct, which **overwrote the real `run_bash_command` entry**. `chat/` sorts
before `scope/`, so by the time `scope/` integration specs pulled
`run_bash_command` from the registry, its function was `#'ignore` — every
invocation became a no-op (no callback fired, no expansion shown), failing the
FSM/callback assertions. None of this reproduces in a single-file or
single-directory run, because the contaminating file never loads there.

## Two coupled smells

- **Write side:** tests mutate shared production global state with no teardown.
- **Read side:** integration tests depend on that global being pristine.

Either alone is survivable; together, every spec's correctness depends on every
other spec's cleanup, across files, in load order.

## Systemic surface (this is a class, not one registry)

The registry is the instance we hit, but the same hazard exists elsewhere:

- **Our own frame-globals** `jf/gptel-scope--expansion-active` and
  `jf/gptel-scope--expansion-queue` are mutated by ~8 test files with
  *inconsistent* discipline (e.g. `expansion-ui-handlers-spec.el` has zero
  `after-each`; others reset in `before-each`, others in `after-each`). It
  survives today only because most expansion specs defensively reset on entry —
  one new spec that doesn't would reintroduce the same cross-spec leakage.
- **Global advice** (`persistent-agent-trace` uses `advice-add`) is another
  process-wide mutation that can leak if enabled and not removed.
- There is **no harness-level snapshot/restore** of any of these (confirmed
  absent in `config/core/testing.el` and the `helpers-spec.el` files).
- The interim mitigation in commit `35bd4619` ("don't name fixtures after real
  tools / use the non-registering constructor") is a **naming convention with no
  enforcement**. It is fragile as the tool catalog grows: `tool-confirm-spec.el`
  still uses the registering `gptel-make-tool` (name `"downloader"` — no
  collision *today*), and nothing prevents a future fixture from reusing a real
  name and silently regressing the exact bug.

## Impact

- Failures that look like product bugs but are pure test contamination.
- Full-suite and per-file/per-directory results disagree, eroding trust in the
  suite and in CI signal.
- It actively obstructed the `scope-deny-symbolp-crash-parallel-tools`
  investigation: a corrected regression test appeared "flaky" until the registry
  clobber was found.
- Quantified: fixing two chat-stream fixtures (commit `35bd4619`) moved
  `./bin/run-tests.sh -d config/gptel -f buttercup` from **21 failed → 3 failed**.
  The remaining 3 (add-to-scope drawer-write cluster) are a *separate* issue and
  out of scope here.

## Evidence / repro pointers

- Diagnostic (temporary `it` added at the failure site, since removed): under a
  full `config/gptel` run, `(gptel-tool-function <run_bash_command>)` returned
  `ignore`; `jf/gptel-scope--validate-bash-tool` correctly denied; treesit bash
  was available; and a *direct* `jf/gptel-scope-authorize-tool-call` worked
  (showed the transient, fired the callback). I.e. the deny path was intact — the
  tool was simply stubbed out from under it.
- Registering fixtures at the time of discovery:
  `config/gptel/chat/test/stream/tool-call-spec.el` and
  `multi-round-tool-use-spec.el` (fixed in `35bd4619`);
  `tool-confirm-spec.el` (still registering, name `"downloader"`).
- Runner: `config/core/testing.el` `jf/test-run-buttercup-directory-batch`
  (~218–229) and `jf/test-run-all-buttercup-batch` (~192–209).
- External registry + registering constructor:
  `runtime/straight/repos/gptel/gptel-request.el:1359` (`gptel--known-tools`),
  `~1405` (`gptel-make-tool`), non-registering struct constructor
  `gptel--make-tool` (~1354).
- Related commits: `a14c069c` (the symbolp-crash fix this surfaced under),
  `35bd4619` (the interim fixture mitigation).

## Questions to scope the deep dive (problem boundary, not solution)

- Is the target "the `gptel--known-tools` registry specifically" or "shared
  mutable global state as a class" (which globals must a spec be insulated from)?
- What is the desired isolation contract — what should a spec be able to assume
  about the global environment it starts in, and should a leak be *detectable*
  (fail at the offending spec) rather than only avoidable?
- What per-spec / per-file overhead is acceptable? This bounds the design space
  and is the main tradeoff to settle before choosing an approach.
- Full inventory of process-global mutations our specs perform (registries,
  defvars, advice, hooks, buffer-locals that outlive their buffer) — needed to
  size the problem before designing anything.

(Original framing deliberately carried no solutions. The deep-dive
context below was added on 2026-06-18.)

---

## Deep-dive context (2026-06-18)

### Framing decision (what we are and are not pursuing)

The design space has three layers. We are NOT committing to the reflexive
fix — "add registry teardown to the existing single-process suite" — because
that accepts the premise we most distrust: that integration tests *should*
read a mutable process-global to acquire their subject. The explicit
rejected approach is **pouring in more code to patch every site that can
touch shared state** (exhaustive snapshot/restore + a complete global
inventory). That is the same remember-to-clean-up discipline that already
failed here, and it is leaky by construction (only as complete as the
inventory).

The guiding principle is to **prefer structural guarantees (fresh process,
dynamic binding, explicit hand-off) over per-site bookkeeping.**

Primary targets:

- **A — Process isolation (runner).** Run each spec file in its own
  `emacs --batch` process. Cross-file contamination becomes structurally
  impossible; concurrency falls out for free (N hermetic OS processes under
  `make -jN` / `xargs -P`, not in-Emacs threads). This is also the Emacs
  ecosystem norm (eldev, makem.sh, Cask all isolate per file/process).
- **C — Remove the read-side dependency (production + tests).** Tests are
  *handed* the real tool struct (or look it up in a `let`-bound local
  registry) instead of fishing it from the process-global `gptel--known-tools`.

Mine **B (in-process hygiene)** only for principles, not the patch-everything
build-out:

- Keep the **leak canary as a *diagnostic*** — a tiny post-spec check that the
  registry is unchanged, failing loudly and *locally*. Detect, don't
  prevent-everywhere. Most useful as an intra-file safety net and during an A
  migration.
- Adopt **`cl-letf`-scoped mocking** over the hand-rolled `fset`-save/teardown
  in the `helpers-spec.el` files (`config/gptel/scope/test/helpers-spec.el:308-346`).
  This is already the project's stated behavioral-test principle (CLAUDE.md)
  that those helpers violate; cleanup by dynamic scope is automatic even on
  error — the anti-patch-everything move.

### Probe 1 — process-isolation cost (the bottleneck is NOT startup)

Measured per-process startup (this machine, Emacs.app batch):

| Bootstrap                                   | Per-process cost |
|---------------------------------------------|------------------|
| Full init (`early-init` + `init` + testing) | ~13.3s           |
| Straight load-path + buttercup only         | ~0.3s            |
| Straight + gptel subsystem loaded by path   | ~1.3s (errored)  |

Startup is cheap; with N independent processes run concurrently, wall-clock is
core-bound, not N×. **The real cost of A is reproducing the dependency
closure**, not process spin-up. A lean bootstrap cascaded because the suite
only runs today on the implicit closure full init pre-`provide`s:

- `config/gptel/scope/scope-validation.el` requires `bash-parser-core` — a
  *separate* subsystem (`config/bash-parser/`) the gptel loader
  (`config/gptel/gptel.el`) never loads → it fails → never `provide`s
  `jf-gptel-scope-validation` → `scope-tool-wrapper` →
  `scope-expansion`/`scope-shell-tools`/`scope-filesystem-tools` all cascade.
- `use-package :straight` keyword unrecognized unless straight's use-package
  integration is activated (init does this; a bare bootstrap does not).
- `gptel.el` calls `evil-define-key` at load — assumes `evil` is globally
  present.
- Bare-name `require`s resolve by **load-order-by-path**, and feature names do
  not match filenames (`gptel-session-constants` ⇐
  `config/gptel/sessions/constants.el`), so load-path lookup can NEVER satisfy
  them. They work only because init loaded the file by absolute path earlier
  (`jf/load-module`, `init.org:52`/`init.org:85`).

Per-file isolation therefore *forces these hidden ordering couplings into the
open* — latent debt the single-process design masks.

Three shapes for A:

1. **Full-init-per-process** — each process runs existing init then one file.
   Correct, zero new bootstrap to maintain, ~13.3s/file but fully parallel.
   Fine for CI at `-j(cores)`, heavy locally.
2. **Lean curated bootstrap** — ~1s/file, but new infrastructure that must
   track the module graph (the maintenance burden we want to avoid).
3. **Portable dump (recommended middle)** — run full init *once*,
   `dump-emacs-portable` to a `.pdmp`, then every spec process starts from the
   dump in ~0.3s with the full correct environment. Best-of-both: full-init
   correctness + minimal-bootstrap speed, standard Emacs tooling, nothing
   extra to hand-maintain.

### Probe 2 — the read side is mostly legitimate; the write side is small

All 15 registry-reading specs categorized by whether they look up a *real
production* tool (true wiring) vs a name they *self-register* (only need a
struct):

**True wiring (look up real tool, never self-register) — 11:**
`tools/test/persistent-agent/creation-spec.el` (PersistentAgent),
`chat/test/menu/preset-wiring-spec.el`,
`scope/test/tool-wrapper/request-scope-expansion-operation-spec.el`,
`scope/test/integration/deny-serialization-spec.el`,
`.../filesystem-scope-integration-spec.el`,
`.../work-root-execution-context-spec.el`,
`.../work-root-worked-example-spec.el`,
`.../parallel-tool-callback-spec.el`,
`.../bash-add-to-scope-bug-spec.el`,
`.../bash-scope-expansion-integration-spec.el`,
`.../bash-multi-violation-expansion-spec.el`.
These are *correct* to want the real wired tool.

**Writers (self-register into the global — the contamination source) — 4:**
`chat/test/stream/tool-call-spec.el` and `.../multi-round-tool-use-spec.el`
(already de-fanged in `35bd4619`),
`scope/test/tool-wrapper/async-flag-spec.el` (registers `test_async_flag` /
`test_callback_sig`) and `.../non-utf8-output-spec.el` (registers
`non_utf8_bash_shape`). No collision *today*, but they mutate the
process-global.

Implication for C: cannot "make everything build its own struct" — 11/15
legitimately want the real wired tool. C therefore =
(a) split *build* from *register* in `gptel-make-scoped-tool`
(`config/gptel/scope/scope-tool-wrapper.el:45`) so there is one struct builder
+ a thin idempotent `install`; (b) point the ~4 writers at the builder (no
global mutation); (c) point the ~11 readers at a `let`-bound local
`gptel--known-tools` populated by `install`. No drift, no global touched by any
test. Note the registration is currently a bare top-level side effect of
`require` (`scope-filesystem-tools.el:41`); making it an explicit, re-runnable
install is itself a production-hygiene improvement.

### Recommendation / sequencing (for continued examination)

- **A (preferably via portable dump) is the structural fix for the bug class.**
  Under per-file isolation, writer contamination cannot cross files regardless
  of C. High-confidence "make this category impossible"; the dump keeps it
  cheap. Open question to settle: is the per-file `.pdmp` startup actually
  ~0.3s end-to-end (Probe 1c was bootstrap-only and errored before running a
  spec), and how does aggregation across N processes report results + exit code
  (precedent: `run-tests.sh --report` already parses buttercup output).
- **C is the durable read-side cleanup**, smaller than feared on the write side
  (4 specs), mechanical on the read side (11 specs + one production refactor).
  Valuable independently but NOT urgent once A lands.
- **B contributes only the canary + `cl-letf` discipline**, not a build-out.

Suggested order: prototype A as a portable-dump runner (confirm the ~0.3s/file
number and design N-process aggregation) → do C as the follow-on (split
build/register, migrate 4 writers + 11 readers) → add the canary in passing.
When this graduates from "examine options" to "design + implement," promote to
a full OpenSpec change (per `.tasks/` promotion bar).

### Evidence pointers added by this deep dive

- Runner: `config/core/testing.el` `jf/test-run-buttercup-directory-batch`
  (~211) / `jf/test-run-all-buttercup-batch` (~192) `load-file` all specs into
  one process then `buttercup-run` once.
- Batch bootstrap loads FULL init: `Makefile:26-30` (`EMACS_TEST_BATCH`).
- Module loader (path-based, not load-path): `init.org:52` (`jf/load-module`),
  `init.org:85` (`jf/resolve-module-path`); gptel subsystem cascade:
  `config/gptel/gptel.el:87-209`.
- Scoped-tool macro (build/register lever for C):
  `config/gptel/scope/scope-tool-wrapper.el:45`; production registration as a
  top-level side effect: `config/gptel/scope/scope-filesystem-tools.el:41`.
- Hand-rolled fset save/teardown to replace with `cl-letf`:
  `config/gptel/scope/test/helpers-spec.el:308-346`.
