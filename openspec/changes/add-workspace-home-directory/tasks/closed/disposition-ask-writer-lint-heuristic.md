---
name: disposition-ask-writer-lint-heuristic
description: User disposition for ask-cycle-20260526-171719-1 — the cross-module writer-lint's proximity heuristic mismatches scaffold.el's actual writer idiom, so the lint passes trivially
change: add-workspace-home-directory
status: done
relations:
  - discovered-from:port-cross-module-home-org-writer-lint
---

## Resolution (2026-06-06, manual review)

**User disposition: delete the lint** (a clean variant of Option D —
delete without the runtime-check substitute). The lint passed for two
unrelated wrong reasons (cycle-3 forward-only proximity window missing
the let-binding-before-write idiom; cycle-4 helper routing leaving zero
`"home.org"` literals in the writer body), so it gave false "covered"
confidence. The architect rated the finding advisory and noted the lint
was "(significantly more) decorative" — the invariant is genuinely true
and per-module review is the primary defense. Building a call-graph
static lint (Option C) to prop up a decorative secondary check was not
warranted.

Implemented:
- Removed `config/workspaces/test/home-org-writer-lint-spec.el`.
- `interfaces.org` — `register/invariant/home-org-user-authored-after-
  creation`: rewrote `enforcement_mechanism` from `kind: lint` to
  `kind: review` (per-module review primary; `home-org-spec.el`'s
  read-only assertion on the reader module as the narrow automated
  backstop), and recorded the removal in `status_note`.

No runtime check was added: the invariant holds, the scaffold writer
already guards with `unless (file-exists-p path)`, and a new writer is
a single-site reviewable change.

Verified: `./bin/run-tests.sh -d config/workspaces` → 231 passed.

<!-- Provenance fields (orchestrator schema):
     discovered_from: port-cross-module-home-org-writer-lint
     discovered_by: reviewer
     discovered_class: spec-signal
     ask_id: ask-cycle-20260526-171719-1
     review_file: .orchestrator/cycles/cycle-20260526-171719/reviews/port-cross-module-home-org-writer-lint.md
     status: blocked (awaiting user disposition; closes when next cycle's
       handshake.asks_for_user_resolved records the decision) -->

## The ask

The cross-module structural lint at
`config/workspaces/test/home-org-writer-lint-spec.el` (landed cycle-3
via `port-cross-module-home-org-writer-lint`) is meant to enforce
`register/invariant/home-org-user-authored-after-creation`
(load_bearing: true) by walking every `.el` under `config/workspaces/`
(excluding `test/`), scanning for file-write primitives near
`"home.org"` literals, and asserting the only match is the canonical
permitted writer `workspace--scaffold-write-home-org` in `scaffold.el`.

The lint's prescribed proximity heuristic — *forward-only*, ~200
characters between write primitive and `"home.org"` literal — is
mismatched to the actual writer's idiom. In `scaffold.el`, the
`"home.org"` literal sits in a surrounding `let` binding ~58 bytes
**BEFORE** the `with-temp-file` call:

```elisp
(defun workspace--scaffold-write-home-org (workspace-home name)
  (let ((path (expand-file-name "home.org" workspace-home)))
    (unless (file-exists-p path)
      (with-temp-file path
        ...))))
```

The lint scans past the literal (forward-only); zero candidate hits;
allow-list never fires. The assertion passes trivially. A future
regression that mirrors the same let-binding idiom (a sibling writer
in another module that puts the path on top and the write below) would
slip through undetected.

## Why this requires user disposition (not auto-fix)

The lint's exact shape — proximity model, window size, direction — was
prescribed in the original cycle-1 scaffold and re-stated in the cycle-3
task body verbatim. Changing it is not a bug-fix; it's a decision about
how the invariant is enforced. The options have different costs and
different ergonomics.

## Options for the user

**Option A — Bidirectional window (smallest change)**

Extend the heuristic from forward-only to ±200 chars (or another
sensible value). Catches the let-binding idiom for any writer. Risk:
false positives on docstrings or string literals that mention both a
write primitive and `"home.org"` in adjacent text. Low risk in
practice; can be tightened with stop-on-paren-balance if it fires.

**Option B — S-expression-aware path resolution**

Replace the proximity model with: parse each candidate function body
as elisp; for each call to a write primitive, evaluate the path-arg's
form in the enclosing lexical scope (or its symbolic shape) and check
whether `"home.org"` is reachable. Catches let-binding, intermediate
helpers, dynamic paths if the helper's signature reveals the pattern.
Higher complexity; possibly a separate `home-org-cross-module-lint.el`
module.

**Option C — AST-level walk against `(write-region with-temp-file ...)` call sites**

Use Emacs's bytecode-disassembler or the `pcase`-style walk-form
machinery (`cl-defmacro walk-tree` or similar) to identify every
write-primitive call site and inspect each call's path-arg AST.
Catches everything proximity catches plus arbitrary indirection.
Highest complexity; might require a third-party AST walker.

**Option D — Keep the lint trivially-passing and replace with a
runtime check**

Drop the static lint; add a runtime check at `workspace--scaffold-write-home-org`
that emits a warning if it's called from a non-`scaffold.el` location
(via `(symbol-file 'caller)` or similar). Lower fidelity than a
compile-time lint; doesn't catch additions that never run at startup.

## Recommendation

The reviewer routed this as `spec-signal` precisely because it's a
spec-shape decision, not an implementation defect. **Recommendation:
Option A** if the user wants a minimal patch that closes the
identified gap; **Option B** if the user is willing to invest in
durable enforcement of the load-bearing invariant.

## Files to modify (per chosen option)

- Option A: `config/workspaces/test/home-org-writer-lint-spec.el`
  (extend the scan window)
- Option B: New module
  `config/workspaces/test/home-org-cross-module-lint.el`
  (lifted into a real module, not a spec helper)
- Option C: Same as B + AST walker dependency
- Option D: `config/workspaces/scaffold.el` (add runtime check) +
  delete `home-org-writer-lint-spec.el`

## Verification

```bash
./bin/run-tests.sh -d config/workspaces
```

The chosen option must produce at least one positive hit on the
existing `workspace--scaffold-write-home-org` writer (i.e. the
allow-list actually fires). A trivially-passing assertion is the
failure mode this task exists to fix.

## Cited register entries

- `register/invariant/home-org-user-authored-after-creation`
  (load_bearing: true) — the invariant the lint is meant to enforce.

## Notes for the implementor

This task is **blocked on user disposition**. When the user resolves
`ask-cycle-20260526-171719-1` in a future cycle's handshake
(`asks_for_user_resolved`), the orchestrator will update this task's
`status: blocked → ready` and the cited "Options for the user"
section will be updated to reflect the chosen option as the
implementation target.

Until then: do not implement. The task body's options are not yet
the implementation contract.

## Context

- Reviewer file: `.orchestrator/cycles/cycle-20260526-171719/reviews/port-cross-module-home-org-writer-lint.md`
- Reconciliation note: `.orchestrator/cycles/cycle-20260526-171719/reconciliations/invariant-home-org-user-authored-after-creation.md`
- PM digest (this cycle): `.orchestrator/cycles/cycle-20260526-171719/pm-digest.md` § Asks for the user

## Cycle 4 updates (cycle-20260526-191802)

### Aggravating evidence: architect finding `arch-cycle-20260526-191802-01`

Cycle-4's `route-scaffold-writer-through-home-org-helper` (merge
`7f74a01`) routed `scaffold.el`'s home.org writer through the
canonical helper `workspace-home-org-path`. As a side effect, the
literal string `"home.org"` no longer appears anywhere in the
writer's source body — the path is now resolved by a helper call.

The cycle-4 architect end-of-cycle audit
(`.orchestrator/cycles/cycle-20260526-191802/findings/arch-cycle-20260526-191802-01.md`)
identified that this leaves the cross-module writer-lint with
**zero** literals to match against its proximity heuristic, even
ignoring the cycle-3 let-binding-before-write gap. The lint's
allow-list arm (which checks "is this writer the permitted
`workspace--scaffold-write-home-org`?") is structurally
unreachable because the input set to `cl-remove-if` is now always
empty.

Net effect: the writer-lint passes for **two different wrong
reasons** post-cycle-4:

1. **Cycle-3 reason** (open in this ask's body): proximity
   heuristic is forward-only, so a literal in a let-binding
   *before* the write is missed.
2. **Cycle-4 reason** (this stanza): no literal exists in the
   writer's body at all — the writer routes through a helper
   that resolves the literal in a different module.

### Decision frame extended

The original options (bidirectional window; s-expression-aware
path resolution; AST-walk against write-region/with-temp-file
sites) addressed only the cycle-3 reason. They do not address the
cycle-4 reason — even a bidirectional window over scaffold.el's
writer body would find zero `"home.org"` literals.

A robust resolution must address both. The cycle-4 architect
recommends one of:

**Option C (call-graph-based detection)**: enumerate every defun
in `config/workspaces/*.el` whose body invokes any write
primitive AND invokes (directly or transitively)
`workspace-home-org-path`. Compare against the allow-list.
Catches both old (literal-in-let) and new (literal-in-helper)
failure modes. Cheap because the call set is tiny.

**Option D (helper-recognition extension)**: extend the lint's
`write-fn-symbols` set to also fire on `workspace-home-org-path`
calls, treating helper invocation as equivalent to literal
"home.org" reference. Partial fix — a future second-layer
indirection would re-introduce the same gap.

### Recommended disposition (per cycle-4 architect)

**Option C (call-graph)** is the preferred resolution. It is
robust to arbitrary indirection depth and matches how a human
reviewer would actually audit the writer surface.

If the user chooses Option C, the disposition payload should
explicitly cover both cycle-3 and cycle-4 failure modes (the
implementation must list the path-resolution call set, not just
the literal-string set).

### Severity context

Architect rated finding `arch-cycle-20260526-191802-01` as
**advisory** (not blocking) because:

- The invariant `register/invariant/home-org-user-authored-after-
  creation` remains *true* — no other module writes `home.org`.
- The cycle-4 routing strengthens the boundary
  `register/boundary/home-org-read-pipeline` (single canonical
  path resolver).
- Per-module review remains effective as the primary defense.

The cross-module lint is the *secondary* defense, now
(significantly more) decorative.


