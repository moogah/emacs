---
name: fix-snapshot-tools-test-mock-and-dedupe-applicator
description: The two modify-list tool tests in `snapshot-rendering-spec.el` `let`-bind `gptel-tools` but the implementation reads `(default-value 'gptel-tools)`, so the tests pass for the wrong reason in batch (where the global default is nil). Fix the mock scoping. Then extract the snapshot-key emission shared by `--snapshot-lines` and `--apply-to-drawer` so they can no longer drift silently when a new key is added.
change: gptel-drawer-as-source-of-truth
status: ready
relations:
  - discovered-from:extend-render-drawer-text-with-preset-snapshot
---

## Files to modify

- `config/gptel/test/snapshot-rendering-spec.el` — fix the `let` → `cl-letf` (or equivalent) on the two modify-list tool tests
- `config/gptel/scope-profiles.org` (and re-tangled `.el`) — extract the shared snapshot-key emission

## Why

**Finding 1 (blocking).** The tests at lines 123 and 130 use `(let ((gptel-tools '(base-A base-B))))` and `(let ((gptel-tools nil)))`. But `jf/gptel-scope-profile--resolve-tool-names` reads the base via `(default-value 'gptel-tools)`. For a `defcustom`, `default-value` ignores `let` dynamic bindings — it returns the global default. In a batch test process where `gptel-tools` is globally nil, the `(:append (extra-C))` test passes because `(append nil '(extra-C))` is `(extra-C)`, not because the merge with `(base-A base-B)` was exercised. The tests are inert; a regression in the merge logic would not be caught.

**Finding 2 (advisory).** `--apply-to-drawer` (scope-profiles.el:436–460) re-implements the same six-key snapshot-mapping that `--snapshot-lines` (scope-profiles.el:274–307) already owns — same key list, same `plist-member` guard for `:tools`, same numeric guards. Adding a seventh snapshot key requires editing two places. This is exactly the kind of duplication that makes the cross-mode idempotency invariant decay silently.

## Implementation steps

1. In `config/gptel/test/snapshot-rendering-spec.el`, replace each `let`-binding of `gptel-tools` in the modify-list tool tests with one of:
   - `cl-letf` that overrides the `default-value` accessor at the symbol level, or
   - `cl-letf` that shadows `gptel--modify-value` directly (cleaner if the merge function is the unit under test), or
   - `setq gptel-tools VALUE` paired with an `unwind-protect` (or `before-each`/`after-each`) cleanup to restore the original default.
2. Re-run `./bin/run-tests.sh -d config/gptel/test` and confirm the two scenarios now actually exercise the documented base values. Sanity-check by deliberately breaking `gptel--modify-value` and confirming the tests fail.
3. In `config/gptel/scope-profiles.org`, extract the snapshot-key mapping into a shared form. Two viable shapes:
   - A small alist `((:model . :GPTEL_MODEL) (:backend . :GPTEL_BACKEND) ...)` with per-key value-formatter functions, iterated by both producers.
   - Have `--apply-to-drawer` parse `(jf/gptel-scope-profile--snapshot-lines preset-spec)` back to property pairs and call `org-set-property` per pair. Tools needs special handling either way (multivalued).
4. Re-tangle: `./bin/tangle-org.sh config/gptel/scope-profiles.org`.
5. Re-run `./bin/run-tests.sh -d config/gptel` to confirm no regressions.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/test
./bin/run-tests.sh -d config/gptel
grep -nE "default-value 'gptel-tools|let \(\(gptel-tools" config/gptel/test/snapshot-rendering-spec.el
```

Expect: no remaining `let`-binding of `gptel-tools` in the test file. All snapshot-rendering specs pass. No regressions elsewhere.

## Context

Full reviewer findings: `.orchestrator/cycles/cycle-1777625426/reviews/extend-render-drawer-text-with-preset-snapshot.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block`, `interfaces.org#register-boundary-scope-profile-applicator` (the `cross_stage_invariants` clause).

## Observations

- **Finding 1 fix shape.** Replaced `(let ((gptel-tools ...)))` with `(let ((original (default-value 'gptel-tools))) (unwind-protect (progn (setq-default gptel-tools ...) ...) (setq-default gptel-tools original)))` in both modify-list test bodies. Chosen over `cl-letf` on `(symbol-function 'default-value)` (too invasive — affects every consumer in the test) and over `cl-letf` on `gptel--modify-value` (would weaken the test to "the resolver dispatches to the merge function" without actually exercising the documented base list). The `setq-default` form mutates the global default, runs the real `gptel--modify-value` against `(base-A base-B)`, and restores cleanly even if the body errors.
- **Finding 1 negative-controls verified.** Sanity-checked by mutating `runtime/straight/repos/gptel/gptel.el`'s `:append` arm to `(append form current)` — the modify-list test failed with `(":GPTEL_TOOLS: extra-C base-A base-B")` instead of `(":GPTEL_TOOLS: base-A base-B extra-C")`. Reverted before commit. The previous `let`-binding form would have continued to pass against this mutation in batch (where the global default is nil and `(append nil '(extra-C))` is `(extra-C)`). Tests are now actually exercising the documented merge contract.
- **Finding 2 fix shape.** Extracted `jf/gptel-scope-profile--snapshot-entries` as the single ordered enumeration of (drawer-key, value, kind) tuples for the six chat-mode snapshot keys. `--snapshot-lines` is now a thin formatter (string mode); `--apply-to-drawer`'s snapshot block is now a thin dispatcher (`pcase` on `:scalar` / `:multivalued`). Adding a seventh key is one edit in `--snapshot-entries`. The `:GPTEL_SYSTEM:` write-side exclusion (`register/invariant/drawer-system-key-write-exclusion`) is preserved by simply not adding a `:system` clause to the entries builder.
- **Round-trip parity preserved.** The renderer's multivalued line shape (`mapconcat #'identity values " "`) is unchanged from the prior implementation, so the cross-stage idempotency invariant of `register/boundary/scope-profile-applicator` (rendered text written verbatim then re-applied is a no-op) continues to hold. Tools today carry no whitespace; if a future multivalued snapshot key does, the formatter must route through `org-entry-protect-space` to match `org-entry-put-multivalued-property`'s on-disk format. Documented in the `--snapshot-lines` docstring.
- **Test scope unchanged.** No new specs were added — the existing 39 specs in `snapshot-rendering-spec.el` already exercise both the renderer's formatted line emission and the applicator's drawer-write semantics. Both now pass against the deduped implementation; the round-trip idempotency spec (`describe "round-trip idempotency"`) is the cross-mode parity guard for the dedupe.

## Verification

```text
$ ./bin/run-tests.sh -d config/gptel/test
Ran 39 specs, 0 failed, in 638.30ms.

$ grep -nE "let \(\(gptel-tools" config/gptel/test/snapshot-rendering-spec.el
(no matches — let-binding removed)

$ ./bin/run-tests.sh -d config/gptel
Ran 1050 specs, 78 failed, in 6.67s.
(Same 78 failures present on baseline — pre-existing run_bash_command
timeout/truncation specs unrelated to snapshot work; verified by
stashing the changes and re-running.)
```

## Discoveries

- **`duplication` (project-promoted-blocking).** The snapshot key set is now extracted in `scope-profiles.org`, but two *additional* sites enumerate the same key set independently:
  1. `config/gptel/chat/menu.org` — `gptel-chat--write-config-drawer` (lines 469–534) is the chat-mode save-path peer producer cited in the register status note. It writes the same six keys (`GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`, `GPTEL_TOOLS`) plus `GPTEL_PRESET`, but reads from buffer-local variables instead of a preset spec, and uses delete-on-empty semantics (not omit-on-empty). The tool-name normalization block at lines 511–522 is *byte-for-byte the same* logic as the inner `mapcar` of `jf/gptel-scope-profile--resolve-tool-names`. Adding a seventh snapshot key still requires editing two sites: `--snapshot-entries` and `gptel-chat--write-config-drawer`. The reviewer's stated invariant — "cross-mode idempotency cannot decay silently when a new key is added" — is only partially achieved; the chat-mode save path remains an independent producer. Out of scope for this task (the task body cites only `scope-profiles.org`), but the architect should consider a follow-up that has `gptel-chat--write-config-drawer` consume `--snapshot-entries` (or a buffer-local variant of it) so the three modes (2a render, 2b apply, chat save) share one enumeration.
  2. `config/gptel/tools/org-roam-tools.org` lines 612–615 and 725–728 emit `:GPTEL_MODEL:` / `:GPTEL_BACKEND:` lines as part of a *roam-node* drawer template (different shape than the chat-mode config drawer; co-emits `:GPTEL_SESSION:`, `:GPTEL_AGENT:`, `:GPTEL_CREATED:`). Less concerning than (1) because the shape and source-of-truth are different, but a maintainer extending `--snapshot-entries` could reasonably miss it. Worth flagging in the register if/when the dedupe is generalized.
- **Affected register entry.** `register/boundary/scope-profile-applicator`'s status note (cycle-5 integrate) describes the chat-mode save path as "a peer producer of an equivalent drawer key set from buffer-local state." This task confirms that "equivalent" is currently maintained by manual key-set parity across two sites, not by shared code. The cross-stage invariant test (round-trip idempotency in `snapshot-rendering-spec.el`) covers modes 2a ↔ 2b but does not cover the chat-mode save path's contribution. Suggest the architect either: (a) widen the invariant test to include a `gptel-chat--write-config-drawer` round-trip after a no-edit save, or (b) refactor the chat-mode writer onto `--snapshot-entries`.
