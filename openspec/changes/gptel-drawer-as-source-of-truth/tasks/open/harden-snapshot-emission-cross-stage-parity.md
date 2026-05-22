---
name: harden-snapshot-emission-cross-stage-parity
description: Cycle-6 reviewer of fix-snapshot-tools-test-mock-and-dedupe-applicator surfaced two leaks in the cross-stage idempotency story that the dedupe didn't fully retire. (a) `gptel-chat--write-config-drawer` is a third producer of the same six-key snapshot set, so a seventh-key author still has to edit two places. (b) `--snapshot-lines` joins multivalued payloads with bare `mapconcat`, while `--apply-to-drawer` routes through `org-entry-protect-space` — agreement on key set + ordering, but disagreement on value escaping. Address both so the cross-mode invariant is enforced rather than documented.
change: gptel-drawer-as-source-of-truth
status: done
relations:
  - discovered-from:fix-snapshot-tools-test-mock-and-dedupe-applicator
---

## Files to modify

- `config/gptel/scope-profiles.org` — extend `--snapshot-entries` to accept a buffer-local snapshot plist (or expose a small extractor); align multivalued formatter with `org-entry-protect-space`
- `config/gptel/chat/menu.org` — route `gptel-chat--write-config-drawer` through the shared enumeration (or, at minimum, add a cross-reference comment + narrow the `--snapshot-entries` docstring claim)

## Why

**Finding 1 (spec-signal).** The cycle-6 dedupe collapsed `--snapshot-lines` and `--apply-to-drawer` onto a shared `--snapshot-entries` enumeration, but `gptel-chat--write-config-drawer` (chat-mode save path) emits the same six keys (`:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:`, `:GPTEL_TOOLS:`) from buffer-local symbols. Adding a seventh snapshot key still requires two edits. The post-dedupe docstring claims `--snapshot-entries` is "the *only* enumeration of the snapshot key set" — true for the PRESET-SPEC pipeline, but inaccurate when read against the boundary register's full producer triangle.

**Finding 2 (advisory).** `--snapshot-lines` joins multivalued snapshot values with `(mapconcat #'identity value " ")`. `--apply-to-drawer` routes through `org-entry-put-multivalued-property` (which escapes whitespace via `org-entry-protect-space`). This is a documented asymmetry — the docstring asserts tool names "are upstream-validated identifiers without whitespace" — but **nothing enforces it.** `--resolve-tool-names` does not validate names, so a downstream caller passing `'(:tools ("name with space"))` would produce a five-token rendered string that round-trips through `org-entry-get-multivalued-property` as five distinct tools, breaking the mode-2a ≡ mode-2b idempotency invariant.

## Implementation steps

1. **Pick one of two shapes for the third-producer dedupe** (Finding 1):
   - **(Preferred)** Extend `--snapshot-entries` to accept either a PRESET-SPEC plist *or* a buffer-local snapshot plist constructed by a small new extractor (`gptel-chat--current-snapshot-plist` or similar) that reads `gptel-model`, `gptel-backend`, `gptel-temperature`, `gptel-max-tokens`, `gptel-num-messages-to-send`, `gptel-tools` from the current buffer. `gptel-chat--write-config-drawer` then becomes a third caller of `--snapshot-entries`, dispatching on `KIND` exactly like `--apply-to-drawer`.
   - **(Minimum)** Narrow the `--snapshot-entries` docstring to "the *only* enumeration of the snapshot key set *for the PRESET-SPEC pipeline*", and add a tickling cross-reference comment in `gptel-chat--write-config-drawer` pointing at `--snapshot-entries`. Less leverage; consider only if option (a) reveals friction.
2. **Align multivalued formatter with `org-entry-protect-space`** (Finding 2):
   - In `--snapshot-lines`, when `KIND :multivalued`, route values through `org-entry-protect-space` before `mapconcat`. Costs nothing for whitespace-free identifiers (the common case) and matches mode-2b on all inputs.
   - Optionally, for a belt-and-braces version, also add an assertion in `--snapshot-entries` (or `--resolve-tool-names`) that multivalued elements contain no embedded newline (whitespace handled by escape, but newlines in values are an org-property hard-constraint).
3. Re-tangle: `./bin/tangle-org.sh config/gptel/scope-profiles.org` and `./bin/tangle-org.sh config/gptel/chat/menu.org`.
4. Add tests:
   - In `config/gptel/test/snapshot-rendering-spec.el`, add a scenario asserting `'(:tools ("name with space"))` produces a single-token-after-unescape round-trip in mode 2a (i.e. `--snapshot-lines` followed by `--apply-to-drawer` recovers `'("name with space")`).
   - If option (a) was chosen for Finding 1, add a buttercup scenario asserting `gptel-chat--write-config-drawer` emits exactly the same six keys (in the same order) as `--snapshot-lines` for an equivalent buffer-local state.
5. Re-run `./bin/run-tests.sh -d config/gptel/test`, `./bin/run-tests.sh -d config/gptel/chat`, and `./bin/run-tests.sh -d config/gptel` for regressions.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/test
./bin/run-tests.sh -d config/gptel/chat
./bin/run-tests.sh -d config/gptel
grep -nE "mapconcat #'identity|org-entry-protect-space" config/gptel/scope-profiles.el
grep -n "snapshot-entries\|six-key" config/gptel/chat/menu.el
```

Expect: only `org-entry-protect-space` (or equivalent escaped form) appears in the multivalued formatter; `gptel-chat--write-config-drawer` is either a caller of `--snapshot-entries` (option a) or has the cross-reference comment (option b).

## Context

Reviewer findings: `.orchestrator/cycles/cycle-1777803706/reviews/fix-snapshot-tools-test-mock-and-dedupe-applicator.md` (Findings 1, 2).

Cited register entries: `interfaces.org#register-shape-drawer-text-block` (structural_invariants), `interfaces.org#register-boundary-scope-profile-applicator` (cross_stage_invariants).
