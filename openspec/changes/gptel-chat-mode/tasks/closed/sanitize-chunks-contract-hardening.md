---
name: sanitize-chunks-contract-hardening
description: Tighten tests and docstrings around the stream insert / sanitize contracts (loud-fail, sibling flush, CR handling, error-message pinning, spec encoding)
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sanitize-chunks
  - discovered-from:marker-insertion-type-contract
  - discovered-from:sanitize-chunk-newline-guard
---

## Files to modify
- `config/gptel/chat/stream.org` (`--stream-insert-flush` docstring)
- `config/gptel/chat/stream.el` (re-tangled; possibly tightened newline
  regex)
- `config/gptel/chat/test/stream/streaming-spec.el` (new specs)
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (one-line behavioural scenario for the insertion-type contract)

## Implementation steps

### Loud-fail branch (sanitize-chunks Finding 1)
1. Add a Buttercup spec asserting that funcalling the stream inserter
   with a non-string, non-`t` value (e.g. `'abort`, `nil`,
   `(tool-call . _)`) signals an error. This pins the contract that
   Decision 10's dispatch layer relies on.

### Sibling flush helper docstring (marker-insertion-type-contract Finding 1)
2. Add the same "MARKER must have insertion-type `t`; enforced by
   `gptel-chat--make-stream-inserter`" sentence to
   `gptel-chat--stream-insert-flush`'s docstring. Currently only
   `--stream-insert-line` carries it.

### Spec-level encoding (marker-insertion-type-contract Finding 2)
3. Add a one-line scenario to `specs/gptel-chat-mode/spec.md` under the
   streaming insertion section:

   > The streaming inserter factory rejects markers that do not advance
   > past inserted text (`insertion-type` must be `t`).

### CR handling (sanitize-chunk-newline-guard Finding 1)
4. Decide explicitly what the sanitize-chunk contract is for line
   terminators:
   - If the intent is "no line terminators of any kind" — tighten the
     guard to `(string-match-p "[\n\r]" line)` and add a spec for
     bare `\r`.
   - If the intent is literally "no `\n`" (because `split-string` is
     the sole upstream author) — add a sentence to the docstring
     explicitly stating `\r` is not checked.

### Error-message pinning (sanitize-chunk-newline-guard Finding 2)
5. Strengthen the negative spec at `streaming-spec.el:171-173` from
   `(:to-throw 'error)` to `(:to-throw 'error '("LINE must not contain newlines"))`
   (or equivalent message-substring match) so a future refactor that
   signals for an unrelated reason doesn't rubber-stamp the contract.

### Edge cases (sanitize-chunk-newline-guard Finding 3)
6. Add additional negative specs covering `\n` at start, `\n` at end,
   multiple `\n` in one line, and only-`\n` — not strictly required
   since `string-match-p` is position-agnostic, but makes the contract
   self-documenting in the suite.

## Design rationale
Three parent tasks (`sanitize-chunks`, `marker-insertion-type-contract`,
`sanitize-chunk-newline-guard`) each surfaced small contract-hardening
findings during their 2026-04-21 reviews. They cluster on the same
artifacts (`stream.{org,el}`, `streaming-spec.el`, the chat-mode spec)
so they're grouped here rather than split into six micro-tasks.

Each step is independently valuable; none is blocking. Total diff
should be small (<~60 LoC).

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/stream` passes with new
  specs.
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- Grep confirms `specs/gptel-chat-mode/spec.md` mentions the
  insertion-type contract.

## Context
- Reviews of `sanitize-chunks`, `marker-insertion-type-contract`,
  `sanitize-chunk-newline-guard` (2026-04-21, orch-review-1776770835).
