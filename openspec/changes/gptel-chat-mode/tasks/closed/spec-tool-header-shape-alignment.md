---
name: spec-tool-header-shape-alignment
description: Update spec.md tool-header example to use the implementation's :args-wrapped shape (canonical via persistent-agent and parser round-trip)
change: gptel-chat-mode
status: done
relations:
  - discovered-from:stream-callback
---

## Files to modify
- `openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  (example at or near line 35)

## Implementation steps
1. Find the `#+begin_tool` example in the spec. Current form (wrong):
   ```
   #+begin_tool (run_bash_command :command "uname")
   ```
2. Replace with the implementation's canonical form:
   ```
   #+begin_tool (run_bash_command :args (:command "uname"))
   ```
3. Verify the parser's `(cdr parsed)` → plist destructuring round-trips
   this form (it does today) and cite the corroborating reference in
   the updated spec prose if helpful (`persistent-agent.org` uses the
   same shape).

## Design rationale
The implementation writes `:args`-wrapped tool headers and the parser
round-trips them. The spec's inline-kwargs example is out of sync. A
user or test author following the spec will disagree with the parser.
Picking the implementation's shape as canonical matches
`persistent-agent.org` precedent and avoids re-plumbing the parser.

Non-blocking — documentation-only correctness.

## Verification
- `grep -n "begin_tool" openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md`
  shows only the `:args`-wrapped form.
- Parser tests continue to pass (no parser change).

## Context
- Review of `stream-callback` (2026-04-21, orch-review-1776774164),
  Finding #4 (non-blocking spec-signal).
- Implementation: `config/gptel/chat/stream.org`
  (`gptel-chat--stream-open-tool-block`).
- Precedent: `config/gptel/tools/persistent-agent.org` tool-header
  formatting.

## Resolution (2026-04-21, superseded)

This task is obsolete. A later task, `reconcile-tool-args-shape`,
re-opened the same question and reached the **opposite** conclusion:
the canonical header contract is Path B `(<name> <plist...>)` — a
single sexp whose `car` is the tool-name symbol and whose `cdr` is
the arguments plist — NOT Path A `(<name> :args <sexp>)` as this
task prescribed. The `(cdr parsed)` destructuring in the parser as
merged is Path B; the writer currently emits `(:args <sexp>)` as
its concrete plist choice, but the parser does not privilege any
particular keyword.

Implementing this task as written would have reintroduced the very
drift that `reconcile-tool-args-shape` resolved. The spec example
at `spec.md:35` already uses valid Path B form
(`(run_bash_command :command "uname")` — a one-entry plist with
`:command` as the key), so no spec edit is needed.

Closed without action as part of `reconcile-tool-args-shape` review.
Cross-reference:
`openspec/changes/gptel-chat-mode/tasks/closed/reconcile-tool-args-shape.md`
§Review.
