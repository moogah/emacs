---
name: implement-frame-state-serializer
description: Implement jf/frame-state display-free JSON observation with bounded output + unit specs
change: drive-isolated-emacs-via-emacsclient
status: ready
relations:
  - "blocked-by:scaffold-target-support-module"
---

## Files to modify
- config/emacs-test-targets/target-support.org (modify) — add `jf/frame-state`
- config/emacs-test-targets/test/unit/frame-state-spec.el (new) — Buttercup unit specs

## Implementation steps
1. Write the unit specs first (`frame-state-spec.el`, Buttercup `describe`/`it`):
   - Given a synthetic current buffer with known contents, `jf/frame-state` returns a
     parseable structure including buffer name, contents, point, window configuration,
     minibuffer state, and recent echo-area messages.
   - A buffer larger than the size limit yields truncated contents WITH a truncation
     indicator.
   - The result round-trips through `json-serialize`/`json-parse-string`.
2. Implement `jf/frame-state` in `target-support.org`:
   - Collect: `(buffer-name)`, bounded `(buffer-string)` (cap via a defcustom/const,
     e.g. `jf/frame-state-max-chars`; when exceeded, truncate and set a
     `:truncated t` field), `(point)`, window configuration (map
     `(window-list)` → displayed buffer names), minibuffer active/contents, and the
     last N lines of `*Messages*`/echo area.
   - Return JSON via `json-serialize` of a plist/hash. Keep keys stable and documented.
3. Tangle + validate: `./bin/tangle-org.sh config/emacs-test-targets/target-support.org`.
4. Run the unit specs: `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup`.

## Design rationale
`jf/frame-state` is the display-free observation contract (specs/frame-state-observation):
a client with no display parses JSON and asserts on target state. Bounded output keeps
an arbitrarily large buffer from producing an unbounded response (spec: Bounded
Observation Output). Borrowed from `llm-test`'s frame-state-as-JSON idea (design.md
Context).

## Verification
- `./bin/run-tests.sh -d config/emacs-test-targets -f buttercup` passes the
  frame-state specs.
- Manual: `emacsclient -s <name> --eval '(jf/frame-state)'` against a daemon returns
  valid JSON (covered end-to-end by the behavioral smoke task).
- Each spec `it` cites `specs/frame-state-observation/spec.md § <Requirement>`.

## Context
- specs/frame-state-observation/spec.md
- design.md § Decisions 'D5' and 'D7. Testing approach'
