---
name: run-bash-multibyte-json-bugs
description: Three pre-existing bugs in run_bash_command's result handling, all triggered by multibyte/non-ASCII output bytes. Surfaces as a PersistentAgent FSM hang whenever a tool returns content containing characters like em-dash. Discovered while landing expansion-queue-frame-global.
status: ready
source: openspec/changes/expansion-queue-frame-global
relations:
  - discovered-from:integration-verify-with-trace
---

> Surfaced during live verification of `expansion-queue-frame-global` on 2026-04-30. With the buffer-routing fix in place, the PersistentAgent flow now correctly routes drawer writes to the agent's session and re-validates against the agent's drawer. But it still hangs whenever a tool returns multibyte output. Three independent bugs, all rooted in unibyte handling of bash command stdout. Externalised to `.tasks/` because they're orthogonal to scope-expansion-queue routing — a separate change ("bash-tool-multibyte-handling" or similar) should own them.

## Repro

PersistentAgent under `system-explorer` preset, prompt: "list files in /tmp and read text files". When the agent issues a `cat` of a file whose content contains non-ASCII bytes (e.g. an em-dash `—` = 0xE2 0x80 0x94), the FSM hangs in TOOL state — no `#+end_tool` is written, no terminal handler fires, the agent never returns. The trace module (`config/gptel/tools/persistent-agent-trace.org`) names the failure points; see captured log `~/.gptel-pa-trace.log` from 14:05:50 on 2026-04-30 for the canonical run.

## The three bugs

### Bug 1 — `json-serialize` rejects unibyte bash stdout

`jf/gptel-bash--execute-command` (in `config/gptel/scope/scope-shell-tools.org`, `bash-execute-command` section) returns the raw bytes captured from the bash subprocess as a unibyte string. The wrapper macro then calls `(json-serialize <plist>)` on a plist whose `:output` is that unibyte string. `json-serialize` requires multibyte strings; on unibyte strings containing high-bit bytes it errors with `(wrong-type-argument json-value-p "<bytes>")`.

Trace signature:

```
auth-allow-ERR err=(wrong-type-argument json-value-p "{\"success\":true,\"output\":\"... \342\200\224 ...")
```

The `\342\200\224` is `prin1`'s octal-escape rendering of UTF-8 em-dash bytes in a unibyte string.

**Fix**: at the boundary where bash output enters the elisp world, decode it via `(decode-coding-string output 'utf-8)`. The boundary is wherever `jf/gptel-bash--execute-command` reads the subprocess output. This is a one-line change at the source of the unibyte string.

### Bug 2 — `select-safe-coding-system` prompt blocks `save-buffer`

`jf/gptel-scope--write-pattern-to-drawer` (in `scope-expansion.org`) calls `(save-buffer)` on the chat buffer after writing the new pattern. If the chat buffer contains text from a prior tool result with the same unibyte-bytes problem, `save-buffer` triggers `select-safe-coding-system`, which pops a window asking the user how to encode the file. The save blocks until the user answers; the user typically can't tell where this prompt came from and dismissing it doesn't unblock the FSM hang from Bug 1.

This is a corollary of Bug 1: once the bash output is properly decoded, the buffer is multibyte-clean, `save-buffer` writes UTF-8 without prompting, and this disappears.

**Fix**: same decode at the boundary as Bug 1. No separate fix required if Bug 1 lands first.

### Bug 3 — Deny-formatter emits `:allowed-patterns` as a plain list

`jf/gptel-scope--format-tool-error` (in `scope-validation.org`) returns a deny-response plist whose `:allowed-patterns` and `:deny-patterns` values are plain lists of strings. When the wrapper's on-deny path serializes that with `(json-serialize deny-response)`, `json-serialize` interprets a non-keyword list as an alist, takes the first element (a string like `"/tmp"`), tries to use it as an alist key, calls `symbol-name` on it, and errors with `(wrong-type-argument symbolp "/tmp")`.

This bug is **independent** of Bug 1. It surfaces here because Bug 1's error cascades through the expansion-callback's `condition-case`, which falls through to the deny path with a deny-response constructed from `--format-tool-error`. Even without Bug 1, any path that delivers a `format-tool-error` response through `json-serialize` would hit this — Bug 1 is just the most reliable trigger.

Trace signature:

```
auth-deny-ERR err=(wrong-type-argument symbolp "/tmp")
```

**Fix**: in `jf/gptel-scope--format-tool-error`, wrap the list-valued plist entries in `vconcat` so they become vectors — `json-serialize` treats vectors unambiguously as JSON arrays. The pattern is already used elsewhere in the codebase (`(vector resource)`, `(vconcat patterns)` in `--add-to-scope--emit-callback`).

## Why these don't belong in `expansion-queue-frame-global`

That change is scoped to "fix `current-buffer` drift in the expansion-UI flow" — Bug A (queue+flag global) and Bug B (origin-buffer preservation across re-entry). The three bugs above are about bash output encoding and JSON serialization shapes; they have no overlap with buffer routing, no overlap with the queue, no overlap with the transient menu, and would not be expected to surface for any reader of the expansion-queue spec. Lumping them together would muddy both stories.

## Files to modify

- `config/gptel/scope/scope-shell-tools.org` — section "bash-execute-command" (or wherever `jf/gptel-bash--execute-command` reads subprocess output). One-line decode at the boundary.
- `config/gptel/scope/scope-validation.org` — section "Error Formatting", `jf/gptel-scope--format-tool-error`. Wrap list values in `vconcat`.
- `config/gptel/scope/test/` — add specs:
  - One under `bash/` (or wherever `bash-execute-command` is tested) that exercises a command whose output contains a UTF-8 multibyte char and asserts the returned `:output` is a multibyte string.
  - One under `validation/error-messages-spec.el` (or sibling) asserting `--format-tool-error`'s deny-response round-trips through `json-serialize` cleanly when patterns contain plain strings.
- `openspec/changes/<this-change>/specs/scope/spec.md` — add scenarios contracting (a) bash output is decoded as UTF-8 at the boundary, (b) deny-response shapes JSON-serialize cleanly.

## Implementation order

Test-first.

1. Write the failing spec for Bug 1 (multibyte string in `:output`).
2. Add the decode-coding-string at the bash-execute boundary; spec turns green.
3. Write the failing spec for Bug 3 (deny-response round-trips through json-serialize with string-list patterns).
4. Wrap list values in `vconcat` in `--format-tool-error`; spec turns green.
5. Run the full PA scenario with the trace module to confirm the agent now completes when reading multibyte content.

Bug 2 has no dedicated spec — it's a corollary of Bug 1. The integration verification at step 5 covers it.

## Verification

```bash
./bin/tangle-org.sh config/gptel/scope/scope-shell-tools.org
./bin/tangle-org.sh config/gptel/scope/scope-validation.org
./bin/run-tests.sh -d config/gptel/scope
```

End-to-end: re-run the agent-test scenario with `M-x jf/gptel-pa-trace-start` active; the trace must show the `cat` of a multibyte-content file producing a `(tool-result . _)` event and the FSM transitioning to `DONE`, not stalling on `auth-allow-ERR`.

## Context

- Trace module: `config/gptel/tools/persistent-agent-trace.org`
- Captured log evidencing the chain: `~/.gptel-pa-trace.log` (last line: `forensic-snapshot ... fsm-state=nil`)
- Wrapper macro that calls `json-serialize` on the body result: `config/gptel/scope/scope-tool-wrapper.org` § "Scoped Tool Macro"
- Where the deny-response is built: `config/gptel/scope/scope-validation.org` § "Error Formatting"
- Where the chat-buffer save fires: `config/gptel/scope/scope-expansion.org` § `jf/gptel-scope--write-pattern-to-drawer`
