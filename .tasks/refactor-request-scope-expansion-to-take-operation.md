---
name: refactor-request-scope-expansion-to-take-operation
description: `request_scope_expansion` takes a `tool_name` string and reconstructs `:validation-type` from it; scope is fundamentally `(operation, paths)`, so the tool should take `operation` directly and derive `:validation-type` the same way the validation pipeline does. Replaces the side-table-keyed-by-tool-name workaround that landed and was reverted in WS-B.
source: scope-rearch-followups
status: ready
relations:
  discovered-from: scope-rearch-followups-bug-1
---

## The shape problem

`request_scope_expansion` (in `config/gptel/scope/scope-shell-tools.el`) is a plain `gptel-make-tool` (not a scoped tool) that lets the LLM pre-emptively request scope expansion before invoking the actual access-needing tool. Its current LLM-facing signature is:

```
request_scope_expansion(tool_name, patterns, justification)
```

Internally it builds a `violation-info` plist that must carry `:validation-type` (`'path` or `'bash`) because downstream consumers — `jf/gptel-scope-prompt-expansion`, the expansion UI, `--add-path-to-scope` / `--add-bash-to-scope` writers — branch on it.

**Today's state (after the WS-B revert at d00949f rollback):** the constructor hardcodes `:validation-type 'path`. This is wrong for bash-backed tools (`run_bash_command`) — any LLM call to `request_scope_expansion` for a bash violation is silently misrouted as a path-shaped violation. No test pins this today, which is why it slipped through.

**Why WS-B's first attempt was the wrong fix:** WS-B introduced a side-table keyed by tool name (`jf/gptel-scope--scoped-tool-operations`) populated by the `gptel-make-scoped-tool` macro, plus a lookup `jf/gptel-scope--tool-validation-type` that decoded validation-type from the recorded `:operation` (non-nil → path, nil → bash). The review surfaced four issues — unknown-tool fallback contradicted the change's own design, no regression spec existed, the docstring lied about lifecycle, and the side-table-keyed-by-name shape is fragile — but the deeper problem is that **the lookup only exists because `tool_name` is the wrong primary argument**.

Scope is managed by `(operation, paths)`. Every other consumer of `:validation-type` derives it from operation directly:

```elisp
;; config/gptel/scope/scope-validation.el:779-785
(if (null operation)
    (append (jf/gptel-scope--validate-bash-tool tool-name args config)
            (list :validation-type 'bash))
  (let ((metadata ...))
    (append (jf/gptel-scope--validate-filesystem-tool ...)
            (list :validation-type 'path))))
```

`request_scope_expansion` is the only consumer that has to recover validation-type from a name string, and that's because we gave it the wrong argument.

## Proposed shape

```
request_scope_expansion(operation, patterns, justification)
```

Where `operation` is one of the closed-set values `gptel-make-scoped-tool` accepts:

- A filesystem-operation symbol (`read`, `write`, `modify`, `execute`) → `:validation-type 'path`
- The literal string/keyword `bash` (or `null`) → `:validation-type 'bash`

Implementation becomes trivial — same derivation as `scope-validation.el:779-785`, no lookup, no side-table:

```elisp
(lambda (callback operation patterns justification)
  (when (vectorp patterns)
    (setq patterns (append patterns nil)))
  (let* ((vtype (if (or (null operation) (equal operation "bash"))
                    'bash 'path))
         (violation-info
          (list :tool nil                  ; or drop entirely — see below
                :resource (car patterns)
                :reason justification
                :validation-type vtype
                :patterns patterns)))
    (jf/gptel-scope-prompt-expansion violation-info callback patterns nil)))
```

The LLM tool description and arg enum should communicate the closed set explicitly.

## Open questions for this rework

1. **Drop `:tool` from `violation-info`?** The downstream prompt-expansion UI and writer surfaces currently use `:tool` for display strings. Check `jf/gptel-scope-prompt-expansion` and the section-targeting writers to see whether `:tool` is load-bearing or cosmetic. If cosmetic, drop; if load-bearing, document the new signature and provide a sensible default ("user pre-emptive request" or similar).

2. **Does `gptel-make-scoped-tool` need to change?** No — scoped tools route through `jf/gptel-scope-authorize-tool-call`, not through `request_scope_expansion`. The refactor is local to the meta-tool.

3. **`scope-rearch-followups` Bug 1 disposition.** That drafted change's `proposal.md` and `design.md` (D1) currently advocate the side-table approach. Bug 1's design notes should be reshaped to describe this operation-based refactor instead — or this `.tasks/` entry should be promoted into `scope-rearch-followups/tasks/open/` and the old design text struck through with the new disposition recorded.

## Files involved

- `config/gptel/scope/scope-shell-tools.org` (and `.el`) — the `request_scope_expansion` `gptel-make-tool` block (~line 224-271 of the `.org`).
- `openspec/changes/scope-rearch-followups/proposal.md` and `design.md` — Bug 1's text needs updating.
- New regression spec: `config/gptel/scope/test/tool-wrapper/request-scope-expansion-operation-spec.el` with at minimum:
  - `it "derives :validation-type 'path from a filesystem operation"`
  - `it "derives :validation-type 'bash from null/bash operation"`
  - `it "rejects an out-of-enum operation value with a structured error"`

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/tool-wrapper
```

After the refactor, the new operation-spec passes; no other scope tests regress.

## Why this is `.tasks/` rather than in-scope

The WS-B fix was attempted as part of the un-task-generated `scope-rearch-followups` change (Bug 1). Reverting it leaves that drafted change without a working Bug 1 solution. Rather than block the rest of `scope-rearch-followups` (Bugs 2 and 3) on this rework — and because the rework is a real LLM-API change that wants its own design conversation — this lives in `.tasks/` until the next maintainer of `scope-rearch-followups` (or a fresh change) picks it up.

## Context

- Reverted commit: WS-B side-table approach was at `d00949f` and was reverted in this session's cleanup (uncommitted at time of writing).
- Review that surfaced the shape problem: `.orchestrator/cycles/post-hoc-2026-05-24/reviews/ws-b-scope-shell-tools-validation-type.md` Finding 4 (spec-signal).
- Sibling drafted change: `openspec/changes/scope-rearch-followups/`.
- Sibling already-externalized `.tasks/` item: `fix-bash-violation-resource-routes-as-command.md` (Bug 4 from the same source change).
