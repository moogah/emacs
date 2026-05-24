---
name: refactor-request-scope-expansion-to-take-operation
description: "Refactor request_scope_expansion's LLM-facing primary arg from tool_name to operation (closed enum read/write/modify/execute/bash); derive :validation-type directly from operation, matching the validation pipeline; add three regression specs."
change: scope-rearch-followups
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/scope-shell-tools.org` (modify) — the `request_scope_expansion` `gptel-make-tool` block (~line 218-260 of the tangled `.el`; same heading in the `.org`). Change `:args` to declare `operation` instead of `tool_name`, change `:description` to document the closed-enum semantics, and rewrite the `:function` lambda to derive `:validation-type` from `operation`.
- `config/gptel/scope/test/tool-wrapper/request-scope-expansion-operation-spec.el` (new) — Buttercup regression spec with three `it` blocks per the scenario mapping in `architecture.md`.

## Why

`request_scope_expansion` is the only consumer of `:validation-type` that has to recover it from a name string. Every other consumer derives `:validation-type` from `operation` directly (see `scope-validation.el:779-785`). Today's hardcoded `:validation-type 'path` mis-routes bash scenarios; a side-table workaround was attempted in commit `d00949f` and reverted in `9ed38c6` after author-blind review surfaced four findings (`.orchestrator/cycles/post-hoc-2026-05-24/reviews/ws-b-scope-shell-tools-validation-type.md`).

The right fix is to align `request_scope_expansion`'s signature with how scope is fundamentally managed: `(operation, paths)`. The LLM already knows which operation it intends (it's about to invoke the corresponding tool), so supplying the operation explicitly is not new cognitive load.

## Implementation steps

1. **Read** `config/gptel/scope/scope-shell-tools.org`, locate the `request_scope_expansion Tool` heading.
2. **Write the failing regression spec FIRST** at `config/gptel/scope/test/tool-wrapper/request-scope-expansion-operation-spec.el`. Three `it` blocks under one `describe "request_scope_expansion operation derivation"`:
   - `it "derives :validation-type 'path from filesystem operations (read/write/modify/execute)"` — call the tool's function with each filesystem operation, assert the violation-info passed to a spy-replaced `jf/gptel-scope-prompt-expansion` carries `:validation-type 'path` and `:operation` as the matching symbol.
   - `it "derives :validation-type 'bash from operation \"bash\""` — call with `"bash"`, assert `:validation-type 'bash`.
   - `it "rejects out-of-enum operation values with :success nil"` — call with `"unknown_verb"` (or even an old tool_name like `"read_file_in_scope"`), assert the tool's function returns a JSON string with `:success false`, the error names the offending value, and `jf/gptel-scope-prompt-expansion` is NOT invoked.
   Look at existing tool-wrapper specs (`config/gptel/scope/test/tool-wrapper/*.el`) for fixture/spy patterns. Reuse `jf/gptel-scope-test--` helpers from `helpers-spec.el` if present.
3. **Run the new spec — confirm it fails** in the expected way (the current code hardcodes `:validation-type 'path` and accepts `tool_name`). Capture the failure output.
4. **Update** the `gptel-make-tool` form in `scope-shell-tools.org`:
   - Change `:description` to explain the closed-enum operation values (`read`, `write`, `modify`, `execute`, `bash`) and that the tool short-circuits with `:success false` on unknown operations.
   - Replace the `tool_name` `:args` entry with `operation` (type string, description naming the five enum values).
   - Rewrite the `:function` lambda per design D1 (proposal.md / design.md):
     ```elisp
     (lambda (callback operation patterns justification)
       (when (vectorp patterns)
         (setq patterns (append patterns nil)))
       (let ((op-sym (and (stringp operation) (intern operation))))
         (cond
          ;; Unknown / out-of-enum: short-circuit.
          ((not (memq op-sym '(read write modify execute bash)))
           (funcall callback
                    (json-serialize
                     `(:success :false
                       :error "unknown_operation"
                       :message ,(format "Unknown operation %S. Valid values: read, write, modify, execute, bash."
                                         operation)))))
          (t
           (let* ((vtype (if (eq op-sym 'bash) 'bash 'path))
                  (violation-info (list :resource (car patterns)
                                        :reason justification
                                        :validation-type vtype
                                        :operation op-sym
                                        :patterns patterns)))
             (jf/gptel-scope-prompt-expansion violation-info callback patterns nil))))))
     ```
   - Drop `:tool tool_name` from the violation-info plist. Verify nothing downstream is load-bearing on `:tool` (check `jf/gptel-scope-prompt-expansion` and the expansion-UI handlers). If anything is, set `:tool nil` instead of dropping the key.
5. **Re-tangle** with `./bin/tangle-org.sh config/gptel/scope/scope-shell-tools.org`.
6. **Run the new spec — confirm it passes**.
7. **Run the broader scope suite** with `./bin/run-tests.sh -d config/gptel/scope --report` to confirm no regressions. Expected: counts match baseline or improve (Bug 1's refactor may incidentally fix the bash-violation resource-routing tracked separately in `.tasks/fix-bash-violation-resource-routes-as-command.md` — note in `## Observations` if so).

## LLM-API surface

This is a breaking change to `request_scope_expansion`'s argument schema. The tool's `:description` and `:args` (visible to the model via `gptel-make-tool`) ARE the entire LLM-facing surface — there are no external system-prompt files documenting the tool (`grep -r request_scope_expansion config/gptel/agents/ runtime/templates/` returns nothing). So updating the `:description` and `:args` in the tool definition completes the LLM-API migration in the same commit.

As a safety net, an unknown operation value (including a stale `tool_name` from a pre-migration prompt) produces `:success false :error "unknown_operation"` rather than a silent mis-route.

## Verification

```bash
./bin/tangle-org.sh config/gptel/scope/scope-shell-tools.org
./bin/run-tests.sh -d config/gptel/scope/test/tool-wrapper
./bin/run-tests.sh -d config/gptel/scope --report
```

Expected: the new spec passes; counts on the scope suite are at least as good as the baseline captured before this task started.

## Out-of-scope

- Changes to validator semantics, pipeline stages, error codes, or the `gptel-make-scoped-tool` macro.
- The bash-violation resource-routing bug (separate `.tasks/` entry). If Bug 1's refactor incidentally fixes it, note in `## Observations`; do NOT touch the .tasks/ entry from this task.
- The Bucket-C string changes (timeout / truncation filter hints) from `d00949f` — already landed in main, untouched by this task.

## Context

- Design D1 in `openspec/changes/scope-rearch-followups/design.md:27-57`.
- Reverted commit: `d00949f` (workaround attempted); restoring commit: `9ed38c6` (revert).
- WS-B review: `.orchestrator/cycles/post-hoc-2026-05-24/reviews/ws-b-scope-shell-tools-validation-type.md`.
- Spec scenarios: `openspec/changes/scope-rearch-followups/specs/gptel/scope-expansion.md` — Requirement "request_scope_expansion tool" (four scenarios).
- Detailed sketch: `.tasks/refactor-request-scope-expansion-to-take-operation.md` (this task supersedes that entry — promote-and-replace at integrate time).

## Observations

_(implementor fills during execution — see template guidance in `roles/implementor.md`)_

## Discoveries

_(implementor fills during execution if anything warrants integrate-phase attention)_
