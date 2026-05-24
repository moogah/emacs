---
name: migrate-bash-add-to-scope-bug-spec
description: Switch the YAML reload step to a drawer read via `org-entry-get-multivalued-property`. Unblocks "Bug 4" — the residual real assertion that the YAML error currently masks.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/bash-add-to-scope-bug-spec.el` (modify) — rewrite `bug--parse-scope-yml` (line 139), `bug--reload-scope-yml-config` infrastructure (~lines 105–148), and the test body's reload assertion shape

## Why

This file documents and tests "Bug 4": after a bash tool denial, the `add-to-scope` action chain should route the denied path into the correct drawer bucket (`paths.read`, not `paths.write`) and the gptel callback should receive a success result on retry. The test fails today with a `void-function` YAML error, masking whatever residual real failure (if any) sits behind it. After migrating to drawer-based reads, the residual assertion will be visible.

The failing assertion (per the agent diagnosis) is at line 371: `(expect (plist-get bug--callback-result :success) :to-be t)`. The test's documented bug chain (in its own comments) is genuine — three chained bugs in `trigger-inline-expansion` + `add-bash-to-scope` + path bucketing. Those fixes are out of scope here; they belong in the drafted `scope-rearch-followups` change. This task's job is just to make the assertion visible by removing the YAML obstacle.

## Implementation steps

1. **Rewrite** `bug--parse-scope-yml` (line 139–142). The current body parses an on-disk scope.yml. New body reads the chat buffer's drawer:
   ```elisp
   (defun bug--read-scope-drawer (buffer)
     "Read scope drawer values from BUFFER as an alist keyed by drawer key."
     (with-current-buffer buffer
       (mapcar (lambda (key)
                 (cons key (org-entry-get-multivalued-property (point-min) key)))
               '("GPTEL_SCOPE_READ" "GPTEL_SCOPE_READ_METADATA"
                 "GPTEL_SCOPE_WRITE" "GPTEL_SCOPE_MODIFY"
                 "GPTEL_SCOPE_EXECUTE" "GPTEL_SCOPE_DENY"))))
   ```
2. **Drop** `bug--make-session-scope-yml` (line 105) and `bug--read-scope-yml` (line 133). These exist only to thread on-disk YAML.
3. **Replace** scope-file setup with a drawer fixture. The test creates a chat buffer with a known baseline drawer (typically empty or single-path), invokes the writer via the validation pipeline, and reads the drawer back. Use `jf/gptel-test--with-scope-drawer` for setup; `current-buffer` becomes the buffer the writer mutates.
4. **Adjust** the test's `helpers-spec-load-scope-config` call (line 183) — replace with a direct call to `helpers-spec-make-scope-config` for baseline-config setup, or read the drawer directly if the assertion is about the writer's output.
5. **Run the test in isolation** after migration. Expected: `Bug 4 …gptel callback receives success after add-to-scope correctly expands scope` either passes (the YAML error was the only obstacle) or fails on a real assertion documenting the three-bug chain — file that residual failure as `scope-rearch-followups` Bug 4 fold-in, per the WS-B plan.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "bug 4|bash-add-to-scope" | head -20
grep -n "jf/gptel-scope-yaml\|helpers-spec-load-scope-config\|bug--make-session-scope-yml" config/gptel/scope/test/integration/bash-add-to-scope-bug-spec.el
```

Expect: no YAML-helper matches; the test runs to its assertion (pass or honest fail) instead of erroring at fixture construction.

## Context

- Per agent diagnosis: this file's "Bug 4" is a REAL BUG (three chained bugs in the scope-expansion path) — unblocked by this migration, fixed by the `scope-rearch-followups` change downstream
- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 5, with Workstream B coordination
