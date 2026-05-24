---
name: migrate-bash-multi-violation-expansion-spec
description: Rewrite local YAML helpers to call `helpers-spec-make-scope-config`; replace writer-round-trip reads from scope.yml with drawer reads via `org-entry-get-multivalued-property`.
change: migrate-integration-tests-to-drawer-helpers
status: ready
relations: []
---

## Files to modify

- `config/gptel/scope/test/integration/bash-multi-violation-expansion-spec.el` (modify) — rewrite `multi--parse-scope-yml` (line 111) + inline scope construction (line 168); switch writer-round-trip reads from disk YAML to in-buffer drawer

## Why

This file tests the writer (`jf/gptel-scope--add-path-to-scope`) end-to-end: the test mutates state via the writer, then reads back to verify the new pattern landed in the right bucket. The original test "wrote scope.yml on disk, then re-parsed it." Cycle-3 retired both the on-disk format and the YAML parser; the writer now produces drawer entries via `org-entry-put-multivalued-property` (`scope-expansion.el:136`). The test's invariant ("writer round-trips to a valid scope config") is still valid — the read step just needs the new vocabulary.

## Implementation steps

1. **Rewrite** `multi--parse-scope-yml` (line 110–112). Currently it parses `multi--scope-file` (disk YAML); now it should read the drawer of the chat buffer the writer wrote to:
   ```elisp
   (defun multi--read-scope-drawer (buffer)
     "Read the scope drawer from BUFFER and return an alist of (drawer-key . values)."
     (with-current-buffer buffer
       (org-with-wide-buffer
        (goto-char (point-min))
        (mapcar (lambda (key)
                  (cons key (org-entry-get-multivalued-property (point-min) key)))
                '("GPTEL_SCOPE_READ" "GPTEL_SCOPE_READ_METADATA"
                  "GPTEL_SCOPE_WRITE" "GPTEL_SCOPE_MODIFY"
                  "GPTEL_SCOPE_EXECUTE" "GPTEL_SCOPE_DENY")))))
   ```
2. **Rewrite** the helper `multi--scope-paths` (line 114) to take the alist from the new reader instead of the YAML plist. Returns the list of patterns for a given drawer key.
3. **Replace `multi--scope-file` setup** with a chat buffer fixture. The pattern (using `jf/gptel-test--with-scope-drawer`):
   ```elisp
   (jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . "/baseline/**"))
     (call-the-writer …)
     (let ((alist (multi--read-scope-drawer (current-buffer))))
       (expect (alist-get "GPTEL_SCOPE_WRITE" alist nil nil #'equal)
               :to-equal '("/baseline/**" "/new/**"))))
   ```
4. **Rewrite** the inline construction at line 168 (the second YAML use). Same shape: build the config via `helpers-spec-make-scope-config` directly.
5. **Drop** `multi--write-empty-scope` (line 89), `multi--scope-file` setup, and any other helpers that exist only to thread an on-disk YAML file. The writer doesn't need a file path; it writes to the current chat buffer's drawer.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/integration 2>&1 | grep -iE "multi-violation|bash multi" | head -20
grep -n "jf/gptel-scope-yaml\|multi--scope-file\|multi--write-empty-scope" config/gptel/scope/test/integration/bash-multi-violation-expansion-spec.el
```

Expect: no YAML matches; the multi-violation describe blocks pass; no remaining references to disk-file plumbing.

## Context

- Production writer: `config/gptel/scope/scope-expansion.el:136` (`jf/gptel-scope--write-pattern-to-drawer`)
- Drawer fixture macro: `config/gptel/scope/test/helpers-spec.el:588` (`jf/gptel-test--with-scope-drawer`)
- Plan: `/Users/jefffarr/.claude/plans/piped-hugging-flamingo.md` Workstream A row 4
