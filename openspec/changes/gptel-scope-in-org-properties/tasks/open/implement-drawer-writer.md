---
name: implement-drawer-writer
description: Implement write-pattern-to-drawer and map-operation-to-drawer-key in scope-expansion.org
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:add-drawer-encoding-contract
---

## Cites register entries

- `register/boundary/scope-pattern-writer` — the single canonical mutator pipeline (op→key collapse, dedup, org-entry-put + save-buffer). Your two functions implement stages 1–3.
- `register/vocabulary/operation-to-drawer-key` — the closed set of granular operations your `--map-operation-to-drawer-key` collapses. Adding a value here that isn't in the entry is a vocabulary-mismatch finding.
- `register/invariant/scope-add-pattern-idempotent` — dedup is part of the writer's contract, not an optimisation.
- `register/invariant/scope-drawer-no-duplication` — `org-entry-put` against the existing drawer must not produce a second one.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-pattern-writer.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/operation-to-drawer-key.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-add-pattern-idempotent.test.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/invariants/scope-drawer-no-duplication.test.el`

## Files to modify
- `config/gptel/scope/scope-expansion.org` (modify) — add a `* Drawer Writer` section with the writer and the operation → key mapper.
- Tangle: `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org`.

## Implementation steps

1. Add `jf/gptel-scope--map-operation-to-drawer-key`:

   ```elisp
   (defun jf/gptel-scope--map-operation-to-drawer-key (operation)
     "Map a denied OPERATION keyword to the matching drawer key string.
   Read-like granular operations collapse to GPTEL_SCOPE_READ. Write-like
   granular operations collapse to GPTEL_SCOPE_WRITE. Defaults to
   GPTEL_SCOPE_READ when OPERATION is nil (safest fallback)."
     (cond
      ((memq operation '(:read :read-directory :read-metadata :match-pattern)) "GPTEL_SCOPE_READ")
      ((memq operation '(:write :create :create-or-modify :append :delete))   "GPTEL_SCOPE_WRITE")
      ((eq operation :modify)  "GPTEL_SCOPE_MODIFY")
      ((eq operation :execute) "GPTEL_SCOPE_EXECUTE")
      ((eq operation :deny)    "GPTEL_SCOPE_DENY")
      (t "GPTEL_SCOPE_READ")))
   ```

2. Add `jf/gptel-scope--write-pattern-to-drawer`:

   ```elisp
   (defun jf/gptel-scope--write-pattern-to-drawer (buffer operation pattern)
     "Append PATTERN to BUFFER's drawer key for OPERATION; save the buffer.
   Idempotent: returns nil and does not modify the buffer when PATTERN is
   already present under the target key. Returns the pattern string when
   the buffer was modified.

   Uses the bare key (e.g. `:GPTEL_SCOPE_READ:') for the first value and
   `org-entry-put-multivalued-property' for subsequent values. The drawer
   at point-min must already exist; this is true for any session created
   by `jf/gptel-scope-profile--apply-to-drawer'."
     (with-current-buffer buffer
       (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (let* ((key (jf/gptel-scope--map-operation-to-drawer-key operation))
                  (existing (org-entry-get-multivalued-property (point) key)))
             (if (member pattern existing)
                 nil
               (let ((updated (append existing (list pattern))))
                 (apply #'org-entry-put-multivalued-property (point) key updated)
                 (save-buffer)
                 pattern)))))))
   ```

3. Note: `org-entry-put-multivalued-property` writes the bare key for the first value and `+:` for subsequent values automatically when used via the canonical entry-point. Verify this against your local Org version; if the helper expects a single-call form, the implementation can fall back to a manual write loop:

   ```elisp
   ;; Manual fallback if `org-entry-put-multivalued-property' is not available
   ;; or behaves differently. Uses `org-entry-put' for the first value and
   ;; appends `+:' lines for the rest. Confirm via the regression test in
   ;; add-drawer-corruption-regression.
   ```

4. Do NOT yet rewire the expansion handlers — that happens in `rewire-expansion-writer`. Land this task with the writer defined and unused.

5. Tangle and confirm `./bin/run-tests.sh -d config/gptel/scope` still passes (no regressions; new functions not yet referenced).

## Design rationale

Per Decision 4 in design.md, the writer must mutate the live buffer (so the change lands in the user's undo ring) and call `save-buffer` (so the on-disk file reflects the change). Eliminating a separate file-write code path means scope-expansion loses its YAML-emitter helpers and the round-trip "preserve structure" requirement.

Idempotency (Decision 4 follow-on): adding the same pattern twice is a no-op because the inline-flow trigger re-enters `authorize-tool-call` after each add-to-scope, and a multi-violation tool call can pass through the same pattern twice on retry. The `(member pattern existing)` short-circuit handles this.

The operation→key mapping mirrors today's `jf/gptel-scope--map-operation-to-scope-section` (see `scope-expansion.org` § Add Path to Scope) — it's the same collapsing logic, just returning a drawer-key string instead of a plist key.

## Design pattern

Use Org's standard property-modification API: `org-entry-get`, `org-entry-put`, `org-entry-get-multivalued-property`, `org-entry-put-multivalued-property`. Position with `goto-char (point-min)` so the file-level drawer (before any heading) is targeted, matching the reader's positioning.

## Verification

- `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/scope` passes (no behavioral change yet).
- Manual smoke: in ielm,
  ```elisp
  (jf/gptel-test--with-scope-drawer '((:GPTEL_SCOPE_READ . ("/a/**")))
    (jf/gptel-scope--write-pattern-to-drawer (current-buffer) :read "/b/**")
    (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ"))
  ```
  should return `("/a/**" "/b/**")`. A second call with `/b/**` should be a no-op (return nil from the writer; the multi-value list unchanged).

## Context

design.md § Decision 4 (Writer mutates buffer, then save-buffer)
architecture.md § Components, § Interfaces
specs/gptel/scope-expansion/spec.md § ADDED Requirements / "Drawer writer preserves structure"
specs/gptel/scope-expansion/spec.md § MODIFIED Requirements / "Add to scope action", "Section-targeted writes"
