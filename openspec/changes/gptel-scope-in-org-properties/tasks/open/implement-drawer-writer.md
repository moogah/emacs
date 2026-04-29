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

## Observations

- Two functions added to `config/gptel/scope/scope-expansion.org` in a new
  `* Drawer Writer` section (between `Helper Functions` and
  `Transient Menu`):
    - `jf/gptel-scope--map-operation-to-drawer-key` — closed-set
      operation → drawer-key (bare-form) collapse, with `:deny` arm and
      `nil → GPTEL_SCOPE_READ` safest fallback (matches the register's
      canonical mapping function and the closed set including `:deny`).
    - `jf/gptel-scope--write-pattern-to-drawer` — three-stage
      (collapse → dedup → emit + save-buffer) writer. Returns nil on
      no-op dedup; returns the pattern string on a successful write.
- Tangle and validate succeeded; new functions are present in
  `config/gptel/scope/scope-expansion.el` (search `map-operation-to-drawer-key`
  and `write-pattern-to-drawer`).
- Scope test suite baseline (`./bin/run-tests.sh -d config/gptel/scope`):
  549 tests, 547 passing; the two failures
  (`run_bash_command: Timeout and resource limits …`) match the documented
  pre-existing baseline and are unrelated to this change.
- New functions are unused at this commit; `--add-to-scope` and friends
  still go through the YAML mutator pipeline. Rewiring is the
  `rewire-expansion-writer` task.
- Confirmed the system Org's signature for
  `org-entry-put-multivalued-property` (in
  `/Applications/Emacs.app/Contents/Resources/lisp/org/org.el.gz` line
  13158): takes `(EPOM PROPERTY &rest VALUES)` and stores all values
  as a single space-separated string under the bare KEY. The
  `(apply #'org-entry-put-multivalued-property point key updated)`
  call in this task's spec matches that signature.

## Discoveries

- class: interface-drift
  what: |
    Test fixture `jf/gptel-test--with-scope-drawer` renders multi-value
    drawer entries with `KEY+:` continuation lines (e.g.
    `:GPTEL_SCOPE_READ: /a` followed by `:GPTEL_SCOPE_READ+: /b`), but
    `org-entry-put-multivalued-property` writes a single bare line with
    space-separated values (e.g. `:GPTEL_SCOPE_READ: /a /b /c`).
    `org-entry-put` matches only the bare key line; if our writer is
    called against a fixture-shaped drawer with `KEY+:` continuations,
    the bare line is replaced (with the full new joined list) but the
    `KEY+:` lines are left intact, producing duplicated values on the
    next read.
  evidence: |
    System Org `/Applications/Emacs.app/.../org.el.gz`:
      - `org-entry-put-multivalued-property` (line 13158) calls
        `(org-entry-put epom property (mapconcat ... " "))` — single
        space-joined string under bare KEY.
      - `org-entry-put` (line 13236) uses `(org-re-property property nil t)`
        which `regexp-quote`s the property name; no `+` allowed — only
        the bare line matches.
      - `org--property-local-values` (line 13028) reads `+` accumulator
        via `(org-element-property (intern (concat ":" PROPERTY "+")))`,
        so a stale `KEY+:` line does contribute to subsequent
        `org-entry-get-multivalued-property` reads.
    Test helper `config/gptel/scope/test/helpers-spec.el` line 631-642
    emits `KEY` for the first value and `KEY+` for subsequent values.
  impact: |
    The manual smoke test in this task body uses a single-value initial
    drawer (`(:GPTEL_SCOPE_READ . ("/a/**"))`) so no `KEY+:` line is
    produced; the smoke test will pass. The corruption only manifests
    when the writer is called against an existing multi-value drawer
    (≥2 entries already under the key). Future spec
    `add-drawer-corruption-regression` should cover this case
    explicitly. Likely resolution: either the `apply-to-drawer`
    applicator must use the same single-bare-line encoding (then real
    sessions never see `KEY+:` continuations), or the writer must
    delete all `KEY` and `KEY+` lines before writing the joined list.
    Resolving this is out of scope for this task; the writer is
    landed unused.
- class: scope-question
  what: |
    The writer calls `save-buffer` unconditionally on the modify path.
    A buffer with no associated file (e.g. `with-temp-buffer` in a
    test fixture) will signal an error from `save-buffer`. The fixture
    helper `jf/gptel-test--with-scope-drawer` uses `with-temp-buffer`
    and does not associate a file. Buttercup specs for the writer will
    therefore need either to (a) wrap the call in `cl-letf` mocking
    `save-buffer` to a no-op, (b) write the fixture content to a
    real tempfile, or (c) `set-visited-file-name` on the temp buffer.
    The task spec is silent on this; the testing-side decision is
    deferred to whichever task adds the writer's specs (likely
    `add-drawer-corruption-regression` and `add-write-pattern-spec`).
  evidence: |
    The writer's body unconditionally calls `(save-buffer)` on the
    modified-buffer branch (per the task body's reference impl).
    `save-buffer` requires `buffer-file-name`; with-temp-buffer leaves
    `buffer-file-name` nil.
  impact: |
    Doesn't affect current commit (writer is unused). Flagged so the
    test-author for the writer's invariant specs picks the right
    stubbing strategy and doesn't get surprised by a save-buffer
    error in `with-temp-buffer`.
- class: vocabulary-mismatch
  what: |
    The register's canonical `jf/gptel-scope--map-operation-to-drawer-key`
    snippet (interfaces.org lines 432–445) signals an `error` for
    unmapped operations, while this task body's reference implementation
    (and the implementation that landed) returns `"GPTEL_SCOPE_READ"`
    on the catch-all `(t ...)` arm. The task body explicitly chose the
    fallback: "Defaults to GPTEL_SCOPE_READ when OPERATION is nil
    (safest fallback)". Both are defensible; flagged so the architect
    can pick one as the authoritative source.
  evidence: |
    interfaces.org line 444-445:
      ((null operation)        "GPTEL_SCOPE_READ")
      (t (error "Unmapped operation: %S" operation))
    implement-drawer-writer.md task body line 38-43:
      (cond
       ((memq operation '(:read ...)) "GPTEL_SCOPE_READ")
       ...
       (t "GPTEL_SCOPE_READ"))
  impact: |
    Implementation matches the task body (which the implementor is
    licensed to follow). Difference: my landed impl is permissive
    (silent default to READ on any unknown op); the register's
    canonical version is strict (error on unknown). The task body's
    permissive form is safer for the inline-flow trigger (an unknown
    op doesn't crash the user's tool call) but loses the
    bash-parser-:read-metadata-into-:paths.write-style early signal.
    Architect should reconcile at integrate.
- class: spec-signal
  what: |
    The register's `operation-to-drawer-key` entry (interfaces.org
    line 432-445) does not list `:deny` as a member of the mapped
    vocabulary, but the task body and the landed implementation both
    include a `(:deny → GPTEL_SCOPE_DENY)` arm. The closed-set
    `drawer-key-set` register entry (line 317-320) DOES list
    `:GPTEL_SCOPE_DENY:` as a valid drawer key.
  evidence: |
    interfaces.org `register/vocabulary/operation-to-drawer-key`
    member list (lines 387-419) enumerates 11 ops:
      :read, :read-directory, :read-metadata, :match-pattern,
      :write, :create, :create-or-modify, :append, :delete,
      :modify, :execute
    No `:deny` entry. But `register/vocabulary/drawer-key-set`
    line 317 includes `:GPTEL_SCOPE_DENY:` as a member.
  impact: |
    The `:deny` operation is presumably surfaced when validation
    deny-rules fire; the writer needs to support it for completeness
    of the closed drawer-key set. Architect should add `:deny` to
    `register/vocabulary/operation-to-drawer-key` member list to
    match the implemented closed set, or remove the
    GPTEL_SCOPE_DENY arm from the writer. Implementation kept the
    arm because the broader closed-set vocabulary entry includes
    DENY and the task body's reference implementation also
    includes it.
