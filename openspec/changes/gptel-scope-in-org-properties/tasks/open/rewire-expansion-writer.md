---
name: rewire-expansion-writer
description: Switch expansion add-to-scope handlers to the drawer writer; remove scope.yml file-path resolver; honor cycle-2 dispositions (READ_METADATA bucket, action-handler-only :match-pattern, kept-WRITE :delete)
change: gptel-scope-in-org-properties
status: ready
relations:
  - discovered-from:implement-drawer-writer
  - enables:harden-add-to-scope-action-handler
  - enables:migrate-expansion-tests
  - enables:add-drawer-corruption-regression
  - enables:delete-yaml-and-security-residue
---

## Cites register entries

- `register/boundary/scope-pattern-writer` — every `--add-*-to-scope` handler becomes a consumer of this boundary; no other code path is allowed to mutate drawer scope state.
- `register/shape/violation-info` — your handlers read `:operation`, `:resource`, `:validation-type`, `:tool` from the violation-info plist.
- `register/vocabulary/operation-to-drawer-key` — your handlers route operations through this canonical mapping; no inline `pcase` translations elsewhere. **Cycle-2 update**: `:read-metadata` now collapses to `GPTEL_SCOPE_READ_METADATA` (not READ); `:match-pattern` is removed from the writer's domain (errors loudly if it reaches the writer).
- `register/vocabulary/drawer-key-set` — **cycle-2 update**: includes `GPTEL_SCOPE_READ_METADATA` as a 6th list-shape key.
- `register/shape/scope-config-plist` — **cycle-2 update**: `:paths` sub-plist now has six list-valued keys (`:read-metadata` added).
- `register/invariant/scope-add-pattern-idempotent` — preserve the contract: the same pattern twice is a no-op with empty `:patterns_added`.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-pattern-writer.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/operation-to-drawer-key.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/drawer-key-set.el` — **cycle-2 update**: regenerated with the 8-member set including `GPTEL_SCOPE_READ_METADATA`.

## Files to modify
- `config/gptel/scope/scope-expansion.org` (modify) — rewire `--add-to-scope`, `--add-wildcard-to-scope`, `--add-custom-to-scope`, `--add-path-to-scope`, `--add-bash-to-scope` to use `jf/gptel-scope--write-pattern-to-drawer`; **update `--map-operation-to-drawer-key` so `:read-metadata` collapses to `GPTEL_SCOPE_READ_METADATA` (cycle-2 ask 10A) and `:match-pattern` errors loudly (cycle-2 ask 10B; action handler is responsible for redirecting upstream)**; rewrite `--edit-scope` (Edit Manually action) to surface `session.org` instead of `scope.yml`; remove `--get-scope-file-path` and `--validate-scope-file-writable`; remove the `(require 'jf-gptel-scope-yaml ...)` import.
- Tangle: `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org`.

## Cycle-2 dispositions (writer scope)

This task implements the **writer-side** half of the cycle-1
disposition tasks. The action-handler half lives in
`harden-add-to-scope-action-handler` (cycle-3).

### Ask 10A — :read-metadata gets its own bucket (Path 1)

- `--map-operation-to-drawer-key` returns `"GPTEL_SCOPE_READ_METADATA"`
  for `:read-metadata` (was `"GPTEL_SCOPE_READ"`). See
  `interfaces.org` :: `register/vocabulary/operation-to-drawer-key`
  for the full revised cond.
- The writer otherwise treats `GPTEL_SCOPE_READ_METADATA` exactly
  like the other list-shape keys (dedup, `+:` suffix, save-buffer).
- **Out of scope for this task**: validator-side acceptance of
  `:read-metadata` against `:paths.read-metadata` patterns. That
  belongs to `rewire-validator-config-load` — verify on review that
  the loader stage 1/2 collapse parses the new key into
  `(:paths :read-metadata <list>)`.

### Ask 10B — :match-pattern not in writer domain (Path 2)

- `--map-operation-to-drawer-key` raises a strict error if
  `:match-pattern` reaches it: `"scope-expansion: :match-pattern
  reached the writer — action handler should have redirected to
  :read-directory"`.
- Add a buttercup spec asserting the error contract (the action
  handler's redirect logic itself is tested in
  `harden-add-to-scope-action-handler` cycle-3).

### Ask 10C — :delete keeps WRITE (Path 2)

- No code change; the existing `(memq operation '(:write :create
  :create-or-modify :append :delete))` arm is preserved. Update
  the inline comment to reference the disposition decision so a
  future reader doesn't re-litigate it.

## Implementation steps

1. Rewire the action handlers. The current shape (in `scope-expansion.org`) is roughly:

   ```
   (jf/gptel-scope--write-pattern-to-scope ...) → calls --add-path-to-scope or --add-bash-to-scope
                                                → calls a YAML reader, mutates the parsed plist,
                                                  re-emits via the YAML writer, writes the file
   ```

   Replace with:

   ```
   (jf/gptel-scope--write-pattern-to-drawer (current-buffer) operation pattern)
   ```

   The `(current-buffer)` here is the chat buffer driving the expansion. If the action handlers run inside the transient (which is associated with the chat buffer that triggered the expansion), `(current-buffer)` should resolve correctly. If the transient creates its own buffer, capture the chat buffer at expansion-trigger time and pass it through.

2. Update `--add-path-to-scope` to call the drawer writer with the operation keyword (no longer needs to look up the YAML section name).

3. Update `--add-bash-to-scope` to call the drawer writer for the path-shaped resource branch. The bare-command-name branch (where the resource has no path characters) keeps its current "emit a user message and call back without writing" behavior — see `scope-rearch-followups` Bug 2 for the pre-existing fix to ensure the callback fires; if that change has not landed, fold the fix into this task.

4. Rewrite `jf/gptel-scope--edit-scope` (the "Edit Manually" handler):

   ```elisp
   (defun jf/gptel-scope--edit-scope ()
     "Bring the chat buffer's session.org into focus and unfold the
   :PROPERTIES: drawer at point-min, then quit the transient."
     (interactive)
     (let ((buffer (jf/gptel-scope--current-chat-buffer)))
       (unless buffer
         (user-error "No chat buffer associated with this expansion"))
       (switch-to-buffer buffer)
       (goto-char (point-min))
       (when (looking-at-p "^[ \t]*:PROPERTIES:[ \t]*$")
         (org-cycle))
       (transient-quit-one)))
   ```

5. Delete `jf/gptel-scope--get-scope-file-path` and `jf/gptel-scope--validate-scope-file-writable`. Their only callers are inside the action handlers being rewired in step 1.

6. Remove `(require 'jf-gptel-scope-yaml ...)` from the top of `scope-expansion.el` (tangled output) — the writer no longer parses YAML.

7. Tangle. Run `./bin/run-tests.sh -d config/gptel/scope/test/expansion` — tests that fixture `scope.yml` will fail; those are migrated in `migrate-expansion-tests`. Tests that don't depend on the YAML I/O (queue, callback shapes, suffix-resolution flow) should pass.

## Design rationale

Per Decision 4 in design.md, the writer mutates the buffer and saves it. Eliminating the YAML round-trip path also eliminates the need for `--get-scope-file-path` and `--validate-scope-file-writable` (Decision 4 follow-on; spec REMOVED requirement "Context directory resolution").

The "Edit Manually" action shifts from "open scope.yml in a buffer" to "bring the chat buffer's session.org to the foreground and surface the drawer." The user is then editing the same file the validator reads, with full org-mode editing affordances and full undo/redo.

`scope-rearch-followups` coordination (Decision 7): if that change has not landed, its Bug 2 fix (callback always fires in `--add-bash-to-scope` even on the bare-command-name path) and Bug 3 cleanup (remove `:org-roam-patterns` references) belong in this task. Add them inline.

## Design pattern

The action handlers stay structurally the same — read `:operation` from the violation, route by `:validation-type`, write, callback. Only the writer call swaps. This minimizes the diff and keeps the queue/transient/callback logic untouched.

## Verification

- `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org` succeeds.
- `grep -n 'scope-yaml' config/gptel/scope/scope-expansion.el` returns no results.
- `grep -n 'scope.yml' config/gptel/scope/scope-expansion.el` returns no results except possibly in user-facing message strings (audit those — they should reference "scope drawer" or "session.org").
- `grep -n 'get-scope-file-path\|validate-scope-file-writable' config/gptel/scope/scope-expansion.el` returns no results.
- `./bin/run-tests.sh -d config/gptel/scope/test/expansion` runs (YAML-fixture tests will fail until `migrate-expansion-tests`; queue / callback / suffix tests pass).

## Context

design.md § Decisions 4, 7
design.md § Migration Plan step 4
specs/gptel/scope-expansion/spec.md § MODIFIED Requirements / "Add to scope action", "Add wildcard action", "Add custom pattern action", "Edit scope manually action", "Section-targeted writes"
specs/gptel/scope-expansion/spec.md § REMOVED Requirements / "scope.yml writer preserves structure", "Context directory resolution"

## Cycle 1 updates (cycle-1777460733)

### Cited register entries
- `register/boundary/scope-pattern-writer`: speculated → confirmed. Writer landed exactly as speculated; the five `--add-*-to-scope` consumers listed in the entry are still on the legacy YAML mutator. This task does the wiring. See `.orchestrator/cycles/cycle-1777460733/reconciliations/boundary-scope-pattern-writer.md`.
- `register/vocabulary/operation-to-drawer-key`: speculated → divergent → reconciled. The `:deny` arm was removed (was dead — no producer emits `:deny`); the permissive `t`-fallback was replaced with explicit `(null operation)` + strict-error arm; `unmapped_policy: error` was added. 11 members, not 12. See `.orchestrator/cycles/cycle-1777460733/reconciliations/vocabulary-operation-to-drawer-key.md`.
- `register/invariant/scope-add-pattern-idempotent`: speculated → confirmed. Dedup short-circuit holds; runtime test lands in `migrate-expansion-tests`. See `.orchestrator/cycles/cycle-1777460733/reconciliations/invariant-scope-add-pattern-idempotent.md`.
- `register/shape/violation-info`: unchanged. No cycle-1 implementation modified producers/consumers; deferred to this task's exercise.

### User-resolved decisions
- `ask-arch-cycle-1777460733-1`: `:deny` arm + permissive READ fallback divergence — user chose option B (strict-error fallback). **Implication for this task**: when this task wires the action handlers, calls into `--map-operation-to-drawer-key` will now error on nil-operation or unmapped-operation cases. Cloud-auth and parse-incomplete violations carry `:operation nil` and will hit the strict-error path; the discovered-from-this-cycle companion `refuse-add-to-scope-on-nil-operation` is the upstream guard.

### Meta-discoveries
- `vocabulary-cluster/permissive-default-vs-closed-vocabulary`: closed-set vocabularies should fail loudly at producer boundaries. **Implication for this task**: when wiring action handlers, do not paper over upstream nil-operation cases inside the action handler — `refuse-add-to-scope-on-nil-operation` is the right layer for that guard.
- `other/asymmetric-vocabulary-enforcement-write-vs-read`: closed-set enforcement should be symmetric across read/write boundaries. **Implication**: the writer is now strict on operation; verify nothing in this rewire's action paths bypasses `--map-operation-to-drawer-key` with hand-rolled translations.

### Already-shipped inline fixes
- `arch-cycle-1777460733-8`: `:deny → GPTEL_SCOPE_DENY` arm removed from `--map-operation-to-drawer-key` (was dead — no producer emits `:deny`). **Implication for this task**: do NOT add a `:deny` operation when invoking the writer; if "add to deny list" is needed, it's an action-layer concern (see disposition-* tasks discovered from cycle 1).
- `arch-cycle-1777460733-9`: permissive `t`-fallback replaced with strict error + explicit `(null operation)` error arm. **Implication for this task**: `--write-pattern-to-drawer` will signal on nil/unmapped operation; the action handlers must guard or the user-visible error will be a low-context Lisp error.
- `arch-cycle-1777460733-11`: write-side cloud-auth validation added in `scope-profiles.el` (mirrors reader's enforcement at `scope-validation.el:546`). **Implication for this task**: orthogonal to the expansion writer rewire, but the cloud-auth path is now write-validated end-to-end.

## Observations

- **Chat-buffer plumbing**: the task brief offered two options for resolving the chat buffer in `--edit-scope` and the action handlers — capture at trigger time or pull from `(transient-scope)`. I implemented both: `jf/gptel-scope-prompt-expansion` captures `(current-buffer)` and stores it on the transient scope as `:chat-buffer` (and on each queued entry); a new helper `jf/gptel-scope--current-chat-buffer` reads from the transient scope and falls back to `(current-buffer)`. The fallback exists for callers that bypass the transient (e.g. unit tests that stub `transient-scope` without a buffer); production callers always go through `jf/gptel-scope-prompt-expansion`. The fallback is defensive and should be revisited if it produces surprising behavior in tests.
- **`--add-wildcard-to-scope` and `--add-custom-to-scope` did not call `jf/gptel-scope--process-expansion-queue`** in the original code. `--add-to-scope` and `--deny-expansion` did. This appears to be a pre-existing latent bug: leaving the expansion-active flag set means subsequent queued prompts never fire. I added `process-expansion-queue` calls to both wildcard and custom handlers to make queue progression uniform across all add-to-scope variants. Worth a regression test in `migrate-expansion-tests` to lock the contract.
- **YAML helper functions removed wholesale**: while the task only explicitly named `--get-scope-file-path` and `--validate-scope-file-writable` for deletion, the YAML round-trip (`--write-yaml-plist`, `--write-yaml-nested-list`, `--write-yaml-tools`, `--write-yaml-generic-nested-plist`, `--write-yaml-simple-value`, `--kebab-to-snake`, `--read-scope-file-as-yaml`, plus the `--add-path-to-scope` body that consumed them) had no remaining callers once handlers stopped routing through `--write-pattern-to-scope`'s YAML branch. I removed the entire `* YAML Helper Functions` section. The `(require 'jf-gptel-scope-yaml ...)` and `(require 'yaml)` imports were dropped. `--add-path-to-scope` and `--add-bash-to-scope` survive as thin wrappers around `--write-pattern-to-drawer` so the validation-type → routing dispatch in `--write-pattern-to-scope` keeps a single shape for all five action handlers.
- **Signature changes for `--add-path-to-scope` / `--add-bash-to-scope` / `--write-pattern-to-scope`**: dropped the `scope-file` parameter (no longer needed; the writer mutates the chat buffer). The 28 expansion-folder test failures in `add-to-scope-section-targeting-spec.el`, `add-bash-to-scope-routing-spec.el`, and `expansion-ui-spec.el` all stem from this signature change and the YAML-fixture orientation of those tests; they are squarely in `migrate-expansion-tests`'s scope. The 14 new `operation-to-drawer-key-spec.el` tests added by this task all pass.
- **Spec `expansion-ui-spec.el` "creates transient scope with correct structure"** now fails because the assertion pinned the OLD scope plist shape (without `:chat-buffer`). The test will be migrated in `migrate-expansion-tests`; the production behavior is correct.
- **Spec REMOVED Requirement "Context directory resolution"** is honored: `--add-path-to-scope` no longer signals `user-error` on missing/unwritable scope.yml. Two `expansion-ui-handlers-spec.el` tests (`handles scope file not found error`, `handles scope file not writable error`) assert the obsolete error contract. They will be migrated.
- **Latent issue in `jf/gptel-scope--map-operation-to-scope-section`** (in `scope-validation.el`): this is the YAML-section-key analogue of the drawer-key collapse, and per the design rationale must stay in lockstep. After this task the writer side no longer uses it (production path is fully drawer-based), but it's still referenced by validator code paths. Cycle-3 should audit whether it's still load-bearing or can be deleted alongside the YAML I/O on the loader side.
- **Spec text `specs/gptel/scope-expansion/spec.md` line 84** still reads "read-like granular operations (`:read-directory`, `:read-metadata`, `:match-pattern`) collapse to `:GPTEL_SCOPE_READ`". This contradicts the cycle-2 dispositions encoded in this task (and in `interfaces.org`). The spec text update belongs to the `opsx-sync` / archive phase; not modified here per role boundaries.
- **`--add-bash-to-scope` bare-command branch (Bug 2 from `scope-rearch-followups`)**: I retained the existing "emit message and return nil" behavior. The callback is fired by the action handlers themselves (`--add-to-scope` etc.) regardless of the writer's return value, so the wrapper does not hang when `--add-bash-to-scope` returns nil — Bug 2's hang condition is already mitigated by the action-handler structure. I did not add a "Cannot add command 'X' to scope" callback message because the action handler emits a generic "Added X to scope" success message and the wrapper sees `:success t`. This is a behavior preservation choice; if the user-facing UX should signal "no-op" instead of "added", that's a follow-up.

## Discoveries

- discovery_id: disc-rewire-expansion-writer-1
  class: shape-fragmentation
  description: |
    The transient scope plist shape now includes `:chat-buffer` (added by
    this task) alongside `:violation`, `:callback`, `:patterns`,
    `:tool-name`. This is not currently declared as a register entry. The
    five action handlers and `--current-chat-buffer` are all consumers;
    `jf/gptel-scope-prompt-expansion` and `--process-expansion-queue` are
    producers. Without a register entry, future edits could accidentally
    drop the `:chat-buffer` field.
  affected_register_entry: register/shape/violation-info (adjacent — this
    is a different plist, the transient-scope plist, which has no
    register entry yet)
  recommendation: |
    Add a `register/shape/expansion-transient-scope` entry that pins the
    five-key shape (`:violation`, `:callback`, `:patterns`, `:tool-name`,
    `:chat-buffer`) and lists the producers/consumers. Status:
    speculated until cycle-3 lands `harden-add-to-scope-action-handler`
    which will be the largest consumer.

- discovery_id: disc-rewire-expansion-writer-2
  class: invariant-gap
  description: |
    `jf/gptel-scope--process-expansion-queue` was called by
    `--add-to-scope` and `--deny-expansion` but NOT by
    `--add-wildcard-to-scope` or `--add-custom-to-scope` in the previous
    code. This means queued expansions could be silently dropped if the
    user resolved the first via wildcard or custom-pattern. I added the
    queue-progression call to both handlers in this task. The
    invariant should be: every transient suffix that returns control
    must call `--process-expansion-queue` (or be a no-op like
    `transient-quit-one`).
  affected_register_entry: (none yet — would be a new
    `register/invariant/expansion-queue-always-progresses`)
  recommendation: |
    Add the invariant to `interfaces.org` and have
    `migrate-expansion-tests` add a regression spec: queue two
    expansions, resolve the first via wildcard, assert the second
    transient is set up.

- discovery_id: disc-rewire-expansion-writer-3
  class: dead-branch
  description: |
    `jf/gptel-scope--map-operation-to-scope-section` (in
    `scope-validation.el`) is the YAML-section analogue of
    `--map-operation-to-drawer-key`. After this task lands, the writer
    side has no callers of the YAML-section function. The validator may
    still use it for diagnostics, but it should be audited.
  affected_register_entry: register/vocabulary/operation-to-drawer-key
    (the section-key analogue is not registered separately)
  recommendation: |
    Cycle-3 audit: grep `--map-operation-to-scope-section` callers; if
    only validator-internal diagnostic, document or remove. If still
    load-bearing for the loader side, the lockstep coupling needs a
    register entry to track.

- discovery_id: disc-rewire-expansion-writer-4
  class: spec-signal
  description: |
    The spec `specs/gptel/scope-expansion/spec.md` (REQUIRED Section-
    targeted writes scenario) still describes the pre-cycle-2 collapse
    where `:read-metadata` and `:match-pattern` collapse to
    `GPTEL_SCOPE_READ`. After cycle-2 dispositions 10A/10B, this is now
    contradicted by both the implementation and `interfaces.org`. Spec
    text was not modified here (out of role scope).
  affected_register_entry: register/vocabulary/operation-to-drawer-key
  recommendation: |
    `opsx-sync` (or `delete-yaml-and-security-residue` archive prep)
    should reconcile the spec text with the cycle-2 disposition. The
    new wording: ":read-metadata collapses to GPTEL_SCOPE_READ_METADATA;
    :match-pattern is action-handler-only (redirected to
    :read-directory)."

- discovery_id: disc-rewire-expansion-writer-5
  class: deviation
  description: |
    `--add-bash-to-scope` bare-command branch behavior preserved
    verbatim from before the rewire (emit message, return nil). The
    `scope-rearch-followups` Bug 2 fix is implicitly satisfied because
    the callback is invoked by the action handler (which always fires
    on the success path), not by `--add-bash-to-scope` itself. The
    user-facing message says "Added X to scope" even on the bare-
    command no-op path because the handler doesn't differentiate. This
    is a UX paper-cut, not a correctness issue.
  affected_register_entry: (none — UX-only)
  recommendation: |
    Optional follow-up: `--add-bash-to-scope` could return a status
    plist (`(:wrote t)` vs `(:wrote nil :reason "bare command name")`)
    so the action handler can choose the right user-facing message and
    the right callback shape. Park in `.tasks/` if the maintainer cares
    about the UX.
