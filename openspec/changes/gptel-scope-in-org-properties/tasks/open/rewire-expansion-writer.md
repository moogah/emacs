---
name: rewire-expansion-writer
description: Switch expansion add-to-scope handlers to the drawer writer; remove scope.yml file-path resolver
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:implement-drawer-writer
---

## Cites register entries

- `register/boundary/scope-pattern-writer` — every `--add-*-to-scope` handler becomes a consumer of this boundary; no other code path is allowed to mutate drawer scope state.
- `register/shape/violation-info` — your handlers read `:operation`, `:resource`, `:validation-type`, `:tool` from the violation-info plist.
- `register/vocabulary/operation-to-drawer-key` — your handlers route operations through this canonical mapping; no inline `pcase` translations elsewhere.
- `register/invariant/scope-add-pattern-idempotent` — preserve the contract: the same pattern twice is a no-op with empty `:patterns_added`.

Scaffolds:
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/boundaries/scope-pattern-writer.el`
- `openspec/changes/gptel-scope-in-org-properties/scaffolding/vocabularies/operation-to-drawer-key.el`

## Files to modify
- `config/gptel/scope/scope-expansion.org` (modify) — rewire `--add-to-scope`, `--add-wildcard-to-scope`, `--add-custom-to-scope`, `--add-path-to-scope`, `--add-bash-to-scope` to use `jf/gptel-scope--write-pattern-to-drawer`; rewrite `--edit-scope` (Edit Manually action) to surface `session.org` instead of `scope.yml`; remove `--get-scope-file-path` and `--validate-scope-file-writable`; remove the `(require 'jf-gptel-scope-yaml ...)` import.
- Tangle: `./bin/tangle-org.sh config/gptel/scope/scope-expansion.org`.

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
