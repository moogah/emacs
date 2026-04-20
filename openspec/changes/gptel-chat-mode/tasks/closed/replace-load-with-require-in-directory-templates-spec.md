---
name: replace-load-with-require-in-directory-templates-spec
description: Use require, not load, in directory-templates-spec.el; drop unused logging.el load
change: gptel-chat-mode
status: needs-review
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el`
  (modify — module-loading section around lines 28-34)

## Implementation steps
1. Replace the `(let ((sessions-dir ...)) (load ...))` block with the
   repo's standard `require` pattern as used in
   `config/gptel/test/session-creation-spec.el:28-33`:
   ```elisp
   (require 'gptel-session-constants)
   (require 'gptel-session-filesystem)
   ```
2. Drop the `(load (expand-file-name "logging.el" sessions-dir) nil t)`
   line — `logging` symbols are not used anywhere in this spec file.
3. If the spec file declares `(require 'cl-lib)` purely defensively
   (no `cl-` calls in the body), consider removing that too — verify
   first.
4. Run the spec to confirm no regression:
   `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem`.

## Design rationale
The `load` form requires `.el` files to be tangled but does not fail
with a clear error if they aren't, and it doesn't integrate with the
`provide`/`features` system. The repo's established convention (see
`session-creation-spec.el`) uses `require` against the feature symbols.
Loading `logging.el` for symbols the test never uses is dead code.

## Verification
- `grep -n "(load " config/gptel/sessions/test/filesystem/directory-templates-spec.el`
  returns nothing.
- Spec passes: `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem`.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #5
- Reference pattern: `config/gptel/test/session-creation-spec.el:28-33`
