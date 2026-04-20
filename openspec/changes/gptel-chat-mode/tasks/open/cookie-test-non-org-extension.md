---
name: cookie-test-non-org-extension
description: Add cookie-activation test on non-.org file extension
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/test/parser/buffer-format-spec.el`

## Implementation steps
1. The current cookie test writes `# -*- gptel-chat -*-` to a
   `.org` tempfile. Since org-mode would activate from the
   extension anyway, the test does not isolate the cookie's
   effect.
2. Add a second `it` block that writes the cookie to a `.txt`
   tempfile (or a file with no extension) and asserts that
   `(gptel-chat-mode)` is active after `find-file-noselect`.
3. Use `unwind-protect` with `delete-file` to clean up the temp
   file.

## Design rationale
Spec scenario "File-local cookie activation" should prove the
cookie alone activates the mode. Using `.org` masks the test —
passing means "either cookie or extension worked", which is
weaker than the spec promises.

## Verification
- `./bin/run-tests.sh -d config/gptel/chat/test/parser` passes.
- The new `it` covers a non-`.org` extension.

## Context
- Review of `mode-definition` task Finding #8.
