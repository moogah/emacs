---
name: remove-redundant-negative-assertions-in-directory-templates-spec
description: Drop :not :to-equal "session.md" mirror assertions; keep positive form only
change: gptel-chat-mode
status: done
relations:
  - discovered-from:sessions-filesystem
---

## Files to modify
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el`
  (modify — assertions around lines 58-59, 68-71, 88-89, 113)

## Implementation steps
1. Locate every `(expect X :not :to-equal "session.md")` and
   `(expect X :not :to-match "/session\\.md\\'")` assertion.
2. Delete each one. The paired positive assertion
   (`(expect X :to-equal "session.org")`) already entails the negative.
3. Run the spec to confirm no regression:
   `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem`.

## Design rationale
Belt-and-braces assertions where the positive form logically entails
the negative add no information and dilute test signal. They look like
tests written to reassure the author that the rename happened, rather
than to verify behaviour. If the actual concern was a code path that
returned both extensions concatenated (`"session.org.md"` or similar),
the assertion would need to be different. As written, it's pure
duplication.

## Verification
- `grep -n "session.md" config/gptel/sessions/test/filesystem/directory-templates-spec.el`
  returns nothing (or only commentary).
- Spec passes: `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem`.

## Context
- Review of sessions-filesystem (orchestrator session 2026-04-20)
  Finding #7
- directory-templates-spec.el assertions at lines 58-59, 68-71, 88-89, 113
