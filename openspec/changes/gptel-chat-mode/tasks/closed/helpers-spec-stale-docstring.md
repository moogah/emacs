---
name: helpers-spec-stale-docstring
description: Update helpers-spec.el commentary to reflect actual mode-test location
change: gptel-chat-mode
status: done
relations:
  - discovered-from:mode-definition
---

## Files to modify
- `config/gptel/chat/test/helpers-spec.el` (commentary block)

## Implementation steps
1. The helpers file's docstring currently says: "parser tests
   exercise the block-level state machine directly and do not
   depend on org-mode being active. Mode-activation tests belong
   in a separate file owned by the `mode-definition` task."
2. The mode-definition task instead folded its activation tests
   into `buffer-format-spec.el` (permitted by its task body).
3. Update the helpers commentary to: mode-activation tests
   currently live alongside parser specs in `buffer-format-spec.el`
   (because the fixture is shared); future splitting is optional.

## Design rationale
The current commentary will mislead a future contributor into
either looking for a non-existent file or creating a duplicate.
A one-line edit keeps the test tree self-explanatory.

## Verification
- The helpers commentary block matches the actual test layout.
- No references to a non-existent mode-spec file remain.

## Context
- Review of `mode-definition` task Finding #3.
