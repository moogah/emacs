---
name: remove-orphan-sanitize-module
description: Remove standalone sanitize.org/sanitize.el (sanitize is internal to stream)
change: gptel-chat-mode
status: ready
relations:
  - discovered-from:scaffold-chat-subsystem
  - enables:sanitize-chunks
---

## Files to modify
- `config/gptel/chat/sanitize.org` (delete)
- `config/gptel/chat/sanitize.el` (delete — generated)
- `config/gptel/chat/chat.org` (modify — remove sanitize loader entry and
  step-3 dependency note)
- `config/gptel/chat/chat.el` (tangled)
- `openspec/changes/gptel-chat-mode/architecture.md` (modify — fix the
  "six modules" header that contradicts the 7-section enumeration)
- `openspec/changes/gptel-chat-mode/tasks/closed/scaffold-chat-subsystem.md`
  (modify — task body says "six feature modules" then lists eight; correct
  the count and remove `sanitize` from the enumerated list)

## Implementation steps
1. Delete `config/gptel/chat/sanitize.org` and `config/gptel/chat/sanitize.el`.
2. In `chat.org`, remove the `(jf/load-module ... "sanitize.el" ...)` line
   and the step-3 dependency note that mentions sanitize.
3. Re-tangle `chat.org`. Confirm `chat.el` no longer references sanitize.
4. In `architecture.md` §Components, reconcile the header word ("six")
   with the sub-section count. Sanitize is a function inside `stream`, not
   a module (per design.md Decision 4 and architecture.md:115's
   `gptel-chat--sanitize-chunk | ... | stream` row).
5. Amend the closed `scaffold-chat-subsystem` task body so the historical
   record matches what the architecture says (verify exact module
   inventory against architecture.md sub-sections and pick a consistent
   number).
6. Verify `./bin/emacs-isolated.sh` boots cleanly with no missing-feature
   errors.

## Design rationale
Architecture.md and design.md both place sanitisation as a function
(`gptel-chat--sanitize-chunk`) inside the `stream` module. The open
`sanitize-chunks` task targets `stream.org`, not `sanitize.org`. The
standalone module is an orphan no task will populate, with a
`(provide 'gptel-chat-sanitize)` feature symbol nothing will `require`.
Task body text was internally inconsistent ("six feature modules"
followed by an eight-item list); the implementing agent silently resolved
this by counting the list, which created the orphan.

User decision (orchestrator session 2026-04-20): keep sanitize internal
to stream; delete the orphan module.

## Verification
- `ls config/gptel/chat/sanitize.*` returns no files.
- `grep -n sanitize config/gptel/chat/chat.org` returns no live loader
  references (TODO/comment hits acceptable).
- `./bin/tangle-org.sh config/gptel/chat/chat.org` succeeds.
- `./bin/emacs-isolated.sh` boots; no `(require 'gptel-chat-sanitize)`
  failure surfaces from any module.
- `architecture.md` §Components header word agrees with the sub-section
  count.

## Context
- Review of scaffold-chat-subsystem (orchestrator session 2026-04-20)
  Finding #1 and Finding #5
- architecture.md §Components and architecture.md:115
- design.md Decision 4
- tasks/open/sanitize-chunks.md (the task that owns the actual sanitize
  function — targets stream.org)
