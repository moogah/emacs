---
name: sessions-filesystem
description: Update sessions directory templates to use session.org
change: gptel-chat-mode
status: done
relations: []
---

## Files to modify
- `config/gptel/sessions/filesystem.org` (modify)
- `config/gptel/sessions/filesystem.el` (tangled)
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el` (new or
  modify — depends on what exists)

## Implementation steps
1. In `filesystem.org`, locate every reference to `session.md` in directory
   templates, branch-creation helpers, and agent-creation helpers. Replace
   with `session.org`:
   - Branch template: `branches/<branch-name>/session.org`
   - Agent template: `agents/<agent-name>/session.org`
   - Any path-building helper that assembles session file paths.
2. Session-id format, `current` symlink behaviour, `metadata.yml` /
   `scope.yml` / `branch-metadata.yml` paths are **unchanged**. Only the
   conversation file's name changes.
3. Write or update Buttercup tests asserting every new session created via
   the filesystem helpers places the conversation file at `session.org`,
   not `session.md`. Tests should mount on a tempdir (`make-temp-file`
   with `directory` t) and verify the on-disk structure directly.
4. Do **not** attempt to migrate or detect pre-existing `session.md` files
   — Decision 19 is an explicit clean break. Old files remain on disk and
   are simply not found by helpers looking for `session.org`.

## Design rationale
Decision 18 chose `.org` because chat-mode's delimiters are org special
blocks, which are not valid markdown. Keeping `.md` would mislead:
- external tooling (file-type detection)
- org-mode's own file-association logic
- contributors browsing the filesystem
- any tree-sitter grammar selection

Alternatives rejected:
- **Keep `.md`, treat as org internally**: tooling friction as above.
- **New extension `.chat` or `.gptel`**: chat-mode files *are* valid org
  files; `.org` is the honest extension.

Decision 19 explicitly disallows migration. This task is purely additive
— it does not touch legacy `session.md` files.

## Design pattern
Directory structure per the sessions-persistence delta spec:
```
~/.gptel/sessions/<session-id>/
├── branches/<branch-name>/
│   ├── session.org         # Conversation in chat-mode block format
│   ├── metadata.yml        # Unchanged
│   ├── scope.yml           # Unchanged
│   ├── branch-metadata.yml # Unchanged
│   └── agents/             # Sub-agents (optional, unchanged)
└── current -> branches/<branch-name>  # Unchanged
```

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/filesystem.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` passes.
- `grep -n "session.md" config/gptel/sessions/filesystem.{org,el} \
     config/gptel/sessions/constants.{org,el}` returns nothing or only
  archival comments. Other live `session.md` callers in
  `commands.org`, `branching.org`, `activities-integration.org`, and
  under `config/gptel/test/` are out of scope here — they are owned by
  the parallel tasks `sessions-auto-init`,
  `sessions-persistent-create`, `sessions-branching`, and
  `sessions-activities`, and by follow-up tasks discovered during
  review.
- Creating a fresh session via the helpers produces `session.org` (not
  `.md`) at the expected path.

## Context
- design.md §Decision 18 (session file format is `session.org`)
- design.md §Decision 19 (clean break — no migration)
- specs/gptel/sessions-persistence.md §"Session file format is session.org
  in chat-mode format" (MODIFIED)
- architecture.md §`sessions/filesystem` (modified)
