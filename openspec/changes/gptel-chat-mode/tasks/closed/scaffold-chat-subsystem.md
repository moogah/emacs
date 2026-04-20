---
name: scaffold-chat-subsystem
description: Create config/gptel/chat/ directory, main loader, add to enabled-modules
change: gptel-chat-mode
status: done
relations: []
---

## Files to modify
- `config/gptel/chat/chat.org` (new — main loader)
- `config/gptel/chat/chat.el` (tangled)
- `init.org` (modify — add to `jf/enabled-modules` before sessions)
- `init.el` (tangled)

## Implementation steps
1. Create `config/gptel/chat/` directory.
2. Create `config/gptel/chat/chat.org` as the module loader. First block
   `:comments no` to keep `lexical-binding: t` on line 1 (repo memory rule).
   Initial loader contents: load seven feature modules (`mode`, `parser`,
   `stream`, `send`, `nav`, `display`, `menu`) in dependency order. Note:
   sanitisation is an internal helper inside the `stream` module
   (`gptel-chat--sanitize-chunk`), not a standalone module (see
   architecture.md §Components and design.md Decision 4). The feature
   modules themselves are created by later tasks — stub `(provide 'gptel-chat-<x>)`
   bodies in each file with a TODO are acceptable at this stage.
3. Add `"gptel/chat/chat"` to `jf/enabled-modules` in `init.org`, positioned
   **before** the sessions modules (sessions will depend on chat-mode once its
   rewrite lands in tasks 14–17).
4. Tangle both files with `./bin/tangle-org.sh`.
5. Start Emacs via `./bin/emacs-isolated.sh`; confirm no module-load errors on
   boot.

## Design rationale
Chat-mode is factored into cohesive modules (architecture §Components) each with
a small public surface. A dedicated loader `chat/chat.org` mirrors the structure
of `config/gptel/gptel.org` and keeps the chat subsystem self-contained. Adding
it to `jf/enabled-modules` *before* sessions is a hard ordering requirement —
the sessions rewrite (tasks 14–17) reuses chat-mode's parser and mode
definitions.

## Design pattern
See `config/gptel/gptel.org` for the established subsystem-loader pattern:
serial `jf/load-module` calls for the feature files. See any existing `.org`
module header for the `:comments no` first-block convention (and the
`MEMORY.md` entry on lexical-binding).

## Verification
- `./bin/tangle-org.sh config/gptel/chat/chat.org` succeeds and validates.
- `./bin/emacs-isolated.sh` starts with no errors; `C-h v jf/enabled-modules`
  shows the new entry in the correct position.
- Other modules are unaffected.

## Context
- architecture.md §Components — module inventory under `config/gptel/chat/`
- architecture.md §Constraints — literate-programming + lexical-binding rules
- CLAUDE.md §Module System — `jf/enabled-modules` ordering and `jf/load-module`
  contract
