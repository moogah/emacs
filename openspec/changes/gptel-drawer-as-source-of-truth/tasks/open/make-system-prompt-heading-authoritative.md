---
name: make-system-prompt-heading-authoritative
description: With the `* System Prompt` heading in place, make its body the authoritative system prompt. On mode activation the chat-mode restore path reads the heading body and installs it buffer-locally as gptel--system-message, winning over the preset. On save the current system message is written back into the heading body. Old sessions with no heading fall back to the preset and materialize the heading on first save.
change: gptel-drawer-as-source-of-truth
status: blocked
relations:
  - blocked-by:emit-system-prompt-and-chat-headings-at-creation
---

## Files to modify

- `config/gptel/chat/menu.org` (modify) — restore path reads the `* System Prompt` body into `gptel--system-message`; save path writes it back
- `config/gptel/chat/mode.org` (modify, if the restore hook ordering lives there) — ensure the system-prompt read runs after preset application and the drawer overlay
- `openspec/changes/gptel-drawer-as-source-of-truth/specs/gptel/chat-mode.md` (modify) — revise the system-prompt restore/save requirements
- `config/gptel/chat/test/menu/preset-wiring-spec.el` and `save-state-spec.el` (modify) — add restore + save + round-trip scenarios

## Why

design.md §Addendum Finding B (Decision B). This task makes the `* System Prompt` heading load-bearing — the document, not the preset, is the source of truth for the system prompt, completing the WYSIWYG contract for the one key Decision 2 had excluded.

Restore precedence becomes: `* System Prompt` heading body > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The writer still never emits `:GPTEL_SYSTEM:` (Decision 2's write-exclusion stands; that property is now legacy-read-only).

## Implementation steps

1. **Restore.** Add a step to the mode-activation path (after `gptel--apply-preset` and `gptel-chat--apply-drawer-overrides`, so it wins) that locates the `* System Prompt` heading, skips its property drawer, and reads the body text. When the body is non-blank, install it as buffer-local `gptel--system-message`. When the heading is absent, or its body is blank, do nothing — the preset's `:system` (or a legacy `:GPTEL_SYSTEM:` drawer entry already applied by the overlay) stands. Treat a blank body as "not authored" so an empty heading does not silently wipe the prompt.
   - Read the body with a simple narrow/regexp scan (e.g. `org-narrow-to-subtree` then skip the drawer) — do **not** introduce `org-element-parse-buffer` (chat-mode parser Decision 1).
2. **Save.** In the `before-save-hook` path (`gptel-chat--save-state`), after the drawer write, serialize the current buffer-local `gptel--system-message` into the `* System Prompt` heading body. When the heading does not exist (old session), create it — file-level drawer, then `* System Prompt` with `:VISIBILITY: folded`, then the body — using the **same heading-construction helper** introduced by `emit-system-prompt-and-chat-headings-at-creation` (single source of truth for the heading shape). The `* Chat` heading is created at the same time if missing so turn blocks remain under it.
3. **Precedence + back-compat.** Confirm a legacy `:GPTEL_SYSTEM:` drawer entry is still honored when there is no `* System Prompt` heading (the existing overlay in `gptel-chat--apply-drawer-overrides` already handles this — the new heading read simply runs after and supersedes it when the heading is present).
4. Re-tangle the touched `.org` files; update `specs/gptel/chat-mode.md`: the "system prompt comes from preset" requirement becomes "system prompt comes from the `* System Prompt` heading body, falling back to the preset"; add the save-writes-heading requirement; keep the "save never writes `:GPTEL_SYSTEM:`" requirement.
5. Add tests:
   - Restore: open a session whose `* System Prompt` body differs from the preset `:system`; assert `gptel--system-message` equals the body.
   - Restore fallback: open an old session with no `* System Prompt` heading; assert `gptel--system-message` equals the preset `:system`.
   - Save: change `gptel--system-message`, save, assert the `* System Prompt` body reflects the new value and no `:GPTEL_SYSTEM:` property was written.
   - Round-trip: create → restore → save → re-restore yields a stable system prompt and a stable file (idempotent save).
6. Re-run `./bin/run-tests.sh -d config/gptel/chat` and `./bin/run-tests.sh -d config/gptel/sessions`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/tangle-org.sh config/gptel/chat/mode.org
./bin/run-tests.sh -d config/gptel/chat
./bin/run-tests.sh -d config/gptel/sessions
grep -n 'System Prompt\|gptel--system-message\|GPTEL_SYSTEM' config/gptel/chat/menu.el
```

Expect: restore reads the heading body; save writes it; precedence heading > drawer > preset; `:GPTEL_SYSTEM:` is never written.

## Context

design.md §Addendum Finding B (Decision B). Current restore/overlay: `gptel-chat--apply-declared-preset` and `gptel-chat--apply-drawer-overrides` in `config/gptel/chat/menu.el`. Current save: `gptel-chat--save-state` / `gptel-chat--write-config-drawer`. Heading-construction helper comes from sibling task `emit-system-prompt-and-chat-headings-at-creation` — reuse it, do not duplicate the heading literals.

This task is `task_class: contract` (per Architect cycle-7 forward-mode drift signal) — it redefines restore/save precedence semantics, not just adds behaviour.

Cited register entries (cycle-7 plan):
- `interfaces.org#register-invariant-system-prompt-heading-authoritative` (`speculated`) — this task **enforces** it; the scaffolded failing test at `openspec/changes/gptel-drawer-as-source-of-truth/scaffolding/invariants/system-prompt-heading-authoritative.test.el` is this task's acceptance criterion. Make it pass, or revise the scaffold and explain in `## Discoveries`.
- `interfaces.org#register-shape-session-document-layout` (`speculated`) — the save-path materialisation for old sessions produces this layout.
- `interfaces.org#register-invariant-drawer-system-key-write-exclusion` (`confirmed`, **load-bearing**) — composes with this task: the writer must still never emit `:GPTEL_SYSTEM:`. Do not violate the write-exclusion while adding heading-body serialisation.
- `interfaces.org#register-invariant-drawer-overlay-wins-over-preset` (`confirmed`, **load-bearing**) — this task changes the restore precedence chain (heading body now sits ahead of the entry's asymmetric `:system` preset-fallback exception). An on-touch Architect audit fires against this entry; integrate will reconcile its `:system`-exception wording. Read it before changing the restore order.
