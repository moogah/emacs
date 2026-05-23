---
name: wire-sibling-file-emission-into-session-creation
description: Wire `jf/gptel--write-system-prompt-sibling-file` into `jf/gptel--create-session-core` so that fresh sessions write `system-prompt.<ext>` next to `session.org` and the configuration drawer carries the new `:GPTEL_SYSTEM_PROMPT_FILE:` property pointing at that basename. Updates session-creation specs for the new property and sibling-file presence.
change: replace-system-prompt-heading-with-sibling-file
status: ready
relations:
  - blocked-by:add-sibling-file-writer-helper
  - blocked-by:revert-initial-session-body-and-delete-headings-block
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — `jf/gptel--create-session-core` calls the writer and threads the basename into the drawer text
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (modify) — assert sibling file presence and the new drawer property
- `config/gptel/sessions/test/commands/preset-application-spec.el` (modify) — same coverage for the preset-application path
- `config/gptel/tools/persistent-agent.org` (modify) — agent creation path also calls the writer (agent sessions also get a sibling file)

## Why

design.md §Decision 1, Decision 3 — the sibling file is created at session creation. The `:GPTEL_SYSTEM_PROMPT_FILE:` drawer key is the resolution link the chat-mode restore will follow.

Threading the basename into the drawer text (rather than calling `org-entry-put` after the fact) keeps drawer rendering as a single string-composition pass — consistent with how the rest of the drawer is rendered by `jf/gptel-scope-profile--render-drawer-text`.

The agent path is included in this task because `jf/gptel-persistent-agent` is the second producer of the canonical session layout; without updating it here, agent sessions would not get sibling files and would silently fall through to preset-only system prompts.

## Implementation steps

1. **`config/gptel/sessions/commands.org`** — in `jf/gptel--create-session-core`:
   - After the session directory is created and the preset spec is resolved (the existing `preset-spec` local), and after `jf/gptel-scope-profile--render-drawer-text` produces the drawer body, call `(jf/gptel--write-system-prompt-sibling-file <branch-dir> preset-spec)` (passing the preset name as a third argument if A1's helper signature requires it).
   - When the writer returns a non-nil basename, append `:GPTEL_SYSTEM_PROMPT_FILE: <basename>` to the drawer text immediately before the `:END:` line. Use string manipulation on the drawer text rather than re-opening the file to call `org-entry-put` — the drawer hasn't been written to disk yet.
   - Wrap the appending in a helper if it occurs in more than one place (e.g., `jf/gptel--append-drawer-property text key value` — three lines, but worth it if `persistent-agent.org` needs the same).
2. **`config/gptel/tools/persistent-agent.org`** — in the agent creation flow (probably `jf/gptel-persistent-agent--task` or wherever the agent's `session.org` content is composed):
   - Call `jf/gptel--write-system-prompt-sibling-file` against the agent's session directory (no `branches/` subdirectory for agents — per `constants.el:79`).
   - Thread the basename into the drawer text the same way.
3. **Spec updates** — `session-org-creation-spec.el` and `preset-application-spec.el`:
   - For a preset with a non-empty `:system`: assert the drawer contains `:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md`, the sibling file exists with the preset's `:system` body verbatim, and the drawer still does NOT contain `:GPTEL_SYSTEM:`.
   - For a preset with nil/empty `:system`: assert the drawer does NOT contain `:GPTEL_SYSTEM_PROMPT_FILE:` and no sibling file exists.
   - Add an agent-path scenario asserting the same shape for agent sessions.
4. Re-tangle the two touched `.org` files. Run the sessions and chat-mode test suites.

## Verification

```bash
./bin/tangle-org.sh config/gptel/sessions/commands.org
./bin/tangle-org.sh config/gptel/tools/persistent-agent.org
./bin/run-tests.sh -d config/gptel/sessions
./bin/run-tests.sh -d config/gptel/chat
grep -n 'GPTEL_SYSTEM_PROMPT_FILE\|write-system-prompt-sibling-file' \
  config/gptel/sessions/commands.el \
  config/gptel/tools/persistent-agent.el
```

Expect: both creation paths call the writer; the drawer text appends the basename. Specs pass with the new assertions. The drawer continues to never contain `:GPTEL_SYSTEM:`.

## Context

architecture.md §Components — `jf/gptel--create-session-core` and the agent path are both producers; they share the writer from A1 (`add-sibling-file-writer-helper`).

design.md §Decision 1 — paths in the drawer property are basenames, resolved relative to `session.org`'s directory. This task writes basenames, never absolute paths.

The previous task `revert-initial-session-body-and-delete-headings-block` establishes the no-heading baseline that this task builds on; the drawer text composition path is already simpler after R3 lands.

## Cycle 1779565028 updates (cycle-1779565028)

- **Unblocked**: both prerequisites landed.
  `add-sibling-file-writer-helper` (merge `125a936`) adds
  `jf/gptel--write-system-prompt-sibling-file` with signature
  `(session-dir preset-name preset-spec)` — **preset-name is
  required**, not optional. Update step 1 of the implementation
  accordingly: the writer call passes the three args directly. No
  third-arg conditional needed.
- `revert-initial-session-body-and-delete-headings-block`
  (merge `ef385f4`) collapsed the agent path's `--initial-body`
  signature to `(prompt)` and dropped the `system-prompt` let
  binding in `jf/gptel-persistent-agent--task`. When this task adds
  the sibling-file writer call to the agent path, re-introduce a
  `preset-spec` binding (still already resolved upstream for the
  drawer renderer) and pass `(preset-name preset-spec)` to the
  writer — do NOT thread `system-prompt` through `--initial-body`
  again.
- **Drawer composition gotcha**: the current `(concat drawer-text
  (jf/gptel--initial-session-body))` has NO separator between
  drawer and body. This task's drawer-property appending must keep
  this invariant: insert the new `:GPTEL_SYSTEM_PROMPT_FILE:` line
  inside the drawer's `:PROPERTIES:`...`:END:` block (e.g., via
  string surgery on `drawer-text` before the concat), not by adding
  it after `:END:` (that would push body separator semantics around).
- **Register diff**: `register/shape/session-document-layout` is
  reconciled this cycle but its prose still describes the old
  heading shape — the rewrite is owned by
  `mark-superseded-interfaces-register-entries`. When updating the
  test fixtures here, do not re-cite the heading-shape invariants;
  cite the new "drawer at point-min + turn block(s)" shape directly
  via behavioral assertion.

