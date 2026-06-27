## Why

The just-archived `gptel-drawer-as-source-of-truth` change made a late pivot in its Addendum (Finding B) to relocate the system prompt out of property-drawer values and into a visible `* System Prompt` heading body inside `session.org`, alongside a sibling `* Chat` heading. The motivation was sound — long, multi-line, special-character system prompts (with markdown, code blocks, XML-like tags) are unwieldy as a single property *value* — but the chosen escape hatch (an org heading body) trades one set of org-mode interactions for another:

- Preset system prompts in this repo are authored in markdown. Pasting markdown verbatim under an org heading interacts badly with org-mode fontification, folding, and emphasis parsing. Documents render in ways that surprise the user; the `* System Prompt` body looks broken even when the underlying text is intact.
- The parser is heading-indifferent (chat-mode parser §Decision 12), so a system-prompt body containing a literal column-0 `#+begin_user` gets mis-parsed as a turn. The previous change recorded this as an accepted known limitation, but any future user-authored or AI-generated prompt that includes example chat-block syntax becomes a latent corruption vector.
- The save-path materialiser for missing `* Chat` headings (`harden-system-prompt-save-against-missing-chat-heading`) is intricate precisely because heading-body editing and turn-block editing live in the same document; small layout drift cascades into recovery code.

The right shape is to take the system prompt *out of `session.org` entirely* and keep it as a sibling file in the session directory, in whatever format the preset authored it in. A markdown file is rendered, edited, and reasoned about as markdown — there is no org-mode interaction to mangle it. The drawer in `session.org` carries an explicit link (`:GPTEL_SYSTEM_PROMPT_FILE:`) to the sibling, restoring the pre-Addendum session shape (config drawer + bare `#+begin_user` block) while making the system prompt a first-class file the user edits directly.

## What Changes

### Revert (Addendum Finding B of `gptel-drawer-as-source-of-truth`)

- **BREAKING (Decision B reversal)**: `session.org` no longer contains a `* System Prompt` heading. The heading and its `:VISIBILITY: folded` properties drawer are removed from the canonical layout.
- **BREAKING**: The `* Chat` heading is also removed. It existed only as a sibling to `* System Prompt`; with the prompt heading gone, `* Chat` has no peer to differentiate it from. The pre-Addendum layout — config drawer at `point-min`, then turn blocks directly — is restored.
- Delete `jf/gptel--session-headings-block` (`config/gptel/sessions/commands.org`). Restore `jf/gptel--initial-session-body` to its pre-Addendum signature and shape (bare drawer + empty `#+begin_user` block; no `system-prompt` argument threading).
- Delete the chat-mode heading reader/writer surface (`config/gptel/chat/menu.org`):
  - Restore-side: `gptel-chat--system-prompt-heading-body-region`, `gptel-chat--system-prompt-heading-body`, `gptel-chat--apply-system-prompt-heading`
  - Save-side: `gptel-chat--write-system-prompt-heading`, `gptel-chat--config-drawer-end`, `gptel-chat--turn-block-marker-re`
- Unwire the call sites: `gptel-chat--apply-declared-preset` no longer calls `apply-system-prompt-heading` as its last step; `gptel-chat--save-state` no longer calls `write-system-prompt-heading` after the drawer write.
- Strip the `:VISIBILITY: folded`-on-`* System Prompt` startup wiring (the per-buffer hook that enables `org-set-visibility-according-to-property` for chat-mode).
- Delete heading-specific tests under `config/gptel/chat/test/menu/` and `config/gptel/sessions/test/commands/`. The parser regression "single user turn even when preset has markdown system prompt" stays — it pins the heading-indifferent parser contract independently and remains a useful future-proofing assertion.
- Reconcile interfaces register entries (`register/shape/session-document-layout`, `register/invariant/system-prompt-heading-authoritative`): mark superseded by this change with a pointer to the new sibling-file shape.

### Add (sibling-file system prompt)

- **NEW canonical layout**:
  ```
  <session-dir>/branches/<branch>/
  ├── session.org
  │     :PROPERTIES:
  │     :GPTEL_PRESET: executor
  │     :GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md
  │     :GPTEL_MODEL: ...
  │     :END:
  │     #+begin_user
  │     ...
  └── system-prompt.md
        <preset :system body, verbatim, no frontmatter>
  ```
- **NEW drawer key**: `:GPTEL_SYSTEM_PROMPT_FILE:` — a path (typically a basename) resolved relative to the directory containing `session.org`. Written by session creation, read by chat-mode restore, never deleted by the save path.
- **NEW** session-creation step in `jf/gptel--create-session-core`: resolve the preset's source file path (basename + `jf/gptel-presets-directory`) to derive the extension; write `system-prompt.<ext>` into the session's branch directory with the preset's `:system` body verbatim; emit `:GPTEL_SYSTEM_PROMPT_FILE:` into the config drawer. When the preset has no `:system`, skip both the sibling file and the property — chat-mode restore will fall through to the preset's empty system prompt.
- **NEW** chat-mode restore step (last in `gptel-chat--apply-declared-preset`): read `:GPTEL_SYSTEM_PROMPT_FILE:` from the drawer; resolve relative to `(file-name-directory buffer-file-name)`; when the file exists and is non-empty, install its contents as buffer-local `gptel--system-message`. Missing file or missing property leaves `gptel--system-message` at whatever the preset / drawer overlay already installed.
- **NEW** pre-send refresh: every chat request re-reads the sibling file before dispatch, so a user edit to `system-prompt.md` is picked up without explicitly reopening `session.org`. Buffer-local `gptel--system-message` becomes a per-request cache, not durable state. (Wired into the request submission path, not the buffer-activation hook.)
- **CHANGED** save path: `gptel-chat--save-state` is unchanged for the upstream-compatible drawer keys (full snapshot still emitted) and continues to never emit `:GPTEL_SYSTEM:`. It does **not** write back to the sibling file — the file is canonical; the buffer never overrides it.
- **CHANGED** menu UX: the chat-mode menu entry that today routes to upstream's `gptel-system-prompt` (minibuffer-editing `gptel--system-message`) is replaced with an "Edit system prompt" entry that `find-file-other-window`s the sibling file. When `:GPTEL_SYSTEM_PROMPT_FILE:` is unset (e.g., a session created from a preset with no `:system`), the entry prompts for a filename (defaulting to `system-prompt.md`), writes the property, creates the file, and opens it.

### Out of scope (deferred)

- `.org`-format preset support. Today `jf/gptel-preset-register-all` scans for `\\.md\\'` only; supporting `.org` presets would mean widening the scanner regex and adding an org-frontmatter parser. The sibling-file design is extension-agnostic and ready for `.org`, but enabling preset authoring in `.org` is a separate change.
- A migration command for existing sessions on disk. See "No migration" below.

## Capabilities

### New Capabilities

_None — this change reshapes existing capabilities, it does not add new ones._

### Modified Capabilities

- `gptel/chat-mode`: restore precedence collapses from three tiers ("`* System Prompt` heading body > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`") to two ("sibling file > preset `:system`"). The legacy `:GPTEL_SYSTEM:` drawer entry remains a read-time fallback via the existing overlay (back-compat for hand-authored entries), but the heading tier is gone. Save path drops the heading writer. The menu's edit-system-prompt affordance routes to a file rather than the minibuffer.
- `gptel/sessions-persistence`: session creation no longer emits `* System Prompt` or `* Chat` headings. It does emit a new sibling `system-prompt.<ext>` file and the `:GPTEL_SYSTEM_PROMPT_FILE:` drawer key when the preset has a non-empty `:system`. The pre-Addendum layout (drawer + bare turn block) is restored, plus the one new drawer key.

## Impact

**Code (delete)**:
- `config/gptel/sessions/commands.org` / `.el` — remove `jf/gptel--session-headings-block`; restore `jf/gptel--initial-session-body` to its pre-Addendum signature.
- `config/gptel/chat/menu.org` / `.el` — remove `gptel-chat--system-prompt-heading-body-region`, `gptel-chat--system-prompt-heading-body`, `gptel-chat--apply-system-prompt-heading`, `gptel-chat--write-system-prompt-heading`, `gptel-chat--config-drawer-end`, `gptel-chat--turn-block-marker-re`. Unwire from `gptel-chat--apply-declared-preset` and `gptel-chat--save-state`.
- `config/gptel/chat/mode.org` / `.el` (if the `:VISIBILITY: folded` startup wiring lives there) — remove the hook that activates `org-set-visibility-according-to-property` for chat-mode buffers.

**Code (modify)**:
- `config/gptel/sessions/commands.org` / `.el` — `jf/gptel--create-session-core` gains the sibling-file writer step (resolve preset path → derive extension → write `system-prompt.<ext>` → put `:GPTEL_SYSTEM_PROMPT_FILE:`).
- `config/gptel/chat/menu.org` / `.el` — `gptel-chat--apply-declared-preset` gains a new last-step `gptel-chat--apply-system-prompt-file` (reads sibling file, installs buffer-local `gptel--system-message`). Pre-send refresh hook is wired into the chat request submission path.
- `config/gptel/chat/menu.org` / `.el` (or wherever the chat-mode transient menu is defined) — replace the upstream `gptel-system-prompt` infix with an "Edit system prompt" command that `find-file-other-window`s the sibling file.

**Tests (rewrite)**:
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` — assert the new layout: drawer at `point-min` with `:GPTEL_SYSTEM_PROMPT_FILE:`, no `* System Prompt` heading, no `* Chat` heading. Assert the sibling file exists with the preset's `:system` content verbatim.
- `config/gptel/sessions/test/commands/preset-application-spec.el` — assert the sibling file is written; assert no heading is emitted.
- `config/gptel/chat/test/menu/preset-wiring-spec.el` — restore precedence is "sibling file > preset". Test sibling-file present / sibling-file empty / property missing / file missing. Drop heading-tier scenarios.
- `config/gptel/chat/test/menu/save-state-spec.el` — assert save never writes the sibling file; drawer write still emits the full snapshot minus `:GPTEL_SYSTEM:`.
- New: pre-send refresh test — modify sibling file on disk, simulate a request submission, assert `gptel--system-message` reflects the new content.
- New: menu-affordance test — invoke the edit-system-prompt entry, assert the right file is opened (or created and opened when absent).

**Tests (delete)**:
- `config/gptel/chat/test/menu/preset-wiring-spec.el` heading-restore scenarios (`scaffolding/invariants/system-prompt-heading-authoritative.test.el` and any related specs).
- `config/gptel/chat/test/menu/save-state-spec.el` heading-materialise scenarios.

**Tests (keep)**:
- The parser regression "single user turn even when preset has markdown system prompt" stays — it pins heading-indifference independently and protects against a future regression even when the heading is gone today.
- `register/invariant/drawer-system-key-write-exclusion` continues to hold: no `:GPTEL_SYSTEM:` writes.

**Specs (revise)**:
- `openspec/specs/gptel/chat-mode.md` — system-prompt restore precedence collapses; menu edit affordance is file-based.
- `openspec/specs/gptel/sessions-persistence.md` — session-creation shape reverts to pre-Addendum drawer + turn block, plus the new `:GPTEL_SYSTEM_PROMPT_FILE:` property and sibling-file emission.
- `register/shape/session-document-layout` — superseded; documents the new (simpler) shape.
- `register/invariant/system-prompt-heading-authoritative` — superseded by the file-as-source-of-truth contract; mark removed, point to new contract.

**Migration**:
- **No migration.** Sessions on disk created under the old `* System Prompt` / `* Chat` shape will, after this change, have stray heading content in `session.org`. The user can delete those headings by hand. Graceful degrade: the sibling file is absent, so chat-mode falls back to the preset's `:system` (same behavior as a pre-Addendum session).
- The legacy `:GPTEL_SYSTEM:` drawer overlay (back-compat for hand-authored drawer entries) remains in place — it's orthogonal to this change and continues to function for the small number of users who ever used it.

**No changes to**:
- The drawer-write contract for non-`:system` keys (full snapshot still written on every save).
- `:GPTEL_BOUNDS:` exclusion.
- `gptel-mode` non-enablement in chat-mode buffers.
- Block-based chat format, scope validator pipeline, registry, branching layout, `branch-metadata.yml`.
- Preset registration / parsing (other than the read-side of "what's this preset's source file extension?", which is derivable from `jf/gptel-presets-directory` + preset name).

**Dependencies**:
- Builds on archived `gptel-drawer-as-source-of-truth` (and its Addendum, which this change partially reverts).
- No upstream gptel coordination required.
