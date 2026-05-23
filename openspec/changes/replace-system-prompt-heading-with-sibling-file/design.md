## Context

The just-archived `gptel-drawer-as-source-of-truth` change made a late pivot in its Addendum (Finding B) to relocate the system prompt out of property-drawer values and into a visible `* System Prompt` heading body inside `session.org`, alongside a sibling `* Chat` heading. The motivation was sound — the original constraint ("long, multi-line, special-character strings are unwieldy as a single property *value*") is real — but the chosen escape hatch (org heading body) trades one set of org-mode interactions for another:

1. **Markdown content in an org buffer.** Preset system prompts in this repo are authored in markdown. Pasting markdown verbatim under an org heading interacts with org-mode fontification (emphasis, code highlighting), folding (`org-set-visibility-according-to-property`), and indent semantics. Documents render in ways that surprise the user; the prompt body looks broken even when the underlying text is intact. The "drawer-value italic span" workaround we shipped earlier (`fix-scope-drawer-value-emphasis`) is a hint of how deep these interactions go.

2. **Parser fragility.** The chat parser is heading-indifferent (§Decision 12 of the prior change): it locates turn blocks by `#+begin_*` markers, not headings. A `* System Prompt` body containing a literal column-0 `#+begin_user` is mis-parsed as a turn. The prior change recorded this as "accepted known limitation"; any future preset or AI-generated prompt that includes example chat-block syntax would silently corrupt the session.

3. **Recovery code complexity.** The `harden-system-prompt-save-against-missing-chat-heading` task ships intricate logic for: (a) detecting a missing `* Chat` heading, (b) bounding the body rewrite at the first turn marker so blind delete-region doesn't sweep turn blocks away, (c) re-materializing `* Chat` and re-attaching turn blocks. That complexity exists because heading-body editing and turn-block editing share the same document — small layout drift cascades.

The constraint set is small:

- The system prompt must NOT live in a drawer property value (the original objection).
- The system prompt must NOT live in `session.org`'s document body either (the new objection, surfaced after shipping).
- Edit experience must be reasonable for long prompts.
- The drawer remains the WYSIWYG source of truth for non-system configuration (`gptel-drawer-as-source-of-truth`'s contract stands).
- No data migration / no back-compat for the brief-lived `* System Prompt` shape.

## Goals / Non-Goals

**Goals:**

- A session's system prompt is a separate file in the session's branch directory, in whatever format the preset authored it (today: markdown).
- `session.org`'s configuration drawer carries an explicit `:GPTEL_SYSTEM_PROMPT_FILE:` property pointing at the sibling. Restore reads it; save preserves it; the file is canonical.
- Edits to the sibling file are picked up on the next request (pre-send refresh), not just on `gptel-chat-mode` activation.
- The chat-mode menu's "edit system prompt" affordance opens the sibling file in another window — the file is the source of truth, the menu routes the user to it.
- `session.org` reverts to the pre-Addendum layout: drawer + bare `#+begin_user` block. No `* System Prompt` heading, no `* Chat` heading.
- The drawer write-exclusion for `:GPTEL_SYSTEM:` continues to hold. The legacy drawer overlay's back-compat read of `:GPTEL_SYSTEM:` continues to hold (it becomes the back-compat middle tier in the precedence chain).

**Non-Goals:**

- Migrating existing on-disk sessions. Stray headings sit until the user deletes them; restore falls through to preset.
- `.org`-format preset support (extension-agnostic design, but enabling org presets is a separate scanner-widening change).
- A "reload system prompt now" command. Pre-send refresh covers the dynamic case; `revert-buffer` covers the static case.
- File-notify watcher on the sibling file. Pre-send refresh is sufficient and avoids the subsystem.
- Reworking the legacy `:GPTEL_SYSTEM:` drawer overlay. Orthogonal; it remains in place as the middle tier.
- A bespoke save-side writer for the sibling file. The file is canonical; the chat-mode buffer never writes back to it.

## Decisions

### Decision 1 — Sibling file lives in the session's branch directory, not under `~/.gptel/presets/`

**Choice**: For each session, create `system-prompt.<ext>` next to `session.org` (i.e., in the same branch directory). The drawer's `:GPTEL_SYSTEM_PROMPT_FILE:` property carries the basename and resolves relative to `(file-name-directory buffer-file-name)`.

**Alternatives considered:**

- *Keep it in `~/.gptel/presets/`*: the preset *is* the prompt; if the user edits the preset, that affects every future session that uses it. The sibling-file design exists precisely to make the prompt per-session-mutable without retroactively affecting other sessions.
- *Store in a central per-session prompts directory* (e.g., `~/.gptel/prompts/<session-id>.md`): adds an extra indirection layer, makes the prompt physically distant from the session. Defeats the "open the session folder and see what's there" mental model.
- *Embed as a base64-encoded drawer property* (`:GPTEL_SYSTEM_BASE64:`): solves the encoding problem but is opaque to the user. The whole point is that the prompt should be readable and editable.

**Rationale**: The branch directory already holds session-scoped state (`tools.org`, `session.log`, future `system-prompts.org` change log). One more sibling file fits the model. The user can `ls <session-dir>/branches/main/` and see the prompt file next to the chat.

### Decision 2 — Extension mirrors the preset's source file extension

**Choice**: When writing the sibling file at session creation, derive the extension from the preset's source file (look up `<jf/gptel-presets-directory>/<preset-name>.*`, take the matching extension). Today that always yields `.md` because the preset scanner registers only `*.md`. The design is extension-agnostic so `.org` presets, when supported, produce `system-prompt.org` siblings.

**Alternatives considered:**

- *Hardcode `.md`*: simpler today, but locks out future `.org` support without code changes. Cheap to avoid.
- *Track the source extension in the registered preset object*: would require widening `gptel-make-preset`'s plist with a `:source-file` key. Pollutes upstream's interface for our convenience.
- *Always emit `.org`*: defeats the goal of avoiding org-mode interactions on the prompt content.

**Rationale**: The look-up is one `directory-files` call against a small directory of known files. The convention "basename = preset name" already holds (it's how registration matches files to preset names). Defensive default to `"md"` when the lookup fails handles the "preset registered but file deleted" edge case without erroring.

### Decision 3 — File is canonical; save path never writes the sibling file

**Choice**: `gptel-chat--save-state` writes the drawer (full snapshot, unchanged) and does nothing with the sibling file. The sibling file is mutated only by direct user edits (typically via the "Edit system prompt" menu affordance that opens it). The chat-mode buffer's `gptel--system-message` is a per-request cache, refreshed before each send (Decision 4).

**Alternatives considered:**

- *Save writes the sibling file when `gptel--system-message` differs from the file*: makes the buffer the source of truth, which re-introduces the encoding pain (now `gptel--system-message` mutations go through whatever editing surface the user happens to use). Defeats the rationale for moving the prompt out of the buffer.
- *Hybrid — initial creation writes, then file is canonical, then "Reset to preset" menu affordance to rewrite from preset*: a reset affordance might be useful in the future. Not in this change; the user can delete the sibling file and reopen the session, and the preset's `:system` will be used.

**Rationale**: Making the file canonical is the simplest contract. The buffer's `gptel--system-message` is read-only state from the chat-mode buffer's perspective — modifications happen by editing the file. This removes a whole class of "did the buffer or the file win on save?" race conditions.

### Decision 4 — Pre-send refresh, not file-notify

**Choice**: Before each chat request is dispatched, re-read the sibling file (when `:GPTEL_SYSTEM_PROMPT_FILE:` is set and the file exists). Buffer-local `gptel--system-message` is refreshed from the file's current on-disk contents. Implementation: filter `gptel-request` (or upstream's send hook, if available) by `(derived-mode-p 'gptel-chat-mode)` and call `gptel-chat--refresh-system-prompt-from-file` as a `:before` side-effect.

**Alternatives considered:**

- *Activation-time only*: the user has to `revert-buffer` `session.org` or close/reopen it to pick up sibling-file edits. Inconvenient when the user is iterating on the prompt mid-session.
- *`file-notify` watcher on the sibling file*: more automatic but adds a subsystem with lifecycle bookkeeping (watcher per chat-mode buffer; cleanup on `kill-buffer-hook`). Pre-send refresh delivers the same outcome with one synchronous read per request.
- *Re-read on every interactive command*: too aggressive; reads on every keystroke menu interaction would be wasted I/O.
- *Add a "reload system prompt" command*: requires the user to remember it. Pre-send refresh is automatic and doesn't preclude adding a reload command later.

**Rationale**: A chat request is the only point where the system prompt actually matters. One file read per request is negligible (file is small, OS cache is warm). The advice is narrow (`:before`, filtered to chat-mode buffers) so non-chat-mode `gptel-request` calls pay zero cost.

### Decision 5 — Menu affordance opens the file in another window; replaces upstream `gptel-system-prompt` infix

**Choice**: The chat-mode transient menu (the `gptel-chat-menu` fork) replaces the upstream `gptel-system-prompt` infix entry with `gptel-chat--edit-system-prompt-file`. This new suffix resolves the sibling file (or prompts to create one), then `find-file-other-window`s it.

**Alternatives considered:**

- *Keep the upstream infix and add a separate "Edit system prompt file" suffix*: both affordances would coexist. The upstream infix would silently bypass the file (mutating only `gptel--system-message`, which the pre-send refresh would then overwrite on the next send). Confusing and racy.
- *Override upstream `gptel-system-prompt` globally*: pollutes non-chat-mode buffers. The chat-mode menu is a fork; the override stays local.
- *Open the file in the current window*: loses the session.org buffer. Other-window keeps both visible side-by-side, which matches the workflow.

**Rationale**: One affordance, one mental model. The user knows the file is the source of truth because the menu takes them there. The upstream infix remains intact for non-chat-mode contexts.

### Decision 6 — Legacy `:GPTEL_SYSTEM:` drawer overlay stays as middle tier

**Choice**: The existing drawer overlay (`gptel-chat--apply-drawer-overrides`) still honors a `:GPTEL_SYSTEM:` drawer entry when present. It becomes the back-compat middle tier in the restore precedence: sibling file > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`.

**Alternatives considered:**

- *Strip the legacy overlay entirely*: cleaner code, but breaks back-compat for the small number of users who hand-authored `:GPTEL_SYSTEM:` in their drawers under the original `gptel-chat-state-persistence` design. Cost of keeping the overlay is near zero (a few lines of read-side logic); cost of breaking back-compat is real.
- *Mark `:GPTEL_SYSTEM:` drawer entries as deprecated with a warning*: would surface the deprecation without fixing it. The user would see a warning every time they opened an affected session. Annoying for an asymmetric back-compat shim.

**Rationale**: The legacy overlay is orthogonal to this change. It was added for back-compat under the prior design and continues to function for that audience. We are not regressing anything.

### Decision 7 — Heading layout deletion is wholesale; no in-between state

**Choice**: Delete `jf/gptel--session-headings-block` and all chat-mode heading reader/writer code in one change. Do not introduce a "headings deprecated but still emitted" intermediate state.

**Alternatives considered:**

- *Two-phase deletion (mark deprecated, then delete in a follow-up)*: the headings were live for one cycle. There is no historical audience to ease off; the prior change shipped and the next one undoes it. A staging period would just leave dead code in the tree for no benefit.

**Rationale**: The heading shape was a brief detour. Single-change deletion keeps the diff and the spec deltas focused.

### Decision 8 — No migration for existing on-disk sessions

**Choice**: Sessions created under the `* System Prompt` / `* Chat` layout are not auto-migrated. On open under the new code, the headings sit as stray content in the buffer; the chat-mode restore looks for `:GPTEL_SYSTEM_PROMPT_FILE:` (which is absent), then the legacy drawer overlay (which is absent), then falls through to the preset. The user can delete the stray headings by hand.

**Alternatives considered:**

- *Auto-migration on open*: would mutate the file on read, which violates "no save without user action." Surprising and irreversible if the user wanted to keep the headings.
- *Migration command (`M-x jf/gptel-migrate-session-headings`)*: useful but adds surface area. Defer; users can simply delete the headings themselves.
- *Detect heading + warn*: noisy. Most users won't have these old-shape sessions for long.

**Rationale**: Symmetric with the prior change's "no migration; first-save expands the drawer" pattern. The graceful degrade is real (preset fallback works); the visual annoyance is bounded (a few headings the user removes once).

## Risks / Trade-offs

**[Risk] Pre-send refresh adds an `insert-file-contents` per chat request.**
→ Mitigation: file is small (single KB at most), OS cache is warm after the first read. The refresh is unconditional only when the property is set; sessions with no sibling file pay zero cost. If a future profile reveals this is a bottleneck (unlikely), the refresh can be gated on `file-attribute-modification-time` change.

**[Risk] The "Edit system prompt" affordance creates a new file when the property is unset, which changes the session state silently.**
→ Mitigation: the affordance prompts the user for a filename (defaulting to `system-prompt.md`). Confirmation is required. The file is created empty and opened immediately; the user sees what just happened.

**[Risk] `revert-buffer` of `session.org` re-triggers chat-mode restore, which reads the sibling file. If the user edited the buffer-local `gptel--system-message` via `M-x gptel-system-prompt` (upstream) without editing the file, that edit is wiped.**
→ Mitigation: this is the intended behavior. The file is canonical; transient buffer edits should not survive a restore. We replace the chat-mode menu's `gptel-system-prompt` infix specifically to discourage this pattern; users who insist on the upstream `M-x gptel-system-prompt` are choosing the override-with-no-persistence path knowingly.

**[Risk] Existing sessions with `* System Prompt` / `* Chat` headings will look broken to users who open them after the change.**
→ Mitigation: documented in the proposal. The headings are inert; functionality (preset fallback) is preserved. A user who wants the cleaner layout deletes the headings themselves. We could add a one-liner in the chat-mode buffer message ("Stray heading detected; this session predates the sibling-file layout") but that's noise for a transient population.

**[Risk] Pre-send refresh path is wired via `advice-add` if upstream lacks a clean pre-send hook. Advice is a coupling point.**
→ Mitigation: investigate first; if upstream offers a `gptel-pre-send-hook`, use it. Advice is `:before`, side-effect-only, filtered to chat-mode buffers — about as narrow as an advice can be. Removable with one `advice-remove`. Document in the implementation that this is a chat-mode-specific advice on a chat-mode predicate.

**[Risk] Two file-existence checks per restore (one in `apply-system-prompt-file`, one in `refresh-system-prompt-from-file`) introduce a TOCTOU window.**
→ Mitigation: the consequences of a race are bounded — file existed at restore but is deleted before send means the cache is stale; the request goes out with the cached value (last-known-good). Not a correctness issue. The opposite case (file did not exist at restore, exists at send) means the refresh installs it. Both behaviors are fine.

**[Risk] Pre-send refresh could fail silently if the file was made unreadable between restore and send (e.g., permissions changed).**
→ Mitigation: log a warning via `jf/gptel--log 'warn ...` when the file is set in the drawer but unreadable at refresh time. The request still goes out with the cached value.

## Migration Plan

No data migration. See Decision 8.

- **Pre-existing session files with `* System Prompt` / `* Chat` headings**: load normally. The chat-mode restore looks for `:GPTEL_SYSTEM_PROMPT_FILE:` (absent in old-shape sessions), then the legacy drawer overlay, then the preset. Functionality is preserved. The headings sit in the buffer as stray content; the user can delete them.
- **Pre-existing sessions with a legacy `:GPTEL_SYSTEM:` drawer entry (pre-Addendum, pre-this-change)**: the drawer overlay still honors them. Behavior unchanged for that population.
- **Brand-new sessions under the new code**: get the new layout (drawer + bare user block + sibling file).
- **Rollback**: revert the touched commits. The next session creation reverts to the heading layout. Existing sessions with sibling files keep working (the file remains; the drawer property remains; the prior code did not read either, so the prompt comes from the preset until the sibling file is removed). Mild back-compat asymmetry, but no data loss.

## Open Questions

1. **Does upstream `gptel` already expose a pre-send hook we can use, or do we need to advise `gptel-request` / `gptel-send`?** Investigate during implementation. If upstream offers a hook, prefer it. If not, narrow `:before` advice with a chat-mode predicate is acceptable.

2. **Should the "Edit system prompt" affordance be a transient suffix (the menu opens, you press the key, the menu closes and the file opens) or a regular command bound to a chat-mode key?** Transient suffix is consistent with the rest of the chat-mode menu. A bound key would be more direct but doubles the surface area. Default to transient suffix; we can add a bound key later if there's demand.

3. **Should we add `:GPTEL_SYSTEM_PROMPT_FILE:` to the list of keys read by the existing `gptel-chat--apply-drawer-overrides` overlay, or keep its handling in a separate `gptel-chat--apply-system-prompt-file` function?** Keep it separate. The overlay maps drawer keys to upstream `gptel--` variables one-for-one; the sibling-file resolution is an indirection that doesn't fit that pattern. Separate function is cleaner.

4. **When the user moves a session directory (e.g., renames or relocates), the relative `:GPTEL_SYSTEM_PROMPT_FILE:` resolution still works (it resolves relative to `session.org`'s current location). Should we document this explicitly in the spec?** Yes — add a brief note to the restore requirement that resolution is relative to `(file-name-directory buffer-file-name)`, not the original creation directory. Already covered in the spec; double-check during review.
