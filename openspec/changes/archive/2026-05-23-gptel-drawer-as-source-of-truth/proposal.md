## Why

The recently-landed `gptel-chat-state-persistence` change made `session.org`'s `:PROPERTIES:` drawer the durable home for chat configuration, but it kept upstream's *delta-from-preset* save semantics: properties whose buffer-local values match the active preset are deleted on every save. The visible consequence is that fresh sessions never display `:GPTEL_TOOLS:`, `:GPTEL_MODEL:`, or other preset-derived keys — the user can't see what's active without reading the preset file. A second consequence is that `gptel-menu`'s "Select tools" defaults to *global* scope, which kills the buffer-local `gptel-tools` binding and leaks the change to other buffers; in a chat-mode session that means menu edits don't reliably round-trip through the drawer.

The intent of moving scope and preset state into the drawer was to make `session.org` a single, WYSIWYG source of truth — open the file and see exactly what configuration is active. The delta-from-preset model defeats that goal. We want the drawer to carry the **full** configuration snapshot at session creation and on every save, with the system prompt as the only deliberate exception (long, multi-line, special-character-heavy strings are unwieldy as a single property value and stay in the preset file).

## What Changes

- **BREAKING (Decision 1 reversal of `gptel-chat-state-persistence`)**: `gptel-chat--save-state` SHALL NOT delegate to upstream `gptel-org-set-properties`. Instead it writes the full upstream-compatible drawer key set (`GPTEL_PRESET`, `GPTEL_MODEL`, `GPTEL_BACKEND`, `GPTEL_TOOLS`, `GPTEL_TEMPERATURE`, `GPTEL_MAX_TOKENS`, `GPTEL_NUM_MESSAGES_TO_SEND`) from current buffer-local state on every save. No delta-from-preset deletion. `:GPTEL_SYSTEM:` is intentionally excluded from the drawer (see below). `:GPTEL_BOUNDS:` remains excluded as before. `:GPTEL_PARENT_SESSION_ID:` is written when set, as today.
- **NEW**: At session creation, the drawer is pre-populated with the full preset snapshot — model, backend, tools, temperature, max-tokens, num-messages-to-send (whatever the preset declares non-nil) — alongside `:GPTEL_PRESET:` and the existing `:GPTEL_SCOPE_*:` keys. `:GPTEL_SYSTEM:` is **not** written; the system prompt is read from the preset file at mode activation.
- **BREAKING (system-prompt exception)**: `:GPTEL_SYSTEM:` is no longer part of the drawer write set. It is read from the active preset's `:system` key at mode activation and installed buffer-locally. If a user authored `:GPTEL_SYSTEM:` in the drawer manually, the read-time overlay still respects it (back-compat); the writer just never emits it.
- **BREAKING (Decision 5 refinement of `gptel-chat-state-persistence`)**: On mode activation, the drawer is the source of truth for everything except `:system`. Restore order: apply preset (for `:system` and as a fallback baseline) → overlay drawer values for all other keys. The drawer overlay no longer functions as "non-preset deltas only" — every drawer-present key wins over the preset value.
- **BREAKING (gptel-menu scope default)**: When `gptel-chat-menu` enters, it sets `gptel--set-buffer-locally` to `t` for the lifetime of the menu, so tool / model / temperature / etc. selections from the chat menu apply buffer-locally. A subsequent `save-buffer` then serializes those buffer-local values to the drawer. Upstream `gptel-menu` (invoked outside chat-mode buffers) is unaffected.
- `jf/gptel-scope-profile--render-drawer-text` (and through it `--create-for-session`) accepts the resolved preset spec and emits the full snapshot. The function name and shape of `register/shape/drawer-text-block` evolve to include the new keys.
- Spec text in `openspec/specs/gptel/chat-mode.md` and `openspec/specs/gptel/sessions-persistence.md` is updated: drawer is "full state minus `:GPTEL_SYSTEM:`"; restore is "preset for system, drawer for everything else"; save is "full snapshot, no delta logic".

## Capabilities

### New Capabilities

_None — this change reshapes existing capabilities, it does not add new ones._

### Modified Capabilities

- `gptel/chat-mode`: save path changes from "delta from preset" to "full snapshot, system excluded". Restore overlay changes from "non-preset deltas only" to "drawer wins over preset for every key except `:system`". `gptel-chat-menu` defaults configuration scope to buffer-local.
- `gptel/sessions-persistence`: session creation drawer expands from `:GPTEL_PRESET:` (+ optional `:GPTEL_PARENT_SESSION_ID:`) + `:GPTEL_SCOPE_*:` to a full preset snapshot (model, backend, tools, temperature, max-tokens, num-messages-to-send) on the same drawer. `:GPTEL_SYSTEM:` is deliberately excluded.
- `gptel/scope` (drawer renderer only): `jf/gptel-scope-profile--render-drawer-text` and `--create-for-session` accept and emit the upstream-compatible config keys in addition to the existing scope keys. The scope-key shape is unchanged.

## Impact

**Code (modify)**:
- `config/gptel/chat/menu.org` / `menu.el` — replace `gptel-org-set-properties` call in `gptel-chat--save-state` with a chat-mode full-snapshot writer; adjust `gptel-chat--apply-drawer-overrides` if the existing "every drawer-present key wins" behavior needs an explicit comment update; wire `gptel--set-buffer-locally = t` into `gptel-chat-menu` entry.
- `config/gptel/scope-profiles.org` / `scope-profiles.el` — `jf/gptel-scope-profile--render-drawer-text` accepts the preset spec and emits the full upstream-compat key set (excluding `:system`); `--create-for-session` and `--apply-to-drawer` follow.
- `config/gptel/sessions/commands.org` / `commands.el` — `jf/gptel--initial-session-content` either gains preset-snapshot rendering or is replaced by a call into the scope-profiles renderer (single source of truth for drawer text).
- `openspec/specs/gptel/chat-mode.md` — revise "Save path writes deltas" → "Save path writes full snapshot, system excluded"; revise "Configuration drawer overlay on restore" to clarify drawer wins over preset for every key except `:system`.
- `openspec/specs/gptel/sessions-persistence.md` — revise the session-creation drawer shape requirement to the full snapshot.

**Tests (rewrite)**:
- `config/gptel/chat/test/menu/save-state-spec.el` — the unit specs that spy on `gptel-org-set-properties` and assert call shape are replaced with assertions on actual drawer text after save (full set present, `:GPTEL_SYSTEM:` absent, `:GPTEL_BOUNDS:` absent). Integration spec updated to assert no delta deletion when buffer state matches preset.
- `config/gptel/chat/test/menu/preset-wiring-spec.el` — overlay scenarios remain valid (drawer-overlay-wins is the new contract); add a scenario verifying `:GPTEL_MODEL:` from the drawer wins over the preset's model.
- `config/gptel/sessions/test/commands/preset-application-spec.el` — assertion that fresh sessions show the preset's tools / model in the drawer.
- `config/gptel/sessions/test/commands/session-org-creation-spec.el` (and the older `config/gptel/test/session-creation-spec.el` if still in use) — assert the expanded drawer shape on creation.
- `config/gptel/scope/test/expansion/...` — no expected change (scope keys unchanged); confirm idempotency invariant still holds with the new render shape.

**Migration**:
- No migration for existing session files. Sessions created under the old shape (`:GPTEL_PRESET:` only) continue to work — the restore path overlays whatever the drawer has and falls back to the preset for missing keys. On first save under the new code, the drawer expands to the full snapshot.
- The `gptel-chat-state-persistence` design.md's open question #1 ("reset to preset" affordance) becomes moot: the drawer is the SoT, so "reset" means deleting drawer keys and re-saving (which re-emits from the just-applied preset). No new menu affordance is required by this change.

**No changes to**:
- `:GPTEL_BOUNDS:` exclusion (still never written).
- `gptel-mode` enablement (still disabled in chat-mode buffers).
- Block-based chat format, scope validator pipeline, registry, branching layout, `branch-metadata.yml`.
- Upstream `gptel-mode` / `gptel-menu` behavior outside chat-mode buffers.

**Dependencies**:
- Builds on archived `gptel-chat-state-persistence` (drawer save/restore infrastructure) and `gptel-scope-in-org-properties` (drawer is the scope home). Both shipped in cycles 1–4 of the recent orchestration.
- No upstream gptel coordination required — the change is entirely on our save path. We continue to use `gptel-org--entry-properties` as the drawer reader.
