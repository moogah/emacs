## Context

The just-archived `gptel-chat-state-persistence` change wired `gptel-chat-mode` into upstream's `:PROPERTIES:` drawer save/restore machinery and removed the `metadata.yml` sidecar. Decision 1 of that change explicitly chose to delegate the chat-mode save hook to upstream's `gptel-org-set-properties`:

> Calling the narrower helper gives upstream semantics for free without the one line we don't want.

That call site uses **delta-from-preset** semantics: every property whose buffer-local value matches the active preset's spec is `org-entry-delete`d from the drawer on every save. The intent was to keep the drawer minimal, mirroring upstream `gptel-mode` files where presets handle the bulk of configuration and the drawer carries only deviations.

In practice this design defeats the goal that motivated the recent gptel work — making `session.org` a single, WYSIWYG source of truth. Concrete symptoms reported in the field:

1. Fresh sessions show only `:GPTEL_PRESET:` and `:GPTEL_SCOPE_*:` in the drawer. There is no way to know which model, which tools, or which temperature is active without reading the preset file separately.
2. The user clarified the design intent: "the goal of the properties drawer is that the session.org file itself is the source of truth and 'WYSIWYG'. The only necessary exception is the system prompt and that is because long prompts with special characters are unwieldy to put in a property."
3. `gptel-menu`'s "Select tools" defaults to `gptel--set-buffer-locally = nil`, which means a tool toggle from the menu in a chat-mode buffer **kills** any buffer-local `gptel-tools` binding installed by the preset and writes the new value to the global `gptel-tools`. The drawer save then sees the global value (or no value, depending on order). This is invisible to the user — they think they're editing the buffer, but they're editing globally.

The constraint set is small but firm:

- The drawer must NOT carry `:GPTEL_SYSTEM:` (long, multi-line, special-character payload — system prompts contain backticks, asterisks, code samples; encoding them as a single property value mangles them and is unreadable).
- The drawer must NOT carry `:GPTEL_BOUNDS:` (preserved from prior decision; incompatible with chat-mode's block format).
- Behavior change must be local to chat-mode buffers. Upstream `gptel-menu` and `gptel-mode` are not modified.
- Existing in-flight `gptel-chat-state-persistence` task `regression-sweep-step-8-agent-verification` (currently `blocked`) is not affected — it's a verification task, and the new write contract still produces a valid drawer. We do not need to wait for it.

## Goals / Non-Goals

**Goals:**

- After session creation, `session.org`'s drawer carries a full snapshot of the configuration the chat is using (model, backend, tools, temperature, max-tokens, num-messages-to-send) plus preset reference, parent-session-id (when applicable), and scope keys.
- After every save, the drawer carries a full snapshot of current buffer-local state for the same key set. No delta-from-preset deletion. The drawer reflects what the buffer is doing right now.
- `:GPTEL_SYSTEM:` is excluded from the writer entirely. It comes from the preset on restore. If a user manually authors `:GPTEL_SYSTEM:` in the drawer, the read-time overlay still respects it (back-compat) but the writer never round-trips it.
- `gptel-chat-menu` invocations (the chat-mode binding, not upstream's `gptel-menu`) default the configuration scope to buffer-local. Tool / model / temperature / etc. toggles from the chat menu apply to the current buffer and serialize to the drawer on save.
- Existing sessions (created before this change, with the minimal old drawer) continue to load. On first save under the new code, the drawer expands to the full snapshot. No migration command required.

**Non-Goals:**

- Persisting `:GPTEL_SYSTEM:` from the writer. Out of scope. (Read-side back-compat for manual entries is the only concession.)
- Persisting `:GPTEL_BOUNDS:`. Still excluded.
- Modifying upstream `gptel-org-set-properties` or `gptel-org--entry-properties`. We use the upstream reader as-is; we replace the upstream writer with our own.
- Modifying upstream `gptel-menu` (the global command). Only `gptel-chat-menu` defaults change.
- A "reset to preset" affordance. With the drawer as full snapshot, "reset" means deleting drawer keys and re-saving — no new menu surface required by this change.
- Migrating already-existing on-disk session files to the new drawer shape proactively. They expand on first save; no command added.
- Recursively re-emitting the snapshot on every typing event or tool result. Only `before-save-hook` triggers the writer.

## Decisions

### Decision 1 — Replace `gptel-org-set-properties` with a chat-mode full-snapshot writer

**Choice**: Define `gptel-chat--write-config-drawer` (or fold logic into `gptel-chat--save-state` directly) that uses `org-entry-put` and `org-entry-put-multivalued-property` to emit the full upstream-compatible key set from current buffer state, with no delta logic. Stop calling `gptel-org-set-properties`.

**Alternatives considered:**

- *Wrap `gptel-org-set-properties` and re-write deletes back*: parses the drawer twice and races with org-entry-* internals. Brittle.
- *Patch upstream to support a `force-write-all` flag*: requires upstream coordination, blocks landing this change.
- *Keep `gptel-org-set-properties` and re-write the snapshot after it returns*: same drawer is touched twice per save; the delete-then-put dance produces visible churn (`org-entry-delete` rewrites `:END:` lines, `org-entry-put` inserts new ones), which can interact badly with the drawer-singleton invariant the recent `add-drawer-corruption-regression` task tightened.

**Rationale**: A dedicated writer is the smallest surface that gives the WYSIWYG contract. The upstream helper is ~50 lines; our writer is the same shape minus the delta `if`-branches and minus the `:GPTEL_SYSTEM:` block. Behavior is easier to spec and easier to test (no preset spec needed in the test harness).

### Decision 2 — `:GPTEL_SYSTEM:` exclusion is in the writer, not the renderer or the overlay

**Choice**: The chat-mode save hook never emits `:GPTEL_SYSTEM:`. The session-creation drawer renderer never emits `:GPTEL_SYSTEM:`. The drawer overlay (Requirement: Configuration drawer overlay on restore) DOES read `:GPTEL_SYSTEM:` from the drawer when present and overlay it buffer-locally — this preserves back-compat for any user-authored entry and matches upstream's restore reader (`gptel-org--entry-properties` returns the system field; we cannot opt out cheaply).

**Alternatives considered:**

- *Strip `:GPTEL_SYSTEM:` from the overlay too*: forfeits back-compat. Any session whose drawer was edited by hand to override the system prompt silently stops working.
- *Require the user to put their custom system prompt in a custom preset*: pushes friction onto the user. The drawer is supposed to be authoritative; making one key non-overridable breaks that contract.

**Rationale**: The user's stated constraint is about the *write* path ("unwieldy to put in a property"). The read path is asymmetric on purpose: writes never produce a `:GPTEL_SYSTEM:` line, but reads honor whatever the user puts there. Practical implication: the system prompt typically rides the preset, but the drawer remains an escape hatch.

### Decision 3 — Drawer wins over preset for every key it carries (no "delta" semantics on restore)

**Choice**: The mode-activation overlay applies every drawer-present key buffer-locally. There is no "if drawer matches preset, skip" check. The drawer is the source of truth.

**Alternatives considered:**

- *Skip overlay when drawer value equals preset value*: micro-optimization (avoids a buffer-local set call). The overlay is already conditional on the key being non-nil; comparing values would require importing `gptel--preset-mismatch-value`-style logic, and any divergence from upstream's comparison semantics would be a subtle bug source.

**Rationale**: With the writer emitting the full snapshot, the drawer always carries the values; the overlay always installs them; the buffer state is always equal to drawer state. Simpler to reason about and to test. Performance is irrelevant — overlay runs once per buffer activation, not per request.

### Decision 4 — Render full snapshot at session creation by extending `--render-drawer-text`

**Choice**: `jf/gptel-scope-profile--render-drawer-text` gains responsibility for emitting the chat-mode snapshot keys in addition to the existing scope keys. It accepts the resolved preset spec (or the preset name and resolves the spec internally) and emits `:GPTEL_MODEL:`, `:GPTEL_BACKEND:`, `:GPTEL_TOOLS:`, `:GPTEL_TEMPERATURE:`, `:GPTEL_MAX_TOKENS:`, `:GPTEL_NUM_MESSAGES_TO_SEND:` from non-nil preset keys.

**Alternatives considered:**

- *Two-pass write at creation time: render scope drawer, then activate `gptel-chat-mode` in a temp buffer, save, close*: heavy. We'd be spinning up a chat-mode buffer just to invoke the save hook. Decision 4 of the previous change explicitly rejected this pattern for the same reason.
- *Have `jf/gptel--initial-session-content` (in `sessions/commands.org`) own the snapshot rendering*: would split drawer-text generation across two modules. Today the scope module owns the renderer and the renderer is the single source of truth for `register/shape/drawer-text-block`. Splitting it would force the round-trip idempotency invariant to be re-established across two writers.

**Rationale**: The renderer already owns drawer-text generation. Adding the snapshot emission keeps that single source of truth intact. The function signature gains one argument (the preset spec or a flag); existing callers in `sessions/commands.org` and `tools/persistent-agent.org` pass it through.

### Decision 5 — Default `gptel--set-buffer-locally` to `t` in `gptel-chat-menu`

**Choice**: The `gptel-chat-menu` transient definition (at `config/gptel/chat/menu.org`) wraps its body so `gptel--set-buffer-locally` is bound to `t` for the lifetime of the menu. Cleanest implementation is a `let`-binding inside the suffix invocations or, if that doesn't compose with transient's machinery, a `:setup` hook on the transient that sets the variable buffer-locally on entry and restores on exit.

**Alternatives considered:**

- *`setq-local gptel--set-buffer-locally t` in chat-mode initialization*: leaks the change to upstream `gptel-menu` invocations in the same buffer. Users who explicitly want global-default behavior via upstream `M-x gptel-menu` would be surprised.
- *Patch every infix in `gptel-chat-menu` to pass `t` explicitly*: Requires forking a half-dozen upstream infixes (`gptel-tools`, `gptel-system-prompt`, `gptel--infix-provider`, …). Extensive churn for a small behavioral default change.
- *Add a chat-mode-specific scope toggle that defaults differently*: doubles the menu surface. Confusing.

**Rationale**: Wrapping the transient is local, reversible, and respects upstream's machinery. The user retains the scope toggle if they want oneshot or global per-invocation.

### Decision 6 — Existing sessions degrade gracefully; no migration

**Choice**: Sessions whose drawers carry only the old-style keys (`:GPTEL_PRESET:` + scope keys, no `:GPTEL_MODEL:` etc.) load normally. The preset is applied; the drawer overlay is a no-op for the absent keys; the buffer-local config matches the preset; on first save under the new code, the writer emits the full snapshot.

**Alternatives considered:**

- *Migration command that walks `~/.gptel/sessions/` and rewrites drawers*: useful but out of scope. Punt to a follow-up if needed.
- *Soft-migrate on every open by writing the snapshot eagerly*: violates the "no save without user action" principle. Opening a file should not modify it.

**Rationale**: First-save expansion is the natural migration path. Users who never re-open or re-save an old session never see a behavior change; users who do, see the drawer expand on the next save. No command, no user-facing migration step.

## Risks / Trade-offs

**[Risk] Drawer churn on every save (writes 7+ properties even when nothing changed).**
→ Mitigation: `org-entry-put` is idempotent in terms of file content — writing the same value produces no diff. `git diff` on a save where nothing changed shows zero changes to the drawer. The work is bounded by the number of keys (~8), not by buffer size.

**[Risk] Existing tests in `save-state-spec.el` and `preset-application-spec.el` spy on `gptel-org-set-properties`. Replacing the call breaks them.**
→ Mitigation: Tests are already in scope for rewrite (proposal Impact section). The new tests assert drawer text directly (string assertions on the saved buffer), which is a stronger contract than the previous spy-based assertions and survives future writer-internal changes.

**[Risk] `:GPTEL_SYSTEM:` overlay back-compat is a hidden contract — users who never authored a system prompt in the drawer see the right behavior; users who DID author one keep seeing it; but a user who re-saves such a buffer loses the entry on subsequent reopens (writer didn't re-emit it).**
→ Mitigation: Document this in the spec scenario "Drawer-authored system prompt still respected on restore" with the explicit NOTE that the next save will not re-write it. If a user wants a durable per-session system prompt, they should put it in a per-session preset (or in dir-locals). Long-term: if this matters, add a `:GPTEL_SYSTEM:` writer with a single-line constraint and an opt-in flag.

**[Risk] `gptel-chat-menu` scope override leaks if the menu setup hook fails to restore `gptel--set-buffer-locally` on transient teardown.**
→ Mitigation: Use `unwind-protect` or transient's `:teardown` hook to guarantee restoration. Add a regression test: invoke `gptel-chat-menu`, simulate an abort, verify `gptel--set-buffer-locally` is at its default after.

**[Risk] Future upstream additions to the drawer key set (e.g., a new `:GPTEL_FOO:`) won't be picked up by our writer.**
→ Mitigation: Test the writer against `gptel-org--entry-properties` (the upstream reader) — if the reader returns a tuple with a new field we don't write, the round-trip test catches it. Add the missing key when this happens.

**[Risk] User has a custom preset where `:tools` is `(:append (...))` (a modify-list spec). Upstream's `gptel--preset-mismatch-value` for `:tools` returns true in that case (treats modify-spec as always mismatching). Our writer doesn't care about this — we always write the buffer's current `gptel-tools` — but the preset application step still resolves the modify-spec correctly via `gptel--apply-preset`, so by the time we save, `gptel-tools` is the resolved list. Snapshot is still correct.**
→ No mitigation needed; called out for completeness.

**[Risk] Scope drawer writer (`jf/gptel-scope--write-pattern-to-drawer`) calls `save-buffer` after appending a scope pattern. With the new save hook, `save-buffer` triggers the full-snapshot rewrite. If the buffer has unsaved user content elsewhere, the snapshot writer will see that content. This is a behavior change from "save just the drawer key" to "save the whole buffer including any in-progress turn".**
→ Mitigation: This already happens today (the upstream save hook also triggers on `save-buffer`). The new contract just means the drawer changes more on each save. Acceptable. No code change required.

## Migration Plan

No data migration is required.

- **Pre-existing session files with old-style drawer (preset + scope only)**: load normally. Drawer overlay is a no-op for absent snapshot keys; preset application installs the buffer-local config. On first `save-buffer`, the writer emits the full snapshot, expanding the drawer in-place.
- **Pre-existing session files with no drawer at all**: load normally. No preset applied (current behavior). User selects via menu and saves; drawer is created from scratch with the full snapshot.
- **Drawer with manually-authored `:GPTEL_SYSTEM:`**: the overlay reads it and installs the system prompt. The next save does NOT re-emit `:GPTEL_SYSTEM:` (writer never emits it). The line stays in the file as-is until the user manually edits or removes it.
- **Rollback**: revert the touched commits. The next save reverts to delta-from-preset semantics; the expanded drawer keys remain in the file but are deleted on the first save under the rolled-back code (delta-from-preset deletes them when they match the preset). Acceptable.

## Open Questions

1. **Should the `gptel-chat-menu` scope default cover *all* infixes or only Tools?** Decision 5 picks "all" for consistency. If user testing reveals friction with model/temperature changes also defaulting to buffer-local (e.g., user wanted to change global default and forgot to toggle scope), we may need to scope-narrow to Tools. Park as a follow-up.
2. **Should the renderer also accept and emit user-supplied delta values at creation time** (e.g., `--create-for-session` taking a `model` override that takes precedence over the preset's `:model`)? Today no caller passes such overrides. If `PersistentAgent` ever needs to override the parent's preset model on a per-agent basis, we'll add an optional plist arg. Park.
3. **Is there a meaningful gain from emitting `:GPTEL_SYSTEM:` *only* when it differs from the preset (delta-only for system, full-snapshot for everything else)?** Hybrid approach. Probably not worth the special case; consistency wins. If users complain about losing manual system-prompt edits across saves, revisit. *(Resolved by Addendum Finding B — the system prompt becomes a visible heading body, not a property at all.)*

## Addendum — user-testing findings (2026-05-22)

After the change was developed and smoke-tested, three findings from real-session use were folded back in. They are tracked as additional tasks on this change (`fix-scope-drawer-value-emphasis`, `emit-system-prompt-and-chat-headings-at-creation`, `make-system-prompt-heading-authoritative`, `fold-config-drawer-on-open`) rather than a separate change. This section records the decisions they introduce.

### Finding A — `:GPTEL_SCOPE_*:` path values render italicized, hiding `/`

A drawer value such as `:GPTEL_SCOPE_READ: /Users/jeff/emacs/` is a syntactically valid org `/emphasis/` span — the leading `/` opens it, the trailing `/` closes it. Org applies emphasis font-lock buffer-wide, including inside property drawers, so the path renders italic and the boundary slashes are dimmed/hidden. Confirmed by a fontification probe: trailing-slash directory values get `(italic org-property-value)`; values with no trailing slash get plain `org-property-value`.

**Decision A.** `gptel-chat-mode` installs a buffer-local font-lock keyword that re-stamps `org-property-value` (with the OVERRIDE flag) over property-drawer value spans. This removes emphasis from drawer values — they are data, not prose — while leaving emphasis intact in chat-turn prose. *Rejected:* disabling `org-fontify-emphasized-text` buffer-wide (would kill emphasis in chat content too); stripping trailing slashes at render time (only fixes our-rendered values, not hand-edited ones, and risks scope-matching semantics).

### Finding B — system prompt should be a visible `* System Prompt` heading (reverses Decision 2's "system rides the preset only" model)

Decision 2 excluded the system prompt from `session.org` because it is "unwieldy as a property *value*." That objection is specific to property *values*; an org *heading body* carries multi-line, special-character text with no escaping. The system prompt is therefore relocated into the document as visible content. A `session.org` file is structured as:

```
:PROPERTIES:        <- file-level config drawer (unchanged location: point-min)
...config + scope keys...
:END:

* System Prompt
:PROPERTIES:
:VISIBILITY: folded
:END:
<system prompt body — authoritative>

* Chat
#+begin_user
...
```

**Decision B.**
- The config `:PROPERTIES:` drawer stays a **file-level drawer at `point-min`** (chosen over making it the heading's own drawer: `org-entry-put` / `gptel-org--entry-properties` at `point-min` are unchanged — lowest code risk).
- The **`* System Prompt` heading body is the authoritative system prompt.** On mode activation the restore path reads it and installs it buffer-locally as `gptel--system-message`, winning over the preset's `:system`. On `before-save-hook` the current `gptel--system-message` is written back into the heading body. The preset's `:system` only *seeds* a new session's heading body at creation.
- Turn blocks (`#+begin_user` / `#+begin_assistant`) live under a `* Chat` heading. The chat parser stays heading-indifferent (Decision 12) — the `* System Prompt` body and the `* Chat` heading are commentary to it — so `gptel-chat-new` scratch buffers (no headings) are unaffected. Only the session-creation renderer emits the new layout.
- `:GPTEL_SYSTEM:` restore precedence becomes: `* System Prompt` heading body > legacy `:GPTEL_SYSTEM:` drawer entry > preset `:system`. The writer still never emits `:GPTEL_SYSTEM:` (Decision 2's write-exclusion stands; the property is now legacy-read-only).
- Existing sessions degrade gracefully (cf. Decision 6): a file with no `* System Prompt` heading falls back to the preset; the heading is materialized on first save under the new code.

**Known limitation.** Because the parser is heading-indifferent, a system-prompt body containing a literal `#+begin_user` at column 0 would be mis-parsed as a turn. Preset system prompts in this repo do not; flagged for the implementor to accept or add a narrow guard.

### Finding C — config drawer and `* System Prompt` heading should be folded by default

**Decision C.** On open, the `* System Prompt` subtree is folded via a `:VISIBILITY: folded` property emitted by the session-creation renderer (org's `org-set-visibility-according-to-property` honors it at startup; `* Chat` stays open). The file-level config drawer is folded by `gptel-chat-mode` on activation. This keeps `session.org` visually clean — open the file and see two headings plus a folded drawer, not a wall of configuration.
