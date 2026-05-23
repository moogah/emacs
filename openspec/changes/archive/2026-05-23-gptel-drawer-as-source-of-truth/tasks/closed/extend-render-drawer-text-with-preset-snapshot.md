---
name: extend-render-drawer-text-with-preset-snapshot
description: Extend `jf/gptel-scope-profile--render-drawer-text` and `--create-for-session` to emit upstream-compatible chat-mode snapshot keys from the resolved preset spec, alongside the existing scope keys.
change: gptel-drawer-as-source-of-truth
status: done
relations: []
---

## Files to modify

- `config/gptel/scope-profiles.org` (modify)
- `config/gptel/scope/test/...` — whichever existing spec covers `--render-drawer-text` and `--create-for-session` (modify; pattern is `*-spec.el`)

## Implementation steps

1. Open `config/gptel/scope-profiles.org` and locate `jf/gptel-scope-profile--render-drawer-text` (currently around `scope-profiles.org` source line ~190 in tangled `scope-profiles.el:190`).
2. Add a `preset-spec` parameter (a plist returned by `gptel-get-preset`, may be nil for the no-preset path). Update the docstring to document the new argument and the new emitted keys.
3. After the existing `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:` push branches and BEFORE the scope keys block, add a block that, when `preset-spec` is non-nil, pushes lines for each non-nil preset key in this set:
   - `:model` → `:GPTEL_MODEL: <gptel--model-name VALUE>`
   - `:backend` → `:GPTEL_BACKEND: <gptel-backend-name VALUE or string>` (handle both `gptel-backend` struct and bare string)
   - `:tools` → `:GPTEL_TOOLS: <space-joined tool names>` — resolve via `gptel-tool-name` for tool structs; for `(:append ...)` modify-list specs, resolve via `gptel--modify-value gptel-tools VALUE` then map to names. Avoid hard-failing if `gptel-tools` is unbound at render time — skip the line.
   - `:temperature` → `:GPTEL_TEMPERATURE: <number-to-string VALUE>`
   - `:max-tokens` → `:GPTEL_MAX_TOKENS: <number-to-string VALUE>`
   - `:num-messages-to-send` → `:GPTEL_NUM_MESSAGES_TO_SEND: <number-to-string VALUE>`
4. Do NOT emit `:GPTEL_SYSTEM:` under any condition. Add a comment at the renderer noting this is a deliberate exclusion (Decision 2 in design.md).
5. Update `jf/gptel-scope-profile--apply-to-drawer` (the buffer-mode applicator) to mirror the same key set: write each snapshot key via `org-entry-put` / `org-entry-put-multivalued-property` from the preset spec when supplied. Same `:GPTEL_SYSTEM:` exclusion. The existing scope-keys behavior is unchanged.
6. Update `jf/gptel-scope-profile--create-for-session` to accept (or internally resolve via `gptel-get-preset`) the preset spec and pass it to `--render-drawer-text`.
7. Update `jf/gptel-scope-profile--validate-cloud-auth` callers and any other helpers if their signatures need to thread the preset spec through (typically they do not — only the renderer/applicator/create-for-session change).
8. Re-tangle: `./bin/tangle-org.sh config/gptel/scope-profiles.org`.
9. Update existing scope-profile tests to cover the new keys: a "preset spec emits snapshot keys" scenario, a "sparse preset omits absent keys" scenario, and a "GPTEL_SYSTEM is never rendered" scenario. Ensure idempotency invariant (`render-text → write to fresh buffer → apply same plist via --apply-to-drawer` is a no-op) is reasserted with the expanded key set.

## Design rationale

The renderer is already the single source of truth for `register/shape/drawer-text-block`. Splitting snapshot rendering across two modules would force the round-trip idempotency invariant to be re-established across two writers. Decision 4 of `design.md` picks the renderer-extension path explicitly.

`:GPTEL_SYSTEM:` is excluded at the writer level so the same code path is reused by both creation-time rendering and save-time application — there is no opportunity for the system prompt to leak into the drawer through one path while being suppressed in the other.

## Verification

- `./bin/tangle-org.sh config/gptel/scope-profiles.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/scope` passes (existing scope tests + new snapshot scenarios).
- A manual `(jf/gptel-scope-profile--render-drawer-text 'system-explorer nil scope-plist preset-spec)` against the `system-explorer` preset returns text containing `:GPTEL_PRESET:`, `:GPTEL_MODEL: claude-sonnet-4-6`, `:GPTEL_TOOLS: PersistentAgent run_bash_command`, and the existing `:GPTEL_SCOPE_*:` keys. No `:GPTEL_SYSTEM:` line.

## Context

- design.md § Decision 4 — "Render full snapshot at session creation by extending `--render-drawer-text`"
- design.md § Decision 2 — "`:GPTEL_SYSTEM:` exclusion is in the writer, not the renderer or the overlay"
- specs/gptel/scope-profiles.md — Requirement: Integration with session creation; Requirement: Mutable scope drawer in session.org
