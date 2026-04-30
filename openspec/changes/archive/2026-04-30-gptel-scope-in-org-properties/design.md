## Context

The gptel scope subsystem currently persists per-session permissions in a `scope.yml` sidecar living in each branch directory. The validator parses it on every tool call; the expansion UI mutates it on "add to scope"; persistent-agent creation writes a child copy of it. Now that sessions are org-mode native (chat-mode in `session.org`, with a file-level `:PROPERTIES:` drawer already storing `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:`), `scope.yml` is the only piece of session state still living outside the buffer.

The architecture is converging toward "the org file *is* the session." Storage that doesn't live in the file is friction: a separate YAML loader, a separate schema, a separate writer, and a separate "where is the source of truth" question. Folding scope into the same `:PROPERTIES:` drawer collapses that to one source.

The proposal also drops `:security` (`enforce_parse_complete`, `max_coverage_threshold`) — the per-session knobs that were never set differently from defaults in any production session and existed mostly as a reason to need a YAML schema.

## Goals / Non-Goals

**Goals:**
- Scope state lives in `session.org`'s file-level `:PROPERTIES:` drawer, encoded as multi-value `+` properties.
- Validator reads buffer-first (live chat buffer reflects user's just-typed edits) with file fallback for the rare buffer-less call.
- Expansion UI's add-to-scope mutates the chat buffer's drawer via `org-entry-put` and saves the buffer, putting the change into the user's undo ring.
- Persistent-agent creation embeds the agent's drawer in the agent's initial `session.org` content; no `scope.yml` is produced for the agent.
- The `scope-yaml` boundary module, the YAML writers in `scope-profiles` and `scope-expansion`, the `:security` plist branch, and the YAML test fixtures all go away.
- `enforce-parse-complete` and `max-coverage-threshold` become module-level constants (`t` and `1.0`) in `scope-validation.el`.

**Non-Goals:**
- Migrating existing sessions. Old `scope.yml`-only sessions are dead per cutover policy; users will recreate them.
- Changing the validator pipeline (stages 1–4, glob, path validator, error codes).
- Changing the expansion UI's six-choice transient menu, queue, or callback shapes.
- Changing scope-profile templates on disk — they remain plain YAML files in `config/gptel/scope-profiles/`.
- Changing the preset extraction pipeline (preset-registration's 5 stages).
- Adding new scope dimensions or modifying `bash-parser` semantic plugins.

## Decisions

### Decision 1 — Multi-value `+:` encoding for list keys

Use Org's standard multi-value property convention: the first value uses the bare key (`:GPTEL_SCOPE_READ:`), subsequent values use the `+:` form (`:GPTEL_SCOPE_READ+:`). Reading goes through `org-entry-get-multivalued-property`, which natively splits on whitespace and respects the `+` chain.

**Why this over alternatives.**

- **Single-line space-separated** (`:GPTEL_SCOPE_READ: a b c`) is more compact but breaks on patterns containing whitespace. Globs typically don't, but `**/some dir/**` is legal. Multi-value is robust.
- **Single property holding a serialized blob** (e.g. JSON or sexp in `:GPTEL_SCOPE:`) defeats the readability and undo-affordance goals.
- **Custom named drawer** (`:SCOPE: ... :END:`) is not understood by `org-entry-get`; we'd hand-roll a parser. This is "we made a config format that happens to look like org," not "we're org-native."
- **Sub-headings** (`* Scope` with its own drawer) interleaves with chat-mode's `#+begin_user`/`#+begin_assistant` blocks and changes the chat buffer structure. Bad fit.

### Decision 2 — Buffer-first read with file fallback

The loader checks for a chat buffer associated with the session (registry's `:buffer` field for `jf/gptel--branch-dir`). If one exists, read the drawer from that buffer. If not, read it from `<branch-dir>/session.org` headlessly via `with-temp-buffer` + `insert-file-contents`.

**Why.** WYSIWYG: when the user has just edited the drawer (e.g. typed an extra deny pattern manually) and not yet saved, the validator should see what's on screen. The file is a stale snapshot until `save-buffer`. The buffer is the live state. The file fallback handles `request_scope_expansion` calls from other contexts and any future programmatic path.

**Alternative rejected.** Always read the file. Simpler but breaks the user's mental model — "I added a pattern in the drawer, why is it not in scope until I save?"

### Decision 3 — Eliminate `:security` from the plist shape entirely

Replace `(plist-get config :security)` references in `scope-validation.el` with module-level `defconst`s `jf/gptel-scope--enforce-parse-complete` (always `t`) and `jf/gptel-scope--coverage-threshold` (always `1.0`). The plist returned by the loader has exactly the keys `:paths` and `:cloud`.

**Why this over keeping the plist key with constant values.** The whole point is removal — keeping a `:security` key that always carries the same values would leave dead structure that adds nothing. The interfaces.org "scope config shape" contract simplifies to two keys; tests checking the plist shape simplify; the merge logic disappears entirely.

**Trade-off.** No per-session escape hatch. If a future session needs `enforce-parse-complete: false`, that's a module-level config change, not a drawer key. The user accepted this.

### Decision 4 — Writer mutates buffer, then `save-buffer`; no separate file path

`jf/gptel-scope--write-pattern-to-drawer` runs `org-entry-put` against the live chat buffer's drawer (creating the drawer at `point-min` if absent — though in practice it already exists from session creation), then calls `save-buffer`. There is no separate "write to file" code path because the buffer-to-file write is what `save-buffer` already does.

**Why.** Two reasons. First, the user gets the mutation in the buffer's undo ring (`C-_` reverts an accidental "add to scope"). Second, eliminating a separate file writer means the scope-expansion module loses ~50 lines of YAML-emitter helpers and the `interfaces.org` "yaml writer preserves structure" requirement.

**Risk.** A failing `save-buffer` (e.g. read-only file, disk full) leaves the buffer ahead of the file. The next call would still see the in-memory drawer. Acceptable: this is the same failure mode as any chat-buffer save, and the user will see the standard "Save aborted" message.

### Decision 5 — Persistent-agent embeds drawer in initial content, doesn't open a buffer

Agent creation today writes `scope.yml` to disk and lets the agent buffer be opened later (during execution). Tomorrow, the same code path builds drawer text via `jf/gptel-scope-profile--render-drawer-text` and prepends it to the agent's initial `session.org` content. No buffer is opened during creation; the file is written in one shot.

**Why this over opening a buffer headlessly to use `org-entry-put`.** Initial-content embedding is simpler and matches how `:GPTEL_PRESET:` and `:GPTEL_PARENT_SESSION_ID:` are already pre-populated (see `persistent-agent.el:139`). It also avoids the "open buffer, mutate, save, kill" lifecycle dance during creation.

### Decision 6 — Profile templates stay YAML on disk

`config/gptel/scope-profiles/*.yml` are not converted to org files. They are reusable, immutable templates with `${project_root}` placeholders, not session state.

**Why.** Profiles aren't bound to a session — they're shared across many sessions and edited by hand or by config-as-code. YAML stays the right format. The change is about session state, not template state.

### Decision 7 — `scope-rearch-followups` ordering

Both this change and `scope-rearch-followups` touch `scope-expansion.org`, `scope-profiles.org`, and `preset-registration.org`. Land `scope-rearch-followups` first if possible. If it has not landed when this change starts, fold its three small fixes into this change's task list (Bugs 1–3: validation-type derivation in `request_scope_expansion`, callback-on-bare-command in `add-bash-to-scope`, `org-roam-patterns` cleanup) — they're small enough to subsume.

**Why.** The merge dance between two changes touching the same files isn't worth the overhead. Either land one first cleanly, or absorb both.

### Decision 8 — No migration

Existing sessions with `scope.yml` are not migrated. The user explicitly directed "no migrations or legacy support — old sessions are dead."

**Why.** A migration code path (read scope.yml, write drawer, delete scope.yml) costs implementation time, test coverage, and a temporary code surface. The active session set is small and the user is willing to recreate them. Strictly more code is removed by skipping migration.

## Risks / Trade-offs

**[Risk] Drawer corruption recurrence** — The `gptel-org-mode-sessions` change addressed duplicate `:PROPERTIES:` drawers caused by duplicate `gptel--save-state` hooks. Loading more state into the same drawer doubles the surface area for any regression of that bug class.
→ **Mitigation.** The writer uses `org-entry-put`, which targets the existing drawer in place rather than inserting a new one. The expansion writer is the only new participant in the save-buffer path. We will add a regression test (`drawer/no-duplicate-drawer-spec.el`) that runs an add-to-scope sequence and asserts exactly one `:PROPERTIES: ... :END:` block remains at point-min.

**[Risk] Buffer-vs-file divergence** — If the user mutates the file outside Emacs (or in a different Emacs instance) while the buffer is open, the validator's buffer-first read will see stale state.
→ **Mitigation.** Same problem as for any open Emacs file. `auto-revert-mode` is the standard fix; it's not specific to this change. We document the buffer-first behavior in the spec; users who need cross-process correctness use auto-revert. (Realistic exposure is near-zero — chat buffers are not edited by other tools.)

**[Risk] Mid-streaming buffer mutation by the writer** — A scoped tool call can fail validation in the middle of streaming, opening the expansion UI; the user's add-to-scope mutates the buffer mid-stream.
→ **Mitigation.** Streaming inserts text into `#+begin_assistant` blocks downstream of `point-min`. The drawer at `point-min` is not in the streamed region. `org-entry-put`'s buffer modifications are at the file-level drawer; they don't disturb streaming markers or overlays. The save-buffer call interleaves with the chat-mode `before-save-hook` (metadata-yml updater) but doesn't introduce new hooks. We will manually verify (in a smoke test) that an add-to-scope during an active stream completes cleanly and the streaming response continues.

**[Risk] Coverage threshold of 1.0 produces noisy warnings** — Stage 5 will warn whenever bash-parser semantic plugin coverage isn't 100%, which is most non-trivial commands.
→ **Mitigation.** This is the user's intent (explicit confirmation). The warning is non-blocking; it surfaces in `*Warnings*`. If the noise becomes painful in practice, the constant is one line to tune later. Not a blocker.

**[Risk] Test migration churn** — Many existing tests fixture `scope.yml` on disk. Each must be rewritten to fixture a drawer.
→ **Mitigation.** A focused `helpers-spec.el` macro (`jf/gptel-test--with-scope-drawer`) reduces per-test rewrite cost to ~3–5 lines. We do the migration as a single sweep early in the task list so subsequent tasks land on the new fixture style.

**[Risk] `scope-rearch-followups` merge contention** — As noted in Decision 7.
→ **Mitigation.** Land that change first or fold its work in.

**[Trade-off] Loss of per-session security knobs** — Decision 3.
→ Accepted. No production session used non-default values; the configurability cost outweighed the value.

**[Trade-off] No backward compatibility** — Decision 8.
→ Accepted. Cleaner removal; smaller diff.

**[Known gap] Parser/validator boundary trigger gaps for `:read-metadata` and `:match-pattern`** — Cycle-4 smoke testing surfaced two cases where the writer/drawer-bucket layer is correct end-to-end but the parser doesn't emit the right operation type, so the expansion UI never fires and the bucket is unreachable through Add-to-Scope:
- `test -e PATH`, `[ -e PATH ]`, and `stat PATH` do not produce `:read-metadata` violations against out-of-scope paths; the calls silently succeed and `:GPTEL_SCOPE_READ_METADATA:` only populates via custom-add or edit-manually.
- `find /home -name "*.txt"` produces a `:match-pattern` op whose `:search-scope` the validator ignores, so even after Add-to-Scope on the cluster, the `:match-pattern` op resolves the glob against cwd and re-denies.
→ Both deferred to follow-up changes. Captured in `.tasks/fix-read-metadata-trigger-gap.md` and `.tasks/fix-match-pattern-parser-validator-boundary.md` with reproductions, evidence, and two viable fix options each. The drawer-migration change is not blocked: writer, loader, drawer schema, and round-trip all work; what's missing is the producer-side operation classification in bash-parser. Custom-add and edit-manually remain available as user-driven fallbacks until the parser handlers land.

## Migration Plan

There is no user-data migration. The implementation order is:

1. **Land `scope-rearch-followups` first** (or fold its tasks into this change at task-generation time).
2. **Add the drawer encoding contract** to `interfaces.org` (key vocabulary, multi-value semantics) and constants in `scope-validation.org`.
3. **Implement the drawer reader** (`load-from-buffer`, `load-from-file`, `load-config`) in `scope-validation.org`. Replace the `(scope-yaml-load-schema scope-file)` call site. All scope tests in `validation/` continue to pass with the new loader because the plist shape they consume is unchanged.
4. **Implement the drawer writer** (`write-pattern-to-drawer`, `map-operation-to-drawer-key`) in `scope-expansion.org`. Replace the YAML-writing branch in `--write-pattern-to-scope` (or rename the entrypoint and rewire callers).
5. **Implement the profile applicator** (`render-drawer-text`, `apply-to-drawer`) in `scope-profiles.org`. Rewire `--create-for-session` to return drawer text for embedding instead of writing `scope.yml`.
6. **Rewire session creation** in `sessions/commands.org` to prepend the drawer text to the chat-mode initial content before `write-region`-ing the new `session.org`.
7. **Rewire persistent-agent creation** in `tools/persistent-agent.org` to use the same drawer-text helper instead of `scope-profile--write-scope-yml`.
8. **Migrate tests** — rewrite YAML-fixture tests under `validation/`, `expansion/`, `tools/test/persistent-agent/`, and `test/session-creation-spec.el` to use `jf/gptel-test--with-scope-drawer` (unit) or tmpdir + assert-on-file-content (integration).
9. **Delete `scope-yaml.{org,el}`** and remove the `(jf/load-module .../scope/scope-yaml.el)` line in `gptel.el`. Delete `config/gptel/scope/test/yaml/`.
10. **Delete `:security` references** — remove `validate-security-config`, the `:security` branch of `merge-schema-defaults` (both gone with `scope-yaml`), and any remaining `(plist-get config :security)` reads. Update `interfaces.org` scope-config-shape to omit `:security`.
11. **Smoke-test by hand** — create a fresh persistent session, verify the drawer is populated; trigger a scope violation and add-to-scope, verify the drawer is updated and the buffer is saved; reopen the file, verify the drawer round-trips.

**Rollback.** Git revert the merge. No data migration, so no data rollback. Existing sessions created during the change's lifetime would lose their drawer-resident scope on revert and gain back nothing on disk; in practice this is the same "old sessions die" cutover. If revert is needed before merge, the proposal's "old sessions are dead" framing limits exposure.

## Open Questions

- **Does `scope-rearch-followups` land first, or do we fold its work in?** Answer at task-generation time based on its merge state. This change's Decision 7 keeps both paths open.
- **Should `:GPTEL_SCOPE_DENY:` appear in the drawer even when empty?** Two options: (a) omit the key entirely (loaded as nil/empty list), (b) emit the key with no value as a placeholder. The reader handles both equivalently. The writer currently leans toward (a) — only emit a key when there are values. Confirmed during implementation by inspection of the produced drawer text in session-creation tests.
- **Is `org-entry-get-multivalued-property` deterministic across Org versions in our pinned deps?** It's been stable for years and is the documented API for multi-value properties; we assume yes. If a future Org upgrade changes the `+:` chain semantics, the regression would surface in the round-trip tests immediately.
