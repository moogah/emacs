## Components

```
                       ┌──────────────────────────────────────────────┐
                       │ session.org (chat-mode buffer)               │
                       │ :PROPERTIES:                                 │
                       │ :GPTEL_PRESET: ...                           │  ◄─── source of truth
                       │ :GPTEL_SCOPE_READ: ...                       │       (buffer-first;
                       │ :GPTEL_SCOPE_WRITE: ...                      │        file fallback)
                       │ :END:                                        │
                       │ #+begin_user...#+end_user                    │
                       └──────────────────────────────────────────────┘
                                  ▲                  ▲
              read fresh per call │                  │ append/idempotent write
                                  │                  │
   ┌──────────────────────────────┴──┐  ┌────────────┴─────────────────┐
   │ scope-validation.el             │  │ scope-expansion.el           │
   │ ─ jf/gptel-scope--load-from-    │  │ ─ jf/gptel-scope--write-     │
   │     buffer                      │  │     pattern-to-drawer        │
   │ ─ jf/gptel-scope--load-from-    │  │ ─ map-operation-to-drawer-   │
   │     file                        │  │     key                      │
   │ ─ jf/gptel-scope--enforce-      │  │ ─ adds patterns idempotently │
   │     parse-complete (CONST t)    │  │ ─ saves the buffer           │
   │ ─ jf/gptel-scope--coverage-     │  │ ─ buffer's undo ring records │
   │     threshold (CONST 1.0)       │  │     mutation                 │
   └─────────────┬───────────────────┘  └────────────┬─────────────────┘
                 │                                   │
                 ▼                                   ▼
   ┌─────────────────────────────────────────────────────────────────┐
   │ scope-validation.el — pipeline (unchanged)                      │
   │   stages 1-4, glob, path-validator, build-violation-info        │
   └─────────────────────────────────────────────────────────────────┘

   ┌──────────────────────────────────────────────────────────────────┐
   │ scope-profiles.el                                                │
   │ ─ --load / --resolve / --expand-variables / --deep-merge (kept)  │
   │ ─ --apply-to-drawer  (REPLACES --write-scope-yml)                │
   │     • returns drawer text for embedding in initial session.org   │
   │     • or applies via org-entry-put to an open buffer             │
   └──────────────────────────────────────────────────────────────────┘

   ┌──────────────────────────────────────────────────────────────────┐
   │ persistent-agent.el                                              │
   │ ─ initial-content builder pre-populates the agent's drawer       │
   │   (:GPTEL_PRESET, :GPTEL_PARENT_SESSION_ID, :GPTEL_SCOPE_*)      │
   └──────────────────────────────────────────────────────────────────┘

   ┌──────────────────────────────────────────────────────────────────┐
   │ DELETED ── scope-yaml.el / scope-yaml.org                        │
   └──────────────────────────────────────────────────────────────────┘
```

### Components added / heavily refactored

- **`jf/gptel-scope--load-from-buffer`** (new) — given a chat buffer, walks its `:PROPERTIES:` drawer at `point-min` via `org-entry-get` and `org-entry-get-multivalued-property`, returning the canonical scope plist `(:paths (...) :cloud (...))`.
- **`jf/gptel-scope--load-from-file`** (new) — same contract, but uses `with-temp-buffer` + `insert-file-contents` for the rare buffer-less case (e.g. programmatic `request_scope_expansion`).
- **`jf/gptel-scope--load-config`** (refactored) — top-level loader; tries the buffer for the current session first, falls back to the file. Replaces the previous `(scope-yaml-load-schema scope-file)` call.
- **`jf/gptel-scope--write-pattern-to-drawer`** (new) — takes a chat buffer, an operation keyword, and a pattern; idempotently appends to the operation's `:GPTEL_SCOPE_*` key (using `+:` for second and later values), then `save-buffer`.
- **`jf/gptel-scope--map-operation-to-drawer-key`** (new) — collapses granular operations to one of the five drawer-key bases (`READ`, `WRITE`, `MODIFY`, `EXECUTE`, `DENY`).
- **`jf/gptel-scope-profile--apply-to-drawer`** (new) — replaces `--write-scope-yml`. Returns drawer text or applies via `org-entry-put` depending on whether a buffer is in scope.
- **`jf/gptel-scope-profile--render-drawer-text`** (new helper) — produces the `:PROPERTIES: ... :END:` block as a string for prepending to a freshly created `session.org`.

### Components removed

- `config/gptel/scope/scope-yaml.{org,el}` — the entire YAML boundary module.
- `jf/gptel-scope-profile--write-scope-yml` and the YAML emitter helpers in `scope-profiles.el`.
- `jf/gptel-scope--get-scope-file-path`, `jf/gptel-scope--validate-scope-file-writable` in `scope-expansion.el`.
- `validate-security-config`, the `:security` branch of `merge-schema-defaults`, and the `:security` plist key throughout.

### Components unchanged

- Validator pipeline stages 1–4, glob matcher, path validator, error codes, `build-violation-info`.
- `gptel-make-scoped-tool` macro and `jf/gptel-scope-authorize-tool-call` dispatcher.
- Filesystem tools (`read_file_in_scope`, `write_file_in_scope`, `edit_file_in_scope`).
- Profile templates in `config/gptel/scope-profiles/*.yml`.
- `bash-parser` semantic plugin system.

## Interfaces

### Drawer ↔ scope-plist contract

```
DRAWER                                  PLIST
──────                                  ─────
:GPTEL_SCOPE_READ:    A                 (:paths (:read    (A B)
:GPTEL_SCOPE_READ+:   B                          :write   (...)
:GPTEL_SCOPE_WRITE:   C                          :modify  (...)
:GPTEL_SCOPE_DENY:    D                          :execute (...)
:GPTEL_SCOPE_DENY+:   E                          :deny    (D E))
:GPTEL_SCOPE_CLOUD_AUTH:      warn       :cloud (:auth-detection     "warn"
:GPTEL_SCOPE_CLOUD_PROVIDERS: aws                :allowed-providers  ("aws")))
:GPTEL_SCOPE_CLOUD_PROVIDERS+: gcp
```

The reader uses `org-entry-get-multivalued-property` for list keys and `org-entry-get` for scalars. Both target the file-level drawer (point-min, before any heading), which `org-entry-get` finds when called with `nil` POM and `(point-min)` positioning.

### Public API surface

| Function (file) | Replaces | Callers |
|---|---|---|
| `jf/gptel-scope--load-config` (`scope-validation`) | `scope-yaml-load-schema` | `jf/gptel-scope-authorize-tool-call` |
| `jf/gptel-scope--write-pattern-to-drawer` (`scope-expansion`) | `--write-pattern-to-scope` (YAML branch) | add-to-scope, add-wildcard, add-custom |
| `jf/gptel-scope-profile--apply-to-drawer` (`scope-profiles`) | `--write-scope-yml` | `--create-for-session`, `persistent-agent` |
| `jf/gptel-scope-profile--render-drawer-text` (`scope-profiles`) | (new — internal) | session creation, agent creation |

The dispatcher's `:on-allow` / `:on-deny` async callback contract is unchanged. The expansion UI's six-choice transient and queue are unchanged.

### Buffer-vs-file resolution

```
jf/gptel-scope--load-config
        │
        ▼
   buffer for jf/gptel--branch-dir's session.org loaded?
        │
   yes ─┴─ no
   │           │
   ▼           ▼
read drawer   open session.org headlessly via with-temp-buffer
from buffer   + insert-file-contents; read drawer; discard buffer
```

The "buffer for this session" lookup uses the registry's `:buffer` field for `jf/gptel--branch-dir`. If absent, the file path is `(expand-file-name "session.org" jf/gptel--branch-dir)`.

## Boundaries

### In scope

- The drawer encoding (key vocabulary, multi-value semantics, defaults).
- The reader (buffer-first, file-fallback) and writer (buffer-side, append-idempotent, save-buffer).
- The terminal applicator in `scope-profiles` and `persistent-agent`.
- Removal of `scope-yaml`, `:security`, `scope.yml` writers, and all `scope.yml` references in scope/sessions/persistent-agent specs.
- Test migration: delete YAML-fixture tests; add drawer-fixture tests.

### Out of scope

- Validator pipeline semantics (stages, error codes, glob, path validator).
- Profile template format on disk (still YAML).
- Preset extraction pipeline (preset-registration's 5 stages).
- `bash-parser` and its semantic plugins.
- Migration of existing sessions (none — old sessions are dead per the cutover policy).
- The chat-mode block format and chat-buffer hooks.

### Coordination boundary

- `scope-rearch-followups` (in flight): touches `scope-expansion.org`, `scope-profiles.org`, `preset-registration.org`. That change should land first; if it has not landed by the time this one starts, its three small fixes (Bugs 1, 2, 3) are folded into this change's task list to avoid a merge dance.

## Testing Approach

### Test Framework

**Buttercup** for everything new. New scope tests are already Buttercup (`config/gptel/scope/test/yaml/`, `validation/`, `expansion/` are all `*-spec.el`). The drawer reader / writer tests follow the same pattern — `describe` / `it` / `expect`, with `before-each` for fixture setup.

ERT is not used for new tests in this change.

### Test Organization

```
config/gptel/scope/test/
├── helpers-spec.el            ← shared fixtures (extended with drawer helpers)
├── drawer/                    ← NEW
│   ├── load-from-buffer-spec.el
│   ├── load-from-file-spec.el
│   ├── write-pattern-spec.el
│   └── operation-mapping-spec.el
├── validation/                ← existing; updated to use drawer fixtures
│   ├── authorize-tool-call-spec.el
│   ├── validate-bash-spec.el
│   └── ... (no schema changes; just fixture migration)
├── expansion/                 ← existing; writer tests rewritten
│   ├── add-to-scope-spec.el
│   └── ...
└── yaml/                      ← DELETED entirely
```

`config/gptel/test/session-creation-spec.el` and `config/gptel/tools/test/persistent-agent/creation-spec.el` are updated: the `scope.yml`-on-disk assertions become drawer-on-disk assertions against the produced `session.org`.

### Naming Conventions

- Files: `<topic>-spec.el` (e.g. `load-from-buffer-spec.el`).
- `(describe "Module or topic" ...)` outermost; `(describe "function name" ...)` for grouping; `(it "<verb phrase>" ...)` for individual cases.
- One assertion per `it` is preferred; multiple `expect` calls are acceptable when they describe one logical outcome.

### Running Tests

```bash
./bin/run-tests.sh -d config/gptel/scope                   # all scope tests
./bin/run-tests.sh -d config/gptel/scope/test/drawer       # just the new drawer tests
./bin/run-tests.sh -d config/gptel/scope/test/expansion    # writer tests
./bin/run-tests.sh -d config/gptel                         # whole subsystem
make test-buttercup-directory DIR=config/gptel/scope       # via make
```

Snapshot used the same way as elsewhere: `--snapshot` writes `test-results.txt` next to the directory; CI compares.

### Test Patterns

**Fixture strategy** (per user direction):

- **Unit tests** — `with-scope-drawer` helper in `helpers-spec.el`. Builds an in-memory buffer with the requested `:GPTEL_SCOPE_*` keys and runs the body inside it. No I/O. Used for the bulk of reader / writer / operation-mapping tests.

  ```elisp
  (defmacro jf/gptel-test--with-scope-drawer (alist &rest body)
    "Run BODY in a temp chat buffer whose :PROPERTIES: drawer is built from ALIST.
  ALIST is a list of (KEY . VALUES) where KEY is a drawer keyword like
  :GPTEL_SCOPE_READ and VALUES is either a list (multi-value) or a string (scalar)."
    `(with-temp-buffer
       (insert (jf/gptel-test--render-drawer ',alist))
       (insert "#+begin_user\n\n#+end_user\n")
       (org-mode)
       (goto-char (point-min))
       ,@body))
  ```

- **File-fallback tests** — explicit `make-temp-file ... t` tmpdir, real `session.org` written to disk, validator invoked outside the buffer's auto-init context. Asserts that the loader can recover the same plist headlessly. Smaller in count (~3–5 cases targeting the buffer-vs-file branch).

- **Persistent-agent creation tests** — full tmpdir flow: invoke the agent creation entrypoint, assert the produced `session.org` file's drawer contents match the expected `:GPTEL_SCOPE_*` keys. The drawer text itself is the contract here (replacing the old `scope.yml`-content assertions).

**Spies** — per existing convention, use `(spy-on 'jf/gptel-scope--load-config :and-call-fake ...)` to stub config in validator-pipeline tests where the test doesn't need a real drawer.

**Test data** — minimal scope fixtures (one read pattern, one deny) for happy-path tests; richer fixtures (multi-value reads + custom cloud config + operation-specific writes) for edge cases. Each requirement scenario in the deltas maps to at least one `it` block; corner-cases (empty list, missing key, invalid value, idempotent dedup) get their own.

### Scenario Mapping

Direct mapping from the delta specs:

| Spec scenario (delta) | Test file |
|---|---|
| `scope/Scope drawer encoding/Multi-value + form is the canonical encoding for list keys` | `drawer/load-from-buffer-spec.el` and `drawer/write-pattern-spec.el` |
| `scope/Scope configuration loading/Config resolved from the chat buffer when one is loaded` | `drawer/load-from-buffer-spec.el` |
| `scope/Scope configuration loading/Config falls back to the file when no buffer is loaded` | `drawer/load-from-file-spec.el` |
| `scope/Scope configuration shape/No :security key in the loaded plist` | `drawer/load-from-buffer-spec.el` |
| `scope-expansion/Add to scope action/Drawer mutation is buffer-side and saved` | `expansion/add-to-scope-spec.el` |
| `scope-expansion/Drawer writer preserves structure/Duplicate patterns are skipped` | `drawer/write-pattern-spec.el` |
| `scope-expansion/Drawer writer preserves structure/First addition for a key uses the bare form, not +` | `drawer/write-pattern-spec.el` |
| `scope-profiles/Mutable scope drawer in session.org/Drawer populated at session creation` | `config/gptel/test/session-creation-spec.el` |
| `persistent-agent/Agent session creation/Drawer with explicit paths only` | `config/gptel/tools/test/persistent-agent/creation-spec.el` |
| `persistent-agent/Agent session creation/No scope.yml written` | same as above (negative assertion: no file at `<agent-dir>/scope.yml`) |

Edge cases worth their own `it` even though no scenario lists them explicitly: drawer with only the `:GPTEL_PRESET:` line and no scope keys (loader returns the defaults plist), drawer with malformed `+` chain (e.g. a `+:` line with no preceding bare line — should still parse via `org-entry-get-multivalued-property`), buffer modified between two consecutive validator calls (loader returns updated values).

## Dependencies

- **Org-mode** (built-in): `org-entry-get`, `org-entry-get-multivalued-property`, `org-entry-put`. The reader's behavior in non-org buffers is governed by these functions; chat-mode buffers are derived modes that satisfy them.
- **Buttercup** (existing dev dep): test framework.
- **`gptel-org-mode-sessions`** (merged): chat-mode buffers as `session.org`, the `:GPTEL_PRESET:` drawer pattern, and the auto-init hook chain. This change layers on top of that contract — without it, no drawer exists to write to.

No new external dependencies. The `yaml` package and the `scope-yaml` boundary are removed from the scope subsystem's dependency closure (the `yaml` package is still used elsewhere for profile templates, metadata.yml, etc.).

## Constraints

- **Buffer-first read with care for streaming.** The drawer reader runs on each scoped tool call. During an LLM streaming response, the chat buffer is being mutated (text inserted into `#+begin_assistant` blocks); the reader scopes `save-restriction` + `goto-char (point-min)` and operates only on the file-level drawer, which is anchored at `point-min` and unaffected by mid-buffer streaming. Reader cost should be O(drawer-size), typically <100µs.
- **Writer must not interleave with streaming.** The expansion writer fires only on tool denials, which are points where streaming is paused awaiting tool resolution. `org-entry-put` modifies `point-min` content; any active overlay or marker downstream is unaffected. Writer triggers `save-buffer`; the existing chat-mode `before-save-hook` (metadata-yml updater) and `after-save-hook` are compatible — no new hooks introduced.
- **Idempotent writes.** Adding the same pattern twice is a no-op (the writer dedups before calling `org-entry-put`). This is required because the inline-flow trigger re-enters `authorize-tool-call` after each add-to-scope, and a multi-violation tool call can pass through the same pattern twice on retry.
- **Undo affordance.** Drawer mutations land in the buffer's undo ring. After an "add to scope" the user can `C-_` to revert. The existing chat-mode undo behavior is unchanged.
- **No drawer corruption regression.** The `gptel-org-mode-sessions` change resolved the duplicate-`:PROPERTIES:` corruption documented in `handoff-property-drawer-corruption.md`. This change does not reintroduce duplicate save-state hooks; the writer uses `org-entry-put` (which targets the existing drawer, not appending a new one) and the metadata-yml updater is the only `before-save-hook` participant.
- **Performance budget.** A typical scoped tool call already pays the cost of `bash-parser` (milliseconds for non-trivial commands). The drawer reader's added cost is dominated by `org-entry-get` lookups, which are cached internally per Org buffer. Acceptable.
