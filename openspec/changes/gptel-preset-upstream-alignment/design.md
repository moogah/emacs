## Context

Our gptel extensions manage presets through a custom pipeline: `gptel-agent-read-file` parses YAML frontmatter from `.md` files, custom functions resolve backend/model/tool names to gptel objects, and custom functions apply them as buffer-local variables. Session directories contain a copied `preset.md` that serves triple duty as model configuration, scope enforcement document, and system message source.

Meanwhile, gptel upstream has built a complete preset lifecycle: `gptel-make-preset` registers named presets, `gptel--apply-preset` resolves and applies them (handling backend strings, tool names, unknown keys), `gptel--save-state` differentially saves only values that diverge from the preset, and `gptel--restore-state` reconstructs state by applying the preset then overlaying saved overrides. This solves the Org property size limit and enables clean session persistence.

The two systems overlap but don't connect: our file-parsed presets end up in `gptel-agent--agents` (a string-keyed alist) while upstream's machinery queries `gptel--known-presets` (a symbol-keyed alist). Only two hardcoded agents (`gptel-agent`, `gptel-plan`) cross the bridge via `gptel-make-preset` calls in `gptel-agent-update`.

### Constraints

- Must support existing preset file format (`.md` with YAML frontmatter + body as system message)
- Must not break PersistentAgent tool (our primary sub-agent mechanism)
- Existing sessions will need graceful migration
- The `yaml.el` package is already available (transitive dependency)

## Goals / Non-Goals

**Goals:**

- Register all preset definitions in `gptel--known-presets` so upstream's full preset lifecycle (transient menu, `@preset` cookies, `gptel-with-preset`, differential save/restore) works with our presets
- Separate mutable scope configuration from immutable preset definitions
- Remove `gptel-agent` package dependency (our `PersistentAgent` is the preferred sub-agent mechanism)
- Eliminate custom load/apply/serialize functions that duplicate upstream behavior
- Clean up vestigial scope code (v1.0 `scope-manager.org`, v2.0 templates in `scope-commands.org`)

**Non-Goals:**

- Change the preset file format (`.md` with YAML frontmatter remains the authoring format)
- Modify upstream gptel code
- Add preset caching or lazy loading
- Change the PersistentAgent tool's user-facing behavior
- Migrate existing sessions automatically (manual migration or graceful fallback is sufficient)
- Implement the `{{AGENTS}}` template expansion that gptel-agent provides (not currently used by any preset)

## Decisions

### 1. Own YAML frontmatter parser replacing `gptel-agent-read-file`

Write `jf/gptel-preset--parse-file` (~80 lines) that reads a `.md` file, extracts YAML frontmatter between `---` delimiters, parses it with `yaml-parse-string`, and returns the body text as `:system`.

**Why not keep gptel-agent just for the parser?** The parser is the only functional dependency on gptel-agent. Keeping the entire package (with its Agent tool, template expansion, two-pass scanning, agent registry) for ~80 lines of parsing is unnecessary coupling. The `yaml.el` package is already available.

**Why not contribute to upstream?** Core gptel deliberately has no file parsing — presets are registered via elisp `gptel-make-preset` calls. File-based definitions are the domain of extensions. Our parser is simple enough that it doesn't warrant an upstream contribution.

### 2. YAML coercion fixes at parse time

The YAML parser produces values that don't match gptel's expectations in three cases:

| Field | YAML produces | gptel expects | Fix |
|---|---|---|---|
| `:model` | `"claude-opus-4-5"` (string) | `'claude-opus-4-5` (symbol) | `(intern model-string)` |
| `:confirm-tool-calls` | `"nil"` / `"auto"` (strings) | `nil` / `'auto` (symbol/nil) | Map known string values to lisp equivalents |
| `:include-tool-results` | `:false` (keyword) | `nil` | `(when (eq val :false) nil)` |

These fixes live in `jf/gptel-preset--coerce-plist`, applied immediately after YAML parsing before registration. This is a parse-time concern, not a registration-time concern — `gptel-make-preset` stores raw plists and `gptel--apply-preset` handles resolution of backends and tools but not these YAML-specific coercion issues.

**Alternative considered:** Contributing coercion fixes to `gptel-agent`'s parser. Rejected because we're dropping `gptel-agent` and these are inherent limitations of YAML-to-elisp conversion that any file-based preset system must handle.

### 3. Scope extraction at parse time, preset registration without scope keys

When parsing preset files, scope-related keys (`:paths`, `:org_roam_patterns`, `:shell_commands`, `:scope_profile`) are extracted from the plist before calling `gptel-make-preset`. This prevents upstream from warning about unknown keys and keeps presets clean.

Extracted scope data is stored in a separate alist `jf/gptel-preset--scope-defaults` mapping preset names to scope plists:

```
jf/gptel-preset--scope-defaults:
  (executor . (:scope-profile "coding" :paths (:read ...) ...))
  (research . (:scope-profile "research"))
  (plan     . (:scope-profile "coding" :paths (:read ...) ...))
```

At session creation, the scope profile is resolved: if a preset specifies `:scope-profile`, that profile template is used. If a preset also has inline scope keys (`:paths`, etc.), they serve as the default scope and a corresponding profile is created implicitly or merged with the named profile.

**Transition approach:** Initially, preset files retain their scope sections alongside a new `scope_profile:` key. The parser extracts both. Over time, scope sections migrate fully to profile templates and are removed from preset files.

### 4. Scope profiles as standalone YAML files

Scope profiles live in `config/gptel/scope-profiles/` as plain YAML files (no frontmatter, no body):

```yaml
# config/gptel/scope-profiles/coding.yml
paths:
  read: ["/**"]
  write: ["${project_root}/**"]
  deny: ["**/.git/**", "**/runtime/**", "**/.env", "**/node_modules/**"]
org_roam_patterns:
  subdirectory: ["gptel/**"]
shell_commands:
  allow: ["ls", "find", "grep", "git", "rg"]
  deny: ["rm -rf", "sudo"]
```

Session-level `scope.yml` has the same format — it's created by copying the profile template and is freely mutable via scope expansion.

**Why not derive scope from tools?** Tools tell you what capabilities are available, but not what resources they can access. A preset enabling `Read` and `Write` tools doesn't tell you which directories are writable. Scope profiles make this explicit.

**Why YAML and not elisp?** Scope config is read by the scope validator (elisp) but is conceptually a data document, not code. YAML is already the format used in the existing scope system and in preset frontmatter. Plain YAML (no frontmatter) is simpler to parse and serialize than the current approach of manipulating YAML embedded inside markdown frontmatter.

**Variable expansion:** Profile templates support `${project_root}` which is resolved at session creation time from the session's project context. This replaces the current approach of injecting project paths into `preset.md` at session creation.

### 5. Session directories: `scope.yml` replaces `preset.md`

Session branch directories change from:

```
branches/main/
  session.md        # conversation + Local Variables
  preset.md         # model config + scope + system message (mutable)
  scope-plan.yml    # metadata only
```

To:

```
branches/main/
  session.md        # conversation + Local Variables (includes gptel--preset)
  scope.yml         # mutable scope config (from profile template)
  metadata.yml      # session metadata (renamed from scope-plan.yml)
```

**What happened to `preset.md`?** The preset name is stored in `session.md`'s Local Variables as `gptel--preset`. The actual preset definition lives in `gptel--known-presets` (registered at init from the file in `config/gptel/presets/`). No copy is needed in the session directory.

**What happened to `scope-plan.yml`?** Renamed to `metadata.yml`. It was already metadata-only (session_id, timestamps, preset name). The name "scope-plan" is a vestige of v1.0 when it held scope permissions.

**Why remove `preset.md` from sessions?** It served three purposes: (1) model/backend config — now in `gptel--preset` Local Variable + upstream restore, (2) system message — now in the registered preset, (3) scope config — now in `scope.yml`. With all three purposes addressed, the file is redundant.

### 6. Delegate restore entirely to upstream

On session resume (`find-file` on `session.md`):

1. `gptel-mode` activates (from Local Variables)
2. `gptel--restore-state` runs (upstream, automatic):
   - Reads `gptel--preset` from Local Variables
   - Looks up preset in `gptel--known-presets`
   - Applies preset (backend, model, tools, system message, temperature, etc.)
   - Overlays any explicitly saved overrides (values user changed during session)
3. Our `find-file-hook` runs:
   - Sets session buffer-local variables (`jf/gptel--session-id`, `jf/gptel--branch-dir`, etc.)
   - Loads `scope.yml` for scope enforcement
   - Registers in session registry

Steps 2 and 3 are independent — upstream handles model/tool config, we handle session identity and scope. Our `jf/gptel--load-preset-from-file` and `jf/gptel--apply-session-preset` are no longer called.

**What if the preset was deleted/renamed since the session was saved?** Upstream's `gptel--restore-state` warns but continues. The session opens with whatever Local Variable overrides were saved. This is acceptable degradation — the user can re-apply a preset from the transient menu.

### 7. PersistentAgent uses `gptel--known-presets` for agent config

Currently PersistentAgent creates an agent directory with a copied `preset.md` and loads config from it. After this change:

1. Agent directory contains `scope.yml` (copied from parent or from preset's default scope profile) and `metadata.yml`
2. Agent config is applied via `gptel-with-preset` using the preset name (already partially done)
3. The preset name comes from the `PersistentAgent` tool's `agent_type` parameter, which maps directly to a registered preset name

The zero-inheritance principle is preserved: agent config comes entirely from the named preset in `gptel--known-presets`, not from the parent session. The agent's `scope.yml` is independent of the parent's.

**Alternative considered:** Having agents inherit the parent's scope with additional restrictions. Rejected — zero inheritance is a deliberate safety constraint documented in the existing spec.

### 8. Drop `gptel-agent` package entirely

With the parser replaced and PersistentAgent not using `gptel-agent` APIs, we remove:

- `use-package gptel-agent` block in `gptel.org`
- `(require 'gptel-agent)` in `persistent-agent.org` (was unused)
- `(require 'gptel-agent)` in `sql-tools.org` (was unused)
- `gptel-agent-read-file` calls in `sessions/commands.org` and `scope/scope-commands.org`
- Skills expansion advice on `gptel-agent-update` (adapts to iterate `gptel--known-presets`)
- Default tools: `(setq-default gptel-tools (list (gptel-get-tool "Agent")))` removed; default tools come from presets instead of a global default

**What about the `Agent` tool?** It's replaced by `PersistentAgent`. Preset files that list "Agent" in their tools section (only `executor.md`) are updated to use "PersistentAgent".

**What about `{{AGENTS}}` template expansion?** Only `executor.md` uses this. It's replaced with a static or dynamically-generated agent list in the system prompt. Since skills `@mention` expansion already handles dynamic content injection into system prompts, the `{{AGENTS}}` functionality can be replicated as a skill if needed.

### 9. Registration timing and load order

Preset registration runs in `gptel.org`'s `:config` block, after gptel and yaml.el are loaded but before session hooks fire:

1. `(require 'yaml)` — ensure parser is available
2. `jf/gptel-preset-register-all` — scan `config/gptel/presets/`, parse each, coerce, strip scope, register
3. Skills `@mention` expansion — iterate `gptel--known-presets` and expand any `@skill` tokens in `:system` strings
4. Session hooks — `find-file-hook` for session detection/restore

This ordering ensures presets are registered before any session opens, which is required for upstream's `gptel--restore-state` to find presets by name.

## Risks / Trade-offs

**[Preset not found on restore]** If a preset file is renamed or removed, existing sessions referencing it by name will warn on restore. → Upstream handles this gracefully (warns, continues with saved overrides). Low severity — user can re-apply a preset.

**[YAML coercion edge cases]** Our coercion fixes handle known cases but YAML has many edge cases (e.g., `on`/`off` parsed as booleans, numbers with leading zeros). → Mitigated by keeping preset files simple and documenting expected value formats. Can add coercion rules as edge cases are discovered.

**[Scope profile variable expansion]** `${project_root}` in profile templates requires knowing the project root at session creation time. → Resolved from `projectile-project-root` or the session directory's parent. Falls back to the session directory itself if no project is detected.

**[Existing session migration]** Sessions created before this change have `preset.md` but no `scope.yml` or `gptel--preset` Local Variable. → The session open hook detects legacy format (presence of `preset.md`, absence of `gptel--preset` in Local Variables) and either: (a) reads the old `preset.md` to determine the preset name and scope, writes `scope.yml`, or (b) opens in a degraded mode with a warning suggesting migration.

**[Loss of `gptel-agent` interactive command]** Users who used `M-x gptel-agent` to create agent buffers lose that entry point. → Our session creation commands (`jf/gptel-persistent-session`) are the preferred entry point and offer more functionality (persistence, branching, scope).

**[No global default tools]** Currently `gptel-tools` defaults to `(list (gptel-get-tool "Agent"))`. After removing gptel-agent, there's no global default. → Presets define their own tools. Non-session gptel buffers (ad-hoc `M-x gptel`) start with no tools, which is upstream's default behavior. Users can set a default preset via gptel's transient menu.

## Migration Plan

### Phase 1: Add new capabilities alongside old

1. Add `jf/gptel-preset--parse-file` and `jf/gptel-preset-register-all`
2. Add scope profile templates in `config/gptel/scope-profiles/`
3. Register presets via `gptel-make-preset` at init (in addition to existing `gptel-agent` flow)
4. Verify presets appear in transient menu and `gptel-with-preset` works

### Phase 2: Switch session creation to new model

1. Session creation uses `gptel--apply-preset` by name instead of custom load/apply
2. Session creation writes `scope.yml` from profile instead of copying `preset.md`
3. Session creation writes `metadata.yml` instead of `scope-plan.yml`
4. Add legacy detection in session open hook (handles old-format sessions)

### Phase 3: Remove old code

1. Remove `gptel-agent` dependency and `use-package` block
2. Remove `jf/gptel--load-preset-from-file`, `jf/gptel--apply-session-preset`, `jf/gptel--normalize-preset-for-serialization`
3. Remove system message save-prevention advice
4. Remove `scope-manager.org` (v1.0) and v2.0 templates from `scope-commands.org`
5. Remove scope sections from preset files, update `executor.md` tools list
6. Clean up stale requires in `persistent-agent.org` and `sql-tools.org`

### Phase 4: Update scope system

1. `scope-core.org` reads from `scope.yml` instead of `preset.md`
2. `scope-expansion.org` writes to `scope.yml` instead of `preset.md`
3. `scope-commands.org` tool parsing reads from registered preset in `gptel--known-presets`
4. Branch creation copies `scope.yml` and `metadata.yml`

### Rollback

Each phase is independently revertible via git. Phase 1 is purely additive. Phase 2 can fall back to old-format session creation. Phase 3 is the point of no return for `gptel-agent` removal — but since PersistentAgent doesn't use it, the practical risk is low.

## Open Questions

1. **Scope profile inheritance:** Should profiles support a `:parent` key for layered composition (e.g., `restricted.yml` inherits `coding.yml` but tightens write paths)? Or is flat-file-per-profile sufficient?

2. **Ad-hoc gptel buffers:** Non-session `M-x gptel` buffers currently get the "Agent" tool by default. After this change they get no tools. Should we set a default preset for ad-hoc buffers, or is toolless the right default?

3. **Preset file hot-reload:** Should we support re-registering presets when files change (e.g., via `auto-revert` or a manual refresh command)? Or is init-time-only registration sufficient?
