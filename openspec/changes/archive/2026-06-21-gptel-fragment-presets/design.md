## Context

Today a gptel preset is a `.md` file: YAML frontmatter → `gptel-make-preset`
config, and the Markdown body sent **verbatim** as `:system`. Registration
(`config/gptel/preset-registration.org`) parses YAML, normalizes snake→kebab
keys, coerces YAML types, and strips scope/mode keys. Separately, three pieces of
runtime framing are hard-coded:

- `gptel-chat--emacs-prelude` (`config/gptel/chat/menu.el`) — static, leads the
  chat-mode system message.
- `jf/gptel-persistent-agent--system-preamble`
  (`config/gptel/tools/persistent-agent.el`) — static, leads the agent system
  prompt, materialized at agent birth.
- `gptel-chat--build-environment-block` (`config/gptel/chat/menu.el`) — dynamic,
  appended at the tail on every send via a `:before` advice on `gptel-request`
  that covers both chat and agent buffers.

The repo is literate: edit `.org` → `./bin/tangle-org.sh` → `.el` loaded by
Emacs. New tests use Buttercup (`*-spec.el`) under `config/<module>/test/`. The
existing prelude/preamble/env are early prototypes — the user has confirmed they
carry no regression obligation and may be rewritten during migration.

## Goals / Non-Goals

**Goals:**
- One model — *fragment / composition / section* — for everything that flows
  into a system message. Presets, the prelude/preamble, and the env block are all
  fragments.
- Best-practice authoring: semantic Org sections rendered to backend-appropriate
  delimiters (Claude/XML), backend as a parameter.
- Promote `config/gptel/presets/` into a sub-module housing the renderer,
  composer, shared fragment sources, and the presets themselves.
- Two fresh presets — `workspace-assistant` (replaces `executor` for workspaces)
  and read-only `system-explorer` — that prove the model.
- Preserve compose-time cost profile (static pre-rendered, dynamic evaluated) and
  the pre-rendered sibling-file behavior.

**Non-Goals:**
- Multi-backend rendering beyond Claude (renderer takes `backend` as a seam only).
- Workspace command-palette tool wiring for `workspace-assistant` (near-future).
- Dynamic fragments at non-tail positions (allowed by the model, not exercised).
- Byte-for-byte preservation of the current prelude/preamble/env text.
- Per-send re-rendering of role content (sibling stays pre-rendered text).

## Decisions

### Decision 1 — Three-level model: composition → fragment → section

A **section** is a top-level Org heading (name = heading text, content = body). A
**fragment** is one `.org` file of ≥1 sections, declared **static** (rendered text
fixed at tangle time) or **dynamic** (text produced by a function at compose
time); kind defaults to static. A **composition** is an ordered fragment list the
**composer** renders and joins into the effective system message.

*Why:* Matches the user's "a fragment may contain multiple sections" and unifies
presets + framing under one concept. *Alternative considered:* fragment == one
section (rejected — forces many tiny files and loses the natural preset grouping).

### Decision 2 — Backend-parametrized renderer; Claude/XML only

`render(fragment, backend)` maps each section to a delimited block. For `claude`:
section `N` → `<tag>\nbody\n</tag>` where `tag = downcase(N)` with spaces → `_`
(e.g. `Output Format` → `<output_format>`, matching the best-practices brief).
The renderer dispatches on `backend`; an unimplemented backend is reported (signal
or log), never silently rendered as Claude/Markdown. Output is **never Markdown by
default** — the format is whatever the backend rendering specifies.

*Why:* One source, correct delimiters per model; backend stays a clean seam.
*Alternative:* author XML by hand in fragments (rejected — couples sources to one
backend, defeats the multi-backend seam).

**Tested with golden snapshot files** (user choice): expected rendered text lives
in `config/gptel/presets/test/golden/<name>.<backend>.txt`; specs assert
`(render …) :to-equal (read-golden …)`. Full output is eyeballable; diffs catch
regressions.

### Decision 3 — Context-derived default composition with an override seam

Default compositions are derived from send context, reproducing today's behavior
as data instead of `concat`:

```
chat   → [emacs-prelude(static), role, environment(dynamic)]
agent  → [agent-preamble(static), role, environment(dynamic)]
```

A preset author normally supplies only config + a role fragment. A composition
MAY override the default list (add/remove/reorder) via a seam, but nothing forces
that on the common case.

*Why:* "Keep it simple" — same mental model as today; extension is opt-in.

### Decision 4 — Static pre-render at tangle time; dynamic evaluate at compose time

Static fragments render to **committed text artifacts** at tangle time (diffable,
no per-send work). Dynamic fragments are functions the composer **evaluates at
compose time**; default position is the **tail** for prompt-cache friendliness.
The mechanism permits non-tail dynamic placement; documented guidance is tail-only
and "introduce a dynamic fragment only when a static one won't do."

*Why:* Preserves today's cost profile (prelude/role constant, env computed each
send) and the repo's tangle philosophy; keeps the cacheable static prefix stable.

### Decision 5 — `config/gptel/presets/` becomes a sub-module

Promote the presets directory into a sub-module (user choice), housing the whole
capability:

```
config/gptel/presets/
├── fragments.org/el            renderer + composer + fragment parsing
├── registration.org/el         tangled-preset loading → gptel-make-preset
│                               (replaces top-level preset-registration)
├── sources/                    shared fragment sources
│   ├── emacs-prelude.org   → emacs-prelude.txt        (static, pre-rendered)
│   ├── agent-preamble.org  → agent-preamble.txt       (static, pre-rendered)
│   └── environment.org/el                              (dynamic fragment fn)
├── workspace-assistant/        per-preset: role fragment + config block → .el
├── system-explorer/
└── test/
    ├── *-spec.el
    └── golden/*.txt
```

Load order (in `gptel.org`): fragments (renderer/composer) → registration →
presets → consumers (chat composer, agent writer, env seam). The `prompt-fragments`
spec capability is realized physically inside this sub-module; capability name and
directory need not match.

*Why:* "presets" is the natural umbrella now that presets, framing, and rendering
share one model; matches the gptel subsystem layout (sessions/, scope/, …).
*Alternative:* a separate `config/gptel/fragments/` dir (rejected per user
preference — fold rendering into the presets sub-module).

### Decision 6 — Preset = Elisp config block + role fragment, tangled to `.el`

A preset `.org` carries (a) an Elisp config block with native-typed keys
(`:backend`, `:model` symbol, `:tools` list, `:temperature`, `:description`,
scope/mode keys) and (b) a static role fragment. Tangling produces an `.el` that
registers via `gptel-make-preset` with `:system` = the rendered role text, scope
keys extracted into `jf/gptel-preset--scope-defaults`. **The YAML pipeline is
deleted** (parse / normalize / coerce gone — net deletion); scope extraction is
retained but reads native Elisp.

*Why:* Elisp config needs no coercion and tangles directly to a registration call
— simpler and repo-native. *Alternative:* keep YAML, read `.org` at load time
(rejected — keeps the coercion machinery and breaks the tangle-to-`.el` pattern).

### Decision 7 — Pre-rendered sibling preserved

The per-send `system-prompt.<ext>` sibling stays **pre-rendered text read
verbatim** each send (today's behavior). Role content does not re-render per send;
any genuinely dynamic content is a dedicated dynamic fragment (env). Mid-session
role edits remain uncommon and out of the hot path.

*Why:* Cheap compose-time, healthy caching, matches current behavior — user
confirmed mid-session prompt edits are rare.

### Decision 8 — Migration carries no regression net

The prelude/preamble/env migrate to fragment sources with **no byte-identical or
structural-equivalence assertion against the old defconst/defun** (user choice —
they are prototypes). New tests assert the *new* behavior (golden snapshots,
composition order, tail placement, idempotent rebuild), and the migration is free
to improve the framing text. Old `.md` presets are deleted **after** confirming no
snapshot/count test asserts the presets directory contents.

### Decision 9 — Two presets; workspace flip

`workspace-assistant` (general workspace helper, no palette tools yet) becomes the
workspace package's initial preset by flipping `jf/gptel-workspace-initial-preset`
from `'executor` to `'workspace-assistant`. `system-explorer` is read-only
(environment audit, no write/modify/deny-violating ops). Both author Claude as
backend, so single-backend rendering suffices.

## Risks / Trade-offs

- **[Deleting the YAML pipeline breaks `preset-registration` tests]** → Rewrite
  those specs for the fragment pipeline as part of this change; the capability
  delta already removes the YAML requirements.
- **[Old presets referenced by a snapshot/count test]** → Before deletion, grep
  for tests that load `jf/gptel-presets-directory` or assert a preset count
  (e.g. test-report snapshots); update/remove them.
- **[`yaml.el` dependency removal]** → Verify no other module requires `yaml`
  before dropping it from the preset path.
- **[Load-order regressions]** → The renderer/composer must load before
  registration and before the chat/agent/env consumers; sequence explicitly in
  `gptel.org` and cover with a load smoke test.
- **[Golden snapshots drift / churn]** → Keep golden files minimal and per-section
  where practical so diffs stay readable; regenerate intentionally, never blindly.
- **[Tangle-time rendering needs the renderer available at tangle]** → Static
  pre-render runs as a babel/build step in the preset `.org`; ensure the renderer
  is loadable in batch (no interactive deps).

## Migration Plan

1. Build the fragment sub-module: parser, `render(fragment, backend)` (Claude),
   composer, context-default compositions. Golden-snapshot specs.
2. Move prelude, agent-preamble, environment to fragment sources; rewire the chat
   composer, agent system-prompt writer, and env pre-send seam to consume
   fragments. Remove the three defconst/defun literals.
3. Rewrite registration: load tangled preset `.el`; delete the YAML pipeline;
   retain native-Elisp scope/mode extraction. Update `preset-registration` tests.
4. Author `workspace-assistant` and `system-explorer` presets (config block +
   role fragment) and their tangled `.el`.
5. Flip `jf/gptel-workspace-initial-preset` → `'workspace-assistant`.
6. Verify no snapshot/count test depends on the presets directory; then delete the
   old `.md` presets (`executor`, `explore`, `plan`, `research`, `zettelkasten`,
   `perplexity-researcher`, `minimal`, `system-explorer`, `test-agent-*`).
7. Drop `yaml` from the preset path if no longer needed. Run the full gptel suite;
   refresh `test-report` snapshots.

*Rollback:* the change is additive until step 6; reverting the commit restores the
`.md` presets and the defconst/defun framing.

## Open Questions

- Do any `test-agent-*.md` fixtures back a live scope/registration test, or are
  they orphaned? (Resolve in step 6 grep before deletion.)
- Exact heading→tag casing for multi-word sections is set to `snake_case`
  (`output_format`); confirm against the best-practices brief's tag names if more
  sections are introduced.
- Where the dynamic `environment` fragment's function lives relative to the
  existing `gptel-chat--build-environment-block` body — wrap-and-relocate vs
  rewrite — to be settled during step 2.
