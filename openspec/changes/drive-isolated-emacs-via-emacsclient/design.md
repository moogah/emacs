## Context

This change lets an external shell-capable LLM client (Claude Code, via its Bash
tool) drive Emacs through `emacsclient`: both **isolated instances** (for testing a
worktree's configuration) and the user's **live session** (for using Emacs as an
API over the user's data, and for preparing demonstrations of completed work). See
`proposal.md` for motivation and `specs/` for the behavioral contract.

It began as an isolated-targets-only change that superseded
`connect-external-llm-clients-mcp-broker` (an in-Emacs MCP broker built on `emcp`).
Hands-on verification showed `emacsclient` is a more direct, richer channel than
any MCP tool surface for a shell-capable client. The scope was widened on
2026-10-10 to cover the live session; decisions D9–D11 were rewritten and D12–D18
added at that point (see *Superseded decisions* at the end).

**Prior art surveyed:**
- **emcp** (Elisp, in-process MCP-over-HTTP) — rejected as a dependency (its
  transport stack is what this change exists to avoid). Its operation set informed
  the introspection tools (D10).
- **emacs-jail-mcp** (Go broker + Podman/Xvfb) — container isolation rejected as
  macOS-hostile; only its lifecycle-as-operations shape informs D3.
- **llm-test** (Elisp) — source of the display-free frame-state-as-JSON idea (D5).
- **ragmacs** (gptel introspection tools) — installed during early gptel
  exploration; last commit 2025-08-02. Its tools are thin wrappers over built-ins
  and it also ships test/demo tools. Not depended on (D10).

**Mechanism verified live (Emacs 30.1, macOS, 2026-06-27):**
- `emacs --daemon=<name>` (or any Emacs calling `(server-start)`) creates a named
  Unix-domain socket at `$TMPDIR/emacs<uid>/<name>`. `emacsclient -s <name> --eval
  '(form)'` evaluates the form in that Emacs and prints the result.
- `bin/emacs-isolated.sh --daemon=<name>` loaded the full config headless; the
  script's arg pass-through forwards `--daemon` untouched.
- A frameless daemon has one internal terminal frame; `emacsclient -s <name> -c` /
  `-t` attaches a real GUI / TTY frame on demand.

**Findings from exploration (2026-10-10, against main after rebase):**
- **The live session holds the default socket** (`$TMPDIR/emacs<uid>/server`). A
  bare `emacsclient --eval` with no `-s` reaches the live session. `org-roam.org`
  calls `(server-start)` only `(unless (server-running-p))`, so isolated instances
  never take the default socket.
- **gptel's registry** (`gptel--known-tools`, gptel 0.9.9.3) is an alist of
  category → (name → `gptel-tool` struct). Structs carry `function`, `name`,
  `description`, typed `args`, `async`, `category`, `confirm`, `include`. gptel
  invokes tools by mapping JSON-decoded plist arguments positionally in `:args`
  order (`gptel--handle-tool-use`, gptel-request.el:1684-1750); async tools take a
  callback as their first argument. There is **no read-only flag**, and `:confirm`
  is not a reliable safety signal — several file writers (`create_file`,
  `create_roam_node`, `org_graph_write_node`) do not set it. The registry holds
  ~80 tools, including shell runners, unconfirmed file writers, interactive UI
  tools, scope tools that deny-all outside a session buffer, and ragmacs's
  `elisp_eval`.
- **Isolation boundary.** `bin/emacs-isolated.sh` redirects only
  `EMACS_USER_DIRECTORY` → `runtime/`. Databases (vulpea, org-roam, org-id) are
  therefore per-worktree; every note path (`org-graph-vault-root`,
  `org-graph-roam-root`, `jf/org-directory`, `org-roam-directory`, agenda, capture,
  gptel dailies) still points at the real `~/org/`. vulpea autosync (fswatch or
  polling) runs in every instance that loads org-graph. org-graph's write
  coordinator is an in-process hash table — it does not coordinate across
  processes. Several of those paths are fixed at module load, before command-line
  `--eval` runs.
- **Workspaces layouts.** A layout is a named `window-state-get` snapshot stored
  in the workspace plist (`:saved-state` / `:working-state`), persisted in
  `state/workspaces/<role>/workspaces.eld`. Every capture path snapshots the
  *selected* frame of the *current* tab; the current workspace is derived from the
  selected frame's tab name; tab switches trigger an autosave into
  `:working-state`, which beats `:saved-state` on restore. All accessors are
  `workspace--` internal; org-graph already reads `workspace--registry` directly.
  The pure data layer (`workspace--upsert-group`, `workspace--layout-make`) makes
  non-disruptive layout creation cheap to add.
- **Autosave is deliberate, not a hazard for the agent.** The saved/working split
  (from `refine-workspaces-two-state-layout`, modelled on activities.el) captures
  `:working-state` only before a tab switch, on idle (`workspaces-mode`) and on
  kill-emacs, gated by `workspace-anti-save-predicates`. An agent that addresses
  workspaces by name and never switches tabs never triggers it. Switching to an
  already-open tab restores tab-bar's own stored window configuration; workspaces
  re-applies a layout only on `workspace-restore` / `workspace-switch-layout`, all
  through the single choke point `workspace--restore-frameset`.
- **Buffer membership is per tab (bufferlo).** Building windows in the user's
  frame — even inside `save-window-excursion` — adds those buffers to the current
  workspace's membership.

## Goals / Non-Goals

**Goals:**
- A target policy that says when to drive the live session vs an isolated target,
  and what each permits (D12), enforced at a single chokepoint (D13).
- A curated, discoverable tool surface harvested from gptel's registry and
  allowlisted by two presets (D9, D11), invoked through an in-Emacs dispatcher
  (D14).
- Owned read-only introspection tools (D10).
- Isolated targets with an explicit data profile (D3, D15).
- `jf/frame-state` and the control helpers for isolated testing (D5, D6).
- The workspaces improvements the demonstration use case depends on (D16) and the
  `workspace_add_layout` tool as the demonstration MVP (D17).
- A minimal static seed (skill + `CLAUDE.md` pointer) (D9/L3).
- Buttercup-tested, with guarded real-daemon smokes (D7).

**Non-Goals:**
- MCP in any form, a broker process, or an `emcp` / ragmacs dependency.
- Filesystem sandboxing / containers.
- Switching layouts in response to the user's actions, ordered walkthrough steps,
  narration (future work; D16 W5/W7 lay groundwork).
- The pane-description format for layouts beyond a minimal MVP (to be explored
  practically).
- Screenshot observation in v1 (`jf/frame-state` is the contract).
- Exposing the whole gptel registry: only preset-allowlisted tools are callable
  through `call` / `call-write`.

## Decisions

### D1. `emacsclient` is the transport; the shell is the client — no broker

The client drives Emacs with `emacsclient`. There is no intermediary process and
no MCP layer. *Alternative considered:* an MCP broker (the discarded change) —
rejected: it adds a dependency, a single-threaded multiplexer and a fixed tool
vocabulary to serve a capability the shell already has. MCP would only earn its
place for a shell-less client; none is in scope.

### D2. `server-start`, not `--daemon`, is the requirement; `--daemon` is the default launch

Control needs a named server socket. `--daemon=<name>` is the convenient way to get
a headless, backgrounded server with a unique name, so it is the default launch for
isolated targets. An interactive Emacs that calls `(server-start)` — such as the
live Emacs.app session — is equally controllable.

### D3. Isolated targets are addressed by socket name; the "registry" is the socket directory

An isolated target is started from a worktree with a name and a data profile (D15)
via `bin/emacs-agent target start` (D13), which wraps
`bin/emacs-isolated.sh --daemon=<name>` (full config) or `emacs -Q --daemon=<name>`
(bare). The socket name is the identifier; listing targets = listing
`$TMPDIR/emacs<uid>/` (excluding `server`, the live session); dispose =
`(kill-emacs)` over the socket. No persistent registry.

### D4. Concurrency and crash containment come free from the OS

Each `emacsclient` call is its own OS process with its own timeout. A hung or
crashed isolated target cannot block calls to another target or the live session.
The live session's own main thread is protected separately by D14's short-call rule.

### D5. A target-side support module provides observation + control helpers

`jf/frame-state` (display-free JSON of current buffer — bounded, with a truncation
indicator — windows, point, minibuffer, recent echo-area lines) and the
`type-text` / `send-keys` / `run-command` helpers ship in an ordinary literate
module. Full-config targets load it as part of the config; `-Q` targets load it on
demand via `eval`. The module defines inert functions only (no server start, no
hooks), so its presence in the live session is harmless — but its control helpers
are not added to either preset: driving keystrokes in the live session is out of
scope.

### D6. Frames are attached on demand

Headless targets serve eval and frame-state with no frame. For keymap/redisplay-
dependent behavior, or for a visible behavior demonstration of unmerged code, the
client attaches a frame with `emacsclient -s <name> -c` (GUI) or `-t` (TTY).

### D7. Testing approach — hybrid (mock the boundary + guarded real-daemon smokes)

- **Framework:** Buttercup (`*-spec.el`).
- **Location:** `config/<module>/test/` with `unit/` and `behavioral/`, plus
  `helpers-spec.el`; workspaces changes test under `config/workspaces/test/`.
- **Unit:** frame-state serialization/truncation; control helpers; dispatcher
  argument mapping (JSON → positional, vectors, `:json-false`, missing optionals)
  against stub `gptel-tool` structs; tier enforcement (preset membership);
  manifest generation; introspection tools; workspaces add-layout, agent-write
  rules (R1–R4) and recipe → window-state expansion against synthetic registries.
- **Behavioral smokes (guarded, skippable):** one `-Q` daemon end-to-end (start →
  eval → frame-state → dispose); one full-config daemon with a `scratch:` data
  profile exercising `call` / `call-write` against org-graph in a temp vault.
  Neither smoke ever targets the live socket.
- **Wrapper:** argument parsing and target resolution are exercised by the
  behavioral smokes; the live-socket guard (refuse an implicit target) is unit
  tested by pointing the wrapper at a fake socket directory.
- **Run:** `./bin/run-tests.sh -d config/<module>`.
- **Scenario mapping:** each `#### Scenario:` maps to one `it` block citing
  `specs/<capability>/spec.md § <Requirement>`.

### D8. Module layout (literate)

```
  config/<module>/                  (name open; e.g. emacs-agent/)
    <module>.org                    loader
    dispatcher.org                  jf/agent-call, tier check, JSON arg mapping, async protocol
    manifest.org                    (jf/agent-api) over preset-allowlisted tools
    presets.org                     emacs-agent-read / emacs-agent-write
    introspection-tools.org         owned read-only introspection gptel tools (D10)
    action-log.org                  live-session action log (D12)
    target-support.org              jf/frame-state + type-text/send-keys/run-command
    test/{unit,behavioral}/
  config/workspaces/                W1, W2, W4–W6 (D16) + agent-tools.org (workspace_add_layout, D17)
  early-init.el / config/core/defaults.org   data-profile path override (D15)
  bin/emacs-agent                   wrapper (D13)
  .claude/skills/emacs-agent/       static seed (D9/L3)
```

### D9. Discoverability — layered, generated from the live registry, minimal static seed

- **L0 — raw `eval`.** Always technically available; gated per target by D12.
- **L1 — curated tools.** gptel tools (D11) allowlisted by the `emacs-agent-read`
  and `emacs-agent-write` presets. The presets are the contract for what an agent
  may call through the API.
- **L2 — manifest.** `(jf/agent-api)` (surfaced as `emacs-agent <target> tools`)
  returns JSON `[{name, category, tier, description, args:[{name, type,
  optional, description}]}]` generated at call time from the presets and the
  registry, so it cannot drift from the code. Drill-down into a tool's function
  uses the introspection tools.
- **L3 — static seed.** A Claude Code skill plus a one-line `CLAUDE.md` pointer
  names the wrapper, the target policy (D12) and the manifest entry point. It does
  not enumerate tools.

*Alternative considered:* a fully static reference doc of operations — rejected,
it drifts; L2 replaces it.

### D10. Introspection tools are owned, thin, and registered as gptel tools

A small read-only set — initial candidates: describe symbol (function/variable
documentation), find definition / function source, variable value, apropos; maybe
find references and manual lookup — implemented over built-in Emacs (`apropos`,
`documentation`, `find-function-noselect`, `xref`, `Info-*`) and registered via
`gptel-make-tool` in category `"introspection"`, listed in `emacs-agent-read`.

*Rationale:* the operations are one-liners; owning them gives our naming, our
read/write split and our tests. *Alternatives rejected:* emcp (heavy transport
dependency tree); ragmacs (unmaintained since 2025-08, mixes test tools and
`elisp_eval` into the registry). Removing ragmacs from `community-tools` is a
candidate follow-up (`.tasks/`), not part of this change.

`-Q` targets have no gptel and therefore no curated tools or manifest. That is
acceptable: isolated targets permit `eval` freely (D12), so `-Q` drivers use raw
Elisp plus the target-support helpers.

### D11. gptel's tool registry is the registry; presets are the allowlists

There is no separate `jf/agent/*` registry. Modules define agent tools once, with
`gptel-make-tool` (as org-graph already does), and both frontends consume them:

```
  module ──gptel-make-tool──► gptel--known-tools
                                   │
              ┌────────────────────┴───────────────────┐
              ▼                                        ▼
  emacs-agent-read / -write presets          in-Emacs gptel sessions
              │                              (may use the same presets)
              ▼
  dispatcher + manifest ──► bin/emacs-agent ──► Claude Code
```

Preset tool lists are resolved to tool names with the existing
`jf/gptel-scope-profile--resolve-tool-names`. Tools are looked up by
`(category name)` or name via `gptel-get-tool`.

**Initial presets:**
- `emacs-agent-read`: `org_graph_query`, `org_graph_typed_edges`; org-roam read
  tools (`search_roam_nodes`, `list_roam_nodes`, `read_roam_node`,
  `get_roam_node_metadata`, `query_roam_backlinks`); the introspection tools (D10).
- `emacs-agent-write`: `org_graph_write_node`, `workspace_add_layout` (D17).

*Coupling accepted:* `gptel--known-tools` is internal to gptel. The dispatcher
isolates that access in one function so a gptel change touches one place.
*Alternatives considered:* a standalone registry that generates gptel tools (the
original D11) — rejected because tools are already being authored gptel-first,
which would mean registering twice or migrating them; tagging tools with a
`:read-only` property — rejected in favor of two presets, which need no new
metadata and line up with the permission tiers (D12) one to one.

### D12. Target policy: choose the target by what is at risk; gate by tier

**Choosing the target:**

| Situation | Target |
|---|---|
| Reading or writing the user's data through the API (org-graph, org-roam) | live |
| Preparing a demonstration in the user's workspace (layouts) | live |
| Exercising or demonstrating unmerged config code | isolated (worktree) |
| Developing data features (e.g. org-graph itself) | isolated with `scratch:` data profile |
| Exploratory / risky Elisp | isolated |

*Principle:* isolation protects against **bad code**, not against **damaged data**.
An isolated instance pointed at the real vault bypasses the live session's write
coordinator and its index; data writes belong in the live session, or in an
isolated instance with a scratch vault.

**Tiers per target:**

| Tier | Live session | Isolated target |
|---|---|---|
| `call` (read preset) | allowed | allowed |
| `call-write` (read ∪ write preset) | allowed | allowed |
| `eval` (raw Elisp) | only with explicit user agreement | allowed |
| loading uncommitted config code | never | the point of isolated targets |

**Live-session safeguards:**
- Calls are short and synchronous; nothing waits on the live session's main
  thread (D14 async protocol).
- Every live call (tier, tool or form, args summary, outcome) is appended to an
  in-Emacs action log buffer, so the user can audit what the agent did.
- Live calls must not change the selected frame, window configuration or current
  tab. `workspace_add_layout` stores a recipe and touches no frame or buffer (D17).

### D13. `bin/emacs-agent` is the enforcement chokepoint

```
emacs-agent live tools                         manifest (live)
emacs-agent live call       <tool> '<json>'    read preset only
emacs-agent live call-write <tool> '<json>'    read ∪ write preset
emacs-agent live eval       '<form>'           raw Elisp
emacs-agent target start <name> [--worktree PATH] [--bare] [--data live|scratch:<dir>|none]
emacs-agent target list | stop <name>
emacs-agent target <name> {tools|call|call-write|eval|state} ...
```

- **Explicit target always.** The wrapper never invokes `emacsclient` without
  `-s`; `live` resolves to the default `server` socket and fails clearly if no
  live session is running.
- **Permission mapping.** Distinct subcommands let Claude Code permission rules
  express the policy directly — e.g. allow `emacs-agent live call:*` and
  `emacs-agent target:*`, ask for `emacs-agent live call-write:*` (or allow, per
  user preference), ask for `emacs-agent live eval:*`.
- **Defense in depth.** Tier enforcement also happens in Emacs (D14): `call`
  rejects a write-preset tool even if invoked by hand.

This resolves the original open question about whether a `bin/` wrapper is needed:
the policy needs a chokepoint that permission rules can see.

### D14. The dispatcher invokes tools the way gptel does; async uses request + poll

`(jf/agent-call TIER TOOL-NAME JSON-ARGS)`:
1. Resolve the tool via the registry; reject if not in the tier's preset(s).
2. Decode JSON with `json-parse-string :object-type 'plist :null-object nil
   :false-object :json-false`; map to positional values in `:args` order,
   matching gptel's own mapping.
3. Sync tools: `apply` inside `condition-case`; return a JSON envelope
   `{ok, result | error}` via `gptel--to-string`-equivalent stringification.
4. Async tools: register a callback that stores the result under a request id and
   return `{pending, id}` immediately; `jf/agent-result ID` returns the stored
   result or `{pending}`. The wrapper polls so Claude Code sees one blocking call.
   The live session's main thread is never held in an `accept-process-output` loop.
5. Append to the action log when the target is the live session.

The initial presets contain only sync tools, so the async protocol may land after
the MVP; until then the dispatcher rejects async tools with a clear error.

Tools that depend on buffer context (the scope tools, which deny-all outside a
session buffer) and interactive tools (`ask_user_questions`) are excluded from the
presets.

### D15. Isolated targets carry a data profile, applied before modules load

`--data live|scratch:<dir>|none` selects which notes an isolated target can see
and write:

- `live` — today's behavior: real `~/org/` (writes bypass the live coordinator;
  use only when that is understood).
- `scratch:<dir>` — every note path points at `<dir>`; workspace-home indexing
  off; workspace state redirected to a temp dir.
- `none` — no vault; org-graph sync disabled.

Mechanism: the wrapper exports an environment variable (name open, e.g.
`JF_ORG_DIR`) read in `early-init.el` / `config/core/defaults.org` before modules
load, so `jf/org-directory` — and everything derived from it (`org-roam-directory`,
agenda files, capture file, gptel dailies) — follows the profile. org-graph's
`org-graph-vault-root` / `org-graph-roam-root` / `vulpea-default-notes-directory`
are derived from the same root rather than hard-coded to `~/org/`, so one variable
controls all of them. `org-graph-watch-workspace-homes` and
`workspace-state-directory-override` are set by the profile. Command-line `--eval`
alone is insufficient because several of these values are fixed at module load.

### D16. Workspaces improvements this change depends on

The design does not bend around current workspaces behavior; it changes
workspaces where that behavior gets in the way. These are a candidate first
split-off change. Status after the 2026-10-10 W4 / recipe discussion:

```
W1  public, name-addressed API                  MVP
W2  add-layout without switching                MVP
W3  capture from a given frame                  DROPPED (recipes need no frame)
W4  agent-write rules R1–R4 (autosave kept)     MVP
W5  layout metadata                             MVP (R2/R4 depend on it)
W6  recipe layouts, minimal format              MVP (promoted from deferred)
W7  layout event hooks                          deferred
```

- **W1 — Public, name-addressed API.** List workspaces, list a workspace's
  layouts (with W5 metadata), get a workspace by name, and answer "is layout L on
  screen" (L is the recent layout of a workspace that has an open tab), without
  relying on the selected frame's tab. Replaces direct `workspace--registry`
  access (including org-graph's).
- **W2 — Add a layout without switching.** `workspace-add-layout (ws name saved
  &optional metadata)`: upsert into the named workspace and persist; never move
  the recent pointer, never touch any frame, tab or buffer. Works for workspaces
  with no open tab. Subject to W4's rules.
- **W4 — Agent-write rules; autosave unchanged.** The two-state autosave model
  stays as is (the user relies on tab-switch autosave; explicit save is rare;
  revert is the safety net). What W4 adds is how programmatic writes coexist with
  the user's working state:
  - **R1.** An agent-created layout stores its content in `:saved-state` with
    `:working-state` nil. Whatever the user does while viewing it drifts into
    `:working-state`; `workspace-revert` restores the agent's version.
  - **R2.** A programmatic write never replaces a **user-origin** layout; the
    caller must choose another name.
  - **R3.** A programmatic write may replace an **agent-origin** layout (discarding
    any working-state drift on it) **unless the layout is on screen**, in which
    case it is refused (MVP). Rationale: tab-bar restores an open tab's own window
    configuration, and the next autosave would bury the replacement under
    `:working-state`, so the update would be silently invisible. Later options —
    versioned names, or an "updated" flag picked up on next switch/revert — are
    deferred with W7.
  - **R4.** An explicit `workspace-save` over an agent-origin layout flips its
    origin to user, after which R2 protects it.
- **W5 — Layout metadata.** Use the reserved `:etc` slot for `:origin`
  (`user` / `agent`; layouts without it are treated as `user`), a description,
  and a source reference (e.g. an opsx change or cycle), so agent-made layouts can
  be listed, distinguished and cleaned up.
- **W6 — Recipe layouts.** `:saved-state` may hold either a captured window-state
  (today) or a tagged **recipe**: a declarative description of the layout. MVP
  format:

  ```elisp
  (:recipe :split columns            ; or rows
           :panes ((:file "/abs/a.org" :line 40)
                   (:file "/abs/design.md")))
  ```

  - **Apply** goes through the single choke point `workspace--restore-frameset`:
    if the effective state is a recipe, it is first expanded into a window-state
    (`workspace--recipe->window-state`) whose leaves carry `workspace-buffer`
    entries with `:filename` (and a position derived from `:line`), then the
    existing deserialize → deferred `window-state-put` path runs unchanged,
    including its generation-counter race guard. Window sizes are fractions, so
    expansion is frame-independent.
  - **Working state is always a captured blob.** After the user views a recipe
    layout, autosave writes a normal snapshot into `:working-state`; revert
    rebuilds from the recipe against the *current* files.
  - **An explicit save replaces the recipe with a snapshot** (and flips origin,
    R4).
  - **Persistence:** the writer emits `:version 4`; the reader accepts 3 and 4
    (a v3 file is a v4 file with no recipes and no metadata), so existing state
    files load without loss and are upgraded on the next flush, while older code
    rejects a v4 file with its existing version notice instead of misapplying a
    recipe.
  - **MVP scope:** file panes only. Non-file panes (magit status, diffs, …) and
    nested splits are the deferred pane-description work, which becomes "extend
    the recipe format".
  - *Alternatives considered:* building real windows in an **invisible frame** and
    capturing them — rejected: runs `after-make-frame-functions`, the macOS port
    is prone to flashing / focus theft with invisible frames, bufferlo creates
    per-tab state, and every file is visited in the live session (mode hooks,
    LSP). Building in the user's frame under **`save-window-excursion`** —
    rejected: pollutes the current workspace's buffer membership. **Synthesizing a
    window-state at creation time** — workable (no frame, no new stored form) but
    goes stale and leaves revert nothing declarative to rebuild from; it survives
    as the *apply-time* expansion step above.
- **W7 — Layout event hooks (deferred).** E.g. layout-added, as groundwork for
  switching layouts in response to the user's actions and for resolving R3's
  on-screen case without refusing.

### D17. `workspace_add_layout` is the demonstration MVP

A gptel tool (in `config/workspaces/agent-tools.org`, registered only when gptel
is loaded, mirroring org-graph's gating) in the write preset:

```
workspace_add_layout(workspace, layout_name, split, panes, description?, source?)
  split: "columns" | "rows"
  panes: [{file, line?}, ...]
```

The tool's arguments *are* the W6 recipe: it validates them (workspace exists,
files exist and are absolute or resolvable against the workspace home, line
numbers positive), then calls `workspace-add-layout` with the recipe as
`:saved-state` and agent-origin metadata (W2, W5). It opens no files, creates no
frames and touches no tabs; the layout materializes only when the user switches to
it. Refusals under R2 (user-origin name) and R3 (layout on screen) return a clear
error naming the conflict so the agent can pick another name. Switching to the
layout is the user's action.

### D18. Scope stays wide during exploration; split later

The change currently spans target policy, the tool surface, isolated targets,
workspaces improvements and the demonstration MVP. It will be split once the shape
is concrete. Likely seams: (1) workspaces improvements W1, W2, W4–W6; (2) data profiles
(D15); (3) the agent tool surface + policy + wrapper (D9–D14, D17); (4) isolated
testing helpers (D3–D6), which may stay with (3).

## Superseded decisions

- **W3 (capture from a given frame) and the invisible-frame build** → dropped in
  favor of recipe layouts (D16 W6, D17).
- **W4 as "rethink autosave-on-switch"** → autosave kept; W4 is now the
  agent-write rules R1–R4.
- **Original D9 (L1 `jf/agent/*` namespace + standalone tool-spec registry)** →
  replaced by gptel's registry + presets (D9, D11).
- **Original D10 (emcp baseline reimplemented as `jf/agent/*` functions)** →
  replaced by owned gptel introspection tools (D10).
- **Original D11 (frontend-agnostic registry; gptel adapter as a sibling change)**
  → inverted: gptel is the registry (D11). The sibling gptel-adapter change is no
  longer needed.
- **Non-Goal "live-session inspection/control is emcp's job"** → removed; the live
  session is in scope under D12.

## Risks / Trade-offs

- **Coupling to gptel internals** (`gptel--known-tools`, argument-mapping
  semantics) → isolated in one dispatcher function; covered by unit specs against
  stub structs; gptel is pinned via straight.
- **Agent harms the live session** → only preset tools without user agreement;
  explicit target naming; short synchronous calls; action log; no frame/tab changes.
  Residual risk: a buggy *tool* (not raw eval) still runs in the live session —
  tools in the write preset need tests that cover their side effects.
- **Preset drift** (a dangerous tool added to a preset by mistake) → presets are
  small and reviewed; the manifest makes the exposed surface visible.
- **Recipe layouts fail late** (a file moved after the layout was created fails
  when the user switches to it, not at creation) → validate at creation; on apply,
  the existing restore fallback chain yields an error buffer for the missing pane
  rather than aborting the whole layout.
- **Two stored forms for `:saved-state`** (snapshot or recipe) → a single tagged
  predicate decides; persistence round-trip specs cover both forms and a
  pre-change (v3) file.
- **Isolated targets on the real vault** bypass the write coordinator → documented
  in D12; `scratch:` profile for data-feature work.
- **Targets are not filesystem-sandboxed** → local-only Unix sockets; trusted
  clients only.
- **Orphan target daemons** → `emacs-agent target list` / `stop`; targets are cheap.
- **`frame-state` on huge buffers** → bounded output with a truncation indicator.

## Migration Plan

Additive, except for workspaces (W1, W2, W4–W6) and the early path override, which
change existing modules and need their own regression coverage. Suggested order:
(1) workspaces W1, W5, W6, W2 + W4 rules; (2) dispatcher, presets, manifest, introspection
tools, wrapper with `live` only; (3) `workspace_add_layout`; (4) data profiles and
`target start`; (5) target-support helpers and smokes; (6) skill + `CLAUDE.md`
pointer. Rollback per step: remove the module/tool/wrapper; workspaces changes
revert with their own commits.

## Open Questions

- **Change name / module name.** `drive-isolated-emacs-via-emacsclient` no longer
  fits; module candidate `emacs-agent`.
- **Write tier default on the live session.** Should `call-write` be allowed or
  ask by default in Claude Code permissions?
- **Preset visibility.** Should `emacs-agent-read` / `-write` appear in gptel's
  preset menu, or be hidden?
- **Introspection final cut.** Whether find-references and manual lookup earn
  their keep.
- **Async protocol timing.** In the MVP, or deferred until a preset needs an async
  tool?
- **Environment variable name** for the data profile root, and whether
  `config/local/<role>.el` should be able to override it.
- **Recipe format beyond the MVP** (non-file panes, nested splits, sizes) — the
  deferred pane-description work; to be explored practically.
- **Default data profile** for `emacs-agent target start` when `--data` is
  omitted (`none` is the safest; `live` matches today's behavior).
- **Screenshot tool** deferred until a graphical-target need appears.
