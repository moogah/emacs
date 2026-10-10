## Why

External shell-capable LLM clients (Claude Code, via its Bash tool) have no
supported, *safe* way to drive Emacs. The need turned out to be broader than the
original framing of this change (isolated instances for testing config changes):

1. **Testing config changes** — start an isolated Emacs loading a worktree's
   configuration under test, drive it, observe it, dispose of it. The original use
   case; still central.
2. **Using Emacs as an API over the user's data** — e.g. reading and writing
   `org-graph` nodes through the graph's own tool functions (deterministic IDs,
   taxonomy, `:agent-draft:` tagging, the write coordinator) instead of the agent
   hand-authoring org files and hoping they are well-formed.
3. **Demonstrating completed work and presenting asks** — after an
   `opsx-orchestrate` cycle, the agent prepares layouts in the relevant workspace
   (changed code, specs, related buffers) that the user can step into and be walked
   through; and for each open orchestrator ask, a layout holding the ask file
   beside the files it references.

Use cases 2 and 3 target the user's **live** Emacs session (Emacs.app), which the
original change explicitly excluded. The live session is the user's stable working
environment: arbitrary Elisp evaluated there can leave it in an unexpected state.
So the change now has to answer a question it previously side-stepped: **when is it
appropriate to drive the live session, when should an isolated instance be used,
and what is the agent allowed to do in each?**

`emacsclient` remains the transport (hands-on verification showed it is a more
direct and richer channel than the MCP broker this change originally replaced). The
preference order also stands: the agent uses a **curated API** of tool functions
first and falls back to ad-hoc Elisp only when no API exists — and on the live
session, only with the user's agreement.

Exploration (2026-10-10) established three facts that shape the design:

- **gptel's tool registry already is the API registry.** `org-graph` exposes its
  agent surface as `gptel-make-tool` definitions with typed argument metadata. The
  original plan to build a separate `jf/agent/*` registry and *generate* gptel tools
  from it is inverted: gptel's registry is the source, and the emacsclient surface
  harvests from it.
- **Isolation protects against bad code, not against damaged data.** An isolated
  instance has its own `runtime/` (package, vulpea, org-roam and org-id databases),
  but every note path still points at the real `~/org/`. Its writes bypass the live
  session's in-process write coordinator. Data operations are therefore *safer* in
  the live session; isolated instances are for exercising unmerged code — and need
  an explicit **data profile** (live / scratch vault / none) to be safe for data
  work such as developing org-graph itself.
- **The workspaces package cannot yet be driven programmatically.** Every layout
  operation captures the selected frame of the current tab, and every accessor is
  internal. Rather than working around this (e.g. building layouts in an invisible
  frame), the change identifies the improvements to `workspaces` it depends on —
  most notably declarative recipe layouts.

## What Changes

- **Target policy (new).** Two kinds of target — the **live session** (default
  server socket of the running Emacs.app) and **isolated targets** (named sockets,
  started from a worktree) — with a documented rule for choosing between them
  (data operations → live; unmerged code / risky experimentation → isolated) and a
  per-target permission tier:
  - `call` — invoke a tool from the read preset. Allowed on any target.
  - `call-write` — invoke a tool from the write preset. Allowed on the live session
    through the API (writes go through the owning module's coordinator).
  - `eval` — raw Elisp. Free on isolated targets; on the live session only with
    explicit user agreement.
- **A `bin/emacs-agent` wrapper (new)** as the single enforcement chokepoint. It
  requires the target to be named explicitly (bare `emacsclient --eval` silently
  hits the live session), exposes the tiers as distinct subcommands so Claude Code
  permission rules can allow / ask / deny each one, and starts / lists / disposes
  isolated targets.
- **Harvested tool surface (replaces the planned standalone registry).** The
  manifest lists gptel tools, curated by two gptel presets —
  `emacs-agent-read` and `emacs-agent-write` — which are the allowlists. An
  in-Emacs dispatcher invokes a tool by name with JSON arguments, mapping them the
  way gptel itself does, and enforces preset membership per tier. The same presets
  can drive an in-Emacs gptel session, so one curated list serves both frontends.
- **Owned introspection tools (new, replaces the emcp-derived baseline).** A small
  set of read-only introspection tools (describe symbol, find definition, variable
  value, apropos, …) implemented thin over built-in Emacs and registered as gptel
  tools. Neither emcp nor ragmacs is depended on.
- **Initial presets.** Read: org-graph query and typed-edges, org-roam read tools,
  the introspection tools. Write: org-graph write-node, `workspace_add_layout`,
  `workspace_remove_layout`.
- **Live-session safeguards (new).** Calls are short and synchronous only; async
  tools use a request-id + poll protocol so the live session's main thread is never
  held waiting; every live call is recorded in an in-Emacs action log.
- **Isolated targets (kept, extended).** Start / address / observe / attach-frame /
  dispose via `emacsclient` and socket names, as before, plus a **data profile**
  chosen at start: `live` (real vault), `scratch:<dir>`, or `none`. Requires a new
  early override (environment variable read before modules load) so `org-roam`,
  agenda, capture and gptel paths follow the profile, not just org-graph's.
- **Frame-state observation and control helpers (kept).** `jf/frame-state` and
  `type-text` / `send-keys` / `run-command` remain the drive-and-observe surface
  for isolated testing.
- **Workspaces improvements (new, likely the first split-off).** A public,
  name-addressed workspaces API; adding a layout to a named workspace without
  switching to it or moving its recent pointer; rules for how programmatic
  writes coexist with the user's autosaved working state (autosave itself is kept);
  layout metadata (origin, description, source reference); and **recipe layouts** —
  a declarative layout description stored as the saved state and materialized only
  when the user switches to it, so creating a layout needs no frame or buffers.
- **`workspace_add_layout` / `workspace_remove_layout` tools (new).** The MVP for
  the demonstration and ask use cases: the agent stores a recipe layout (nested
  splits of file panes) in a workspace — addressed by name or by a directory it
  owns — without touching the user's frames, tabs or buffers, and removes its own
  layouts when the artifact they present closes. Switching to it remains the user's action.
- **Discoverability seed (kept, reframed).** A Claude Code skill (plus a
  `CLAUDE.md` pointer) names the wrapper, the policy, and the manifest entry point;
  the manifest itself is generated live.

## Capabilities

### New Capabilities
- `agent-target-policy`: The two target kinds, the rule for choosing between them,
  the `call` / `call-write` / `eval` tiers and what each target permits, the
  `bin/emacs-agent` wrapper as the enforcement chokepoint and its mapping onto
  Claude Code permission rules, explicit target naming, and the live-session
  safeguards (short synchronous calls, async request/poll protocol, action log).
- `agent-introspection-tools`: The owned, read-only introspection tool set,
  registered as gptel tools.

### Modified (within this change, not yet archived)
- `isolated-emacs-targets`: Adds data profiles (`live` / `scratch:<dir>` / `none`)
  and the early path override they require; targets are started via the wrapper.
- `control-surface-discoverability`: The registry is gptel's tool registry curated
  by the `emacs-agent-read` / `emacs-agent-write` presets; the manifest and
  dispatcher operate over it; the static seed describes the wrapper and policy.
  The standalone `jf/agent/*` registry and the frontend-agnostic metadata seam are
  removed.
- `emacs-control-tools`: Raw eval becomes the `eval` tier, gated per target by
  `agent-target-policy`.
- `frame-state-observation`: Unchanged in substance.

### Modified Capabilities
- `workspaces`: Public name-addressed API; add-layout-without-switching;
  agent-write rules against working state; layout metadata; recipe layouts.
- `workspace-integrations`: Possibly a layout-added hook (deferred; see design).

## Impact

- **New code:** a literate `config/` module holding the dispatcher, manifest,
  presets, introspection tools and target-support helpers; `bin/emacs-agent`;
  a Claude Code skill.
- **Modified code:** `config/workspaces/` (W1, W2, W4–W6 in design), a gptel tool
  definition for `workspace_add_layout`, and an early path override in
  `early-init.el` / `config/core/defaults.org` read by org-roam, org, gptel and
  org-graph.
- **Depends on** `org-graph` (now on main) and gptel's tool registry
  (`gptel--known-tools`, an internal variable — a coupling accepted deliberately).
- **Dependencies:** none added. emcp and ragmacs are prior art only; removing
  ragmacs from `community-tools` is a candidate follow-up once the owned
  introspection tools exist (it also removes ragmacs's `elisp_eval` tool from the
  registry).
- **Security/trust posture:** control is local-only via Unix sockets. The live
  session is reachable only through the wrapper's tiers; raw Elisp there requires
  user agreement. Isolated targets are process- and runtime-isolated but not
  filesystem-sandboxed; their data profile decides which notes they can touch.
- **Follow-up outside this repo:** `opsx-orchestrate` creates an `ask/<id>`
  layout per open decision ask and removes it when the ask leaves the inbox,
  degrading gracefully when no live Emacs is running.
- **Likely splits:** the workspaces improvements are the first candidate to become
  their own change; the async protocol and demonstration sequencing may follow.
- **Out of scope:** MCP entirely; containers / filesystem sandboxing; switching
  layouts when the user acts and narrated walkthroughs (future work); the pane
  description format for layouts (to be explored practically later).
- **Name:** the change directory is still `drive-isolated-emacs-via-emacsclient`,
  which no longer describes its scope; renaming is pending a decision.
