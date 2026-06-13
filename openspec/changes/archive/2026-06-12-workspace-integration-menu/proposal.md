## Why

The workspaces package has no menu — its 12 commands hang off a `C-x w`
keymap (`workspaces.org:361-372`), undiscoverable and with no single entry
point for creation. And the scaffold pipeline's "initial gptel session"
(`workspace--scaffold-initial-session`, `scaffold.org:132`) writes a flat
`<date>-initial.org` stub containing only `#+TITLE:`, which fails every one
of gptel's auto-init gates (`commands.org:225`): wrong filename, wrong layout,
no `:PROPERTIES:` drawer. It is inert — opening it gives plain org, no
chat-mode, no preset, no scope.

That stub **cannot** be fixed in place: the directionality contract
`register/boundary/gptel-sessions-workspace-consult` forbids any
`config/workspaces/*.el` from naming a `gptel-sessions-*` symbol, so workspaces
is structurally unable to build a real session. Both problems point at one
missing primitive — a published **extension point** where external subsystems
hook into workspace creation and operation. gptel-session is its first,
proving client; git worktrees are the obvious second. A context-aware
transient menu is the surface that makes both discoverable.

## What Changes

- **New: a workspace integration registry.** A published extension point in
  `config/workspaces/` where external subsystems register an *integration*
  declaring an optional `:on-create` handler (auto-fires at workspace birth,
  non-interactive) and an optional `:menu` entry (on-demand, may prompt).
  Workspaces owns the registry and iterates it; it never names its consumers.
  This is a registry workspaces walks (like the existing
  `workspace-anti-save-predicates` defcustom) — **not** the `advice-add
  :override` anti-pattern (`activities-patterns-catalog.md` N1, explicit
  non-goal).

- **New: the integration contract (anchor payload + result protocol).**
  - *Push, not consult.* Workspaces hands each integration an **anchor
    payload** — a plist of workspace self-knowledge (`:name`, `:home`,
    `:sessions-dir`, `:context`). Integrations never reach for "the current
    workspace"; they source all domain-specific context themselves (a
    worktree integration scans repos / prompts for a name; workspaces never
    learns the word "branch"). Payload is an extensible plist (forward-compat
    for later pre-staged creation choices).
  - *Domain-agnostic outcomes.* Each integration reports `ok` / `skipped` /
    `failed`+reason. Workspaces interprets these generically — it never needs
    to know *why* an integration skipped, only that it did.
  - *Additive, never load-bearing.* Each integration runs inside an error
    guard; a failure becomes a `failed` result, never a thrown error.
    A workspace is valid **iff its own scaffold succeeded**; integration
    results decorate a workspace, they never define it. Failures are
    **visible but non-fatal** (a `*Messages*` notice naming the integration +
    reason; no rollback) — same philosophy as broken-home handling
    (`proposal.md:55-57`, "a visible broken state beats silent masking").

- **New: integration dispatch as a sibling of scaffold.** A
  `run-integrations` step fired by the `workspace--new-*` entry points
  (`tabs.org`) in **all three** birth contexts (`fresh`,
  `anchored-scaffolded`, `anchored-existing`), *after* registration — not
  buried inside the 6-stage scaffold pipeline. Scaffold lays down the
  workspace's own filesystem skeleton; integration-dispatch lets others react.
  The `:context` label is carried in the payload; each `:on-create` handler
  decides its own auto-fire rule (e.g. gptel skips `anchored-existing`).

- **New: a context-aware workspaces transient menu.** A single menu that is
  both the **creation front-door** and the operational surface, with three
  states detected from the current tab:
  - *No current workspace* → entry actions only (New, Switch, Restore).
  - *In a healthy workspace* → full surface (layouts, state, manage) **plus**
    a dynamically-populated *Integrations* group (registry-driven, via
    `:setup-children` like the skills menu, `skills-transient.org:291-351`).
  - *In a broken workspace* → recovery only (re-anchor, purge, delete);
    integrations suppressed (the anchor can't be built — consistent with
    `workspace-sessions-dir` returning nil for broken).
  The transient is where consult lives: it inspects the current workspace to
  decide what to show **and** to build the anchor payload it pushes into the
  chosen integration. It funnels into the same `workspace--new-*` internals
  as `M-x workspace-new` (one creation path, two callers).

- **gptel-session becomes the first real integration.** A handler on the
  gptel side (`config/gptel/sessions/`, which *may* name gptel symbols)
  registers into the workspaces registry: `:on-create` builds a real
  `<id>/branches/main/session.org` via `jf/gptel--create-session-core` scoped
  to the workspace home (skipping `anchored-existing`); `:menu` adds a session
  to the current workspace on demand. **The broken
  `workspace--scaffold-initial-session` stub is deleted** (scaffold stage 5).

- **Deferred (explicitly out of scope):**
  - The git-worktree integration (named as the second client to validate the
    registry isn't gptel-shaped, but not built here).
  - **β creation flow**: integration toggles/parameters chosen interactively
    in the transient *before* birth. The contract is designed to accommodate
    it (extensible payload), but birth-time stays auto-fire-with-defaults for
    this change.
  - Any change to gptel's own session internals beyond adding the handler.

## Capabilities

### New Capabilities

- `workspace-integrations`: the integration extension point — registry,
  registration shape (`:label`, `:on-create`, `:menu`), the anchor-payload
  push contract, the `ok`/`skipped`/`failed` result protocol, the
  additive-never-load-bearing safety invariant, and dispatch semantics
  (sibling of scaffold, fires in all three birth contexts, after
  registration, error-isolated, visible-but-non-fatal).

### Modified Capabilities

- `workspaces`: gains the context-aware transient menu (creation front-door +
  three operational states); creation entry points fire integration dispatch;
  the broken `workspace--scaffold-initial-session` stub and its scaffold stage
  are removed (sessions/ is created empty when no integration populates it).

## Impact

- **Code (new):**
  - `config/workspaces/` — new module(s) for the integration registry +
    dispatch and the transient menu (e.g. `integrations.org`,
    `workspaces-transient.org`).
  - `config/gptel/sessions/` — new handler registering the gptel-session
    integration (`:on-create` + `:menu`), built on the existing
    `jf/gptel--create-session-core`.

- **Code (modified):**
  - `config/workspaces/scaffold.org` — delete `workspace--scaffold-initial-session`
    and its pipeline stage; update the stage list / contract prose.
  - `config/workspaces/tabs.org` — `workspace--new-default-path` /
    `workspace--new-anchor-existing` fire integration dispatch after
    registration, in all branches, with the `:context` label.
  - `config/workspaces/workspaces.org` — the transient may supersede or
    augment the `C-x w` global bindings; `workspace-sessions-dir` becomes the
    transient's payload-building helper.

- **Specs:**
  - New `openspec/specs/workspaces/` capability spec sibling:
    `workspace-integrations` (extension-point contract).
  - Modified `openspec/specs/workspaces/spec.md` (transient surface; scaffold
    stub removal; dispatch trigger).

- **Tests:**
  - Workspaces side (Buttercup, `config/workspaces/test/`): registry
    registration/iteration; dispatch fires in all three contexts with correct
    payload + `:context`; error isolation (a failing integration leaves a
    valid workspace + visible notice); transient state-detection
    (no-workspace / healthy / broken) shows the right suffixes; directionality
    grep still passes (new workspaces code names no gptel symbol).
  - gptel side (Buttercup, `config/gptel/sessions/test/`): `:on-create` builds
    a real `session.org` with a drawer; skips `anchored-existing`; `:menu`
    adds to the current workspace; behavioral round-trip — open the produced
    file → chat-mode active, preset applied.

- **Contracts touched:** honors `register/boundary/gptel-sessions-workspace-consult`
  (workspaces publishes; gptel attaches). Adds a new published boundary: the
  integration registry + anchor-payload contract.

- **Dependencies:** no new external dependencies. gptel keeps its soft
  `featurep 'workspaces` posture; workspaces keeps its zero-knowledge posture
  toward gptel.
