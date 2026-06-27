## Why

External LLM clients (Claude Code, and other MCP-speaking agents) have no way to
exercise new/under-development Emacs features in a throwaway instance — to load a
feature, drive it, and observe the result without a human at the GUI and without
risking a running Emacs. Today the only AI integration in this repo is gptel, which
runs the *inverse* direction: it makes Emacs the client reaching out to a model.
The missing direction is an **MCP server** that lets an external agent connect *in*
to Emacs.

This server is scoped specifically to **testing new features in isolated Emacs
instances**. Inspecting or driving the user's *live* editing session is explicitly
NOT a goal — `emcp` (https://codeberg.org/martenlienen/emcp) already covers that and
is the right tool for it. This repo already owns the hard part of the isolation:
worktree-isolated `runtime/` via `bin/emacs-isolated.sh`. The broker turns that into
an on-demand, externally-drivable test fixture.

## What Changes

- **New: an in-Emacs MCP broker, built by extending `emcp`** — a *dedicated,
  headless controller Emacs daemon* (never the user's live Emacs) that serves one
  MCP endpoint to external clients, maintains a registry of isolated *target* Emacs
  instances, and routes each tool call to the selected target. NOTE: because the
  broker is Elisp, the broker process is itself an Emacs — the architecture is two
  tiers of Emacs (one stable broker, N disposable targets), not a server plus an
  Emacs. There is no separate non-Emacs server process; the earlier "in-process vs
  external broker" framing was a false fork (every Elisp MCP server is in-process).
  The real, decided architecture: **reuse emcp's transport + session/handshake +
  `emcp-deftool` macro + `emcp--server-build`, and supply our own fan-out tools.**
- **New: async-first execution (load-bearing constraint)** — emcp's tool model is
  genuinely non-blocking: a handler receives a `send-result` callback and resolves
  it whenever ready, and the HTTP transport defers the response. To preserve this on
  a single-threaded Emacs, **targets MUST be driven via async `make-process` +
  per-call timeout that resolves `send-result`** — never synchronous `call-process`.
  This is what keeps one hung target from stalling the whole broker.
- **New: target lifecycle as first-class MCP tools** — clients `start` / `inspect`
  / `stop` / `dispose` isolated Emacs targets on demand (modeled on
  emacs-jail-mcp's `emacs_jail_control`). Every target is a throwaway test
  instance; there is no "attach to the live session" mode.
- **New: isolated targets reuse existing isolation** — a target is an Emacs
  *daemon* launched through `bin/emacs-isolated.sh` (full worktree config, isolated
  `runtime/` state dir) or `emacs -Q` (bare). The broker talks to it via async
  `emacsclient --eval`. No container, no Xvfb.
- **New: a control tool surface** — `eval-elisp`, `send-keys` (kbd notation),
  `type-text` (literal), `run-command` (interactive commands), routed to the
  active target.
- **New: display-free observation** — a `frame-state` tool returning the target's
  state as **JSON** (buffer contents, window configuration, minibuffer status,
  echo-area messages), borrowed from `llm-test`, so a client with no display can
  parse and assert on Emacs state. Screenshots are an optional, secondary path.
- **New: permissive-by-default trust model** — because every target is an isolated,
  disposable instance (never the live session), arbitrary `eval` and command
  execution are permitted by default with no confirmation gate. This is essentially
  free: emcp's *framework* has no gating engine — confirmation is opt-in code inside
  emcp's own `eval` tool, and its "profiles" are merely curated tool lists. Our
  tools simply omit the confirmation step. (Contrast: emcp must gate because it
  drives the live session; we avoid that problem by construction.)
- Establishes the broker's tool vocabulary deliberately close to the shape used by
  gptel's own tools, leaving the door open to a future shared tool interface
  (out of scope here, noted as forward synergy).

## Capabilities

### New Capabilities
- `mcp-broker`: The in-Emacs broker endpoint, built by extending emcp (its
  transport, MCP session/handshake, and `emcp-deftool`/`emcp--server-build`); the
  isolated-target registry; request routing to the active target; the target
  lifecycle tools (start / inspect / stop / dispose); the async-`make-process`
  execution discipline; and the permissive-by-default trust posture (no confirmation
  gate, justified by isolated-only targets).
- `emacs-control-tools`: The drive surface exposed over MCP — `eval-elisp`,
  `send-keys`, `type-text`, `run-command` — and how each is dispatched to a target.
- `frame-state-observation`: The display-free observation primitive — target frame
  state serialized to JSON (buffers, windows, minibuffer, echo area), plus the
  optional screenshot path and when each is used.
- `isolated-emacs-targets`: Spawning, addressing, and disposing isolated Emacs
  daemon targets via `bin/emacs-isolated.sh` (worktree runtime) or `emacs -Q`,
  including the `emacsclient` control channel and disposal semantics.

### Modified Capabilities
<!-- None. gptel is explicitly out of scope (inverse direction); no existing spec's
     requirements change. The broker is purely additive. -->

## Impact

- **New code area** (no existing module touched): a new literate `config/` module
  for the broker. Transport is now decided — **extend emcp** rather than build our
  own MCP layer (emcp is the only Elisp MCP server tracking the current spec,
  2025-11-25; mcp.el lags). Remaining design work: module layout, the target
  registry/lifecycle internals, the async-`emacsclient` proxy, remote frame-state,
  and a single adapter isolating our one coupling to emcp's internal
  `emcp--server-build`.
- **Reuses** `bin/emacs-isolated.sh`, the worktree/`runtime/` isolation model, and
  `emacsclient` as the daemon control channel. Likely needs `--daemon` support added
  to `emacs-isolated.sh`.
- **Dependencies**: `emcp` (vendored via straight.el, pinned to a commit) for the
  MCP transport, session/handshake, and `emcp-deftool` macro; this transitively
  pulls Emacs **30.1**, `http-server`, and (unused-by-us) `elisp-refs` +
  `magit-section`. Plus `emacsclient` and Emacs daemon mode. No Podman, Xvfb, or
  container runtime (explicitly rejected as macOS-hostile).
- **Design constraints / risks** (from source investigation of emcp):
  - *Async discipline (load-bearing)*: targets must be driven with async
    `make-process` + timeout resolving `send-result`; a synchronous `call-process`
    would let one hung target freeze the single-threaded broker. emcp's transport
    and tool model already support deferred results — we must not defeat them.
  - *emcp coupling*: injecting our own tool set cleanly uses `emcp--server-build`
    (a double-dash internal). Isolate behind one adapter fn and pin emcp to a commit
    so an upstream API change hits one place.
  - *Young dependency*: emcp is single-author and recent. Reused only for its
    protocol plumbing (we replace ~all of its tools, which assume local self-control;
    ours proxy to remote targets). Pinning + the adapter contain the risk.
- **Security/trust posture**: permissive by default — no confirmation gate. This is
  safe *with respect to the user's live Emacs session and editing state* (every
  target is a separate, disposable process with its own `runtime/`). It is **not a
  filesystem sandbox**: because the container was rejected, a target shares the host
  filesystem, so permissive `eval` could still affect host files. Acceptable for a
  user-driven testing tool exercising the user's own features; this server must not
  be exposed to untrusted clients. (The container's fs containment is the one
  protection deliberately not carried over; see the parked COW item for partial
  `runtime/`-state reset.)
- **Out of scope**: inspecting/driving the user's live Emacs session (that is
  `emcp`'s job, by design), gptel integration (inverse direction), in-Emacs LLM test
  orchestration (the `llm-test` orchestrator — only its frame-state-JSON and
  test-in-isolation ideas are borrowed), and container/Xvfb filesystem sandboxing.
- **Related, parked separately**: COW worktree-runtime seeding optimization lives
  at `.tasks/cow-clone-worktree-runtime-seeding.md` (discovered alongside this work,
  but independent of it).
