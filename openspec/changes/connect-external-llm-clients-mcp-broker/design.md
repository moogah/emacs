## Context

This change adds an MCP server that lets external LLM clients (Claude Code, etc.)
spawn, drive, and observe **isolated** Emacs instances for the purpose of testing
new/under-development features — without a human at the GUI and without risking a
running Emacs. Inspecting the user's *live* session is explicitly not a goal (that is
`emcp`'s job). See `proposal.md` for motivation and `specs/` for the behavioral
contract.

Prior art surveyed (recorded in the exploration that produced this change):
- **emcp** (Elisp, in-process MCP-over-HTTP, current spec 2025-11-25) — we extend it.
- **emacs-jail-mcp** (Go broker + Podman/Xvfb) — we borrow its broker *shape* and
  lifecycle-as-tools, but reject the container/Xvfb (macOS-hostile). Its COW
  disposability idea is parked separately at
  `.tasks/cow-clone-worktree-runtime-seeding.md`.
- **llm-test** (Elisp) — we borrow its display-free *frame-state-as-JSON* observation
  and its test-package-in-isolation idea; its in-Emacs orchestrator is out of scope.

Decisions already fixed upstream of this document: Elisp implementation; isolated
targets only; permissive-by-default trust; a *dedicated headless broker daemon*
(never the user's live Emacs); async-first execution. Source investigation of emcp
confirmed its tool model is genuinely non-blocking (handlers receive a `send-result`
callback; the HTTP transport defers responses), that its "profiles" are merely
curated tool lists with no framework gate, and that its dependency floor is Emacs
30.1 plus `http-server`, `elisp-refs`, `magit-section`.

## Goals / Non-Goals

**Goals:**
- A dedicated broker (an Emacs daemon) that serves one loopback MCP endpoint and
  manages a fleet of isolated target Emacs daemons.
- Lifecycle (start / inspect / dispose), control (eval-elisp / send-keys / type-text
  / run-command), and observation (frame-state) exposed as MCP tools.
- Reuse emcp's transport + `emcp-deftool` + `emcp--server-build`; our differentiator
  (spawn / async proxy / registry / remote frame-state) is the only new protocol code.
- Stay responsive under a hung target (async `make-process` + per-call timeout).
- A literate `config/` module, Buttercup-tested, that is NOT loaded into the user's
  interactive Emacs — only into the broker daemon.

**Non-Goals:**
- Live-session inspection/control (emcp).
- Filesystem sandboxing of targets (rejected with the container).
- gptel integration / a shared tool interface (noted as future synergy only).
- Screenshot observation in this change (optional/secondary; deferred — frame-state
  is the v1 observation contract).
- The COW worktree-seeding optimization (parked in `.tasks/`).

## Decisions

### D1. Two tiers of Emacs: broker daemon + target daemons

```
  external client ──MCP/HTTP(loopback)──▶ BROKER (emacs -Q --daemon + broker module)
                                            │ make-process: emacs --daemon=<id>
                                            │ async emacsclient -s <id> --eval …
                                            ▼
                                          TARGET daemons (isolated runtime/, headless)
```
The broker is launched by a new `bin/mcp-broker.sh` as a dedicated `emacs -Q --daemon`
that loads the broker module and starts the server. The broker module is deliberately
NOT added to `jf/enabled-modules` — the user's editing Emacs must never start an MCP
server. *Alternative considered:* load the broker into the live Emacs (convenient in
dev) — rejected, it reintroduces "a bug here disturbs my session," the exact property
the isolated-only scope eliminated.

### D2. Extend emcp as a vendored transport; isolate the one coupling

Add emcp via straight.el, pinned to a commit. Reuse: HTTP/SSE transport, MCP
handshake/session/capabilities, `emcp-deftool`. Build the server by calling
`emcp--server-build :name … :tools (our-tools)`. Because that entry point is
double-dash internal, **all contact with it goes through a single adapter function**
(`jf/mcp-broker--build-server`) so an upstream change hits one place. Transport is
therefore HTTP/SSE (emcp's model); stdio is not pursued (fights Emacs's batch model,
and emcp is HTTP-based). *Alternative considered:* build our own MCP layer — rejected;
protocol compliance + spec-tracking is the expensive, non-differentiating work, and
emcp is the only current-spec Elisp server.

### D3. Targets are isolated Emacs daemons addressed by server-socket name

`start` launches either `bin/emacs-isolated.sh --daemon=<id>` (worktree config +
isolated `runtime/`; default mode) or `emacs -Q --daemon=<id>` (bare). The daemon's
unique server-socket name IS the target id; the registry maps id → struct {process,
socket-name, mode, status, start-time}. This requires adding a `--daemon[=NAME]` pass-
through to `bin/emacs-isolated.sh` (it currently `exec`s a GUI/batch Emacs). The
registry is an in-memory hash table on the broker; no persistence (targets are
ephemeral).

### D4. Async proxy with per-call timeout (the load-bearing rule)

Every control/observation call is dispatched by building an Elisp form and sending it
to the target via **async `make-process` running `emacsclient -s <id> --eval`**, with
a process sentinel that calls emcp's `send-result`, and a per-call timer that resolves
`send-result` with a timeout error if the target doesn't answer. Synchronous
`call-process`/`emacsclient` is forbidden — it would freeze the single-threaded broker
on a hung target. This is the concrete mechanism behind the spec's "a hung target does
not freeze the broker" scenario.

### D5. Targets are seeded with a small support library at spawn

`frame-state`, literal `type-text`, and key/command helpers need code present *in the
target*. On spawn, the broker loads a minimal `broker-target-support.el` into the new
daemon (via `emacsclient -s <id> --eval (load …)`), so even `-Q` targets gain the
helpers. `eval-elisp` needs no seeding. `frame-state` is implemented there: it walks
the current buffer (contents bounded/truncated with an indicator per spec), window
configuration, point, minibuffer state, and recent `*Messages*`/echo-area lines, and
returns JSON via `json-serialize`.

### D6. Tool surface → emcp-deftool handlers

Each tool is an `emcp-deftool` whose handler (a) validates the target id against the
registry (unknown/dead → structured error per spec), (b) builds the target-side form,
(c) dispatches via D4, (d) resolves `send-result`. `eval-elisp` returns the printed
value; `run-command` calls the named command via `call-interactively`; `type-text`
inserts literal text; `send-keys` runs `execute-kbd-macro` on the kbd-parsed sequence.

### D7. Testing approach — hybrid (mock the boundary + one real-daemon smoke)

- **Framework**: Buttercup (`*-spec.el`), per repo preference for new suites.
- **Location**: `config/mcp-broker/test/` with `unit/` and `behavioral/`, plus
  `helpers-spec.el` for shared fixtures/matchers (mirrors the scope/test layout).
- **Unit specs** mock the process/`emacsclient` boundary: `cl-letf` scoped to the
  function-under-test stubs the broker's "dispatch-to-target" seam to record the form
  sent and return canned JSON. These cover registry, routing, unknown-target errors,
  timeout handling, result/error formatting, and frame-state serialization/truncation
  — fast, no daemons.
- **Behavioral smoke** spawns ONE real isolated daemon (`emacs -Q --daemon`) and runs
  start → eval-elisp → frame-state → dispose end-to-end, asserting real integration.
  Guarded to skip when no suitable Emacs is available so constrained CI still passes.
- **Run**: `./bin/run-tests.sh -d config/mcp-broker` (or `-f buttercup`).
- **Scenario mapping**: each `#### Scenario:` in `specs/` maps to one `it` block, with
  a comment citing `specs/<capability>/spec.md § <Requirement>`.

### D8. Module layout (literate)

```
  config/mcp-broker/
    mcp-broker.org           loader + straight(emcp) + bin glue
    broker-core.org          server build adapter (D2), registry, routing, timeouts
    broker-targets.org       spawn/inspect/dispose, emacsclient channel (D3,D4)
    broker-tools.org         emcp-deftool definitions (D6)
    broker-frame-state.org   frame-state serializer (D5) — tangles into support lib
    broker-target-support.el target-side helpers seeded at spawn (D5)
    test/{unit,behavioral}/  Buttercup specs (D7)
  bin/mcp-broker.sh          launches the dedicated broker daemon
  bin/emacs-isolated.sh      + new --daemon[=NAME] pass-through (D3)
```

## Risks / Trade-offs

- **Single-threaded broker could stall on a hung target** → D4: async `make-process`
  + per-call timeout; synchronous dispatch forbidden by design and covered by a unit
  spec.
- **Coupling to emcp internal `emcp--server-build`** → D2 adapter + pinned commit;
  one place to fix on upstream change.
- **Emacs 30.1 floor + two unused transitive deps** (`elisp-refs`, `magit-section`)
  → accepted (user confirmed); pinned via straight.
- **Headless daemons have no frame**, which key-driven tools (`send-keys`,
  `run-command`) may need for keymap/interactive context → see Open Questions; `eval-
  elisp` + `frame-state` need no frame and are the reliable core.
- **frame-state on huge buffers** → bounded/truncated output with an indicator (spec
  requirement) implemented in D5.
- **Targets are not filesystem-sandboxed** → endpoint is loopback-only and the server
  is documented as trusted-clients-only; consistent with the spec's documented
  isolation boundary.
- **Real-daemon smoke tests are slower / could be flaky** → keep the behavioral suite
  minimal and guarded/skippable; the bulk of coverage is fast mocked unit specs.
- **Orphan target daemons if the broker dies abnormally** → broker disposes tracked
  targets on shutdown (spec requirement); add a best-effort reaper for stale sockets
  on startup.

## Migration Plan

Purely additive; nothing existing changes behavior. Steps: (1) add emcp to straight
(pinned); (2) add `--daemon[=NAME]` to `bin/emacs-isolated.sh`; (3) create the
`config/mcp-broker/` module; (4) add `bin/mcp-broker.sh`; (5) document client MCP
config (the loopback URL). Rollback = stop the broker daemon and remove the module +
straight entry; no persistent state to unwind.

## Open Questions

- **Frame requirement for key-driven tools.** Do `send-keys`/`run-command` need the
  target to have a frame (offscreen/tty frame created at spawn via `emacsclient -c`),
  or do we restrict v1 to eval-based interaction and treat key injection as
  best-effort? Resolve during implementation of `broker-target-support`.
- **Port selection & discovery.** Fixed default port vs dynamic; how the client learns
  the URL (printed by `bin/mcp-broker.sh`, written to a known file, etc.).
- **emcp tool registration without forking (verification #2).** Confirm
  `emcp--server-build` is callable and stable at the pinned commit; if not, carry a
  thin patch or register a profile upstream.
- **Screenshot tool** is deferred; revisit as a follow-up if a graphical target use
  case appears.
