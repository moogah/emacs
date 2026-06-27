## Context

This change lets an external shell-capable LLM client (Claude Code, via its Bash
tool) start an isolated Emacs instance loading the **full configuration under test**,
then inspect and drive it — without a human at the GUI and without risking a running
Emacs. See `proposal.md` for motivation and `specs/` for the behavioral contract.

It supersedes a discarded change, `connect-external-llm-clients-mcp-broker`, which
proposed an in-Emacs MCP broker built on the `emcp` package. Hands-on verification
(below) showed that layer is unnecessary for a shell-capable client and that
`emacsclient` is a more direct, richer channel. That change has been deleted; this
document carries forward its prior-art survey and the rationale for dropping MCP.

**Prior art surveyed (from the discarded exploration):**
- **emcp** (Elisp, in-process MCP-over-HTTP, current spec 2025-11-25) — the prior
  change planned to extend it. *Now rejected*: it exists to give MCP-only clients a
  tool surface over HTTP; a shell-capable client gets a strictly richer surface from
  `emacsclient --eval` (arbitrary Elisp vs. a fixed tool list) with zero dependency.
  emcp remains the right tool for driving the user's *live* session — explicitly out
  of scope here.
- **emacs-jail-mcp** (Go broker + Podman/Xvfb) — its container/Xvfb isolation was
  already rejected as macOS-hostile; only its lifecycle-as-operations *shape* informs
  this change.
- **llm-test** (Elisp) — source of the display-free *frame-state-as-JSON* observation
  idea, carried into `specs/frame-state-observation`. Its in-Emacs orchestrator is
  out of scope.

**Mechanism verified live on this machine (Emacs 30.1, macOS):**
- `emacs --daemon=<name>` (or any Emacs calling `(server-start)`) creates a named
  Unix-domain server socket at `$TMPDIR/emacs<uid>/<name>`. `emacsclient -s <name>
  --eval '(form)'` evaluates the form in that Emacs and prints the result.
- Running the exact `bin/emacs-isolated.sh` invocation with `--daemon=<name>`
  appended loaded the **full config** (26 modules, evil, gptel) headless; the
  script's existing arg pass-through forwarded `--daemon` untouched. Queried
  `jf/emacs-dir` / `jf/enabled-modules`, created buffers, and read state back.
- A frameless daemon reports `(length (frame-list))` = 1 (its internal terminal
  frame). `emacsclient -s <name> -c`/`-t` attaches a real GUI/TTY frame on demand.

## Goals / Non-Goals

**Goals:**
- Documented, scriptable recipes for an agent to start / eval / observe /
  attach-frame / dispose isolated Emacs targets using `emacsclient` via Bash.
- A small, literate `config/` module providing `jf/frame-state` (display-free JSON
  observation) and target-side control helpers (`type-text`, `send-keys`,
  `run-command`), loadable into a target.
- Full-config targets (worktree `runtime/`) and bare (`-Q`) targets, addressed by
  server-socket name.
- Buttercup-tested, with one guarded real-daemon behavioral smoke.

**Non-Goals:**
- Live-session inspection/control (that is emcp's job).
- Any MCP server, broker daemon, or `emcp` dependency.
- Filesystem sandboxing / containers.
- gptel integration / a shared tool interface (possible future synergy only).
- Screenshot observation in v1 (optional/secondary; `jf/frame-state` is the contract).

## Decisions

### D1. `emacsclient` is the transport; the shell is the client — no broker

The client drives targets directly with `emacsclient -s <name> --eval`. There is no
intermediary process, no MCP layer, no in-Elisp registry. *Alternative considered:*
an MCP broker (the discarded change) — rejected because it adds a dependency, a
single-threaded multiplexing process, and a *fixed* tool vocabulary, all to serve a
capability the shell already has more richly. MCP would only earn its place for a
*shell-less* client (e.g. an MCP-only IDE panel); none is in scope.

### D2. `server-start`, not `--daemon`, is the requirement; `--daemon` is the default launch

The control requirement is a named server socket (`server-start`). `--daemon=<name>`
is simply the convenient way to get a headless, backgrounded server with a unique
name, so it is the default launch path. This corrects the discarded design's
conflation of "controllable" with "daemon". *Alternative considered:* requiring an
interactive Emacs that calls `(server-start)` — viable and useful when a visible
window is wanted, but it pops a frame and backgrounds awkwardly; offered as a
documented variant, not the default.

### D3. Targets are isolated Emacs servers addressed by socket name; the "registry" is the socket directory

`start` = `bin/emacs-isolated.sh --daemon=<name>` (full config + isolated `runtime/`)
or `emacs -Q --daemon=<name>` (bare). The socket name IS the identifier. Enumerating
targets = listing `$TMPDIR/emacs<uid>/`; liveness = a control call against the
socket; dispose = `emacsclient -s <name> --eval '(kill-emacs)'`. No persistent
registry is built — the OS already tracks this. *Alternative considered:* an in-Elisp
registry struct (the discarded design) — rejected as redundant with the socket
directory.

### D4. Concurrency and crash-containment come free from the OS

Each `emacsclient` call is its own OS process with its own timeout (the client's Bash
tool already provides per-call timeouts). A hung or crashed target cannot block calls
to other targets or to the live session, because nothing multiplexes them. This
deletes the discarded design's load-bearing async-`make-process` dispatch discipline
(which existed only because one broker Emacs served all clients).

### D5. A small target-side support module provides observation + control helpers

`jf/frame-state` and the `type-text`/`send-keys`/`run-command` helpers must be
*present in the target*. They ship as an ordinary literate `config/` module. For
worktree-configured targets it is loaded as part of the config; for `-Q` targets it
is loaded on demand via `emacsclient -s <name> --eval '(load "…")'`. `eval-elisp`
needs no helper — it is `emacsclient --eval` itself. `jf/frame-state` walks the
current buffer (contents bounded/truncated with an indicator per spec), window
configuration, point, minibuffer state, and recent `*Messages*`/echo-area lines, and
returns JSON via `json-serialize`.

### D6. Frames are attached on demand, resolving the old open question

Headless targets serve `eval-elisp` + `jf/frame-state` with no frame. For
keymap/redisplay-dependent behavior (`send-keys`, some `run-command`), the client
attaches a real frame with `emacsclient -s <name> -c` (GUI) or `-t` (TTY). This
turns the discarded design's open risk ("do key-driven tools need a frame?") into a
documented, on-demand step.

### D7. Testing approach — hybrid (mock the boundary + one real-daemon smoke)

- **Framework**: Buttercup (`*-spec.el`), per repo preference for new suites.
- **Location**: `config/<module>/test/` with `unit/` and `behavioral/`, plus a
  `helpers-spec.el` for shared fixtures/matchers (mirrors the scope/test layout).
- **Unit specs**: exercise `jf/frame-state` serialization and truncation, and the
  control helpers, against synthetic buffers/windows in-process — fast, no daemons.
- **Behavioral smoke**: spawns ONE real isolated daemon (`emacs -Q --daemon`) and
  runs start → eval-elisp → frame-state → dispose end-to-end. Guarded to skip when no
  suitable Emacs/`emacsclient` is available so constrained CI still passes.
- **Run**: `./bin/run-tests.sh -d config/<module>` (or `-f buttercup`).
- **Scenario mapping**: each `#### Scenario:` in `specs/` maps to one `it` block,
  citing `specs/<capability>/spec.md § <Requirement>`.

### D8. Module layout (literate)

```
  config/<module>/                 (name finalized in tasks; e.g. emacs-test-targets/)
    <module>.org                   loader + autoload of the support module
    target-support.org             jf/frame-state + type-text/send-keys/run-command
    test/{unit,behavioral}/        Buttercup specs (D7)
    RECIPES.md (or .org)           emacsclient client recipes for agents
  bin/emacs-isolated.sh            optional small --daemon convenience (pass-through already works)
```

## Risks / Trade-offs

- **Targets are not filesystem-sandboxed** → control socket is local-only (Unix
  domain) and the module is documented as trusted-clients-only; consistent with the
  spec's isolation boundary. The container's fs containment is deliberately not
  carried over.
- **`-Q` targets lack the helpers until loaded** → load `target-support.el` on demand
  via `emacsclient --eval '(load …)'`; documented in RECIPES.
- **`frame-state` on huge buffers** → bounded/truncated output with an indicator (spec
  requirement) implemented in D5.
- **Real-daemon smoke is slower / could be flaky** → keep it minimal and
  guarded/skippable; bulk coverage is fast in-process unit specs.
- **Orphan target daemons if a client forgets to dispose** → targets are cheap and
  ephemeral; RECIPES documents enumerate-and-kill via the socket directory. No broker
  to own lifecycle, so disposal is the client's responsibility (acceptable for a
  user-driven tool).
- **Support module accidentally loaded into the user's live Emacs** → it defines only
  inert helper functions (no server start, no hooks); it is not added to
  `jf/enabled-modules` unless explicitly wanted.

## Migration Plan

Purely additive; nothing existing changes behavior. Steps: (1) create the
`config/<module>/` support module (`jf/frame-state` + control helpers); (2) write the
RECIPES doc of `emacsclient` one-liners; (3) optionally add a `--daemon` convenience
flag to `bin/emacs-isolated.sh` (pass-through already works); (4) add Buttercup unit
specs + one guarded real-daemon smoke. Rollback = remove the module and doc; no
persistent state, no dependency to unwind.

## Open Questions

- **Module name / location.** Finalize the `config/` module name during tasks
  (candidate: `emacs-test-targets`).
- **`-Q` helper loading ergonomics.** Single `(load …)` of a tangled `.el` vs. a
  combined eval payload — decide when implementing `target-support`.
- **Convenience wrapper scope.** Whether to add any thin `bin/` wrapper beyond the
  RECIPES doc, given the raw `emacsclient` one-liners already suffice.
- **Screenshot tool** deferred; revisit if a graphical-target use case appears.
</content>
