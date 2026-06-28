## Why

External shell-capable LLM clients (Claude Code, via its Bash tool) have no
supported way to spin up an isolated Emacs instance that loads the **full
configuration under test**, drive it, and observe the result — without a human at
the GUI and without risking a running Emacs. This repo already owns the hard part
of isolation (worktree-isolated `runtime/` via `bin/emacs-isolated.sh`); what's
missing is a documented, scriptable way for an agent to *connect into* such an
instance and inspect it.

A prior exploration (the discarded `connect-external-llm-clients-mcp-broker`
change) proposed an in-Emacs MCP broker built on the `emcp` package to expose this
over MCP-over-HTTP. Hands-on verification showed that layer is unnecessary for a
shell-capable client: **`emacsclient` already provides a more direct and richer
connection to a running Emacs than an MCP tool surface would.** A client that can
run a shell needs no tool-server — `emacsclient --eval` *is* the tool surface, and
it can evaluate arbitrary Elisp in the real loaded config rather than a fixed,
pre-defined set of MCP tools. This change pursues that direct path and drops MCP,
the broker daemon, and the `emcp` dependency entirely.

## What Changes

- **New: isolated Emacs instances as `emacsclient`-controllable targets.** A target
  is a headless Emacs server started via `bin/emacs-isolated.sh --daemon=<name>`
  (full worktree config + isolated `runtime/`) or `emacs -Q --daemon=<name>` (bare).
  It is addressed by its server-socket name and driven with
  `emacsclient -s <name> --eval '(form)'`. Verified working today against the full
  config (26 modules, evil, gptel) on Emacs 30.1.
- **Clarified mechanism (corrects a misconception in the prior change):** the
  requirement for client control is `server-start` (a named server socket), **not**
  `--daemon`. `--daemon=<name>` is simply the convenient way to obtain a headless,
  backgrounded server with a unique name. Any long-lived Emacs that calls
  `(server-start)` is equally controllable.
- **New: display-free observation primitive.** A `jf/frame-state` support function
  serializes a target's observable state to JSON (current buffer name + bounded
  contents, window configuration, point, minibuffer state, recent echo-area
  messages), so a client with no display can parse and assert on Emacs state.
  Borrowed from `llm-test`, carried over from the discarded change.
- **New: small control helpers** (literal `type-text`, kbd-notation `send-keys`,
  `run-command`) loaded into the target as ordinary Elisp, invoked via
  `emacsclient --eval`. `eval-elisp` needs no helper — it is `emacsclient --eval`
  directly.
- **New: layered discoverability (replaces MCP's tool manifest).** A curated
  `jf/agent/*` operation set (baseline ported from emcp — apropos/describe/
  find-definition/get-variable/set-variable plus this change's eval/type-text/
  send-keys/run-command/frame-state) backed by a frontend-agnostic tool-spec registry
  with typed-arg metadata, a self-describing `(jf/agent-api)` manifest generated at call
  time from that registry, per-operation drill-down via standard introspection, and a
  minimal static seed (skill + `CLAUDE.md` pointer + RECIPES) naming only the entry
  point. Raw `--eval` stays as the escape hatch beneath. This is how a shell-capable
  client learns what it can call without a static, drift-prone tool list. emcp is used
  as **prior art only — reimplemented thin over built-in Emacs, with no `emcp`
  dependency.**
- **New: frames attached on demand.** A headless target carries only its internal
  terminal frame; tests needing real keymap/redisplay behavior attach a real frame
  with `emacsclient -s <name> -c` (GUI) or `-t` (TTY). This resolves the prior
  design's open question about key-driven tools needing a frame.
- **New: documented client recipes** — start / eval / observe / attach-frame /
  dispose as `emacsclient` one-liners an agent runs via Bash. The "registry" of live
  targets is just the socket files on disk (`ls` the server-socket dir); no in-Elisp
  registry, no broker process.
- **Removed (vs. the discarded change):** the MCP broker daemon, the `emcp`
  dependency (and its Emacs 30.1 floor + `http-server`/`elisp-refs`/`magit-section`
  transitive deps), the in-Elisp target registry, and the async-`make-process`
  dispatch discipline. The latter existed only because a single-threaded broker
  multiplexed all clients; with direct `emacsclient`, each call is its own OS process
  with its own timeout and the OS/client handles concurrency.

## Capabilities

### New Capabilities
- `isolated-emacs-targets`: Starting, addressing, and disposing isolated Emacs
  *server* targets — full-config (worktree `runtime/`) or bare (`-Q`) — via
  `bin/emacs-isolated.sh --daemon` / `emacs -Q --daemon`, addressed by server-socket
  name, controlled over the `emacsclient` channel; process/session/runtime isolation
  and crash containment (automatic, since targets are separate OS processes); the
  documented isolation boundary (shared filesystem, trusted-clients-only). Carried
  over and simplified from the discarded change (registry → socket files; no broker).
- `emacs-control-tools`: The drive surface exposed via `emacsclient --eval` —
  `eval-elisp` (direct), and target-side helpers `type-text`, `send-keys`,
  `run-command` — plus the documented Bash recipes and on-demand frame attachment.
- `frame-state-observation`: The display-free observation primitive — target state
  serialized to JSON (current buffer + bounded/truncated contents, windows, point,
  minibuffer, echo area), with the optional screenshot path available only when a
  target has a graphical frame. Carried over from the discarded change.
- `control-surface-discoverability`: How a shell-capable client *learns* what it can
  call — the layer that replaces MCP's tool manifest. A curated `jf/agent/*` set of
  intention-revealing operations (baseline ported from emcp as prior art), backed by a
  frontend-agnostic tool-spec registry carrying typed-arg metadata; a self-describing
  runtime manifest `(jf/agent-api)` generated from that registry (so it cannot drift);
  per-operation drill-down via standard introspection; and a minimal static bootstrap
  seed (skill + `CLAUDE.md` pointer + RECIPES) that names the entry point without
  enumerating operations. The registry is designed so a future gptel adapter can
  register the same operations as `gptel-make-tool` entries with no core changes. Raw
  `--eval` (in `emacs-control-tools`) remains the escape hatch beneath it.

### Modified Capabilities
<!-- None. This is purely additive new code in a new config/ module. gptel is out
     of scope (inverse direction). No existing spec's requirements change. -->

## Impact

- **New code area** (no existing module's behavior changed): a new literate
  `config/` module providing `jf/frame-state` and the control helpers, plus a client
  recipes doc and optionally a thin convenience wrapper script.
- **Reuses** `bin/emacs-isolated.sh`, the worktree/`runtime/` isolation model, and
  `emacsclient`/Emacs server mode as the control channel. The script's existing
  arg pass-through already forwards `--daemon=<name>` to Emacs; no script change is
  strictly required, though a small convenience flag may be added.
- **Dependencies removed vs. prior change:** `emcp` and everything it dragged in.
  Net new external dependency: none beyond `emacsclient` + Emacs server mode, both
  already present.
- **Security/trust posture:** targets are isolated from the user's live session and
  from each other by process + `runtime/` separation, but are **not** filesystem
  sandboxed (a target shares the host filesystem). Acceptable for a user-driven
  testing tool exercising the user's own code; control is local-only via the Unix
  socket and must not be exposed to untrusted clients.
- **Supersedes** `connect-external-llm-clients-mcp-broker` (deleted as part of this
  change; its MCP architecture is obsolete). The prior-art survey
  (emcp / emacs-jail-mcp / llm-test) and the rationale for rejecting MCP are carried
  into this change's `design.md`.
- **Planned sibling (not this change):** a gptel adapter that registers the same
  `jf/agent/*` operations as `gptel-make-tool` entries. This change builds the
  frontend-agnostic registry it will consume (see `design.md` D11); the adapter and its
  gptel-subsystem spec land separately so gptel's specs stay isolated.
- **Out of scope:** inspecting/driving the user's *live* Emacs session (that is
  `emcp`'s job), the gptel adapter code itself (planned sibling, above), filesystem
  sandboxing / containers, and MCP entirely.
