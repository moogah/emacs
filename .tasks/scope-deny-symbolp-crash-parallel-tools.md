---
name: scope-deny-symbolp-crash-parallel-tools
description: "Live PersistentAgent test: denying a scope expansion throws `Wrong type argument: symbolp, \"<glob pattern>\"` and leaves the parent session stuck on the agent overlay. Root-caused to symbol-name on a non-symbol operation at scope-validation.org:246, reached via the parallel-tool / mixed expansion-queue path."
source: live work-root Phase-3 test session 2026-06-18 (session ~/.gptel/sessions/wr-live-20260618124704)
status: ready
relations:
  - discovered-from:gptel-work-root-default-directory-verify-change
  - related-to:gptel-preexisting-async-scope-test-failures
---

## Symptom (observed live, 2026-06-18)

During Phase-3 live verification of `gptel-work-root-default-directory`, an
fs-scope test agent (`test-agent-fs-scope`) was asked to run four file ops,
three of which it issued **in parallel**, with a prompt that also nudged it
toward `request_scope_expansion`. When the user **denied** the scope-expansion
prompt:

```
Error invoking callback: Wrong type argument: symbolp,
  "/Users/jefffarr/emacs-gptel-default-directory/config/gptel/presets/**"
```

The parent session buffer was then **stuck** displaying the agent overlay; the
only recovery was killing the buffer and deleting the agent directory.

The string in the error is the agent's **allowed read pattern** (an
expansion-offered pattern), not the denied resource.

## Root cause (high confidence)

`config/gptel/scope/scope-validation.org:246`
(`jf/gptel-scope--validate-filesystem-tool`):

```elisp
(op-keyword (intern (concat ":" (symbol-name operation))))
```

`operation` is contracted to be a **symbol** (`read`/`write`/`modify`/`execute`).
This is the ONLY `symbol-name`-on-`operation` site in the scope subsystem (the
other `intern` at :550 takes a string). When `operation` is bound to a glob
**string** like `/…/presets/**`, `symbol-name` raises exactly the observed
`Wrong type argument: symbolp, "<pattern>"`.

The macro call site quotes a symbol (`',operation`, scope-tool-wrapper.org:159)
and the add-to-scope re-entry reuses that same symbol, so the single-tool path
is sound. The corruption — a **pattern reaching the `operation` slot** — only
manifests on the **parallel-tool** path: the agent fired multiple scoped tool
calls concurrently and (per the prompt) likely also called
`request_scope_expansion`, whose handler builds its own violation-info with
`:patterns` (scope-shell-tools.org:279-324). The global expansion transient +
shared queue (`jf/gptel-scope--process-expansion-queue`) is the documented home
of the pre-existing **"Parallel tool callback: transient collision / session
stuck"** failures (`.tasks/gptel-preexisting-async-scope-test-failures.md`).
The live repro gives that cluster a concrete crash site and a concrete trigger.

The uncaught throw lands inside the deny callback chain
(`jf/gptel-scope--deny-expansion` → `:callback` funcall, scope-expansion.org:489),
which is why the FSM never advances and the session hangs.

## NOT implicated: the work-root change

The work-root outputs are all correct on disk for this very session — the agent
drawer carries `:GPTEL_WORK_ROOT: …/presets`, `:GPTEL_SCOPE_READ: …/presets/**`
(D6 auto-include fired even with `read_paths nil`), `:GPTEL_SCOPE_WRITE: /tmp/**`,
and a later run shows add-to-scope **persisting** `init.el` / `pa-fs-test.txt`
into the drawer. Denial *does* occur; the bug is purely in the parallel-deny
callback path.

## Two fix options

1. **Defensive guard at line 246 (cheap, stops the hang).** Normalize
   `operation`: accept a keyword or symbol, and on a non-symbol value return a
   clean denial plist (or `error` caught upstream) instead of throwing. Turns a
   stuck session into a graceful per-violation denial. Band-aid: it masks the
   arg-corruption rather than fixing it, but it removes the worst symptom.

2. **Real fix in the parallel/queue path (correct, larger).** Find where a
   pattern reaches the `operation` slot when multiple expansions (filesystem
   deny + `request_scope_expansion`) are queued concurrently, and stop the
   transient/queue from crossing one entry's `:patterns` into another's
   `operation`. This is the pre-existing transient-collision cluster; fixing it
   should also green several of the 21 quarantined async/callback specs.

Recommend doing (1) now as a safety net and scheduling (2) as the real fix
(likely promote to an openspec change, since it touches the async
expansion/queue design).

## Secondary observation (separate, lower priority)

In the expansion-efficacy retry run (step 8), the agent surfaced a **"create
directory" permission prompt twice** instead of a scope-expansion transient.
Likely the `write_file_in_scope` body's `(make-directory dir t)` or a gptel
`:confirm`, not a scope path. Investigate separately; capture which tool/agent
turn issued it from `~/.gptel/sessions/wr-live-20260618124704`.

## Repro

From a parent persistent session buffer, spawn `test-agent-fs-scope` with a
narrow `work_root` (so out-of-scope reads are genuinely denied) and a prompt
that asks for several file ops "in parallel" plus a `request_scope_expansion`
fallback, then **deny** the first expansion prompt. See the design doc
`.tasks/design-working-directory-scoping-for-sessions-and-agents.md` and the
captured session at `~/.gptel/sessions/wr-live-20260618124704`.
