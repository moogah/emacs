---
name: design-working-directory-scoping-for-sessions-and-agents
description: Define and implement how working directories (default-directory) are managed for both main gptel-chat-mode sessions and persistent agents, and how that interacts with path-scope validation. Surfaced during live PersistentAgent testing (2026-06-13): agents run with default-directory = their own sandbox dir, so relative paths resolve into the sandbox instead of the project, the scope boundary is never actually crossed, and an out-of-scope relative write silently lands in the agent dir. Needs a design pass (likely promote to an openspec change) before implementation.
status: ready
source: live PersistentAgent test session 2026-06-13 (commit 3f7feee added the test presets + agent preamble; this is the directory-scoping follow-up)
relations: discovered-from:persistent-agent-rebuild
---

> Surfaced 2026-06-13 during the first live end-to-end test of the
> reworked PersistentAgent tool (test presets + auto-injected agent
> preamble, commit `3f7feee`). The agent machinery itself behaved
> correctly; the test instead exposed that **working-directory
> management is undefined** for agents (and arguably for main
> sessions), which makes path-scope validation untestable and
> partially ineffective. Jeff has independently flagged session/agent
> directory scoping as known-pending work — this task records the
> concrete evidence so the design session can start from a worked
> example rather than from scratch.

## The finding in one sentence

A persistent agent's buffer has `default-directory` left at its own
sandbox dir (`~/.gptel/sessions/<parent>/branches/main/agents/<agent>/`),
so every relative path the model emits resolves *inside the sandbox* —
real project files are never found, the declared read/write scope is
never actually crossed, and a relative out-of-scope write silently
succeeds into the agent's own directory.

## Worked example (this test session)

Parent session: `~/.gptel/sessions/pa-test-2-20260613120058`
(preset `executor`, `:GPTEL_SCOPE_READ: /**`, write defaulted).

Agent: `branches/main/agents/test-agent-fs-scope-20260613120328-fs-scope/`
Drawer at creation: `:GPTEL_SCOPE_READ:
/Users/jefffarr/emacs-persistent-agent-review/config/gptel/presets/**`,
`:GPTEL_SCOPE_WRITE: /tmp/**`, standard deny set.

The task asked the agent (using **relative** paths — see "Test-harness
caveat" below) to:

| Step | Tool call (as the model emitted it) | Expected | Actual |
|------|-------------------------------------|----------|--------|
| 1 | `read_file_in_scope config/gptel/presets/minimal.md` | in-scope read OK | `file_not_found` — resolved to `…/<agent>/config/gptel/presets/minimal.md` (sandbox), absent |
| 2 | `read_file_in_scope init.el` | `scope_violation` | `file_not_found` — resolved to `…/<agent>/init.el` (sandbox), absent |
| 3 | `write_file_in_scope /tmp/pa-fs-test.txt` | OK | ✅ OK (absolute path, in `/tmp/**`) |
| 4 | `write_file_in_scope ./pa-fs-test.txt` | `scope_violation` | ✅ **written** to `…/<agent>/pa-fs-test.txt` — the sandbox dir, *not* in `:GPTEL_SCOPE_WRITE:` |

The bash agent (`test-agent-bash-scope-20260613120506-bash-scope`,
tool `run_bash_command`) showed the same pattern: `cat
config/gptel/presets/minimal.md`, `cat init.el`, `cat .env` all
returned shell "No such file or directory" because cwd was the sandbox;
only `echo hello` (no-op) and `touch /tmp/...` (absolute) behaved as
intended. `run_bash_command` emitted "Command contains absolute path
arguments. Directory scope may not protect these paths." for the `/tmp`
and absolute cases.

## What this proves vs. what it doesn't

**Validated and working (not part of this task):** preamble injection
and composition, interactive-session exclusion from the preamble, no
self-delegation (agents used their own tools, never re-called
`PersistentAgent`), single-final-message return, multi-tool FSM without
hang, persistence + drawer shape, genuine in-scope successes (`/tmp`
writes, `echo` no-op). See commit `3f7feee` and
`openspec/specs/gptel/persistent-agent.md`.

**Not proven by this test:** that scope denial and scope expansion
actually work at the boundary. Because relative paths never left the
sandbox, no `scope_violation` was ever produced; the
`request_scope_expansion` calls returned success and a y/n prompt did
appear, but the retries did not change the outcome (the files still
didn't exist in the sandbox), so expansion efficacy is **unconfirmed**.

## Distinct findings to resolve in design

1. **Agent working directory is undefined.**
   `jf/gptel-persistent-agent--task`
   (`config/gptel/tools/persistent-agent.org`) never sets
   `default-directory`; `find-file-noselect` leaves it at the agent's
   session dir. Decide what an agent's cwd *should* be — candidates:
   the parent session's project root; the common root of
   `allowed_paths`; an explicit new tool parameter; or the dir the
   parent buffer was operating in. Whatever is chosen must align with
   how scope patterns are written so relative paths and
   `run_bash_command` behave intuitively.

2. **Main-session working directory is also undefined.**
   `scope-validation.org` resolves relative command paths against
   `default-directory` ("the session's current working directory"),
   but nothing pins that for interactive sessions either. The design
   should cover both session kinds, not just agents.

3. **The session/agent dir appears implicitly read+write allowed.**
   The relative out-of-scope write (step 4) succeeded into the agent
   dir, and the out-of-scope relative reads passed the scope check
   (returning `file_not_found`, i.e. the body ran) rather than
   `scope_violation`. Determine whether the session dir is intentionally
   exempt from scope (it must be writable for `session.org`/autosave) and,
   if so, scope that exemption narrowly to session bookkeeping rather than
   arbitrary file writes.

4. **Relative-path policy for scope tools.** Decide whether
   `read_file_in_scope` / `write_file_in_scope` / `edit_file_in_scope`
   and `run_bash_command` should (a) resolve relative paths against a
   defined project root, (b) require absolute paths, or (c) reject
   relative paths with a clear instruction. Today they silently resolve
   against whatever `default-directory` happens to be.

5. **`request_scope_expansion` response contract is inconsistent.**
   The filesystem path returned `{success, patterns_added:[…],
   message:"…Added N pattern(s) to request_scope_expansion"}` while the
   bash path returned `{success, allowed_once:true, message:"Permission
   granted for this invocation only."}` — two different shapes and two
   different semantics (persistent pattern add vs allow-once) from the
   same tool in one session. Reconcile. (Area churned recently — see
   `.tasks/done/refactor-request-scope-expansion-to-take-operation.md`.)

## Likely shape of the fix

This is cross-cutting (persistent-agent + scope + sessions) and needs a
design discussion, so it should probably be **promoted to an openspec
change** when work starts rather than implemented ad hoc. The smallest
useful first step (if a quick win is wanted before the full design) is
finding 1: set the agent's `default-directory` in `--task` to a
deliberately chosen project root and confirm relative paths then hit the
real scope boundary.

## How to reproduce / verify (next session)

Re-run the scope scenarios with **absolute** paths to real files
*outside* the agent sandbox, so denial and expansion are genuinely
exercised. From a parent `jf/gptel-persistent-session` buffer:

```elisp
(jf/gptel-persistent-agent--task
 (lambda (txt) (message "AGENT FINAL >>> %s" txt))
 "test-agent-fs-scope" "fs scope abs"
 "Do each and report outcome:
  1. read /Users/jefffarr/emacs-persistent-agent-review/config/gptel/presets/minimal.md (expect allowed)
  2. read /Users/jefffarr/emacs-persistent-agent-review/init.el                         (expect scope_violation)
  3. write 'ok' to /tmp/pa-fs-test.txt                                                  (expect allowed)
  4. write 'ok' to /Users/jefffarr/emacs-persistent-agent-review/pa-fs-test.txt         (expect scope_violation)"
 '("/Users/jefffarr/emacs-persistent-agent-review/config/gptel/presets/**"))
```

A correct system would then: allow step 1, deny step 2 with
`scope_violation` (and expansion, if granted, would let the retry
succeed), allow step 3, deny step 4. Compare against the relative-path
run above to confirm the cwd fix.

## Context

- Test presets + agent preamble: commit `3f7feee` (branch
  `persistent-agent-review`).
- Agent creation / cwd gap: `config/gptel/tools/persistent-agent.org`
  (`jf/gptel-persistent-agent--task` — no `default-directory` set).
- Scope path resolution: `config/gptel/scope/scope-validation.org`
  (relative paths resolved against `default-directory`),
  `config/gptel/scope/scope-filesystem-tools.org`,
  `config/gptel/scope/scope-tool-wrapper.org`
  (`jf/gptel-scope-authorize-tool-call`),
  `config/gptel/scope/scope-shell-tools.org` (`run_bash_command`,
  `request_scope_expansion`).
- Related: `.tasks/done/refactor-request-scope-expansion-to-take-operation.md`,
  `.tasks/harden-persistent-agent-callback-on-error.md`.
- Example session left on disk for inspection:
  `~/.gptel/sessions/pa-test-2-20260613120058`.
</content>
</invoke>
