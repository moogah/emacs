---
name: emacs-agent
description: Use when you need to drive or inspect a running Emacs - reading or writing the user's notes through org-graph / org-roam, preparing a layout in the user's workspaces, looking up what a function or variable does in the live session, or testing a config change (an unmerged worktree, exploratory Elisp) in an isolated Emacs. Covers bin/emacs-agent, choosing the live session or an isolated target, the call / call-write / eval tiers, and listing what is callable with `tools`.
---

# Driving Emacs through bin/emacs-agent

`bin/emacs-agent` (in this checkout) is the one entry point for driving Emacs. It
always names its target, picks a permission tier, and talks to Emacs through
`emacsclient -s <socket>`. Never run `emacsclient` yourself (bare, or with
`-s server`): that reaches the user's Emacs around the wrapper and its safeguards.

Run `bin/emacs-agent --help` for the full grammar; it is the authority if this page
and the wrapper ever disagree.

## Targets

- **`live`** - the user's running Emacs (the default `server` socket). Their real
  notes, their frames, their workspaces.
- **An isolated target** - a separate Emacs you start by name; nothing you do there
  reaches the user's session.

```
bin/emacs-agent live tools
bin/emacs-agent live call       TOOL 'JSON'
bin/emacs-agent live call-write TOOL 'JSON'
bin/emacs-agent live eval       'FORM'          only with the user's agreement

bin/emacs-agent target start NAME [--worktree PATH] [--data PROFILE]
bin/emacs-agent target start NAME --bare        emacs -Q, no configuration
bin/emacs-agent target list
bin/emacs-agent target stop NAME
bin/emacs-agent target NAME tools | call | call-write | eval | state
```

`target start` returns once the target's server is ready. `--worktree` defaults to
the checkout holding the script. `target NAME state` prints the target's frame state
as JSON. Stop every target you started when you are done with it.

`--data PROFILE` chooses which notes a worktree target sees:

| profile | notes |
|---|---|
| `none` (the default) | no notes |
| `scratch:<dir>` | a scratch vault at the absolute `<dir>`, with its own note databases |
| `live` | the user's real notes - see the rule below before using it |

## Choosing the target

Choose by what is at risk. Isolation protects against **bad code**, not against
**damaged data**.

| You need to | Use |
|---|---|
| Read the user's notes or other data through the API | `emacs-agent live call` |
| Write the user's data (create or change a note, store a layout) | `emacs-agent live call-write` |
| Prepare a demonstration in the user's workspaces | `emacs-agent live call-write` |
| Exercise or demonstrate unmerged config code (a worktree, a branch) | `emacs-agent target start NAME --worktree PATH`, then `target NAME ...` |
| Try exploratory or risky Elisp | an isolated target (`--bare` when no config is needed), then `target NAME eval` |
| Develop a data feature (org-graph itself, note indexing) | `emacs-agent target start NAME --worktree PATH --data scratch:<dir>` |

- **Data writes go through `live call-write`.** The live session coordinates writes
  to the vault and keeps its index current; an isolated target started with
  `--data live` bypasses both. Do not write the user's notes from an isolated target.
- **Unmerged code is exercised only in `target start --worktree`.** Never load
  uncommitted or unmerged config code into the live session.

## Tiers

| tier | runs | live session | isolated target |
|---|---|---|---|
| `call` | a tool from the read preset | yes | yes |
| `call-write` | a tool from the read or the write preset | yes | yes |
| `eval` | raw Elisp | **only with the user's explicit agreement** | yes |

- `live eval` needs the user's agreement every time: ask first, say what the form
  does, and prefer a tool through `call` / `call-write` when one does the job. The
  permission rules may also prompt for it; that prompt is not a substitute for asking.
- `call` refuses a tool of the write preset and names the tier it needs; a tool in
  neither preset is refused by both tiers.
- Every live call (tier, tool or form, arguments, outcome) is logged in the user's
  Emacs, in the `*emacs-agent-log*` buffer.
- Replies: `tools` and `state` print JSON. `call`, `call-write` and `live eval`
  print a JSON envelope, `{"ok":true,"result":...}` or `{"ok":false,"error":...}`.
  `target NAME eval` prints the value's printed form.
- Exit status: 0 success; 1 a failure envelope or an eval error; 2 a usage error or
  a target that is not running.

## Finding out what is callable

This page names no tool on purpose: the set changes. Ask the target:

```
bin/emacs-agent live tools
bin/emacs-agent target NAME tools
```

`tools` prints a JSON array, one object per tool: `name`, `category`, `tier`
(`read` - callable with `call`; `write` - needs `call-write`), `description`, and
`args` (each with `name`, `type`, `optional`, `description`). Pass the arguments as
one JSON object keyed by those names:

```
bin/emacs-agent live call TOOL '{"arg": "value"}'
```

The manifest includes introspection tools that describe a function or variable of
the target, which is how to look further into what a tool does.

## When the live session does not have it yet

The live session can answer only once the `config/emacs-agent` module is in the
checkout Emacs.app loads and Emacs has been restarted. Until then `live tools`
prints `[]`, and `live call` / `live call-write` fail with a void-function error
for `jf/agent-call`. That means the module is not deployed to the live session,
not that the call was wrong: use an isolated target started from a checkout that
has the module, and tell the user the live session needs the deploy and a restart.

If no Emacs is serving the live socket, the wrapper says no live session is
running and exits 2; it never falls back to an isolated target.
