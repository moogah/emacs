---
name: add-emacs-isolated-daemon-flag
description: Add a --daemon[=NAME] pass-through to bin/emacs-isolated.sh so isolated targets can run as addressable daemons
change: connect-external-llm-clients-mcp-broker
status: ready
relations: []
---

## Files to modify
- bin/emacs-isolated.sh (modify)

## Implementation steps
1. In the option parser, add a `--daemon` / `--daemon=NAME` case. When present, set a
   flag and capture the optional server-socket NAME. Keep all existing options
   (`--init-runtime`, `--source`) working and continue passing unknown args to Emacs.
2. In the launch section, when the daemon flag is set, invoke Emacs in daemon mode
   instead of the interactive `exec`. Preserve the isolated configuration: still
   `-q --load early-init.el --load init.el` with `EMACS_USER_DIRECTORY=$RUNTIME_DIR`,
   but add `--daemon=NAME` (or `--daemon` when no name given). The socket NAME is what
   the broker will use as the target identifier.
3. Ensure the script still `exec`s (or backgrounds appropriately) so the daemon
   lifecycle is clean. Daemonized Emacs detaches on its own; verify the script returns
   promptly with the daemon running.
4. Update the script's usage/comment header to document `--daemon[=NAME]`.
5. Keep the macOS Emacs.app / PATH detection logic unchanged.

## Implementation notes
- `emacs --daemon=NAME` creates a server socket addressable via
  `emacsclient -s NAME`. The broker (other tasks) relies on this exact mechanism.
- Do not assume a display; the daemon is headless until a frame is requested.

## Design rationale
Targets reuse this repo's existing isolation (worktree config + isolated `runtime/`)
rather than a container. The only missing ingredient to make an isolated Emacs
externally drivable is daemon mode + a known socket name. Adding `--daemon[=NAME]`
turns `emacs-isolated.sh` into the spawn primitive for worktree-configured targets;
bare targets use `emacs -Q --daemon=NAME` directly.

## Verification
- `./bin/emacs-isolated.sh --daemon=mcp-test-x` starts a daemon; `emacsclient -s
  mcp-test-x --eval '(+ 1 2)'` returns `3`; then `emacsclient -s mcp-test-x --eval
  '(kill-emacs)'` tears it down.
- Existing non-daemon launches (`./bin/emacs-isolated.sh`, with a file arg, `--batch`)
  still behave as before.

## Context
design.md § Decisions 'D3. Targets are isolated Emacs daemons addressed by
server-socket name'.
