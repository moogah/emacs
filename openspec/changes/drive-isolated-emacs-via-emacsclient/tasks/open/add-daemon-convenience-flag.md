---
name: add-daemon-convenience-flag
description: Optional — add an explicit --daemon convenience to bin/emacs-isolated.sh
change: drive-isolated-emacs-via-emacsclient
status: ready
relations: []
---

## Files to modify
- bin/emacs-isolated.sh (modify)

## Implementation steps
1. NOTE: this is optional. The script's existing `*)` arg pass-through already forwards
   `--daemon=<name>` to Emacs untouched (verified working with the full config). Only
   do this task if explicit handling adds real value (e.g. validating the name,
   printing the socket path, or a friendlier `--daemon NAME` spelling).
2. If proceeding, add a parsed `--daemon[=NAME]` option that:
   - Generates a name when omitted, and
   - Prints the resulting server-socket path (`$TMPDIR/emacs$(id -u)/<name>`) so the
     client learns the address without guessing.
3. Keep the GUI/batch behavior unchanged when `--daemon` is absent.
4. Add/extend a shell-level check or note; document the flag in the script header.

## Design rationale
design.md D2/D3: `--daemon=<name>` is the default launch path and pass-through already
works, so this is a convenience, not a requirement. Printing the socket path resolves
the prior change's "how does the client learn the URL" question at the launcher rather
than in a removed broker.

## Verification
- `bin/emacs-isolated.sh --daemon=test-x` starts a headless full-config daemon and the
  socket appears at the printed path; `emacsclient -s test-x --eval '(emacs-version)'`
  succeeds; `emacsclient -s test-x --eval '(kill-emacs)'` disposes it.

## Context
- design.md § Decisions 'D2' and 'D3'
- design.md § Open Questions 'Convenience wrapper scope'
