---
name: stable-role-base
description: Track the session's role content (system-prompt body) in a dedicated buffer-local gptel-chat--system-prompt-base, set wherever role content is installed, so the pre-send composer can rebuild the system message from a stable source without reading back its own composed output. Prerequisite for env-block idempotency in the no-sibling case.
change: gptel-dynamic-environment-preamble
status: ready
relations: []
---

> Second of three tasks; independent of `env-block-builder`. This isolates the
> idempotency mechanism (design D2) from the composition wiring. Without it, the
> pre-send composer (next task) would read back its own `role + env` output in
> the no-sibling-file case and accumulate environment blocks across sends.

## Goal

A buffer-local `gptel-chat--system-prompt-base` that always holds the current
ROLE content (the system-prompt body, WITHOUT any environment block), set at
every point that installs role content today. The next task's composer reads
this base — never the composed `gptel--system-message` — for the no-sibling
case. (The sibling-file case already has a stable source: the file, re-read each
send.)

## Files to modify

- `config/gptel/chat/menu.org` — declare the buffer-local and set it alongside
  the existing `(set (make-local-variable 'gptel--system-message) ...)` sites.
  From the current grep these are:
  - preset application at activation (~`menu.org:268`)
  - `gptel-chat--apply-system-prompt-file` (~`menu.org:372`)
  - `gptel-chat--refresh-system-prompt-from-file` (~`menu.org:435`)
  Edit the `.org`; tangle regenerates `menu.el`.
- `config/gptel/chat/test/menu/environment-preamble-spec.el` — add a
  `describe "role base"` block (file created by `env-block-builder`; if running
  this task first, create the file).

## Implementation steps

1. `(defvar-local gptel-chat--system-prompt-base nil "Role content (system-prompt
   body) without the dynamic environment block; the stable source the pre-send
   composer rebuilds from.")`
2. At each role-install site above, set `gptel-chat--system-prompt-base` to the
   SAME body string being installed into `gptel--system-message` (the preset
   `:system`, or the sibling-file body). Set the base BEFORE/together with the
   existing assignment; do not change the existing assignment's value.
3. Do not introduce a second source of truth for the file case — the composer
   will still prefer the freshly re-read sibling body when a sibling exists; the
   base is the fallback for buffers with no sibling file. (Setting it at the
   sibling-refresh site too is harmless and keeps it consistent.)
4. `./bin/tangle-org.sh config/gptel/chat/menu.org`.

## Tests (Buttercup)

`describe "role base"`:
- after activation/preset application, `gptel-chat--system-prompt-base` equals
  the installed role body.
- the base contains NO environment block (it is role-only) — guards against a
  later regression where someone sets it from the composed value.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n "system-prompt-base" config/gptel/chat/menu.org
```

## Spec scenarios covered

Enables (does not itself assert) `specs/gptel/environment-preamble.md` →
Requirement "Idempotent wholesale rebuild" — the two-send no-duplication
scenario is asserted in `pre-send-compose-and-wire`, which depends on this base.
