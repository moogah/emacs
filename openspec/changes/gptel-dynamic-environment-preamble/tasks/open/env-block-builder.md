---
name: env-block-builder
description: Add a workspace-neutral builder that produces the environment block (work root + verbatim scope globs + prose framing + live-note) from the buffer's drawer and default-directory, degrading gracefully when no scope drawer is present. Unit-tested in isolation.
change: gptel-dynamic-environment-preamble
status: done
relations: []
---

> First of three tasks. Produces the pure builder; no pre-send wiring here
> (that is `pre-send-compose-and-wire`). Builder reads only intrinsic inputs —
> the buffer's drawer and `default-directory` — so it works with the workspaces
> package absent.

## Goal

A function that returns the environment-block string for the current buffer,
built from the work root (`default-directory`) and the scope drawer keys
(`GPTEL_SCOPE_READ` / `GPTEL_SCOPE_WRITE` / `GPTEL_SCOPE_DENY`), rendering scope
as verbatim glob patterns. Implements design D3 and D5 (content + degradation).

## Files to modify

- `config/gptel/chat/menu.org` — add the builder function (and a small render
  helper if useful) in a new subtree near the pre-send refresh section. Edit the
  `.org`; `./bin/tangle-org.sh` regenerates `menu.el`.
- `config/gptel/chat/test/menu/environment-preamble-spec.el` — NEW Buttercup
  spec file (builder describe-block only in this task).

## Implementation steps

1. Add `gptel-chat--build-environment-block` (no args; operates on the current
   buffer):
   - Read the drawer alist via `jf/gptel--scan-session-drawer-keys` (the same
     scanner the binder uses). Multi-value scope keys serialize through the
     `+`-convention; reuse the existing accessor that the scope/drawer code uses
     to collect each `GPTEL_SCOPE_*` key's full pattern list (grep
     `scope-expansion.org` / `commands.org` for how `GPTEL_SCOPE_READ` etc. are
     read back — do NOT re-implement the `+` collapse).
   - Work root = `default-directory` (already a directory path).
   - Render the markdown block exactly per design D3:
     ```
     # Environment
     You are working in the directory below. The file-access scope lists what you may
     read and write; this information is current as of this message.

     - Working directory: <default-directory>
     - Readable: <read globs, comma-joined>
     - Writable: <write globs, comma-joined>
     - Denied:   <deny globs, comma-joined>
     ```
   - Degradation (D5): when NO `GPTEL_SCOPE_*` keys are present, replace the three
     scope lines with a single `- File access: no scope restrictions (this buffer
     is not a scoped session)` line. Never error; never return an empty string.
2. Keep the builder free of any `workspaces` symbol (`featurep`/`fboundp`/
   `workspace-*`); it must build from drawer + `default-directory` only.
3. `./bin/tangle-org.sh config/gptel/chat/menu.org` (tangles + validates parens).

## Tests (Buttercup, unit)

In `environment-preamble-spec.el`, `describe "gptel-chat--build-environment-block"`:
- reports the work root from `default-directory`.
- renders read/write/deny as verbatim globs from the drawer keys.
- includes the live-note sentence ("current as of this message").
- with no `GPTEL_SCOPE_*` keys: reports `default-directory` + "no scope
  restrictions", and does not error / is non-empty.
- builds with the workspaces package absent (no workspaces symbol referenced).

Set up buffer state by inserting a drawer and/or `setq-local default-directory`;
follow patterns in `config/gptel/chat/test/test-helpers.el` and
`config/gptel/chat/test/menu/pre-send-refresh-spec.el`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/chat/menu.org
./bin/run-tests.sh -d config/gptel/chat/test/menu
grep -n "build-environment-block\|no scope restrictions" config/gptel/chat/menu.org
# must NOT match a workspaces symbol inside the builder:
grep -n "workspace" config/gptel/chat/menu.org
```

## Spec scenarios covered

`specs/gptel/environment-preamble.md` → Requirement "Environment block content
is drawer-sourced and workspace-neutral" (all scenarios) and Requirement
"Graceful degradation without a scope drawer".
