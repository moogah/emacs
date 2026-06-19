---
name: workspace-flip
description: Flip the workspace package's initial gptel preset from executor to workspace-assistant.
change: gptel-fragment-presets
status: blocked
relations:
  - "blocked-by:preset-workspace-assistant"
---

## Files to modify

- `config/gptel/sessions/workspace-integration.org/el` (modify) — change
  `jf/gptel-workspace-initial-preset` default `'executor` → `'workspace-assistant`.
- `config/gptel/sessions/test/workspace-integration-spec.el` (modify, if it
  asserts the default preset) — update expectation.

## Implementation steps

1. In `workspace-integration.org`, change the `defcustom`
   `jf/gptel-workspace-initial-preset` default value from `'executor` to
   `'workspace-assistant` (around line 53). Update the docstring if it names
   `executor`.
2. Grep the module for any other hard reference to `executor`; update to
   `workspace-assistant`.
3. Update any spec asserting the workspace initial preset.
4. Tangle: `./bin/tangle-org.sh config/gptel/sessions/workspace-integration.org`.
5. Run the workspace-integration suite.

## Design rationale

Switching the workspace package's auto-created session to the new
`workspace-assistant` preset is the user-visible point of the change (proposal.md;
design §Decision 9). The coupling is a single `defcustom`, so the flip is small
and isolated.

## Verification

- `grep -n "workspace-initial-preset\|executor\|workspace-assistant" config/gptel/sessions/workspace-integration.el`
- `./bin/run-tests.sh -d config/gptel/sessions/test`

## Context pointers

- Current code: `config/gptel/sessions/workspace-integration.org:53`
  (`jf/gptel-workspace-initial-preset` defcustom).
- Depends on `preset-workspace-assistant` (the target preset must exist/register).
