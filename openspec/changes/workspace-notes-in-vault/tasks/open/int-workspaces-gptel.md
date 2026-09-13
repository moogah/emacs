---
name: int-workspaces-gptel
description: Relocate the birth session and the "g" menu entry into the integration layer, own the workspace-initial-preset defcustom, and inject org-graph tools into the workspace-assistant preset.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:int-scaffold-and-lint
  - blocked-by:gptel-single-root
  - blocked-by:og-extraction-root-and-agent-dir
  - enables:int-migration-command
---

## Files to modify
- config/integrations/workspaces-gptel.org (modify; tangle)
- config/integrations/test/workspaces-gptel-spec.el (new)
- config/gptel/presets/workspace-assistant/preset.org (read only — the `:tools` slot is populated at runtime)

## Implementation steps
1. `(defcustom jf/gptel-workspace-initial-preset 'workspace-assistant ...)`
   — same name as the deleted gptel module's knob so user customisations
   carry over.
2. Register integration `gptel-session` (after `org-graph-project`, i.e.
   this file loads after `workspaces-org-graph.org`):
   - `:on-create (payload)`: generate a session id from `:name`
     (`jf/gptel--generate-session-id`), then
     `jf/gptel--create-session-core id session-dir preset nil nil (plist-get payload :home) nil`
     — match the current signature exactly (read `commands.org`); the sixth
     positional is `project-root`. Return `ok`. Because creation goes through
     the public core, `jf/gptel-session-created-hook` fires and
     `int-gptel-org-graph` writes the `part-of` edge — but the hook needs the
     workspace: pass it explicitly. Add `:node-id` (from the payload) into
     the hook payload? gptel must not carry that key. Instead this handler
     binds a dynamic variable `jf-integrations--creating-for-node-id` around
     the call that the gptel→org-graph subscriber consults first, falling
     back to the current tab's workspace.
   - `:menu ("g" . add-session)`: same as on-create but interactive preset
     choice (port `jf/gptel--workspace-read-preset`).
3. Preset tools: under `(with-eval-after-load 'gptel-preset-workspace-assistant ...)`
   (the feature the old org-graph handler waited on — confirm the exact
   feature name in `config/gptel/presets/`), set the preset's `:tools` to
   `org-graph/agent-tools` when non-empty. Port
   `--populate-assistant-tools` from the deleted org-graph module.
4. Specs: on-create calls the session core with `project-root` = home and
   the initial preset (spy); the dynamic variable is bound during the call;
   the menu command prompts for a preset (stub `completing-read`); the
   preset's `:tools` slot is populated after the preset feature loads.

## Design rationale
The birth session existed inside gptel as a workspace integration, which
violated the independence rule. It is relocated unchanged in behaviour.
Its `project-root` argument is how the assistant's `${project_root}` write
scope and `GPTEL_WORK_ROOT` become the workspace home; nothing about
scope changes. Tool injection into the workspace-assistant preset moves
here from org-graph because it names both a gptel preset and org-graph
tools.

## Design pattern
Port the handler bodies from the archived
`config/gptel/sessions/workspace-integration.org` (birth session, `g`
entry, preset read) and `config/org-graph/workspace-integration.org`
(tool injection) — both are in git history at 310c8c93 and earlier.

## Verification
- `./bin/tangle-org.sh config/integrations/workspaces-gptel.org` validates.
- `./bin/run-tests.sh -d config/integrations` — green.
- `grep -n 'jf/gptel-workspace-initial-preset\|create-session-core\|agent-tools' config/integrations/workspaces-gptel.el` → present.

## Context
design.md § D8 (workspaces-gptel.org)
specs/workspace-graph-integration/spec.md § 'Birth-time session and workspace menu entries'
specs/sessions-persistence/spec.md § 'Session creation' (programmatic project-root scenario)
