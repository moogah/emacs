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
- config/workspaces/docs/README.org (modify — :262-269 still says the gptel-session `:on-create` / `g` handler lives under `config/gptel/`; re-point at config/integrations/)
- config/gptel/presets/test/workspace-assistant-spec.el (re-check — see Cycle 1 updates)

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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/invariant/sessions-single-root`: speculated → reconciled. There is no per-workspace sessions dir any more: `:sessions-dir` is gone from the anchor payload, `jf/gptel--target-sessions-root` / force-global / `jf/gptel-persistent-session-global` are deleted, and creation resolves through the mkdir-ing `jf/gptel--ensure-sessions-root` (filesystem.el:16). **Implication for step 2**: compute `session-dir` as `(jf/gptel--create-session-directory id)` (filesystem.el:30) — never from the payload; the payload's `:home` is only the sixth positional `project-root`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-sessions-single-root.md`.
- `register/shape/workspace-integration-anchor-payload`: speculated → reconciled. Handlers receive `(:name :home :node-id :context)`, rebuilt PER HANDLER from the live record (`workspace--integration-live-record`, integrations.el:59), so this handler — registered AFTER `org-graph-project` — observes the `:node-id` that handler set. **Implication**: step 2's dynamic variable `jf-integrations--creating-for-node-id` binds `(plist-get payload :node-id)`, which is reliably populated by then. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-workspace-integration-anchor-payload.md`.
- `register/boundary/session-lifecycle-hooks` / `register/shape/session-created-hook-payload`: reconciled. The hook fires inside `jf/gptel--create-session-core` (commands.el:661) with `:session-file :session-id :branch :session-dir :branch-dir :project-root`, before any buffer or registry entry. **Implication**: your dynamic binding must wrap the `create-session-core` call itself (the hook runs synchronously inside it), exactly as step 2 says. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-session-lifecycle-hooks.md`.
- `register/invariant/packages-independent-no-cross-naming`: reconciled with an interim gap. `jf/gptel-workspace-initial-preset` is named by `config/gptel/presets/workspace-assistant/preset.org:10` and is UNDEFINED until step 1 defines it under config/integrations/ — the only place it may live. Also absorbed here (arch ot-single-root-1): `config/workspaces/docs/README.org:262-269` still documents the birth-session handler as living under `config/gptel/` — rewrite it to name config/integrations/workspaces-gptel.org. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-packages-independent-no-cross-naming.md`.
- `register/shape/session-file-head`: reconciled. PersistentAgent remains a second producer variant (drawer + body only; no keywords; no hook). See ask-12 below and `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-session-file-head.md`.

### User-resolved decisions
- `ask-cycle-1789334430-12` (open; default carried — `disposition-persistent-agent-session-head` option (a)): narrow the spec to user-facing sessions; agent transcripts get no keywords/hook. **Implication for this task**: the birth session and the `g` entry go through `jf/gptel--create-session-core` (so the hook fires and `int-gptel-org-graph` writes `part-of`); do NOT route persistent-agent creation through this layer unless the user picks (b), which would spawn a separate follow-up.

### Already-shipped inline fixes
- `36cfb2d1` (review ws-node-id F1-F4): dispatchers rebuild the payload per handler; docs/README.org's dangling gptel refs around :185 were fixed — the :262-269 block above is what remains. **Implication**: verify the fix held (no `jf/gptel-persistent-session-global` / `workspace-sessions-dir` in README.org) before rewriting :262-269.
- `.tasks/update-stale-workspace-assistant-tools-spec.md` was re-pointed at THIS task at integrate: `config/gptel/presets/test/workspace-assistant-spec.el` asserted the preset carries no `:tools`; the presets suite is green today only because tool injection was removed with the org-graph workspace module. **Implication**: step 3 re-introduces injection, so re-check that spec at review and branch its expectation on `(fboundp 'org-graph/agent-tools)` (and fix its format-string crash) if it goes red.

### Meta-discoveries
- `untouched-neighbour-consumer-drift`: the archived handler bodies you port (`git show ad69b5dc^:config/gptel/sessions/workspace-integration.org`, `git show b6678ebc^:config/org-graph/workspace-integration.org`) read `:sessions-dir` and `workspace-sessions-dir` — both gone; port the shape, not the lookups.
- `stub-bounded-regression`: step 4 spies on the session core; add ONE un-stubbed scenario (temp `jf/gptel-sessions-directory`, real `workspace--dispatch-create-integrations`, real core) asserting the session lands under the single root with `GPTEL_WORK_ROOT` = the workspace home.
