---
name: update-stale-workspace-assistant-tools-spec
description: workspace-assistant-spec.el still asserts the preset carries no :tools; org-graph's workspace integration now intentionally populates :tools with org-graph/agent-tools, so the spec fails on every full run with org-graph loaded (and its failure message crashes formatting). Update or delete the stale assertion.
status: ready
source: openspec/changes/vulpea-human-commands
relations:
  - discovered-from:runbook-and-verify
discovered_by: implementor
discovered_class: interface-drift
---

> Surfaced by cycle-1786636086's runbook-and-verify full-suite gate run
> (discovery disc-runbook-and-verify-1). Externalised: the failing spec
> belongs to the gptel presets subsystem, outside the vulpea-human-commands
> file set. The register entry `register/boundary/org-graph-agent-tools`
> (confirmed) is the authority the old spec now contradicts.

## Symptom

Full `./bin/run-tests.sh` fails
`config/gptel/presets/test/workspace-assistant-spec.el`, spec
"registers no palette/agent tools yet (out of scope for this change)".
It asserts the workspace-assistant preset carries no `:tools`, but
`config/org-graph/workspace-integration.el` now intentionally populates the
preset's `:tools` slot with `org-graph/agent-tools` (the confirmed
three-tool agent boundary). The spec encodes the pre-org-graph contract and
fails whenever org-graph is loaded in the full run. Secondary defect: its
failure message dies with "Not enough arguments for format string", masking
the actual diff.

## Fix sketch

Update (or delete) the stale spec so the full suite reflects the confirmed
boundary: workspace-assistant's preset `:tools` IS populated with the three
org-graph tools when org-graph is loaded. If the spec should remain
meaningful in org-graph-less runs, branch the expectation on whether
`org-graph/agent-tools` is bound rather than asserting emptiness. Fix the
format-string crash in the assertion message while there.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets` — green.
- Full `./bin/run-tests.sh` no longer counts this spec among failures.
