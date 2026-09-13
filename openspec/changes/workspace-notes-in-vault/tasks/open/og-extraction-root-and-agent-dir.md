---
name: og-extraction-root-and-agent-dir
description: Remove org-graph-roam-root; extract typed edges from the whole vault; add org-graph-agent-notes-directory for agent drafts and refuse writes outside the vault.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:int-workspaces-gptel
---

## Files to modify
- config/org-graph/org-graph.org (modify; tangle — defcustoms ~:59-84 and the vault-root-vs-roam-root prose)
- config/org-graph/extractor.org (modify; tangle — scope gate in `org-graph-extractor/extract` ~:386)
- config/org-graph/tools.org (modify; tangle — `org-graph-tools--roam-root` ~:77-83, `write-node` ~:217-243, description ~:269)
- config/org-graph/edge-type.org (modify; tangle — seed installer target directory)
- config/org-graph/test/extractor-spec.el, tools-spec.el, module-load-spec.el (the roam-root default assertion), edge-type-spec.el (modify)

## Implementation steps
1. `org-graph.org`: delete `org-graph-roam-root`; add
   `(defcustom org-graph-agent-notes-directory "~/org/roam/" ...)` documented
   as "where agent tools place new notes; must lie under
   `org-graph-vault-root`". Rewrite the boundary prose: one vault root for
   discovery and extraction; agent placement is a subdirectory choice only.
2. `extractor.org`: the scope gate becomes "path is under
   `org-graph-vault-root`" (`file-in-directory-p`). It is defensive — every
   indexed note is under the vault — keep it, drop the "excluded for
   workspace-local notes" prose, and update the extractor docstring.
3. `tools.org`: rename `org-graph-tools--roam-root` to
   `org-graph-tools--agent-notes-directory` returning the expanded new
   defcustom; `write-node` refuses (returns a tool error, writes nothing)
   when the resolved directory is not under the vault root. Regenerate the
   tool description text from the variable: "notes anywhere under the vault
   root participate in typed-edge extraction; writes outside the vault are
   refused".
4. `edge-type.org`: seed installer writes into the agent-notes directory
   (unchanged default path, new variable name).
5. Specs: extractor emits rows for a note at `~/org/sessions/x/session.org`
   (temp vault root) and for one at vault top level; `write-node` to
   `/tmp/elsewhere` returns an error and creates no file; default directory
   equals `~/org/roam/`; `module-load-spec.el` asserts the new defcustom
   default and the absence of `org-graph-roam-root`.

## Design rationale
The spike gated extraction to `~/org/roam/` only because its spec literally
excluded workspace-local notes and the human-commands change did not want
to touch that requirement. This change wants edges from chats under
`~/org/sessions/` and from top-level vault notes, so the gate widens to the
vault. The one thing the old variable also did — choose where agent
drafts land — is preserved by a dedicated variable with the same default,
so nothing moves on disk.

## Design pattern
Same `boundp`-guarded defcustom access as `org-graph-vault-root`
consumers; keep tool descriptions generated from variables (tools.org
already formats the directory into its description).

## Verification
- `./bin/tangle-org.sh` for all four `.org` files validates.
- `./bin/run-tests.sh -d config/org-graph` — green.
- `grep -rn 'org-graph-roam-root' config/org-graph/*.el` → no matches.
- `grep -n 'org-graph-agent-notes-directory' config/org-graph/org-graph.el config/org-graph/tools.el config/org-graph/edge-type.el` → defined and consumed.

## Context
design.md § D6 (Extraction root bullet); § Risks (roam-root removal)
specs/org-graph/spec.md § 'Typed Semantic Edges' (extraction root paragraph and 'Session heading contributes an edge'), 'Agent-Facing Graph Tools'
