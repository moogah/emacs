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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/boundary/vault-root-vs-roam-root`: speculated (annotated). `og-vault-only-discovery` already scrubbed the workspace vocabulary from the `org-graph-roam-root` docstring (org-graph.el:19-27); the knob and its three readers are still live. Whole-tree grep confirmed the readers to remove are exactly: `org-graph-extractor--roam-note-p` extractor.el:166-181 (plus the `(defvar org-graph-roam-root)` at :166), `org-graph-tools--roam-root` tools.el:14-20, and the seed installer's `user-error` fallback edge-type.el:176-178. **Implication**: steps 1-4 stand; the org-line estimates in "Files to modify" (tools.org ~:77-83, extractor ~:386) are stale — locate by the tangled lines above. See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-vault-root-vs-roam-root.md`.
- `register/invariant/bounded-discovery-roots`: speculated → reconciled. `org-graph/index-roots` (discovery.el:24-36) returns exactly the expanded `org-graph-vault-root`; the `boundp` fallback to `~/org/` is pinned (discovery-spec.el:93). **Implication**: step 2's "every indexed note is under the vault" is now a tested fact, so the widened gate is purely defensive as the step says. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-bounded-discovery-roots.md`.
- `register/boundary/typed-edge-extraction-scope` (unchanged this cycle): this task is its cycle-2 owner — re-state it when the gate widens to the vault root.

### Meta-discoveries
- `untouched-neighbour-consumer-drift`: the Verification grep over `config/org-graph/*.el` misses prose and specs. Add `grep -rn 'org-graph-roam-root\|roam-root' config/org-graph/ openspec/specs/ config/org-graph/docs/` and clean every survivor (module-load-spec.el's roam-root default assertion, tools-spec / edge-type-spec fixtures, spike-eval.org prose).
- `vulpea-api-verify-against-source`: `write-node`'s refusal path (step 3) sits in front of `vulpea-create`; read `runtime/straight/repos/vulpea/vulpea.el` for how `:file-name` / directory are resolved before asserting "creates no file".
