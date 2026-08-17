---
name: vault-root-discovery
description: Introduce org-graph-vault-root; widen index roots to ~/org/ and pin note placement/filename template
change: vulpea-human-commands
status: done
merge_commit: 66b6593a
relations:
  - enables:authoring-module
---

## Files to modify
- config/org-graph/org-graph.org (modify — new defcustom; tangle to org-graph.el)
- config/org-graph/discovery.org (modify — index roots, placement section; tangle to discovery.el)
- config/org-graph/test/discovery-spec.el (modify)

## Implementation steps
1. In `org-graph.org` (the loader), next to the existing `org-graph-roam-root`
   defcustom, add:
   ```elisp
   (defcustom org-graph-vault-root "~/org/"
     "Root of the durable note vault indexed by org-graph.
   Covers the whole vault (including its roam/ subdirectory), and is
   where new notes are placed by default.  Distinct from
   `org-graph-roam-root', which continues to gate typed-edge
   extraction and the agent write target."
     :type 'directory :group 'org-graph)
   ```
   Do NOT change `org-graph-roam-root` — it keeps its `~/org/roam/`
   default and its consumers (extractor gating, edge-type scan, tools).
2. In `discovery.org` § Index roots, change `org-graph/index-roots` to seed
   from `org-graph-vault-root` instead of `org-graph-roam-root` (keep the
   `boundp` guard pattern with a `"~/org/"` fallback literal). Update the
   docstring: the bounded set is vault root + active workspace homes.
3. In `discovery.org` § Default notes directory (section exists; the
   `vulpea-default-notes-directory` setq already landed there):
   - Derive from the vault root instead of a literal:
     `(setq vulpea-default-notes-directory (file-name-as-directory (expand-file-name (if (boundp 'org-graph-vault-root) org-graph-vault-root "~/org"))))`
   - Add the filename template (dash, matching the existing corpus
     convention `20260813104944-some_note.org`; vulpea's default uses an
     underscore):
     `(setq vulpea-create-default-template '(:file-name "${timestamp}-${slug}.org"))`
   - Declare `(defvar vulpea-create-default-template)` in the
     soft-dependency block alongside the existing declarations.
4. Tangle: `./bin/tangle-org.sh config/org-graph/org-graph.org` and
   `./bin/tangle-org.sh config/org-graph/discovery.org`.
5. Update `test/discovery-spec.el`: index-roots assertions expect the vault
   root (`~/org/`), not `~/org/roam/`; add specs asserting
   `vulpea-default-notes-directory` equals the expanded vault root and
   `vulpea-create-default-template` carries the dash file-name template.

## Design rationale
The eval showed notes created at the vault top level are indexed at birth
(`vulpea-create` calls `vulpea-db-update-file` directly) but silently
dropped on any full re-scan or DB rebuild, because re-scans only walk
`vulpea-db-sync-directories`. Aligning the index root with the note
placement directory closes that trap. Two variables (vault root vs roam
root) because the spike spec's Typed Semantic Edges requirement literally
excludes notes "outside `~/org/roam/`" from the `typed_edges` index and
this change's spec delta does not modify that requirement — repointing
`org-graph-roam-root` wholesale would silently widen extraction and move
agent drafts. Survey: `~/org/` holds 1106 org files, 1099 already under
`roam/`; the widening adds a handful of files and vulpea skips those
without an `:ID:`. `setq` (not custom defcustoms of our own) is safe for
the vulpea variables because `defcustom` never clobbers an existing
binding, making the settings load-order-independent.

## Design pattern
Follow the existing `boundp`-guarded access pattern used in
`discovery.el` (`(if (boundp 'org-graph-roam-root) org-graph-roam-root "~/org/roam/")`)
and the soft-dependency `defvar`/`declare-function` block at the top of
`discovery.org`. The `Default notes directory` org section in
`discovery.org` shows the prose+block shape for placement settings.

## Verification
- `./bin/tangle-org.sh config/org-graph/org-graph.org && ./bin/tangle-org.sh config/org-graph/discovery.org` — both validate.
- `./bin/run-tests.sh -d config/org-graph` — all pass.
- `grep -n "org-graph-vault-root" config/org-graph/discovery.el config/org-graph/org-graph.el` — defcustom in loader, consumed in discovery.
- Eval check (spec scenario "Vault note outside roam/ is indexed"): with the
  module loaded, `(org-graph/index-roots)` returns `~/org/` (expanded) as its
  first element and does NOT contain `~/org/roam/` as a separate root.

## Context
design.md § Decisions 'D1 — Separate org-graph-vault-root' and 'D2 — Note placement'
specs/org-graph/spec.md § 'Workspace-Substrate Discovery' (MODIFIED), 'Boot-Order-Independent Default Note Placement'

## Observations

- Added a two-line default assertion for `org-graph-vault-root` to
  `config/org-graph/test/module-load-spec.el` (alongside the existing
  `org-graph-roam-root` one). This file is not in the task body's
  files-to-modify list, but the load-bearing register entry
  `register/boundary/vault-root-vs-roam-root` explicitly enumerates it
  in `consumers_exhaustive` ("loader-defcustom default assertion (new,
  alongside the org-graph-roam-root one at :99-101)"). Followed the
  register's consumer map; recorded here as the deviation from the task
  body's file list.
- Weak-assertion note on the new placement specs in
  `discovery-spec.el`: the placement `setq` runs at module load, which
  in the isolated spec process happens BEFORE the spec file's
  `(defvar org-graph-vault-root "~/org/")` shim — so what the
  assertions actually exercise is the `boundp` guard's `"~/org"`
  fallback, not the defcustom-derived branch. The fallback and the
  defcustom default expand to the identical directory, so the asserted
  value is correct either way, and the defcustom default itself is
  pinned by the new module-load-spec assertion (which loads the real
  loader). The defcustom-derived branch of the guard is therefore
  covered only by the combination, not by a single spec.
- Verified the agent-side half of the dash filename convention while
  implementing the human-side half:
  `grep -n '%s-%s.org' config/org-graph/tools.el` → line 122, as the
  `note-filename-template-dash` entry records. tools.el untouched.
- Boundary constraint audit: `grep -rn org-graph-vault-root config/`
  confirms the only non-test consumers are `discovery.el` and the
  defining `org-graph.el`; `extractor.el`, `edge-type.el`, and
  `tools.el` still read only `org-graph-roam-root`, per the
  `vault-root-vs-roam-root` boundary_constraints.

## Discoveries

none — the three cited speculated entries (`vault-root-vs-roam-root`,
`bounded-discovery-roots`, `note-filename-template-dash`) matched what
the code wanted exactly; no push-backs. Recommend the integrate phase
flip all three toward confirmed as implemented.
