---
name: remove-org-roam-patterns-dead-code
description: "Remove :org-roam-patterns from jf/gptel-preset--extract-scope's extraction list (with a legacy-key warning) and trim residual org-roam-patterns / org_roam_patterns references from scope-profiles.org. The empty-scope writer and scope-yaml.org are already dead post-cycle-3 — this task's surface is smaller than design D3 implies."
change: scope-rearch-followups
status: ready
relations: []
---

## Files to modify

- `config/gptel/preset-registration.org` (modify) — remove `:org-roam-patterns` from the `scope-keys` list in `jf/gptel-preset--extract-scope` (around `.el` line 129 / `.org` line 203); add a one-line `display-warning` for legacy keys; trim the docstrings on lines 18 / 124 / 198 (`.org` 42 / 50 / 198) that name `:org-roam-patterns` as a supported key.
- `config/gptel/scope-profiles.org` (modify) — trim `:org-roam-patterns` mentions in the introductory prose (line 8), the section list (line 38), the YAML example (line 109), and the `--load` docstring (line 245).
- `config/gptel/scope/test/integration/preset-scope-key-extraction-spec.el` (new) — Buttercup regression spec. Renamed from the prompt-suggested `test/yaml/empty-scope-fallback-spec.el` because `test/yaml/` was retired post-cycle-3 AND the empty-scope writer (`jf/gptel-scope-profile--write-scope-yml`) is itself dead now (`.el:582-593` returns an error stub). The meaningful regression today is on extraction, not on written-YAML contents.

## Why

Cycle-3 killed the `scope.yml` writer and `scope-yaml.org`, but `jf/gptel-preset--extract-scope` still lists `:org-roam-patterns` as a scope key it harvests from preset frontmatter, and the `scope-profiles.org` documentation still names it as a supported section. This is dead code that can surprise future readers; the documentation is how the drift survived the rearch.

## Scope reductions vs design D3

Design D3 (`design.md:78-91`) enumerates four touches. Cycle-3 invalidated two of them; the remaining two are the real work:

| D3 step | Status | Action |
|---|---|---|
| 1. `preset-registration.org:188` — remove `:org-roam-patterns` from `scope-keys` + warn | Live | DO |
| 2. `scope-profiles.org:400` — remove `:org-roam-patterns` from empty-scope fallback writer | **Obsolete** — writer is gone (`scope-profiles.el:582-593`) | SKIP |
| 3. `scope-profiles.org:19,205,487` — trim docstrings | Live (lines have shifted; see "Files to modify") | DO |
| 4. `scope-expansion.org:414,415,518,524` and `scope-yaml.org:116` — rewrite example | **Partially obsolete** — `scope-expansion.org` has no `org-roam-patterns` references (`grep` returned nothing); `scope-yaml.org` file is gone | SKIP (no live references to rewrite) |

Note this reduction in `## Observations` so it surfaces at integrate time. The proposal/design will need a delta note at archive.

## Implementation steps

1. **Write the failing regression spec FIRST** at `config/gptel/scope/test/integration/preset-scope-key-extraction-spec.el`. Two `it` blocks under `describe "jf/gptel-preset--extract-scope"`:
   - `it "does NOT extract :org-roam-patterns into scope-defaults"` — feed a synthetic preset plist with both `:paths` and `:org-roam-patterns`; assert the resulting scope-defaults entry contains `:paths` but NOT `:org-roam-patterns`.
   - `it "issues a warning when legacy scope keys are present in a preset"` — feed a preset plist with `:org-roam-patterns` (and optionally `:shell-commands` / `:bash-tools`); spy on `display-warning`; assert it was called with a message naming the preset and the ignored key(s).
   Borrow setup patterns from any existing preset-registration tests; if none exist, base on similar integration specs in `config/gptel/scope/test/integration/`.
2. **Run the new spec — confirm it fails** (the extraction today still includes `:org-roam-patterns`; no warning fires).
3. **Edit** `config/gptel/preset-registration.org`:
   - In the `scope-keys` list inside `jf/gptel-preset--extract-scope`, remove `:org-roam-patterns`. (Keep `:shell-commands` and `:bash-tools` per design — they're a separate cleanup.)
   - Add a scan-and-warn before the extraction loop. Pseudocode:
     ```elisp
     (let ((legacy-keys '(:org-roam-patterns))
           (preset-plist <the plist being extracted from>))
       (dolist (key legacy-keys)
         (when (plist-member preset-plist key)
           (display-warning
            'jf-gptel-preset
            (format "Preset %S contains legacy scope key %S; the key is ignored. Remove it from the preset definition."
                    <preset-name> key)
            :warning))))
     ```
   - Trim the function/file docstrings (`.org` lines 42, 50, 198 — `.el` lines 18, 124, 198) that name `:org-roam-patterns` as a supported key. Keep the docstring honest about what IS supported (`:paths`, `:shell-commands`, `:bash-tools`, `:scope-profile`).
4. **Edit** `config/gptel/scope-profiles.org`:
   - Line 8 (intro prose) — drop "org-roam patterns" from the parenthetical.
   - Line 38 (section list) — remove the `*org-roam-patterns*` bullet.
   - Line 109 (YAML example) — remove the `org_roam_patterns:` block from the example.
   - Line 245 (the `--load` docstring) — drop `:org-roam-patterns` from the "Returns a plist with..." line.
   - **Do NOT remove** the line ~207 reference if it's about the key normalizer (`org_roam_patterns` → `org-roam-patterns`). The normalizer can still handle the legacy key when reading existing user YAMLs; just nothing consumes the result. Verify before editing.
5. **Re-tangle both files** with `./bin/tangle-org.sh`.
6. **Run the new spec — confirm it passes**.
7. **Run the broader scope suite** with `./bin/run-tests.sh -d config/gptel/scope --report`.

## Verification

```bash
./bin/tangle-org.sh config/gptel/preset-registration.org config/gptel/scope-profiles.org
./bin/run-tests.sh -d config/gptel/scope/test/integration
./bin/run-tests.sh -d config/gptel/scope --report
grep -n 'org-roam-patterns\|org_roam_patterns' config/gptel/preset-registration.el config/gptel/scope-profiles.el config/gptel/preset-registration.org config/gptel/scope-profiles.org
```

Expected: the new spec passes; the grep returns only the normalizer's documentation (if any) and no live extraction or empty-scope writer references; scope suite counts match baseline or improve.

## Out-of-scope

- `:shell-commands` / `:bash-tools` legacy-key cleanup (separate work per design D3).
- Migration tooling for existing user `scope.yml` files containing `org_roam_patterns` blocks — out of scope per design's Non-Goals.
- Bug 1 (`request_scope_expansion` refactor) and Bug 2 (callback hang) — separate parallel tasks; no shared file conflicts.

## Context

- Design D3 in `openspec/changes/scope-rearch-followups/design.md:78-91`.
- Spec scenarios: `openspec/changes/scope-rearch-followups/specs/gptel/scope-profiles.md` — Requirement "Integration with preset registration", Scenarios "Scope defaults stored by preset name" / "Removed keys are not extracted".
- Cycle-3 archive context: `openspec/changes/archive/` (writer removal) and the `scope-profiles.el:582-593` error-stub.

## Observations

_(implementor fills during execution — must note the design-D3-vs-current-codebase reductions surfaced above)_

## Discoveries

_(implementor fills during execution)_
