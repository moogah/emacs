---
name: fragment-core
description: Create the presets sub-module skeleton, the fragment/section parser, and the backend-parametrized renderer (Claude/XML), with a golden-snapshot test harness.
change: gptel-fragment-presets
status: ready
relations: []
---

## Files to modify

- `config/gptel/presets/fragments.org` (create) — fragment parser + `render`.
- `config/gptel/presets/fragments.el` (tangled).
- `config/gptel/presets/test/fragments-spec.el` (create) — Buttercup specs.
- `config/gptel/presets/test/golden/` (create) — golden snapshot `.txt` files.
- `config/gptel/presets/test/helpers-spec.el` (create) — `read-golden` helper.

## Implementation steps

1. Define the fragment data model in `fragments.org`:
   - A fragment is parsed from an `.org` source into: a kind (`static` |
     `dynamic`, default `static`) and an ordered list of sections.
   - A section = a top-level Org heading: `(name . body)` where `name` is the
     heading text and `body` is the content beneath it (trimmed).
   - Provide `jf/gptel-fragment--parse-source` (string/file → fragment struct or
     plist) using Org parsing primitives (`org-element` or a heading regexp;
     keep batch-loadable — no interactive deps).
2. Implement `jf/gptel-fragment-render (fragment backend)`:
   - Dispatch on `backend`. Implement `claude` only.
   - Claude rendering: each section `N`/`body` → `"<tag>\nbody\n</tag>"` where
     `tag = (replace-regexp-in-string " " "_" (downcase N))`. Join sections with
     a blank line.
   - For an unimplemented backend, signal a clear error (or `jf/gptel--log 'warn`
     + signal) — never emit Claude/Markdown as if correct.
   - Never emit Markdown by default.
3. Add a `read-golden` helper that reads
   `config/gptel/presets/test/golden/<name>.<backend>.txt`.
4. Write `fragments-spec.el`:
   - Section parsing: 3 headings → 3 sections; single-section fragment; kind
     defaults to static.
   - Claude render of `Role`/`Background`/`Constraints` → assert
     `:to-equal (read-golden "core-sample.claude.txt")`.
   - Multi-word heading `Output Format` → `<output_format>`.
   - Unimplemented backend → `:to-throw` (or logs + signals).
5. Create golden files for the above fixtures.
6. Tangle: `./bin/tangle-org.sh config/gptel/presets/fragments.org`.
7. Wire `fragments.el` into `gptel.org` load order **before** registration and
   before the chat/agent/env consumers (see composer/registration tasks).

## Design rationale

Realizes the `prompt-fragments` capability's fragment-source-format and
section-rendering requirements (design.md §Decision 1, §Decision 2). The renderer
takes `backend` as a parameter so additional backends are a pure extension; only
`claude` ships now. Golden snapshots (user choice) make full rendered output
eyeballable and regressions diff-visible.

## Verification

- `./bin/run-tests.sh -d config/gptel/presets/test`
- `grep -n "defun jf/gptel-fragment-render" config/gptel/presets/fragments.el`
- Confirm no YAML/markdown assumptions in the renderer:
  `grep -ni "markdown\|yaml" config/gptel/presets/fragments.el` (expect none).

## Context pointers

- Spec: `openspec/changes/gptel-fragment-presets/specs/prompt-fragments/spec.md`
  (Fragment source format; Section rendering is backend-parametrized).
- Best-practices tag names (`<output_format>`, `<constraints>`, …): proposal.md.
