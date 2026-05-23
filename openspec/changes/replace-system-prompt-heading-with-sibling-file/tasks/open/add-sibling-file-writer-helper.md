---
name: add-sibling-file-writer-helper
description: Add `jf/gptel--preset-source-file-extension` and `jf/gptel--write-system-prompt-sibling-file` in `config/gptel/sessions/commands.org` — the standalone helpers that derive a preset's source file extension and write `system-prompt.<ext>` with the preset's `:system` body verbatim. Pure helpers, unit-tested in isolation. Wiring into the session-creation path is the next task.
change: replace-system-prompt-heading-with-sibling-file
status: ready
relations: []
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — add the two helper functions
- `config/gptel/sessions/test/commands/sibling-system-prompt-file-spec.el` (add) — focused unit tests

## Why

design.md §Decision 1, Decision 2 — the sibling file lives next to `session.org` in the session's branch directory, with its extension mirroring the preset's source file. Today all presets are `.md`; the helper's extension-lookup keeps the design open to `.org` presets without further code change.

design.md §Decision 3 — the file is canonical; the writer is called once at session creation and never again from the save path. Isolating the writer in a single helper makes it easy to test independently and easy to call from any future "reset to preset" affordance.

architecture.md §Interfaces — defining the helper and its contract first (with tests) before wiring it into `jf/gptel--create-session-core` (next task `wire-sibling-file-emission-into-session-creation`) keeps the diff for the wiring task narrow and the helper's contract verified before integration.

## Implementation steps

1. Add `jf/gptel--preset-source-file-extension` to `config/gptel/sessions/commands.org`:
   - Signature: `(preset-name)` — accepts a symbol or string
   - Returns: the extension (`"md"` or `"org"`) of the file at `<jf/gptel-presets-directory>/<preset-name>.*`
   - Implementation: use `directory-files` against `jf/gptel-presets-directory` filtering for `^<preset-name>\\.\\(md\\|org\\)$`; take the first match's extension via `file-name-extension`
   - Defensive default: when the lookup fails (preset registered but file deleted; preset name unknown), return `"md"` and log a warning via `jf/gptel--log 'warn`
2. Add `jf/gptel--write-system-prompt-sibling-file` to the same file:
   - Signature: `(session-dir preset-spec)` — `session-dir` is the branch directory absolute path; `preset-spec` is the resolved preset plist (the same shape `gptel--apply-preset` consumes)
   - Behavior: when `(plist-get preset-spec :system)` is a non-empty, non-whitespace-only string, derive the extension via `jf/gptel--preset-source-file-extension` (using the preset name available on the spec — confirm during implementation; if the resolved spec doesn't carry the name, pass it in as a third argument), write `system-prompt.<ext>` into `session-dir` with the body verbatim, and return the basename `"system-prompt.<ext>"`. When `:system` is nil, empty, or whitespace-only, return `nil` and do not write.
   - Use `write-region` (not `with-temp-file` → `write-file`, which would mutate buffer-local state) for atomic write semantics.
3. **Verbatim** means no trimming, no transformation, no escaping. The on-disk file is byte-identical to the preset's `:system` body. (The preset registration step already trims via `string-trim` when extracting `:system`; this writer does not re-trim.)
4. New file `config/gptel/sessions/test/commands/sibling-system-prompt-file-spec.el`:
   - `describe "jf/gptel--preset-source-file-extension"`:
     - `it "returns 'md' for a registered .md preset"` (e.g., `'coding`)
     - `it "returns 'md' as the defensive default for an unknown preset name"` and asserts a warning is logged via spy
     - When `.org`-preset support is enabled (future), add a `'org'` case; today this is a parking-spot comment in the spec file rather than a live test
   - `describe "jf/gptel--write-system-prompt-sibling-file"`:
     - `it "writes system-prompt.md with the preset :system verbatim"`
     - `it "returns the basename on successful write"`
     - `it "returns nil and does not write when :system is nil"`
     - `it "returns nil and does not write when :system is whitespace-only"`
     - `it "preserves exact whitespace and special characters from :system"` (assert byte-for-byte equality including markdown fences, XML-like tags, leading/trailing newlines)
   - Setup/teardown: `before-each` creates a temp directory via `make-temp-file ... t`; `after-each` deletes it via `delete-directory ... t`.
5. Re-tangle `commands.org`. Run the new spec.

## Verification

```bash
./bin/tangle-org.sh config/gptel/sessions/commands.org
./bin/run-tests.sh -d config/gptel/sessions/test/commands
grep -n 'preset-source-file-extension\|write-system-prompt-sibling-file' config/gptel/sessions/commands.el
```

Expect: both helpers tangle and load cleanly. The new spec passes. No changes elsewhere in the sessions test suite (this task adds a standalone spec, doesn't touch existing ones).

## Context

architecture.md §Interfaces (new internal symbols, `sessions/commands.org`) — this task adds those symbols. Wiring them into `jf/gptel--create-session-core` is the consumer task `wire-sibling-file-emission-into-session-creation`.

design.md §Decision 2 — extension lookup defaults defensively to `"md"`; rationale recorded there.

The `preset-spec` plist's `:system` is the body string with frontmatter already stripped, per `jf/gptel-preset--parse-file` at `config/gptel/preset-registration.el:60-61`.
