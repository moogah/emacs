---
name: rename-chat-mode-internals
description: Rename 5 chat-mode --prefixed internals to public symbols and update all in-tree callers
change: persistent-agent-rebuild
status: done
relations: []
---

## Files to modify

- `config/gptel/chat/parser.org` (modify) — rename `gptel-chat--parse-buffer`, `gptel-chat--turns-to-messages`
- `config/gptel/chat/parser.el` (tangled output)
- `config/gptel/chat/send.org` (modify) — rename `gptel-chat--open-assistant-block`, `gptel-chat--fsm-handlers`; update `gptel-chat-send` body to use new names
- `config/gptel/chat/send.el` (tangled output)
- `config/gptel/chat/stream.org` (modify) — rename `gptel-chat--stream-callback`
- `config/gptel/chat/stream.el` (tangled output)
- `config/gptel/chat/menu.org/el` (modify if greppable references found)
- `config/gptel/chat/test/parser/*-spec.el` (modify) — update test references
- `config/gptel/chat/test/send/*-spec.el` (modify) — update test references
- Any other in-tree callers (verify with grep below)

## Implementation steps

1. **Inventory every reference** to the five doomed symbols:
   ```
   grep -rn 'gptel-chat--\(parse-buffer\|turns-to-messages\|open-assistant-block\|stream-callback\|fsm-handlers\)\b' config/
   ```
   Save the list. Every occurrence gets renamed in this task.

2. **Rename in `parser.org`**:
   - `gptel-chat--parse-buffer` → `gptel-chat-parse-buffer`
   - `gptel-chat--turns-to-messages` → `gptel-chat-turns-to-messages`
   - Promote each docstring to public-API style: lead with the contract (signature, return shape, semantics) before any implementation note. Move implementation-only notes to `;;;` comments inside the function body.
   - Make sure both functions remain `defun` (not `cl-defun` / `defmacro` — the contract says "function").

3. **Rename in `send.org`**:
   - `gptel-chat--open-assistant-block` → `gptel-chat-open-assistant-block`
   - `gptel-chat--fsm-handlers` → `gptel-chat-fsm-handlers`
   - Update `gptel-chat-send` body to call the new names.
   - Promote docstrings and the `defvar` doc-string to public-API style.

4. **Rename in `stream.org`**:
   - `gptel-chat--stream-callback` → `gptel-chat-stream-callback`
   - Promote docstring to public-API style.

5. **Verify other in-tree callers** by re-running the grep from step 1. Update each occurrence. Common locations to check:
   - `config/gptel/chat/menu.org/el` (preset/lifecycle bookkeeping may reference)
   - `config/gptel/chat/test/parser/*-spec.el` and `send/*-spec.el` (test internals)
   - `config/gptel/chat/test/test-helpers.el` (shared fixtures may not — verify)

6. **No `defalias` shims**. Do not add backwards-compatibility aliases. The whole codebase updates in one pass.

7. **Tangle each modified `.org` file**:
   ```
   ./bin/tangle-org.sh config/gptel/chat/parser.org
   ./bin/tangle-org.sh config/gptel/chat/send.org
   ./bin/tangle-org.sh config/gptel/chat/stream.org
   ```
   Tangling auto-runs `check-parens` validation.

8. **Run the chat-mode regression suite**:
   ```
   ./bin/run-tests.sh -d config/gptel/chat
   ```
   All existing chat-mode tests must continue to pass with the renamed symbols.

## Design rationale

The persistent-agent rebuild needs to compose chat-mode's parse → messages → assistant-block → stream-callback → fsm-handlers pipeline programmatically. Today those primitives are `gptel-chat--`-prefixed (internal-by-naming). A non-chat-mode caller depending on `--`-prefixed names is an anti-pattern: it pretends the dependency is allowed when by convention it isn't.

The fix is to promote the five primitives to public names with documented contracts (one-time API formalization), then build the agent on the public API. No `defalias` shims because (a) all callers are in-tree and update in this single change, and (b) keeping shims hides whether the rename actually landed everywhere.

This is layer 1 of the change. Layer 2 (the agent rebuild) depends on the new symbol names existing.

## Verification

- `./bin/tangle-org.sh config/gptel/chat/parser.org` succeeds (no paren errors).
- `./bin/tangle-org.sh config/gptel/chat/send.org` succeeds.
- `./bin/tangle-org.sh config/gptel/chat/stream.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/chat` — all existing tests pass with renamed symbols.
- `grep -rn 'gptel-chat--\(parse-buffer\|turns-to-messages\|open-assistant-block\|stream-callback\|fsm-handlers\)\b' config/` returns empty.
- After loading a fresh Emacs, `(fboundp 'gptel-chat--parse-buffer)` returns `nil` (sanity check; covered by a test in a later task).

**Done means**: all five symbols renamed, all callers updated, no `defalias` aliases, full chat-mode test suite green, no `--`-prefixed references remain.

## Context

design.md § "Layer 1: Chat-mode public-API rename"
specs/chat-mode/spec.md (delta) § "Public programmatic-send API"

## Review

Reviewed by reviewer agent against `proposal.md`, `design.md § "Layer 1"`,
`architecture.md`, and `specs/chat-mode/spec.md (delta)`. Implementation
commit: `dad64de`. Merge commit: `be4fc91`.

### Verified clean

- All five renames landed across `config/`; verification grep returns
  zero matches.
- No `defalias` shims.
- Five renamed symbols remain `defun`/`defvar` (not `cl-defun`/`defmacro`)
  with original arities preserved.
- Docstrings promoted to public-API style (contract-first sentence;
  implementation notes moved to `;;` body comments). Spot-checked at
  `parser.el:335-354`, `parser.el:556-586`, `send.el:218-224`,
  `send.el:408-416`, `stream.el:556-583`.
- `gptel-chat-send` body (`send.el:523-531`) now invokes the renamed
  primitives.
- `.org`/`.el` pairs in sync.
- Tests exercise the real renamed functions (no spy-on / cl-letf
  stubs of the renamed surface).
- Five-symbol public surface matches what `architecture.md` and
  `design.md` promise downstream tasks; no missing surface.
- Other `--`-prefixed chat-mode internals (`--lifecycle-state`,
  `--blank-content-p`, `--declared-preset`, `--turn-to-messages`)
  are legitimate internals out of scope.

### Findings

- **Stale references in `openspec/specs/gptel/sessions-branching.md`**
  (lines 41, 52, 93). The task's verification grep was scoped to
  `config/` only and missed live behavioural specs. Three occurrences
  of `gptel-chat--parse-buffer` referencing the renamed symbol.
  - **Fix applied inline**: replaced all three with
    `gptel-chat-parse-buffer`. Verification grep across `openspec/specs/`
    and `config/` now returns empty. Doc-only change; no regression run
    needed.

### Verdict

Clean review with one inline fix. Flipping `status: needs-review` →
`status: done`. No follow-up tasks created; no dependents to repoint.
