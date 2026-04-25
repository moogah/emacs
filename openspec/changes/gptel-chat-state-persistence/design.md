## Context

The `gptel-chat-mode` change that landed as `2026-04-24-gptel-chat-mode` introduced two design decisions that, in combination, silently discard any configuration change made via `gptel-menu` in a chat-mode buffer:

- **Decision 16 point 1**: chat-mode must not enable `gptel-mode`. **Correct; preserved.**
- **Decision 16 point 2 / Decision 18 persistence clause**: chat-mode must not invoke `gptel--save-state` / `gptel--restore-state`; `metadata.yml` is authoritative. **Wrong.** It conflated `GPTEL_BOUNDS` (response-span tracking — genuinely incompatible with blocks) with `GPTEL_PRESET` + config deltas (the canonical mechanism that makes upstream gptel-mode's menu edits durable — orthogonal to response bounds).

Smoke-test symptom: freshly-created `session.org` has no `:PROPERTIES:` drawer; `gptel-menu`-driven tool addition disappears on `save-buffer`.

With the drawer restored, `metadata.yml` — originally carrying `session_id`, `created`, `updated`, `preset`, `type`, `parent_session_id` — loses its last remaining job (preset selection). `session_id` and `type` are derivable from the session path. `created` / `updated` are redundant with filesystem mtime. `preset` and `parent_session_id` move to the drawer. The user has confirmed there is no legacy constraint: `metadata.yml` can go away.

**Goal: stay aligned with upstream gptel behavior as closely as the block format allows.** Reuse upstream's `gptel-org-set-properties` and `gptel-org--entry-properties`, keep the drawer shape bit-for-bit identical to what a `gptel-mode`-enabled org buffer would produce (modulo `GPTEL_BOUNDS`), collapse our session sidecar files down to the irreducible set (`scope.yml` for permissions, `branch-metadata.yml` for branching only).

## Goals / Non-Goals

**Goals:**

- `save-buffer` on a chat-mode buffer writes a `:PROPERTIES:` drawer indistinguishable from what upstream `gptel-mode` would write, minus `GPTEL_BOUNDS`, plus `GPTEL_PARENT_SESSION_ID` for agents.
- `find-file` on such a buffer restores all persisted configuration (preset + deltas + parent-session-id) using upstream's own reader.
- User-initiated `gptel-menu` changes in a session buffer survive `save-buffer` + close + reopen.
- `metadata.yml` is removed from the codebase. No writer, no reader, no helper functions, no references.
- `session.org` is the single authoritative file for session-level state. `scope.yml` and `branch-metadata.yml` remain as they are.
- Zero changes to `gptel-mode` enablement, block format, registry, activities worktree storage, scope profiles.

**Non-Goals:**

- Enabling `gptel-mode`. Decision 16 point 1 stands.
- Persisting `GPTEL_BOUNDS`.
- Legacy support for old session files (user confirmed not needed).
- Writing drawer properties into any sidecar file.
- `branch-metadata.yml` changes (parent_branch, branch_point_position stay put).

## Decisions

### Decision 1 — Reuse upstream `gptel-org-set-properties`, not fork or wrap

**Choice**: call `gptel-org-set-properties (point-min)` directly from the chat-mode before-save-hook.

**Alternatives considered:**
- *Call `gptel-org--save-state`*: also writes `GPTEL_BOUNDS` — disallowed.
- *Reimplement a chat-mode-specific property writer*: duplicates delta-from-preset logic, rots as upstream evolves, adds test surface for no behavioral benefit.
- *Upstream the "opt-out of GPTEL_BOUNDS" flag*: clean, but requires upstream coordination.

**Rationale**: `gptel-org-set-properties` is already the narrower helper — `gptel-org--save-state` is a two-line wrapper that calls it and then `org-entry-put "GPTEL_BOUNDS" ...`. Calling the narrower helper gives upstream semantics for free without the one line we don't want.

### Decision 2 — Reuse upstream `gptel-org--entry-properties` for restore

**Choice**: call `(gptel-org--entry-properties (point-min))` to destructure the drawer, then overlay non-preset fields buffer-locally.

**Rationale**: same as Decision 1 in reverse — upstream already factored out the tuple-returning reader. We avoid `gptel-org--restore-state` because it also reads `GPTEL_BOUNDS` and restores text-property response bounds we don't use.

### Decision 3 — `GPTEL_PARENT_SESSION_ID` as a chat-mode extension to the drawer

**Choice**: persist agent parent-session-id as a new drawer key `GPTEL_PARENT_SESSION_ID`. Write via `org-entry-put` after `gptel-org-set-properties` returns; read via `org-entry-get` after `gptel-org--entry-properties` returns.

**Alternatives considered:**
- *File-local variable `-*- jf/gptel--parent-session-id: ... -*-`*: would be honored by `hack-local-variables`, but file-local variables on the first line are visible in the buffer text and uglier for a human reader.
- *Separate sidecar file `parent.txt`*: minimal, but re-introduces the "multiple sources of truth" problem `metadata.yml` removal is intended to end.
- *Encode in `session_id`*: loses the parent/child relationship when session-ids are renamed.

**Rationale**: the drawer is already the authoritative place for session-level configuration; parent-session-id is session-level configuration. Upstream gptel-mode tolerates unknown drawer keys (`gptel-org--entry-properties` reads a fixed set), so adding `GPTEL_PARENT_SESSION_ID` does not break cross-tool compat.

### Decision 4 — Pre-populate the drawer at session creation

**Choice**: `jf/gptel--create-session-core` writes `session.org` whose initial content begins with a `:PROPERTIES:` drawer containing `GPTEL_PRESET` (and `GPTEL_PARENT_SESSION_ID` for agents), then the empty user block. A helper `jf/gptel--initial-session-content (preset &optional parent-id)` generates this string.

**Alternatives considered:**
- *Activate `gptel-chat-mode` in a temp buffer, set `gptel--preset`, save to disk*: heavy — spins up a buffer just to write a file. Correct for exercising the full save path but overkill for creation.
- *Write empty `session.org`, set preset via `gptel--apply-preset` in the opened buffer, save*: order-sensitive, and "open" happens later in the user's flow (after `jf/gptel-persistent-session` returns).

**Rationale**: constructing the drawer as a string at creation time is the simplest single-pass write. The shape is identical to what the save hook would produce on first save with no overrides, so behavior converges deterministically.

### Decision 5 — Restore-order precedence: preset first, drawer overlay second

**Choice**: on mode activation, apply the declared preset (if any), then overlay the drawer's non-preset properties. In the session-file path, mode activation does both jobs; auto-init no longer does a second preset apply.

**Rationale**: preset-then-overlay is exactly how upstream `gptel-mode` already operates in `gptel-org--restore-state`. We stay on that rail.

### Decision 6 — Auto-init reads nothing from disk beyond path extraction

**Choice**: `jf/gptel--auto-init-session-buffer` no longer reads `metadata.yml`, `scope.yml`, or any sidecar. All session-level state is either path-derivable (session-id, branch-name) or drawer-derived (preset, parent-session-id) via the chat-mode hook.

**Alternatives considered:**
- *Keep a minimal `session.meta` or similar*: same "multiple sources of truth" anti-pattern. Rejected.
- *Auto-init reads drawer directly*: unnecessary — mode activation fires first (ordering is already correct), and mode activation already reads the drawer.

**Rationale**: single source of truth (`session.org`) is the whole point. Auto-init becomes substantially smaller (the metadata-read block is a large chunk of the current function).

### Decision 7 — Branching does not copy `metadata.yml` (because there isn't one)

**Choice**: `jf/gptel--branch-session-core` stops copying `metadata.yml` from parent to child branch. The preset is inherited via the drawer already embedded in the truncated `session.org`.

**Rationale**: trivial consequence of Decision 6. Branch-metadata.yml is unchanged.

### Decision 8 — Activities-integration guard replaces metadata existence check

**Choice**: replace `(unless (and (fboundp 'jf/gptel--read-session-metadata) (fboundp 'jf/gptel--register-session)) (error ...))` with a guard on a still-present function — `(unless (fboundp 'jf/gptel--register-session) (error ...))`.

**Rationale**: the guard's intent is "sessions module is loaded." `jf/gptel--register-session` is a stable symbol representing that contract.

### Decision 9 — Every `metadata.yml`-referencing test is rewritten, not deleted

**Choice**: scan and rewrite all test files that wrote or asserted on `metadata.yml`. Replace metadata writes with drawer pre-population; replace metadata asserts with drawer asserts. Delete tests only when they encode a contract that no longer exists (e.g., "metadata.yml wins over drawer when both present" — that scenario vanishes).

**Rationale**: preserves coverage while updating the authoritative source.

### Decision 10 — Save hook is buffer-local, added from `gptel-chat--install-preset-hooks`

**Choice**: `before-save-hook` entry added buffer-locally from the `gptel-chat-mode-hook` handler that already installs the `hack-local-variables-hook` entry.

**Rationale**: symmetry with the existing pattern; no global hook registration; avoids `derived-mode-p` checks in unrelated buffers.

## Risks / Trade-offs

**[Risk] Upstream `gptel-org-set-properties` signature change.**
→ **Mitigation**: one integration test exercises the real helper (not a spy). A breaking signature change fails that test immediately.

**[Risk] User opens a pre-existing session file that was saved before this change (no drawer).**
→ **Mitigation**: file loads as a bare chat-mode buffer. No preset applied; user re-selects via `gptel-menu`; next save writes the drawer. User has confirmed no legacy support is required.

**[Risk] `gptel-org-set-properties` behavior when `gptel--preset` is nil: it writes every non-default field as a delta (no baseline to diff against).**
→ **Mitigation**: spec scenario "Drawer omitted when no preset is applied and no config changed" verifies the common case. If this proves noisy in practice, guard the save with `(when (or gptel--preset gptel-tools gptel-model ...) ...)` in a follow-up.

**[Risk] Activities-integration guard false-positive: after deleting `jf/gptel--read-session-metadata`, third-party code that relied on its presence as a feature-detection probe will silently misbehave.**
→ **Mitigation**: grep for the symbol repo-wide during the task list. `jf/gptel--register-session` is the new probe and is mentioned in the activities-integration docstring.

**[Risk] `branch-metadata.yml` could be mistaken for `metadata.yml` and accidentally deleted during the purge.**
→ **Mitigation**: task list names each file explicitly. `branch-metadata.yml` is NOT removed. Tests for branching remain.

**[Risk] `GPTEL_PARENT_SESSION_ID` clashes with a future upstream drawer key of the same name.**
→ **Mitigation**: unlikely (upstream's drawer set is narrow and stable across years). If it happens, rename to `GPTEL_CHAT_PARENT_SESSION_ID`.

**[Risk] Initial content with a drawer breaks the chat-mode parser if the parser assumes `#+begin_user` is at point-min.**
→ **Mitigation**: the existing parser already handles `:PROPERTIES:` drawers and org headings at the top of the buffer (see `chat-mode.md` Key Concepts §Metadata and Commentary — "drawers that appear outside turn blocks are permitted"). Spec scenario for pre-populated drawer verifies.

## Migration Plan

No data migration is required (user confirmed no legacy constraint).

- **Pre-existing session files without a drawer**: continue to load. No preset applied. User re-selects via `gptel-menu`, saves, drawer appears.
- **Pre-existing session directories with `metadata.yml`**: the file is simply no longer read. If a user is curious and deletes it, nothing breaks. If they leave it, it becomes orphaned — a future cleanup PR may `git rm` stale `metadata.yml` files from the user's sessions directory, but that is out of scope.
- **Rollback**: revert the touched commits. Subsequent saves stop writing the drawer; existing drawers are harmlessly ignored by the old restore path (which reads only `GPTEL_PRESET`). Metadata reads would fail because the module is gone — rollback must be clean, not partial.

## Open Questions

None that require a decision before implementation. Follow-ups to consider after the change lands:

1. Should `gptel-menu` gain a "reset this buffer to preset defaults (delete drawer deltas)" affordance?
2. Should we emit a one-time migration notice when opening a session directory that still contains an orphan `metadata.yml`? Low priority — the file is inert.
