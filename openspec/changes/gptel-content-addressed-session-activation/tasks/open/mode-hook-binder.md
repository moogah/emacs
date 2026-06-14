---
name: mode-hook-binder
description: Add a guarded gptel-chat-mode-hook binder that sets the four buffer-local session vars, registers the buffer, and enables autosave using drawer identity and the ancestor-walk session-dir.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:drawer-identity-resolver"
  - "blocked-by:session-dir-ancestor-walk"
  - "blocked-by:magic-mode-alist-activation"
---

## Files to modify

- `config/gptel/sessions/commands.org` (modify) — add `jf/gptel--bind-session-buffer`; register it on `gptel-chat-mode-hook`.
- `config/gptel/sessions/test/commands/mode-hook-binder-spec.el` (new) — Buttercup specs for binding, the non-session no-op, and branch-dir derivation.

## Implementation steps

1. Write the spec first. Cover:
   - activating `gptel-chat-mode` in a signature-bearing buffer → the four buffer-locals (`jf/gptel--session-id`, `jf/gptel--session-dir`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`) are set from drawer-resolved identity, the buffer is registered in `jf/gptel--session-registry`, and `jf/gptel-autosave-enabled` is t;
   - activating chat-mode in a buffer with no `:GPTEL_` drawer (scratch chat) → binder is a no-op (no registry entry, no session vars);
   - `jf/gptel--branch-dir` equals `(file-name-directory (buffer-file-name))` — no `../..` walk.
2. Implement `jf/gptel--bind-session-buffer`:
   - Guard: return early unless `(buffer-file-name)` and `(jf/gptel--session-signature-p)` (signature in the buffer) — NOT a path-layout test.
   - `branch-dir` = `(file-name-directory (buffer-file-name))`.
   - `session-id` / `branch-name` via the resolvers (drawer-identity-resolver); `session-type` via `jf/gptel--session-type`.
   - `session-dir` via `jf/gptel--session-dir-from-branch-dir` (session-dir-ancestor-walk).
   - `setq-local` the four vars (after mode activation; the hook fires after `kill-all-local-variables`).
   - Register via `jf/gptel--register-session`; set `jf/gptel-autosave-enabled` t.
   - Wrap registry work in `condition-case` so a registry failure does not abort var-setting (mirror the old auto-init isolation). Do NOT enable `gptel-mode`; do NOT call `gptel--save-state` / `gptel--restore-state`.
3. Register `(add-hook 'gptel-chat-mode-hook #'jf/gptel--bind-session-buffer)`. Ordering: it is an independent hook entry alongside `gptel-chat--apply-declared-preset`; neither depends on the other's success.
4. Tangle `commands.org`; run the new spec.

## Design rationale

Binding belongs to the mode, not to a global file-open hook. The signature guard (drawer carries a `:GPTEL_` key) lets the same hook serve real sessions and scratch chat buffers without a path test. `branch-dir` is the file's own directory — derived, never reverse-engineered. The hook-entry pattern matches the codebase's existing `apply-startup-visibility` / `install-preset-hooks` conventions and the documented reason buffer-local setup must run after the mode body. (design.md §Decision D4; specs `sessions-persistence` Requirement "Content-addressed activation and binding".)

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/commands.org` succeeds.
- `./bin/run-tests.sh -d config/gptel/sessions/test/commands` — green, including the non-session no-op.
- Done = opening a session activates+binds via the hook; scratch chat buffers are unaffected.

## Context

design.md § Decision "D4. Binding"; specs `sessions-persistence` Requirement "Content-addressed activation and binding" and "Buffer-local session state".

## Cycle 1 updates (cycle-1781448273)

- The guard is `jf/gptel--session-signature-p` (merged) — drawer-carries-a-:GPTEL_-key, NOT a path
  test; a scratch chat buffer (no drawer) is a no-op by construction. `register/boundary/session-content-signature`
  is **reconciled** and safe to depend on.
- Identity resolution depends on `drawer-identity-resolver` (blocker, not yet built) and session-dir
  on `session-dir-ancestor-walk` (deferred-ready, not yet built) — both still open.

## Cycle 2 updates (cycle-1781451784)

### Cited register entries
- `register/boundary/drawer-first-identity-resolution`: speculated → **confirmed** (resolvers merged 1ec479f). Use `jf/gptel--resolve-session-id` / `jf/gptel--resolve-branch-name` / `jf/gptel--session-type` directly; do not re-derive identity.
- `register/boundary/session-content-signature`: reconciled (cycle-1). The guard `jf/gptel--session-signature-p` is safe to depend on; a scratch chat buffer (no drawer) is a no-op by construction.
- `register/invariant/activation-and-identity-are-content-not-path`: still speculated; this task supplies the BINDING half (no path test in the guard).

### Blocker status
- Two of three blockers are now done: `drawer-identity-resolver` (resolvers) and `magic-mode-alist-activation` (the hook fires via content-addressed activation). **Remaining blocker: `session-dir-ancestor-walk`** (provides `jf/gptel--session-dir-from-branch-dir`, cycle-3). This task unblocks once the walk lands.

## Cycle 3 updates (cycle-1781453946)

### Now fully unblocked — top cycle-4 critical-path pick
- **All three blockers are done:** `drawer-identity-resolver` (cycle-2, 1ec479f), `magic-mode-alist-activation` (cycle-2, 7e524af), and `session-dir-ancestor-walk` (cycle-3, df3dcf2). This task is the natural next critical-path task.

### Cited register entries
- `register/boundary/session-dir-marker-walk`: speculated → **confirmed** (cycle-3). Consume `jf/gptel--session-dir-from-branch-dir` for `session-dir`. **Producer signature is pinned `(branch-dir type)`** — pass the resolved TYPE symbol from `jf/gptel--session-type` (`branch`|`agent`), NOT a drawer-alist. Return value is NOT truename-normalized; compare paths with `file-equal-p`, never `string=`. See `.orchestrator/cycles/cycle-1781453946/reconciliations/boundary-session-dir-marker-walk.md`.
- `register/boundary/drawer-first-identity-resolution`: **caller obligation documented** (cycle-3). When you resolve branch-name, feed the resolver a path that carries the trailing `branches/<branch>/` segment (the session.org file path), not a bare branch-dir — else the fallback regex yields nil for legacy (no-`GPTEL_BRANCH`) sessions.

### Carried meta-discovery (cycle-2, still load-bearing here)
- **Cross-subsystem reference:** this binder is chat-side code calling sessions symbols. Use a forward `declare-function` + call-time resolution (the hook fires only after both subsystems load), **never** a `require`/autoload of sessions from chat — `jf/load-module` loads by absolute path, so a feature-name autoload won't resolve and it would invert the one-directional chat→sessions dependency.

## Observations

- **The binder LIVES IN sessions (`commands.org`), not chat.** The brief framed this as "chat-side-adjacent code calling sessions symbols," but the file we modify (`config/gptel/sessions/commands.org`) is itself a sessions module that already `require`s `gptel-session-filesystem` / `gptel-session-registry`. So every sessions symbol the binder calls (`jf/gptel--session-signature-p`, the resolvers, `jf/gptel--session-dir-from-branch-dir`, `jf/gptel--register-session`) is directly available at load time — no `declare-function` needed for them. The cross-subsystem `declare-function` rule applies only to `gptel-chat-mode` (the chat symbol), which `commands.org` already declares at the top (line 76). No new forward declaration was required; the carried meta-discovery is satisfied by the pre-existing declaration. The hook is *registered* by a sessions module onto a chat-defined hook variable (`gptel-chat-mode-hook`), which is fine — registering a function on a hook does not require the hook's defining feature to be loaded.

- **Drawer-alist sourced from the LIVE buffer, not a disk re-read.** The brief's reference material was unsure whether to call `jf/gptel--read-session-drawer-head` (on-disk, takes a FILE) or a buffer variant. I used `jf/gptel--scan-session-drawer-keys` (the current-buffer scanner that `jf/gptel--session-signature-p` itself wraps), because the hook runs inside the live session buffer whose content is authoritative and may differ from disk (e.g. unsaved edits, or activation before the visit completes). It returns the identical bare-string-keyed alist `jf/gptel--session-type` / `jf/gptel--resolve-*` expect, so no signature mismatch. `jf/gptel--read-session-drawer-head` would also work but adds a redundant disk read.

- **Adjacent-test breakage from hook coexistence (RESOLVED, surgically).** Installing the binder on `gptel-chat-mode-hook` broke two pre-existing examples in `auto-init-chat-mode-spec.el` that drive `find-file-noselect` against real on-disk session files. Root cause is a genuine model divergence (see Discoveries disc-2), not a binder defect:
  1. The `parent-session-id` example wrote a drawer with `GPTEL_PARENT_SESSION_ID` but no `GPTEL_SESSION_ID`/`GPTEL_BRANCH`, in a `branches/feature-x/` path. The content classifier (`jf/gptel--session-type`) keys type SOLELY on the presence of `GPTEL_PARENT_SESSION_ID` ⇒ `agent`; the agent branch of `session-dir-from-branch-dir` returns branch-dir itself, so session-id resolved to the dir basename `feature-x` instead of `bar-...`. Because the binder runs first (during mode activation) and sets `jf/gptel--session-id`, the old `find-file-hook` auto-init then early-returns on its `(not (bound-and-true-p jf/gptel--session-id))` guard, so the binder's value is what's observed. **Fix:** made that fixture self-describing (added `:GPTEL_SESSION_ID:`/`:GPTEL_BRANCH:` via new `cl-defun` keyword args) so content and path agree. This is the correct authoring for a real branch session under the new model.
  2. The `non-session .org` example asserted absolute `hash-table-count == 0`; the binder legitimately registers the real fixture from example (1), leaking one entry past that test's key-scoped cleanup. **Fix:** changed the assertion to "no NEW entry vs. count-before" — a more correct framing that does not depend on global registry emptiness.
  These are minimal test-fixture corrections in an adjacent (path-layout) spec, documented rather than expanded; no production behavior of the old auto-init was touched.

- **Old `find-file-hook` auto-init was intentionally NOT removed.** It coexists with the new binder for now (the binder runs first and, by setting `jf/gptel--session-id`, suppresses auto-init for signature-bearing buffers). Removing the path-layout auto-init is a separate task's scope; see Discoveries disc-3 for the coexistence/teardown follow-up signal.

## Discoveries
- discovery_id: disc-mode-hook-binder-1
  class: invariant-gap
  description: |
    register/invariant/activation-and-identity-are-content-not-path
    (SPECULATED) held 1:1 for the BINDING half. The binder uses NO
    path-layout test anywhere: the guard is the content signature
    (jf/gptel--session-signature-p, a :GPTEL_ drawer key), session-type
    is drawer-only (jf/gptel--session-type), session-id/branch-name come
    from the drawer-first resolvers, and the ONLY path consumption is
    branch-dir = (file-name-directory (buffer-file-name)) [derived, not
    reverse-engineered] and session-dir via the structural branches/
    marker walk [navigation, not identity parsing]. No path test was
    needed to make binding work; the invariant did not have to be
    relaxed.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    Partial-positive evidence for the BINDING half. Promote toward
    confirmed once the ACTIVATION and IDENTITY halves (other tasks) also
    land content-only. The binder contributes no path-layout violation.

- discovery_id: disc-mode-hook-binder-2
  class: interface-drift
  description: |
    The content classifier and the legacy path-layout auto-init DISAGREE
    on the type of a session whose drawer carries GPTEL_PARENT_SESSION_ID
    but whose ON-DISK PATH is a branch layout (.../branches/<b>/session.org).
    Content model: GPTEL_PARENT_SESSION_ID present ⇒ 'agent ⇒
    session-dir = branch-dir, session-id = branch-dir basename. Path
    model (old auto-init): a branches/ path ⇒ 'branch ⇒ session-dir =
    ../.. , session-id from the grandparent dir. A test fixture in
    auto-init-chat-mode-spec.el used exactly this contradictory
    combination (parent-id drawer in a branch path) and surfaced the
    drift when the binder began running first on gptel-chat-mode-hook.
    Per register/vocabulary/identity-drawer-keys and
    register/boundary/drawer-first-identity-resolution, the content
    classification is authoritative; the fixture was the bug (a real
    agent session lives at .../agents/<name>/, and a real branch session
    omits parent-id or includes its own GPTEL_SESSION_ID). Fixed by
    making the fixture self-describing.
  affected_register_entry: register/boundary/session-dir-marker-walk
  recommendation: |
    When the legacy path-layout auto-init is retired (separate task),
    re-audit any session.org fixtures/data that pair a
    GPTEL_PARENT_SESSION_ID drawer with a branches/ path — under the
    content model those are agent-typed and resolve to branch-dir. No
    code change to the marker walk is needed; its (branch-dir type)
    contract held exactly as pinned.

- discovery_id: disc-mode-hook-binder-3
  class: duplication
  description: |
    Two hooks now derive-and-set the four buffer-local session vars:
    the new content-addressed jf/gptel--bind-session-buffer on
    gptel-chat-mode-hook (authoritative, runs first during activation),
    and the legacy path-layout jf/gptel--auto-init-session-buffer on
    find-file-hook. They coexist because the binder sets
    jf/gptel--session-id first, and auto-init early-returns on its
    (not (bound-and-true-p jf/gptel--session-id)) guard. This is benign
    today (binder wins for signature-bearing buffers; auto-init only
    acts on signatureless legacy paths the binder skips) but is
    redundant machinery and a path-layout dependency that the change as
    a whole intends to eliminate.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    A follow-up task should retire jf/gptel--auto-init-session-buffer and
    its find-file-hook registration (config/gptel/sessions/commands.org)
    once content-addressed ACTIVATION via magic-mode-alist fully
    supersedes find-file-hook detection, removing the last path-layout
    cond. Out of scope for mode-hook-binder (additive only).
