## Context

Today a gptel session is recognized, activated, bound, and identified by reverse-engineering its filesystem path. A global `find-file-hook` (`jf/gptel--auto-init-session-buffer`, `config/gptel/sessions/commands.org`) fires on every file open, matches `session.org` against three hardcoded layout regexes (branch / nested-agent / flat-agent), walks up with `../..` / `../../..`, and derives identity from a directory basename (`jf/gptel--session-id-from-directory`, `filesystem.org:124`). Offline discovery (`jf/gptel--init-registry`, `registry.org:58`) independently derives identity from directory names, opening no files.

The session's *configuration* (preset, scope, parent link) already lives self-describingly in the `:PROPERTIES:` drawer at `point-min`; only activation and identity still depend on the path. This change finishes the move so a session is recognized by what it *contains* and identified by what its drawer *says*. It folds Thread A (activation/binding) and Thread F (identity) from `.tasks/explore-working-directory-scoping.org`.

Grounding facts established during exploration (verified to file:line):
- Both creation paths write complete content (drawer + body) in a single `write-region` *before* opening: `create-session-core` (`commands.org:47`, open at `:805`) and `persistent-agent--task` (`persistent-agent.org:352` write, `:597` `find-file-noselect`). So a content signature is always on disk at open time.
- `set-auto-mode` precedence is mode-cookie → `magic-mode-alist` → `auto-mode-alist`, so a `magic-mode-alist` entry overrides the default `.org → org-mode` mapping; `gptel-chat-mode` derives from `org-mode`, so org features survive.
- The `current` symlink is written at 3 sites (`commands.org:363,654`; `branching.org:321`) but its only *reader* (`jf/gptel--get-current-branch-name`) is called solely by a test — no production reader.
- `jf/gptel--list-branches` (`filesystem.org:200`) enumerates `branches/` subdirs as a file locator; this remains valid as storage traversal even when identity moves to the drawer.

Constraints (from the exploration doc, binding on this design):
- gptel must remain operable with the **workspaces package absent** (one-way dependency).
- Prefer **self-describing** sessions over path reverse-engineering.
- **Beta, no-migration default** — basename-fallback is the grace path, not a migration; no on-disk rewrite of existing sessions.

## Goals / Non-Goals

**Goals:**
- Replace the global `find-file-hook` activation with content-addressed activation via `magic-mode-alist`, keyed on the drawer signature (any `:GPTEL_`-prefixed property in a real `point-min` drawer).
- Make identity drawer-resident: `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:` authoritative, with basename/segment fallback for legacy files.
- Move buffer binding (the four buffer-locals, registry registration, autosave) into a guarded `gptel-chat-mode-hook` function.
- Make offline discovery (`init-registry`) read drawers for identity instead of directory names.
- Retire the `find-file-hook` registration, `jf/gptel--auto-init-session-buffer`, the three layout regexes, and the now-dead `current` symlink machinery — completely.
- Preserve auto-adoption: opening any drawer-bearing `session.org` (dired, recentf, bookmark) still activates `gptel-chat-mode`.

**Non-Goals:**
- Setting `default-directory` / a work-root (Thread B) — separate change.
- The dynamic preamble, agent cwd/scope selection, or worktree manifest (Threads D/E).
- Using dir-locals as the workspaces↔gptel preamble-source seam (Thread C) — that's the right home for dir-locals, but it's tree-shared config, out of scope here.
- Any on-disk migration of existing sessions.

## Decisions

### D1. Activation: `magic-mode-alist` keyed on the drawer signature

Register one `magic-mode-alist` entry `(jf/gptel--session-signature-p . gptel-chat-mode)`. The predicate, scanning only the buffer head:
1. skips leading blank lines;
2. requires a `:PROPERTIES:` line as the first non-blank content;
3. requires, before the drawer's `:END:`, at least one property line whose key matches `:GPTEL_[A-Z_]+:`.

This reuses the parsing shape already proven in `gptel-chat--declared-preset` (`menu.org:152-173`), which does native regex drawer reading with no dependency on org being loaded.

**Why over alternatives:**
- *vs `find-file-hook`* (today): the hook runs on every file open and dispatches by path; magic-mode-alist runs through `set-auto-mode` only at mode selection and dispatches by content. Content-addressed, no per-open cost on unrelated files.
- *vs `auto-mode-alist` on `session\.org\'`*: filename-based, collides with any other `session.org`, and still says nothing about identity. Rejected.
- *vs file-local `mode:` cookie as primary*: most explicit, but a first-line cookie collides with the `point-min` drawer invariant that `declared-preset`/`apply-drawer-overrides` rely on, and a trailing `Local Variables:` cookie revives the LV-block scar tissue (`clean-duplicate-local-vars`). A cookie does not auto-adopt old/foreign files. The signature in a real drawer is itself an explicit authored marker, so content-addressing here is "recognize an explicit declaration," not "guess from layout." (A cookie remains supported as a secondary route — see chat-mode spec — just not the primary mechanism.)

**Anchoring / false-match guard:** the predicate matches only a real drawer at `point-min`; an org file that merely quotes `:GPTEL_PRESET:` in prose or a src block does not match (covered by a spec scenario and a Buttercup test).

### D2. Identity: drawer-resident, drawer-first with basename fallback

New drawer keys `:GPTEL_SESSION_ID:` and `:GPTEL_BRANCH:` are authoritative. Resolution helper returns the drawer value when present, else falls back to `jf/gptel--session-id-from-directory` (basename) for the id and the `branches/<branch>/` segment for the branch. `jf/gptel--session-id-from-directory` is retained but demoted to the fallback path.

**Why:** fully self-describing and move-safe for new sessions; zero-friction back-compat for the (beta) existing sessions, with no rewrite. The fallback is the no-migration grace path the constraints require.

**Writers** emit the keys into the drawer they already render:
- `create-session-core` (`commands.org:544`) — branch sessions.
- `persistent-agent--task` (`persistent-agent.org`) — agents (own id + `:GPTEL_PARENT_SESSION_ID:`).
- Branch creation (`branching.org`) — copies parent drawer verbatim then overwrites `:GPTEL_BRANCH:` with the new branch name and sets `:GPTEL_SESSION_ID:` to the shared id.

Splicing reuses `jf/gptel--append-drawer-property` (`commands.org:445`), which preserves the `:PROPERTIES:`/`:END:` adjacency invariant.

### D3. Agent identity: own `GPTEL_SESSION_ID` + parent link

Agents carry their own `:GPTEL_SESSION_ID:` (like any session) *plus* `:GPTEL_PARENT_SESSION_ID:` for the link. Session **type** is inferred from `:GPTEL_PARENT_SESSION_ID:` presence (agent) vs absence (branch) — no path-layout discrimination.

**Why over parent-id + agent-name composite:** one uniform identity rule for branches and agents; the registry keys uniformly on `"session-id/branch-name"`; no second identity scheme to special-case.

### D4. Binding: guarded function on `gptel-chat-mode-hook`

A binder runs from `gptel-chat-mode-hook`. It:
1. Guards on the session signature (drawer carries a `:GPTEL_` key) — *not* on a path layout — so a `gptel-chat-new` scratch buffer (no drawer) is a no-op.
2. Resolves identity (D2) and sets `jf/gptel--session-id`, `jf/gptel--branch-name`, `jf/gptel--branch-dir`, `jf/gptel--session-dir`. `branch-dir` = `(file-name-directory (buffer-file-name))`. `session-dir` per D5.
3. Registers in `jf/gptel--session-registry`; enables `jf/gptel-autosave-enabled`.

Ordering note: mode activation wipes buffer-locals via `kill-all-local-variables`, so the binder (a hook entry) runs after the wipe by construction — same property the old auto-init relied on. The existing `gptel-chat--apply-declared-preset` (also on `gptel-chat-mode-hook`) continues to apply preset/drawer-overrides/parent-id; the binder and preset-application are independent hook entries and must not depend on each other's success (mirrors the old `condition-case` isolation).

**Why a hook entry, not the mode body:** matches the established pattern in this codebase (`apply-startup-visibility`, `install-preset-hooks` are all `add-hook` entries, not body forms — `mode.org:1030`, `menu.org:960`) and the documented reason that `org-fold` and buffer-local setup behave correctly only after the body runs.

### D5. session-dir: ancestor-marker walk

`session-dir` = the nearest ancestor directory of `branch-dir` that contains a `branches/` child; for agent sessions (which live under `branches/<b>/agents/<agent>/` and do not branch), `session-dir` = the agent's own directory.

**Why over dir-locals / drawer key:** `session-dir`'s only remaining job (after the symlink retires, D6) is "the directory under which `branches/` lives" — which the walk computes *as its literal definition*. It is move-safe and stores nothing. dir-locals was rejected because the value is a static literal (absolute = stale on move; `eval` form prompts), and because the cascade hands agents the parent's session-dir — the per-entity-inheritance trap the exploration doc flagged. A relative drawer key would work but is redundant with what the walk derives for free. Finding a *container by structural marker* is legitimate navigation, distinct from the rejected practice of parsing *identity* out of path segments.

### D6. Retire the `current` symlink

Remove `jf/gptel--update-current-symlink` and its 3 callers, the reader `jf/gptel--get-current-branch-name`, the `jf/gptel-session--current-link` constant (`constants.org:85`) / `current-symlink-path` helper, and the symlink tests.

**Why:** no production code reads it (only a test does); once identity is drawer-resident and `session-dir` is marker-derived, the symlink encodes nothing anyone consumes. Leaving write-only dead code would contradict the "complete removal" intent.

### D7. Discovery: `init-registry` reads drawers

`init-registry` continues to locate session files via directory traversal (`list-session-directories` → `list-branches`) but learns each file's identity by a cheap head-read of its drawer (the same `magic`-signature parse, run with `insert-file-contents` into a temp buffer over the first ~4KB). Registry keys come from the drawer (basename/segment fallback). Directory names lose all identity meaning.

**Why:** the user chose full path-independence over the basename==drawer-id invariant (which would have re-introduced the very coupling Thread F removes). Cost is one small read per session at init — modest next to the directory stat-walk already performed.

## Risks / Trade-offs

- **[Signature false-match on a non-session org file]** → Anchor the predicate to a real `point-min` drawer with a `:GPTEL_`-keyed property line; never a bare substring. Covered by an explicit "does not false-match" spec scenario + test.
- **[magic-mode-alist hijacks a user's own `session.org` that isn't gptel's]** → Only files whose `point-min` drawer carries a `:GPTEL_` property match; ordinary org `session.org` files (no such drawer) fall through to `org-mode`.
- **[Discovery I/O regression with many sessions]** → Head-read is bounded (~4KB via `insert-file-contents` partial read) and runs once at registry init. Acceptable; can be lazy-loaded later if it ever bites.
- **[Old sessions lack identity keys]** → Basename/segment fallback keeps them recognized (via `:GPTEL_PRESET:`) and bound. No rewrite; beta/no-migration honored.
- **[Removing the symlink breaks an external consumer]** → Grep confirms no production reader; the only caller is a test, removed with it. If an out-of-tree script relied on it, that is accepted breakage under the beta policy.
- **[Binder runs on a non-session chat buffer]** → The signature guard makes it a no-op without a drawer; scratch chat buffers are unaffected (spec scenario + test).
- **[Hook ordering between binder and preset-application]** → Both are independent `gptel-chat-mode-hook` entries; neither depends on the other's success (condition-case isolation preserved). branch-dir/identity do not require preset application to have run.

## Migration Plan

No data migration. Rollout is purely code:
1. Add the signature predicate + `magic-mode-alist` registration and the mode-hook binder (additive; can coexist with the old hook briefly during development).
2. Add identity-key emission to the three writers and the drawer-first/basename-fallback resolver.
3. Switch `init-registry`/discovery to drawer reads.
4. Remove the `find-file-hook` registration, `jf/gptel--auto-init-session-buffer` + regexes, and the `current` symlink machinery.
5. Rework affected tests; tangle (`./bin/tangle-org.sh`) and run `./bin/run-tests.sh -d config/gptel`.

Rollback: revert the commit(s); existing sessions are unaffected (no on-disk change was made).

## Testing Approach

- **Framework:** Buttercup (`*-spec.el`), the project-preferred framework. ERT files touched only where they already exist (e.g. `filesystem-test.el` symlink tests are deleted with D6).
- **Location / naming:** co-located under the existing `config/gptel/**/test/` trees, mirroring source layout:
  - `config/gptel/sessions/test/commands/` — signature predicate (incl. false-match), mode-hook binder (incl. non-session no-op), identity-key emission.
  - `config/gptel/sessions/test/` (filesystem/registry) — drawer-first/basename-fallback resolution, drawer-based discovery, session-dir ancestor-marker walk.
  - `config/gptel/sessions/test/branching/` — branch drawer identity (shared id, overwritten branch), registry integration.
  - `config/gptel/tools/test/persistent-agent/` — agent identity keys, content-addressed activation; rework `auto-init-reload-spec.el`.
  - Retire/replace `auto-init-chat-mode-spec.el`, `auto-init-resilience-spec.el` (they assert the find-file-hook pipeline).
- **Running:** `./bin/run-tests.sh -d config/gptel` (all); `-d config/gptel/sessions/test/commands` etc. for subsets; `make test-report DIR=config/gptel` for a concise pass.
- **Patterns:** behavioral level — exercise real code paths, mock only at the boundary (Emacs primitives / `find-file-noselect`) via `cl-letf` scoped to the function under test, per the project's testing doctrine. Declare drawer/file fixtures explicitly rather than relying on stateful global setup.
- **Scenario mapping:** each `#### Scenario:` in the four delta specs maps to a Buttercup `it`, referenced by a comment `;; Scenario: specs/<capability>/spec.md § <requirement>`.

## Open Questions

- Exact head-read budget for the discovery drawer parse (full file vs first N bytes) — pick the smallest that reliably captures the `point-min` drawer; verify against the largest real `session.org`.
- Whether `list-branches` should additionally tolerate a branch dir whose `session.org` lacks any `:GPTEL_` drawer (corrupt/partial) — current plan: skip it from the registry and log at debug, matching today's `valid-*-directory-p` gating.
