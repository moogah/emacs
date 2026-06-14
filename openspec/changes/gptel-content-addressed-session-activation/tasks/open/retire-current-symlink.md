---
name: retire-current-symlink
description: Remove the current-symlink machinery (writer and its callers, reader, constant, helper, and tests) now that it has no production reader and identity is drawer-resident.
change: gptel-content-addressed-session-activation
status: ready
relations:
  - "blocked-by:retire-find-file-hook"
---

## Files to modify

- `config/gptel/sessions/filesystem.org` (modify) — remove `jf/gptel--update-current-symlink` (`:183`), `jf/gptel--get-current-branch-name` (`:149`), and `jf/gptel--current-symlink-path` (`:145`).
- `config/gptel/sessions/constants.org` (modify) — remove `jf/gptel-session--current-link` (`:85`).
- `config/gptel/sessions/commands.org` (modify) — remove the `jf/gptel--update-current-symlink` call at `:654` (the create path). (The `:363` call lived inside the now-deleted auto-init function — confirm it is already gone.)
- `config/gptel/sessions/branching.org` (modify) — remove the `jf/gptel--update-current-symlink` call at `:321`.
- `config/gptel/sessions/filesystem-test.el` (modify) — remove the `get-current-branch-name` assertion (`:111`).
- `config/gptel/sessions/test/filesystem/directory-templates-spec.el` (modify) — drop the symlink update step / assertions (`:146`, header note `:23`).

## Implementation steps

1. Grep first to confirm the full caller set is exactly the known sites:
   `grep -rn "update-current-symlink\|get-current-branch-name\|current-symlink-path\|current-link" config/gptel --include="*.org" --include="*-spec.el" --include="*-test.el"`.
2. Remove the three functions and the constant.
3. Remove the two production callers (create path `commands.org:654`, branch path `branching.org:321`). Branch/session creation no longer maintains a `current` pointer.
4. Remove the symlink tests / assertions.
5. Tangle every edited `.org`; run the affected test dirs.

## Design rationale

The `current` symlink had no production reader (only a test read it). Once identity is drawer-resident and session-dir is marker-derived, it encodes nothing anyone consumes; leaving write-only dead code would contradict the complete-removal intent. (design.md §Decision D6; risk "Removing the symlink breaks an external consumer" — grep-confirmed none.)

## Verification

- `./bin/tangle-org.sh` on all edited `.org` files succeeds.
- `grep -rn "current-symlink\|get-current-branch-name\|current-link" config/gptel` returns NOTHING outside historical docs.
- `./bin/run-tests.sh -d config/gptel/sessions/test/filesystem` and the ERT `filesystem-test.el` — green.
- Done = no symlink writer, reader, constant, or test remains.

## Context

design.md § Decision "D6. Retire the current symlink".

## Cycle 5 updates (cycle-1781463961)

- **Blocker cleared — now READY.** `retire-find-file-hook` merged (e398898). It already removed the **only production caller that wrote the symlink** — the `jf/gptel--update-current-symlink` call inside the deleted `jf/gptel--auto-init-session-buffer`. So this task's remaining work is the symlink machinery itself: the writer `jf/gptel--update-current-symlink`, its other callers (check `jf/gptel--create-session-core` / branch creation per the on-touch architect note), the reader `jf/gptel--get-current-branch-name`, the `jf/gptel--current-symlink-path` helper / `jf/gptel-session--current-link` constant, and the symlink tests.
- **Cited register entry** `register/invariant/activation-and-identity-are-content-not-path`: still **speculated**. With the find-file-hook/auto-init/regex remnant gone (cycle-5), the symlink is ONE of the two last path-derived remnants (the other is the 2 `session-id-from-directory` identity callers — consumer-migration). Landing this task + that migration + `regression-sweep`'s grep-audit flips the invariant to `confirmed`. See `.orchestrator/cycles/cycle-1781463961/reconciliations/invariant-activation-and-identity-are-content-not-path.md`.
- File contention: this task edits `filesystem.org` + `constants.org` + `commands.org`. No other task is ready, so it is the natural cycle-6 solo pick.

## Observations

### Full grep map (step-1, every hit classified live-vs-doc)

Initial grep (`update-current-symlink|get-current-branch-name|current-symlink-path|current-link`) over config/gptel `.org`/`-spec.el`/`-test.el`:

| Site | Classification | Action |
|------|----------------|--------|
| constants.org:85 `jf/gptel-session--current-link` defconst | live symbol | REMOVED |
| filesystem.org:290 `jf/gptel--current-symlink-path` defun | live symbol | REMOVED |
| filesystem.org:294 `jf/gptel--get-current-branch-name` defun | live symbol | REMOVED |
| filesystem.org:297/304 uses of `current-symlink-path` (inside readers) | live symbol | REMOVED with readers |
| filesystem.org:328 `jf/gptel--update-current-symlink` defun (writer) | live symbol | REMOVED |
| filesystem.org:331 use of `current-symlink-path` (inside writer) | live symbol | REMOVED with writer |
| commands.org:596 `jf/gptel--update-current-symlink` (create-path caller) | live caller | REMOVED |
| branching.org:403 `jf/gptel--update-current-symlink` (branch-path caller) | live caller | REMOVED |
| filesystem-test.el:98/106/108/111 (ERT `test-directory-creation-org-symlink`) | live test (writer+readers) | REMOVED whole deftest |
| directory-templates-spec.el:23 (header comment) | doc | REMOVED |
| directory-templates-spec.el:142/143/146/147 (symlink `it` block) | live test | REMOVED whole `it` |
| branching-integration-spec.el:287/288/289 (symlink assertion in "creates branches/..." it) | live test assertion | REMOVED assertion block only |

### BEYOND the known sites — two extra readers the task body did not list

1. **`jf/gptel--get-current-branch-dir`** (filesystem.org:301) — a symlink *reader* that calls `jf/gptel--current-symlink-path`. NOT in the task's "remove these 3 functions" list, but it is dead (zero callers anywhere in config/gptel — confirmed by grep `get-current-branch-dir`) and it depends on the `current-symlink-path` helper the task DOES remove. Leaving it would either leave a live `current-symlink-path` reference (gate failure) or leave a broken function. It is squarely the "reader/helper" the task description says to remove ("writer and its callers, reader, constant, helper"). REMOVED. See disc-retire-current-symlink-1.

2. **branching-integration-spec.el** "flushes unsaved parent edits before deriving branch-point bytes" (was ~:470) — a behavioral test that discovered the newly-created branch dir SOLELY via `(file-truename (expand-file-name "current" session-dir))`. The step-1 grep did NOT catch this because it hardcodes the literal path string `"current"` with no `current-link`/`current-symlink` token. This is a load-bearing test reader, not a trivial assertion, so I did NOT delete the test — I rewrote the discovery to scan `branches/` for the non-"main" branch via `jf/gptel--list-branches` + `jf/gptel--branch-dir-path` (both surviving filesystem helpers). The test's actual subject (dirty-buffer flush + truncation) is unchanged and still passes. The 7th file `branching-integration-spec.el` was therefore NOT in the task's 6-file list but had to be touched.

### Test assertions removed vs kept

- **filesystem-test.el**: removed the entire `test-directory-creation-org-symlink` ERT deftest (its only purpose was the symlink round-trip). Other 7 deftests untouched and green.
- **directory-templates-spec.el**: removed the single symlink `it` ("points current symlink at the active branch...") and one header comment line. All other coverage (session.org constant, path builders, branch layout, valid-branch-p) kept.
- **branching-integration-spec.el**: removed only the 3-line "Current symlink resolves to the new branch" assertion from the "creates branches/<timestamp>-<name>/session.org" it (kept all its other expectations: session.org exists, not session.md, branch-metadata.yml exists, metadata.yml absent). Rewrote the dirty-buffer test's branch discovery (see above). Removed one stale header comment.

### Auto-init call site confirmation

Confirmed: the symlink call inside the deleted `jf/gptel--auto-init-session-buffer` is already gone (removed by cycle-5 `retire-find-file-hook`). The only remaining `jf/gptel--update-current-symlink` callers were the create-path (commands.org) and branch-path (branching.org), both removed here. The "auto-initialize" strings still in commands.org:748/755 are a user-facing message about content-addressed activation, unrelated to the symlink.

### Residual `current`/`current-link` mentions intentionally left

None. Beyond the symbol gate, I also removed three stale prose references to "current symlink" (branching.org §425-block, commands.org create-session docstring, commands.org persistent-session section) and two stale test header comments, since they describe behavior that no longer exists. Final sweep: `grep -rni "symlink" config/gptel/sessions/` returns NOTHING.

### Note on the 2 session-id-from-directory identity callers (noted, NOT acted)

Per scope boundary I left both `jf/gptel--session-id-from-directory` IDENTITY callers untouched: `branching.org:399` (inside `jf/gptel--rewrite-branch-identity-keys` arg list — a DIFFERENT line from the :403 symlink call I removed) and `persistent-agent.org:602` (not edited at all). I additionally noticed the dirty-buffer test at branching-integration-spec.el:424 also calls `jf/gptel--session-id-from-directory` to seed a buffer-local var — that is test scaffolding for the identity callers' concern, left as-is. These remain the last path-derived remnant for consumer-migration.

## Discoveries
- discovery_id: disc-retire-current-symlink-1
  class: dead-branch
  description: |
    The task body named three functions to remove (current-symlink-path,
    get-current-branch-name, update-current-symlink) but a FOURTH symlink
    reader, `jf/gptel--get-current-branch-dir` (filesystem.org), also depends
    on the removed `current-symlink-path` helper and has ZERO callers anywhere
    in config/gptel. It is write-only dead code of the same family. I removed
    it too (it is the "reader/helper" the description's prose covers). Also,
    the step-1 grep token set MISSED a load-bearing symlink reader in
    branching-integration-spec.el that discovers the new branch dir via the
    literal path "current" with no hyphenated token — caught only by running
    the branching suite. Rewrote it to scan branches/ instead of deleting it.
  affected_register_entry: register/invariant/activation-and-identity-are-content-not-path
  recommendation: |
    The symlink REMOVAL half of the invariant is now fully advanced: no writer,
    no reader (incl. the previously-unlisted get-current-branch-dir), no
    constant, no helper, no test, and no prose remains — `grep -rn
    "current-symlink|get-current-branch-name|current-link" config/gptel` and
    `grep -rni "symlink" config/gptel/sessions/` are both empty. The ONLY
    remaining path-derived remnant before the invariant can flip from
    speculated to confirmed is the 2 `jf/gptel--session-id-from-directory`
    IDENTITY callers (branching.org:399, persistent-agent.org:602) — the
    separate consumer-migration task. integrate should run regression-sweep's
    grep-audit only AFTER that migration lands; this task does not close it.
