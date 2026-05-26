---
name: port-cross-module-home-org-writer-lint
description: Port the cross-module writer-allow-list lint from the home-org-user-authored-after-creation scaffold into a real test spec
change: add-workspace-home-directory
status: ready
relations:
  - discovered-from:home-org-reader-module
---

<!-- finding-source: reviewer (review of home-org-reader-module,
     advisory finding 1, cycle-20260525-200459). Captured here in
     the body since the orchestrator's task-frontmatter schema only
     permits blocked-by / discovered-from / enables on relations. -->

## Why this task exists

The scaffolded invariant
`openspec/changes/add-workspace-home-directory/scaffolding/invariants/home-org-user-authored-after-creation.el`
defines three `it` blocks:

1. **Cross-module writer-allow-list lint** — walks every `.el` under
   `config/workspaces/` (excluding `test/`) and checks for file-write
   primitives (`write-region`, `with-temp-file`, `append-to-file`,
   `delete-file`, `rename-file`, `copy-file`, `write-file`,
   `save-buffer`) whose body references `"home.org"`. Compares hits
   against a `permitted-writers` allow-list:
   `'(("scaffold.el" . "workspace--scaffold-write-home-org"))`. Any
   unexpected writer is a defect.

2. **Reader-module-specific assertion** — narrower check on
   `home-org.el` for zero file-write primitives at all.

3. **Byte-for-byte snapshot test** — forward-pinned to cycle 2+ for
   when `scaffold-module` and the command modules are wired.

The cycle-1 `home-org-reader-module` implementor ported only (2) into
`config/workspaces/test/home-org-spec.el`. Cycle-1 reviewer flagged
the omission of (1) as an advisory finding:

> "The cross-module lint is the durable enforcement mechanism for the
> load-bearing invariant, and porting it now (with an empty
> permitted-writers allow-list) would make the cycle-2 scaffold-module
> task immediately accountable to it. Without it, cycle 2's
> implementor has to remember to add the broader lint themselves, and
> the load-bearing invariant has weaker enforcement in the interim."

This task ports (1) into a real test spec, scheduled after
`scaffold-module` lands so `workspace--scaffold-write-home-org` is
present in the allow-list.

## Files to modify

- `config/workspaces/test/home-org-cross-module-spec.el` (new — Buttercup)

## Implementation steps

1. Create a NEW spec file
   `config/workspaces/test/home-org-cross-module-spec.el`. Do not
   merge into `home-org-spec.el` — the cross-module assertion is a
   structural lint that belongs in its own file (different concern;
   different setup; touches every file in the subsystem).

2. Port the cycle-1 scaffold's first `it` block from
   `openspec/changes/add-workspace-home-directory/scaffolding/invariants/home-org-user-authored-after-creation.el`
   (lines ~50-80 — read the scaffold for the canonical shape). The
   scaffold's body is a `(error "TODO: ...")` shell describing the
   implementation; your job is to make it real.

3. Use the scaffold's helper functions
   (`home-org-user-authored--ws-elisp-files`,
   `home-org-user-authored--permitted-writers`,
   `home-org-user-authored--write-fn-symbols`) as reference. Adapt:
   - Rename to `home-org-cross-module--*` to avoid collision if the
     scaffold ever becomes loadable.
   - Update `permitted-writers` allow-list to include
     `("scaffold.el" . "workspace--scaffold-write-home-org")`. After
     `scaffold-module` lands, this writer exists; before that, an
     allow-list with no matching hits is acceptable (the test passes
     trivially).

4. The lint logic:
   - Walk `(directory-files-recursively "config/workspaces/" "\\.el\\'")`.
   - Exclude paths matching `"/test/"`.
   - For each remaining file, scan the body for any of
     `(write-region with-temp-file append-to-file delete-file
     rename-file copy-file write-file save-buffer)` followed within
     ~200 chars by a string literal containing `"home.org"`.
   - For each hit, check `(basename(file) . enclosing-defun-name)` is
     in `permitted-writers`. If not, accumulate as an unexpected hit.
   - Expect the unexpected-hit list to be empty.

5. Add ONE Buttercup `describe` block with ONE `it` clause:

   ```elisp
   (describe "Invariant: home-org-user-authored-after-creation (cross-module lint)"
     (it "no module under config/workspaces/ writes to home.org except permitted scaffolder"
       (let ((unexpected (home-org-cross-module--scan-for-unexpected-writers)))
         (expect unexpected :to-equal nil
                 :reason
                 (format "Unexpected writers found: %S. Each must either be \
removed or added to home-org-cross-module--permitted-writers (and the register \
entry register/invariant/home-org-user-authored-after-creation updated to \
reflect the new permitted writer)." unexpected)))))
   ```

6. Run the verification command:
   ```bash
   ./bin/run-tests.sh -d config/workspaces
   ```
   Expected: pre-existing specs all pass (zero regressions) AND your
   new lint passes (no module writes to home.org outside the
   permitted allow-list).

## Cited register entries

- `register/invariant/home-org-user-authored-after-creation`
  (load_bearing: true) — this task is the structural enforcement
  mechanism for the invariant across the full workspaces module set.

## Verification

```bash
./bin/run-tests.sh -d config/workspaces

# Confirm exactly one writer is permitted (post-scaffold-module):
grep -n 'scaffold.el.*workspace--scaffold-write-home-org' config/workspaces/test/home-org-cross-module-spec.el

# Confirm the scan walks the subsystem (not just one file):
grep -n 'directory-files-recursively' config/workspaces/test/home-org-cross-module-spec.el
```

## Notes for the implementor

- This task is a follow-up from the cycle-1 home-org-reader-module
  reviewer (finding `home-org-reader-module-finding-1`,
  severity: advisory). The work is structural enforcement of a
  load-bearing invariant — keep the lint tight and the failure
  message actionable.
- The scaffolded `it #2` (reader-module-specific assertion) is
  already in `home-org-spec.el`; do not duplicate. The scaffolded
  `it #3` (byte-for-byte snapshot) is forward-pinned to a later
  cycle when workspace operations (autosave, switch, idle tick,
  kill-emacs) can be exercised against a real scaffolded workspace.


## Cycle 2 updates (cycle-20260525-213500)

### Status

- `status: blocked` → `status: ready`. Blocker `scaffold-module`
  closed at merge `5be66c7`. The canonical writer is now live as
  `workspace--scaffold-write-home-org` in `config/workspaces/scaffold.el`.

### Cycle-2 register-diff hits relevant to this task

- `register/invariant/home-org-user-authored-after-creation`:
  re-confirmed cycle 2. The scaffold's writer is named exactly per
  the invariant's allow-list (`workspace--scaffold-write-home-org`)
  with the `(unless (file-exists-p path) ...)` idempotency guard.
  This task becomes mechanical: lift the scaffolded structural-lint
  assertion (the cycle-1 `scaffolding/invariants/home-org-user-authored-after-creation.el`
  file) into a real spec file (suggested location:
  `config/workspaces/test/home-org-writer-lint-spec.el`), with the
  allow-list of permitted writers initially set to
  `'(workspace--scaffold-write-home-org)`.

### Implementation hint (lifted from the scaffold)

The scaffold file at
`openspec/changes/add-workspace-home-directory/scaffolding/invariants/home-org-user-authored-after-creation.el`
already holds the assertion shape (walk every `.el` under
`config/workspaces/` excluding `test/`, grep for any file-write
primitive whose body within ~200 chars references `home.org`,
collect hits, expect the set equals the allow-list). Lift verbatim;
mark `scaffolding_status_at_integrate: archived` for the cycle-1
scaffold once your spec lands. (Note: the cycle-2 integrate
disposition already marked the scaffold as `archived` against
`workspace-scaffold-pipeline` and friends; the home-org one was
already marked archived at cycle-1 integrate.)

### Cycle-2 inline-fix hits

None directly. The cycle-2 cross-contract collision (orchestrator
fix `63d60ec` removing the dead featurep branch from scaffold.el)
does not affect the writer's identity or signature.

## Observations

- The task body's prescribed `expect ... :to-equal nil :reason
  (format ...)` call shape is not valid buttercup syntax — `expect`
  passes any trailing positional args to the matcher, so `:to-equal`
  would receive `(nil :reason "...")` and signal an arity error.
  Implemented an equivalent assertion that surfaces the same
  actionable failure message: a `home-org-writer-lint--describe-failure`
  helper renders the hits list as either the empty string (success)
  or the prescribed remediation hint with the unexpected hits
  pretty-printed in. The spec asserts `(expect report :to-equal "")`,
  so on failure the buttercup output prints the full hint verbatim.
  Wording of the hint is preserved verbatim from the task body
  (modulo the helper-name rename `home-org-cross-module-` →
  `home-org-writer-lint-` per the cycle-2 file-location guidance).

- Helper-name prefix is `home-org-writer-lint--` (matching the
  cycle-2 suggested filename `home-org-writer-lint-spec.el` rather
  than the task body's earlier `home-org-cross-module-spec.el`
  proposal). Both filenames were declared acceptable in the
  spawn brief; chose the cycle-2 form. The scaffold's
  `home-org-user-authored--*` helpers are renamed wholesale to
  the new prefix in case the scaffold ever becomes loadable
  side-by-side with this spec — the scaffold is already marked
  archived per cycle-1 / cycle-2 integrate, but eliminating the
  collision risk is cheap.

- Confirmed by inspection (`grep -nE
  '\b(write-region|with-temp-file|append-to-file|delete-file|rename-file|copy-file|write-file|save-buffer)\b'`
  across `config/workspaces/*.el` excluding `test/`): the only
  current `home.org`-adjacent write site is `scaffold.el` line 29
  (`with-temp-file path` inside `workspace--scaffold-write-home-org`),
  which is on the allow-list. `scaffold.el` line 41
  (`workspace--scaffold-initial-session`) is a `with-temp-file` for
  the session-org file; `home.org` first appears in the
  `workspace-scaffold` docstring well outside the 200-char window
  from that write call. `persistence.el` line 106 is a
  `with-temp-file (workspace--state-file)` with no `home.org`
  literal anywhere nearby. The reader module `home-org.el` has
  zero write primitives. The lint passes cleanly today and is
  positioned to surface any future regression.

- The lint's correctness depends on the 200-char proximity window
  being narrower than the docstring + body gap between
  `workspace--scaffold-initial-session`'s `with-temp-file` and the
  `workspace-scaffold` docstring's `home.org` mentions. Today's gap
  is ~300+ chars; the window is comfortable but not enormous. If a
  future refactor compresses scaffold.el (e.g. inlines the
  initial-session writer next to the home.org writer), the lint
  could produce a false positive against `initial-session` —
  remediation is the same workflow the failure message prescribes
  (verify, add to allow-list, update register entry), so a false
  positive is recoverable without invariant erosion.

## Discoveries

- discovery_id: disc-port-cross-module-home-org-writer-lint-1
  class: invariant-gap
  description: |
    The cycle-1 reader-module spec (`home-org-spec.el`, "Invariant:
    reader module is read-only") and this cycle-2 cross-module
    writer-lint spec (`home-org-writer-lint-spec.el`) together
    enforce the structural half of the invariant
    `register/invariant/home-org-user-authored-after-creation`. The
    byte-for-byte snapshot half (it #3 in the original scaffold)
    is still forward-pinned; the register's `enforcement_mechanism`
    notes a `test` enforcement layer "added at cycle 2+ when the
    scaffold and command modules are wired". With cycle-2's
    scaffold-module now landed and command modules (workspace-new,
    workspace-switch, workspace-save, idle-tick, kill-emacs hook)
    visible in the merged tree, the snapshot test is implementable
    today — but is intentionally out-of-scope for this task per
    the spawn brief. The register entry's `status_note` already
    flags the snapshot as "naturally lands with scaffold-module";
    with scaffold-module merged, that natural landing has not
    happened, leaving an asymmetry between "lint is durable"
    (now true) and "snapshot is durable" (still deferred).
    Recommend integrate confirm the snapshot follow-up is filed
    (or file one) so the invariant's third enforcement leg is not
    silently dropped.
  affected_register_entry: register/invariant/home-org-user-authored-after-creation
  recommendation: |
    No change to the entry's status or fields. Verify the snapshot-
    test follow-up exists; if not, file a task `home-org-byte-equivalence-snapshot`
    so the third enforcement leg (it #3 in the cycle-1 scaffold) is
    on the backlog. Today's two enforcement legs (narrow read-only
    on `home-org.el`; broad writer-allow-list on `config/workspaces/`)
    are sufficient to keep the invariant load-bearing in the
    interim.
