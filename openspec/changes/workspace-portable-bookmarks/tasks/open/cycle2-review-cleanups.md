---
name: cycle2-review-cleanups
description: Refuse remote workspace homes (user decision); move consumer specs off file-equal-p; make the off-workspace "no workspace" answer honest
change: workspace-portable-bookmarks
status: blocked
task_class: bug
on_critical_path: false
cycle: cycle-1790453714
cites_register_entries:
  - register/invariant/directory-identity-by-device-inode
  - register/invariant/no-silent-wrong-file-open
relations:
  - discovered-from:workspaces-file-equal-p-timestamp-race
  - discovered-from:docs-readme
  - blocked-by:discovery-scope-hardening
discovered_from: [review workspaces-file-equal-p-timestamp-race findings 1+2, review docs-readme finding 1]
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
created_at: 2026-09-27T23:00:00+00:00
---

> Follow-up created by the orchestrator during cycle-1790453714 execute. Runs after `discovery-scope-hardening` merges
> (step 3 edits portable-bookmarks.org).

## Steps
1. **Remote workspace homes are unsupported (USER DECISION 2026-09-27: "never; refuse them").** Since the core identity predicate
   `workspace--same-file-p` answers nil for remote paths, the duplicate-home guard (`workspace--registered-for-home-p`, tabs) no longer
   catches a second anchor of the same TRAMP home, and registration then silently overwrites the entry. Refuse a remote home
   (`file-remote-p`) at every entry point that registers one — create (scaffold) and anchor (`C-u`), and any other path that puts a home
   into the registry — with a clear `user-error` naming the path, before any filesystem work. Spec each entry point. State it in the
   `tabs.org` prose, the README (*Creating workspaces* / the double-registration paragraph) and report the register wording for
   `directory-identity-by-device-inode` in `## Discoveries`.
2. **Specs off `file-equal-p`.** ~20 assertions in `test/portable-bookmarks-jump-spec.el` and `test/portable-bookmarks-off-workspace-spec.el`
   use `file-equal-p` for path equality (jump-spec :168 and :261 compare directories — the exact race case). Switch them to
   `workspace--same-file-p`.
3. **The "no workspace" answer at the off-workspace prompt.** The README (*Off-workspace jumps*) and the prompt function's docstring in
   `portable-bookmarks.org` say an empty answer means "no workspace". With a default set (the first holder) stock `completing-read`
   returns the default on empty input, and vertico's RET picks the preselected candidate, so "no workspace" is reachable only when no
   workspace holds the repo, and then only via M-RET. Make docs and behaviour agree: the smallest honest fix is to correct the docstring
   and README; if a real "no workspace" choice is wanted, add it as an explicit candidate. Pick the former unless the code already
   intends the latter, and add a spec that exercises `completing-read`'s real default-on-empty semantics (not a replaced `completing-read`).
4. Tangle each touched `.org`; commit `.org` + `.el`.

## Verification
- `./bin/run-tests.sh -d config/workspaces </dev/null` green; full suite NO-REGRESSION via compare-baseline.py.
- `grep -n file-equal-p config/workspaces/test/portable-bookmarks-*-spec.el` empty.
