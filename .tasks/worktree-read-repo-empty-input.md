---
name: worktree-read-repo-empty-input
description: "In jf/workspace--worktree-read-repo, RET on empty input returns default-directory instead of reaching the read-directory-name floor; with a remote default-directory that also opens a connection."
source: workspace-portable-bookmarks cycle-1789935943 (ownership-and-picker-polish review, noted, not raised)
status: ready
task_class: bug
discovered_from: ownership-and-picker-polish
discovered_by: reviewer
discovered_class: invariant-gap
reconciled_into: null
relations:
  - discovered-from:ownership-and-picker-polish
---

`completing-read` returns "" on empty RET. `(expand-file-name "")` is
`default-directory`, which `file-directory-p` accepts, so the function returns
the current buffer's directory as the "Source git repo" without ever showing the
floor. If `default-directory` is remote, that check opens a connection. This
predates workspace-portable-bookmarks. Fix: treat an empty or whitespace-only
choice as "no choice" and fall through to `read-directory-name`, and add a spec.
