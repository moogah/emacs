# Emacs Bookmarks

This directory is where this configuration keeps its Emacs bookmark file
(built-in bookmark system). Only this README is in git.

## Files

- `bookmarks` - the default bookmark file, set in `config/core/defaults.org`:

  ```elisp
  (setq bookmark-default-file
        (expand-file-name "state/bookmarks/bookmarks" jf/emacs-dir))
  (setq bookmark-save-flag 1)   ; rewrite the file on every change
  ```

- `apploi-mac-bookmarks` - used instead on the `apploi-mac` machine role, which
  overrides `bookmark-default-file` in `config/local/apploi-mac.el`. No other
  machine role overrides it today; a role can do the same in its own
  `config/local/{machine-role}.el`.

## Not in git, not shared

- **Not version-controlled.** `.gitignore` ignores everything under `state/`
  except directories and `README.md` files, so the bookmark files here are
  local to the checkout. There is no git history or backup of them.
- **Not shared across worktrees.** `jf/emacs-dir` is the root of whichever
  checkout Emacs was started from, so every checkout or worktree of this
  configuration writes its own `state/bookmarks/` file. A bookmark made in
  one install is not seen by another unless the file is copied.
- **Older checkouts use a different file.** A checkout that predates the
  `bookmark-default-file` setting (for example the tag `v0.2.0-rc2`) does not
  set it at all, so Emacs uses its stock default under that checkout's
  `runtime/` directory (`runtime/bookmarks`).

Where a bookmark file shared between installs or machines should live is an
open question for a later piece of work.

## Bookmarks and workspace worktrees

A different problem is a bookmark on a file in a git repository that is checked
out in several places (the main clone plus one worktree per workspace). With
the workspaces portable-bookmarks module loaded, such bookmarks resolve to the
current workspace's checkout instead of the absolute path they were made in.
See *Portable bookmarks* in `config/workspaces/docs/README.org`.

## About Emacs Bookmarks

Emacs bookmarks (`C-x r m` to set, `C-x r b` to jump) provide quick access to:
- Files with saved positions
- Directories (dired bookmarks)
- Info pages
- Special buffers (org headings, occur buffers, etc.)

The bookmark system is extensible - major modes can define custom bookmark handlers.

## Key Commands

- `C-x r m` - Set bookmark at current location
- `C-x r b` - Jump to bookmark
- `C-x r l` - List all bookmarks
- `M-x bookmark-delete` - Delete a bookmark
- `M-x bookmark-save` - Save bookmarks to file
