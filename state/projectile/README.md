# Projectile Known Projects

This directory is where this configuration keeps projectile's known-projects
list. Only this README is in git.

## Files

- `projectile-bookmarks.eld` - the default known-projects file, set in
  `config/language-modes/projectile.org`:

  ```elisp
  (projectile-known-projects-file
   (expand-file-name "state/projectile/projectile-bookmarks.eld" jf/emacs-dir))
  ```

- `apploi-mac-bookmarks.eld` - used instead on the `apploi-mac` machine role,
  which overrides `projectile-known-projects-file` in
  `config/local/apploi-mac.el`. No other machine role overrides it today; a
  role can do the same in its own `config/local/{machine-role}.el`:

  ```elisp
  (setq projectile-known-projects-file
        (expand-file-name "state/projectile/{machine-role}-bookmarks.eld" jf/emacs-dir))
  ```

## Not in git, not shared

- **Not version-controlled.** `.gitignore` ignores everything under `state/`
  except directories and `README.md` files, so the project lists here are
  local to the checkout. There is no git history or backup of them.
- **Not shared across worktrees.** `jf/emacs-dir` is the root of whichever
  checkout Emacs was started from, so every checkout or worktree of this
  configuration keeps its own `state/projectile/` list.
