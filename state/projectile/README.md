# Projectile Known Projects

This directory contains git-controlled projectile bookmarks (known projects list).

## Files

- `projectile-bookmarks.eld` - Default list of known projects (base configuration)

## Machine-Specific Configuration

**Default behavior**: Each machine uses its own project list file.

In `config/local/{machine-role}.el`, set the machine-specific bookmarks file:

```elisp
;; Use machine-specific bookmarks file (recommended)
(setq projectile-known-projects-file
      (expand-file-name "state/projectile/apploi-mac-bookmarks.eld" jf/emacs-dir))
```

This allows each machine to have different projects while keeping them in git.

To use a shared project list across all machines:

```elisp
;; Use shared bookmarks file (alternative)
(setq projectile-known-projects-file
      (expand-file-name "state/projectile/projectile-bookmarks.eld" jf/emacs-dir))
```

## Why Git-Control Projectile Bookmarks?

Unlike the default behavior (gitignored runtime state), version-controlling projectile bookmarks allows:
- **Backup**: Restore project lists if configuration is reset
- **Sharing**: Use the same base project set across multiple machines
- **Machine-specific**: Override with local bookmarks per machine
- **History**: Track which projects were active when

## Configuration

The default location is configured in the projectile module. Machine-specific overrides go in `config/local/{machine-role}.el`.
