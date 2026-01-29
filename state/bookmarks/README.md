# Emacs Bookmarks

This directory contains git-controlled Emacs bookmarks (built-in bookmark system).

## Files

- `bookmarks` - Default/template bookmarks file
- `{machine-role}-bookmarks` - Machine-specific bookmarks (e.g., `apploi-mac-bookmarks`)

## Machine-Specific Configuration

**Default behavior**: Each machine uses its own bookmarks file.

In `config/local/{machine-role}.el`, set the machine-specific bookmarks file:

```elisp
;; Use machine-specific bookmarks file (recommended)
(setq bookmark-default-file
      (expand-file-name "state/bookmarks/apploi-mac-bookmarks" jf/emacs-dir))
```

This allows each machine to have different bookmarks while keeping them in git.

## About Emacs Bookmarks

Emacs bookmarks (`C-x r m` to set, `C-x r b` to jump) provide quick access to:
- Files with saved positions
- Directories (dired bookmarks)
- Info pages
- Special buffers (org headings, occur buffers, etc.)

The bookmark system is extensible - major modes can define custom bookmark handlers.

## Why Git-Control Bookmarks?

Unlike the default behavior (gitignored or in ~/.emacs.d), version-controlling bookmarks allows:
- **Backup**: Restore bookmarks if configuration is reset
- **Machine-specific**: Different bookmarks per machine (work vs personal)
- **History**: Track bookmark changes over time
- **Sync**: Share across worktrees automatically

## Configuration

The default location is set via `bookmark-default-file` variable. Machine-specific overrides go in `config/local/{machine-role}.el`.

## Key Commands

- `C-x r m` - Set bookmark at current location
- `C-x r b` - Jump to bookmark
- `C-x r l` - List all bookmarks
- `M-x bookmark-delete` - Delete a bookmark
- `M-x bookmark-save` - Save bookmarks to file
