---
name: emacs-modular-config
description: Use when adding/modifying Emacs modules, debugging module loading issues, or managing packages with straight.el
---

# Emacs Modular Configuration System

## Overview

**Modular Loading**: Jeff's Emacs uses `jf/enabled-modules` list and `jf/load-module()` function for explicit module control with error handling.

**Core principle**: Define modules in list → loader finds files → loads with error handling → config continues even if module fails

## When to Use

Use this skill when:
- Adding new Emacs functionality or modules
- Module fails to load or causes errors
- Need to reload module for testing changes
- Package installation fails or needs debugging
- Understanding module load order matters

## Quick Reference

| Task | Function/Variable | Usage |
|------|------------------|-------|
| Add module | `jf/enabled-modules` | Add module path to list in init.org |
| Load module | `jf/load-module` | Automatic during init |
| Reload for testing | `jf/reload-module` | `M-x jf/reload-module` |
| Path resolution | `jf/resolve-module-path` | Converts module path to file path |
| Config root | `jf/emacs-dir` | Root emacs directory |
| Machine name | `jf/machine-name` | Current hostname |
| Debug loading | `jf/module-debug` | Set to `t` for verbose output |

## Module System

### Key Functions

**jf/load-module**: Loads module with error handling
- Continues loading even if module fails
- Error details go to `*Messages*` buffer
- Machine-specific configs auto-loaded from `local/{hostname}.el`

**jf/reload-module**: Interactive reload for testing
- Use for iterative development
- No need to restart Emacs

**jf/resolve-module-path**: Converts module path to file path
- Handles category directories automatically
- Resolves relative to `jf/emacs-dir`

### Adding New Modules

1. Add to `jf/enabled-modules` in init.org:
```elisp
(setq jf/enabled-modules
  '("core/completion"
    "major-modes/org-roam"
    "your-new-module"))  ; Add here
```

2. Create module file following structure (see emacs-literate-programming skill)
3. Test with `M-x jf/reload-module` before full restart

### Module Load Order

- Core modules first
- Order matters for dependencies
- Machine configs loaded last (automatic)
- Error handling allows continuation

## Package Management (straight.el)

### Configuration
- **Package manager**: straight.el (not package.el)
- **Default behavior**: `straight-use-package-by-default t`
- **Storage**: `~/.emacs.d/straight/`

### Quick Reference

| Issue | Solution |
|-------|----------|
| Package won't install | Check `*Messages*` for errors, verify repo access |
| Stale package | `M-x straight-rebuild-package` |
| Corrupted cache | Delete straight directories, restart |
| Wrong version | Check straight recipe, rebuild package |

### Package Template
```elisp
(use-package package-name
  :straight (package-name :type git :host github :repo "user/repo")
  :bind (("C-c k" . package-function))
  :hook (mode . package-mode)
  :config
  (setq package-setting value))
```

## Debugging

### Module Loading Issues

1. **Check variable**: `C-h v jf/machine-name` to verify hostname
2. **Enable debug**: `(setq jf/module-debug t)` in init.org
3. **Check messages**: `*Messages*` buffer shows load errors
4. **Verify path**: Module file must exist at resolved path
5. **Check list**: Module must be in `jf/enabled-modules`

### Package Issues

1. Check `*Messages*` for error details
2. Verify network/repo accessibility
3. Try `M-x straight-rebuild-package`
4. Delete straight cache if corrupted

## Common Mistakes

| Mistake | Why Bad | Fix |
|---------|---------|-----|
| Module not in list | Won't load | Add to `jf/enabled-modules` |
| Wrong file path | Load fails | Check `jf/resolve-module-path` output |
| No error checking | Silent failures | Check `*Messages*` buffer |
| Restart for every change | Slow workflow | Use `jf/reload-module` for testing |
| Using package.el | Wrong package manager | Use straight.el with use-package |
| Skipping machine configs | Settings not applied | Machine configs auto-load from `local/{hostname}.el` |
