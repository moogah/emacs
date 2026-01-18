---
name: emacs-literate-programming
description: Use when modifying Emacs config files - ensures you edit .org source files not generated .el files, and properly tangle changes
---

# Emacs Literate Programming Workflow

## Overview

**Literate Configuration**: Jeff's Emacs config uses org-mode files as source. Editing `.el` files directly is forbidden - they're auto-generated and changes will be overwritten.

**Core principle**: `.org` files are source of truth → tangle to `.el` files → Emacs loads `.el` files

## When to Use

Use this skill when:
- About to edit any file in `emacs/` directory
- User mentions Emacs configuration changes
- You see both `.org` and `.el` files in emacs/ directories
- Auto-tangle fails or syntax errors appear

**DO NOT edit `.el` files directly - they get overwritten when tangled**

## Quick Reference

| Task | Method | Command/Trigger |
|------|--------|-----------------|
| Normal editing | Auto-tangle | Save `.org` file (requires `#+auto_tangle: y`) |
| Manual tangle | Emacs command | `C-c C-v t` |
| CLI tangling | Script | `./bin/tangle-org.sh path/to/file.org` |
| Batch tangle | Script + find | `find emacs/ -name "*.org" -exec ./bin/tangle-org.sh {} \;` |

## Tangling Methods

### Auto-Tangle (Primary Method)
- **Trigger**: Save the `.org` file
- **Requirement**: `#+auto_tangle: y` header in file
- **Result**: Automatically generates `.el` file

### Manual Tangle in Emacs
- **When**: Auto-tangle fails or disabled
- **Command**: `C-c C-v t` (org-babel-tangle)

### CLI Tangling (bin/tangle-org.sh)
```bash
# Single file
./bin/tangle-org.sh emacs/major-modes/org.org

# All org files
find emacs/ -name "*.org" -exec ./bin/tangle-org.sh {} \;
```

**Features**: Auto-finds Emacs, validates files, batch mode, clear errors

## Required File Headers

```org
#+title: Module Name
#+property: header-args:emacs-lisp :tangle module-name.el
#+auto_tangle: y
```

## After Tangling

1. **Restart Emacs** or use `jf/reload-module` to load changes
2. **Test changes** ensure they work
3. **Commit both files** - keep .org and .el in sync

## Common Mistakes

| Mistake | Why Bad | Fix |
|---------|---------|-----|
| Editing `.el` files directly | Changes overwritten on next tangle | Edit `.org` file instead |
| Committing only `.org` | `.el` out of sync, breaks config | Commit both `.org` and `.el` |
| Forgetting to tangle | Changes in `.org` not applied | Save (auto-tangle) or run `C-c C-v t` |
| Missing `#+auto_tangle: y` | Manual tangling required | Add header to `.org` file |
| Property line not activated | Tangling fails silently | Press `C-c C-c` on `#+PROPERTY` line |
| Syntax errors in org blocks | Tangle fails silently | Use Claude Code hook for validation |

## Troubleshooting

When auto-tangling or manual tangling fails, see [troubleshooting-tangling.md](references/troubleshooting-tangling.md) for:
- Systematic debugging process
- Common issues and solutions (especially property activation with `C-c C-c`)
- Understanding multiple tangle directives and precedence
- Path resolution (relative vs absolute)
- Header arguments syntax reference

**Quick fix for most issues**: Press `C-c C-c` on the `#+PROPERTY: header-args` line to activate it.

## Syntax Protection

Claude Code hook (`validate_elisp_syntax.py`) validates elisp before writing `.el` files to prevent invalid code.
