#!/usr/bin/env bash
# invalidate-runtime.sh - Remove runtime artifacts to force rebuild
#
# This script removes runtime/straight/ and runtime/tree-sitter/ artifacts,
# forcing Emacs to rebuild from scratch on next launch. Useful when:
# - Package or grammar versions need to be updated
# - Build artifacts are corrupted
# - Testing fresh installation
#
# Usage:
#   ./bin/invalidate-runtime.sh [OPTIONS] [PACKAGE]
#
# Options:
#   --worktree PATH         Target specific worktree (default: current directory)
#   --force                 Skip confirmation prompt
#   --tree-sitter-only      Remove only tree-sitter grammars
#   --no-tree-sitter        Skip removing tree-sitter grammars
#   --help                  Show this help message
#
# Examples:
#   ./bin/invalidate-runtime.sh                    # Remove all packages + tree-sitter
#   ./bin/invalidate-runtime.sh magit              # Remove only magit package
#   ./bin/invalidate-runtime.sh --tree-sitter-only # Remove only tree-sitter grammars
#   ./bin/invalidate-runtime.sh --no-tree-sitter   # Remove packages but keep grammars
#   ./bin/invalidate-runtime.sh --worktree ~/emacs-test  # Target specific worktree
#   ./bin/invalidate-runtime.sh --force            # Skip confirmation

set -euo pipefail

# Default values
WORKTREE_PATH="$(pwd)"
FORCE=false
PACKAGE=""
TREE_SITTER_ONLY=false
NO_TREE_SITTER=false

# Show help
show_help() {
    cat << EOF
invalidate-runtime.sh - Remove runtime artifacts to force rebuild

Usage:
  $0 [OPTIONS] [PACKAGE]

Options:
  --worktree PATH         Target specific worktree (default: current directory)
  --force                 Skip confirmation prompt
  --tree-sitter-only      Remove only tree-sitter grammars
  --no-tree-sitter        Skip removing tree-sitter grammars
  --help                  Show this help message

Examples:
  $0                              # Remove all packages + tree-sitter
  $0 magit                        # Remove only magit package
  $0 --tree-sitter-only           # Remove only tree-sitter grammars
  $0 --no-tree-sitter             # Remove packages but keep grammars
  $0 --worktree ~/emacs-test      # Target specific worktree
  $0 --force                      # Skip confirmation
  $0 --worktree ~/emacs-test magit --force  # Remove magit from specific worktree

What gets removed:
  - All packages:         runtime/straight/ and runtime/tree-sitter/
  - Specific package:     runtime/straight/repos/PACKAGE and runtime/straight/build/PACKAGE
  - Tree-sitter only:     runtime/tree-sitter/
  - With --no-tree-sitter: Only runtime/straight/ (keeps tree-sitter)

After removal, Emacs will rebuild artifacts on next launch.
EOF
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --worktree)
            WORKTREE_PATH="$2"
            shift 2
            ;;
        --force)
            FORCE=true
            shift
            ;;
        --tree-sitter-only)
            TREE_SITTER_ONLY=true
            shift
            ;;
        --no-tree-sitter)
            NO_TREE_SITTER=true
            shift
            ;;
        --help|-h)
            show_help
            exit 0
            ;;
        -*)
            echo "ERROR: Unknown option: $1" >&2
            echo "Use --help for usage information" >&2
            exit 1
            ;;
        *)
            if [[ -z "$PACKAGE" ]]; then
                PACKAGE="$1"
            else
                echo "ERROR: Multiple packages specified. Remove one at a time." >&2
                exit 1
            fi
            shift
            ;;
    esac
done

# Validate flag combinations
if [[ "$TREE_SITTER_ONLY" = true ]] && [[ "$NO_TREE_SITTER" = true ]]; then
    echo "ERROR: Cannot use both --tree-sitter-only and --no-tree-sitter" >&2
    exit 1
fi

if [[ "$TREE_SITTER_ONLY" = true ]] && [[ -n "$PACKAGE" ]]; then
    echo "ERROR: Cannot specify a package with --tree-sitter-only" >&2
    exit 1
fi

# Expand path
WORKTREE_PATH="${WORKTREE_PATH/#\~/$HOME}"

# Validate worktree
if [[ ! -d "$WORKTREE_PATH" ]]; then
    echo "ERROR: Worktree not found: $WORKTREE_PATH" >&2
    exit 1
fi

RUNTIME_DIR="$WORKTREE_PATH/runtime"
STRAIGHT_DIR="$RUNTIME_DIR/straight"
TREESITTER_DIR="$RUNTIME_DIR/tree-sitter"

if [[ ! -d "$RUNTIME_DIR" ]]; then
    echo "ERROR: Runtime directory not found: $RUNTIME_DIR" >&2
    echo "       This doesn't appear to be a valid Emacs worktree" >&2
    exit 1
fi

# Determine what to remove
REMOVE_PATHS=()
TARGET_DESC=""

if [[ "$TREE_SITTER_ONLY" = true ]]; then
    # Remove only tree-sitter grammars
    if [[ ! -d "$TREESITTER_DIR" ]]; then
        echo "No tree-sitter directory found at: $TREESITTER_DIR"
        echo "Nothing to invalidate."
        exit 0
    fi
    TARGET_DESC="tree-sitter grammars"
    REMOVE_PATHS=("$TREESITTER_DIR")
elif [[ -n "$PACKAGE" ]]; then
    # Remove specific package
    if [[ ! -d "$STRAIGHT_DIR" ]]; then
        echo "No straight.el directory found at: $STRAIGHT_DIR"
        echo "Nothing to invalidate."
        exit 0
    fi

    REPOS_PATH="$STRAIGHT_DIR/repos/$PACKAGE"
    BUILD_PATH="$STRAIGHT_DIR/build/$PACKAGE"

    # Check if package exists
    if [[ ! -d "$REPOS_PATH" ]] && [[ ! -d "$BUILD_PATH" ]]; then
        echo "ERROR: Package not found: $PACKAGE" >&2
        echo "       Checked: $REPOS_PATH" >&2
        echo "       Checked: $BUILD_PATH" >&2
        exit 1
    fi

    TARGET_DESC="package '$PACKAGE'"
    [[ -d "$REPOS_PATH" ]] && REMOVE_PATHS+=("$REPOS_PATH")
    [[ -d "$BUILD_PATH" ]] && REMOVE_PATHS+=("$BUILD_PATH")
else
    # Remove all packages (and optionally tree-sitter)
    if [[ ! -d "$STRAIGHT_DIR" ]] && [[ ! -d "$TREESITTER_DIR" ]]; then
        echo "No straight.el or tree-sitter directories found"
        echo "Nothing to invalidate."
        exit 0
    fi

    if [[ "$NO_TREE_SITTER" = true ]]; then
        TARGET_DESC="all packages (keeping tree-sitter)"
        [[ -d "$STRAIGHT_DIR" ]] && REMOVE_PATHS=("$STRAIGHT_DIR")
    else
        TARGET_DESC="all packages and tree-sitter grammars"
        [[ -d "$STRAIGHT_DIR" ]] && REMOVE_PATHS+=("$STRAIGHT_DIR")
        [[ -d "$TREESITTER_DIR" ]] && REMOVE_PATHS+=("$TREESITTER_DIR")
    fi
fi

# Check if there's anything to remove
if [[ ${#REMOVE_PATHS[@]} -eq 0 ]]; then
    echo "Nothing to remove."
    exit 0
fi

# Calculate size to be removed
TOTAL_SIZE=$(du -sh "${REMOVE_PATHS[@]}" 2>/dev/null | awk '{sum+=$1} END {print sum}' || echo "unknown")
if [[ "$TOTAL_SIZE" != "unknown" ]]; then
    TOTAL_SIZE=$(du -shc "${REMOVE_PATHS[@]}" 2>/dev/null | tail -1 | cut -f1)
fi

# Show what will be removed
echo "Invalidate runtime for worktree: $WORKTREE_PATH"
echo ""
echo "Will remove: $TARGET_DESC"
for path in "${REMOVE_PATHS[@]}"; do
    echo "  - $path"
done
echo ""
echo "Size: $TOTAL_SIZE"
echo ""

# Confirm unless --force
if [[ "$FORCE" != true ]]; then
    read -p "Remove these files? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
fi

# Remove files
echo "Removing..."
for path in "${REMOVE_PATHS[@]}"; do
    rm -rf "$path"
    echo "  ✓ Removed: $path"
done

echo ""
echo "✓ Runtime invalidated!"
echo ""
echo "Next steps:"
if [[ "$TREE_SITTER_ONLY" = true ]]; then
    echo "  1. Launch Emacs: cd $WORKTREE_PATH && ./bin/emacs-isolated.sh"
    echo "     Tree-sitter grammars will be recompiled automatically"
elif [[ -n "$PACKAGE" ]]; then
    echo "  1. Launch Emacs: cd $WORKTREE_PATH && ./bin/emacs-isolated.sh"
    echo "     straight.el will rebuild $PACKAGE automatically"
else
    echo "  1. Launch Emacs: cd $WORKTREE_PATH && ./bin/emacs-isolated.sh"
    if [[ "$NO_TREE_SITTER" = true ]]; then
        echo "     straight.el will clone and build all packages from scratch"
    else
        echo "     straight.el will clone and build all packages from scratch"
        echo "     Tree-sitter grammars will be recompiled automatically"
    fi
    echo ""
    echo "  Or copy from another worktree:"
    echo "     ./bin/init-worktree-runtime.sh $WORKTREE_PATH"
fi
