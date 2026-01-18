#!/usr/bin/env bash
# invalidate-runtime.sh - Remove straight.el artifacts to force rebuild
#
# This script removes runtime/straight/ artifacts, forcing Emacs to rebuild
# packages from scratch on next launch. Useful when:
# - Package versions need to be updated
# - Build artifacts are corrupted
# - Testing fresh package installation
#
# Usage:
#   ./bin/invalidate-runtime.sh [OPTIONS] [PACKAGE]
#
# Options:
#   --worktree PATH    Target specific worktree (default: current directory)
#   --force            Skip confirmation prompt
#   --help             Show this help message
#
# Examples:
#   ./bin/invalidate-runtime.sh                    # Remove all packages (current worktree)
#   ./bin/invalidate-runtime.sh magit              # Remove only magit package
#   ./bin/invalidate-runtime.sh --worktree ~/emacs-test  # Target specific worktree
#   ./bin/invalidate-runtime.sh --force            # Skip confirmation

set -euo pipefail

# Default values
WORKTREE_PATH="$(pwd)"
FORCE=false
PACKAGE=""

# Show help
show_help() {
    cat << EOF
invalidate-runtime.sh - Remove straight.el artifacts to force rebuild

Usage:
  $0 [OPTIONS] [PACKAGE]

Options:
  --worktree PATH    Target specific worktree (default: current directory)
  --force            Skip confirmation prompt
  --help             Show this help message

Examples:
  $0                              # Remove all packages (current worktree)
  $0 magit                        # Remove only magit package
  $0 --worktree ~/emacs-test      # Target specific worktree
  $0 --force                      # Skip confirmation
  $0 --worktree ~/emacs-test magit --force  # Remove magit from specific worktree

What gets removed:
  - All packages:     runtime/straight/
  - Specific package: runtime/straight/repos/PACKAGE and runtime/straight/build/PACKAGE

After removal, straight.el will rebuild on next Emacs launch.
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

# Expand path
WORKTREE_PATH="${WORKTREE_PATH/#\~/$HOME}"

# Validate worktree
if [[ ! -d "$WORKTREE_PATH" ]]; then
    echo "ERROR: Worktree not found: $WORKTREE_PATH" >&2
    exit 1
fi

RUNTIME_DIR="$WORKTREE_PATH/runtime"
STRAIGHT_DIR="$RUNTIME_DIR/straight"

if [[ ! -d "$RUNTIME_DIR" ]]; then
    echo "ERROR: Runtime directory not found: $RUNTIME_DIR" >&2
    echo "       This doesn't appear to be a valid Emacs worktree" >&2
    exit 1
fi

if [[ ! -d "$STRAIGHT_DIR" ]]; then
    echo "No straight.el directory found at: $STRAIGHT_DIR"
    echo "Nothing to invalidate."
    exit 0
fi

# Determine what to remove
if [[ -n "$PACKAGE" ]]; then
    # Remove specific package
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
    REMOVE_PATHS=()
    [[ -d "$REPOS_PATH" ]] && REMOVE_PATHS+=("$REPOS_PATH")
    [[ -d "$BUILD_PATH" ]] && REMOVE_PATHS+=("$BUILD_PATH")
else
    # Remove entire straight.el directory
    TARGET_DESC="all packages"
    REMOVE_PATHS=("$STRAIGHT_DIR")
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
if [[ -n "$PACKAGE" ]]; then
    echo "  1. Launch Emacs: cd $WORKTREE_PATH && ./bin/emacs-isolated.sh"
    echo "     straight.el will rebuild $PACKAGE automatically"
else
    echo "  1. Launch Emacs: cd $WORKTREE_PATH && ./bin/emacs-isolated.sh"
    echo "     straight.el will clone and build all packages from scratch"
    echo ""
    echo "  Or copy from another worktree:"
    echo "     ./bin/init-worktree-runtime.sh $WORKTREE_PATH"
fi
