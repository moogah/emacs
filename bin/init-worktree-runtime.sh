#!/usr/bin/env bash
# init-worktree-runtime.sh - Copy straight.el artifacts from main worktree to new worktree
#
# This script copies the runtime/straight/ directory from the main worktree (or specified
# source) to a target worktree. This avoids re-downloading and rebuilding 142 packages
# (~922MB) when creating new worktrees.
#
# Usage:
#   ./bin/init-worktree-runtime.sh TARGET_WORKTREE [--source SOURCE_WORKTREE]
#
# Examples:
#   ./bin/init-worktree-runtime.sh ~/emacs-feature-name
#   ./bin/init-worktree-runtime.sh ~/emacs-test --source ~/.emacs.d
#
# What gets copied:
#   - runtime/straight/repos/  (746MB) - Git clones of all packages
#   - runtime/straight/build/  (175MB) - Built/compiled packages
#   - runtime/straight/build-cache.el (529KB) - Build cache metadata
#
# What does NOT get copied (worktree-specific state):
#   - custom.el, org-roam.db, history, bookmarks, recentf
#   - cache/, state/, data/, persist/, backups/
#   - snippets/, templates/ (use 'git submodule update --init')

set -euo pipefail

# Ignore SIGPIPE to handle cases where output might be truncated
trap '' PIPE

# Parse arguments
TARGET_WORKTREE=""
SOURCE_WORKTREE="$HOME/emacs"

while [[ $# -gt 0 ]]; do
    case $1 in
        --source)
            SOURCE_WORKTREE="$2"
            shift 2
            ;;
        -*)
            echo "ERROR: Unknown option: $1" >&2
            echo "Usage: $0 TARGET_WORKTREE [--source SOURCE_WORKTREE]" >&2
            exit 1
            ;;
        *)
            if [[ -z "$TARGET_WORKTREE" ]]; then
                TARGET_WORKTREE="$1"
            else
                echo "ERROR: Multiple target worktrees specified" >&2
                exit 1
            fi
            shift
            ;;
    esac
done

# Validate target worktree was provided
if [[ -z "$TARGET_WORKTREE" ]]; then
    echo "ERROR: Target worktree path required" >&2
    echo "Usage: $0 TARGET_WORKTREE [--source SOURCE_WORKTREE]" >&2
    exit 1
fi

# Expand paths
SOURCE_WORKTREE="${SOURCE_WORKTREE/#\~/$HOME}"
TARGET_WORKTREE="${TARGET_WORKTREE/#\~/$HOME}"

# Validate source worktree
if [[ ! -d "$SOURCE_WORKTREE" ]]; then
    echo "ERROR: Source worktree not found: $SOURCE_WORKTREE" >&2
    exit 1
fi

SOURCE_RUNTIME="$SOURCE_WORKTREE/runtime"
SOURCE_STRAIGHT="$SOURCE_RUNTIME/straight"

if [[ ! -d "$SOURCE_STRAIGHT" ]]; then
    echo "ERROR: Source straight.el directory not found: $SOURCE_STRAIGHT" >&2
    echo "       Run Emacs in source worktree first to initialize packages" >&2
    exit 1
fi

# Validate target worktree
if [[ ! -d "$TARGET_WORKTREE" ]]; then
    echo "ERROR: Target worktree not found: $TARGET_WORKTREE" >&2
    echo "       Create worktree first: git worktree add $TARGET_WORKTREE -b branch-name" >&2
    exit 1
fi

TARGET_RUNTIME="$TARGET_WORKTREE/runtime"

if [[ ! -d "$TARGET_RUNTIME" ]]; then
    echo "ERROR: Target runtime directory not found: $TARGET_RUNTIME" >&2
    echo "       This doesn't appear to be a valid Emacs worktree" >&2
    exit 1
fi

TARGET_STRAIGHT="$TARGET_RUNTIME/straight"

# Check if target already has straight.el installed
if [[ -d "$TARGET_STRAIGHT" ]]; then
    echo "WARNING: Target worktree already has straight.el packages installed"
    echo "         Target: $TARGET_STRAIGHT"
    echo ""
    read -p "Overwrite existing packages? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
    echo "Removing existing straight.el directory..."
    rm -rf "$TARGET_STRAIGHT"
fi

# Show what we're about to copy
echo "Copying straight.el artifacts:"
echo "  From: $SOURCE_STRAIGHT"
echo "  To:   $TARGET_STRAIGHT"
echo ""

# Calculate source size (skip in non-TTY to avoid pipe issues)
if [[ -t 1 ]]; then
    SOURCE_SIZE=$(du -sh "$SOURCE_STRAIGHT" 2>/dev/null | cut -f1)
    echo "Source size: $SOURCE_SIZE"
    echo ""
fi

# Copy straight.el directory
echo "Copying (this may take a minute)..."
START_TIME=$(date +%s)

# Create target directory
mkdir -p "$TARGET_STRAIGHT"

# Copy repos, build, and build-cache.el
# Prefer Homebrew rsync for better progress reporting
if command -v rsync &> /dev/null; then
    # Check rsync version to determine which flags to use
    RSYNC_VERSION=$(rsync --version | head -1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
    RSYNC_MAJOR=$(echo "$RSYNC_VERSION" | cut -d. -f1)
    RSYNC_MINOR=$(echo "$RSYNC_VERSION" | cut -d. -f2)

    # Detect if running in a TTY (show progress) or non-TTY (run quietly)
    if [[ -t 1 ]]; then
        # TTY detected - show progress
        # --info=progress2 requires rsync 3.1.0+
        if [[ "$RSYNC_MAJOR" -gt 3 ]] || [[ "$RSYNC_MAJOR" -eq 3 && "$RSYNC_MINOR" -ge 1 ]]; then
            rsync -a --info=progress2 "$SOURCE_STRAIGHT/" "$TARGET_STRAIGHT/"
        else
            echo "NOTE: Using older rsync version $RSYNC_VERSION (install newer version with: brew install rsync)"
            rsync -a --progress "$SOURCE_STRAIGHT/" "$TARGET_STRAIGHT/"
        fi
    else
        # Non-TTY - run quietly to avoid overwhelming output
        rsync -a "$SOURCE_STRAIGHT/" "$TARGET_STRAIGHT/"
    fi
else
    # Fallback to cp if rsync not available
    echo "NOTE: rsync not found, using cp (install rsync with: brew install rsync)"
    cp -R "$SOURCE_STRAIGHT"/* "$TARGET_STRAIGHT/"
fi

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo ""
echo "✓ Copy complete!"

# Show detailed stats only in TTY mode
if [[ -t 1 ]]; then
    # Calculate target size
    TARGET_SIZE=$(du -sh "$TARGET_STRAIGHT" 2>/dev/null | cut -f1)
    echo "  Copied: $TARGET_SIZE"
    echo "  Time: ${ELAPSED}s"
    echo ""
    echo "Next steps:"
    echo "  1. Initialize submodules: cd $TARGET_WORKTREE && git submodule update --init"
    echo "  2. Launch Emacs: cd $TARGET_WORKTREE && ./bin/emacs-isolated.sh"
    echo ""
    echo "Note: Packages are now shared. To force rebuild, use:"
    echo "      ./bin/invalidate-runtime.sh --worktree $TARGET_WORKTREE"
else
    # Simpler output for non-TTY
    echo "  Time: ${ELAPSED}s"
    echo "  Target: $TARGET_WORKTREE/runtime/straight"
fi
