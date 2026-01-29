#!/usr/bin/env bash
# init-worktree-runtime.sh - Copy runtime artifacts from main worktree to new worktree
#
# This script copies the runtime/straight/ and runtime/tree-sitter/ directories from the
# main worktree (or specified source) to a target worktree. This avoids re-downloading and
# rebuilding 142 packages (~922MB) and recompiling tree-sitter grammars (~10MB) when
# creating new worktrees.
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
#   - runtime/tree-sitter/     (~10MB) - Compiled tree-sitter grammars
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
SOURCE_TREESITTER="$SOURCE_RUNTIME/tree-sitter"

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
TARGET_TREESITTER="$TARGET_RUNTIME/tree-sitter"

# Check if target already has straight.el installed
OVERWRITE_NEEDED=false
if [[ -d "$TARGET_STRAIGHT" ]]; then
    OVERWRITE_NEEDED=true
fi
if [[ -d "$TARGET_TREESITTER" ]]; then
    OVERWRITE_NEEDED=true
fi

if [[ "$OVERWRITE_NEEDED" = true ]]; then
    echo "WARNING: Target worktree already has runtime artifacts installed"
    [[ -d "$TARGET_STRAIGHT" ]] && echo "         - straight.el: $TARGET_STRAIGHT"
    [[ -d "$TARGET_TREESITTER" ]] && echo "         - tree-sitter: $TARGET_TREESITTER"
    echo ""
    read -p "Overwrite existing artifacts? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
    echo "Removing existing directories..."
    [[ -d "$TARGET_STRAIGHT" ]] && rm -rf "$TARGET_STRAIGHT"
    [[ -d "$TARGET_TREESITTER" ]] && rm -rf "$TARGET_TREESITTER"
fi

# Show what we're about to copy
echo "Copying runtime artifacts:"
echo "  straight.el:"
echo "    From: $SOURCE_STRAIGHT"
echo "    To:   $TARGET_STRAIGHT"
if [[ -d "$SOURCE_TREESITTER" ]]; then
    echo "  tree-sitter:"
    echo "    From: $SOURCE_TREESITTER"
    echo "    To:   $TARGET_TREESITTER"
fi
echo ""

# Calculate source size (skip in non-TTY to avoid pipe issues)
if [[ -t 1 ]]; then
    SOURCE_SIZE=$(du -sh "$SOURCE_STRAIGHT" 2>/dev/null | cut -f1)
    echo "Source sizes:"
    echo "  straight.el: $SOURCE_SIZE"
    if [[ -d "$SOURCE_TREESITTER" ]]; then
        TREESITTER_SIZE=$(du -sh "$SOURCE_TREESITTER" 2>/dev/null | cut -f1)
        echo "  tree-sitter: $TREESITTER_SIZE"
    fi
    echo ""
fi

# Copy straight.el directory
echo "Copying straight.el (this may take a minute)..."
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
echo "✓ straight.el copy complete! (${ELAPSED}s)"

# Copy tree-sitter grammars if they exist
if [[ -d "$SOURCE_TREESITTER" ]]; then
    echo ""
    echo "Copying tree-sitter grammars..."
    TS_START_TIME=$(date +%s)

    # Create target directory
    mkdir -p "$TARGET_TREESITTER"

    # Copy tree-sitter grammars (.dylib on macOS, .so on Linux, .dll on Windows)
    if command -v rsync &> /dev/null; then
        if [[ -t 1 ]]; then
            rsync -a --progress "$SOURCE_TREESITTER/" "$TARGET_TREESITTER/"
        else
            rsync -a "$SOURCE_TREESITTER/" "$TARGET_TREESITTER/"
        fi
    else
        cp -R "$SOURCE_TREESITTER"/* "$TARGET_TREESITTER/"
    fi

    TS_END_TIME=$(date +%s)
    TS_ELAPSED=$((TS_END_TIME - TS_START_TIME))

    echo ""
    echo "✓ tree-sitter copy complete! (${TS_ELAPSED}s)"
else
    echo ""
    echo "NOTE: No tree-sitter grammars found in source worktree"
    echo "      Grammars will be installed on first Emacs launch"
fi

echo ""
echo "✓ All runtime artifacts copied!"

# Show detailed stats only in TTY mode
if [[ -t 1 ]]; then
    # Calculate target sizes
    echo ""
    echo "Summary:"
    TARGET_STRAIGHT_SIZE=$(du -sh "$TARGET_STRAIGHT" 2>/dev/null | cut -f1)
    echo "  straight.el: $TARGET_STRAIGHT_SIZE"
    if [[ -d "$TARGET_TREESITTER" ]]; then
        TARGET_TS_SIZE=$(du -sh "$TARGET_TREESITTER" 2>/dev/null | cut -f1)
        echo "  tree-sitter: $TARGET_TS_SIZE"
    fi
    TOTAL_ELAPSED=$((END_TIME - START_TIME))
    if [[ -d "$SOURCE_TREESITTER" ]]; then
        TOTAL_ELAPSED=$((TS_END_TIME - START_TIME))
    fi
    echo "  Total time: ${TOTAL_ELAPSED}s"
    echo ""
    echo "Next steps:"
    echo "  1. Initialize submodules: cd $TARGET_WORKTREE && git submodule update --init"
    echo "  2. Launch Emacs: cd $TARGET_WORKTREE && ./bin/emacs-isolated.sh"
    echo ""
    echo "Note: Runtime artifacts are now shared. To force rebuild, use:"
    echo "      ./bin/invalidate-runtime.sh --worktree $TARGET_WORKTREE"
else
    # Simpler output for non-TTY
    TOTAL_ELAPSED=$((END_TIME - START_TIME))
    if [[ -d "$SOURCE_TREESITTER" ]]; then
        TOTAL_ELAPSED=$((TS_END_TIME - START_TIME))
    fi
    echo "  Total time: ${TOTAL_ELAPSED}s"
    echo "  Target: $TARGET_WORKTREE/runtime/"
fi
