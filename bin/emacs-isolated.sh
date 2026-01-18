#!/usr/bin/env bash
# emacs-isolated.sh - Launch isolated Emacs instance
#
# This script launches an isolated Emacs instance that:
# - Uses runtime/ for all packages, cache, and state (not ~/.emacs.d)
# - Doesn't interfere with system Emacs installations
# - Loads configuration from this repository (init files at root, modules in config/)
#
# Usage:
#   ./bin/emacs-isolated.sh [OPTIONS] [emacs arguments...]
#
# Options:
#   --init-runtime     Initialize runtime from main worktree if straight.el missing
#   --source PATH      Source worktree for --init-runtime (default: ~/emacs)
#
# Examples:
#   ./bin/emacs-isolated.sh                    # Launch GUI Emacs
#   ./bin/emacs-isolated.sh file.txt          # Open file
#   ./bin/emacs-isolated.sh --batch --eval ... # Batch mode
#   ./bin/emacs-isolated.sh --init-runtime     # Auto-init runtime if needed

set -euo pipefail

# Parse our options before passing to Emacs
INIT_RUNTIME=false
SOURCE_WORKTREE="$HOME/emacs"
EMACS_ARGS=()

while [[ $# -gt 0 ]]; do
    case $1 in
        --init-runtime)
            INIT_RUNTIME=true
            shift
            ;;
        --source)
            SOURCE_WORKTREE="$2"
            shift 2
            ;;
        *)
            # Not our option, pass to Emacs
            EMACS_ARGS+=("$1")
            shift
            ;;
    esac
done

# Detect script directory and config root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUNTIME_DIR="$CONFIG_ROOT/runtime"
EARLY_INIT="$CONFIG_ROOT/early-init.el"
INIT="$CONFIG_ROOT/init.el"

# Verify required directories and files exist
if [[ ! -d "$RUNTIME_DIR" ]]; then
    echo "ERROR: Runtime directory not found: $RUNTIME_DIR" >&2
    exit 1
fi

if [[ ! -f "$EARLY_INIT" ]]; then
    echo "ERROR: early-init.el not found: $EARLY_INIT" >&2
    exit 1
fi

if [[ ! -f "$INIT" ]]; then
    echo "ERROR: init.el not found: $INIT" >&2
    exit 1
fi

# Initialize runtime from source worktree if requested and needed
if [[ "$INIT_RUNTIME" == true ]]; then
    STRAIGHT_DIR="$RUNTIME_DIR/straight"
    if [[ ! -d "$STRAIGHT_DIR" ]]; then
        echo "Runtime not initialized. Copying from source worktree..."
        INIT_SCRIPT="$SCRIPT_DIR/init-worktree-runtime.sh"
        if [[ ! -f "$INIT_SCRIPT" ]]; then
            echo "ERROR: init-worktree-runtime.sh not found: $INIT_SCRIPT" >&2
            exit 1
        fi
        "$INIT_SCRIPT" "$CONFIG_ROOT" --source "$SOURCE_WORKTREE"
        echo ""
    else
        echo "Runtime already initialized, skipping copy."
        echo ""
    fi
fi

# Detect Emacs binary
if [[ -f "/Applications/Emacs.app/Contents/MacOS/Emacs" ]]; then
    EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
elif command -v emacs &> /dev/null; then
    EMACS=$(command -v emacs)
else
    echo "ERROR: Emacs not found. Install Emacs or add it to PATH" >&2
    exit 1
fi

# Set isolated user-emacs-directory
export EMACS_USER_DIRECTORY="$RUNTIME_DIR"

# Launch Emacs with isolated configuration
# -q: Skip loading default init file
# --load: Load our early-init.el and init.el
# "${EMACS_ARGS[@]+"${EMACS_ARGS[@]}"}": Pass through all additional arguments
if [[ ${#EMACS_ARGS[@]} -gt 0 ]]; then
    exec "$EMACS" -q \
        --load "$EARLY_INIT" \
        --load "$INIT" \
        "${EMACS_ARGS[@]}"
else
    exec "$EMACS" -q \
        --load "$EARLY_INIT" \
        --load "$INIT"
fi
