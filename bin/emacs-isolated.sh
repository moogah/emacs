#!/usr/bin/env bash
# emacs-isolated.sh - Launch isolated Emacs instance
#
# This script launches an isolated Emacs instance that:
# - Uses runtime/ for all packages, cache, and state (not ~/.emacs.d)
# - Doesn't interfere with system Emacs installations
# - Loads configuration from this repository's config/ directory
#
# Usage:
#   ./bin/emacs-isolated.sh [emacs arguments...]
#
# Examples:
#   ./bin/emacs-isolated.sh                    # Launch GUI Emacs
#   ./bin/emacs-isolated.sh file.txt          # Open file
#   ./bin/emacs-isolated.sh --batch --eval ... # Batch mode

set -euo pipefail

# Detect script directory and config root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RUNTIME_DIR="$CONFIG_ROOT/runtime"
EARLY_INIT="$CONFIG_ROOT/config/early-init.el"
INIT="$CONFIG_ROOT/config/init.el"

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
# "$@": Pass through all additional arguments
exec "$EMACS" -q \
    --load "$EARLY_INIT" \
    --load "$INIT" \
    "$@"
