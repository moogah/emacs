#!/bin/bash
# Migrate existing gptel sessions to new simplified format
#
# What this script does:
# 1. Removes tools.org files (replaced by native tool persistence)
# 2. Updates preset.json to add include-tool-results: true
# 3. Preserves all conversation history and metadata
#
# Usage:
#   ./bin/migrate-sessions.sh [--sessions-dir DIR] [--dry-run] [--force]
#
# Options:
#   --sessions-dir DIR   Sessions directory (default: ~/.gptel-sessions)
#   --dry-run           Show what would be done without making changes
#   --force             Skip confirmation prompt

set -euo pipefail

# Default values
SESSIONS_DIR="${HOME}/.gptel-sessions"
DRY_RUN=false
FORCE=false

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --sessions-dir)
      SESSIONS_DIR="$2"
      shift 2
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --force)
      FORCE=true
      shift
      ;;
    --help)
      grep '^#' "$0" | sed 's/^# //' | sed 's/^#//'
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      echo "Use --help for usage information"
      exit 1
      ;;
  esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
info() {
  echo -e "${GREEN}✓${NC} $*"
}

warn() {
  echo -e "${YELLOW}!${NC} $*"
}

error() {
  echo -e "${RED}✗${NC} $*" >&2
}

# Check if jq is available for JSON manipulation
if ! command -v jq &> /dev/null; then
  error "jq is required for JSON manipulation"
  error "Install with: brew install jq"
  exit 1
fi

# Check if sessions directory exists
if [[ ! -d "$SESSIONS_DIR" ]]; then
  error "Sessions directory not found: $SESSIONS_DIR"
  exit 1
fi

# Count sessions to migrate
session_count=$(find "$SESSIONS_DIR" -mindepth 1 -maxdepth 1 -type d ! -name ".*" | wc -l | tr -d ' ')

if [[ "$session_count" -eq 0 ]]; then
  info "No sessions found in $SESSIONS_DIR"
  exit 0
fi

# Show what will be done
echo
echo "GPTEL Sessions Migration"
echo "========================"
echo
echo "Sessions directory: $SESSIONS_DIR"
echo "Sessions to migrate: $session_count"
echo
echo "Changes to be made:"
echo "  1. Remove tools.org files (obsolete)"
echo "  2. Update preset.json to enable native tool persistence"
echo
if [[ "$DRY_RUN" == true ]]; then
  warn "DRY RUN MODE - No changes will be made"
  echo
fi

# Confirmation prompt (unless --force)
if [[ "$FORCE" != true ]] && [[ "$DRY_RUN" != true ]]; then
  echo -n "Continue with migration? (y/N) "
  read -r response
  if [[ ! "$response" =~ ^[Yy]$ ]]; then
    echo "Migration cancelled"
    exit 0
  fi
  echo
fi

# Migration counters
migrated=0
tools_removed=0
presets_updated=0
skipped=0
errors=0

# Process each session
for session_dir in "$SESSIONS_DIR"/*; do
  if [[ ! -d "$session_dir" ]]; then
    continue
  fi

  session_name=$(basename "$session_dir")
  echo "Processing: $session_name"

  # Check if metadata.json exists (validates it's a session)
  if [[ ! -f "$session_dir/metadata.json" ]]; then
    warn "  Skipping: No metadata.json found"
    ((skipped++))
    continue
  fi

  # Remove tools.org if it exists
  if [[ -f "$session_dir/tools.org" ]]; then
    if [[ "$DRY_RUN" == true ]]; then
      info "  Would remove: tools.org"
    else
      rm -f "$session_dir/tools.org"
      info "  Removed: tools.org"
    fi
    ((tools_removed++))
  fi

  # Update preset.json if it exists
  if [[ -f "$session_dir/preset.json" ]]; then
    # Check if include-tool-results is already present
    has_include=$(jq 'has("include-tool-results")' "$session_dir/preset.json")

    if [[ "$has_include" == "true" ]]; then
      info "  Preset already has include-tool-results"
    else
      if [[ "$DRY_RUN" == true ]]; then
        info "  Would update: preset.json (add include-tool-results: true)"
      else
        # Add include-tool-results: true to preset.json
        jq '. + {"include-tool-results": true}' "$session_dir/preset.json" > "$session_dir/preset.json.tmp"
        mv "$session_dir/preset.json.tmp" "$session_dir/preset.json"
        info "  Updated: preset.json (added include-tool-results: true)"
      fi
      ((presets_updated++))
    fi
  else
    warn "  No preset.json found (session may be old format)"
  fi

  ((migrated++))
  echo
done

# Summary
echo "================================"
echo "Migration Summary"
echo "================================"
echo "Sessions processed: $migrated"
echo "Tools.org files removed: $tools_removed"
echo "Preset.json files updated: $presets_updated"
echo "Sessions skipped: $skipped"
if [[ "$errors" -gt 0 ]]; then
  echo "Errors encountered: $errors"
fi
echo

if [[ "$DRY_RUN" == true ]]; then
  warn "DRY RUN COMPLETE - No changes were made"
  echo "Run without --dry-run to apply changes"
else
  info "Migration complete!"
  echo
  echo "Next steps:"
  echo "  1. Restart Emacs to load updated session configuration"
  echo "  2. Resume a session to verify tool persistence works"
  echo "  3. Use VisibleAgent tool to create subagents in visible buffers"
fi

exit 0
