#!/usr/bin/env bash
# get-machine-role.sh - Get stable machine role from ~/.machine-role
#
# This script reads the machine role from ~/.machine-role file.
# The machine role is used throughout the dotfiles for machine-specific configuration.
#
# Available roles:
#   - apploi-mac: Work MacBook Air
#   - personal-mac: Personal MacBook Pro
#   - personal-mac-air: Personal MacBook Air
#
# Setup:
#   echo "apploi-mac" > ~/.machine-role

set -euo pipefail

MACHINE_ROLE_FILE="$HOME/.machine-role"

if [[ ! -f "$MACHINE_ROLE_FILE" ]]; then
    cat >&2 <<EOF
ERROR: Machine role file not found: $MACHINE_ROLE_FILE

To set up your machine role, create the file with one of the following values:
  - apploi-mac (Work MacBook Air)
  - personal-mac (Personal MacBook Pro)
  - personal-mac-air (Personal MacBook Air)

Example:
  echo "apploi-mac" > ~/.machine-role

This file ensures stable machine role identification even when hostname changes.
EOF
    exit 1
fi

# Read the machine role, removing any trailing whitespace
MACHINE_ROLE=$(cat "$MACHINE_ROLE_FILE" | tr -d '[:space:]')

if [[ -z "$MACHINE_ROLE" ]]; then
    echo "ERROR: Machine role file is empty: $MACHINE_ROLE_FILE" >&2
    exit 1
fi

echo "$MACHINE_ROLE"
