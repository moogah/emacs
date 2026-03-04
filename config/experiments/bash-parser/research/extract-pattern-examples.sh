#!/usr/bin/env bash
#
# extract-pattern-examples.sh
#
# Extract pattern-specific examples from coverage-analysis.tsv for targeted analysis.
# Creates pattern-extracts/ subdirectory with separate TSV files for each pattern type.
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TSV_FILE="${SCRIPT_DIR}/coverage-analysis.tsv"
OUTPUT_DIR="${SCRIPT_DIR}/pattern-extracts"

# Check prerequisites
if [[ ! -f "$TSV_FILE" ]]; then
    echo "ERROR: Coverage analysis file not found: $TSV_FILE"
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Extract header for reuse
HEADER=$(head -1 "$TSV_FILE")

echo "Extracting pattern-specific examples from coverage-analysis.tsv..."
echo

# Function to create TSV with header, data, and stats comment
create_extract() {
    local pattern="$1"
    local output_file="$2"
    local description="$3"
    local grep_pattern="$4"
    local sort_col="${5:-3}"  # Default to coverage_pct (column 3)

    echo "Processing: $description"

    # Extract matching rows (skip header), sort by coverage ascending
    local matches=$(grep -E "$grep_pattern" "$TSV_FILE" | grep -v "^command" | sort -t$'\t' -k${sort_col}n)
    local count=$(echo "$matches" | grep -S "." | wc -l | tr -d ' ')

    if [[ $count -eq 0 ]]; then
        echo "  No matches found for $pattern"
        return
    fi

    # Calculate stats
    local min_coverage=$(echo "$matches" | head -1 | cut -f3)
    local max_coverage=$(echo "$matches" | tail -1 | cut -f3)
    local avg_coverage=$(echo "$matches" | awk -F'\t' '{sum+=$3; count++} END {printf "%.1f", sum/count}')

    # Write output file
    {
        echo "# Pattern: $pattern"
        echo "# Description: $description"
        echo "# Total commands: $count"
        echo "# Coverage range: ${min_coverage}% - ${max_coverage}%"
        echo "# Average coverage: ${avg_coverage}%"
        echo "#"
        echo "# Commands sorted by coverage_pct (ascending) to prioritize gaps"
        echo "#"
        echo "$HEADER"
        echo "$matches"
    } > "$output_file"

    echo "  ✓ Extracted $count commands to $(basename "$output_file")"
    echo "    Coverage: ${min_coverage}% - ${max_coverage}% (avg: ${avg_coverage}%)"
}

# Extract heredoc patterns (exclude repetitive git commit patterns)
create_extract \
    "heredoc" \
    "$OUTPUT_DIR/heredoc-examples.tsv" \
    "Heredoc patterns" \
    $'\theredoc$|\theredoc,|,heredoc'

# Count git commit heredocs before filtering
heredoc_total=$(grep -E $'\theredoc$|\theredoc,|,heredoc' "$TSV_FILE" | grep -v "^command" | wc -l | tr -d ' ')
git_commit_count=$(grep -E $'\theredoc$|\theredoc,|,heredoc' "$TSV_FILE" | grep -v "^command" | grep -c "git commit -m.*<<'EOF'" || true)

# Filter out repetitive git commit patterns from heredoc file
if [[ -f "$OUTPUT_DIR/heredoc-examples.tsv" ]]; then
    # Keep header and stats, filter data
    {
        head -9 "$OUTPUT_DIR/heredoc-examples.tsv" | sed "s/Total commands: .*/Total commands: $heredoc_total (filtered: $((heredoc_total - git_commit_count)) shown, $git_commit_count git commits excluded)/"
        grep -v "^#" "$OUTPUT_DIR/heredoc-examples.tsv" | grep -v "^command" | grep -v "git commit -m.*<<'EOF'" || true
    } > "$OUTPUT_DIR/heredoc-examples.tmp"
    mv "$OUTPUT_DIR/heredoc-examples.tmp" "$OUTPUT_DIR/heredoc-examples.tsv"
    echo "  ℹ Filtered $git_commit_count repetitive git commit patterns"
fi

# Extract for-loop patterns
create_extract \
    "for-loop" \
    "$OUTPUT_DIR/for-loop-examples.tsv" \
    "For-loop patterns" \
    $'\tfor-loop$|\tfor-loop,|,for-loop'

# Extract conditional patterns
create_extract \
    "conditional" \
    "$OUTPUT_DIR/conditional-examples.tsv" \
    "Conditional (if/test) patterns" \
    $'\tconditional$|\tconditional,|,conditional'

# Extract process-substitution patterns
create_extract \
    "process-substitution" \
    "$OUTPUT_DIR/process-substitution-examples.tsv" \
    "Process substitution patterns" \
    $'\tprocess-substitution$|\tprocess-substitution,|,process-substitution'

# Extract command-substitution patterns
create_extract \
    "command-substitution" \
    "$OUTPUT_DIR/command-substitution-examples.tsv" \
    "Command substitution patterns" \
    $'\tcommand-substitution$|\tcommand-substitution,|,command-substitution'

# Extract combined patterns (semantic_gap_count >= 2)
echo "Processing: Combined patterns (multiple gap types)"
combined_matches=$(awk -F'\t' '$9 >= 2' "$TSV_FILE" | grep -v "^command" | sort -t$'\t' -k3n)
combined_count=$(echo "$combined_matches" | grep -S "." | wc -l | tr -d ' ')

if [[ $combined_count -gt 0 ]]; then
    min_coverage=$(echo "$combined_matches" | head -1 | cut -f3)
    max_coverage=$(echo "$combined_matches" | tail -1 | cut -f3)
    avg_coverage=$(echo "$combined_matches" | awk -F'\t' '{sum+=$3; count++} END {printf "%.1f", sum/count}')
    avg_gaps=$(echo "$combined_matches" | awk -F'\t' '{sum+=$9; count++} END {printf "%.1f", sum/count}')

    {
        echo "# Pattern: combined"
        echo "# Description: Commands with 2+ semantic gap types (complex patterns)"
        echo "# Total commands: $combined_count"
        echo "# Coverage range: ${min_coverage}% - ${max_coverage}%"
        echo "# Average coverage: ${avg_coverage}%"
        echo "# Average gap count: ${avg_gaps}"
        echo "#"
        echo "# Commands sorted by coverage_pct (ascending) to prioritize gaps"
        echo "#"
        echo "$HEADER"
        echo "$combined_matches"
    } > "$OUTPUT_DIR/combined-patterns.tsv"

    echo "  ✓ Extracted $combined_count commands to combined-patterns.tsv"
    echo "    Coverage: ${min_coverage}% - ${max_coverage}% (avg: ${avg_coverage}%)"
    echo "    Average gaps: ${avg_gaps}"
fi

# Extract critical gaps (critical_gaps > 0)
echo "Processing: Critical gaps"
critical_matches=$(awk -F'\t' '$10 > 0' "$TSV_FILE" | grep -v "^command" | sort -t$'\t' -k3n)
critical_count=$(echo "$critical_matches" | grep -S "." | wc -l | tr -d ' ')

if [[ $critical_count -gt 0 ]]; then
    min_coverage=$(echo "$critical_matches" | head -1 | cut -f3)
    max_coverage=$(echo "$critical_matches" | tail -1 | cut -f3)
    avg_coverage=$(echo "$critical_matches" | awk -F'\t' '{sum+=$3; count++} END {printf "%.1f", sum/count}')

    {
        echo "# Pattern: critical"
        echo "# Description: Commands with critical semantic gaps (high-impact failures)"
        echo "# Total commands: $critical_count"
        echo "# Coverage range: ${min_coverage}% - ${max_coverage}%"
        echo "# Average coverage: ${avg_coverage}%"
        echo "#"
        echo "# Commands sorted by coverage_pct (ascending) to prioritize gaps"
        echo "#"
        echo "$HEADER"
        echo "$critical_matches"
    } > "$OUTPUT_DIR/critical-gaps.tsv"

    echo "  ✓ Extracted $critical_count commands to critical-gaps.tsv"
    echo "    Coverage: ${min_coverage}% - ${max_coverage}% (avg: ${avg_coverage}%)"
fi

echo
echo "Extraction complete. Results in: $OUTPUT_DIR/"
echo
echo "Summary by pattern:"
ls -lh "$OUTPUT_DIR"/*.tsv | awk '{printf "  %s (%s)\n", $9, $5}'
