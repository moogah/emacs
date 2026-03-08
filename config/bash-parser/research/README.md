# Bash Parser Research Infrastructure

## Purpose

Research infrastructure to evaluate bash parser completeness using real-world LLM agent commands.

## Three-Stage Analysis Pipeline

### Stage 1: Command Extraction
- **Tool:** `extract-bash-commands.py`
- **Input:** Claude Code session histories (`~/.claude/projects/-Users-jefffarr-emacs/*.jsonl`)
- **Output:** `bash-commands-from-sessions.jsonl` (JSONL database)
- **Format:** Each line contains: session, timestamp, gitBranch, command, description

### Stage 2: Parse Analysis
- **Tools:** `analyze-with-coverage.el` + `run-coverage-analysis.sh`
- **Input:** `bash-commands-from-sessions.jsonl`
- **Output:** `coverage-analysis.tsv` (TSV with metrics)
- **Columns:** command, parse_status, coverage_pct, missing_count, file_ops_count, command_type, dangerous, missing_tokens, semantic_gap_count, critical_gaps, gap_types

### Stage 3: Gap Evaluation
Two complementary analyses:

**Token-based Coverage** (`compute-parse-coverage.el`):
- Extracts tokens from tree-sitter AST (input)
- Extracts tokens from parser output (structured result)
- Computes coverage percentage (how much text was captured)
- Identifies missing tokens

**Semantic Gap Detection** (`semantic-gap-detection.el`):
- Detects semantic structures in AST (command substitution, loops, conditionals, etc.)
- Verifies parser output contains corresponding semantic fields
- Reports gaps with severity (critical/medium/low) and security impact
- Example: `echo $(whoami)` may have 100% text coverage but critical semantic gap if nested command not extracted

## Usage

### Extract Commands
```bash
./config/experiments/bash-parser/research/extract-bash-commands.py
```
Re-scans all session files and regenerates corpus.

### Analyze Corpus
```bash
./config/experiments/bash-parser/research/run-coverage-analysis.sh
```
Runs both coverage and semantic gap detection on all commands, outputs TSV.

### Query Results
```bash
# Commands with semantic gaps
awk -F'\t' '$9 > 0' coverage-analysis.tsv

# Commands with critical gaps
awk -F'\t' '$10 > 0' coverage-analysis.tsv

# Commands with perfect coverage but semantic gaps
awk -F'\t' '$3 == 100.0 && $9 > 0' coverage-analysis.tsv
```

## Testing

Run semantic gap detection tests:
```bash
emacs -Q --batch -l test-semantic-gaps.el
```

## Files

- `extract-bash-commands.py` - Command extraction from session histories
- `bash-commands-from-sessions.jsonl` - Corpus database (1,552 commands)
- `compute-parse-coverage.el` - Token-based coverage computation
- `semantic-gap-detection.el` - Semantic structure gap detection
- `analyze-with-coverage.el` - Integrated analysis (both layers)
- `run-coverage-analysis.sh` - Analysis runner
- `coverage-analysis.tsv` - Current analysis results
- `test-semantic-gaps.el` - Test utilities
