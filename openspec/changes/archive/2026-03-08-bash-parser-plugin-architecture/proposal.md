## Why

The bash parser currently extracts only filesystem operations, but commands operate across multiple domains (cloud services, databases, network, containers). Security analysis and AI context require understanding the full scope of impact across all domains. A plugin-based architecture enables extensible semantic extraction with coverage metrics to identify blindspots.

## What Changes

- **BREAKING**: Replace `jf/bash-extract-file-operations` with unified `jf/bash-extract-semantics` API
- Add token inventory to parsed command structures for granular coverage tracking
- Add parse completeness flag (`:parse-complete`) to indicate successful syntax parsing
- Implement plugin registry system for domain-specific semantic extractors
- Refactor filesystem extraction as the first plugin implementation
- Add coverage calculation system to report what percentage of tokens have semantic meaning
- Provide visibility into uninterpreted tokens (blindspots)

## Capabilities

### New Capabilities
- `bash-parser-plugins`: Plugin registry, orchestration, and protocol for domain-specific semantic extraction
- `bash-parser-coverage`: Coverage calculation and reporting for semantic analysis
- `bash-parser-tokens`: Token inventory system for granular parse element tracking
- `cloud-auth-extraction`: Extract cloud authentication scope (AWS, GCP, Azure accounts/profiles)

### Modified Capabilities
- `bash-parser`: Add token inventory and parse completeness tracking to parser output
- `bash-file-operations`: Refactor as plugin, add token claiming for coverage tracking

## Impact

**Code Changes:**
- `config/experiments/bash-parser/bash-parser-core.org` - Add token tracking
- `config/experiments/bash-parser/bash-parser-file-ops.org` - Refactor as plugin
- New modules: `bash-parser-plugins.org`, `bash-parser-coverage.org`, `bash-parser-cloud-auth.org`

**API Changes:**
- **BREAKING**: `jf/bash-extract-file-operations` API replaced with `jf/bash-extract-semantics`
- New plugin registration API: `jf/bash-register-plugin`
- New struct: `jf/bash-plugin-result` for plugin return values

**Dependencies:**
- gptel scope system integration needs update to use new semantic extraction API
- Any other consumers of `jf/bash-extract-file-operations` need migration

**Performance:**
- Parser now tracks additional token metadata (minimal overhead)
- Multiple plugins run on each command (linear with plugin count, expected 2-5 plugins initially)
