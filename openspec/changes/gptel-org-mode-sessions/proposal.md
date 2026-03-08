## Why

Session files are currently stored as `session.md` in markdown format, requiring custom conversion logic to integrate with org-roam and missing the rich structural features of org-mode. The upstream gptel package has mature, well-tested support for org-mode buffers with automatic markdown-to-org conversion (`gptel-org-convert-response`, enabled by default), allowing LLMs to output natural markdown while the package handles conversion transparently. Transitioning to org-mode as the primary format aligns with gptel's design intent, simplifies the codebase by removing redundant conversion logic, and enables better integration with org-roam and org-mode features (folding, properties, agenda) while maintaining backward compatibility.

## What Changes

- **Session file format**: Change from `session.md` to `session.org` as the primary format for new sessions
- **Automatic conversion**: Leverage gptel's built-in `gptel-org-convert-response` (enabled by default) to convert LLM markdown responses to org-mode format
- **File detection**: Update auto-initialization hooks to detect and initialize both `.md` and `.org` session files
- **Local Variables**: Migrate from markdown HTML comments (`<!-- gptel-model: ... -->`) to Emacs Local Variables format (`# gptel-model: ...`) which works in both markdown and org-mode
- **Initial content**: Change session initialization from markdown heading (`###\n`) to org-mode heading (`* Session\n`)
- **Org-roam simplification**: Remove redundant `jf/markdown-to-org` conversion in `create_roam_node` tool (content is already org-mode)
- **Activities integration**: Update session creation to use org-mode format with org-appropriate initial content
- **Backward compatibility**: Maintain support for existing markdown sessions (dual-format support)

## Capabilities

### New Capabilities

- `org-mode-sessions`: Support for org-mode as the primary session file format with automatic markdown-to-org conversion via gptel's built-in converter

### Modified Capabilities

- `sessions-persistence`: File format changes from `session.md` to `session.org`, initial content format changes, and Local Variables format migration
- `sessions-branching`: Local Variables extraction/writing must support both markdown HTML comments and Emacs Local Variables format during transition
- `preset-registration`: Optional mode configuration per preset to support format-specific session creation

## Impact

**Code Changes:**
- `config/gptel/sessions/constants.org`: Change `jf/gptel-session--context-file` from `"session.md"` to `"session.org"`
- `config/gptel/sessions/commands.org`: Update auto-init hooks to detect `.org` files (lines 249, 260, 269)
- `config/gptel/sessions/branching.org`: Rewrite Local Variables handling (lines 176-229) to support Emacs Local Variables format
- `config/gptel/sessions/activities-integration.org`: Update initial content and worktree storage format
- `config/gptel/tools/org-roam-tools.org`: Remove redundant `jf/markdown-to-org` conversion from `create_roam_node`
- `config/gptel/tools/persistent-agent.org`: Remove hardcoded `(markdown-mode)` call, let Emacs detect mode from file extension

**Dependencies:**
- Relies on upstream gptel's `gptel-org-convert-response` feature (present and enabled by default)
- No new external dependencies (pandoc no longer needed for session conversion)

**User Impact:**
- New sessions created in org-mode format
- Existing markdown sessions continue to work (backward compatible)
- Improved org-mode features: folding, properties, agenda integration
- Simplified org-roam integration (no conversion needed)

**Migration Path:**
- No forced migration of existing sessions
- Users can manually rename `.md` → `.org` if desired
- Both formats supported indefinitely
