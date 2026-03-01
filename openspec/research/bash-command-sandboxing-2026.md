# Bash Command Sandboxing for LLM Agents: Research Report

**Date:** 2026-03-01
**Context:** gptel bash tools system - improving command validation beyond naive first-token checking
**Target Platform:** macOS (with consideration for Linux portability)

## Executive Summary

The current gptel bash tools implementation validates commands by extracting the first token (`split-string resource "[ |><;&]"`), which is insufficient for controlling command impact. This research examines proven approaches from production LLM agent systems and proposes emacs-native solutions leveraging existing directory-based scope control.

**Key Finding:** Rather than OS-level sandboxing (complex, maintenance burden), enhance command parsing with semantic argument analysis while maintaining the existing directory-based scope model. This provides practical security without introducing heavyweight dependencies.

## Current Implementation Analysis

### Strengths
- Directory-based read/write scope control (glob patterns)
- Category-based command classification (read_only, safe_write, dangerous)
- Git-aware workflow integration
- Structured error responses guiding LLM to request expansions
- No caching (filesystem as source of truth)

### Critical Weakness
```elisp
;; config/gptel/scope/scope-expansion.el:252
(cmd-name (car (split-string resource "[ |><;&]" t)))
```

**Problem:** Only checks the first token before pipes/redirects/separators.

**Impact:**
- `python --version` (safe) requires allowing `python` (unsafe)
- `git log` (safe) requires allowing `git` (but `git push` is dangerous)
- `rm -rf /` would be allowed if `rm` is in any category
- No validation of command arguments or target paths

## Industry Approaches: Three Models

### 1. OS-Level Sandboxing (Heavy Isolation)

**Technologies:**
- **Linux:** Bubblewrap (used by Claude Code, Flatpak)
- **macOS:** Seatbelt/sandbox-exec (deprecated but functional)
- **Cross-platform:** gVisor (userspace kernel, used by Google/Anthropic)

**How it works:**
- Creates mount namespaces with explicit filesystem bindings
- Process cannot access files not explicitly mounted
- Network traffic routed through proxies

**Example - Bubblewrap:**
```bash
bwrap \
  --ro-bind /usr /usr \
  --ro-bind /bin /bin \
  --bind ~/project ~/project \
  --tmpfs /tmp \
  --dev /dev \
  --proc /proc \
  --unshare-all \
  -- ls ~/project
```

**Example - macOS Seatbelt:**
```scheme
(version 1)
(deny default)
(allow file-read-data (regex "^/usr/lib"))
(allow file-read-data (regex "^/Users/jefffarr/emacs"))
(allow file-write* (regex "^/Users/jefffarr/emacs/state"))
(allow process-exec (literal "/usr/bin/git"))
```

**Sources:**
- [Claude Code Sandboxing](https://www.anthropic.com/engineering/claude-code-sandboxing)
- [Bubblewrap Guide](https://claudefa.st/blog/guide/sandboxing-guide)
- [macOS Sandbox Examples](https://github.com/s7ephen/OSX-Sandbox--Seatbelt--Profiles)

**Assessment for gptel:**
- ✅ Strongest isolation
- ❌ Complex setup and maintenance
- ❌ Seatbelt deprecated on macOS (though still functional)
- ❌ Requires external binary execution wrapper
- ❌ Breaks emacs integration (process buffers, comint modes)
- ❌ Difficult to debug for users

### 2. Container-Based Isolation (Medium Weight)

**Technologies:**
- Docker/Podman
- LLM-Sandbox (Python wrapper)
- Kubernetes for multi-agent systems

**How it works:**
- Run all agent commands inside containerized environment
- Mount specific directories as volumes
- Container destroyed after command completes

**Example:**
```bash
docker run --rm \
  -v ~/emacs:/workspace:ro \
  -v ~/emacs/state:/workspace/state:rw \
  alpine:latest \
  ls /workspace
```

**Sources:**
- [5 Code Sandboxes for AI Agents](https://www.kdnuggets.com/5-code-sandbox-for-your-ai-agents)
- [NVIDIA Agentic Security Guidance](https://developer.nvidia.com/blog/practical-security-guidance-for-sandboxing-agentic-workflows-and-managing-execution-risk/)

**Assessment for gptel:**
- ✅ Good isolation
- ✅ Reproducible environments
- ❌ Requires Docker/Podman installation
- ❌ 746MB+ package overhead (straight.el runtime)
- ❌ Slow startup time per command
- ❌ Complex volume mount configuration
- ❌ Breaks emacs process integration

### 3. Virtual Filesystem (Lightweight but Limited)

**Technologies:**
- just-bash (TypeScript/JavaScript bash implementation)
- WASM-based execution
- In-memory virtual filesystems

**How it works:**
- Implements bash shell in JavaScript/WASM
- All filesystem operations happen in memory
- No actual system access

**Example - just-bash:**
```javascript
import { bash } from 'just-bash';
const result = await bash`ls -la`;
// All operations happen in virtual FS
```

**Sources:**
- [A thousand ways to sandbox an agent](https://michaellivs.com/blog/sandbox-comparison-2026/)
- [Vercel just-bash](https://github.com/vercel/just-bash)

**Assessment for gptel:**
- ✅ Lightweight
- ✅ Perfect isolation (no system access)
- ❌ Not applicable to elisp/emacs environment
- ❌ Cannot access real project files
- ❌ Limited utility set (40 commands)
- ❌ No integration with system tools (git, grep, etc.)

## Recommended Approach: Enhanced Semantic Parsing

### Core Insight

Your existing directory-based scope control is **already excellent**. The problem isn't filesystem isolation—it's command validation. You need **semantic command analysis**, not OS-level sandboxing.

### Philosophy

**Control what directories can be affected, validate commands semantically within those boundaries.**

This aligns with your existing architecture:
- `paths.read`: glob patterns for read-allowed directories
- `paths.write`: glob patterns for write-allowed directories
- `paths.deny`: explicit deny patterns (precedence)
- Git-controlled directories: writes are safe (reversible)

### Proposed Enhancement: Command+Arguments Validation

Replace naive first-token extraction with **semantic argument analysis**:

```elisp
;; Instead of: (car (split-string resource "[ |><;&]" t))
;; Parse command + arguments + flags

(defun jf/gptel-scope--parse-command-semantics (command-string)
  "Parse command string into semantic components.
Returns plist with:
  :base-command - executable name
  :subcommand - subcommand if applicable (git log, npm install)
  :flags - list of flags
  :arguments - list of non-flag arguments
  :targets - list of file/directory targets
  :has-dangerous-flags - t if contains risky flags (--force, -rf)
  :pipe-commands - list of piped command names"
  ...)
```

### Strategy 1: Subcommand-Aware Whitelisting

**Problem:** `git` is too broad (git log ≠ git push)

**Solution:** Categorize by subcommand:

```yaml
bash_tools:
  categories:
    read_only:
      commands:
        - name: git
          subcommands: [log, show, diff, status, branch, ls-files]
          allowed_flags: [-p, --name-only, --stat, -1, -n]
        - name: ls
          allowed_flags: [-la, -lh, -R, -t]
        - name: grep
          allowed_flags: [-r, -n, -i, -E, -l]
          denied_flags: [--color=always]  # output pollution

    safe_write:
      commands:
        - name: git
          subcommands: [add, commit, checkout, branch, stash]
          allowed_flags: [-m, -b, -p, --patch]
          denied_flags: [--force, -f]
        - name: mkdir
          allowed_flags: [-p]
        - name: touch
          allowed_flags: []

    dangerous:
      commands:
        - name: git
          subcommands: [push, reset, clean]
          requires_confirmation: true
        - name: rm
          # Never allowed without explicit user approval
```

**Benefits:**
- Precise control: `git log` allowed, `git push` requires approval
- Flag validation: `rm -rf` blocked even if `rm` somehow allowed
- Clear semantics for LLM: error messages specify allowed subcommands

### Strategy 2: Argument Path Validation

**Problem:** Commands can target files outside directory scope via arguments

**Current workaround:** Warning about absolute paths (not enforced)

**Enhanced solution:**

```elisp
(defun jf/gptel-scope--validate-command-targets (parsed-command directory scope-config)
  "Validate that all file/directory targets in command are within scope.
PARSED-COMMAND is result of jf/gptel-scope--parse-command-semantics.
DIRECTORY is the command's working directory.
SCOPE-CONFIG is the scope configuration plist.

Returns:
  :allowed t/nil
  :violations list of paths outside scope"
  (let* ((targets (plist-get parsed-command :targets))
         (operation (plist-get parsed-command :operation-type)) ; :read or :write
         (violations '()))

    (dolist (target targets)
      (let ((abs-target (expand-file-name target directory)))
        ;; Check if target matches appropriate scope patterns
        (unless (jf/gptel-scope--path-in-scope-p abs-target operation scope-config)
          (push abs-target violations))))

    (if violations
        (list :allowed nil
              :reason "targets-outside-scope"
              :violations violations
              :message (format "Command targets files outside scope: %s"
                              (string-join violations ", ")))
      (list :allowed t))))
```

**Example scenarios:**

```bash
# Working directory: ~/emacs/config/gptel (paths.write)
# Command: "cp ~/.bashrc ."
# → DENIED: source ~/.bashrc not in paths.read

# Command: "cp ./scope.yml ./backup/"
# → ALLOWED: both in scope

# Command: "grep -r 'defun' /usr/lib"
# → DENIED: /usr/lib not in paths.read (even though grep is read_only)
```

### Strategy 3: Git-Aware Safety

**Insight:** Your CLAUDE.md mentions git-controlled directories should have special handling.

**Enhancement:**

```elisp
(defun jf/gptel-scope--directory-is-git-safe-p (directory)
  "Check if directory is in a git repo with clean revert path.
Returns t if:
  1. Directory is inside a git repository
  2. Git working tree is clean OR changes are already staged
  3. No untracked files would be affected by write operations

This means any writes can be safely reverted with git restore/reset."
  (let ((default-directory directory))
    (and (file-directory-p (expand-file-name ".git" (vc-root-dir)))
         ;; Could expand with more checks:
         ;; - uncommitted changes are ok (can revert)
         ;; - untracked files in target would need special handling
         t)))
```

**Policy enhancement:**

```yaml
paths:
  write:
    - "~/emacs/**"

git_safety:
  # Allow more write operations in git-controlled directories
  allow_destructive_in_git: true
  auto_stage_before_dangerous: true  # git add before rm/mv
  require_clean_worktree_for:
    - "git reset --hard"
    - "git clean -f"
```

**Benefits:**
- Leverages existing git worktree architecture
- Writes are safe because they're reversible
- Aligns with development workflow

### Strategy 4: Emacs-Native Advantages

**Leverage emacs process handling:**

```elisp
(defun jf/gptel-scope--execute-command-with-validation (command directory scope-config)
  "Execute command with emacs-native monitoring.
Uses `make-process` with:
  - Real-time output streaming to *gptel-command* buffer
  - Process sentinel for exit code
  - Timer for timeout (already implemented)
  - Ability to interrupt (process-send-signal)"

  (let* ((parsed (jf/gptel-scope--parse-command-semantics command))
         (validation (jf/gptel-scope--validate-enhanced parsed directory scope-config)))

    (if (not (plist-get validation :allowed))
        validation  ; Return error

      ;; Execute with monitoring
      (let ((proc (make-process
                   :name "gptel-bash"
                   :buffer (get-buffer-create "*gptel-command*")
                   :command (list shell-file-name "-c" command)
                   :connection-type 'pipe
                   :sentinel #'jf/gptel-scope--command-sentinel)))

        ;; User can view *gptel-command* buffer in real-time
        ;; Can interrupt with process-send-signal if needed
        ...))))
```

**Benefits:**
- No external dependencies
- Native emacs process control
- User can monitor/interrupt commands in real-time
- Integrates with existing emacs workflows (comint, compilation-mode)

## Implementation Roadmap

### Phase 1: Enhanced Parsing (Week 1)
1. Implement `jf/gptel-scope--parse-command-semantics`
   - Extract base command, subcommands, flags, arguments
   - Identify file/directory targets
   - Detect dangerous patterns (-rf, --force, recursive operations)

2. Unit tests for parsing
   - Complex git commands
   - Pipelines with multiple commands
   - Edge cases (quoted arguments, escaped characters)

### Phase 2: Subcommand Validation (Week 2)
1. Extend YAML schema for subcommands
2. Implement subcommand-aware validation
3. Update error messages with allowed subcommands
4. Add presets for common tools (git, npm, docker)

### Phase 3: Argument Path Validation (Week 2-3)
1. Implement target path extraction
2. Validate all targets against scope
3. Block commands with out-of-scope targets
4. Enhanced error messages showing violated paths

### Phase 4: Git-Aware Safety (Week 3-4)
1. Detect git-controlled directories
2. Implement reversibility checks
3. Optional auto-staging for dangerous operations
4. User prompts for git operations

### Phase 5: Documentation & Presets (Week 4)
1. Update openspec/specs/gptel/bash-tools.md
2. Create example scope.yml configurations
3. Preset configurations for common workflows
4. Migration guide for existing users

## Alternative: Hybrid Approach

If you want **defense in depth**, consider:

1. **Semantic validation (primary):** Enhanced parsing as described above
2. **macOS sandbox-exec (optional secondary):** For users who want maximum isolation

```elisp
(defcustom jf/gptel-scope-use-os-sandbox nil
  "When non-nil, execute bash commands inside macOS sandbox-exec.
Requires macOS and provides additional OS-level isolation.
May break some emacs integrations."
  :type 'boolean
  :group 'gptel)

(defun jf/gptel-scope--execute-with-optional-sandbox (command directory scope-config)
  (if (and jf/gptel-scope-use-os-sandbox (eq system-type 'darwin))
      (jf/gptel-scope--execute-via-seatbelt command directory scope-config)
    (jf/gptel-scope--execute-direct command directory scope-config)))
```

**Benefits:**
- Users can opt-in to OS-level sandboxing
- Doesn't break existing workflows by default
- Power users get additional protection

**Implementation:**
- Generate seatbelt profile from scope.yml
- Wrap command execution in sandbox-exec
- Handle sandbox violations gracefully

## Testing Strategy

### Unit Tests
```elisp
(ert-deftest jf/gptel-scope--parse-git-log ()
  (let ((result (jf/gptel-scope--parse-command-semantics "git log --oneline -10")))
    (should (equal (plist-get result :base-command) "git"))
    (should (equal (plist-get result :subcommand) "log"))
    (should (member "--oneline" (plist-get result :flags)))
    (should-not (plist-get result :has-dangerous-flags))))

(ert-deftest jf/gptel-scope--parse-dangerous-rm ()
  (let ((result (jf/gptel-scope--parse-command-semantics "rm -rf /tmp/test")))
    (should (plist-get result :has-dangerous-flags))
    (should (member "/tmp/test" (plist-get result :targets)))))
```

### Integration Tests
- Validate real-world command scenarios
- Test git-aware safety with actual repos
- Verify error messages guide LLM correctly
- Test scope expansion flow

### Characterization Tests
- Document current behavior before changes
- Ensure no regressions in existing functionality
- Use `/characterization-testing` skill

## Security Considerations

### Threat Model

**In scope:**
- Accidental destructive operations (rm -rf)
- Commands targeting wrong directories
- Overly broad tool permissions (python → arbitrary scripts)
- Data exfiltration to network (curl, wget if allowed)

**Out of scope (accept risk):**
- Malicious LLM explicitly trying to escape
- Kernel exploits
- Physical security
- Social engineering

### Residual Risks

Even with enhanced validation:
1. **Command injection via arguments:** Mitigated by shell escaping, not eliminated
2. **Logic bombs:** LLM could stage malicious code for later execution
3. **Indirect effects:** Allowed commands could have unintended side effects

**Mitigation:**
- User review before allowing new commands permanently
- Clear audit trail (log all commands)
- Session-scoped scope.yml (doesn't affect system globally)
- Git safety net (reversible operations)

## Comparison with Claude Code

**Claude Code approach:**
- Bubblewrap (Linux) / Seatbelt (macOS) for isolation
- Network traffic through proxies
- Reduced permission prompts by 84%

**Your approach (recommended):**
- Semantic command validation (no OS dependencies)
- Directory-based scope control (already implemented)
- Git-aware reversibility checks
- Emacs-native process handling

**Advantages of your approach:**
- Simpler to implement and maintain
- No external dependencies
- Better emacs integration
- More transparent to users
- Easier to debug and customize

**Trade-offs:**
- Less isolation (no separate mount namespace)
- Relies on command parsing (could miss edge cases)
- No network isolation

**Verdict:** For an emacs-native tool used by sophisticated users, your approach is more appropriate. Claude Code serves general developers who need foolproof isolation. Your users are emacs users who value transparency and hackability.

## References

### Primary Sources
- [Claude Code Sandboxing Engineering](https://www.anthropic.com/engineering/claude-code-sandboxing) - Anthropic's approach using OS-level primitives
- [A thousand ways to sandbox an agent](https://michaellivs.com/blog/sandbox-comparison-2026/) - Comprehensive comparison of sandboxing approaches
- [AI Agents Run Unsandboxed Code — How to Fix It](https://dev.to/tan_genie_6a51065da7b63b6/ai-agents-run-unsandboxed-code-how-to-fix-it-2026-1np4) - Industry overview

### Security Research
- [Claude Code Security Flaws (CVE-2025-59536, CVE-2026-21852)](https://research.checkpoint.com/2026/rce-and-api-token-exfiltration-through-claude-code-project-files-cve-2025-59536/) - Recent vulnerabilities in hooks/MCP
- [NVIDIA Agentic Workflows Security](https://developer.nvidia.com/blog/practical-security-guidance-for-sandboxing-agentic-workflows-and-managing-execution-risk/) - Defense in depth approach
- [OWASP ASI Top 10 for Agentic AI](https://www.kaspersky.com/blog/top-agentic-ai-risks-2026/55184/) - Security risks for AI agents

### Technical Implementation
- [Bubblewrap Documentation](https://github.com/containers/bubblewrap) - Linux sandboxing used by Claude Code
- [macOS Seatbelt Profiles](https://github.com/s7ephen/OSX-Sandbox--Seatbelt--Profiles) - Example seatbelt configurations
- [HN Discussion on macOS Sandbox-exec](https://news.ycombinator.com/item?id=44283454) - Community perspectives on deprecation

### Git-Based Isolation
- [Git Worktrees for AI Agents](https://nx.dev/blog/git-worktrees-ai-agents) - Using git worktrees for parallel agent workflows
- [Beyond Git Worktrees](https://medium.com/@llupRisingll/the-quest-for-true-development-environment-isolation-on-linux-71dffbf23aad) - Limitations and complementary approaches
- [Worktrunk](https://github.com/max-sixty/worktrunk) - Git worktree manager for AI agents

### Alternative Approaches
- [5 Code Sandboxes for AI Agents](https://www.kdnuggets.com/5-code-sandbox-for-your-ai-agents) - Virtual FS and container options
- [just-bash](https://github.com/vercel/just-bash) - JavaScript-based virtual bash implementation
- [LLM-Sandbox](https://medium.com/@sharathhebbar24/sandboxing-running-llm-generated-code-in-secure-environment-392869c32c06) - Container wrapper for LLM code execution

## Conclusion

**Recommendation:** Implement **Enhanced Semantic Parsing** (Strategies 1-4) rather than OS-level sandboxing.

**Rationale:**
1. **Aligns with existing architecture:** You already have excellent directory-based scope control
2. **Emacs-native:** No external dependencies, better integration
3. **Maintainable:** Pure elisp, easier to debug and extend
4. **Transparent:** Users understand what's happening
5. **Sufficient security:** For sophisticated emacs users with git safety net

**Next Steps:**
1. Review this research with stakeholders
2. Create OpenSpec change for enhanced validation
3. Implement Phase 1 (parsing) with tests
4. Iterate based on real-world usage

The goal is not perfect isolation (impossible without kernel-level sandboxing) but **practical safety that prevents accidents while enabling productivity**. Your existing scope system provides the foundation; enhanced command validation completes it.
