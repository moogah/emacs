# Migration Guide: scope.yml v3 → v4

## Overview

Schema v4 introduces **breaking changes** to scope validation with operation-specific path scoping, cloud authentication detection, and semantic validation using bash-parser. **No backward compatibility** - all scope.yml files must be manually migrated.

## Breaking Changes

### 1. New Path Sections Required

**v3 behavior**: Single `paths.read` and `paths.write` sections.

**v4 requirement**: Add `paths.execute` and `paths.modify` for script execution and in-place file editing.

**Migration impact**: Commands requiring execute or modify operations will be denied until paths added.

### 2. Pipeline Validation Enforced

**v3 behavior**: Only first command in pipeline validated. Example: `ls | xargs rm` allowed (only `ls` checked).

**v4 behavior**: ALL pipeline commands validated. Example: `ls | xargs rm` now rejects `rm` if in deny list.

**Migration impact**: Pipeline commands previously bypassing validation will now be caught.

### 3. File Path Validation Active

**v3 behavior**: Directory validation only. Command arguments not checked.

**v4 behavior**: File paths extracted from command arguments and validated against scope patterns.

**Migration impact**: Commands like `cat /etc/passwd` in scoped `/workspace` directory now rejected.

### 4. Cloud Authentication Detection

**v3 behavior**: Cloud commands treated like any other command.

**v4 behavior**: Cloud auth commands (aws-vault, gcloud, az) detected and policy enforced.

**Migration impact**: Cloud commands may require `cloud` section or will be denied/warned based on policy.

## Migration Examples

### Example 1: Basic Migration (Add Execute/Modify Paths)

**v3 schema:**
```yaml
paths:
  read:
    - "/workspace/**"
  write:
    - "/workspace/**"
  deny:
    - "/etc/**"

bash_tools:
  categories:
    read_only: ["ls", "cat", "grep"]
    safe_write: ["mkdir", "touch"]
    dangerous: []
  deny: ["rm", "sudo"]
```

**v4 schema:**
```yaml
paths:
  read:
    - "/workspace/**"
  write:
    - "/workspace/**"
  execute:                      # NEW: Add for script execution
    - "/workspace/scripts/**"
    - "/workspace/bin/**"
  modify:                       # NEW: Add for in-place edits
    - "/workspace/config/**"
  deny:
    - "/etc/**"

bash_tools:
  categories:
    read_only: ["ls", "cat", "grep"]
    safe_write: ["mkdir", "touch"]
    dangerous: []
  deny: ["rm", "sudo"]

cloud:                          # NEW: Add cloud policy
  auth_detection: "warn"        # "allow", "warn", or "deny"
  allowed_providers: []         # Empty = all providers allowed

security:                       # NEW: Add security settings
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
```

**What changed:**
- Added `paths.execute` for scripts in `/workspace/scripts/` and `/workspace/bin/`
- Added `paths.modify` for config files in `/workspace/config/`
- Added `cloud` section with default "warn" mode
- Added `security` section with strict parse enforcement

**Why:** Without execute/modify paths, commands like `bash /workspace/scripts/deploy.sh` or `sed -i 's/foo/bar/' /workspace/config/app.conf` will be denied.

### Example 2: Cloud-Aware Migration

**v3 schema:**
```yaml
paths:
  read:
    - "/workspace/**"
    - "~/.aws/**"               # AWS config readable
  write:
    - "/workspace/**"

bash_tools:
  categories:
    read_only: ["ls", "cat", "aws"]
    safe_write: ["mkdir"]
    dangerous: []
  deny: []
```

**v4 schema:**
```yaml
paths:
  read:
    - "/workspace/**"
    - "~/.aws/**"
  write:
    - "/workspace/**"
  execute:
    - "/workspace/scripts/**"
  modify:
    - "/workspace/config/**"
  deny: []

bash_tools:
  categories:
    read_only: ["ls", "cat", "aws"]
    safe_write: ["mkdir"]
    dangerous: []
  deny: []

cloud:
  auth_detection: "warn"        # Warn on cloud auth commands
  allowed_providers:
    - aws                       # Only allow AWS commands

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
```

**What changed:**
- Added `cloud.allowed_providers: ["aws"]` to permit only AWS commands
- Set `auth_detection: "warn"` to get warnings but allow execution

**Why:** Without `allowed_providers`, all cloud commands denied by default. With `["aws"]`, only AWS permitted.

**Alternative:** Set `auth_detection: "allow"` to permit all cloud providers without warnings.

### Example 3: Pipeline Command Migration

**v3 behavior (bypassed):**
```yaml
paths:
  read: ["/workspace/**"]
  write: ["/workspace/**"]

bash_tools:
  categories:
    read_only: ["ls", "find", "xargs"]
    safe_write: []
    dangerous: ["rm"]
  deny: ["rm"]
```

**Command:** `find /workspace -name '*.tmp' | xargs rm`
**v3 result:** ALLOWED (only `find` validated, `rm` bypassed)

**v4 migration:**
```yaml
paths:
  read: ["/workspace/**"]
  write: ["/workspace/**"]
  execute: []
  modify: []
  deny: []

bash_tools:
  categories:
    read_only: ["ls", "find", "xargs"]
    safe_write: []
    dangerous: ["rm"]
  deny: ["rm"]

cloud:
  auth_detection: "warn"
  allowed_providers: []

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
```

**Command:** `find /workspace -name '*.tmp' | xargs rm`
**v4 result:** DENIED (both `find` and `rm` validated, `rm` in deny list)

**To allow this pattern:**
1. Remove `rm` from deny list (UNSAFE - only if truly needed)
2. Change command to non-piped version: `find /workspace -name '*.tmp' -delete`
3. Request scope expansion for specific cleanup script

**Recommended:** Use safer alternatives instead of allowing `rm` in pipelines.

### Example 4: File Path Scoping

**v3 behavior (directory-only validation):**
```yaml
paths:
  read: ["/workspace/**"]
  write: []
  deny: ["/etc/**"]

bash_tools:
  categories:
    read_only: ["cat", "grep"]
    safe_write: []
    dangerous: []
  deny: []
```

**Command:** `cat /etc/passwd` (in `/workspace` directory)
**v3 result:** ALLOWED (directory `/workspace` in read scope)

**v4 migration:**
```yaml
paths:
  read: ["/workspace/**"]
  write: []
  execute: []
  modify: []
  deny: ["/etc/**"]

bash_tools:
  categories:
    read_only: ["cat", "grep"]
    safe_write: []
    dangerous: []
  deny: []

cloud:
  auth_detection: "warn"
  allowed_providers: []

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
```

**Command:** `cat /etc/passwd` (in `/workspace` directory)
**v4 result:** DENIED (file path `/etc/passwd` extracted and validated, matches paths.deny)

**What changed:**
- File paths in command arguments now extracted and validated
- Absolute paths checked against scope patterns regardless of working directory

**To allow reading specific system files:**
```yaml
paths:
  read:
    - "/workspace/**"
    - "/etc/os-release"    # Specific file
    - "/proc/**"           # System info
```

### Example 5: Permissive Parse Mode

**v4 strict mode (default):**
```yaml
security:
  enforce_parse_complete: true   # Reject unparseable commands
  max_coverage_threshold: 0.8
```

**Command:** `complex-script-with-edge-case-syntax.sh`
**Result:** DENIED with `incomplete_parse` error if bash-parser cannot fully parse

**v4 permissive mode:**
```yaml
security:
  enforce_parse_complete: false  # Allow unparseable with warning
  max_coverage_threshold: 0.5    # Lower coverage threshold
```

**Command:** `complex-script-with-edge-case-syntax.sh`
**Result:** ALLOWED with warning about incomplete parse

**Use permissive mode when:**
- Environment has complex bash commands bash-parser cannot fully parse
- Validation failures due to edge-case syntax, not security issues
- Willing to accept reduced validation coverage for compatibility

**Warning:** Permissive mode reduces security guarantees. Prefer fixing commands to be parseable.

## Migration Checklist

1. **Add new path sections:**
   - [ ] Add `paths.execute` with script directories
   - [ ] Add `paths.modify` with editable file patterns
   - [ ] Review `paths.deny` for absolute path patterns

2. **Add cloud section:**
   - [ ] Set `cloud.auth_detection` ("allow", "warn", or "deny")
   - [ ] Set `cloud.allowed_providers` if using cloud commands

3. **Add security section:**
   - [ ] Set `security.enforce_parse_complete` (true = strict, false = permissive)
   - [ ] Set `security.max_coverage_threshold` (0.8 recommended)

4. **Review pipeline commands:**
   - [ ] Test pipelines that previously worked - may now be denied
   - [ ] Update bash_tools.categories to include all pipeline commands
   - [ ] Remove dangerous commands from allow lists

5. **Review file path access:**
   - [ ] Test commands with absolute paths - may now be denied
   - [ ] Add specific system paths to `paths.read` if needed
   - [ ] Ensure scripts can access required files

6. **Test configuration:**
   - [ ] Run test commands to verify validation behavior
   - [ ] Check error messages for helpful guidance
   - [ ] Verify scope expansion requests work

## Common Migration Errors

### Error: "path_out_of_scope" for execute operation

**Symptom:**
```elisp
{:error "path_out_of_scope"
 :path "/workspace/scripts/deploy.sh"
 :operation :execute
 :required-scope "paths.execute"}
```

**Cause:** Missing `paths.execute` section.

**Fix:** Add execute paths:
```yaml
paths:
  execute:
    - "/workspace/scripts/**"
```

### Error: "command_denied" for pipeline command

**Symptom:**
```elisp
{:error "command_denied"
 :command "rm"
 :pipeline-position 1
 :full-command "ls | xargs rm"}
```

**Cause:** Pipeline validation now active, `rm` in deny list.

**Fix:** Either remove `rm` from deny list (UNSAFE) or rewrite command without pipeline:
```bash
# Instead of: ls | xargs rm
# Use: find . -name 'pattern' -delete
```

### Error: "incomplete_parse" for complex command

**Symptom:**
```elisp
{:error "incomplete_parse"
 :message "Cannot validate command: parse incomplete"}
```

**Cause:** Bash-parser cannot fully parse command syntax.

**Fix:** Simplify command OR set `security.enforce_parse_complete: false`:
```yaml
security:
  enforce_parse_complete: false  # Allow unparseable commands with warning
```

### Error: "cloud_auth_denied" for AWS command

**Symptom:**
```elisp
{:error "cloud_auth_denied"
 :provider :aws
 :allowed-providers []}
```

**Cause:** Cloud authentication detected but not allowed.

**Fix:** Add AWS to allowed providers:
```yaml
cloud:
  auth_detection: "warn"
  allowed_providers:
    - aws
```

## Testing Your Migration

**Step 1: Validate schema syntax**
```bash
# Check YAML is valid
yamllint scope.yml
```

**Step 2: Test common commands**
```bash
# In Emacs, test with gptel session:
# 1. Open gptel buffer with new scope.yml
# 2. Try commands that should work:
ls /workspace
cat /workspace/README.md
./workspace/scripts/test.sh

# 3. Try commands that should be denied:
cat /etc/passwd
rm /workspace/file.txt
ls | xargs rm
```

**Step 3: Check error messages**

Error messages should include:
- Clear reason for denial
- Required scope section
- Allowed patterns
- Suggestion to use request_scope_expansion

**Step 4: Test scope expansion**

Try requesting scope expansion for denied command:
1. LLM should receive error with suggestion
2. LLM should call `request_scope_expansion` tool
3. User should see transient menu with options
4. Approval should update scope.yml or allow-once list

## Rollback Strategy

If critical issues discovered after migration:

**Option 1: Git revert**
```bash
# Revert to v3 implementation
git revert <integration-commit>

# Revert scope.yml changes
git checkout HEAD~1 scope.yml
```

**Option 2: Disable validation temporarily**

NOT SUPPORTED - no dual-mode fallback. Rollback requires full git revert.

## Support and Resources

- **Spec documentation:** `openspec/specs/gptel/bash-tools.md`
- **Architecture:** `openspec/changes/bash-parser-integration/architecture.md`
- **Design rationale:** `openspec/changes/bash-parser-integration/design.md`
- **CLAUDE.md section:** "GPTEL Bash Tools and Scope Validation"

**Need help?** Check error messages for suggestions, or request scope expansion to see what's needed.
