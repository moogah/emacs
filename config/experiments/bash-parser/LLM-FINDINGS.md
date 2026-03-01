# LLM Scenario Testing - Initial Findings

## Summary

Tested 4 representative LLM-generated commands. Results show the parser is **robust** but reveals **critical gaps in danger detection**.

## Test Results

### ✅ Test 1: Complex Flag Combinations

**Command:** `tar -czf backup.tar.gz --exclude=.git --exclude=node_modules /home/user`

**Result:**
```elisp
(:command-name "tar"
 :flags ("-czf" "--exclude=.git" "--exclude=node_modules")
 :positional-args ("backup.tar.gz" "/home/user")
 :dangerous-p nil)
```

**Status:** ✅ **SUCCESS** - Correctly parses multiple long flags and positional args

---

### ✅ Test 2: Sudo with Dangerous Command (FIXED 2026-03-01)

**Command:** `sudo rm -rf /tmp/test`

**Result:**
```elisp
(:command-name "sudo"
 :flags nil
 :positional-args ("rm" "-rf" "/tmp/test")
 :dangerous-p t)
```

**Status:** ✅ **FIXED** - Wrapper command support implemented

**Solution:** Added special handling for command wrappers (sudo, env, time, nice, nohup)
- Sudo is now recognized as a wrapper command
- Wrapper flags (like `-u`, `-E`) are correctly separated from wrapped command
- All sudo commands are marked as dangerous (`:dangerous-p t`)
- Nested command arguments preserved in `:positional-args`

**Implementation Details:**
- New database: `jf/bash-parser-wrapper-commands` with flag specifications
- Special parser: `jf/bash-parse--parse-wrapper-command`
- Tree-sitter limitation: bash grammar treats all words after command_name as flat structure
- Our parser understands wrapper semantics and separates wrapper flags from wrapped command

**Impact:** HIGH - Sudo commands now correctly flagged as dangerous for:
- Package installation: `sudo apt-get install`
- File operations: `sudo rm -rf`
- System configuration: `sudo systemctl restart`
- User management: `sudo useradd`

---

### ⚠️  Test 3: Git Clean Extended Flags

**Command:** `git clean -fdx`

**Result:**
```elisp
(:command-name "git"
 :subcommand "clean"
 :flags ("-fdx")
 :dangerous-p nil)
```

**Status:** ⚠️ **GAP IN PATTERN MATCHING**

**Problem:** Database has `git clean -fd` but NOT `-fdx`
- The `-x` flag is MORE dangerous (removes ignored files like build artifacts, IDE configs)
- Pattern matching is too specific - misses flag variations

**Impact:** MEDIUM - LLMs might generate `-fdx` instead of `-fd` for thorough cleanup

**Recommendation:**
1. Update dangerous pattern to match: `-fd`, `-fdx`, `-dfx`, etc.
2. Consider "any-flag-contains" approach: if flags contain BOTH 'f' AND 'd', it's dangerous
3. Document that `-x` makes it even more dangerous

---

### ✅ Test 4: Nested Pipeline and Chain

**Command:** `git diff HEAD | grep -v '^-' | wc -l && echo 'Lines added'`

**Result:**
```elisp
(:type :pipeline
 :command-count 4)
```

**Status:** ✅ **SUCCESS** - Correctly identifies as pipeline with 4 commands

**Note:** The chain operator `&&` is handled correctly within the pipeline structure.

---

## Critical Security Gaps Discovered

### 1. Sudo Commands (FIXED 2026-03-01)

**Status:** ✅ FIXED - Wrapper command support implemented

**Current implementation:**
```elisp
(sudo . (:flags-with-args ("-u" "-g" "-C" ...)
         :flags-no-args ("-A" "-b" "-E" ...)
         :dangerous t))
```

**Working sudo commands:**
- `sudo rm -rf /path` → correctly marked as dangerous
- `sudo apt-get install -y package` → correctly parsed
- `sudo systemctl restart service` → correctly parsed
- `sudo -u www-data php script.php` → flags separated correctly
- `sudo -E env PATH=... command` → complex wrapper chains work

**Note:** Also implemented for `env`, `time`, `nice`, `nohup` wrappers

### 2. Flag Variation Gaps

**Current database is too specific:**
```elisp
(git . ((:subcommand "clean" :flags ("-f" "-fd" "-df"))))
```

**Missing variations:**
- `-fdx`, `-dfx`, `-xfd`, `-xdf`, `-fxd`, `-dxf`
- Combined short flags: `-rf`, `-fr`, `-Rf`, `-fR`

**Better approach:**
```elisp
(git . ((:subcommand "clean"
         :any-flag-contains ("f" "d"))))  ; Any combo of f and d
```

### 3. Chmod Overly Permissive

**Not in database:**
```elisp
(chmod . ((:flags ("-R"))           ; -R makes it recursive
          (:any-flag-contains ("R"))
          (:positional-contains ("777" "666"))))  ; Overly permissive modes
```

### 4. dd Command Entirely Missing

**Not in database:**
```elisp
(dd . ((:any t)))  ; dd is ALWAYS dangerous - can destroy disks
```

### 5. wget/curl Piped to Shell

**Pattern not detected:**
```bash
wget -qO- url | bash
curl -sSL url | sh
```

**Should detect:**
- Pipeline where first command is `wget`/`curl` with URL
- Second command is shell interpreter (`bash`, `sh`, `zsh`)

### 6. Docker System Prune

**Not in database:**
```elisp
(docker . ((:subcommand "system" :flags ("-a" "-af" "--volumes"))
           (:subcommand "volume" :subsubcommand "prune")))
```

### 7. Fork Bomb and Malicious Patterns

**Not handled:**
```bash
:(){:|:&};:
```

Parser should detect and reject obviously malicious syntax patterns.

## Recommendations

### Immediate Actions

1. ~~**Add sudo to dangerous patterns**~~ (FIXED - 2026-03-01)
   - ✅ Sudo wrapper support implemented
   - ✅ All sudo commands marked as dangerous
   - TODO: Consider adding `doas`, `su`, `pkexec` as wrappers

2. **Fix git clean pattern** (HIGH)
   ```elisp
   (git . ((:subcommand "clean" :any-flag-contains ("f" "d"))))
   ```

3. **Add chmod pattern** (HIGH)
   ```elisp
   (chmod . ((:flags ("-R" "-r"))
             (:any-flag-contains ("R" "r"))
             (:requires-review-when-mode "777" "666")))
   ```

4. **Add dd pattern** (HIGH)
   ```elisp
   (dd . ((:any t)))
   ```

5. **Add docker system prune** (MEDIUM)
   ```elisp
   (docker . ((:subcommand "system" :flags ("prune"))
              (:subcommand "volume" :subsubcommand "prune")))
   ```

### Pattern Matching Enhancements

**Current approach:** Exact flag matching
**Better approach:** Flag component matching

```elisp
;; Instead of:
(:flags ("-rf" "-fr" "-Rf" "-fR"))

;; Use:
(:any-flag-contains ("r" "f"))  ; Matches any combination
```

**Implementation:**
```elisp
(defun jf/bash-parser--flag-contains-chars (flag chars)
  "Check if FLAG contains all CHARS."
  (cl-every (lambda (c) (string-match-p (regexp-quote c) flag))
            chars))
```

### Danger Levels

Not all dangerous commands are equally dangerous. Consider levels:

- **CRITICAL** (always block): `sudo`, `dd`, fork bombs
- **HIGH** (require approval): `rm -rf`, `git push --force`, `chmod -R 777`
- **MEDIUM** (warn): `rm -r`, `docker system prune --dry-run`
- **LOW** (log): `git clean -n` (dry-run flags)

### Recursive Parsing for Sudo

When command is `sudo`, parse the positional args as a nested command:

```elisp
(defun jf/bash-parse--handle-sudo (result)
  "If RESULT is sudo command, recursively parse its arguments."
  (when (string= (plist-get result :command-name) "sudo")
    (let* ((args (plist-get result :positional-args))
           (nested-cmd (string-join args " "))
           (nested-result (jf/bash-parse nested-cmd)))
      ;; Merge nested danger into sudo result
      (plist-put result :dangerous-p t)
      (plist-put result :nested-command nested-result)
      (plist-put result :nested-dangerous-p
                 (plist-get nested-result :dangerous-p)))))
```

## Next Steps

1. **Run full LLM scenario test suite**
   ```elisp
   M-x jf/bash-parser-llm-scenarios-report
   ```

2. **Update dangerous patterns database** with findings above

3. **Implement flag component matching** for more flexible pattern detection

4. **Add danger levels** to patterns (critical/high/medium/low)

5. **Test sudo recursive parsing** approach

6. **Document unsupported patterns** that can't be safely parsed

7. **Create security policy** for gptel bash tool integration:
   - Always require approval for CRITICAL commands
   - Show full command + nested command for sudo
   - Log all executed commands with danger level

## Success Metrics

After implementing fixes, we should achieve:

- ✅ 100% detection of `sudo` commands (currently 0%)
- ✅ Detection of `git clean` flag variations (currently fails)
- ✅ Detection of `chmod` with overly permissive modes (currently missing)
- ✅ Detection of `dd` commands (currently missing)
- ✅ Detection of download-and-execute patterns (currently missing)
- ✅ All 55+ LLM scenario tests documented with expected behavior

## Conclusion

The bash parser is **structurally sound** - it correctly handles:
- ✅ Complex flag combinations
- ✅ Multi-stage pipelines
- ✅ Command chains
- ✅ Nested operators
- ✅ Wrapper commands (sudo, env, time, etc.) - FIXED 2026-03-01

The remaining gaps are in the **dangerous pattern database**:
- ~~⚠️ Missing sudo command~~ (FIXED 2026-03-01)
- ⚠️ Missing other dangerous commands (chmod, dd)
- ⚠️ Too-specific flag matching (misses variations)

**Bottom line:** Parser works great for wrapper commands. Pattern database still needs expansion for other dangerous patterns (chmod, dd, docker system prune, etc.).
