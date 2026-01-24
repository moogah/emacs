# Testing Guide: GPTel Persistence Architecture Fix

## Overview

We've aligned the preset management system with gptel's native persistence mechanism. Key changes:

1. **Presets are now templates** - Applied once at session creation
2. **gptel Local Variables are source of truth** - Settings saved/restored automatically by gptel
3. **No preset reapplication on resume** - Prevents overwriting evolved settings
4. **Duplicate hook prevention** - Defensive code prevents duplicate Local Variables blocks
5. **Diagnostic tools** - New functions to detect and fix issues

## Architecture Summary

```
Session Creation:
  preset.md → (ONE TIME) → Buffer Settings → gptel Local Variables
                                                    ↓
                                            Source of Truth

Session Resume:
  Local Variables → gptel Restore → Buffer Settings
  (preset.md IGNORED)
```

## Testing Checklist

### 1. Clean Up Existing Sessions

Remove duplicate Local Variables from existing session files:

```elisp
;; In Emacs:
M-x jf/gptel--batch-clean-sessions
;; This scans all sessions and cleans duplicates
```

Or clean a specific open session file:

```elisp
;; Open session file, then:
M-x jf/gptel--clean-duplicate-local-vars
C-x C-s  ; Save the cleaned file
```

### 2. Test New Session Creation

```elisp
;; Create a new session
M-x jf/gptel-persistent-session
;; Enter name: test-persistence-fix
;; Select projects: n (for simplicity)

;; Verify:
;; 1. Session opens
;; 2. Check hooks
M-x jf/gptel--check-duplicate-hooks
;; Should show: "before-save-hook has N entries, 1 are gptel--save-state"

;; 3. Interact with LLM (type a prompt, get response)
;; 4. Save the buffer
C-x C-s

;; 5. Check end of file - should have ONE Local Variables block
M->  ; Jump to end
;; Look for:
;; <!-- Local Variables: -->
;; <!-- gptel-model: claude-opus-4-5-20251101 -->
;; <!-- gptel--backend-name: "Claude" -->
;; <!-- gptel--system-message: "..." -->
;; <!-- gptel--tool-names: (...) -->
;; <!-- gptel--bounds: (...) -->
;; <!-- End: -->

;; 6. Check hooks again after save
M-x jf/gptel--check-duplicate-hooks
;; Should still show count=1
```

### 3. Test Session Resume

```elisp
;; Close the session buffer
C-x k

;; Resume the session
M-x jf/gptel-resume-session
;; Select: test-persistence-fix...

;; Verify:
;; 1. Buffer opens
;; 2. Settings are restored (check modeline for model)
;; 3. Check hooks
M-x jf/gptel--check-duplicate-hooks
;; Should show count=1

;; 4. Verify tools are present
M-x describe-variable RET gptel-tools RET
;; Should show the full list of tools

;; 5. Continue conversation
;; Type another prompt, get response

;; 6. Save again
C-x C-s

;; 7. Check end of file - should STILL have ONE Local Variables block
M->
;; No duplicates should have appeared

;; 8. Check hooks after second save
M-x jf/gptel--check-duplicate-hooks
;; Should still show count=1
```

### 4. Test Multiple Save/Resume Cycles

```elisp
;; With the session still open:
;; 1. Save
C-x C-s
M-x jf/gptel--check-duplicate-hooks  ; count=1

;; 2. Close and resume
C-x k
M-x jf/gptel-resume-session
M-x jf/gptel--check-duplicate-hooks  ; count=1

;; 3. Save again
C-x C-s
M-x jf/gptel--check-duplicate-hooks  ; count=1

;; 4. Check file - still ONE Local Variables block
M->

;; Repeat this cycle 3-4 times
;; Hook count should always be 1
;; File should always have ONE Local Variables block
```

### 5. Test Setting Changes Persist

```elisp
;; Open the test session
M-x jf/gptel-resume-session

;; Change a setting
M-x gptel-menu
;; Navigate to "Model" and change it (or use C-u M-x gptel-send)

;; Interact with LLM using new settings

;; Save
C-x C-s

;; Close and resume
C-x k
M-x jf/gptel-resume-session

;; Verify the setting change persisted
;; Check modeline or:
M-x describe-variable RET gptel-model RET
;; Should show the NEW model, not the original preset value
```

### 6. Verify Preset File is Static

```elisp
;; Create a new session
M-x jf/gptel-persistent-session
;; Name: preset-static-test

;; Check preset file
;; In a terminal:
cat ~/.gptel/sessions/preset-static-test-*/preset.md
;; Note the model value

;; In Emacs, change the model
M-x gptel-menu
;; Change model

;; Interact and save
C-x C-s

;; Check preset file again
cat ~/.gptel/sessions/preset-static-test-*/preset.md
;; Should be UNCHANGED (preset is static template)

;; Check session file
tail -50 ~/.gptel/sessions/preset-static-test-*/session.md
;; Local Variables should show NEW model
```

### 7. Test Diagnostics

```elisp
;; Open any session
M-x jf/gptel-resume-session

;; Run full diagnostics
M-x jf/gptel-session-diagnostics

;; Should display:
;; - Buffer name, session ID, directory
;; - GPTel Mode: enabled
;; - Auto-save: enabled
;; - Save Hook Status: gptel--save-state hooks: 1
;; - No warnings about duplicates
```

### 8. Test Subagent Resume (If Using Subagents)

```elisp
;; If you have subagents:
M-x jf/gptel-resume-subagent

;; Verify:
;; 1. Subagent opens correctly
;; 2. Settings restored from Local Variables (not preset)
;; 3. Check hooks
M-x jf/gptel--check-duplicate-hooks
;; count=1
```

## Expected Behavior

### ✅ GOOD - What Should Happen

1. **Hook count always 1** - No matter how many save/resume cycles
2. **ONE Local Variables block** - At end of file, not duplicated
3. **Settings persist across resume** - Model, tools, temperature restored correctly
4. **Setting changes preserved** - Changes made during session are saved
5. **Preset file static** - preset.md doesn't update when settings change
6. **No "overwrites" warnings** - Preset not reapplied on resume

### ❌ BAD - What Should NOT Happen

1. **Multiple hook entries** - Would cause duplicate Local Variables on save
2. **Duplicate Local Variables blocks** - Multiple blocks at end of file
3. **Settings reset on resume** - Reverting to original preset values
4. **Preset file updating** - preset.md should never change after creation

## Troubleshooting

### Problem: Duplicate hooks detected

```elisp
;; Check current count
M-x jf/gptel--check-duplicate-hooks

;; If count > 1, the helper function will auto-fix on next session creation/resume
;; Or manually fix:
(remove-hook 'before-save-hook #'gptel--save-state t)
(add-hook 'before-save-hook #'gptel--save-state nil t)
```

### Problem: Duplicate Local Variables blocks remain

```elisp
;; Clean current buffer
M-x jf/gptel--clean-duplicate-local-vars
C-x C-s

;; Or clean all sessions
M-x jf/gptel--batch-clean-sessions
```

### Problem: Settings not persisting

```elisp
;; Check if Local Variables are being written
;; After saving, check end of file:
M->
;; Should see gptel-model, gptel--backend-name, etc.

;; If missing, check if gptel-mode is enabled:
M-x describe-variable RET gptel-mode RET
;; Should be t

;; Check hooks:
M-x describe-variable RET before-save-hook RET
;; Should include gptel--save-state
```

### Problem: Preset being reapplied on resume

This should no longer happen with the new code. If it does:

```elisp
;; Check which version of commands.el is loaded:
M-x describe-function RET jf/gptel-resume-session RET
;; Click the file link - should show the NEW version
;; Look for comment: "DO NOT overwrite with preset!"

;; If old version is loaded, reload:
M-x load-file RET config/gptel/sessions/commands.el RET
```

## Success Criteria

After testing, you should observe:

- [ ] No duplicate Local Variables blocks in any session file
- [ ] Hook count always equals 1 in active session buffers
- [ ] Settings persist correctly across close/resume cycles
- [ ] Changed settings are preserved (not reset to preset)
- [ ] Preset files remain static after session creation
- [ ] Diagnostics show no warnings
- [ ] Multiple save/resume cycles don't accumulate issues

## Next Steps After Testing

Once confirmed working:

1. **Delete old duplicate blocks** - Run batch clean on all sessions
2. **Commit the changes** - The new architecture is stable
3. **Update any documentation** - Reflect preset-as-template model
4. **Optional: Update existing presets** - Regenerate stale preset files for reference

## Additional Diagnostic Commands

```elisp
;; List all sessions
M-x jf/gptel-list-sessions

;; Browse session hierarchy
M-x jf/gptel-browse-sessions-hierarchical

;; Refresh session registry
M-x jf/gptel-refresh-sessions

;; Check specific buffer variables
M-x describe-variable RET gptel-backend RET
M-x describe-variable RET gptel-model RET
M-x describe-variable RET gptel-tools RET
M-x describe-variable RET gptel--system-message RET
```

## Files Modified

- `config/gptel/sessions/commands.org` - Main changes
- `config/gptel/sessions/commands.el` - Tangled output

## Backup

Before testing, consider backing up your sessions:

```bash
cp -r ~/.gptel/sessions ~/.gptel/sessions.backup.$(date +%Y%m%d)
```

## Questions or Issues?

If you encounter unexpected behavior:

1. Check the diagnostics output
2. Verify hook counts
3. Examine the Local Variables block manually
4. Check *Messages* buffer for warnings
5. Review the session's metadata.json file

The new architecture should be more robust and maintainable!
