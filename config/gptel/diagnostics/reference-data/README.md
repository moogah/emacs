# GPTEL Session Restoration Bug - Reference Data

This directory contains reference files documenting the `gptel--inherit-stickiness` bug discovered during session restoration testing.

## Files

### session-before-restoration.md
- **Source**: Original uncorrupted session from test-20260126142209
- **Status**: Correct text properties saved in Local Variables
- **Key bounds**: `((tool (80 1346 "toolu_...")) (ignore (37 80) (1346 1351)) ...)`
- **Note**: Position 37 should have `ignore` property, position 80 should have `tool` property

### session-after-restoration.md
- **Source**: Corrupted session from test-restore-2 after opening and sending prompt
- **Status**: Incorrect text properties saved after restoration bug
- **Key bounds**: `((tool (37 1346 "toolu_...")) (ignore (1346 1351)) ...)`
- **Bug**: Position 37 has `tool` property instead of `ignore` (missing the (37 80) ignore range)

### full-diagnostic-log.txt
- **Source**: Complete diagnostic trace from test-restore-2 session
- **Contents**: Full backtraces showing `gptel--inherit-stickiness` overwriting properties
- **Key evidence**: Lines 128-184 show the backtrace proving the bug
  - Line 145: `add-text-properties(37 80 (gptel ignore))` - correct call
  - Line 142: `gptel--inherit-stickiness(37 80 43)` - after-change-functions triggered
  - Line 141: `add-text-properties(37 80 (gptel (tool ...)))` - overwrites ignore!

## Bug Summary

**Root cause**: `gptel--inherit-stickiness` function (gptel.el:698-706) runs during restoration via `after-change-functions`, copying adjacent properties and overwriting intentional property assignments.

**Symptom**: Tool calls fail to serialize correctly, showing as `"name": {}` and `"input": {}` in API requests.

**Fix needed**: `gptel--restore-props` should set `inhibit-modification-hooks` to prevent `after-change-functions` from running during restoration.

## Testing Methodology

1. Start with uncorrupted session (test-20260126142209)
2. Copy to new location: `cp -r test-20260126142209 test-restore-2`
3. Clear diagnostic log: `> ~/.gptel-diagnostics.log`
4. Open session.md in Emacs (fresh restart)
5. Send a prompt to trigger restoration and saving
6. Examine logs and resulting session.md file

## Related Files

- `../diagnostics.org` - Complete diagnostic module with detailed findings
- `../diagnostics.el` - Tangled elisp code for diagnostics
