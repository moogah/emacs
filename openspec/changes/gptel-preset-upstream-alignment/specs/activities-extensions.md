## MODIFIED Requirements

### Requirement: Gptel preset selection in transient UI

The activities transient UI SHALL list available presets from `gptel--known-presets` instead of scanning the filesystem via `jf/gptel--list-preset-templates`.

#### Scenario: Preset listing from registry
- **WHEN** the transient UI renders the preset selection column
- **AND** `gptel--known-presets` is bound and non-nil
- **THEN** preset names are derived from `gptel--known-presets` (symbol names)
- **AND** descriptions are shown from each preset's `:description` key

#### Scenario: Fallback when gptel not loaded
- **WHEN** `gptel--known-presets` is not bound (gptel not loaded)
- **THEN** the preset selection column is not shown
- **AND** activity creation proceeds without gptel preset selection

### Requirement: Session creation receives preset name symbol

The activities integration SHALL pass a preset name symbol (not a template filename) to the session creation function.

#### Scenario: Preset passed by name
- **WHEN** user selects preset "executor" in the transient UI and creates an activity
- **THEN** the session creation function receives the symbol `executor` as the preset parameter
- **AND** session creation applies the preset via `gptel--apply-preset` with buffer-local setter
- **AND** creates `scope.yml` from the preset's scope defaults in `jf/gptel-preset--scope-defaults`

#### Scenario: No preset selected
- **WHEN** user creates an activity with `--no-gptel` flag
- **THEN** no session is created
- **AND** no preset lookup occurs

### Requirement: Integration contract unchanged

The data contract between activities and gptel sessions SHALL remain the same:

**Gptel Session Plist (stored in activity metadata):**
```elisp
(:session-id "session-identifier"
 :session-file "/path/to/session.md"
 :session-dir "/path/to/session-dir")
```

Session resume via `jf/gptel-session--open-existing` is unchanged — it opens the session.md file, which triggers the find-file-hook for session auto-initialization.

#### Scenario: Resume opens session normally
- **WHEN** resuming an activity with gptel session data
- **THEN** `jf/gptel-session--open-existing` opens the session file
- **AND** the session auto-initialization hook handles preset restoration via upstream's `gptel--restore-state`

## REMOVED Requirements

### Requirement: Filesystem-based preset template listing
**Reason**: `jf/gptel--list-preset-templates` scanned `config/gptel/presets/` for `.md` files. With presets registered in `gptel--known-presets` at init time, the registry is the authoritative source. Listing registered presets is faster and guaranteed consistent with what the session system uses.
**Migration**: Replace `(fboundp 'jf/gptel--list-preset-templates)` guard with `(bound-and-true-p gptel--known-presets)` guard. Replace filesystem scan with `(mapcar #'car gptel--known-presets)` for name listing.
