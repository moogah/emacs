# gptel/sessions-branching (delta)

## MODIFIED Requirements

### Requirement: Configuration inheritance via drawer

The system SHALL inherit the parent branch's preset, scope, and session-level configuration into the new branch via the file-level `:PROPERTIES:` drawer carried at `point-min` of the parent's `session.org`.

Configuration inheritance SHALL:
- Copy the parent's `session.org` content (drawer + conversation history) verbatim up to the branch point — see "Requirement: Context truncation"
- Preserve all inherited drawer keys (`:GPTEL_PRESET:`, `:GPTEL_PARENT_SESSION_ID:` when present, the upstream-compatible chat-mode snapshot keys, and the `:GPTEL_SCOPE_*:` keys) in the new branch's `session.org`
- Set the new branch's own identity keys in its drawer: `:GPTEL_SESSION_ID:` equal to the shared session id, and `:GPTEL_BRANCH:` equal to the new branch name (the inherited `:GPTEL_BRANCH:` from the copied parent content SHALL be overwritten with the new branch's name)
- NOT write any session-level sidecar (`scope.yml`, `metadata.yml`, `scope-plan.yml` are dead — see sessions-persistence.md)

Subsequent changes to the drawer in either branch SHALL NOT affect the other, since each branch owns its own `session.org`.

The branch lineage (parent branch name, branch point position) is the only data the new branch records SEPARATELY from the drawer — it lives in `branch-metadata.yml` and is documented in "Requirement: Branch metadata" below.

**Session identity** lives in the drawer (`:GPTEL_SESSION_ID:`), not in the directory path. All branches of one session share the same `:GPTEL_SESSION_ID:` value; they are distinguished by their per-branch `:GPTEL_BRANCH:`. (The shared `<session-dir>` remains a storage convention but is no longer the source of session identity.)

#### Scenario: Drawer is preserved verbatim across branch creation
- **WHEN** creating a new branch from a parent whose `session.org` carries `:GPTEL_PRESET: executor`, `:GPTEL_SCOPE_READ:` patterns, and chat-mode snapshot keys
- **THEN** the new branch's `session.org` SHALL contain the same `:PROPERTIES:` drawer at `point-min` with all those configuration keys intact
- **AND** no separate sidecar file SHALL be written for preset or scope state
- **AND** subsequent drawer edits in either branch SHALL NOT propagate to the other

#### Scenario: Session ID consistency via the drawer
- **WHEN** a session has multiple branches (main, feature-1, feature-2)
- **THEN** every branch's `session.org` drawer SHALL carry the same `:GPTEL_SESSION_ID:` value
- **AND** each branch's drawer SHALL carry its own `:GPTEL_BRANCH:` value
- **AND** the registry key `"<session-id>/<branch-name>"` is sourced from those drawer values

## REMOVED Requirements

### Requirement: Auto-initialization of new branches

**Reason**: New-branch activation no longer depends on the global `find-file-hook`. A newly created branch's `session.org` is written complete (drawer + body, including the branch's identity keys) before it is opened; opening it triggers content-addressed activation via the `magic-mode-alist` session signature (see `gptel/chat-mode`), and the `gptel-chat-mode-hook` binder establishes the branch's buffer-local state and registry entry from the drawer (see `gptel/sessions-persistence` Requirement: Content-addressed activation and binding).

**Migration**: None — branch creation continues to open the new branch file (`find-file`), which now activates and binds through the mode rather than the retired hook. The detection-by-path-pattern step (`*/branches/*/session.org` + path extraction) is removed; identity comes from the new branch's drawer.

## MODIFIED Requirements

### Requirement: Registry integration

The system SHALL register new branches in the in-memory session registry when the branch buffer is opened.

Registry integration SHALL:
1. Add an entry with key `"<session-id>/<branch-name>"`, sourced from the branch drawer's `:GPTEL_SESSION_ID:` / `:GPTEL_BRANCH:`
2. Store session-dir, branch-dir, and buffer reference
3. Enable O(1) lookup for branch buffers

The registry enables fast session/branch lookup without filesystem scanning.

**Note:** Branch creation does NOT explicitly register the branch. Registration happens implicitly when the new branch's `session.org` is opened: content-addressed activation flips the buffer to `gptel-chat-mode`, and the `gptel-chat-mode-hook` binder registers it. This ensures the buffer exists before registration and avoids race conditions.

#### Scenario: Implicit registration via buffer opening
- **WHEN** a new branch `20260128153042-feature` is created in session `my-session-20260205`
- **AND** the branch's `session.org` is opened via `find-file`
- **THEN** content-addressed activation flips the buffer to `gptel-chat-mode`
- **AND** the mode-hook binder registers the branch with key `"my-session-20260205/20260128153042-feature"` from the drawer
- **AND** stores the branch directory path and buffer reference
