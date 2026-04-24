---
name: activities-integration-metadata-guard
description: Replace the jf/gptel--read-session-metadata feature-detection guard in activities-integration with a check on a still-present function, and drop the require of the deleted module.
change: gptel-chat-state-persistence
status: done
relations:
  - "blocked-by:session-creation-drawer-prepopulate"
---

## Files to modify

- `config/gptel/sessions/activities-integration.org` (modify) — drop `(require 'gptel-session-metadata)`; update the `fboundp` guard.
- `config/gptel/sessions/test/activities/activity-session-chat-spec.el` (modify, if still referencing metadata.yml) — align with the new drawer-authoritative contract (may overlap with session-creation-drawer-prepopulate task — coordinate).

## Implementation steps

1. In `activities-integration.org`, remove `(require 'gptel-session-metadata)`.
2. Update the guard in `jf/gptel-persistent-session-for-activity`:
   ```elisp
   (unless (and (fboundp 'jf/gptel--read-session-metadata)
                (fboundp 'jf/gptel--register-session))
     (error "GPTEL session registry not available"))
   ```
   → replace with:
   ```elisp
   (unless (fboundp 'jf/gptel--register-session)
     (error "GPTEL session registry not available"))
   ```
3. Audit the rest of the file for any other metadata references; remove them.
4. If the `jf/gptel--create-session-core` call in this file needs a `parent-session-id` argument (added by the session-creation-drawer-prepopulate task), pass `nil` — activities are top-level, not agents.
5. Tangle: `./bin/tangle-org.sh config/gptel/sessions/activities-integration.org`.
6. Run `./bin/run-tests.sh -d config/gptel/sessions/test/activities`.

## Design rationale

The guard's intent is "sessions module is loaded and ready." `jf/gptel--register-session` is a stable public-surface symbol from the registry module; it is a good probe for "can I create and register a session?" (design.md §Decision 8).

Removing the `jf/gptel--read-session-metadata` probe prevents a future regression where a downstream change accidentally re-introduces `metadata.yml` via this probe assumption.

## Verification

- `./bin/tangle-org.sh config/gptel/sessions/activities-integration.org`
- `./bin/run-tests.sh -d config/gptel/sessions/test/activities`
- `grep -n "gptel-session-metadata\|jf/gptel--read-session-metadata" config/gptel/sessions/activities-integration.el` — no matches.

## Context

- proposal.md §Impact (Code (modify) — activities-integration)
- architecture.md §Components (Modified — activities-integration.el)
- design.md §Decision 8

## Review

Reviewed inline 2026-04-24 by orchestrator. Impl commit `1c94e29`, merge `5aa37bf`.

- Diff matches spec: `(require 'gptel-session-metadata)` dropped from both
  `.org` and `.el`, guard simplified to `(fboundp 'jf/gptel--register-session)`
  per design.md §Decision 8, dependencies doc line updated ("registry,
  metadata, filesystem" → "registry, filesystem").
- `activities-integration.el:91` already passes `nil` for `parent-session-id`
  (spec step 4 — overlap with session-creation-drawer-prepopulate was
  anticipated).
- `grep -n "gptel-session-metadata\|jf/gptel--read-session-metadata"
  config/gptel/sessions/activities-integration.el` — no matches. Docstring
  mentions of `metadata.yml` on lines 37/79 are historical "no sidecar is
  written" notes, explicitly permitted by the regression-sweep task.
- Targeted tests: `./bin/run-tests.sh -d config/gptel/sessions/test/activities`
  — 8 specs, 0 failed.

Findings: none. Flipping to `done`.
