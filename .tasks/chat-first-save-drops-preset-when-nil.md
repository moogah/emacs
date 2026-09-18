---
name: chat-first-save-drops-preset-when-nil
description: Investigate the chat subsystem's first save-buffer of a new session rewriting the property drawer without :GPTEL_PRESET: when gptel--preset is nil at save time (observed in batch Emacs during gptel-session-keywords-and-hooks).
status: externalised
source: openspec/changes/workspace-notes-in-vault
relations:
  - discovered-from:gptel-session-keywords-and-hooks
discovered_from: gptel-session-keywords-and-hooks
discovered_by: implementor
discovered_class: invariant-gap
created_at: 2026-09-17T00:00:00Z
---

> Externalised by cycle-1789334430 integrate (change workspace-notes-in-vault). Latent,
> pre-existing chat/menu behaviour surfaced while testing the session head keywords; likely a
> test-environment artefact, but the drawer-writer contract is load-bearing (drawer
> corruption has cost real time before), so it is worth a deliberate look.

## Observation
In batch Emacs, the first `save-buffer` of a freshly created session rewrote the
`:PROPERTIES:` drawer without the `:GPTEL_PRESET:` line when `gptel--preset` was nil at save
time. Suspect: the chat-mode save-state writer (config/gptel/chat/menu.org or the drawer
writer) treats a nil preset as "remove the key" rather than "leave the persisted key alone".

## Steps
1. Reproduce with a spec: create a session through `jf/gptel--create-session-core`, visit it
   with chat mode active and `gptel--preset` nil, `save-buffer`, re-read the drawer head.
2. Decide the contract (nil preset preserves the persisted key vs. clears it) against
   `register/shape/session-file-head` and the drawer-writer invariants; fix or document.

## Verification
```
./bin/run-tests.sh -d config/gptel/chat --report
```
