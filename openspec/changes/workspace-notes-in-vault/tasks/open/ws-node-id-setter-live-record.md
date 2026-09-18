---
name: ws-node-id-setter-live-record
description: Make workspace-set-node-id resolve the live registry record (accept NAME-OR-RECORD, copy-on-write, puthash before flush) so out-of-package callers holding only the payload's :name can set the node-id; retire workspace--integration-live-record.
change: workspace-notes-in-vault
status: ready
task_class: contract
on_critical_path: true
cites_register_entries:
  - register/shape/workspace-plist-v3
  - register/shape/workspace-integration-anchor-payload
  - register/invariant/registry-name-equals-basename
relations:
  - enables:int-workspaces-org-graph
  - discovered-from:arch-cycle-1789334430-eoc-2
discovered_from: arch-cycle-1789334430-eoc-2
discovered_by: architect
discovered_class: interface-drift
created_at: 2026-09-17T00:00:00Z
planned_at: null
---

> Created by cycle-1789334430 integrate from end-of-cycle architect finding
> `arch-cycle-1789334430-eoc-2` (advisory, interface-drift against
> `register/shape/workspace-plist-v3`). Must land before `int-workspaces-org-graph`,
> the first out-of-package caller of the setter.

## Summary
`workspace-set-node-id` (config/workspaces/persistence.org, tangled persistence.el ~:276-295)
mutates the record it is handed in place and flushes. It only works when handed the LIVE
record held in `workspace--registry`: a copy (as every other mutator produces via
copy-on-write) or a superseded record silently no-ops on the next flush. The integration
layer's handlers receive a payload plist carrying only `:name`, and there is no public
name->record lookup, so the cycle-1 inline fix added the private
`workspace--integration-live-record` (integrations.el ~:59) as a stop-gap. Give the setter
the same discipline as `workspace-re-anchor`: resolve the live record by name, copy-on-write,
`puthash`, then flush.

## Files to modify
- config/workspaces/persistence.org (modify; tangle) — setter signature and body
- config/workspaces/integrations.org (modify; tangle) — delete `workspace--integration-live-record` and its prose if nothing else needs it
- config/workspaces/docs/README.org ~:184-186 — setter contract prose
- config/workspaces/test/persistence-v3-spec.el (modify) — new scenarios

## Implementation steps
1. Change `workspace-set-node-id` to accept `NAME-OR-RECORD`: a string resolves through
   `workspace--registry`; a plist resolves by its `:name` (and signals `user-error` when the
   registry's current entry for that name is not `eq` to the plist AND the plist's node-id
   differs from the live one — i.e. refuse to write from a stale copy).
2. Apply copy-on-write: `(copy-sequence record)` + `plist-put :node-id` + `puthash` under the
   name key, THEN `workspace--flush-state`. Return the new record.
3. Keep the `(or null string)` validation and the synchronous flush.
4. Retire `workspace--integration-live-record`: dispatchers keep rebuilding the payload per
   handler from the live record (register/shape/workspace-integration-anchor-payload), but a
   handler that needs to set node-id now calls the public setter with the payload's `:name`.
5. Specs: (a) setter called with a name updates the registry and flushes; (b) setter called
   with a stale copy is refused; (c) a later handler in the same dispatch observes the node-id
   set by an earlier handler through the public setter (un-stubbed, end to end);
   (d) round-trip through persistence keeps the value.

## Verification
```
./bin/tangle-org.sh config/workspaces/persistence.org
./bin/tangle-org.sh config/workspaces/integrations.org
./bin/run-tests.sh -d config/workspaces --report
grep -n 'workspace--integration-live-record' config/workspaces/integrations.el config/workspaces/persistence.el   # expect none
grep -n 'workspace-set-node-id' config/workspaces/persistence.el config/workspaces/docs/README.org
```

## Out-of-scope
The anchor-payload signature `(ws context)`, the `:node-id` slot itself, and the
int-workspaces-org-graph subscriber that will call the setter.

## Observations

## Discoveries
