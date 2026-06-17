---
name: fix-persistent-agent-build-scope-plist-arity
description: persistent-agent --task caller passes 1 arg to the 2-arg (read-paths write-paths) build-scope-plist; agent session-creation errors with wrong-number-of-arguments
source: gptel-work-root-default-directory (canonicalize-project-root-at-source verification)
status: ready
relations:
  - discovered-from:canonicalize-project-root-at-source
---

## Provenance
- discovered_by: implementor (canonicalize-project-root-at-source, change gptel-work-root-default-directory)
- discovered_while: running `./bin/run-tests.sh -d config/gptel/sessions` to verify an unrelated sessions change
- discovered_class: invariant-gap (incomplete caller migration)

## Why
The `agent-build-scope-plist-split` change (commit ff98330e) split
`jf/gptel-persistent-agent--build-scope-plist` into a read/write-aware function:

```elisp
(defun jf/gptel-persistent-agent--build-scope-plist (read-paths write-paths) ...)
```

But the caller in `jf/gptel-persistent-agent--task` was NOT migrated — it still
passes a single positional arg:

```elisp
;; config/gptel/tools/persistent-agent.org §567 / persistent-agent.el:337
(scope-plist
 (jf/gptel-persistent-agent--build-scope-plist allowed-paths-list))
```

At runtime this raises `(wrong-number-of-arguments #[(read-paths write-paths) ...] 1)`,
breaking the agent session-creation path. It surfaces in the sessions suite as a
failing spec: `config/gptel/sessions/test/commands/identity-keys-emission-spec.el`
("jf/gptel-persistent-agent--task ... writes the agent's OWN :GPTEL_SESSION_ID:
:GPTEL_BRANCH: main, and parent link").

## Files to modify
- `config/gptel/tools/persistent-agent.org` — `jf/gptel-persistent-agent--task`
  (the `scope-plist` binding, §567) — supply the read/write split the split
  function now expects. Then re-tangle.

## Implementation steps
1. Inspect `jf/gptel-persistent-agent--build-scope-plist`'s new contract
   (`:read` = read-paths verbatim, `:write` = write-paths + `/tmp/**` scratch,
   `:deny` = standard set) and the `agent-build-scope-plist-split` task/design to
   determine how `allowed-paths-list` should split into read vs write.
2. Update the §567 caller to pass both `read-paths` and `write-paths`.
3. `./bin/tangle-org.sh config/gptel/tools/persistent-agent.org`.
4. `./bin/run-tests.sh -d config/gptel/sessions` and
   `./bin/run-tests.sh -d config/gptel/tools` green.

## Verification
- `grep -n "build-scope-plist" config/gptel/tools/persistent-agent.el` — the
  caller passes two args.
- `identity-keys-emission-spec.el` passes; no `wrong-number-of-arguments`.

## Context
register/shape/scope-config-plist; commit ff98330e (agent-build-scope-plist-split);
config/gptel/tools/persistent-agent.org §202 (def) / §567 (caller).
