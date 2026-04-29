---
name: refuse-add-to-scope-on-nil-operation
description: "Upstream guard at the --add-to-scope action handler — refuse on nil-operation bash violations (cloud-auth, parse-incomplete) and surface the right action instead."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:rewire-expansion-writer
  - discovered-from:implement-drawer-writer
---

## Cites register entries

- `register/vocabulary/operation-to-drawer-key` — `unmapped_policy: error` documents that `:operation nil` is intentionally not collapsed at the writer; the upstream fix lives at the action layer.
- `register/shape/violation-info` — the `:operation` field is nil for cloud-auth and parse-incomplete violations.

## Background

Architect audit finding `arch-cycle-1777460733-9` (related context):
the writer's strict-error fallback (applied in cycle-1 integrate) now
errors when `:operation` is nil. That's the correct behaviour at the
writer layer, but the resulting error is user-hostile — it surfaces as
a generic "scope-expansion: cannot map nil :operation" message inside
the transient menu's Lisp callback, with no indication of the right
next step.

The cloud-auth violation path
(`scope-validation.el:434-461`, `--build-violation-info`) builds a
violation plist with `:provider` and `:command` but no `:operation`.
The parse-incomplete branch at line 353 does the same. Pressing `a`
(add to scope) on either of these violation kinds reaches the writer
with `:operation nil` and now errors loudly.

## Implementation

In `config/gptel/scope/scope-expansion.org`:

1. At the top of `--add-to-scope` (and `--add-path-to-scope` and
   `--add-bash-to-scope` if they're entered directly), check
   `(plist-get violation :operation)`. If nil, branch on
   `(plist-get violation :validation-type)`:

   - `:cloud-auth` → prompt "add provider `<provider>` to allow-list?"
     The action writes to `:GPTEL_SCOPE_CLOUD_PROVIDERS:` rather than
     a path bucket. (`:provider` is on the violation plist.)
   - `:parse-incomplete` → user-error: "this command could not be
     parsed; review and edit it manually". Surface the bash command
     for inspection. No drawer write.
   - any other `:validation-type` with nil operation → user-error:
     "no operation associated with this violation; cannot add to
     scope". Suggest allow-once.

2. The transient menu (`jf/gptel-scope-expansion-menu`) should hide
   the `a` action when `:operation` is nil and `:validation-type` is
   `:parse-incomplete` (no useful path forward). For
   `:cloud-auth` the menu re-labels `a` as "add provider to
   allow-list" so the user knows what they're consenting to.

3. Add a buttercup spec asserting:
   - `:cloud-auth` violation → `add-to-scope` writes to
     `:GPTEL_SCOPE_CLOUD_PROVIDERS:`, not to a path bucket.
   - `:parse-incomplete` violation → `add-to-scope` errors with a
     useful message that does not reach the writer.
   - `:bash` violation with `:operation nil` (defensive, shouldn't
     happen but worth pinning) → user-error.

## Verification

```bash
./bin/run-tests.sh -d config/gptel/scope/test/expansion
```

The new spec runs alongside `test-expansion-menu-spec.el` (or wherever
the existing menu specs live).
