---
name: disposition-read-metadata-bucket
description: "User-decision task (finding-10A). Choose how to handle :read-metadata violations — new READ_METADATA bucket, allow-once-only at action layer, or keep current READ collapse."
change: gptel-scope-in-org-properties
status: blocked
relations:
  - blocked-by:rewire-expansion-writer
  - discovered-from:implement-drawer-writer
---

## Cites register entries

- `register/vocabulary/operation-to-drawer-key` — current entry has `:read-metadata → GPTEL_SCOPE_READ` with an `open_question` annotation pointing at this task.
- `register/shape/violation-info` — `:operation :read-metadata` arrives on the violation plist.

## Background

Architect audit finding `arch-cycle-1777460733-10` (sub-case A): when the
LLM runs `[ -f /etc/passwd ]`, `which python`, or `dirname /etc/shadow`
and gets denied, the expansion UI offers "add to scope". The current
writer collapses `:read-metadata → READ` and writes the file path to
`GPTEL_SCOPE_READ`. The next `cat /etc/passwd` then succeeds without
re-prompting.

The historical `:read-metadata`-into-`paths.write` incident the user
referenced *failed safe* (denied later reads). The current direction
*fails open* — a metadata stat grants persistent content read access,
which is plausibly not what the user expected when they pressed `a` on
a `[ -f file ]` violation.

## Three resolution paths

1. **Add `GPTEL_SCOPE_READ_METADATA` as a 6th list-shaped drawer key.**
   Validator treats `:read-metadata` as requiring `paths.read-metadata`
   OR `paths.read` OR `paths.write`. Preserves a permission hierarchy
   (metadata < content read < write).

2. **Refuse add-to-scope on metadata violations; offer allow-once
   only.** Expansion UI detects `:operation :read-metadata` and removes
   the "add to scope" option from the menu, leaving "allow once" /
   "deny" / "edit manually". Metadata reads are typically transient;
   persistent grants are rarely the right outcome.

3. **Keep current READ collapse.** Document the tradeoff
   (`:read-metadata` grants persistent content reads); accept that
   users who want narrower grants must use `--add-bash-to-scope` with
   the bash-specific syntax.

## Decision required

Which path? Routes to the user via the integrate→plan handshake
(`asks_for_user_open` at next plan).

## Implementation work (after decision)

- **Path 1**: extend `register/vocabulary/drawer-key-set` (8th key);
  extend `register/vocabulary/operation-to-drawer-key`; update
  `--map-operation-to-drawer-key` and `--load-from-buffer` and
  `--render-drawer-text` and `--apply-to-drawer`; add a buttercup
  spec for the new key's round-trip.
- **Path 2**: edit `scope-expansion.org` `--add-to-scope` action to
  filter the menu when `:operation` is `:read-metadata`; remove the
  `:read-metadata` arm from `--map-operation-to-drawer-key` (with a
  comment that `:read-metadata` is action-layer-only); update the
  register entry's `members` to mark `:read-metadata` as
  "no-collapse".
- **Path 3**: no code change; update the register entry's
  `open_question` to `accepted-tradeoff` with rationale.

## Verification

After implementation, run `./bin/run-tests.sh -d config/gptel/scope`.
Specific spec depending on path chosen.
