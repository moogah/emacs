---
name: gptel-branch-identity
description: Give each branch a fresh file-level org ID and title, strip heading-level IDs from the copied history, and run jf/gptel-branch-created-hook after the rewrite.
change: workspace-notes-in-vault
status: blocked
relations:
  - blocked-by:gptel-session-keywords-and-hooks
  - blocked-by:gptel-hidden-agents
  - enables:int-gptel-org-graph
---

## Files to modify
- config/gptel/sessions/branching.org (modify; tangle — `jf/gptel--rewrite-branch-identity-keys` ~:313, `jf/gptel--create-branch-session` ~:348, `jf/gptel--copy-truncated-context` ~:192)
- config/gptel/sessions/test/branching/*-spec.el (add)

## Implementation steps
1. In `jf/gptel--rewrite-branch-identity-keys` (operates on the new file in a
   temp buffer): after setting `GPTEL_SESSION_ID` / `GPTEL_BRANCH`, replace
   the drawer's `:ID:` value with `(org-id-new)`; register it with
   `org-id-add-location` under `org-id-overriding-file-name` bound to the
   branch file (same trick as `jf/gptel--stamp-session-org-id`). If the
   parent drawer had no `:ID:` (pre-ID-stamping sessions), add one. Then
   rewrite the `#+title:` line (first `^#\+title:` after the drawer) to
   `<session-id> (<branch-name>)`; add it if absent. Leave `#+filetags:`
   and any other file-level drawer (e.g. `EDGES`) byte-identical.
2. Add `jf/gptel--strip-heading-ids` and call it from
   `jf/gptel--create-branch-session` after the truncated copy, before the
   identity rewrite: in a temp buffer, skip the file-level drawer at
   `point-min`, then for every subsequent `org-property-drawer-re` match
   delete the `^:ID:.*\n` line inside it; if the drawer is left with only
   `:PROPERTIES:`/`:END:`, delete the whole drawer. Regex is sufficient
   (drawers are line-anchored); do not run `org-mode` on the buffer.
3. Run `jf/gptel-branch-created-hook` (declared in
   `gptel-session-keywords-and-hooks`) at the end of
   `jf/gptel--create-branch-session` with
   `(:branch-file :parent-file :session-id :branch)`, same guarded runner as
   the session hook.
4. Specs (temp session tree): new branch `:ID:` ≠ parent's and parent
   unchanged; a copied heading with `:ID:` and `:GPTEL_TOPIC:` keeps the
   topic and loses the ID; a heading whose drawer held only `:ID:` loses the
   drawer; file-level `EDGES` drawer and `#+filetags:` survive; title is
   `<id> (<branch>)`; the hook sees the fresh ID and new branch name; no two
   `session.org` files under the temp root share an `:ID:` after two
   successive branches.

## Design rationale
Branches byte-copy the parent including its `:ID:` and any heading IDs.
vulpea's `notes` table has the ID as primary key with a plain insert, so
the second file carrying an ID fails to index and `org-id-locations`
flip-flops. A branch is therefore its own note with its own ID. Heading
IDs are stripped rather than regenerated because a topic node's identity
belongs to the branch that created it: links to it must keep resolving to
the parent's heading. The `part-of` edge the integration layer writes
lives in a file-level `EDGES` drawer directly after the property drawer,
so the verbatim copy carries the project association forward and the
rewrite step must not disturb it.

## Design pattern
Extend the existing identity-rewrite step (it already edits the drawer
head in a temp buffer and writes back with `write-region`); add the strip
as a separate pure function over the copied buffer, mirroring
`jf/gptel--copy-truncated-context`'s temp-buffer style.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/branching.org` validates.
- `./bin/run-tests.sh -d config/gptel/sessions/test/branching` — green.
- `grep -n 'org-id-new\|strip-heading-ids\|branch-created-hook' config/gptel/sessions/branching.el` → all three present.

## Context
design.md § D5 (Branch identity bullet), § D7 'Edge placement and prompt hygiene for sessions'
specs/sessions-branching/spec.md § 'Branch note identity', 'Branch-created hook', 'Branch creation model'
