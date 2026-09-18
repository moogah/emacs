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

## Cycle 1 updates (cycle-1789334430)

### Cited register entries
- `register/shape/branch-created-hook-payload`: speculated (annotated, unchanged). `jf/gptel-branch-created-hook` is DECLARED and INERT at commands.el:395-414 (defvar only; no runner anywhere in config/gptel/sessions/*.el); this task is its first producer. **Implication**: step 3's runner is `jf/gptel--run-lifecycle-hook HOOK PAYLOAD` (commands.el:416-437) — reuse it via `(declare-function jf/gptel--run-lifecycle-hook "commands")` in branching.org, because branching.el loads BEFORE commands.el; keys stay `:branch-file :parent-file :session-id :branch`. See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-branch-created-hook-payload.md`.
- `register/boundary/session-lifecycle-hooks`: speculated → reconciled. The session hook fires at the end of `jf/gptel--create-session-core` (commands.el:661) after file + file-level `:ID:` + sibling are on disk, with NO visiting buffer and NO registry entry. **Implication**: mirror that ordering — run the branch hook only after the identity rewrite and `write-region` (the fresh `:ID:` must be on disk: `org-graph/write-edge` refuses an ID-less node). See `.orchestrator/cycles/cycle-1789334430/reconciliations/boundary-session-lifecycle-hooks.md`.
- `register/shape/session-created-hook-payload`: speculated → reconciled; `:branch-dir` is ALWAYS present (commands.el:666). Not consumed here, but the branch payload should be equally self-sufficient (see ask-3). See `.orchestrator/cycles/cycle-1789334430/reconciliations/shape-session-created-hook-payload.md`.
- `register/vocabulary/session-tree-directory-names`: speculated → reconciled. `.agents` is single-sourced from `jf/gptel-session--agents-dir` (constants.el:48) via `jf/gptel--agents-dir-path` (filesystem.el:206-212); drawer-trace's session regex is composed from the constant. **Implication**: nothing to rename in branching.org; `jf/gptel--copy-branch-agents` already routes through the helper — keep it that way for ask-9. See `.orchestrator/cycles/cycle-1789334430/reconciliations/vocabulary-session-tree-directory-names.md`.
- `register/invariant/edge-drawer-immediately-after-property-drawer`: speculated → DIVERGENT (ask-cycle-1789334430-8; disposition-edge-drawer-meta-and-placement). The placement your Design rationale relies on is implemented as pinned, but vulpea parses that drawer as META. Under options (a)/(c) nothing changes here; under (b) the drawer moves after META but stays file-level, so step 1's "leave `EDGES` byte-identical" and the byte-copy argument still hold. Note the divergence in Observations. See `.orchestrator/cycles/cycle-1789334430/reconciliations/invariant-edge-drawer-immediately-after-property-drawer.md`.
- `register/invariant/signature-anchored-to-point-min-drawer` / `register/shape/session-file-head`: confirmed / reconciled — the `#+title:` line follows the drawer's `:END:` directly (session-org-creation-spec.el:97). **Implication**: step 1's "first `^#\+title:` after the drawer" is guaranteed to be adjacent for core-created sessions; PersistentAgent transcripts carry NO `#+title:` (second producer variant; ask-12), so "add it if absent" is a live path.

### User-resolved decisions
- `ask-cycle-1789334430-3` (open; default carried — `disposition-design-doc-corrections` option (a)): hook subscribers must NOT read `jf/gptel--session-registry` at hook time; the payload carries everything. **Implication for this task**: the branch payload must be self-sufficient (absolute `:branch-file` / `:parent-file`); do not defer any key to a registry lookup.
- `ask-cycle-1789334430-4` (open; default carried — `disposition-cycle2-design-confirmations`): `jf/gptel--read-session-drawer-head` returns only `:GPTEL_*:` keys, so `int-gptel-org-graph` must read the parent's file-level `:ID:` itself OR this task adds `:parent-id` to the branch payload. **Recommendation**: add `:parent-id` here — step 1 already has the parent drawer's `:ID:` in hand before replacing it; record the added key against `register/shape/branch-created-hook-payload`.
- `ask-cycle-1789334430-9` (open; `disposition-empty-agents-dir-scenario`): the "Empty agents directory" scenario says a child SHALL have an empty `.agents/` when the parent has none, but `jf/gptel--copy-branch-agents` only mkdirs when the parent's exists. Default (a): relax the scenario and assert the listing, not the directory. **Implication for this task**: fold the chosen one-file tweak (branching.org and hidden-agents-dir-spec.el or the branching spec) into this task rather than a separate one.

### Meta-discoveries
- `stub-bounded-regression`: keep step 4's "no two `session.org` under the temp root share an `:ID:` after two successive branches" as an UN-STUBBED scenario over real files (real `jf/gptel--create-branch-session`, real `org-id-add-location`).
