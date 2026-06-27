---
name: auto-id-scaffold
description: Extend workspace scaffold and gptel session creation to auto-assign org IDs so home.org and session files participate in the vulpea index.
change: org-graph-spike
status: ready
relations:
  - blocked-by:install-packages
cites_register_entries:
  - register/invariant/indexable-requires-id
---

## Files to modify
- `config/workspaces/scaffold.org` (modify) — add `org-id-get-create` to the
  `home.org` write stage
- `config/gptel/sessions/commands.org` (modify) — add `org-id-get-create` in
  `jf/gptel--create-session-core` after the session file is created

## Implementation steps
1. In the workspace scaffold pipeline (the stage that writes the `home.org`
   skeleton, `config/workspaces/scaffold.org` ~line 152-183), after writing
   the skeleton, open the file and call `org-id-get-create` so `home.org`
   gets a stable `:ID:`. Ensure the ID is written before the initial `git
   add . && commit` so it is captured in the first commit.
2. In `jf/gptel--create-session-core` (session org writer,
   `config/gptel/sessions/commands.el:326`), after the session file exists
   and its property drawer is written, call `org-id-get-create` on the
   session buffer so `sessions/*.org` files are indexable nodes.
3. Make both additive and idempotent: `org-id-get-create` is a no-op if an
   `:ID:` already exists, so re-running scaffold or re-saving a session does
   not churn IDs.
4. Do NOT change any other scaffold/session behavior, naming, or the
   `GPTEL_WORK_ROOT` drawer. This task only adds IDs.
5. Tangle both org files and validate: `./bin/tangle-org.sh
   config/workspaces/scaffold.org` and `./bin/tangle-org.sh
   config/gptel/sessions/commands.org`.

## Design rationale
RE-2a: vulpea only indexes notes carrying an `:ID:`
(`vulpea-db-extract.el:475`). Workspace `home.org` and `sessions/*.org` are
scaffolded without IDs, so without this they are invisible to the index and
cannot be `id:` link targets. Auto-assigning IDs at birth is the minimal,
purely-additive way to make workspace homes and sessions first-class nodes in
the graph. This is the one place org-graph reaches into the workspaces /
gptel-sessions code.

## Design pattern
Match the existing literate style of `scaffold.org` and `commands.org`. The
change is a single `org-id-get-create` call at the right point — keep the
babel block small and well-commented as to WHY (RE-2a).

## Verification
- `./bin/run-tests.sh -d config/workspaces` and `-d config/gptel/sessions`
  stay green (existing scaffold/session specs unaffected; assertions about
  the skeleton content still hold since IDs are additive).
- Manual: create a workspace, confirm `home.org` and a new session file each
  contain an `:ID:` property and appear in `(vulpea-db-query)` after a sync.
- Re-running scaffold against an existing home does not change the existing
  IDs.

## Context
design.md § Re-evaluation (RE-2a); config/workspaces/scaffold.org;
config/gptel/sessions/commands.el (jf/gptel--create-session-core).

## Orchestrator brief addenda (cycle-1782551613)

From the foundation Architect audit; cited register entry in `interfaces.org`.

- **`register/invariant/indexable-requires-id` is your acceptance contract.**
  ID assignment MUST be additive and idempotent: a file that already has an
  `:ID:` is left unchanged. Acceptance gate: existing `config/workspaces` and
  `config/gptel/sessions` specs stay green, AND a new assertion that a re-run
  (re-scaffold / re-save) leaves an existing `:ID:` untouched (no churn).

- **This is the ONLY batch task that edits code outside `config/org-graph/`**
  (it touches `config/workspaces/scaffold.org` and
  `config/gptel/sessions/commands.org`). Confirm zero behavioural change beyond
  the ID stamping — no change to scaffold naming, skeleton content, or the
  `GPTEL_WORK_ROOT` drawer. The reviewer will check exactly this.

- **Literate discipline.** Edit the `.org` sources and tangle both
  (`./bin/tangle-org.sh config/workspaces/scaffold.org` and
  `… config/gptel/sessions/commands.org`); commit `.org` + `.el` together.

## Observations

- Implemented file-level `org-id-get-create` stamping in both writers,
  additively and idempotently, with all three target suites green:
  `config/workspaces` 356/0, `config/gptel/sessions` 160/0,
  `config/org-graph` 22/0 (unperturbed).
- **home.org** (`config/workspaces/scaffold.org`): stamping is composed
  in a throwaway `org-mode` buffer and written in the *single* existing
  `with-temp-file` of `workspace--scaffold-write-home-org` (new helper
  `workspace--scaffold-home-org-content`). The file-level `:ID:` drawer
  necessarily precedes `#+TITLE:` (a file-level property drawer must be
  the first element). The stage-3 `(unless (file-exists-p path) ...)`
  guard is retained, so the `:ID:` is captured in the first commit on
  fresh creation and re-runs never touch an existing home.
- **session.org** (`config/gptel/sessions/commands.org`): new helper
  `jf/gptel--stamp-session-org-id` stamps the composed content before
  the existing single `write-region`. The `:ID:` lands *inside* the
  existing point-min `:PROPERTIES:` drawer, so the drawer-first-element
  and drawer->body adjacency invariants are preserved byte-for-byte at
  the head/tail. The caller-supplied `INITIAL-CONTENT` override is left
  verbatim (NOT stamped) to honour the override contract.
- New assertions added: scaffold-spec.el (`:ID:` present, captured in
  HEAD:home.org, idempotent across re-run); session-org-creation-spec.el
  (`:ID:` inside drawer, override not stamped, helper idempotent).
- `(require 'org-id)` added to both modules.

## Discoveries

- class: register-pushback
  affected_register_entry: register/invariant/indexable-requires-id
  detail: |
    The core invariant (ID assignment is additive + idempotent; an
    existing :ID: is left unchanged) HELD and is enforced by new tests.
    BUT the entry's enforcement note "existing scaffold/session specs
    stay green (IDs additive)" was only partly right. "Additive" is true
    at the org-element level (a property/drawer is added, never user
    content removed) but it is NOT byte-prefix-stable for home.org: a
    file-level :ID: drawer MUST be the document's first element, so it
    lands ABOVE #+TITLE:. This broke one over-specified scaffold-spec
    assertion that pinned `\`#\+TITLE:` (start-of-STRING). I retargeted
    that assertion to the reader's actual contract (`^#\+TITLE:`,
    start-of-LINE — which the home-org reader already uses and is
    documented to tolerate content above the title) and added a positive
    assertion that the file begins with the :ID: drawer. Session specs
    needed no such change (the :ID: lands inside the pre-existing drawer,
    so head/tail anchors still match).
    RECOMMENDATION: speculated -> confirmed, with the wording refined:
    "additive at the org-element level (never removes user content); for
    home.org the :ID: drawer precedes #+TITLE:, which is safe because the
    title reader is start-of-line anchored."

- class: implementation-constraint
  affected_register_entry: register/invariant/indexable-requires-id
  detail: |
    org-id-get-create insists on a file-visiting buffer (it errors in
    org-id-add-location when buffer-file-name is nil). To stamp in a
    throwaway buffer I bind `org-id-overriding-file-name` to the target
    path and use `delay-mode-hooks (org-mode)`. This was deliberate: a
    session.org carries the :GPTEL_*: drawer signature that
    magic-mode-alist uses to drive gptel-chat-mode, whose save hook
    rewrites the drawer on every save. Visiting/saving the file to stamp
    it would risk perturbing the drawer (see MEMORY: drawer corruption).
    Stamping the content string before the single write avoids any
    chat-mode activation or save-hook entirely.

- class: deviation
  affected_register_entry: register/invariant/indexable-requires-id
  detail: |
    The task/brief described a pipeline-level stamp ("after writing the
    home.org skeleton ... open the file and call org-id-get-create";
    "org-id-get-create on the session buffer"). I deviated to
    compose-and-stamp-before-write in both cases. Rationale: (1) sessions
    have no live buffer in create-core and opening one triggers the
    chat-mode/save-hook risk above; (2) for home.org it preserves the
    single-with-temp-file write and the
    home-org-user-authored-after-creation "sole writer" invariant (no
    second toucher of home.org), and is the most minimal behavioural
    change (only effect: the file now carries an :ID: drawer). Net
    on-disk result is identical to the brief's intent.

- class: scope-nuance
  affected_register_entry: register/invariant/indexable-requires-id
  detail: |
    Because home.org stamping lives behind the stage-3
    `(unless (file-exists-p path) ...)` guard, the "anchor an existing
    repo whose home.org already exists" sub-case does NOT stamp that
    pre-existing user file (it has no :ID: and stays unindexed until the
    user or another process adds one). This is the conservative reading
    of home-org-user-authored-after-creation (never modify a user's
    existing home.org). Freshly-scaffolded homes — the common case — are
    always stamped. Flagging in case the spike wants anchored homes
    indexed too (would require relaxing the guard for additive stamping).
