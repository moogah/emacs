---
name: gptel-session-keywords-and-hooks
description: Emit #+title and #+filetags on new sessions, add jf/gptel-session-created-hook under an error guard, and add a pure-gptel "new topic with ID" command.
change: workspace-notes-in-vault
status: done
reviewed_at: 2026-09-14T00:50:00Z
findings_path: .orchestrator/cycles/cycle-1789334430/reviews/gptel-session-keywords-and-hooks.md
inline_fix_commit: 67362f29
merge_commit: 43d3414a
merged_at: 2026-09-14T00:05:00Z
task_class: contract
on_critical_path: true
cycle: cycle-1789334430
relations:
  - enables:gptel-branch-identity
  - enables:int-gptel-org-graph
cites_register_entries:
  - register/shape/session-created-hook-payload
  - register/shape/branch-created-hook-payload
  - register/boundary/session-lifecycle-hooks
  - register/shape/session-file-head
  - register/invariant/signature-anchored-to-point-min-drawer
created_at: 2026-09-13T00:00:00Z
planned_at: 2026-09-13T21:20:30Z
---

## Files to modify
- config/gptel/sessions/commands.org (modify; tangle)
- config/gptel/gptel.org (modify; tangle — "GPTEL Org Topic Workflow" section)
- config/gptel/sessions/test/commands/*-spec.el (modify/add)
- config/gptel/chat/*.org (read only — confirm the save-path drawer materialiser and the magic-mode signature ignore keywords after the drawer)

## Implementation steps
1. `jf/gptel--initial-session-body` (commands.org): after the rendered
   drawer text emit `#+title: <session-id>\n#+filetags: :gptel_session:\n`,
   then the blank line and `#+begin_user\n\n#+end_user\n`. Keep the
   `:ID:` stamping (`jf/gptel--stamp-session-org-id`) as is — it operates on
   the drawer at point-min and is unaffected by keywords after it.
2. Confirm (read `config/gptel/chat/mode.org` save path and the
   `magic-mode-alist` signature in `commands.org` ~:250-300) that neither
   rewrites or matches on lines after the drawer's `:END:`. Add a spec that
   saves a chat buffer and asserts the two keywords survive byte-identically.
3. Define `(defvar jf/gptel-session-created-hook nil ...)` and
   `(defvar jf/gptel-branch-created-hook nil ...)` in `commands.org` (the
   branch hook is *run* in `gptel-branch-identity`; declaring both here keeps
   the public surface in one place). At the end of
   `jf/gptel--create-session-core`, after the sibling system-prompt file is
   written and the registry entry exists, run the session hook via
   `run-hook-wrapped` with a `condition-case` per function that logs through
   `jf/gptel--log 'error` and returns nil (so all functions run). Payload
   plist: `:session-file :session-id :branch :session-dir :project-root`
   (`project-root` is the existing optional argument; nil when absent).
4. Topic command (OQ-A, approved): in `config/gptel/gptel.org` under the
   "GPTEL Org Topic Workflow" `use-package gptel-org-utils` block (or a
   sibling block), add `jf/gptel-org-new-topic-with-id (topic)`: call
   `gptel-org-utils-new-topic`, then move to the new topic heading and
   `org-id-get-create`. Pure org-id; no org-graph symbol. Bind nothing yet
   (menu binding is out of scope).
5. Specs: keywords present in the created file in the right order; hook
   fires once with the documented keys after the file exists on disk (spy
   asserts `file-exists-p` inside the hook); a hook function that signals
   does not prevent creation, registration, or a second hook function;
   `jf/gptel-org-new-topic-with-id` leaves a heading with `GPTEL_TOPIC` and
   an `:ID:`.

## Design rationale
Indexed sessions need distinguishable titles (vulpea falls back to the
basename, so every session would be "session"), and an index needs a way
to recognise session files without the sessions package naming any
indexer — a filetag gptel owns (`gptel_session`) does that. The hooks are
the package's only outward-facing seam: other packages react to session
creation without gptel consulting them. The error guard keeps a
misbehaving subscriber from breaking session creation. The topic command
makes "mark a topic" and "fork a conversation" the same gesture, since
branching context is already on and an ID'd heading becomes a note.

## Design pattern
`run-hook-wrapped` + `condition-case` + `jf/gptel--log`, as used for other
guarded callbacks in the sessions modules. Keywords are emitted by the
same body builder that renders the drawer so ordering is fixed in one
function.

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/commands.org` and `config/gptel/gptel.org` validate.
- `./bin/run-tests.sh -d config/gptel/sessions` — green.
- `grep -n 'gptel_session\|session-created-hook' config/gptel/sessions/commands.el` → keyword emission and hook run present.
- One un-stubbed scenario: real `jf/gptel--create-session-core` in a temp root with a hook function that asserts `file-exists-p` on `:session-file` and reads the drawer `:ID:` — no stub on the creation path (stub-bounded regression pattern).
- `grep -n 'branch-created-hook' config/gptel/sessions/*.el` → declaration only; no `run-hook` call (the runner lands in gptel-branch-identity, cycle 2 — record as inert).

## Context
design.md § D5 (Hooks, Keywords bullets); § Open Questions OQ-A (approved for inclusion)
specs/sessions-persistence/spec.md § 'Session lifecycle hooks', 'Session file title and filetag', 'Session creation'

## Cited register entries

Register entries are reference material to pressure-test, not authority to defer to. Speculated entries carry explicit licence to push back via Observations / Discoveries.

- `register/shape/session-created-hook-payload` — NEW, speculated. Keys are the minimum; if creation has a value the integration layer will need (e.g. the sibling system-prompt path) add it and record it as a Discovery rather than a silent extension.
- `register/shape/branch-created-hook-payload` — NEW, speculated; DECLARED this cycle, RUN in cycle 2 (gptel-branch-identity). The variable is inert until then — say so in its docstring and in a spec comment so a green run is not mistaken for coverage (green-on-empty lineage meta-discovery).
- `register/boundary/session-lifecycle-hooks` — NEW, speculated. Pressure-test the ordering: hook runs after the file, sibling file and registry entry exist and BEFORE the buffer is displayed. Your spec must assert `file-exists-p` inside the hook against the real creation path — do not stub `jf/gptel--create-session-core` (stub-bounded regression pattern).
- `register/shape/session-file-head` — NEW, speculated. Pins the head order drawer → (EDGES) → #+title → #+filetags → blank → first block. Verify the chat-mode save path and the magic-mode signature leave lines after `:END:` alone; if either rewrites them, that is a Discovery.
- `register/invariant/signature-anchored-to-point-min-drawer` — Confirmed invariant; keywords after the drawer must not move it off point-min.

## Out-of-scope

See sibling tasks in this change; do not fold in cycle-2 work (gptel-branch-identity (runs the branch hook), int-gptel-org-graph).

## Plan-phase notes (architect forward-mode, cycle-1789334430)

- The registry entry is NOT created inside `jf/gptel--create-session-core`: `jf/gptel--register-session` runs from the chat-mode binder (commands.org ~:307) at `find-file` time, after the core returns. Step 3's "after ... the registry entry exists" is therefore unsatisfiable inside the core. Run the hook at the end of the core (file + sibling file exist, org ID stamped); the spec's "session is created, opened, and registered normally" holds because the guard never blocks the core's return. Record the ordering you land in Discoveries against `register/boundary/session-lifecycle-hooks`.
- PersistentAgent composes its own `session.org` in `config/gptel/tools/persistent-agent.org` and bypasses the core, so agent sessions would get neither the keywords nor the hook. Decide explicitly: either route agent creation through the same body builder (preferred if cheap) or document agent sessions as a second producer variant of `register/shape/session-file-head` in Discoveries. Do not leave it implicit.
- `jf/gptel--initial-session-body` currently takes no argument; emitting `#+title: <session-id>` needs the id passed in (or composition in the core). Either is fine; keep the keyword emission in one function.

## Observations

- Registry-entry timing (plan note 1) confirmed empirically: inside the
  hook the session file, its `:ID:`, and (when the preset has one) the
  sibling system-prompt file exist on disk, while `get-file-buffer`
  and the registry lookup are both nil. The hook runs at the end of the
  core, before the caller's `find-file`; design D5's "after ... the
  registry entry exist[s]" and the delta spec's "registered normally"
  are met only via the guard never blocking the core's return. Both
  texts still say "after the registry entry"; see disc-1.
- PersistentAgent (plan note 2) was NOT routed through the body
  builder. Reasons: `persistent-agent.org` is edited by sibling task
  gptel-hidden-agents this cycle (merge risk); the filetag exists so an
  index can *recognise* session files, and agent transcripts live under
  the hidden agents directory precisely so an index skips them — tagging
  them `gptel_session` would work against that; the register already
  records that the hook does not fire for agents. Recorded as a second
  producer variant (disc-2). If a later cycle wants agent titles, split
  the keyword half of `jf/gptel--initial-session-body` into a shared
  helper both composers call.
- Body-builder signature (plan note 3): `jf/gptel--initial-session-body`
  now takes a required `session-id`; the core is its only caller. The
  keywords are emitted at that one site.
- Topic-command spec lives at `config/gptel/test/org-topic-with-id-spec.el`,
  not under `sessions/test/commands/` as the file list suggests: the
  command is defined in `gptel.org` (loaded by `init.el` in the test
  Emacs), and that directory already holds the other gptel.org-level
  specs. Run it with `./bin/run-tests.sh -d config/gptel/test` (49
  specs, 0 failed). The scoped sessions run does not include it.
- Spec-authoring hazard worth keeping: the guard under test swallows
  every `error`, and a failing buttercup `expect` IS an error. Hook
  functions in the new spec therefore only record what they observe and
  every assertion runs after the core returns. An `expect` placed inside
  a hook function would be logged and silently pass.
- Latent (pre-existing, chat subsystem, out of scope): in the batch test
  Emacs, the first real `save-buffer` of a freshly created session
  rewrote the drawer WITHOUT `:GPTEL_PRESET: executor` (and without the
  scope keys) — `gptel-chat--write-config-drawer` deletes the key when
  `gptel--preset` is nil at save time, and the chat-mode preset
  application did not set it in that environment. Upstream
  `gptel--apply-preset` sets `gptel--preset`, so interactively it is
  probably fine, but worth a look from whoever next touches
  `chat/menu.org`. The keywords were unaffected either way; the
  save-path spec asserts on the identity keys instead.
- The delta-spec scenario "User-edited title survives saves" says "sends
  a turn" between the saves; the send path needs a backend, so the spec
  substitutes a body edit inside the user block between two real
  `save-buffer` calls (the drawer writer runs both times).
- Stale prose left in place on purpose: "Create Persistent Session →
  Workflow" item 5 in `commands.org` still says the drawer is "followed
  directly by an empty #+begin_user block". That section is being edited
  by gptel-single-root this cycle; a one-line prose fix after merge.
  Similarly `gptel.org` still `jf/load-module`s
  `config/gptel/sessions/workspace-integration.el`, which
  gptel-single-root deletes — the sibling should drop that load line.
- `mode-activation-spec.el` (chat) did not gain the "keywords + EDGES
  drawer present" positive case the invariant's status note asks for
  (chat tests are outside this task's file list). The equivalent
  signature cases live in `session-keywords-and-hooks-spec.el` ("the
  signature stays anchored..."), and the end-to-end open of a
  keyword-bearing file into `gptel-chat-mode` is exercised by the
  save-path scenario there.
- `jf/gptel--run-lifecycle-hook` is generic (takes the hook symbol) so
  cycle-2 gptel-branch-identity can reuse it for the branch hook.
  `branching.el` loads before `commands.el`; the call resolves at
  runtime, but `branching.org` should `declare-function` it.
- Scoped runs: sessions 160 → 176 specs, 0 failed; `config/gptel/test`
  44 → 49 specs, 0 failed. Both `.org` files tangle and pass
  `check-parens`.

## Discoveries

- discovery_id: disc-gptel-session-keywords-and-hooks-1
  class: spec-signal
  description: |
    Design D5 ("runs ... after the sibling file and the registry entry
    exist") and the delta spec's "registered normally" wording describe
    an ordering the core cannot provide: `jf/gptel--register-session`
    runs from the chat-mode binder at the caller's find-file, after the
    core returns. Verified inside the hook on the real creation path:
    session.org exists with :ID: and identity keys, the sibling
    system-prompt file exists, `get-file-buffer` is nil, the registry
    lookup is nil. Landed order = register stages 1-6 then 7 (caller).
  affected_register_entry: register/boundary/session-lifecycle-hooks
  recommendation: |
    Keep the entry's status_note CORRECTION as the authoritative order
    (hook before display and before registration). Reword design D5
    "Hooks" bullet and the delta spec requirement to "after the session
    file, its org :ID:, and any sibling file exist on disk, and before
    the buffer is displayed or registered".

- discovery_id: disc-gptel-session-keywords-and-hooks-2
  class: shape-fragmentation
  description: |
    session-file-head now has two deliberate producer variants: the
    core (regions 1, 3-6 — drawer, title, filetags, blank, empty user
    block) and PersistentAgent (regions 1 + populated 6, no keywords,
    no hook). Not unified this cycle: persistent-agent.org is owned by
    sibling task gptel-hidden-agents, and the `gptel_session` filetag
    exists for index recognition while agent transcripts are placed
    under a hidden directory specifically to be skipped by an index.
  affected_register_entry: register/shape/session-file-head
  recommendation: |
    Record the agent variant in `producers` as intentional (regions 1 +
    6 only; no title/filetags; hook not fired) rather than as drift. If
    agent titles are ever wanted, extract the keyword half of
    `jf/gptel--initial-session-body` into a shared
    `jf/gptel--session-head-keywords (session-id)` both composers call,
    and decide then whether agents get the filetag.

- discovery_id: disc-gptel-session-keywords-and-hooks-3
  class: interface-drift
  description: |
    `jf/gptel--initial-session-body` now takes a required SESSION-ID and
    returns regions 3-6; the entry's producers text still says "takes
    no argument and emits only region 6". Sole caller is the core.
  affected_register_entry: register/shape/session-file-head
  recommendation: |
    Update producers: `jf/gptel--initial-session-body (session-id)` →
    "#+title: <id>\n#+filetags: :gptel_session:\n\n#+begin_user\n\n#+end_user\n";
    the core concatenates drawer-text + that string with no separator.

- discovery_id: disc-gptel-session-keywords-and-hooks-4
  class: deviation
  description: |
    Payload built once at the end of the core with the required keys
    plus optional `:branch-dir` (already in the return plist). To make
    "absolute path" structural rather than validated, the core now
    canonicalizes SESSION-DIR once at entry via `expand-file-name`
    (same pattern as PROJECT-ROOT); the return plist's `:session-dir`
    is therefore canonical too. No-op for every existing caller (all
    pass `expand-file-name` output) and for every existing spec. No
    `:sibling-file` key was added: nothing consumes it, and
    `:session-file` + the drawer's `GPTEL_SYSTEM_PROMPT_FILE` locate it.
  affected_register_entry: register/shape/session-created-hook-payload
  recommendation: |
    Move `:branch-dir` from optional to "always present". Note the
    entry-canonicalization of SESSION-DIR under producers so the
    validator's `file-name-absolute-p` checks are known to hold by
    construction. Keep `:preset`/sibling path as future/not built.

- discovery_id: disc-gptel-session-keywords-and-hooks-5
  class: deviation
  description: |
    The guard is a single generic runner,
    `jf/gptel--run-lifecycle-hook (hook payload)`, rather than an inline
    `run-hook-wrapped` form per site. It logs
    "<hook-symbol>: function <fn> failed: <msg>" at level `error` via
    `jf/gptel--log`, not the entry's literal
    "session-created hook %S failed: %s" / "branch-created hook %S
    failed" strings. Semantics are as specified: per-function
    `condition-case`, wrapper always returns nil, every function runs,
    runner returns nil.
  affected_register_entry: register/boundary/session-lifecycle-hooks
  recommendation: |
    Update stage 6 and B4 notes to name `jf/gptel--run-lifecycle-hook`
    and its message format. Cycle-2 gptel-branch-identity should call
    `(jf/gptel--run-lifecycle-hook 'jf/gptel-branch-created-hook payload)`
    as the last step of `jf/gptel--create-branch-session` (with a
    `declare-function`, since branching.el loads before commands.el).
