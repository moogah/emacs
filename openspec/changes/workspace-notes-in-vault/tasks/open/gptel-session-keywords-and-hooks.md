---
name: gptel-session-keywords-and-hooks
description: Emit #+title and #+filetags on new sessions, add jf/gptel-session-created-hook under an error guard, and add a pure-gptel "new topic with ID" command.
change: workspace-notes-in-vault
status: ready
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

(empty — implementor appends during execution)

## Discoveries

(empty — implementor appends structured discoveries; see task-body template)
