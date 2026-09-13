---
name: gptel-session-keywords-and-hooks
description: Emit #+title and #+filetags on new sessions, add jf/gptel-session-created-hook under an error guard, and add a pure-gptel "new topic with ID" command.
change: workspace-notes-in-vault
status: ready
relations:
  - enables:gptel-branch-identity
  - enables:int-gptel-org-graph
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

## Context
design.md § D5 (Hooks, Keywords bullets); § Open Questions OQ-A (approved for inclusion)
specs/sessions-persistence/spec.md § 'Session lifecycle hooks', 'Session file title and filetag', 'Session creation'
