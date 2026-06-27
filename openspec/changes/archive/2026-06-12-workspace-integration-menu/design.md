# Design: workspace integration menu

## Context

Two problems established in the proposal converge on one missing primitive:
the workspaces package has no discoverable menu (12 commands on a bare `C-x w`
keymap), and its scaffold writes an inert `<date>-initial.org` gptel-session
stub that cannot be fixed in place because the directionality contract
(`register/boundary/gptel-sessions-workspace-consult`) forbids
`config/workspaces/*.el` from naming any `gptel-sessions-*` symbol.

The specs define the answer as a published **integration registry** (the
`workspace-integrations` capability) plus a **context-aware transient** (a
`workspaces` modification). This document fixes the concrete shapes: the
registry data structure, the registration entry point, the anchor-payload and
result protocol representations, the dispatch wiring into the existing
`workspace--new-*` entry points, the transient's three-state structure, and
the gptel consumer that proves it end-to-end.

Current relevant surfaces (verified against the tree):
- `workspace--new-default-path` / `workspace--new-anchor-existing` (`tabs.org`) —
  the creation entry points; both scaffold-then-`puthash`-then-tab.
- `workspace--current-name` (`tabs.org:86`), `workspace--registry`
  (`tabs.org:36`), `workspace--home` (`data-model.org:278`),
  `workspace--broken-p` (`data-model.org:397`), `workspace--sessions-dir`
  (`data-model.org:472`).
- `workspace--scaffold-initial-session` (`scaffold.org:132`) — the stub to delete.
- gptel session builder: `jf/gptel--create-session-core (session-id
  session-dir preset-name &optional initial-content worktree-paths
  project-root parent-session-id)` (`commands.org:544`); id generator
  `jf/gptel--generate-session-id` (`filesystem.org:115`).
- Transient idioms: dynamic suffix population via `:setup-children` +
  `transient-parse-suffixes` reading a registry (`skills-transient.org:291-351`);
  per-group `:if` predicates for context-gating; `transient-append-suffix` for
  post-definition injection (`skills-transient.org:377-393`).

## Goals / Non-Goals

**Goals:**
- A registry + registration entry point in `config/workspaces/` that names no
  consumer, runs in-order, and replaces on re-registration.
- An anchor-payload push contract and an `ok`/`skipped`/`failed` result
  protocol with per-handler error isolation and visible-but-non-fatal failure.
- Creation-time dispatch wired into all three birth contexts, after
  registration, without weakening the existing "scaffold failure → no registry
  entry" semantics.
- A context-aware `workspace-menu` transient (three states) that is the
  creation front-door and hosts the registry-driven Integrations group.
- gptel-session as the first real integration (`:on-create` + `:menu`),
  deleting the broken stub.

**Non-Goals:**
- The git-worktree integration (named as the validating second client; not built).
- The β interactive-creation flow (toggling/parameterizing integrations before
  birth). The payload is kept extensible so it can be added later.
- Including integration-created files in the `Initial workspace` commit
  (deliberately deferred — see Decision 4).
- Lifecycle hooks beyond creation (`:on-delete`/`:on-purge`/`:on-switch`).

## Decisions

### Decision 1 — Registry as an ordered alist; registration via `cl-defun`

`workspace--integrations` is an **alist** `((ID . PLIST) ...)`, not a hash
table, because the spec requires in-registration-order dispatch and stable
menu ordering, which a hash table does not guarantee.

Registration is a plain function (not a macro), so it is trivially callable
from tests and from `with-eval-after-load` blocks:

```elisp
(cl-defun workspace-register-integration (id &key label on-create menu)
  "Register a workspace integration under ID (a symbol).
LABEL is a human string.  ON-CREATE is an optional handler called at
workspace creation with the anchor payload.  MENU is an optional
(KEY . COMMAND) cons for on-demand invocation.  At least one of
ON-CREATE / MENU is required.  Re-registering ID replaces it in place,
preserving order."
  ...)
```

*Alternative considered:* a `workspace-define-integration` macro mirroring
`transient-define-*`. Rejected — it buys nicer syntax but obstructs
programmatic registration in Buttercup specs and adds expansion complexity for
no behavioral gain. A function keeps the registry pure data.

### Decision 2 — Anchor payload is a flat, extensible plist

```elisp
(list :name NAME :home HOME :sessions-dir (workspace--sessions-dir HOME)
      :context CONTEXT)   ; CONTEXT ∈ {fresh anchored-scaffolded anchored-existing}
```

Built by one private constructor `workspace--integration-payload` so both the
creation-dispatch site and the transient's menu commands produce identical
shapes. A plist (vs a `cl-defstruct`) keeps it open for extension (β's
`:integration-choices`) without a struct redefinition, and matches the repo's
plist-heavy data-model idiom.

`:sessions-dir` is derived via the existing `workspace--sessions-dir` helper,
so the payload never duplicates path logic.

### Decision 3 — Result protocol: return-value convention + error-guard fallback

A handler reports its outcome by **return value**:
- `ok` — performed its action,
- `skipped` — deliberately no-op,
- `(failed . REASON)` — tried and failed, REASON a string.

The dispatcher wraps every call in `condition-case`; a *signalled* error is
normalized to `(failed . (error-message-string err))`. So handlers have two
ergonomic failure paths (return `failed`, or just let an error propagate), and
the dispatcher treats them identically.

```elisp
(defun workspace--run-one-integration (entry payload)
  (condition-case err
      (pcase (funcall (plist-get (cdr entry) :on-create) payload)
        ('ok                 (cons (car entry) 'ok))
        ('skipped            (cons (car entry) 'skipped))
        (`(failed . ,reason) (cons (car entry) (cons 'failed reason)))
        (other               (cons (car entry) 'ok)))   ; lenient: treat unknown as ok
    (error (cons (car entry) (cons 'failed (error-message-string err)))))) 
```

*Alternative considered:* failure-by-signal only (no `failed` return).
Rejected — forcing handlers to `error` to report a soft, expected failure
(e.g. "git absent") conflates expected and exceptional control flow and makes
"skipped vs failed" awkward.

### Decision 4 — Dispatch after registration; commit stays the last scaffold step

Confirmed with the user: **preserve the existing failure semantics.** The
`git commit "Initial workspace"` remains the final fail-fast scaffold step,
*before* `puthash`/tab creation. Integration dispatch runs *after* registration
(so the registry/tab exist and a failing integration cannot prevent a clean
workspace). Consequence, stated in the spec: a `fresh`-context session created
by gptel's `:on-create` lands after the commit and is therefore **untracked**
in the new repo. Accepted; "include integration output in the initial commit"
is a deferred follow-up.

Because the payload is *pushed* (Decision 2), dispatch does not need the
workspace to be current — but running it post-registration is free and keeps a
single, uniform dispatch site:

```elisp
(defun workspace--dispatch-create-integrations (name home context)
  "Run every registered :on-create handler once, in order.  Additive:
never signals; surfaces failures via `message'; returns the result alist."
  (let ((payload (workspace--integration-payload name home context))
        results)
    (dolist (entry workspace--integrations (nreverse results))
      (when (plist-get (cdr entry) :on-create)
        (let ((r (workspace--run-one-integration entry payload)))
          (push r results)
          (pcase (cdr r)
            (`(failed . ,reason)
             (message "workspace: integration %s failed: %s" (car r) reason))))))))
```

Wired at three call sites:
- `workspace--new-default-path` → `(... 'fresh)` after `puthash`+home-builder.
- `workspace--new-anchor-existing` → `'anchored-existing` (case 1),
  `'anchored-scaffolded` (case 2), `'fresh` (case 3), after registration.

This is a *sibling* of scaffold, not a scaffold stage — `workspace-scaffold`
itself is untouched except for deleting `workspace--scaffold-initial-session`
and its call.

### Decision 5 — `workspace-menu` transient: groups gated by context predicates

A single `transient-define-prefix workspace-menu`. Context is read once per
display from the current tab. Predicates:

```elisp
(defun workspace--menu-current ()            ; the current workspace plist or nil
  (when-let ((n (workspace--current-name))) (gethash n workspace--registry)))
(defun workspace--menu-healthy-p () (let ((w (workspace--menu-current))) (and w (not (workspace--broken-p w)))))
(defun workspace--menu-broken-p  () (let ((w (workspace--menu-current))) (and w (workspace--broken-p w))))
(defun workspace--menu-in-ws-p   () (and (workspace--menu-current) t))
```

Group gating:
- **Entry** (new / switch / restore): always shown.
- **Layouts + State** (switch/save/recent; save/revert): `:if workspace--menu-healthy-p`.
- **Manage / Recover** (delete / purge / re-anchor): `:if workspace--menu-in-ws-p`
  (these are the recovery actions in a broken workspace and the manage actions
  in a healthy one — same three commands, shown whenever there *is* a workspace).
- **Integrations**: `:if workspace--menu-healthy-p` + dynamic `:setup-children`
  reading `workspace--integrations`, emitting one suffix per entry that has a
  `:menu`, each invoking the command with a freshly-built current-workspace
  payload (mirrors `skills-transient.org:291-351`).

The create suffix calls the same `workspace-new` entry path, satisfying the
"creating from the menu matches workspace-new" scenario.

*Binding (decided):* **promote `C-x w` to open `workspace-menu`** as the
single entry point. The individual `C-x w <key>` direct chords are retired —
every command remains reachable through the menu and via `M-x`. This is the
larger surface edit (touches the `global-set-key` block in `workspaces.org:361`)
but gives one coherent front door. The menu's own suffix keys preserve the old
mnemonics where practical (e.g. `n` new, `s` switch, `S` save) so muscle memory
mostly survives as `C-x w` + same letter.

### Decision 6 — gptel consumer: a new sessions module, attached via `with-eval-after-load`

New file `config/gptel/sessions/workspace-integration.org` (this side *may*
name gptel symbols). It defines two handlers and registers them only once
workspaces is present, so load order is irrelevant and gptel still works
without workspaces:

```elisp
(defcustom jf/gptel-workspace-initial-preset 'executor
  "Preset for the session auto-created at workspace birth; nil disables it."
  :type '(choice (const :tag "No auto-session" nil) symbol) :group 'jf-gptel)

(defun jf/gptel--workspace-on-create (payload)
  "Build a real initial gptel session for a freshly-created workspace."
  (pcase (plist-get payload :context)
    ('anchored-existing 'skipped)               ; never inject into an adopted workspace
    (_ (if (null jf/gptel-workspace-initial-preset) 'skipped
         (let* ((home  (plist-get payload :home))
                (sdir  (plist-get payload :sessions-dir))
                (id    (jf/gptel--generate-session-id (or (plist-get payload :name) "initial")))
                (sd    (expand-file-name id sdir)))
           (jf/gptel--create-session-core id sd jf/gptel-workspace-initial-preset
                                          nil nil home nil)   ; project-root = home → scoped
           'ok)))))

(defun jf/gptel--workspace-add-session (payload)
  "On-demand: add a session to the current workspace (may prompt)."
  ...)                                            ; interactive name/preset choice

(with-eval-after-load 'workspaces
  (workspace-register-integration 'gptel-session
    :label "gptel session"
    :on-create #'jf/gptel--workspace-on-create
    :menu (cons "g" #'jf/gptel--workspace-add-session)))
```

The session is **scope-bound to the workspace home from birth** via
`project-root = home` — an improvement over the removed activities path, which
did not scope. `:on-create` is non-interactive (defaults only); the richer
interactive flow lives in the `:menu` command.

## Risks / Trade-offs

- **[on-create non-interactivity is a convention, not enforced]** A handler
  could prompt mid-creation and wedge the flow. → Document the contract loudly
  in the registry docstring and the spec; the gptel handler honors it. Not
  worth a hard `inhibit`-style guard for one in-tree consumer.
- **[uncommitted initial session]** (Decision 4) The auto-created session is
  untracked in a `fresh` repo. → Accepted; visible to the user as untracked,
  recoverable, and a clean follow-up if it bothers anyone.
- **[partial artifacts on a failing handler]** A handler that errors midway
  could leave a half-written session dir. → Workspace validity is unaffected
  (additive invariant); cleanup of its own partial output is the integration's
  responsibility. The failure is surfaced, not hidden.
- **[`pcase`-style result parsing leniency]** Treating unknown returns as `ok`
  could mask a malformed handler. → Acceptable; the alternative (erroring on
  unknown returns) would make the dispatcher fragile to benign handler drift.
- **[transient `:setup-children` rebuilds each display]** Negligible cost (a
  short alist walk), and it is the established repo idiom.

## Testing Approach

**Framework:** Buttercup (BDD), per repo convention for new suites. ERT not used here.

**Locations:**
- Workspaces side: `config/workspaces/test/` — new
  `integration-registry-spec.el`, `integration-dispatch-spec.el`,
  `transient-menu-spec.el`; extend the existing directionality spec
  (`gptel-integration-spec.el`).
- gptel side: `config/gptel/sessions/test/` — new
  `workspace-integration-spec.el`.

**Patterns:**
- *Registry/dispatch* tested with **fake integrations** registered in
  `before-each` and cleared in `after-each` (rebind `workspace--integrations`
  to a fresh value via `let`/spy to avoid cross-test pollution — a known
  hazard called out in this repo's test-isolation tasks).
- *Dispatch error-isolation* uses a fake handler that `error`s and asserts the
  workspace still exists, the other handler ran, and a `*Messages*` notice was
  emitted (spy on `message`).
- *gptel `:on-create`* mocks the boundary with `cl-letf` over
  `jf/gptel--create-session-core` to assert it is called with `project-root =
  home` and the right session dir, and is *not* called when `:context` is
  `anchored-existing` or the preset is nil. One behavioral test opens a
  produced `session.org` and asserts chat-mode + preset application (real
  round-trip).
- *Transient* tested by invoking the predicates and the `:setup-children`
  builder directly (unit-level), asserting which groups/suffixes are produced
  for no-workspace / healthy / broken fixtures — rather than driving the
  interactive transient UI.
- *Directionality* extends the existing grep test to the new workspaces files,
  asserting zero `gptel-sessions-*` references.

**Scenario → test mapping:** each `#### Scenario` in the two spec files maps to
one `it` block; the spec's context-matrix scenarios (fresh /
anchored-scaffolded / anchored-existing) become a parameterized describe over
the three contexts.

## Resolved Decisions (from review)

- **Menu binding** — *promote*: `C-x w` opens `workspace-menu`; individual
  `C-x w <key>` chords retired (Decision 5).
- **Menu "add session" UX** — the `:menu` `g` command **prompts for both name
  and preset** each invocation (`jf/gptel--workspace-add-session`). `:on-create`
  stays non-interactive (defaults only).

## Open Questions

1. **Auto-session preset** — `jf/gptel-workspace-initial-preset` defaults to
   `'executor` (matches the interactive command). Confirm, or pick a lighter
   default for birth-time sessions. (Non-blocking — it is a one-line defcustom
   default, changeable any time.)
2. **Future lifecycle hooks** — `:on-delete` / `:on-purge` are out of scope
   now; the registry shape leaves room. No action this change.
