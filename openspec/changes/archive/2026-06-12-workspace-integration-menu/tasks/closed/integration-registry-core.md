---
name: integration-registry-core
description: New integrations.org — registry, registration fn, anchor payload, result normalizer, and dispatch logic
change: workspace-integration-menu
status: done
relations: []
---

## Files to modify
- `config/workspaces/integrations.org` (new — the `workspace-integrations` capability)
- `config/workspaces/workspaces.org` (modify — load the new submodule after data-model)
- `config/workspaces/test/integration-registry-spec.el` (new — Buttercup)

## Implementation steps

1. **Create `config/workspaces/integrations.org`** with the literate headers
   (`#+title`, `#+property: header-args:emacs-lisp :tangle integrations.el`,
   `#+auto_tangle: y`) and `(provide 'workspace-integrations)` at the end.
   It depends on `workspace--sessions-dir` from `data-model.el`.

2. **Registry storage — an ordered alist** (NOT a hash table; dispatch order
   and menu order must follow registration order):
   ```elisp
   (defvar workspace--integrations nil
     "Alist of (ID . PLIST) registered workspace integrations, in order.")
   ```

3. **Registration entry point** — a `cl-defun` so tests register fakes easily:
   ```elisp
   (cl-defun workspace-register-integration (id &key label on-create menu)
     "Register integration ID (symbol). LABEL string. ON-CREATE optional
   handler (payload -> ok|skipped|(failed . reason)). MENU optional
   (KEY . COMMAND). At least one of ON-CREATE/MENU required; else user-error.
   Re-registering ID replaces it IN PLACE, preserving order.")
   ```
   - Require at least one of `:on-create`/`:menu`; otherwise `user-error`.
   - Replace-in-place: if `id` already present, overwrite its cdr; else append
     to the end (use `assq` + `setcdr`, or rebuild preserving position).

4. **Anchor payload constructor** — the single source of payload shape:
   ```elisp
   (defun workspace--integration-payload (name home context)
     (list :name name :home home
           :sessions-dir (workspace--sessions-dir home)
           :context context))   ; context ∈ {fresh anchored-scaffolded anchored-existing}
   ```
   Plist (not struct) so future keys (β's `:integration-choices`) extend freely.

5. **Per-integration runner with error guard + result normalization:**
   ```elisp
   (defun workspace--run-one-integration (entry payload)
     "Run ENTRY's :on-create on PAYLOAD; return (ID . OUTCOME) where OUTCOME is
   'ok | 'skipped | (failed . REASON). A signalled error normalizes to failed."
     (condition-case err
         (pcase (funcall (plist-get (cdr entry) :on-create) payload)
           ('ok '...) ('skipped '...) (`(failed . ,r) '...) (_ 'ok-default))
       (error (cons (car entry) (cons 'failed (error-message-string err))))))
   ```

6. **Dispatch entry point** — additive, never signals, surfaces failures:
   ```elisp
   (defun workspace--dispatch-create-integrations (name home context)
     "Run every :on-create once, in order, on the payload. message' each
   failure. Return the result alist. NEVER signals.")
   ```
   Skip entries without `:on-create`. For each `(failed . reason)` emit
   `(message "workspace: integration %s failed: %s" id reason)`.

7. **Wire loading** in `config/workspaces/workspaces.org`: add
   `(jf/load-module (expand-file-name "config/workspaces/integrations.el" jf/emacs-dir))`
   immediately AFTER the `data-model.el` line (~123) and BEFORE `scaffold.el`/`tabs.el`
   — `tabs` (next task) will call the dispatcher, so the registry must load first.

8. **Tests** `config/workspaces/test/integration-registry-spec.el` (Buttercup):
   register/replace/order; `user-error` on neither-surface; payload shape +
   `:sessions-dir` derivation + `:context` passthrough; runner maps
   `ok`/`skipped`/`(failed . r)` and normalizes a thrown error to `failed`;
   dispatch runs handlers in order, skips no-`:on-create` entries, and never
   signals. Rebind `workspace--integrations` via `let` in `before-each` to
   avoid cross-test pollution.

## Design rationale
Function-not-macro registration keeps the registry pure data and trivially
testable (design Decision 1). The ordered alist satisfies the spec's
in-registration-order dispatch and stable menu ordering. The push payload
(Decision 2) means handlers never consult global state. The return-value +
error-guard protocol (Decision 3) gives handlers two ergonomic failure paths
treated identically. Dispatch is a pure function here; wiring it into the
creation pipeline is the next task.

## Verification
- `./bin/tangle-org.sh config/workspaces/integrations.org`
- `./bin/tangle-org.sh config/workspaces/workspaces.org`
- `./bin/run-tests.sh -d config/workspaces`
- Done when: registry registers/replaces/orders correctly; payload + result
  protocol behave per spec; dispatch never signals; module loads after data-model.

## Context
design.md § Decisions 1–4; spec `workspace-integrations` (Requirements:
Integration registry and registration; Anchor payload contract; Integration
result protocol; Creation-time dispatch).

## Observations
- The (failed . REASON) result, when returned as the cdr of a (ID . OUTCOME)
  pair, produces the 3-element improper list `(ID failed . REASON)` —
  e.g. `(boom failed . "explode")`. This is the natural consequence of the
  prescribed `(ID . OUTCOME)` shape combined with `OUTCOME = (failed . REASON)`
  and is consistent across the runner and dispatch. Tests assert against this
  exact structure (`caar`/`cadar`/`cddar`). Callers in later tasks (tabs
  dispatch consumer) should pattern-match outcomes via
  `(pcase outcome ('ok ...) ('skipped ...) (`(failed . ,r) ...))` rather than
  indexing, to stay robust to this nesting.
- Decision 3's "unknown return values are treated leniently as `ok`" is
  implemented via the `(_ 'ok)` pcase arm. A handler that returns the symbol
  `nil` (a common accidental return) therefore normalizes to `ok`, not
  `skipped`. This matches the design's lenient stance but is worth flagging:
  handlers that intend "no-op" must return the symbol `skipped` explicitly.
- `workspace-register-integration` always stores the full plist
  `(:label L :on-create H :menu M)` even when keys are nil, so
  `plist-get … :menu` returns nil cleanly for handler-only entries. No
  partial-plist branching needed; `assq` + `setcdr` gives stable in-place
  replacement.
- The spec file loads `data-model.el` then `integrations.el` directly via
  `load` (mirroring `scaffold-spec.el`), independent of the module-loader
  wiring, so the registry is testable in isolation.

## Discoveries
None
