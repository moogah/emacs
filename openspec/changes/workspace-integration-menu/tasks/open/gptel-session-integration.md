---
name: gptel-session-integration
description: gptel consumer — register the gptel-session integration (:on-create scoped session + :menu add-session); load it in gptel.org
change: workspace-integration-menu
status: blocked
relations:
  - blocked-by:integration-registry-core
---

## Files to modify
- `config/gptel/sessions/workspace-integration.org` (new — the consumer)
- `config/gptel/gptel.org` (modify — load the new submodule after commands.el)
- `config/gptel/sessions/test/workspace-integration-spec.el` (new — Buttercup)

## Implementation steps

1. **Create `config/gptel/sessions/workspace-integration.org`** (literate
   headers, `:tangle workspace-integration.el`, `(provide
   'jf-gptel-workspace-integration)`). THIS file may name gptel symbols (it is
   on the gptel side of the directionality boundary).

2. **Defcustom for the birth-time preset:**
   ```elisp
   (defcustom jf/gptel-workspace-initial-preset 'executor
     "Preset for the session auto-created at workspace birth; nil disables it."
     :type '(choice (const :tag "No auto-session" nil) symbol) :group 'jf-gptel)
   ```

3. **`:on-create` handler (non-interactive, defaults only):**
   ```elisp
   (defun jf/gptel--workspace-on-create (payload)
     (pcase (plist-get payload :context)
       ('anchored-existing 'skipped)              ; never inject into an adopted workspace
       (_ (if (null jf/gptel-workspace-initial-preset) 'skipped
            (let* ((home (plist-get payload :home))
                   (sdir (plist-get payload :sessions-dir))
                   (id   (jf/gptel--generate-session-id (or (plist-get payload :name) "initial")))
                   (sd   (expand-file-name id sdir)))
              (jf/gptel--create-session-core
               id sd jf/gptel-workspace-initial-preset nil nil home nil) ; project-root=home → scoped
              'ok)))))
   ```
   Note the exact core signature: `jf/gptel--create-session-core (session-id
   session-dir preset-name &optional initial-content worktree-paths
   project-root parent-session-id)` (`commands.org:544`).

4. **`:menu` handler (on-demand, prompts for BOTH name and preset — decided):**
   ```elisp
   (defun jf/gptel--workspace-add-session (payload)
     "Create a gptel session in the current workspace. Prompts name + preset."
     ...) ; read name (read-string) + preset (completing-read over presets),
          ; then jf/gptel--create-session-core into (plist-get payload :sessions-dir)
          ; with project-root = (plist-get payload :home). Return 'ok.
   ```

5. **Register, load-order-independent:**
   ```elisp
   (with-eval-after-load 'workspaces
     (workspace-register-integration 'gptel-session
       :label "gptel session"
       :on-create #'jf/gptel--workspace-on-create
       :menu (cons "g" #'jf/gptel--workspace-add-session)))
   ```
   `with-eval-after-load` means gptel still works when workspaces is absent,
   and registration happens whenever both are present regardless of load order.

6. **Wire loading** in `config/gptel/gptel.org`: add
   `(jf/load-module (expand-file-name "config/gptel/sessions/workspace-integration.el" jf/emacs-dir))`
   AFTER the `commands.el` load (~gptel.org:346), since it calls
   `jf/gptel--create-session-core` / `jf/gptel--generate-session-id`.

7. **Tests** `config/gptel/sessions/test/workspace-integration-spec.el`
   (Buttercup): mock the boundary with `cl-letf` over
   `jf/gptel--create-session-core` — assert `:on-create` calls it with
   `project-root = home` and a session dir under `:sessions-dir`; returns
   `'skipped` when `:context` is `anchored-existing` or preset is nil; `:menu`
   handler prompts (spy `read-string`/`completing-read`) and creates under the
   payload's sessions-dir. One behavioral round-trip: open a produced
   `session.org` and assert `gptel-chat-mode` active + preset applied.

## Design rationale
The fix lives on the gptel side because the directionality contract forbids
workspaces from naming gptel (design Decision 6). `project-root = home` scopes
the session to the workspace from birth — an improvement over the removed
activities path. `:on-create` honors the non-interactive contract (defaults
only); the interactive richness lives behind `:menu`. The `anchored-existing`
skip implements the spec's "don't inject artifacts into an adopted workspace."

## Verification
- `./bin/tangle-org.sh config/gptel/sessions/workspace-integration.org`
- `./bin/tangle-org.sh config/gptel/gptel.org`
- `./bin/run-tests.sh -d config/gptel/sessions`
- Done when: `:on-create` builds a real scoped `session.org` (drawer present,
  opens in chat-mode), skips adopted/nil-preset; `:menu` prompts name+preset
  and files under the workspace; registration is load-order-independent.

## Context
design.md § Decision 6; spec `workspace-integrations` (On-demand integration
invocation; Anchor payload contract; Registry is the published boundary);
proposal § "gptel-session becomes the first real integration".
