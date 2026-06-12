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

## Observations
- The defcustom `:group` in the task body was `jf-gptel`, but the
  established sessions group symbol (per `constants.org`) is plain
  `gptel`. Used `:group 'gptel` to match the existing modules; flagged
  as a discovery below.
- The `:on-create` and core signatures matched the task body exactly:
  `jf/gptel--create-session-core (session-id session-dir preset-name
  &optional initial-content worktree-paths project-root parent-session-id)`
  (commands.org:544) and `jf/gptel--generate-session-id (base-name)`
  (filesystem.org:115). No interface drift in the builder.
- Preset enumeration reuses the interactive command's mechanism:
  `(mapcar #'car gptel--known-presets)` with a `:description`
  annotator, extracted into `jf/gptel--workspace-read-preset` and
  shared by the `:menu` handler.
- The real-builder round-trip confirms `register/shape/
  session-sibling-system-prompt-file`: the produced `session.org`
  carries a `:PROPERTIES:` drawer with `:GPTEL_PRESET:`, opens in
  `gptel-chat-mode`, and applies the preset's buffer-locals
  (temperature 0.63). No hand-rolled session file.
- Loading wired in `gptel.org` immediately after `commands.el`
  (before persistent-agent), respecting the documented sessions load
  order. Registration uses `with-eval-after-load 'workspaces`, so it
  is inert when workspaces is absent and fires regardless of load order.
- Test count: baseline 98 → 103 specs on this branch, 0 failed
  (added 5: three `:on-create`, one `:menu`, one behavioral round-trip).
- `runtime/straight` and `runtime/tree-sitter` appear as untracked
  symlinks in this worktree (not git-ignored here); staged only the
  five task files explicitly to keep them out of the commit.

## Discoveries
- discovery_id: disc-gptel-session-integration-1
  class: interface-drift
  description: The task body's defcustom snippet specifies `:group
    'jf-gptel`, but no `jf-gptel` defgroup exists; all existing
    sessions defcustoms use `:group 'gptel` (constants.org lines
    38/51/65/101). Implemented with `:group 'gptel` to match the
    established convention.
  affected_register_entry: none (cosmetic group-symbol mismatch in the
    task body, not a published register entry)
  recommendation: Treat `gptel` as the canonical customize group for
    gptel sessions defcustoms; update future task bodies to say
    `:group 'gptel` rather than `:group 'jf-gptel`.
