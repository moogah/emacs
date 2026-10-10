;;; workspace-integration.el --- org-graph workspace integration -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar vulpea-db-sync-directories)
(defvar gptel--known-presets)
(declare-function workspace-register-integration "workspace-integrations"
                  (id &rest keys))
(declare-function vulpea-db-sync-update-directory "vulpea-db-sync" (dir))
(declare-function org-graph/configure-sync "org-graph-discovery" ())
(declare-function org-graph/agent-tools "org-graph-tools" ())

(defun org-graph-workspace-integration--on-create (payload)
  "Watch a new workspace `:home' for vulpea indexing, from PAYLOAD.
PAYLOAD is the anchor plist (`:name' `:home' `:sessions-dir'
`:context').  Returns `skipped' when `org-graph-watch-workspace-homes'
is nil or PAYLOAD carries no `:home'; otherwise appends the
canonicalised home to `vulpea-db-sync-directories' (deduped) and calls
`vulpea-db-sync-update-directory' to index it immediately (a one-shot
index, NOT a filenotify watcher install — ongoing watching of a
post-autosync directory is picked up on the next autosync restart, via
the `:menu'/`org-graph/configure-sync' path) — and returns `ok'.  Reads ONLY the pushed
PAYLOAD; never consults the current/global workspace state.  A signalled
error propagates to the registry's per-integration guard, which
normalises it to (failed . REASON) without rolling back the workspace."
  (let ((home (plist-get payload :home)))
    (cond
     ((not (and (boundp 'org-graph-watch-workspace-homes)
                org-graph-watch-workspace-homes))
      'skipped)
     ((null home) 'skipped)
     (t (let ((dir (file-name-as-directory (expand-file-name home))))
          (add-to-list 'vulpea-db-sync-directories dir t)
          (vulpea-db-sync-update-directory dir)
          'ok)))))

(defun org-graph-workspace-integration--menu (_payload)
  "Re-index the active org-graph roots from the workspaces transient.
_PAYLOAD is the current-workspace anchor plist supplied by
`workspace--menu-invoke-integration'; it is ignored because
`org-graph/configure-sync' re-reads the live workspace registry itself.
Returns `ok'."
  (org-graph/configure-sync)
  'ok)

(defun org-graph-workspace-integration-register ()
  "Register the `org-graph' integration with the workspaces registry.
Declares an `:on-create' watch-add handler and a `:menu' re-index entry.
Re-registration is idempotent (the registry keys by id)."
  (workspace-register-integration 'org-graph
    :label "org-graph"
    :on-create #'org-graph-workspace-integration--on-create
    :menu (cons "G" #'org-graph-workspace-integration--menu)))

(with-eval-after-load 'workspaces
  (org-graph-workspace-integration-register))

(defun org-graph-workspace-integration--populate-assistant-tools ()
  "Add the org-graph agent tools to the `workspace-assistant' preset.
Sets the preset's `:tools' slot to `org-graph/agent-tools' (gptel-tool
objects), preserving every other slot.  Returns `skipped' when the tool
list is empty (gptel not loaded / tools unregistered) or the preset is
absent; otherwise mutates the slot in place and returns `ok'."
  (let ((tools (and (fboundp 'org-graph/agent-tools) (org-graph/agent-tools))))
    (if (null tools)
        'skipped
      (let ((cell (and (boundp 'gptel--known-presets)
                       (assq 'workspace-assistant gptel--known-presets))))
        (if (null cell)
            'skipped
          (setcdr cell (plist-put (cdr cell) :tools tools))
          'ok)))))

(with-eval-after-load 'gptel-preset-workspace-assistant
  (org-graph-workspace-integration--populate-assistant-tools))

(provide 'org-graph-workspace-integration)
;;; workspace-integration.el ends here
