;;; workspaces-transient.el --- Workspaces transient menu -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun workspace--menu-current ()
  "Return the current tab's workspace plist, or nil.
Resolves `workspace--current-name' against `workspace--registry'."
  (when-let ((n (workspace--current-name)))
    (gethash n workspace--registry)))

(defun workspace--menu-healthy-p ()
  "Return non-nil when the current tab is a non-broken workspace."
  (let ((w (workspace--menu-current)))
    (and w (not (workspace--broken-p w)))))

(defun workspace--menu-broken-p ()
  "Return non-nil when the current tab is a broken workspace."
  (let ((w (workspace--menu-current)))
    (and w (workspace--broken-p w) t)))

(defun workspace--menu-in-ws-p ()
  "Return non-nil when the current tab is a workspace (healthy or broken)."
  (and (workspace--menu-current) t))

(defun workspace--menu-invoke-integration (command)
  "Call integration COMMAND on a freshly built current-workspace payload.
The payload is constructed from the live current workspace via
`workspace--integration-payload' with context `menu-invoke'.  Signals a
`user-error' when invoked outside a workspace (the Integrations group is
healthy-only, so this is a defensive guard)."
  (let ((ws (workspace--menu-current)))
    (unless ws
      (user-error "No current workspace"))
    (funcall command
             (workspace--integration-payload
              (workspace--name ws)
              (workspace--home ws)
              'menu-invoke))))

(defun workspace--menu-integration-children (&optional _)
  "Build transient suffix specs for registry integrations with a `:menu'.
Walks `workspace--integrations' in order and returns, for every entry
whose plist carries a `:menu' (KEY . COMMAND) pair, a suffix spec
\(KEY DESC THUNK) where THUNK invokes COMMAND on the live workspace
payload.  Entries lacking `:menu' are skipped.  Returns nil when no
integration exposes a menu surface.  The optional argument is ignored;
it exists to match the `:setup-children' calling convention."
  (let (children)
    (dolist (entry workspace--integrations)
      (let* ((id   (car entry))
             (plist (cdr entry))
             (menu  (plist-get plist :menu)))
        (when (consp menu)
          (let* ((key     (car menu))
                 (command (cdr menu))
                 (label   (or (plist-get plist :label)
                              (symbol-name id))))
            (push (list key label
                        (lambda ()
                          (interactive)
                          (workspace--menu-invoke-integration command)))
                  children)))))
    (nreverse children)))

(with-eval-after-load 'transient
  (defun workspace--menu-setup-integration-children (_)
    "`:setup-children' callback for the Integrations group.
Parses `workspace--menu-integration-children' against `workspace-menu'."
    (transient-parse-suffixes
     'workspace-menu
     (workspace--menu-integration-children)))

  (transient-define-prefix workspace-menu ()
    "Context-aware menu for workspace commands.
Renders the Entry group always; the Layouts + State and Integrations
groups only in a healthy workspace; and the Manage / Recover group in
any workspace (recovery affordances in a broken one, management in a
healthy one)."
    ["Workspace"
     ["Entry"
      ("n" "New"     workspace-new)
      ("s" "Switch"  workspace-switch)
      ("o" "Restore" workspace-restore)]
     ["Layouts + State"
      :if workspace--menu-healthy-p
      ("l" "Switch layout"  workspace-switch-layout)
      ("L" "Save layout"    workspace-save-layout)
      ("T" "Recent layout"  workspace-switch-to-recent-layout)
      ("S" "Save workspace" workspace-save)
      ("r" "Revert"         workspace-revert)]
     ["Manage / Recover"
      :if workspace--menu-in-ws-p
      ("D" "Delete"        workspace-delete)
      ("P" "Purge"         workspace-purge)
      ("R" "Re-anchor"     workspace-re-anchor)
      ("b" "Remove buffer" workspace-remove-buffer)]
     ["Integrations"
      :if workspace--menu-healthy-p
      :setup-children workspace--menu-setup-integration-children]]))

(provide 'workspaces-transient)
;;; workspaces-transient.el ends here
