;;; workspaces.el --- Named tab-based workspaces -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defgroup workspaces nil
  "Named tab-based workspaces with per-workspace layouts and buffer scoping."
  :group 'convenience
  :prefix "workspace-")

(declare-function workspace-default-home-builder "workspace-tabs" (workspace-name))

(defcustom workspace-home-builder #'workspace-default-home-builder
  "Function called to build the `home' layout for a newly-created workspace.
Called with one argument WORKSPACE-NAME, in the context of the freshly
activated workspace.  The default implementation opens
=<:home>/home.org= for the workspace via a registry lookup; any buffers
it displays become members of the new workspace."
  :type 'function
  :group 'workspaces)

(defcustom workspaces-default-parent-directory
  (expand-file-name "emacs-workspaces" (or (getenv "HOME") "~"))
  "Default parent directory under which `workspace-new' creates new workspaces.
When `workspace-new NAME' is invoked without a prefix arg, the new
workspace's home directory is `(expand-file-name NAME this-dir)'."
  :type 'directory
  :group 'workspaces)

(defun workspace--backtrace-visible-p ()
  "Return non-nil if a *Backtrace* window is visible on the current frame."
  (cl-some (lambda (w)
             (string= (buffer-name (window-buffer w)) "*Backtrace*"))
           (window-list nil 'no-mini)))

(defcustom workspace-anti-save-predicates
  '(active-minibuffer-window
    workspace--backtrace-visible-p)
  "List of nullary predicates consulted before any workspace autosave.
If any predicate returns non-nil, the autosave is skipped silently.
The explicit `workspace-save' command never consults this list."
  :type '(repeat function)
  :group 'workspaces)

(jf/load-module (expand-file-name "config/workspaces/data-model.el"        jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/integrations.el"      jf/emacs-dir))
;; Consumer of the integration registry (must load after integrations.el).
;; Droppable: registration is gated behind `with-eval-after-load 'magit',
;; so removing this one line leaves workspaces fully functional.
(jf/load-module (expand-file-name "config/workspaces/git-worktrees.el"     jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/home-org.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/scaffold.el"          jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/tabs.el"              jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/buffer-membership.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/layouts.el"           jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/persistence.el"       jf/emacs-dir))
(jf/load-module (expand-file-name "config/workspaces/workspaces-mode.el"   jf/emacs-dir))
(require 'workspaces-mode)
;; Consumer of the workspace registry (must load after tabs.el, which owns
;; `workspace--registry'/`workspace--current-name', and after persistence.el,
;; which defines `workspace-restore').  Droppable: removing this one line
;; removes the bookmark advice and returns behaviour to stock.
(jf/load-module (expand-file-name "config/workspaces/portable-bookmarks.el" jf/emacs-dir))
;; Front-door transient: needs `workspace--current-name'/`workspace--registry'
;; (tabs), `workspace--broken-p' (data-model), and `workspace--integrations'
;; (integrations), so it loads last.
(jf/load-module (expand-file-name "config/workspaces/workspaces-transient.el" jf/emacs-dir))
;; gptel tools workspace_add_layout / workspace_remove_layout: call the
;; programmatic layout API (layouts.el) and `workspace-for-directory' (this
;; file) at run time only.  Droppable: gptel is a soft dependency and
;; registration is gated behind `with-eval-after-load 'gptel', so removing
;; this one line leaves workspaces fully functional.
(jf/load-module (expand-file-name "config/workspaces/agent-tools.el"       jf/emacs-dir))

(defun workspace-re-anchor (name new-home)
  "Point broken workspace NAME at NEW-HOME on the filesystem.

Clears the broken-state tag and updates `:home'.  If
=(file-name-nondirectory (directory-file-name NEW-HOME))= differs
from NAME, the registry entry is renamed to match the new basename
(per the registry-name-equals-basename invariant).

The new path must be local (remote workspace homes are not supported;
refused before any filesystem work), must exist, must be a directory,
and must not be ANOTHER workspace's =:home= (compared by
`workspace--same-file-p', so a case or symlink variant of another
workspace's home is refused too: two workspaces never share one
directory)."
  (interactive
   (list (completing-read "Re-anchor workspace: "
                          (workspace--registered-names) nil t)
         (file-name-as-directory
          (read-directory-name "New home directory: " nil nil t))))
  (let ((ws (gethash name workspace--registry)))
    (unless ws
      (user-error "No workspace named %s" name))
    (workspace--refuse-remote-home new-home)
    (unless (file-directory-p new-home)
      (user-error "Not a directory: %s" new-home))
    (when (workspace--registered-for-home-p new-home name)
      (user-error "Already a registered workspace: %s" new-home))
    (let* ((new-name (file-name-nondirectory
                      (directory-file-name new-home)))
           ;; Update :home AND :name in lockstep (per
           ;; register/invariant/registry-name-equals-basename, which
           ;; pins identity on the plist :name accessor, not on the
           ;; registry key — both must move together when basename
           ;; differs, otherwise the rename is lost across save/
           ;; restore because the serializer writes (workspace--name
           ;; ws) and the deserializer puthashes under the same).
           (next-ws (workspace--clear-broken
                     (workspace--set-name
                      (workspace--set-home ws new-home)
                      new-name))))
      ;; If basename changed, rename the registry key too.
      (cond
       ((string= name new-name)
        (puthash name next-ws workspace--registry))
       (t
        (when (gethash new-name workspace--registry)
          (user-error
           "Cannot rename to %s — a workspace with that name already exists"
           new-name))
        ;; Capture the tab index BEFORE we mutate the registry —
        ;; `workspace--tab-index-for' consults `workspace--registry'
        ;; for ownership, so after `remhash' the lookup would fail.
        (let ((tab-idx (workspace--tab-index-for name)))
          (remhash name workspace--registry)
          (puthash new-name next-ws workspace--registry)
          ;; Update tab label if a live tab exists.
          (when tab-idx
            (tab-bar-rename-tab new-name tab-idx)))))
      (workspace--flush-state)
      (message "Workspace re-anchored: %s → %s" name new-home))))

(defun workspace--purge-in-scope-p (home)
  "Return non-nil when HOME lies STRICTLY below `workspaces-default-parent-directory'.
Decided by `workspace--path-within-p' and `workspace--same-file-p' (file
identity, never spelling).  The parent itself is NOT in scope: purging a
workspace anchored at the parent would delete every workspace below it,
so, like any external home, it needs the prefix-argument opt-in.  A HOME
that no longer exists (a broken workspace) is judged by its nearest
existing ancestor, so a vanished home under (or directly in) the parent
is in scope and one under an external tree is not.  Nil for a remote
HOME.  Never signals."
  (and (stringp home)
       (not (file-remote-p home))
       (let ((path (directory-file-name (expand-file-name home)))
             (previous nil))
         ;; The `equal' is the loop guard at the filesystem root, not a
         ;; containment verdict.
         (while (and (not (file-exists-p path)) (not (equal path previous)))
           (setq previous path
                 path (directory-file-name (file-name-directory path))))
         (and (workspace--path-within-p path workspaces-default-parent-directory)
              ;; PREVIOUS is non-nil only when HOME itself was missing:
              ;; PATH is then a proper ancestor of HOME, so HOME is below
              ;; the parent even when PATH is the parent.  An existing
              ;; HOME must not BE the parent.
              (or previous
                  (not (workspace--same-file-p
                        path workspaces-default-parent-directory)))))))

(defun workspace--unregister (name)
  "Remove workspace NAME from the registry, close its tab, flush state.
The unregister steps shared by `workspace-delete' and `workspace-purge'.
Touches nothing under NAME's home directory."
  ;; Capture the tab index BEFORE the remhash (`workspace--tab-index-for'
  ;; is registry-gated), then unregister BEFORE closing the tab.  Closing
  ;; the current tab fires the tab-switch autosave advice; unregistering
  ;; first makes `workspace--current-name' return nil so the advice
  ;; no-ops — we neither capture nor flush working-state for a workspace
  ;; being removed, and the only disk write is the explicit flush below.
  (let ((tab-idx (workspace--tab-index-for name)))
    (remhash name workspace--registry)
    (when tab-idx
      (tab-bar-close-tab tab-idx)))
  (workspace--flush-state))

(defun workspace-delete (name)
  "Remove workspace NAME from the registry and close its tab.
The home directory and all its contents (including .git/, home.org,
and sessions/) remain on disk untouched.  To delete the directory
too, use `workspace-purge'.

Interactively, completes over registered workspace names."
  (interactive
   (list (completing-read "Delete workspace (unregister only): "
                          (workspace--registered-names) nil t)))
  (unless (gethash name workspace--registry)
    (user-error "No workspace named %s" name))
  (workspace--unregister name)
  (message "Workspace %s unregistered (home directory left on disk)" name))

(defun workspace-purge (name)
  "Unregister workspace NAME and recursively delete its home directory.
Prompts for `yes-or-no-p' confirmation showing the absolute path.

Refuses to operate when `:home' is not a proper descendant of
`workspaces-default-parent-directory' (the parent itself included)
unless a prefix argument is supplied.  This guards against accidentally
purging an anchored external project (e.g., =~/code/myproj/=), or the
parent that holds every scaffolded workspace.  Scope is checked before
the prompt and once more after the confirmation, before anything is
changed: a home that left scope while the prompt was open is refused
with the workspace still registered, its tab open and nothing flushed.
That answer is the only one the destructive steps (unregister, tab
close, state flush, integration teardown, recursive delete) rest on.

For broken-state workspaces (=:home= missing on disk), no
filesystem deletion is attempted — the registry entry is still
removed (register/vocabulary/workspace-broken-disposition pins
purge as =:allowed= on broken).

A remote (TRAMP) =:home= is never contacted: remote workspace homes are
unsupported, and such an entry is only ever loaded broken.  Purge then
asks to unregister it, with or without a prefix argument, and touches
nothing on the remote host — no scope check, no integration teardown,
no deletion."
  (interactive
   (list (completing-read "Purge workspace (DELETES HOME DIR): "
                          (workspace--registered-names) nil t)))
  (let* ((ws (gethash name workspace--registry))
         (home (and ws (workspace--home ws)))
         ;; `file-remote-p' answers from the name alone; it never connects.
         (remote (and home (file-remote-p home))))
    (unless ws
      (user-error "No workspace named %s" name))
    (unless home
      (user-error "Workspace %s has no :home (data corruption?)" name))
    ;; A remote home is unsupported and never contacted: unregister only.
    (when remote
      (unless (yes-or-no-p
               (format "Unregister workspace %s? Its remote home %s is not touched. "
                       name home))
        (user-error "Cancelled")))
    ;; Scope safeguard: refuse to nuke a dir outside the default parent
    ;; unless the user explicitly opts in via prefix arg.  Checked BEFORE
    ;; the yes-or-no-p so an anchored external project never even shows
    ;; the destructive prompt without a deliberate two-step opt-in
    ;; (design.md §D8).
    (unless (or remote
                current-prefix-arg
                (workspace--purge-in-scope-p home))
      (user-error
       "Refusing to purge %s; outside %s. Use `C-u M-x workspace-purge' to override."
       home workspaces-default-parent-directory))
    ;; Final confirmation.
    (unless (or remote
                (yes-or-no-p (format "Recursively delete %s? " home)))
      (user-error "Cancelled"))
    ;; The one decision point: the filesystem can change while the prompt
    ;; is open (a missing parent reappearing as a symlink out of the
    ;; default parent), so scope is asked again here, before the first
    ;; irreversible step.  Nothing below asks it again.
    (unless (or remote
                current-prefix-arg
                (workspace--purge-in-scope-p home))
      (user-error "Not deleting %s: it left %s while you were confirming"
                  home workspaces-default-parent-directory))
    ;; Unregister first; even if delete-directory errors below the
    ;; registry is already clean.
    (workspace--unregister name)
    ;; Filesystem delete (best-effort; broken-home case has nothing
    ;; to do — :home is already absent).
    (when (and (not remote) (file-directory-p home))
      ;; Additive integration teardown BEFORE the directory is gone:
      ;; integrations clean up resources anchored under :home (e.g. git
      ;; worktrees) while the tree still exists (design.md §D3).  The
      ;; dispatcher is error-guarded and never signals; its outcome
      ;; alist is ignored for control flow — a (failed . REASON) result
      ;; is surfaced via `message' but NEVER aborts the purge.  Fires on
      ;; purge only; `workspace-delete' leaves :home intact and so has
      ;; nothing to tear down.
      (workspace--dispatch-purge-integrations name home 'purge)
      (delete-directory home t))
    (message "Workspace %s purged" name)))

(global-set-key (kbd "C-x w") #'workspace-menu)

(workspace--install-tab-bar-display-name)

(defun workspace-sessions-dir ()
  "Return the sessions/ directory of the current workspace, or nil.

Returns nil when no workspace owns the current tab, when the workspace
is broken, or when its :home/sessions/ directory is missing on disk.

This function is the soft-dependency entry point used by other
subsystems (notably gptel/sessions) to route new artifacts under the
active workspace's home.  It MUST NOT signal and has no side effects."
  (when-let* ((name (workspace--current-name))
              (ws   (gethash name workspace--registry))
              ((not (workspace--broken-p ws)))
              (home (workspace--home ws)))
    (let ((dir (workspace--sessions-dir home)))
      (when (file-directory-p dir)
        dir))))

(defun workspace-names ()
  "Return the names of the registered workspaces."
  (workspace--registered-names))

(defun workspace-get (name)
  "Return the workspace plist registered as NAME, or nil.
The plist is a top-level copy: changing its slots does not change the
registry.  The slots' values are shared with the registry; treat them
as read-only."
  (let ((ws (gethash name workspace--registry)))
    (and ws (copy-sequence ws))))

(cl-defun workspace-layouts (name &key origin source)
  "Return the layouts of workspace NAME as plists, in group order.
Each entry is (:name N :origin O :description D :source S), the
metadata read from the group's recent layout.  When ORIGIN is non-nil
keep only the entries whose origin is `eq' to it; when SOURCE is
non-nil keep only those whose source is `equal' to it.  Return nil
when NAME is not registered.  Changes nothing."
  (when-let* ((ws (gethash name workspace--registry)))
    (seq-filter
     (lambda (entry)
       (and (or (null origin) (eq origin (plist-get entry :origin)))
            (or (null source) (equal source (plist-get entry :source)))))
     (mapcar (lambda (group)
               (let ((layout (workspace--group-recent-layout group)))
                 (list :name (workspace--group-name group)
                       :origin (workspace--layout-origin layout)
                       :description (workspace--layout-description layout)
                       :source (workspace--layout-source layout))))
             (workspace--layout-groups ws)))))

(defun workspace-for-directory (dir)
  "Return the name of the workspace whose home contains DIR, or nil.
Containment is by file identity (`workspace--path-within-p'), so the
spelling of DIR does not matter.  When several homes contain DIR, the
one whose truename is deepest wins.  A remote or missing home owns
nothing and is never contacted."
  (let (best best-depth)
    (dolist (name (workspace--registered-names))
      (let ((home (workspace--home (gethash name workspace--registry))))
        (when (workspace--path-within-p dir home)
          (let ((depth (length (split-string (file-truename home) "/" t))))
            (when (or (null best-depth) (> depth best-depth))
              (setq best name
                    best-depth depth))))))
    best))

(defun workspace-layout-on-screen-p (name layout)
  "Return non-nil when LAYOUT of workspace NAME is on screen.
That is: LAYOUT is NAME's recent layout group and some live frame has
a tab named NAME, whether or not it is that frame's current tab."
  (let ((ws (gethash name workspace--registry)))
    (and ws layout
         (equal layout (workspace--recent-group ws))
         (seq-some (lambda (frame)
                     (seq-some (lambda (tab)
                                 (equal (workspace--tab-name tab) name))
                               (frame-parameter frame 'tabs)))
                   (frame-list))
         t)))

(when (fboundp 'workspace--restore)
  (workspace--restore))

(provide 'workspaces)
;;; workspaces.el ends here
