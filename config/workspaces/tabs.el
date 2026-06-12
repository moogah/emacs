;;; tabs.el --- Workspaces tab-bar integration -*- lexical-binding: t; -*-

(require 'tab-bar)
(require 'workspace-data-model)
(require 'workspace-home-org)
(require 'workspace-scaffold)
(require 'workspace-integrations)

(defvar workspace--registry (make-hash-table :test 'equal)
  "WORKSPACE-NAME → workspace plist.
Mutated by all `workspace-*' commands.  The registry is the in-memory
source of truth; persistence (later task) walks it to produce the
on-disk form.")

(defun workspace--registered-names ()
  "Return the list of workspace names currently in the registry."
  (let (names)
    (maphash (lambda (k _v) (push k names)) workspace--registry)
    (nreverse names)))

(defun workspace--frame-current-tab ()
  "Return the live current-tab alist from the selected frame's `tabs' parameter.
Distinct from `tab-bar--current-tab', which returns a fresh copy."
  (assq 'current-tab (frame-parameter nil 'tabs)))

(defun workspace--tab-name (tab)
  "Return the `name' slot of TAB, or nil."
  (cdr (assq 'name tab)))

(defun workspace--tab-workspace-name (tab)
  "Return the workspace name associated with TAB, or nil.

A tab is considered owned by workspaces if its `name' matches a key
in `workspace--registry'."
  (let ((name (workspace--tab-name tab)))
    (when (and name (gethash name workspace--registry))
      name)))

(defun workspace--current-name ()
  "Return the workspace name of the currently selected tab, or nil."
  (workspace--tab-workspace-name (workspace--frame-current-tab)))

(defun workspace--tab-for (name)
  "Return the live tab alist whose `name' is NAME and is in the registry."
  (seq-find (lambda (tab)
              (and (equal (workspace--tab-name tab) name)
                   (gethash name workspace--registry)))
            (frame-parameter nil 'tabs)))

(defun workspace--tab-index-for (name)
  "Return the 1-based index of the tab whose `name' is NAME (workspace).
Returns nil if no such tab exists."
  (let ((tabs (frame-parameter nil 'tabs))
        (i 1)
        result)
    (while (and tabs (not result))
      (when (and (equal (workspace--tab-name (car tabs)) name)
                 (gethash name workspace--registry))
        (setq result i))
      (setq tabs (cdr tabs)
            i (1+ i)))
    result))

(defun workspace--display-name (name)
  "Return the cosmetic display name for workspace NAME.
The `#+TITLE:' of the workspace's home.org when present (live read,
no caching), otherwise NAME itself.  Purely cosmetic: it NEVER changes
the registry key, the tab's `name' slot, or the persistence key (per
register/invariant/registry-name-equals-basename).  Returns NAME
unchanged when no workspace is registered under NAME."
  (let* ((ws (gethash name workspace--registry))
         (home (and ws (workspace--home ws))))
    (or (and home (workspace-home-org-title home))
        name)))

(defvar workspace--prev-tab-bar-tab-name-format-function nil
  "The `tab-bar-tab-name-format-function' present before workspaces
installed its display-name wrapper.  Used to delegate rendering and to
restore the prior formatter if workspaces is unloaded.")

(defun workspace--tab-bar-tab-name-format (tab i)
  "Render TAB's tab-bar label, substituting the workspace display name.
For a tab owned by a workspace whose home.org carries a `#+TITLE:',
the label shows that title (via `workspace--display-name') instead of
the registry name; the tab's actual `name' slot is left untouched.
Delegates propertized rendering to the wrapped formatter."
  (let* ((name (cdr (assq 'name tab)))
         (ws (and name (gethash name workspace--registry)))
         (display (and ws (workspace--display-name name)))
         (fn (or workspace--prev-tab-bar-tab-name-format-function
                 #'tab-bar-tab-name-format-default)))
    (if (and display (not (equal display name)))
        (funcall fn
                 (cons (car tab)
                       (cons (cons 'name display)
                             (assq-delete-all 'name (copy-alist (cdr tab)))))
                 i)
      (funcall fn tab i))))

(defun workspace--install-tab-bar-display-name ()
  "Install the workspace display-name tab-bar formatter (idempotent).
Captures the prior `tab-bar-tab-name-format-function' once so the
wrapper can delegate to it; a second call is a no-op."
  (unless (eq tab-bar-tab-name-format-function
              #'workspace--tab-bar-tab-name-format)
    (setq workspace--prev-tab-bar-tab-name-format-function
          tab-bar-tab-name-format-function
          tab-bar-tab-name-format-function
          #'workspace--tab-bar-tab-name-format)))

(defun workspace-default-home-builder (workspace-name)
  "Default `workspace-home-builder': `find-file' the workspace's home.org.
Looks up the workspace by WORKSPACE-NAME and opens
`<:home>/home.org' in a single window.  Every registered workspace
has a `:home' (see `register/invariant/home-required-no-floating-
workspaces'), so no missing-home fallback is carried here."
  (delete-other-windows)
  (let ((home (workspace--home (gethash workspace-name workspace--registry))))
    ;; Route through the home-org-read-pipeline helper rather than
    ;; duplicating the (expand-file-name "home.org" HOME) literal,
    ;; per register/boundary/home-org-read-pipeline's stage-1
    ;; producer contract.
    (find-file (workspace-home-org-path home))))

(defun workspace--registered-for-home-p (home)
  "Return non-nil if any workspace in the registry has =:home= equal to HOME.

Path comparison is by `file-equal-p' on directory-normalized,
`expand-file-name'-d paths so trailing-slash and =~= variants do
not produce false negatives.  Broken-state workspaces are NOT
exempt: a broken workspace's =:home= remains a registered home for
the purposes of the uniqueness invariant."
  (let ((target (file-name-as-directory (expand-file-name home))))
    (catch 'found
      (maphash
       (lambda (_name ws)
         (let ((ws-home (workspace--home ws)))
           (when (and ws-home
                      (file-equal-p (file-name-as-directory
                                     (expand-file-name ws-home))
                                    target))
             (throw 'found t))))
       workspace--registry)
      nil)))

(defun workspace--new-anchor-existing ()
  "Anchor an existing directory as a workspace.
Prompts for an existing directory (`read-directory-name' with
mustmatch=t) and registers it as a workspace.  Three sub-cases by
directory state:

  1. Git repo AND has home.org  → register only; no scaffolding.
  2. Git repo, no home.org      → scaffold files; NO git ops
                                  (the user owns this repo).
  3. Not a git repo             → full scaffold (mkdir, git init,
                                  files, initial commit).

Signals `user-error' if HOME is already a registered workspace's
=:home= (compared by `file-equal-p').  Returns the freshly-registered
workspace plist on success."
  (let* ((home (file-name-as-directory
                (read-directory-name "Anchor workspace at: " nil nil t)))
         (name (file-name-nondirectory
                (directory-file-name home))))
    (when (workspace--registered-for-home-p home)
      (user-error "Already a registered workspace: %s" home))
    (let* ((is-repo? (file-directory-p (expand-file-name ".git" home)))
           (has-home-org? (workspace-home-org-exists-p home))
           ;; The per-case integration :context label.  Each cond
           ;; branch picks its own; dispatch fires once after
           ;; registration with this value (design Decision 4).
           (context
            (cond
             ;; Case 1: repo + home.org → register only.
             ((and is-repo? has-home-org?)
              ;; No scaffolding; just register.
              'anchored-existing)
             ;; Case 2: repo, no home.org → scaffold files, no git ops.
             (is-repo?
              (workspace-scaffold home name :init-and-commit? nil)
              'anchored-scaffolded)
             ;; Case 3: non-repo → full scaffold including git init + commit.
             (t
              (workspace-scaffold home name :init-and-commit? t)
              'fresh))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)
      (puthash name (workspace--make name home) workspace--registry)
      (when (and (boundp 'workspace-home-builder)
                 (functionp workspace-home-builder))
        (funcall workspace-home-builder name))
      ;; Dispatch AFTER registration so a failing integration cannot
      ;; block an otherwise-clean workspace, and so mid-pipeline
      ;; scaffold failure (above) leaves no registry entry and runs no
      ;; :on-create handler (design Decision 4).  Case 1 dispatches too,
      ;; so consumers can decide/skip even when nothing was scaffolded.
      (workspace--dispatch-create-integrations name home context)
      (gethash name workspace--registry))))

(defun workspace--new-default-path (name)
  "Default-path branch of `workspace-new'.
Scaffold a fresh workspace directory at
=(expand-file-name NAME workspaces-default-parent-directory)=.

Signal `user-error' if that directory already exists (the user's
remedy is the prefix-arg form, which anchors an existing dir).
Otherwise: scaffold first (creates the directory, git-inits it,
writes home.org and the empty sessions/ directory, commits); only on
scaffold success do we register the workspace, create a tab, and
run the home builder.  The home builder runs last so any buffers it
displays become members of the freshly-registered workspace.  After
registration (and after the initial commit) we fire
`workspace--dispatch-create-integrations' with the `fresh' context so
integrations can react to the new workspace; a failing integration
never blocks the already-clean workspace (design Decision 4)."
  (let* ((home (expand-file-name
                name
                workspaces-default-parent-directory)))
    (when (file-exists-p home)
      (user-error
       "Cannot scaffold %s: %s already exists. Use `C-u %s' to anchor an existing dir."
       name home (key-description (where-is-internal 'workspace-new nil t))))
    ;; Scaffold first; only register if scaffold succeeded.
    (workspace-scaffold home name :init-and-commit? t)
    (tab-bar-new-tab)
    (tab-bar-rename-tab name)
    (puthash name (workspace--make name home) workspace--registry)
    (when (and (boundp 'workspace-home-builder)
               (functionp workspace-home-builder))
      (funcall workspace-home-builder name))
    ;; Fire creation-time integration dispatch AFTER registration and
    ;; AFTER the initial commit: a failing integration cannot block an
    ;; already-clean workspace, and a mid-pipeline scaffold failure
    ;; (which propagates above, before this point) leaves no registry
    ;; entry and runs no :on-create handler (design Decision 4).
    (workspace--dispatch-create-integrations name home 'fresh)
    (gethash name workspace--registry)))

(defun workspace-new (name &optional anchor-existing)
  "Create a new workspace named NAME.

With no prefix argument (ANCHOR-EXISTING is nil), scaffold a fresh
directory at `(expand-file-name NAME workspaces-default-parent-directory)'.
Signal `user-error' if that directory already exists.

With a prefix argument (ANCHOR-EXISTING is non-nil), prompt for an
existing directory to anchor (NAME is ignored — the workspace name
is derived from the chosen directory's basename, per
=register/invariant/registry-name-equals-basename=).  See the
docstring of `workspace--new-anchor-existing' for the three
sub-cases."
  (interactive "sWorkspace name: \nP")
  (if anchor-existing
      (workspace--new-anchor-existing)
    (workspace--new-default-path name)))

(defun workspace-switch (name)
  "Select the tab for workspace NAME.

Interactively completes over workspaces currently backed by a tab.

Signals `user-error' if NAME is in a broken state (its `:home' no
longer exists on disk).  Use `workspace-re-anchor' to point it at a
new path or `workspace-purge' to remove the registry entry."
  (interactive
   (list (completing-read
          "Switch to workspace: "
          (seq-filter #'workspace--tab-for (workspace--registered-names))
          nil t)))
  (let ((ws (gethash name workspace--registry)))
    (when (and ws (workspace--broken-p ws))
      (user-error
       "Workspace %s is broken: :home %s no longer exists. Use `workspace-re-anchor' or `workspace-purge'."
       name (workspace--home ws))))
  (let ((idx (workspace--tab-index-for name)))
    (unless idx
      (user-error "No tab found for workspace %s" name))
    (tab-bar-select-tab idx)))

(provide 'workspace-tabs)
;;; tabs.el ends here
