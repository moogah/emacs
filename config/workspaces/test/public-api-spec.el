;;; public-api-spec.el --- The public, name-addressed workspaces read API -*- lexical-binding: t; -*-

;; Change workspaces-programmatic-layouts, design.md D1 and § Seams:
;;   seam/public-read-api      `workspace-names', `workspace-get',
;;                             `workspace-layouts'
;;   seam/directory-ownership  `workspace-for-directory'
;;   seam/on-screen            `workspace-layout-on-screen-p'
;;
;; Each spec cites the scenario of specs/workspaces/spec.md it pins
;; (`Scenario: workspaces § <title>').  They are pending (`xit') until
;; workspaces.el defines the functions; the hooks use only what HEAD
;; defines, because buttercup runs the hooks of a pending spec.
;;
;; Registry isolation and real homes come from the shared fixtures
;; (portable-bookmarks-fixtures.el); the remote home from the core fake
;; remote handler (workspaces-test-helpers.el).  The counting validator in
;; portable-bookmarks-spec.el fails the suite on an inline copy of either.
;; The state file is redirected with `workspace-state-directory-override',
;; since a tab switch below runs the autosave.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'tab-bar)

(defconst public-api-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(let ((dir public-api-spec--this-dir))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir))
  ;; The public API lives in workspaces.el (design.md D1).
  (load (expand-file-name "../workspaces.el"        dir))
  (load (expand-file-name "portable-bookmarks-fixtures.el" dir)))

(defconst public-api-spec--remote-prefix "/wsapi-remote:"
  "Prefix of the stand-in remote file names below.")

(defvar public-api-spec--saved-state-override nil
  "`workspace-state-directory-override' as it was before the spec.")

(defun public-api-spec--setup ()
  "Isolate the registry, the bookmarks and the state file."
  (workspace-bookmark-fixtures-setup-with-empty-registry)
  (setq public-api-spec--saved-state-override workspace-state-directory-override
        workspace-state-directory-override
        (workspace-bookmark-fixtures-temp-dir "wsapi-state-")))

(defun public-api-spec--teardown ()
  "Close every tab but the first, then undo `public-api-spec--setup'."
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (tab-bar-select-tab 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq workspace-state-directory-override public-api-spec--saved-state-override)
  (workspace-bookmark-fixtures-teardown))

(defun public-api-spec--put-layout (ws-name layout-name layout)
  "Store LAYOUT as LAYOUT-NAME of WS-NAME through the pure data layer."
  (puthash ws-name
           (workspace--upsert-group (gethash ws-name workspace--registry)
                                    layout-name layout)
           workspace--registry))

(defun public-api-spec--set-recent (ws-name layout-name)
  "Make LAYOUT-NAME the recent layout of WS-NAME."
  (puthash ws-name
           (workspace--set-recent-group (gethash ws-name workspace--registry)
                                        layout-name)
           workspace--registry))

(defun public-api-spec--open-tab (name)
  "Open a tab named NAME, as `workspace-restore' leaves one."
  (tab-bar-new-tab)
  (tab-bar-rename-tab name))

(defun public-api-spec--rows (entries)
  "ENTRIES from `workspace-layouts' as (NAME ORIGIN DESCRIPTION SOURCE) lists."
  (mapcar (lambda (entry)
            (list (plist-get entry :name) (plist-get entry :origin)
                  (plist-get entry :description) (plist-get entry :source)))
          entries))

(describe "Reading workspaces by name (seam/public-read-api)"
  (before-each (public-api-spec--setup))
  (after-each (public-api-spec--teardown))

  (it "names and gets the registered workspaces (seam/public-read-api)"
    (let ((home (workspace-bookmark-fixtures-make-workspace "code")))
      (workspace-bookmark-fixtures-make-workspace "notes")
      (expect (sort (copy-sequence (workspace-names)) #'string<)
              :to-equal '("code" "notes"))
      (expect (workspace--home (workspace-get "code")) :to-equal home)
      (expect (workspace-get "absent") :to-be nil)))

  (it "lists the layouts and metadata of a workspace that has no tab, opening none (Scenario: workspaces § Listing layouts of a workspace that has no tab)"
    (workspace-bookmark-fixtures-make-workspace "notes")
    (public-api-spec--put-layout "notes" "home" (workspace--layout-make 'home-state))
    (public-api-spec--put-layout
     "notes" "review"
     (workspace--layout-with-metadata (workspace--layout-make 'review-state)
                                      :origin 'agent :description "Review"
                                      :source "c1"))
    (let ((frame (selected-frame))
          (tab-names (mapcar #'workspace--tab-name (frame-parameter nil 'tabs)))
          (entry (copy-tree (gethash "notes" workspace--registry))))
      (spy-on 'workspace--flush-state)
      (expect (public-api-spec--rows (workspace-layouts "notes"))
              :to-equal '(("home" user nil nil)
                          ("review" agent "Review" "c1")))
      (expect (selected-frame) :to-be frame)
      (expect (mapcar #'workspace--tab-name (frame-parameter nil 'tabs))
              :to-equal tab-names)
      (expect (workspace--tab-index-for "notes") :to-be nil)
      (expect (gethash "notes" workspace--registry) :to-equal entry)
      (expect 'workspace--flush-state :not :to-have-been-called)))

  (it "lists only the agent layouts of one source when filtered (Scenario: workspaces § Listing agent layouts for one source)"
    (workspace-bookmark-fixtures-make-workspace "code")
    (public-api-spec--put-layout "code" "home" (workspace--layout-make 'home-state))
    (dolist (source '("a1" "a2"))
      (public-api-spec--put-layout
       "code" (concat "ask/" source)
       (workspace--layout-with-metadata (workspace--layout-make 'ask-state)
                                        :origin 'agent :source source)))
    (expect (mapcar #'car (public-api-spec--rows
                           (workspace-layouts "code" :origin 'agent :source "a1")))
            :to-equal '("ask/a1"))
    (expect (mapcar #'car (public-api-spec--rows
                           (workspace-layouts "code" :origin 'agent)))
            :to-equal '("ask/a1" "ask/a2"))
    (expect (mapcar #'car (public-api-spec--rows
                           (workspace-layouts "code" :origin 'user)))
            :to-equal '("home"))))

(describe "The workspace that owns a directory (seam/directory-ownership)"
  (before-each (public-api-spec--setup))
  (after-each (public-api-spec--teardown))

  (it "finds the owner of a nested directory under any spelling, and none outside every home (Scenario: workspaces § Workspace found from a nested directory)"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "emacs-mcp"))
           (nested (file-name-as-directory (expand-file-name "emacs/openspec" home)))
           (link-parent (workspace-bookmark-fixtures-temp-dir "wsapi-link-"))
           (link (expand-file-name "via-link" link-parent))
           (elsewhere (workspace-bookmark-fixtures-temp-dir "wsapi-elsewhere-")))
      (make-directory nested t)
      (make-symbolic-link (directory-file-name home) link)
      (expect (workspace-for-directory nested) :to-equal "emacs-mcp")
      (expect (workspace-for-directory (directory-file-name nested))
              :to-equal "emacs-mcp")
      (expect (workspace-for-directory (expand-file-name "emacs/openspec" link))
              :to-equal "emacs-mcp")
      (expect (workspace-for-directory home) :to-equal "emacs-mcp")
      (expect (workspace-for-directory elsewhere) :to-be nil)))

  (it "answers the workspace whose home is deepest (Scenario: workspaces § The deepest owning home wins)"
    (let* ((outer (workspace-bookmark-fixtures-make-workspace "outer"))
           (inner (file-name-as-directory (expand-file-name "inner" outer))))
      (make-directory (expand-file-name "src" inner) t)
      (make-directory (expand-file-name "docs" outer) t)
      (workspace-bookmark-fixtures-make-workspace "inner" inner)
      (expect (workspace-for-directory (expand-file-name "src" inner))
              :to-equal "inner")
      (expect (workspace-for-directory (expand-file-name "docs" outer))
              :to-equal "outer")))

  (it "never answers a remote home and never contacts it (Scenario: workspaces § A remote home owns nothing and is not contacted)"
    (let ((local (workspace-bookmark-fixtures-make-workspace "local"))
          (elsewhere (workspace-bookmark-fixtures-temp-dir "wsapi-elsewhere-")))
      (workspaces-test-with-fake-remote
          (public-api-spec--remote-prefix
           :allow (expand-file-name file-name-as-directory))
        ;; Registered under the handler: the constructor canonicalises the
        ;; home by name only, which is all the handler lets through.
        (workspace-bookmark-fixtures-register-workspace
         "remote" (concat public-api-spec--remote-prefix "/srv/remote/"))
        (expect (workspace-for-directory local) :to-equal "local")
        (expect (workspace-for-directory elsewhere) :to-be nil))
      (expect workspaces-test-fake-remote-touches :to-be nil))))

(describe "Whether a layout is on screen (seam/on-screen)"
  (before-each (public-api-spec--setup))
  (after-each (public-api-spec--teardown))

  (it "reports the recent layout of a workspace with an open tab, and nothing else (Scenario: workspaces § On-screen check)"
    (dolist (name '("code" "notes"))
      (workspace-bookmark-fixtures-make-workspace name)
      (public-api-spec--put-layout name "home" (workspace--layout-make 'home-state))
      (public-api-spec--set-recent name "home"))
    (public-api-spec--put-layout "code" "demo" (workspace--layout-make 'demo-state))
    (public-api-spec--open-tab "code")
    (expect (workspace-layout-on-screen-p "code" "home") :to-be-truthy)
    (expect (workspace-layout-on-screen-p "code" "demo") :to-be nil)
    (expect (workspace-layout-on-screen-p "notes" "home") :to-be nil)
    ;; An open tab need not be the current one.
    (tab-bar-select-tab 1)
    (expect (workspace-layout-on-screen-p "code" "home") :to-be-truthy)))

;;; public-api-spec.el ends here
