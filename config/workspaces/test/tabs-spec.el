;;; tabs-spec.el --- Integration tests for workspace tab integration -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tab-bar)
(require 'frameset)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  ;; persistence.el installs the live :after advice
  ;; `workspace--persistence-after-tab-switch' on tab-bar-select-tab
  ;; / tab-bar-switch-to-tab; its dependencies (buffer-membership,
  ;; layouts) must load first.  bufferlo is stubbed if the package
  ;; is not installed in the test environment.
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir)))

;; Forward-declare for the byte-compiler in batch runs that don't load
;; the parent workspaces.el (which holds the defcustom).
(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar workspaces-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun workspaces-spec--reset ()
  "Reset tab-bar and workspace registry to a known empty state.
Also stubs `workspace-scaffold' and points
`workspaces-default-parent-directory' at a fresh tmpdir, so
`workspace-new' is filesystem-isolated for these specs (cycle-3 wired
the scaffold pipeline; pre-existing specs predate that and don't
exercise it).  Must be called from `before-each' — buttercup's
`spy-on' requires it."
  (clrhash workspace--registry)
  ;; Close all but one tab.
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq workspaces-spec--parent-dir
        (make-temp-file "ws-tabs-spec-" t)
        workspaces-default-parent-directory workspaces-spec--parent-dir))

(defun workspaces-spec--cleanup ()
  (when (and workspaces-spec--parent-dir
             (file-directory-p workspaces-spec--parent-dir))
    (delete-directory workspaces-spec--parent-dir t)))

(describe "workspace-new"
  (before-each (workspaces-spec--reset))
  (after-each (workspaces-spec--cleanup))

  (it "creates a new tab whose name identifies the workspace"
    (let ((count-before (length (funcall tab-bar-tabs-function))))
      (workspace-new "alpha")
      (expect (length (funcall tab-bar-tabs-function))
              :to-equal (1+ count-before))
      (expect (workspace--current-name) :to-equal "alpha")
      (expect (workspace--tab-name (workspace--frame-current-tab))
              :to-equal "alpha")))

  (it "inserts the workspace into the registry"
    (workspace-new "alpha")
    (expect (gethash "alpha" workspace--registry) :not :to-be nil)
    (expect (workspace--name (gethash "alpha" workspace--registry))
            :to-equal "alpha"))

  ;; The cycle-1 "re-select existing workspace's tab on duplicate name"
  ;; test was removed in cycle-3 (task workspace-new-default-path).
  ;; The contract changed: per design D2 + spec scenario "Default-path
  ;; collision is rejected", a duplicate workspace name whose home
  ;; directory already exists now signals `user-error' (the user's
  ;; remedy is the prefix-arg form, which anchors an existing dir).
  ;; The collision case is covered in workspace-new-default-spec.el.
  )

(describe "workspace-switch"
  (before-each (workspaces-spec--reset))
  (after-each (workspaces-spec--cleanup))

  (it "selects the named workspace's tab"
    (workspace-new "alpha")
    (workspace-new "beta")
    (expect (workspace--current-name) :to-equal "beta")
    (workspace-switch "alpha")
    (expect (workspace--current-name) :to-equal "alpha"))

  (it "errors on an unknown workspace name"
    (expect (workspace-switch "nonexistent") :to-throw 'user-error)))

(describe "tab-switch advice"
  (before-each (workspaces-spec--reset))
  (after-each (workspaces-spec--cleanup))

  (it "workspace--persistence-after-tab-switch no-ops on non-workspace tabs"
    ;; The cycle-1 two-state-layout task installed
    ;; `workspace--persistence-after-tab-switch' as the live :after
    ;; advice on tab-bar-switch-to-tab / tab-bar-select-tab.  Its
    ;; load-bearing guarantee is that it is a no-op when the current
    ;; tab is not a workspace tab (i.e. carries no :workspace-name),
    ;; so it does not interfere with activities-tabs-mode during
    ;; side-by-side development (design.md §D8).
    ;; Create a vanilla tab without going through workspace-new.
    (tab-bar-new-tab)
    (expect (workspace--current-name) :to-be nil)
    ;; Selecting the previous (also untagged) tab should not error;
    ;; the live :after advice runs and must early-return on nil name.
    (tab-bar-select-tab 1)
    (expect (workspace--current-name) :to-be nil)
    ;; Direct invocation: with no workspace tab current, the advice
    ;; must be a no-op (return nil, no side effects, no error).
    (expect (workspace--persistence-after-tab-switch) :to-be nil))

  (it "survives a tab-bar-select-tab round-trip"
    ;; This is the regression for the dropped-tab-parameter bug.
    (workspace-new "alpha")
    (workspace-new "beta")
    (workspace-switch "alpha")
    (expect (workspace--current-name) :to-equal "alpha")
    (workspace-switch "beta")
    (expect (workspace--current-name) :to-equal "beta")
    (workspace-switch "alpha")
    (expect (workspace--current-name) :to-equal "alpha")))

(provide 'tabs-spec)
;;; tabs-spec.el ends here
