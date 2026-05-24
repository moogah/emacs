;;; tabs-spec.el --- Integration tests for workspace tab integration -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir)))

;; Forward-declare for the byte-compiler in batch runs that don't load
;; the parent workspaces.el (which holds the defcustom).
(defvar workspace-home-builder #'workspace-default-home-builder)

(defun workspaces-spec--reset ()
  "Reset tab-bar and workspace registry to a known empty state."
  (clrhash workspace--registry)
  ;; Close all but one tab.
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2)))))

(describe "workspace-new"
  (before-each (workspaces-spec--reset))

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

  (it "re-selects an existing workspace's tab on duplicate name (and does not create another)"
    (workspace-new "alpha")
    (workspace-new "beta")
    (let ((count-before (length (funcall tab-bar-tabs-function))))
      (workspace-new "alpha")
      (expect (length (funcall tab-bar-tabs-function))
              :to-equal count-before)
      (expect (workspace--current-name) :to-equal "alpha"))))

(describe "workspace-switch"
  (before-each (workspaces-spec--reset))

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

  (it "no-ops on tabs not present in the workspaces registry"
    ;; Create a vanilla tab without going through workspace-new.
    (tab-bar-new-tab)
    (expect (workspace--current-name) :to-be nil)
    ;; Selecting the previous (also untagged) tab should not error.
    (tab-bar-select-tab 1)
    (expect (workspace--current-name) :to-be nil))

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
