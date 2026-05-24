;;; layouts-spec.el --- Behavioral tests for workspace layout commands -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)
(require 'frameset)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defun layouts-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2)))))

(describe "workspace-save-layout"
  (before-each (layouts-spec--reset))

  (it "stamps a `home' layout when a workspace is created"
    (workspace-new "alpha")
    (let* ((ws (gethash "alpha" workspace--registry))
           (group (workspace--find-group ws "home")))
      (expect group :not :to-be nil)
      (expect (workspace--group-recent-layout group) :not :to-be nil)
      (expect (workspace--recent-group ws) :to-equal "home")))

  (it "adds a named layout to the current workspace"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (let* ((ws (gethash "alpha" workspace--registry))
           (groups (workspace--layout-groups ws))
           (names (mapcar #'workspace--group-name groups)))
      (expect names :to-equal '("home" "magit"))
      (expect (workspace--recent-group ws) :to-equal "magit")))

  (it "overwrites an existing layout when saved with the same name"
    (workspace-new "alpha")
    (workspace-save-layout "scratch")
    (let* ((ws-before (gethash "alpha" workspace--registry))
           (group-before (workspace--find-group ws-before "scratch"))
           (ts-before (workspace--layout-timestamp
                       (workspace--group-recent-layout group-before))))
      ;; Force the next layout to have a strictly later timestamp.
      (sleep-for 1.1)
      (workspace-save-layout "scratch")
      (let* ((ws-after (gethash "alpha" workspace--registry))
             (group-after (workspace--find-group ws-after "scratch"))
             (ts-after (workspace--layout-timestamp
                        (workspace--group-recent-layout group-after))))
        ;; MVP keeps one layout per group; overwritten, not appended.
        (expect (length (workspace--group-layouts group-after))
                :to-equal 1)
        (expect ts-after :to-be-greater-than ts-before))))

  (it "errors when not on a workspace tab"
    (expect (workspace-save-layout "x") :to-throw 'user-error)))

(describe "workspace-switch-layout"
  (before-each (layouts-spec--reset))

  (it "updates the recent-layout pointer to the chosen layout"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-save-layout "tests")
    (workspace-switch-layout "home")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "home")
    (workspace-switch-layout "magit")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit"))

  (it "snapshots the outgoing layout before restoring"
    ;; Set up two layouts and verify that switching captures the
    ;; current frame into the outgoing layout's slot.
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    ;; Switch to home; the current frame should be snapshotted into
    ;; magit (the layout we're leaving).
    (let ((before-ts (workspace--layout-timestamp
                      (workspace--group-recent-layout
                       (workspace--find-group
                        (gethash "alpha" workspace--registry) "magit")))))
      (sleep-for 1.1)
      (workspace-switch-layout "home")
      (let ((after-ts (workspace--layout-timestamp
                       (workspace--group-recent-layout
                        (workspace--find-group
                         (gethash "alpha" workspace--registry) "magit")))))
        (expect after-ts :to-be-greater-than before-ts))))

  (it "errors when asked to switch to a non-existent layout"
    (workspace-new "alpha")
    (expect (workspace-switch-layout "nonexistent") :to-throw 'user-error)))

(describe "layout names are scoped to their workspace"
  (before-each (layouts-spec--reset))

  (it "permits independent layouts in different workspaces sharing a name"
    (workspace-new "alpha")
    (workspace-save-layout "tests")
    (workspace-new "beta")
    (workspace-save-layout "tests")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "tests")
            :not :to-be nil)
    (expect (workspace--find-group
             (gethash "beta" workspace--registry) "tests")
            :not :to-be nil)
    ;; Removing alpha's `tests' leaves beta's `tests' untouched —
    ;; the real independence guarantee.
    (workspace-switch "alpha")
    (workspace-delete-layout "tests")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "tests")
            :to-be nil)
    (expect (workspace--find-group
             (gethash "beta" workspace--registry) "tests")
            :not :to-be nil)))

(describe "workspace-delete-layout"
  (before-each (layouts-spec--reset))

  (it "removes the named layout"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-delete-layout "magit")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "magit")
            :to-be nil))

  (it "refuses to delete the reserved `home' layout"
    (workspace-new "alpha")
    (expect (workspace-delete-layout "home") :to-throw 'user-error)
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "home")
            :not :to-be nil))

  (it "reassigns the recent pointer when the deleted layout was current"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-save-layout "tests")
    ;; Recent is "tests".  Delete it.
    (workspace-delete-layout "tests")
    (let ((recent (workspace--recent-group
                   (gethash "alpha" workspace--registry))))
      (expect recent :not :to-equal "tests")
      ;; Pointer falls back to a remaining group ("home" or "magit").
      (expect (member recent '("home" "magit")) :to-be-truthy))))

(describe "recent-layout pointer"
  (before-each (layouts-spec--reset))

  (it "survives workspace switching"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    ;; alpha's recent is "magit".
    (workspace-new "beta")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit")
    (workspace-switch "alpha")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit"))

  (it "workspace-switch-to-recent-layout jumps to the recent group"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-switch-layout "home")
    ;; Now recent is "home".  Switch back to "magit" then call recent.
    (workspace-switch-layout "magit")
    (workspace-switch-layout "home")
    (workspace-switch-to-recent-layout)
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "home")))

(provide 'layouts-spec)
;;; layouts-spec.el ends here
