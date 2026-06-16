;;; transient-menu-spec.el --- Tests for the workspaces transient menu -*- lexical-binding: t; -*-

;; Unit-level coverage for `config/workspaces/workspaces-transient.el':
;; the three context predicates (`workspace--menu-healthy-p',
;; `workspace--menu-broken-p', `workspace--menu-in-ws-p') and the
;; registry-driven Integrations children builder
;; (`workspace--menu-integration-children').
;;
;; These tests exercise the predicates and the children builder DIRECTLY
;; — they never drive the interactive transient UI.  The current
;; workspace is faked by stubbing `workspace--current-name',
;; `workspace--registry', and `workspace--broken-p' via `cl-letf'; the
;; registry is rebound per test via `let'.
;;
;; The spec names NO integration consumer (no gptel symbol): it only
;; registers anonymous `:menu' entries on the generic
;; `workspace--integrations' alist, honouring the publish/attach
;; directionality boundary.

(require 'buttercup)
(require 'cl-lib)

(defconst transient-menu-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

;; Dependency order: data-model (accessors + broken-p), integrations
;; (registry + payload), then the transient under test.
(load (expand-file-name "../data-model.el" transient-menu-spec--this-dir))
(load (expand-file-name "../integrations.el" transient-menu-spec--this-dir))
(load (expand-file-name "../workspaces-transient.el" transient-menu-spec--this-dir))

(describe "workspaces-transient menu"

  ;; Fixtures: a healthy and a broken workspace plist, plus a registry
  ;; mapping their names.  Tests stub the *current name* and *broken-p*
  ;; per scenario.
  (let* ((healthy-ws (list :name "alpha" :home "/tmp/alpha"))
         (broken-ws  (list :name "beta"  :home "/tmp/beta"))
         (registry   (let ((h (make-hash-table :test 'equal)))
                       (puthash "alpha" healthy-ws h)
                       (puthash "beta"  broken-ws h)
                       h)))

    (describe "context predicates"

      (describe "with no current workspace"
        ;; `workspace--current-name' returns nil → no workspace at all.
        (it "reports healthy-p, broken-p, and in-ws-p all nil"
          (cl-letf (((symbol-function 'workspace--current-name) (lambda () nil))
                    (workspace--registry registry))
            (expect (workspace--menu-healthy-p) :to-be nil)
            (expect (workspace--menu-broken-p)  :to-be nil)
            (expect (workspace--menu-in-ws-p)   :to-be nil))))

      (describe "with a healthy current workspace"
        (it "reports in-ws-p and healthy-p, not broken-p"
          (cl-letf (((symbol-function 'workspace--current-name) (lambda () "alpha"))
                    ((symbol-function 'workspace--broken-p) (lambda (_) nil))
                    (workspace--registry registry))
            (expect (workspace--menu-in-ws-p)   :to-be-truthy)
            (expect (workspace--menu-healthy-p) :to-be-truthy)
            (expect (workspace--menu-broken-p)  :to-be nil))))

      (describe "with a broken current workspace"
        (it "reports in-ws-p and broken-p, not healthy-p"
          (cl-letf (((symbol-function 'workspace--current-name) (lambda () "beta"))
                    ((symbol-function 'workspace--broken-p) (lambda (_) t))
                    (workspace--registry registry))
            (expect (workspace--menu-in-ws-p)   :to-be-truthy)
            (expect (workspace--menu-broken-p)  :to-be-truthy)
            ;; healthy-p nil → operational + Integrations groups suppressed;
            ;; in-ws-p truthy → the Manage / Recover group stays visible.
            (expect (workspace--menu-healthy-p) :to-be nil)))))

    (describe "Integrations children builder"

      (it "yields no suffixes with no current workspace and empty registry"
        (cl-letf (((symbol-function 'workspace--current-name) (lambda () nil))
                  (workspace--registry registry))
          (let ((workspace--integrations nil))
            (expect (workspace--menu-integration-children) :to-be nil))))

      (it "yields no suffixes when registered integrations expose no :menu"
        ;; on-create-only integrations contribute nothing to the menu.
        (let ((workspace--integrations nil))
          (workspace-register-integration 'on-create-only
                                          :label "Headless"
                                          :on-create #'ignore)
          (expect (workspace--menu-integration-children) :to-be nil)))

      (it "yields one suffix per :menu integration, in registration order"
        (let ((workspace--integrations nil))
          (workspace-register-integration 'first
                                          :label "First"
                                          :menu (cons "f" #'ignore))
          (workspace-register-integration 'second
                                          :label "Second"
                                          :menu (cons "g" #'ignore))
          (let ((children (workspace--menu-integration-children)))
            (expect (length children) :to-equal 2)
            ;; spec shape is (KEY DESC THUNK)
            (expect (nth 0 (nth 0 children)) :to-equal "f")
            (expect (nth 1 (nth 0 children)) :to-equal "First")
            (expect (functionp (nth 2 (nth 0 children))) :to-be-truthy)
            (expect (nth 0 (nth 1 children)) :to-equal "g")
            (expect (nth 1 (nth 1 children)) :to-equal "Second"))))

      (it "skips on-create-only entries but keeps :menu entries"
        (let ((workspace--integrations nil))
          (workspace-register-integration 'headless
                                          :label "Headless"
                                          :on-create #'ignore)
          (workspace-register-integration 'visible
                                          :label "Visible"
                                          :menu (cons "v" #'ignore))
          (let ((children (workspace--menu-integration-children)))
            (expect (length children) :to-equal 1)
            (expect (nth 0 (nth 0 children)) :to-equal "v"))))

      (it "falls back to the integration id when :label is absent"
        (let ((workspace--integrations nil))
          (workspace-register-integration 'no-label
                                          :menu (cons "x" #'ignore))
          (let ((children (workspace--menu-integration-children)))
            (expect (nth 1 (nth 0 children)) :to-equal "no-label")))))

    (describe "integration suffix invocation"

      (it "calls the integration command on a menu-invoke payload built from the current workspace"
        (let ((captured nil)
              (workspace--integrations nil))
          (cl-letf (((symbol-function 'workspace--current-name) (lambda () "alpha"))
                    ((symbol-function 'workspace--broken-p) (lambda (_) nil))
                    (workspace--registry registry))
            (workspace-register-integration
             'capturing
             :label "Capture"
             :menu (cons "c" (lambda (payload) (setq captured payload))))
            (let* ((children (workspace--menu-integration-children))
                   (thunk (nth 2 (nth 0 children))))
              (funcall thunk)
              (expect (plist-get captured :name) :to-equal "alpha")
              (expect (plist-get captured :home) :to-equal "/tmp/alpha")
              (expect (plist-get captured :context) :to-equal 'menu-invoke)))))

      (it "signals a user-error when invoked outside a workspace"
        (let ((workspace--integrations nil))
          (cl-letf (((symbol-function 'workspace--current-name) (lambda () nil))
                    (workspace--registry registry))
            (expect (workspace--menu-invoke-integration #'ignore)
                    :to-throw 'user-error)))))))

;;; transient-menu-spec.el ends here
