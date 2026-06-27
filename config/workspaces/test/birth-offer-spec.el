;;; birth-offer-spec.el --- Tests for workspace-new birth-transient-offer -*- lexical-binding: t; -*-

;; Task `birth-transient-offer' covers:
;;
;;   - After a workspace is born and its tab is current,
;;     `workspace-new' raises the `C-x w' front-door transient
;;     (`workspace-menu') so the user can immediately reach the
;;     registry-driven Integrations group (e.g. add a git worktree);
;;   - the offer is gated STRICTLY on the workspace-creation context
;;     (=register/vocabulary/workspace-creation-context=): it fires for
;;     `fresh' and `anchored-scaffolded', and NEVER for
;;     `anchored-existing' (an adopted directory the user already owned);
;;   - `workspace-new' names NO integration — it only raises the generic
;;     front door (=register/boundary/workspace-integration-registry=);
;;   - the offer is a no-op under `noninteractive' (batch) so the test
;;     runner never launches a real transient;
;;   - declining/dismissing the menu leaves a valid registered workspace
;;     with a live tab (the menu pop is purely additive).
;;
;; The helper-under-test is `workspace--offer-birth-menu'.  The two
;; creation branches (`workspace--new-default-path' and
;; `workspace--new-anchor-existing') call it with the per-case context.
;; We spy on the helper to assert it is invoked exactly once per birth
;; with the correct context, and we exercise the helper directly to
;; assert its strict gate and its `noninteractive' no-op.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el"   dir))
  (load (expand-file-name "../home-org.el"     dir))
  (load (expand-file-name "../scaffold.el"     dir))
  (load (expand-file-name "../integrations.el" dir))
  (load (expand-file-name "../tabs.el"         dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../workspaces.el"        dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar bo-spec--tmp-dir nil
  "Per-test temp directory tree used for births.")

(defvar bo-spec--saved-default-parent 'unset
  "Captured `workspaces-default-parent-directory' for restore.")

(defun bo-spec--git (dir &rest args)
  "Run git with ARGS inside DIR; signal on non-zero exit."
  (let* ((default-directory (file-name-as-directory dir))
         (status (apply #'call-process "git" nil nil nil args)))
    (unless (zerop status)
      (error "git %s failed in %s (status %d)"
             (mapconcat #'identity args " ") dir status))))

(defun bo-spec--init-repo (dir)
  "Make DIR a git repo with one commit so HEAD exists."
  (bo-spec--git dir "init" "-q")
  (bo-spec--git dir "config" "user.email" "test@example.com")
  (bo-spec--git dir "config" "user.name"  "Test User")
  (bo-spec--git dir "commit" "--allow-empty" "-q" "-m" "initial"))

(defun bo-spec--reset ()
  "Reset tab-bar + registry; allocate a fresh tmpdir."
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (when (eq bo-spec--saved-default-parent 'unset)
    (setq bo-spec--saved-default-parent workspaces-default-parent-directory))
  (setq bo-spec--tmp-dir (make-temp-file "ws-birth-offer-" t)
        workspaces-default-parent-directory bo-spec--tmp-dir))

(defun bo-spec--cleanup ()
  "Remove the per-test tmpdir tree; restore the defcustom."
  (when (and bo-spec--tmp-dir (file-directory-p bo-spec--tmp-dir))
    (delete-directory bo-spec--tmp-dir t))
  (setq bo-spec--tmp-dir nil)
  (unless (eq bo-spec--saved-default-parent 'unset)
    (setq workspaces-default-parent-directory bo-spec--saved-default-parent
          bo-spec--saved-default-parent 'unset)))

(defun bo-spec--call-anchor (home)
  "Invoke the anchor-existing flow, stubbing the directory prompt to HOME."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) home)))
    (workspace-new "ignored" t)))

;;; ---------------------------------------------------------------------------
;;; Per-birth dispatch: the helper is invoked once with the right context.
;;; ---------------------------------------------------------------------------

(describe "birth-transient-offer — fresh (default-path) birth"
  (before-each
    (bo-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    ;; Spy through so the real helper runs (and no-ops under
    ;; `noninteractive'); we only observe the call + its argument.
    (spy-on 'workspace--offer-birth-menu :and-call-through))
  (after-each (bo-spec--cleanup))

  (it "offers the birth menu exactly once, with the `fresh' context"
    (workspace-new "alpha")
    (expect 'workspace--offer-birth-menu :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'workspace--offer-birth-menu 0)
            :to-equal '(fresh)))

  (it "leaves a valid registered workspace with a live tab (menu is additive)"
    (workspace-new "alpha")
    (let ((ws (gethash "alpha" workspace--registry)))
      (expect ws :not :to-be nil)
      (expect (workspace--name ws) :to-equal "alpha")
      (expect (workspace--tab-for "alpha") :not :to-be nil))))

(describe "birth-transient-offer — anchored-scaffolded birth (repo, no home.org)"
  (before-each
    (bo-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace--offer-birth-menu :and-call-through))
  (after-each (bo-spec--cleanup))

  (it "offers the birth menu exactly once, with `anchored-scaffolded'"
    (let ((home (file-name-as-directory
                 (expand-file-name "beta" bo-spec--tmp-dir))))
      (make-directory home t)
      (bo-spec--init-repo home)
      (bo-spec--call-anchor home)
      (expect 'workspace--offer-birth-menu :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'workspace--offer-birth-menu 0)
              :to-equal '(anchored-scaffolded))
      ;; Workspace is registered with a live tab.
      (let ((ws (gethash "beta" workspace--registry)))
        (expect ws :not :to-be nil)
        (expect (workspace--tab-for "beta") :not :to-be nil)))))

(describe "birth-transient-offer — anchored-existing adoption (repo + home.org)"
  (before-each
    (bo-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through)
    (spy-on 'workspace--offer-birth-menu :and-call-through)
    ;; A real (stubbed) front door so we can prove it is NOT raised.
    (spy-on 'workspace-menu))
  (after-each (bo-spec--cleanup))

  (it "still calls the helper but the helper does NOT raise the menu"
    (let* ((home (file-name-as-directory
                  (expand-file-name "gamma" bo-spec--tmp-dir))))
      (make-directory home t)
      (bo-spec--init-repo home)
      (with-temp-file (expand-file-name "home.org" home)
        (insert "#+TITLE: gamma\n* Pre-existing\n"))
      (bo-spec--call-anchor home)
      ;; The branch always calls the helper, passing `anchored-existing'.
      (expect 'workspace--offer-birth-menu :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'workspace--offer-birth-menu 0)
              :to-equal '(anchored-existing))
      ;; But the menu front door is NEVER raised for an adopted dir.
      (expect 'workspace-menu :not :to-have-been-called)
      ;; And the adopted workspace is registered with a live tab.
      (let ((ws (gethash "gamma" workspace--registry)))
        (expect ws :not :to-be nil)
        (expect (workspace--tab-for "gamma") :not :to-be nil)))))

;;; ---------------------------------------------------------------------------
;;; The helper's strict gate + noninteractive no-op, exercised directly.
;;; ---------------------------------------------------------------------------

(describe "workspace--offer-birth-menu strict context gate"
  ;; Force the interactive path so the gate — not the `noninteractive'
  ;; guard — is what decides.  A stubbed `workspace-menu' records calls.
  (it "raises the menu for `fresh'"
    (spy-on 'workspace-menu)
    (let ((noninteractive nil))
      (workspace--offer-birth-menu 'fresh))
    (expect 'workspace-menu :to-have-been-called-times 1))

  (it "raises the menu for `anchored-scaffolded'"
    (spy-on 'workspace-menu)
    (let ((noninteractive nil))
      (workspace--offer-birth-menu 'anchored-scaffolded))
    (expect 'workspace-menu :to-have-been-called-times 1))

  (it "does NOT raise the menu for `anchored-existing'"
    (spy-on 'workspace-menu)
    (let ((noninteractive nil))
      (workspace--offer-birth-menu 'anchored-existing))
    (expect 'workspace-menu :not :to-have-been-called))

  (it "is a no-op under `noninteractive' even for `fresh'"
    (spy-on 'workspace-menu)
    (let ((noninteractive t))
      (workspace--offer-birth-menu 'fresh))
    (expect 'workspace-menu :not :to-have-been-called)))

(provide 'birth-offer-spec)
;;; birth-offer-spec.el ends here
