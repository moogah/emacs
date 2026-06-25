;;; integration-dispatch-spec.el --- Tests for creation-time integration dispatch -*- lexical-binding: t; -*-

;; Task `wire-create-dispatch' covers the wiring of
;; `workspace--dispatch-create-integrations' into BOTH workspace
;; creation entry points:
;;
;;   - `workspace--new-default-path' fires dispatch once with context
;;     `fresh', AFTER registration and the initial commit;
;;   - `workspace--new-anchor-existing' fires dispatch once per
;;     sub-case, AFTER registration, with the correct per-case
;;     context:
;;       case 1 (repo + home.org, register-only) → anchored-existing
;;       case 2 (repo, no home.org, scaffold no-commit) → anchored-scaffolded
;;       case 3 (non-repo, full scaffold) → fresh
;;
;; Behavioral invariants (design Decision 4):
;;   - a handler that signals does NOT abort creation: the workspace
;;     is registered, a tab exists, a *Messages* notice is emitted
;;     (surfaced via `message'), and a SECOND handler still runs;
;;   - a mid-pipeline scaffold failure leaves no registry entry and
;;     runs NO :on-create handler (dispatch is downstream of the
;;     fail-fast scaffold + registration).
;;
;; This spec names NO gptel symbol: it registers fake generic
;; integrations only (the directionality contract — workspaces
;; publishes the dispatch, integrations attach to it).

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el"   dir))
  (load (expand-file-name "../integrations.el" dir))
  (load (expand-file-name "../home-org.el"     dir))
  (load (expand-file-name "../scaffold.el"     dir))
  (load (expand-file-name "../tabs.el"         dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../workspaces.el"        dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar idsp--tmp nil
  "Per-test temp directory.")

(defvar idsp--saved-parent 'unset
  "Saved `workspaces-default-parent-directory' for restore.")

;;; Helpers ---------------------------------------------------------

(defun idsp--git (dir &rest args)
  "Run git ARGS inside DIR; signal on non-zero exit."
  (let* ((default-directory (file-name-as-directory dir))
         (status (apply #'call-process "git" nil nil nil args)))
    (unless (zerop status)
      (error "git %s failed in %s (status %d)"
             (mapconcat #'identity args " ") dir status))))

(defun idsp--init-repo (dir)
  "Make DIR a git repo with one initial commit so HEAD exists."
  (idsp--git dir "init" "-q")
  (idsp--git dir "config" "user.email" "test@example.com")
  (idsp--git dir "config" "user.name"  "Test User")
  (idsp--git dir "commit" "--allow-empty" "-q" "-m" "initial"))

(defun idsp--reset ()
  "Reset tab-bar, registry, integration registry, and tmpdir."
  (clrhash workspace--registry)
  (setq workspace--integrations nil)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (when (eq idsp--saved-parent 'unset)
    (setq idsp--saved-parent workspaces-default-parent-directory))
  (setq idsp--tmp (make-temp-file "ws-dispatch-" t)
        workspaces-default-parent-directory idsp--tmp))

(defun idsp--cleanup ()
  "Remove the per-test tmpdir tree and restore the defcustom."
  (when (and idsp--tmp (file-directory-p idsp--tmp))
    (delete-directory idsp--tmp t))
  (setq idsp--tmp nil)
  (unless (eq idsp--saved-parent 'unset)
    (setq workspaces-default-parent-directory idsp--saved-parent
          idsp--saved-parent 'unset)))

(defun idsp--call-anchor (home)
  "Invoke `workspace-new' with a prefix arg, stubbing the directory
prompt to return HOME."
  (cl-letf (((symbol-function 'read-directory-name)
             (lambda (&rest _) home)))
    (workspace-new "ignored" t)))

;;; Dispatch fires with the correct context per path ---------------

(describe "creation-time integration dispatch — context per birth path"
  (before-each
    (idsp--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through))
  (after-each (idsp--cleanup))

  (it "fires once with `fresh' from the default-path branch"
    (let ((seen nil))
      (workspace-register-integration
       'probe :on-create (lambda (p) (push p seen) 'ok))
      (workspace-new "foo")
      (expect (length seen) :to-equal 1)
      (let ((payload (car seen)))
        (expect (plist-get payload :name) :to-equal "foo")
        (expect (plist-get payload :context) :to-equal 'fresh)
        ;; :home is the resolved scaffold directory.
        (expect (file-equal-p (plist-get payload :home)
                              (expand-file-name "foo" idsp--tmp))
                :to-be t))))

  (it "fires once with `anchored-existing' for case 1 (repo + home.org)"
    (let* ((home (file-name-as-directory
                  (expand-file-name "alpha" idsp--tmp)))
           (seen nil))
      (make-directory home t)
      (idsp--init-repo home)
      (with-temp-file (expand-file-name "home.org" home)
        (insert "#+TITLE: alpha\n* Pre-existing\n"))
      (workspace-register-integration
       'probe :on-create (lambda (p) (push p seen) 'ok))
      (idsp--call-anchor home)
      (expect (length seen) :to-equal 1)
      (expect (plist-get (car seen) :name) :to-equal "alpha")
      (expect (plist-get (car seen) :context) :to-equal 'anchored-existing)))

  (it "fires once with `anchored-scaffolded' for case 2 (repo, no home.org)"
    (let* ((home (file-name-as-directory
                  (expand-file-name "beta" idsp--tmp)))
           (seen nil))
      (make-directory home t)
      (idsp--init-repo home)
      (workspace-register-integration
       'probe :on-create (lambda (p) (push p seen) 'ok))
      (idsp--call-anchor home)
      (expect (length seen) :to-equal 1)
      (expect (plist-get (car seen) :name) :to-equal "beta")
      (expect (plist-get (car seen) :context) :to-equal 'anchored-scaffolded)))

  (it "fires once with `fresh' for case 3 (non-repo full scaffold)"
    (let* ((home (file-name-as-directory
                  (expand-file-name "gamma" idsp--tmp)))
           (seen nil))
      (make-directory home t)
      (workspace-register-integration
       'probe :on-create (lambda (p) (push p seen) 'ok))
      (idsp--call-anchor home)
      (expect (length seen) :to-equal 1)
      (expect (plist-get (car seen) :name) :to-equal "gamma")
      (expect (plist-get (car seen) :context) :to-equal 'fresh))))

;;; A throwing handler is additive-never-load-bearing --------------

(describe "creation-time integration dispatch — failure is never fatal"
  (before-each
    (idsp--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through))
  (after-each (idsp--cleanup))

  (it "a handler that errors does not abort creation; a second handler still runs"
    (let ((second-ran nil))
      ;; First integration explodes; second must still run, and the
      ;; workspace must still be created.
      (workspace-register-integration
       'boom :on-create (lambda (_p) (error "kaboom")))
      (workspace-register-integration
       'after :on-create (lambda (_p) (setq second-ran t) 'ok))
      (spy-on 'message :and-call-through)
      ;; Creation completes without signalling.
      (expect (workspace-new "foo") :not :to-throw)
      ;; Workspace exists in the registry...
      (expect (gethash "foo" workspace--registry) :not :to-be nil)
      ;; ...with a tab present.
      (expect 'tab-bar-new-tab :to-have-been-called)
      (expect (workspace--tab-for "foo") :not :to-be nil)
      ;; The second handler still ran.
      (expect second-ran :to-be t)
      ;; A *Messages* notice was emitted naming the failed integration.
      (expect 'message :to-have-been-called-with
              "workspace: integration %s failed: %s" 'boom "kaboom"))))

;;; A mid-scaffold failure runs no handler -------------------------

(describe "creation-time integration dispatch — scaffold failure runs no handler"
  (before-each
    (idsp--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    ;; Make the scaffold pipeline fail mid-flight.  Dispatch is
    ;; downstream of scaffold + registration, so it must not run.
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (&rest _) (user-error "Simulated scaffold failure"))))
  (after-each (idsp--cleanup))

  (it "leaves no registry entry and runs no :on-create when scaffold fails"
    (let ((ran nil))
      (workspace-register-integration
       'probe :on-create (lambda (_p) (setq ran t) 'ok))
      (expect (workspace-new "foo") :to-throw 'user-error)
      ;; No registry entry — registration is downstream of scaffold.
      (expect (gethash "foo" workspace--registry) :to-be nil)
      ;; No tab created.
      (expect 'tab-bar-new-tab :not :to-have-been-called)
      ;; The :on-create handler never fired.
      (expect ran :to-be nil))))

(provide 'integration-dispatch-spec)
;;; integration-dispatch-spec.el ends here
