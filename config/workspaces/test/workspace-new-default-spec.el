;;; workspace-new-default-spec.el --- Tests for workspace-new default-path scaffold flow -*- lexical-binding: t; -*-

;; Cycle-3 task `workspace-new-default-path' covers:
;;
;;   - the `workspaces-default-parent-directory' defcustom resolves
;;     the workspace's home directory under the configured parent;
;;   - `workspace-new' (no prefix arg) scaffolds via the cycle-2
;;     `workspace-scaffold' pipeline (=register/boundary/workspace-
;;     scaffold-pipeline=) BEFORE registering the workspace or
;;     creating a tab;
;;   - collision against an existing directory is a hard `user-error'
;;     (=register/invariant/scaffold-leave-partial-on-failure= +
;;     design D2);
;;   - scaffold failure leaves the registry empty and no tab created;
;;   - the default home builder opens =<:home>/home.org= via the
;;     home-org read pipeline.
;;
;; The scaffold pipeline runs git in a tmpdir, which is acceptable
;; (and exercised in scaffold-spec.el's happy path).  We use it
;; directly for the happy-path case so end-to-end wiring is
;; exercised; failure cases stub the pipeline to inject the user-error.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../home-org.el"   dir))
  (load (expand-file-name "../scaffold.el"   dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../workspaces.el"        dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar wnd-spec--tmp-parent nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun wnd-spec--reset ()
  "Reset tab-bar, registry, and per-test tmpdir."
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq wnd-spec--tmp-parent (make-temp-file "ws-new-default-" t)
        workspaces-default-parent-directory wnd-spec--tmp-parent))

(defun wnd-spec--cleanup ()
  "Remove the per-test tmpdir tree if it still exists."
  (when (and wnd-spec--tmp-parent
             (file-directory-p wnd-spec--tmp-parent))
    (delete-directory wnd-spec--tmp-parent t)))

(describe "workspace-new default-path happy path"
  (before-each
    (wnd-spec--reset)
    ;; Spy on tab-bar so we can assert it was called (and avoid
    ;; touching the live tab-bar state — close out cleanup is enough).
    (spy-on 'tab-bar-new-tab :and-call-through)
    (spy-on 'tab-bar-rename-tab :and-call-through))
  (after-each (wnd-spec--cleanup))

  (it "scaffolds <parent>/<name>/, creates a tab, and registers with :home set"
    (workspace-new "foo")
    ;; Scaffold ran end-to-end: directory exists, contains home.org
    ;; and sessions/, and is a git repo (init-and-commit? t).
    (let ((home (expand-file-name "foo" wnd-spec--tmp-parent)))
      (expect (file-directory-p home) :to-be t)
      (expect (file-exists-p (expand-file-name "home.org" home))
              :to-be t)
      (expect (file-directory-p (expand-file-name "sessions" home))
              :to-be t)
      (expect (file-directory-p (expand-file-name ".git" home))
              :to-be t)
      ;; Tab created.
      (expect 'tab-bar-new-tab :to-have-been-called)
      ;; Registry has foo with :home set to the canonicalised resolved
      ;; path.  Per register/shape/workspace-plist-v3 the constructor
      ;; pins :home to (file-name-as-directory (expand-file-name ...)),
      ;; so the registered :home carries a trailing slash even though
      ;; the default-path call-site computed home via expand-file-name
      ;; (no trailing slash).  See cycle-5 task
      ;; canonicalize-workspace-home-path-form.
      (let ((ws (gethash "foo" workspace--registry)))
        (expect ws :not :to-be nil)
        (expect (workspace--name ws) :to-equal "foo")
        (expect (workspace--home ws)
                :to-equal (file-name-as-directory home))))))

(describe "workspace-new default-path collision"
  (before-each
    (wnd-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    ;; Pre-create the target directory.
    (make-directory (expand-file-name "foo" wnd-spec--tmp-parent) t))
  (after-each (wnd-spec--cleanup))

  (it "signals user-error, leaves registry unchanged, and does not create a tab"
    (let ((tabs-before (length (funcall tab-bar-tabs-function))))
      (expect (workspace-new "foo") :to-throw 'user-error)
      ;; Registry still has no foo entry.
      (expect (gethash "foo" workspace--registry) :to-be nil)
      ;; No tab was created.
      (expect 'tab-bar-new-tab :not :to-have-been-called)
      (expect (length (funcall tab-bar-tabs-function))
              :to-equal tabs-before))))

(describe "workspace-new default-path scaffold failure"
  (before-each
    (wnd-spec--reset)
    (spy-on 'tab-bar-new-tab :and-call-through)
    ;; Stub workspace-scaffold to fail.  workspace-new must propagate
    ;; the user-error and leave the registry / tab-bar untouched.
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (&rest _) (user-error "Simulated scaffold failure"))))
  (after-each (wnd-spec--cleanup))

  (it "propagates the user-error, leaves registry empty, and creates no tab"
    (let ((tabs-before (length (funcall tab-bar-tabs-function))))
      (expect (workspace-new "foo") :to-throw 'user-error)
      (expect (gethash "foo" workspace--registry) :to-be nil)
      (expect 'tab-bar-new-tab :not :to-have-been-called)
      (expect (length (funcall tab-bar-tabs-function))
              :to-equal tabs-before))))

(describe "workspace-default-home-builder"
  (before-each (wnd-spec--reset))
  (after-each (wnd-spec--cleanup))

  (it "find-files <:home>/home.org for the named workspace"
    ;; Create a workspace via workspace-new (real scaffold) so home.org
    ;; exists with the expected #+TITLE skeleton.
    (workspace-new "foo")
    (let* ((home (expand-file-name "foo" wnd-spec--tmp-parent))
           (homeorg-path (expand-file-name "home.org" home)))
      ;; The default builder ran during workspace-new and should have
      ;; left the home.org buffer current.  Compare via file-truename
      ;; to neutralise symlink-resolved tmpdir prefixes (e.g. macOS
      ;; /var/folders ↔ /private/var/folders).
      (expect (file-truename (buffer-file-name))
              :to-equal (file-truename homeorg-path))))

  (it "falls back to *scratch* when the workspace has no :home"
    ;; Defensive branch: register a workspace plist without :home and
    ;; invoke the builder directly.  In practice this should never
    ;; happen (=register/invariant/home-required-no-floating-workspaces=),
    ;; but the fallback exists so the builder cannot crash.
    (puthash "bare" (list :name "bare") workspace--registry)
    (workspace-default-home-builder "bare")
    (expect (buffer-name) :to-equal "*scratch*")))

(provide 'workspace-new-default-spec)
;;; workspace-new-default-spec.el ends here
