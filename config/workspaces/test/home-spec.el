;;; home-spec.el --- Tests for the workspace home builder -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defun home-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq workspaces-default-parent-directory
        (make-temp-file "ws-home-spec-" t)))

(describe "workspace-home-builder"
  (before-each (home-spec--reset))

  (it "invokes the configured builder with the new workspace name"
    (let* ((calls nil)
           (workspace-home-builder
            (lambda (name) (push name calls))))
      (workspace-new "writing")
      (expect calls :to-equal '("writing"))))

  (it "runs the builder in the context of the newly-selected tab"
    (let* ((current-when-builder-ran nil)
           (workspace-home-builder
            (lambda (_name)
              (setq current-when-builder-ran (workspace--current-name)))))
      (workspace-new "context-check")
      (expect current-when-builder-ran :to-equal "context-check"))))

(describe "home reserved-name policy"
  (it "treats `home' as reserved at the data layer"
    ;; This is the pure-data check exercised end-to-end in layouts-spec.el
    ;; once `workspace-delete-layout' lands (next task).
    (expect (workspace--group-name-reserved-p "home") :to-be t))

  (it "Home layout cannot be deleted via `workspace-delete-layout'"
    (home-spec--reset)
    (workspace-new "writing")
    (expect (workspace-delete-layout "home") :to-throw 'user-error)
    (expect (workspace--find-group
             (gethash "writing" workspace--registry) "home")
            :not :to-be nil))

  (it "Re-saving `home' overwrites without invoking the builder"
    (home-spec--reset)
    (let* ((builder-calls 0)
           (workspace-home-builder
            (lambda (_name) (cl-incf builder-calls))))
      (workspace-new "writing")
      (expect builder-calls :to-equal 1)
      ;; Sleep so the second layout has a strictly later timestamp.
      (sleep-for 1.1)
      (workspace-save-layout "home")
      ;; The builder runs at create time only; resaving must not retrigger.
      (expect builder-calls :to-equal 1)
      (let* ((ws (gethash "writing" workspace--registry))
             (home-group (workspace--find-group ws "home")))
        (expect home-group :not :to-be nil)
        (expect (length (workspace--group-layouts home-group)) :to-equal 1)))))

(provide 'home-spec)
;;; home-spec.el ends here
