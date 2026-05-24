;;; revert-spec.el --- Behavioral tests for workspace-revert -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar revert-spec--tmp-dir nil
  "Per-test temp directory backing the persistence file.")

(defun revert-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (delete-other-windows)
  (setq revert-spec--tmp-dir
        (make-temp-file "ws-revert-" t)))

(defun revert-spec--cleanup ()
  (when (and revert-spec--tmp-dir
             (file-directory-p revert-spec--tmp-dir))
    (delete-directory revert-spec--tmp-dir t)))

(defmacro revert-spec--with-state-file (&rest body)
  (declare (indent 0))
  `(let ((tmp revert-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun revert-spec--drain-timers ()
  (sit-for 0)
  (sit-for 0)
  (accept-process-output nil 0.05))

(describe "workspace-revert"
  (before-each (revert-spec--reset))
  (after-each (revert-spec--cleanup))

  (it "clears :working-state and re-applies :saved-state"
    (revert-spec--with-state-file
      (workspace-new "alpha")
      ;; Establish a :saved-state baseline.
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout-before (workspace--group-recent-layout
                             (workspace--find-group ws "home")))
             (saved-marker (workspace--layout-saved-state layout-before)))
        ;; Accumulate :working-state drift.
        (workspace--autosave-current-layout :working-state)
        (let* ((ws (gethash "alpha" workspace--registry))
               (layout (workspace--group-recent-layout
                        (workspace--find-group ws "home"))))
          (expect (workspace--layout-working-state layout) :not :to-be nil))
        ;; Revert.
        (workspace-revert)
        (revert-spec--drain-timers)
        (let* ((ws (gethash "alpha" workspace--registry))
               (layout (workspace--group-recent-layout
                        (workspace--find-group ws "home"))))
          (expect (workspace--layout-working-state layout) :to-be nil)
          ;; :saved-state preserved.
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-marker)))))

  (it "errors when not on a workspace tab"
    (revert-spec--with-state-file
      ;; Tab 1 is the initial non-workspace tab; we're on it.
      (expect (workspace-revert) :to-throw 'user-error)))

  (it "is a no-op when :working-state is already nil"
    (revert-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :to-be nil))
      ;; Calling revert is harmless.
      (expect (workspace-revert) :not :to-throw)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :to-be nil)
        (expect (workspace--layout-saved-state layout) :not :to-be nil))))

  (it "clears the file-on-disk :working-state after revert"
    (revert-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      ;; Populate :working-state and flush to disk.
      (workspace--autosave-current-layout :working-state)
      (workspace--flush-state)
      ;; Sanity: disk state has :working-state non-nil.
      (let* ((state (workspace--read-state))
             (workspaces (plist-get state :workspaces))
             (ws (seq-find (lambda (w) (equal (workspace--name w) "alpha"))
                           workspaces))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :not :to-be nil))
      ;; Revert; disk reflects cleared :working-state.
      (workspace-revert)
      (revert-spec--drain-timers)
      (let* ((state (workspace--read-state))
             (workspaces (plist-get state :workspaces))
             (ws (seq-find (lambda (w) (equal (workspace--name w) "alpha"))
                           workspaces))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :to-be nil)))))

(provide 'revert-spec)
;;; revert-spec.el ends here
