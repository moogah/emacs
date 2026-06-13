;;; anti-save-spec.el --- Behavioral tests for workspace anti-save predicates -*- lexical-binding: t; -*-

;; Covers register/invariant/explicit-save-bypasses-anti-save and the
;; stage-1 anti-save-check of register/boundary/autosave-guard-pipeline.

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
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir))
  ;; workspaces.el carries the defcustom + helper.
  (load (expand-file-name "../workspaces.el"        dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)
(defvar jf/emacs-dir)
(defvar jf/machine-role)

(defvar anti-save-spec--tmp-dir nil
  "Per-suite temp directory backing the persistence file.")

(defvar anti-save-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun anti-save-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq anti-save-spec--tmp-dir
        (make-temp-file "ws-anti-save-" t))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq anti-save-spec--parent-dir
        (make-temp-file "ws-anti-save-spec-" t)
        workspaces-default-parent-directory anti-save-spec--parent-dir))

(defun anti-save-spec--cleanup ()
  (when (and anti-save-spec--tmp-dir
             (file-directory-p anti-save-spec--tmp-dir))
    (delete-directory anti-save-spec--tmp-dir t))
  (when (and anti-save-spec--parent-dir
             (file-directory-p anti-save-spec--parent-dir))
    (delete-directory anti-save-spec--parent-dir t)))

(defmacro anti-save-spec--with-state-file (&rest body)
  "Run BODY with `workspace--state-file' pointed at a temp directory."
  (declare (indent 0))
  `(let ((tmp anti-save-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun anti-save-spec--layout-for (ws-name)
  "Return the recent layout plist for the workspace named WS-NAME."
  (let* ((ws (gethash ws-name workspace--registry))
         (group-name (workspace--recent-group ws))
         (group (and group-name (workspace--find-group ws group-name))))
    (and group (workspace--group-recent-layout group))))

(describe "Default workspace-anti-save-predicates list"
  (it "contains active-minibuffer-window and workspace--backtrace-visible-p"
    (expect (memq 'active-minibuffer-window workspace-anti-save-predicates)
            :to-be-truthy)
    (expect (memq 'workspace--backtrace-visible-p workspace-anti-save-predicates)
            :to-be-truthy))

  (it "all entries are funcallable"
    (dolist (pred workspace-anti-save-predicates)
      (expect (functionp pred) :to-be-truthy))))

(describe "workspace--backtrace-visible-p"
  (it "returns nil when no *Backtrace* window is visible"
    (cl-letf (((symbol-function 'window-list)
               (lambda (&optional _frame _minibuf _window) nil)))
      (expect (workspace--backtrace-visible-p) :to-be nil)))

  (it "returns non-nil when a window shows the *Backtrace* buffer"
    (let ((bt (get-buffer-create "*Backtrace*")))
      (unwind-protect
          (cl-letf* ((fake-window (cons 'fake-window nil))
                     ((symbol-function 'window-list)
                      (lambda (&optional _frame _minibuf _window)
                        (list fake-window)))
                     ((symbol-function 'window-buffer)
                      (lambda (w)
                        (if (eq w fake-window) bt (current-buffer)))))
            (expect (workspace--backtrace-visible-p) :to-be-truthy))
        (kill-buffer bt)))))

(describe "Anti-save predicates gate tab-switch autosave"
  (before-each (anti-save-spec--reset))
  (after-each (anti-save-spec--cleanup))

  (it "minibuffer activity blocks the working-state snapshot"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      ;; Baseline: no :working-state yet.
      (let ((layout (anti-save-spec--layout-for "alpha")))
        (expect (workspace--layout-working-state layout) :to-be nil))
      ;; Stub `active-minibuffer-window' to claim the minibuffer is open.
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () 'pretend-minibuffer-window)))
        (delete-other-windows)
        (split-window-right)
        (tab-bar-select-tab 1))
      ;; The autosave was skipped — :working-state still nil,
      ;; :saved-state untouched.
      (let ((layout (anti-save-spec--layout-for "alpha")))
        (expect (workspace--layout-working-state layout) :to-be nil)
        (expect (workspace--layout-saved-state layout) :not :to-be nil))))

  (it "backtrace visibility blocks the working-state snapshot"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((saved-before (workspace--layout-saved-state
                           (anti-save-spec--layout-for "alpha"))))
        ;; Stub the predicate itself rather than its dependencies; mocking
        ;; `window-list' globally would corrupt the tab-bar machinery
        ;; (which itself walks windows during the switch).
        (cl-letf (((symbol-function 'workspace--backtrace-visible-p)
                   (lambda () t)))
          (delete-other-windows)
          (split-window-right)
          (tab-bar-select-tab 1))
        (let ((layout (anti-save-spec--layout-for "alpha")))
          (expect (workspace--layout-working-state layout) :to-be nil)
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-before))))))

(describe "Anti-save predicates gate the kill-emacs flush capture"
  (before-each (anti-save-spec--reset))
  (after-each (anti-save-spec--cleanup))

  (it "skips the working-state capture when a predicate returns non-nil"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((autosave-calls nil))
        (cl-letf* ((workspace-anti-save-predicates (list (lambda () t)))
                   ((symbol-function 'workspace--autosave-current-layout)
                    (lambda (&optional slot) (push slot autosave-calls))))
          (workspace--kill-emacs-flush)
          (expect autosave-calls :to-equal nil)))))

  (it "still flushes the registry to disk even when capture is skipped"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((flush-count 0))
        (cl-letf* ((workspace-anti-save-predicates (list (lambda () t)))
                   ((symbol-function 'workspace--write-state)
                    (lambda (_form) (cl-incf flush-count))))
          (workspace--kill-emacs-flush)
          (expect flush-count :to-equal 1))))))

;;; Mirror of the scaffolded spec: explicit-save-bypasses-anti-save.

(describe "Invariant: explicit-save-bypasses-anti-save"
  (before-each (anti-save-spec--reset))
  (after-each (anti-save-spec--cleanup))

  (it "workspace-save proceeds even when an always-non-nil predicate is registered"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (let ((workspace-anti-save-predicates (list (lambda () t))))
        (workspace-save))
      ;; :saved-state was populated and the state file was written.
      (let ((layout (anti-save-spec--layout-for "alpha")))
        (expect (workspace--layout-saved-state layout) :not :to-be nil)
        (expect (workspace--layout-working-state layout) :to-be nil))
      (expect (file-exists-p (workspace--state-file)) :to-be-truthy)))

  (it "tab-switch autosave IS suppressed by the same predicate"
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((saved-before (workspace--layout-saved-state
                           (anti-save-spec--layout-for "alpha")))
            (workspace-anti-save-predicates (list (lambda () t))))
        (delete-other-windows)
        (split-window-right)
        (tab-bar-select-tab 1)
        ;; :working-state is unchanged (predicate suppressed the
        ;; tab-switch autosave) and :saved-state is intact.
        (let ((layout (anti-save-spec--layout-for "alpha")))
          (expect (workspace--layout-working-state layout) :to-be nil)
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-before)))))

  (it "workspace-save does not consult workspace-anti-save-predicates"
    ;; Structural assertion: instrument the predicate-list lookup and
    ;; verify workspace-save never reads from it.  We do this by
    ;; let-binding the variable to a list whose sole element is a
    ;; predicate that records being called.
    (anti-save-spec--with-state-file
      (workspace-new "alpha")
      (let* ((calls 0)
             (workspace-anti-save-predicates
              (list (lambda () (cl-incf calls) t))))
        (workspace-save)
        ;; The predicate would have been called if workspace-save
        ;; consulted the list via run-hook-with-args-until-success.
        (expect calls :to-equal 0)))))

(provide 'anti-save-spec)
;;; anti-save-spec.el ends here
