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

(defvar revert-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun revert-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (delete-other-windows)
  (setq revert-spec--tmp-dir
        (make-temp-file "ws-revert-" t))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq revert-spec--parent-dir
        (make-temp-file "ws-revert-spec-" t)
        workspaces-default-parent-directory revert-spec--parent-dir))

(defun revert-spec--cleanup ()
  (when (and revert-spec--tmp-dir
             (file-directory-p revert-spec--tmp-dir))
    (delete-directory revert-spec--tmp-dir t))
  (when (and revert-spec--parent-dir
             (file-directory-p revert-spec--parent-dir))
    (delete-directory revert-spec--parent-dir t)))

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

;;; Change workspaces-programmatic-layouts: revert on layouts a program
;;; added (design.md § Scenario coverage).  Pending (`xit') until
;;; `workspace-add-layout' and recipe expansion exist; the hooks use only
;;; what HEAD defines.

(defvar revert-spec--files-dir nil
  "Per-test directory holding the files recipe panes name, or nil.")

(defun revert-spec--write-file (name)
  "Create NAME in the pane-file dir with 60 numbered lines; return its path."
  (let ((path (expand-file-name name revert-spec--files-dir)))
    (with-temp-file path
      (dotimes (i 60) (insert (format "line %d\n" (1+ i)))))
    path))

(defun revert-spec--layout (ws-name layout-name)
  "The layout LAYOUT-NAME of WS-NAME in the registry, or nil."
  (let* ((ws (gethash ws-name workspace--registry))
         (group (and ws (workspace--find-group ws layout-name))))
    (and group (workspace--group-recent-layout group))))

(defun revert-spec--window-files ()
  "Truenames of the files shown by the selected frame's windows, in order."
  (mapcar (lambda (window)
            (let ((file (buffer-file-name (window-buffer window))))
              (and file (file-truename file))))
          (window-list nil 'no-mini (frame-first-window))))

(describe "workspace-revert on a layout a program added"
  (before-each
    (revert-spec--reset)
    (setq revert-spec--files-dir
          (file-name-as-directory
           (file-truename (make-temp-file "ws-revert-files-" t)))))
  (after-each
    (when (and revert-spec--files-dir (file-directory-p revert-spec--files-dir))
      ;; String comparison only: never a file operation on another
      ;; spec's (possibly remote) buffer.
      (dolist (buffer (buffer-list))
        (let ((file (buffer-file-name buffer))
              (default (buffer-local-value 'default-directory buffer)))
          (when (or (and (stringp file)
                         (string-prefix-p revert-spec--files-dir file))
                    (and (stringp default)
                         (string-prefix-p revert-spec--files-dir default)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))))
      (delete-directory revert-spec--files-dir t))
    (setq revert-spec--files-dir nil)
    (revert-spec--cleanup))

  (xit "restores the layout as added and clears the drift (Scenario: workspaces § User drift on an added layout is revertible)"
    (revert-spec--with-state-file
      (workspace-new "code")
      (delete-other-windows)
      (split-window-right)
      (let ((state (prog1 (workspace--capture-frameset) (delete-other-windows))))
        (workspace-add-layout "code" "demo" state :origin 'agent)
        (workspace-switch-layout "demo")
        (revert-spec--drain-timers)
        (expect (length (window-list)) :to-equal 2)
        ;; The user rearranges; an autosave captures it.
        (delete-other-windows)
        (workspace--autosave-current-layout :working-state)
        (expect (workspace--layout-working-state (revert-spec--layout "code" "demo"))
                :not :to-be nil)
        (workspace-revert)
        (revert-spec--drain-timers)
        (expect (length (window-list)) :to-equal 2)
        (let ((demo (revert-spec--layout "code" "demo")))
          (expect (workspace--layout-working-state demo) :to-be nil)
          (expect (workspace--layout-saved-state demo) :to-equal state)))))

  (it "re-applies the recipe and keeps it as :saved-state (Scenario: workspaces § Revert rebuilds from the recipe)"
    (revert-spec--with-state-file
      (workspace-new "code")
      (let* ((a (revert-spec--write-file "a.org"))
             (design (revert-spec--write-file "design.md"))
             (recipe `(:recipe :split columns
                               :panes ((:file ,a :line 40) (:file ,design)))))
        (puthash "code"
                 (workspace--upsert-group (gethash "code" workspace--registry)
                                          "demo" (workspace--layout-make recipe))
                 workspace--registry)
        (workspace-switch-layout "demo")
        (revert-spec--drain-timers)
        (expect (revert-spec--window-files) :to-equal (list a design))
        ;; The user rearranges; an autosave captures it.
        (delete-other-windows)
        (workspace--autosave-current-layout :working-state)
        (workspace-revert)
        (revert-spec--drain-timers)
        (expect (revert-spec--window-files) :to-equal (list a design))
        (let ((demo (revert-spec--layout "code" "demo")))
          (expect (workspace--layout-working-state demo) :to-be nil)
          (expect (workspace--layout-saved-state demo) :to-equal recipe))))))

(provide 'revert-spec)
;;; revert-spec.el ends here
