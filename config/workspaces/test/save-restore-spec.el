;;; save-restore-spec.el --- Behavioral tests for workspace-save / workspace-restore -*- lexical-binding: t; -*-

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

(defvar save-restore-spec--tmp-dir nil
  "Per-test temp directory backing the persistence file.")

(defun save-restore-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (delete-other-windows)
  (setq save-restore-spec--tmp-dir
        (make-temp-file "ws-save-restore-" t))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq workspaces-default-parent-directory
        (make-temp-file "ws-save-restore-spec-" t)))

(defun save-restore-spec--cleanup ()
  (when (and save-restore-spec--tmp-dir
             (file-directory-p save-restore-spec--tmp-dir))
    (delete-directory save-restore-spec--tmp-dir t)))

(defmacro save-restore-spec--with-state-file (&rest body)
  "Run BODY with `workspace--state-file' pointed at a temp directory."
  (declare (indent 0))
  `(let ((tmp save-restore-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun save-restore-spec--drain-timers ()
  "Drain any pending immediate timers.
`workspace--restore-frameset' defers `window-state-put' via
`run-at-time nil nil ...' to avoid racing against
`bookmark--jump-via''s buffer-display call (design.md §D2, Gotcha 2);
batch tests must run the event loop briefly so the deferred closure
fires before assertions run."
  (sit-for 0)
  (sit-for 0)
  (accept-process-output nil 0.05))

(defun save-restore-spec--count-windows (form)
  "Count `leaf' symbols in FORM.
Works for both window-state forms and frameset-shaped data — both
encode each window as a `leaf' node."
  (let ((count 0))
    (cl-labels ((walk (x)
                  (cond
                   ((eq x 'leaf) (cl-incf count))
                   ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk form))
    count))

(defun save-restore-spec--lookup-workspace-on-disk (name)
  "Read the state file and return the persisted workspace plist for NAME."
  (let* ((state (workspace--read-state))
         (workspaces (plist-get state :workspaces)))
    (seq-find (lambda (ws) (equal (workspace--name ws) name))
              workspaces)))

(describe "workspace-save"
  (before-each (save-restore-spec--reset))
  (after-each (save-restore-spec--cleanup))

  (it "captures the live window configuration into the registry"
    (save-restore-spec--with-state-file
      (workspace-new "alpha")
      (delete-other-windows)
      (split-window-right)
      (expect (length (window-list)) :to-equal 2)
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group))
             (captured (workspace--layout-effective-state layout)))
        (expect (save-restore-spec--count-windows captured)
                :to-equal 2))))

  (it "writes the captured state to disk synchronously"
    (save-restore-spec--with-state-file
      (workspace-new "alpha")
      (delete-other-windows)
      (split-window-right)
      (workspace-save)
      (let* ((ws (save-restore-spec--lookup-workspace-on-disk "alpha"))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group))
             (captured (workspace--layout-effective-state layout)))
        (expect ws :not :to-be nil)
        (expect (save-restore-spec--count-windows captured)
                :to-equal 2))))

  (it "errors when not on a workspace tab"
    (save-restore-spec--with-state-file
      (expect (workspace-save) :to-throw 'user-error))))

(describe "file buffer round-trip"
  (before-each (save-restore-spec--reset))
  (after-each (save-restore-spec--cleanup))

  (it "restores a file buffer to the window it was saved in"
    (save-restore-spec--with-state-file
      ;; Use a real, readable file so find-file-noselect works.
      (let ((path (expand-file-name "init.el"
                                    (or (and (boundp 'jf/emacs-dir) jf/emacs-dir)
                                        default-directory))))
        (workspace-new "alpha")
        (find-file path)
        (workspace-save)
        ;; The file buffer must be recorded in :buffer-files for restore.
        (expect (member path (workspace--buffer-files
                              (gethash "alpha" workspace--registry)))
                :to-be-truthy)
        ;; Simulate "close tab but keep workspace": switch off alpha
        ;; first (the tab-switch advice captures the file-buffer frame
        ;; into :working-state at this point; v2 design.md §D4), then
        ;; close the tab and kill the file buffer.  Doing the kill
        ;; AFTER the switch preserves the working-state's pointer at a
        ;; bookmark-restorable resource.
        (tab-bar-select-tab 1)
        (let ((idx (workspace--tab-index-for "alpha")))
          (tab-bar-close-tab idx))
        (let ((buf (find-buffer-visiting path)))
          (when buf (kill-buffer buf)))
        (expect (workspace--tab-index-for "alpha") :to-be nil)
        (expect (find-buffer-visiting path) :to-be nil)
        ;; Restore: tab should be recreated and the file buffer displayed.
        (workspace-restore "alpha")
        (save-restore-spec--drain-timers)
        (expect (workspace--current-name) :to-equal "alpha")
        (expect (buffer-file-name (window-buffer (selected-window)))
                :to-equal path)))))

(describe "workspace-restore"
  (before-each (save-restore-spec--reset))
  (after-each (save-restore-spec--cleanup))

  (it "switches to the existing tab when the workspace is already materialized"
    (save-restore-spec--with-state-file
      (workspace-new "alpha")
      (workspace-new "beta")
      (let ((tab-count-before (length (frame-parameter nil 'tabs))))
        (workspace-restore "alpha")
        (expect (workspace--current-name) :to-equal "alpha")
        (expect (length (frame-parameter nil 'tabs))
                :to-equal tab-count-before))))

  (it "creates a tab and reconstructs the saved window layout when no tab exists"
    (save-restore-spec--with-state-file
      (workspace-new "alpha")
      (delete-other-windows)
      (split-window-right)
      (workspace-save)
      ;; Move off alpha and close its tab while keeping the registry entry.
      (tab-bar-select-tab 1)
      (let ((idx (workspace--tab-index-for "alpha")))
        (tab-bar-close-tab idx))
      (expect (gethash "alpha" workspace--registry) :not :to-be nil)
      (expect (workspace--tab-index-for "alpha") :to-be nil)
      ;; Restore.
      (workspace-restore "alpha")
      (save-restore-spec--drain-timers)
      (expect (workspace--current-name) :to-equal "alpha")
      (expect (length (window-list)) :to-equal 2)))

  (it "errors when no saved workspace by that name exists"
    (save-restore-spec--with-state-file
      (expect (workspace-restore "nonexistent") :to-throw 'user-error))))

(provide 'save-restore-spec)
;;; save-restore-spec.el ends here
