;;; persistence-spec.el --- Behavioral tests for workspace persistence -*- lexical-binding: t; -*-

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
  (load (expand-file-name "../persistence.el"       dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

;; Declared so `let' bindings in tests below are dynamic.
(defvar jf/emacs-dir)
(defvar jf/machine-role)

(defvar persistence-spec--tmp-dir nil
  "Per-suite temp directory backing the persistence file.")

(defun persistence-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq persistence-spec--tmp-dir
        (make-temp-file "ws-state-" t))
  ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
  ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq workspaces-default-parent-directory
        (make-temp-file "ws-persistence-spec-" t)))

(defun persistence-spec--cleanup ()
  (when (and persistence-spec--tmp-dir
             (file-directory-p persistence-spec--tmp-dir))
    (delete-directory persistence-spec--tmp-dir t)))

(defmacro persistence-spec--with-state-file (&rest body)
  "Run BODY with `workspace--state-file' pointed at a temp directory."
  (declare (indent 0))
  `(let ((tmp persistence-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(describe "Persistence directory is per-machine"
  (it "derives the path from `jf/machine-role'"
    (let ((jf/machine-role "personal-mac")
          (jf/emacs-dir "/tmp/wsdir"))
      (expect (workspace--state-directory)
              :to-equal "/tmp/wsdir/state/workspaces/personal-mac/")))

  (it "falls back to `default' when jf/machine-role is nil"
    (let ((jf/machine-role nil)
          (jf/emacs-dir "/tmp/wsdir"))
      (expect (workspace--state-directory)
              :to-equal "/tmp/wsdir/state/workspaces/default/"))))

(describe "Missing persistence file is non-fatal"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "returns nil and leaves the registry empty"
    (persistence-spec--with-state-file
      (expect (workspace--read-state) :to-equal nil)
      (workspace--restore)
      (expect (hash-table-count workspace--registry) :to-equal 0))))

(describe "serialize / deserialize round-trip"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "round-trips two workspaces with multiple groups each"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save-layout "magit")
      (workspace-save-layout "tests")
      (workspace-new "beta")
      (workspace-save-layout "scratch")
      ;; Serialize → write → clear → read → deserialize → assert.
      (workspace--write-state (workspace--serialize-registry))
      (clrhash workspace--registry)
      (let ((state (workspace--read-state)))
        (expect state :not :to-be nil)
        (workspace--deserialize-state state))
      (expect (hash-table-count workspace--registry) :to-equal 2)
      (let* ((alpha (gethash "alpha" workspace--registry))
             (beta  (gethash "beta"  workspace--registry))
             (alpha-names (mapcar #'workspace--group-name
                                  (workspace--layout-groups alpha)))
             (beta-names  (mapcar #'workspace--group-name
                                  (workspace--layout-groups beta))))
        (expect alpha :not :to-be nil)
        (expect beta  :not :to-be nil)
        (expect alpha-names :to-equal '("home" "magit" "tests"))
        (expect beta-names  :to-equal '("home" "scratch"))
        ;; Each workspace's recent pointer survives.
        (expect (workspace--recent-group alpha) :to-equal "tests")
        (expect (workspace--recent-group beta)  :to-equal "scratch")))))

(describe "Workspaces survive restart"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "restores tabs from the persisted file"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-new "beta")
      (workspace--flush-state)
      ;; Simulate restart: clear in-memory state, close tabs, then restore.
      (clrhash workspace--registry)
      (let ((tabs (frame-parameter nil 'tabs)))
        (when (> (length tabs) 1)
          (dotimes (_ (1- (length tabs)))
            (tab-bar-close-tab 2))))
      (workspace--restore)
      (let ((names (mapcar (lambda (tab) (workspace--tab-workspace-name tab))
                           (frame-parameter nil 'tabs))))
        (expect (member "alpha" names) :to-be-truthy)
        (expect (member "beta"  names) :to-be-truthy)))))

(describe "workspace-save-state debouncing"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "coalesces rapid saves into a single disk write"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (let ((write-count 0))
        (cl-letf (((symbol-function 'workspace--write-state)
                   (lambda (_form) (cl-incf write-count))))
          (workspace-save-state)
          (workspace-save-state)
          (workspace-save-state)
          ;; Force the timer to fire now by flushing directly.
          (workspace--flush-state)
          (expect write-count :to-equal 1))))))

(describe "kill-emacs hook is installed"
  (it "registers workspace--kill-emacs-flush on kill-emacs-hook"
    (expect (member #'workspace--kill-emacs-flush kill-emacs-hook)
            :to-be-truthy)))

(describe "Schema version check"
  ;; Note: v3-specific behavior (v2 rejection, :home skip, broken-home
  ;; tagging) lives in `persistence-v3-spec.el` and
  ;; `broken-home-load-spec.el`. This block retains the framework-
  ;; agnostic checks that were authored at the v2 cutover and continue
  ;; to hold under v3.
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "accepts a file whose :version equals workspace--state-version"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace--flush-state)
      (expect (workspace--read-state) :not :to-be nil)
      (expect (plist-get (workspace--read-state) :version)
              :to-equal workspace--state-version)))

  (it "rejects a v1 file with a notice and returns nil"
    (persistence-spec--with-state-file
      (let* ((file (workspace--state-file))
             (messages-before (with-current-buffer "*Messages*"
                                (buffer-string))))
        ;; Hand-craft a v1-shaped file.
        (make-directory (workspace--state-directory) t)
        (with-temp-file file
          (prin1 '(:version 1
                            :workspaces ((:name "stale"
                                                :recent-layout-group nil
                                                :buffer-files nil
                                                :layout-groups nil)))
                 (current-buffer)))
        (expect (workspace--read-state) :to-be nil)
        ;; The notice mentions :version and the file path.
        (let ((messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect (substring messages-after (length messages-before))
                  :to-match ":version")))))

  (it "version mismatch leaves the registry empty after workspace--restore"
    (persistence-spec--with-state-file
      (let ((file (workspace--state-file)))
        (make-directory (workspace--state-directory) t)
        (with-temp-file file
          (prin1 '(:version 99 :workspaces ()) (current-buffer)))
        (workspace--restore)
        (expect (hash-table-count workspace--registry) :to-equal 0)))))

(describe "Tab-switch advice writes working-state"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "snapshots the outgoing workspace into :working-state, not :saved-state"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group))
             (saved-marker (workspace--layout-saved-state layout)))
        (expect saved-marker :not :to-be nil)
        (expect (workspace--layout-working-state layout) :to-be nil)
        ;; Switch to tab 1 (non-workspace) — fires the before-advice.
        (delete-other-windows)
        (split-window-right)
        (tab-bar-select-tab 1)
        ;; alpha's :working-state is now populated; :saved-state is
        ;; unchanged (clobber-impossible per register/invariant/
        ;; autosave-never-writes-saved-state).
        (let* ((ws (gethash "alpha" workspace--registry))
               (group (workspace--find-group ws "home"))
               (layout (workspace--group-recent-layout group)))
          (expect (workspace--layout-working-state layout) :not :to-be nil)
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-marker)))))

  (it "is a no-op when the outgoing tab is not a workspace tab"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      ;; Switch to non-workspace tab 1.
      (tab-bar-select-tab 1)
      ;; Now switch from tab 1 back to another non-workspace position.
      ;; Since we're not on alpha, the advice should not fire.
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group))
             (ws-before (workspace--layout-working-state layout)))
        ;; tab-bar-select-tab on the current (non-workspace) tab is
        ;; effectively a no-op for the advice site.
        (tab-bar-select-tab 1)
        (let* ((ws-after (workspace--layout-working-state
                          (workspace--group-recent-layout
                           (workspace--find-group
                            (gethash "alpha" workspace--registry) "home")))))
          ;; No change attributable to a second non-workspace switch.
          (expect ws-after :to-equal ws-before))))))

(describe "kill-emacs flush captures working-state"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "calls autosave with :working-state once before writing"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((calls nil))
        (cl-letf (((symbol-function 'workspace--autosave-current-layout)
                   (lambda (&optional slot) (push slot calls))))
          (workspace--kill-emacs-flush)
          ;; Exactly one call, and it targets :working-state.
          (expect (length calls) :to-equal 1)
          (expect (car calls) :to-equal :working-state)))))

  (it "leaves :saved-state intact after the flush"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group))
             (saved-before (workspace--layout-saved-state layout)))
        (workspace--kill-emacs-flush)
        (let* ((ws (gethash "alpha" workspace--registry))
               (group (workspace--find-group ws "home"))
               (layout (workspace--group-recent-layout group)))
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-before))))))

(provide 'persistence-spec)
;;; persistence-spec.el ends here
