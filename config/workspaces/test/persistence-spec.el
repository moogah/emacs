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

(defvar persistence-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

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
  (setq persistence-spec--parent-dir
        (make-temp-file "ws-persistence-spec-" t)
        workspaces-default-parent-directory persistence-spec--parent-dir))

(defun persistence-spec--cleanup ()
  (when (and persistence-spec--tmp-dir
             (file-directory-p persistence-spec--tmp-dir))
    (delete-directory persistence-spec--tmp-dir t))
  (when (and persistence-spec--parent-dir
             (file-directory-p persistence-spec--parent-dir))
    (delete-directory persistence-spec--parent-dir t)))

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
  ;; These exercise tier 3 of `workspace--state-directory' (the
  ;; per-machine default).  Tiers 1-2 (override / batch sandbox) take
  ;; precedence, and the test process itself is `noninteractive', so we
  ;; bind `noninteractive' to nil and leave the override unset to reach
  ;; the per-machine computation under test.
  (it "derives the path from `jf/machine-role'"
    (let ((jf/machine-role "personal-mac")
          (jf/emacs-dir "/tmp/wsdir")
          (noninteractive nil)
          (workspace-state-directory-override nil))
      (expect (workspace--state-directory)
              :to-equal "/tmp/wsdir/state/workspaces/personal-mac/")))

  (it "falls back to `default' when jf/machine-role is nil"
    (let ((jf/machine-role nil)
          (jf/emacs-dir "/tmp/wsdir")
          (noninteractive nil)
          (workspace-state-directory-override nil))
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

  (it "hydrates the registry from the persisted file without creating tabs"
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
      ;; Baseline the tab count after the restart simulation, before restore.
      (let ((baseline-tabs (length (frame-parameter nil 'tabs))))
        (workspace--restore)
        ;; Registry is hydrated: both workspaces present.
        (expect (gethash "alpha" workspace--registry) :not :to-be nil)
        (expect (gethash "beta"  workspace--registry) :not :to-be nil)
        ;; Startup creates NO tabs: tab-bar is unchanged by restore.
        (expect (length (frame-parameter nil 'tabs)) :to-equal baseline-tabs)
        (let ((names (mapcar (lambda (tab)
                               (workspace--tab-workspace-name tab))
                             (frame-parameter nil 'tabs))))
          (expect (member "alpha" names) :to-be nil)
          (expect (member "beta"  names) :to-be nil))
        ;; An explicit restore materializes a tab for the chosen workspace.
        (workspace-restore "alpha")
        (let ((names (mapcar (lambda (tab)
                               (workspace--tab-workspace-name tab))
                             (frame-parameter nil 'tabs))))
          (expect (member "alpha" names) :to-be-truthy)
          ;; beta was not restored, so it remains unmaterialized.
          (expect (member "beta" names) :to-be nil)))))

  (it "startup hydration creates no tabs for healthy workspaces"
    ;; v3 file with two healthy (existing :home) workspaces: assert the
    ;; registry hydrates, the tab count is unchanged, and both names are
    ;; offered by `workspace--registered-names' (the restore candidate list).
    (persistence-spec--with-state-file
      (let* ((home-a (file-name-as-directory
                      (make-temp-file "ws-home-a" t)))
             (home-b (file-name-as-directory
                      (make-temp-file "ws-home-b" t)))
             (file (workspace--state-file)))
        (unwind-protect
            (progn
              (make-directory (workspace--state-directory) t)
              (with-temp-file file
                (prin1 `(:version 3
                                  :workspaces
                                  ((:name "alpha" :home ,home-a
                                          :recent-layout-group nil
                                          :buffer-files nil
                                          :layout-groups nil)
                                   (:name "beta" :home ,home-b
                                          :recent-layout-group nil
                                          :buffer-files nil
                                          :layout-groups nil)))
                       (current-buffer)))
              (clrhash workspace--registry)
              (let ((baseline-tabs (length (frame-parameter nil 'tabs))))
                (workspace--restore)
                (expect (gethash "alpha" workspace--registry) :not :to-be nil)
                (expect (gethash "beta"  workspace--registry) :not :to-be nil)
                (expect (length (frame-parameter nil 'tabs))
                        :to-equal baseline-tabs)
                (let ((names (workspace--registered-names)))
                  (expect (member "alpha" names) :to-be-truthy)
                  (expect (member "beta"  names) :to-be-truthy))))
          (delete-directory home-a t)
          (delete-directory home-b t))))))

(describe "workspace-restore home-builder fallback (no saved layout)"
  ;; Delta scenario "Restore a workspace with no saved layout opens
  ;; home.org": a recovered/registry-only workspace whose layout-groups
  ;; carry no effective state must materialize via the home builder, NOT
  ;; via the layout-applying frameset path (design.md §D3).
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "runs the home builder and attempts no window-state-put"
    (persistence-spec--with-state-file
      ;; Real scaffold pipeline (scaffold is stubbed to mkdir) registers
      ;; alpha and gives it a default "home" layout group.  Strip the
      ;; layout groups to simulate a layout-less recovered workspace.
      (workspace-new "alpha")
      (let ((ws (gethash "alpha" workspace--registry)))
        ;; Mirror the spec's `:layout-groups is nil' precondition; also
        ;; drop the recent-group pointer so no effective state survives.
        (puthash "alpha"
                 (workspace--set-recent-group
                  (plist-put (copy-sequence ws) :layout-groups nil) nil)
                 workspace--registry))
      ;; Remove alpha's live tab so workspace-restore takes the create
      ;; branch rather than switching to the existing tab.
      (when-let ((idx (workspace--tab-index-for "alpha")))
        (tab-bar-close-tab idx))
      (expect (workspace--tab-index-for "alpha") :to-be nil)
      ;; Sanity: no effective layout state remains.
      (let* ((ws (gethash "alpha" workspace--registry))
             (recent (workspace--recent-group ws))
             (group (and recent (workspace--find-group ws recent)))
             (layout (and group (workspace--group-recent-layout group))))
        (expect (and layout (workspace--layout-effective-state layout))
                :to-be nil))
      (spy-on 'workspace-default-home-builder)
      (spy-on 'workspace--restore-frameset)
      (spy-on 'window-state-put)
      (let ((workspace-home-builder #'workspace-default-home-builder))
        (workspace-restore "alpha"))
      ;; A tab for alpha was created+selected.
      (expect (workspace--tab-index-for "alpha") :not :to-be nil)
      ;; The home builder ran with the workspace name; the saved-layout
      ;; path (frameset restore → deferred window-state-put) did not.
      (expect 'workspace-default-home-builder :to-have-been-called-with "alpha")
      (expect 'workspace--restore-frameset :not :to-have-been-called)
      (expect 'window-state-put :not :to-have-been-called)))

  (it "still applies the saved layout when one exists (branch not inverted)"
    (persistence-spec--with-state-file
      ;; alpha WITH a saved layout must go through the layout-applying
      ;; path, never the home builder — guards against the if/else
      ;; inverting.
      (workspace-new "alpha")
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (recent (workspace--recent-group ws))
             (group (and recent (workspace--find-group ws recent)))
             (layout (and group (workspace--group-recent-layout group))))
        ;; Precondition: an effective layout state is present.
        (expect (and layout (workspace--layout-effective-state layout))
                :not :to-be nil))
      (when-let ((idx (workspace--tab-index-for "alpha")))
        (tab-bar-close-tab idx))
      (expect (workspace--tab-index-for "alpha") :to-be nil)
      (spy-on 'workspace--apply-saved-layout)
      (spy-on 'workspace-default-home-builder)
      (let ((workspace-home-builder #'workspace-default-home-builder))
        (workspace-restore "alpha"))
      (expect 'workspace--apply-saved-layout :to-have-been-called-with "alpha")
      (expect 'workspace-default-home-builder :not :to-have-been-called))))

(describe "synchronous flush (no debounce)"
  (before-each (persistence-spec--reset))
  (after-each (persistence-spec--cleanup))

  (it "writes to disk synchronously when workspace-new runs"
    (persistence-spec--with-state-file
      (let ((write-count 0))
        (cl-letf (((symbol-function 'workspace--write-state)
                   (lambda (_form) (cl-incf write-count))))
          ;; `workspace-new' stamps the home layout via the :after advice,
          ;; which flushes synchronously — no idle timer, no debounce.
          (workspace-new "alpha")
          (expect write-count :to-be-greater-than 0)))))

  (it "each deliberate flush is its own immediate disk write"
    (persistence-spec--with-state-file
      (workspace-new "alpha")
      (let ((write-count 0))
        (cl-letf (((symbol-function 'workspace--write-state)
                   (lambda (_form) (cl-incf write-count))))
          ;; No coalescing: three direct flushes are three writes.
          (workspace--flush-state)
          (workspace--flush-state)
          (workspace--flush-state)
          (expect write-count :to-equal 3)))))

  (it "no debounce machinery remains"
    (expect (fboundp 'workspace-save-state) :to-be nil)
    (expect (boundp 'workspace--save-timer) :to-be nil)
    (expect (boundp 'workspace-save-idle-delay) :to-be nil)))

(describe "kill-emacs hook is installed"
  (it "registers workspace--kill-emacs-flush on kill-emacs-hook"
    (expect (member #'workspace--kill-emacs-flush kill-emacs-hook)
            :to-be-truthy)))

(describe "persistence is sandboxed under batch"
  ;; Regression guard for the teardown-clobber leak: a batch/test run
  ;; must never resolve the real per-machine state directory, because the
  ;; kill-emacs-hook flush fires at process exit — after every spec's
  ;; `cl-letf' state-dir rebinding has already unwound — and would
  ;; otherwise serialise a dirty global registry onto the developer's
  ;; production `state/workspaces/<role>/workspaces.eld'.
  (it "runs under noninteractive (precondition for the safety net)"
    (expect noninteractive :to-be-truthy))

  (it "never resolves the real per-machine state dir with no override"
    (let ((workspace-state-directory-override nil))
      (expect (workspace--state-directory)
              :not :to-match "/state/workspaces/")
      (expect (file-in-directory-p (workspace--state-directory)
                                   temporary-file-directory)
              :to-be-truthy)))

  (it "honours workspace-state-directory-override when set"
    (let ((workspace-state-directory-override "/tmp/ws-override-sentinel"))
      (expect (workspace--state-directory)
              :to-equal "/tmp/ws-override-sentinel/")
      (expect (workspace--state-file)
              :to-equal "/tmp/ws-override-sentinel/workspaces.eld"))))

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
      (let* ((message-log-max t)
             (file (workspace--state-file))
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
