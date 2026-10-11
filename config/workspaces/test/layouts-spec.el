;;; layouts-spec.el --- Behavioral tests for workspace layout commands -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'map)
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
  ;; persistence.el provides workspace-save and workspace--flush-state;
  ;; the "explicit save clears :working-state" describe needs it.
  (load (expand-file-name "../persistence.el"       dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar layouts-spec--parent-dir nil
  "Per-test temp directory used as `workspaces-default-parent-directory'.")

(defun layouts-spec--reset ()
  "Reset state for each spec.
Also stubs `workspace-scaffold' and points
`workspaces-default-parent-directory' at a fresh tmpdir so
`workspace-new' is filesystem-isolated (cycle-3 wired the scaffold
pipeline)."
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (spy-on 'workspace-scaffold :and-call-fake
          (lambda (home _name &rest _) (make-directory home t) home))
  (setq layouts-spec--parent-dir
        (make-temp-file "ws-layouts-spec-" t)
        workspaces-default-parent-directory layouts-spec--parent-dir))

(defun layouts-spec--cleanup ()
  (when (and layouts-spec--parent-dir
             (file-directory-p layouts-spec--parent-dir))
    (delete-directory layouts-spec--parent-dir t)))

(describe "workspace-save-layout"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "stamps a `home' layout when a workspace is created"
    (workspace-new "alpha")
    (let* ((ws (gethash "alpha" workspace--registry))
           (group (workspace--find-group ws "home")))
      (expect group :not :to-be nil)
      (expect (workspace--group-recent-layout group) :not :to-be nil)
      (expect (workspace--recent-group ws) :to-equal "home")))

  (it "adds a named layout to the current workspace"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (let* ((ws (gethash "alpha" workspace--registry))
           (groups (workspace--layout-groups ws))
           (names (mapcar #'workspace--group-name groups)))
      (expect names :to-equal '("home" "magit"))
      (expect (workspace--recent-group ws) :to-equal "magit")))

  (it "overwrites an existing layout when saved with the same name"
    (workspace-new "alpha")
    (workspace-save-layout "scratch")
    (let* ((ws-before (gethash "alpha" workspace--registry))
           (group-before (workspace--find-group ws-before "scratch"))
           (ts-before (workspace--layout-timestamp
                       (workspace--group-recent-layout group-before))))
      ;; Force the next layout to have a strictly later timestamp.
      (sleep-for 1.1)
      (workspace-save-layout "scratch")
      (let* ((ws-after (gethash "alpha" workspace--registry))
             (group-after (workspace--find-group ws-after "scratch"))
             (ts-after (workspace--layout-timestamp
                        (workspace--group-recent-layout group-after))))
        ;; MVP keeps one layout per group; overwritten, not appended.
        (expect (length (workspace--group-layouts group-after))
                :to-equal 1)
        (expect ts-after :to-be-greater-than ts-before))))

  (it "errors when not on a workspace tab"
    (expect (workspace-save-layout "x") :to-throw 'user-error)))

(describe "workspace-switch-layout"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "updates the recent-layout pointer to the chosen layout"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-save-layout "tests")
    (workspace-switch-layout "home")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "home")
    (workspace-switch-layout "magit")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit"))

  (it "snapshots the outgoing layout before restoring"
    ;; Set up two layouts and verify that switching captures the
    ;; current frame into the outgoing layout's slot.
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    ;; Switch to home; the current frame should be snapshotted into
    ;; magit (the layout we're leaving).
    (let ((before-ts (workspace--layout-timestamp
                      (workspace--group-recent-layout
                       (workspace--find-group
                        (gethash "alpha" workspace--registry) "magit")))))
      (sleep-for 1.1)
      (workspace-switch-layout "home")
      (let ((after-ts (workspace--layout-timestamp
                       (workspace--group-recent-layout
                        (workspace--find-group
                         (gethash "alpha" workspace--registry) "magit")))))
        (expect after-ts :to-be-greater-than before-ts))))

  (it "errors when asked to switch to a non-existent layout"
    (workspace-new "alpha")
    (expect (workspace-switch-layout "nonexistent") :to-throw 'user-error)))

(describe "layout names are scoped to their workspace"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "permits independent layouts in different workspaces sharing a name"
    (workspace-new "alpha")
    (workspace-save-layout "tests")
    (workspace-new "beta")
    (workspace-save-layout "tests")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "tests")
            :not :to-be nil)
    (expect (workspace--find-group
             (gethash "beta" workspace--registry) "tests")
            :not :to-be nil)
    ;; Removing alpha's `tests' leaves beta's `tests' untouched —
    ;; the real independence guarantee.
    (workspace-switch "alpha")
    (workspace-delete-layout "tests")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "tests")
            :to-be nil)
    (expect (workspace--find-group
             (gethash "beta" workspace--registry) "tests")
            :not :to-be nil)))

(describe "workspace-delete-layout"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "removes the named layout"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-delete-layout "magit")
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "magit")
            :to-be nil))

  (it "refuses to delete the reserved `home' layout"
    (workspace-new "alpha")
    (expect (workspace-delete-layout "home") :to-throw 'user-error)
    (expect (workspace--find-group
             (gethash "alpha" workspace--registry) "home")
            :not :to-be nil))

  (it "reassigns the recent pointer when the deleted layout was current"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-save-layout "tests")
    ;; Recent is "tests".  Delete it.
    (workspace-delete-layout "tests")
    (let ((recent (workspace--recent-group
                   (gethash "alpha" workspace--registry))))
      (expect recent :not :to-equal "tests")
      ;; Pointer falls back to a remaining group ("home" or "magit").
      (expect (member recent '("home" "magit")) :to-be-truthy))))

(describe "recent-layout pointer"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "survives workspace switching"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    ;; alpha's recent is "magit".
    (workspace-new "beta")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit")
    (workspace-switch "alpha")
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "magit"))

  (it "workspace-switch-to-recent-layout jumps to the recent group"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    (workspace-switch-layout "home")
    ;; Now recent is "home".  Switch back to "magit" then call recent.
    (workspace-switch-layout "magit")
    (workspace-switch-layout "home")
    (workspace-switch-to-recent-layout)
    (expect (workspace--recent-group
             (gethash "alpha" workspace--registry))
            :to-equal "home")))

(describe "workspace--window-state-walk-leaves"
  (it "applies func to a leaf at the top level (one-window frame)"
    ;; A one-window-frame state looks like:
    ;;   (1 2 3 leaf (buffer . ...) (parameters . ...))
    ;; with the prefix-bookkeeping followed by `leaf' and its attrs.
    (let* ((state '(1 2 3 leaf (buffer "x" . ((point . 1)))
                       (parameters)))
           (translated
            (workspace--window-state-walk-leaves
             state
             (lambda (leaf)
               ;; tag the leaf so we can detect we were called
               (pcase-let ((`(leaf . ,attrs) leaf))
                 (setf (map-elt attrs 'tag) 'visited)
                 (cons 'leaf attrs))))))
      (expect (cl-position 'leaf translated) :to-be-truthy)
      (let* ((leaf-pos (cl-position 'leaf translated))
             (leaf-body (cl-subseq translated leaf-pos)))
        (expect (map-elt (cdr leaf-body) 'tag) :to-equal 'visited))))

  (it "applies func to every leaf in a multi-window tree"
    (let* ((state '(vc (total-height . 100)
                       (leaf (buffer "a" . nil) (parameters))
                       (leaf (buffer "b" . nil) (parameters))))
           (visits 0)
           (_ (workspace--window-state-walk-leaves
               state
               (lambda (leaf)
                 (cl-incf visits)
                 leaf))))
      (expect visits :to-equal 2)))

  (it "preserves atoms and key-atom cons cells unchanged"
    (let* ((sentinel '(other 99))
           (state `(hc 4 ,sentinel
                       (leaf (buffer "a" . nil) (parameters))))
           (result (workspace--window-state-walk-leaves
                    state #'identity)))
      ;; `hc' atom kept; (other . 99) kept; leaf passed through identity.
      (expect (memq 'hc result) :to-be-truthy)
      (expect (member sentinel result) :to-be-truthy))))

(describe "workspace--window-state-serialize / -deserialize"
  (before-each
    (clrhash workspace--registry))

  (it "embeds a workspace-buffer struct in each leaf's parameters on capture"
    ;; Fake a state with a leaf referencing a real buffer name; the
    ;; serializer should call workspace--serialize-buffer on it and
    ;; place the result under parameters.workspace-buffer.
    (with-temp-buffer
      (rename-buffer " *layouts-spec-capture-test*" t)
      (let* ((bname (buffer-name))
             (state `(leaf (buffer ,bname . ((point . 1)))
                           (parameters))))
        (let* ((serialized (workspace--window-state-serialize state))
               (leaf-pos (cl-position 'leaf serialized))
               (attrs (cdr (cl-subseq serialized leaf-pos)))
               (params (map-elt attrs 'parameters))
               (wb (map-elt params 'workspace-buffer)))
          (expect (workspace-buffer-p wb) :to-be t)
          (expect (workspace-buffer-name wb) :to-equal bname)))))

  (it "rewrites each leaf's buffer slot on deserialize"
    (with-temp-buffer
      (rename-buffer " *layouts-spec-restore-test*" t)
      (let* ((bname (buffer-name))
             (live-buf (current-buffer))
             (wb (make-workspace-buffer :name bname))
             (state `(leaf (buffer "GHOST" . ((point . 1)))
                           (parameters (workspace-buffer . ,wb)))))
        (let* ((restored (workspace--window-state-deserialize state))
               (leaf-pos (cl-position 'leaf restored))
               (attrs (cdr (cl-subseq restored leaf-pos)))
               (buf-cell (map-elt attrs 'buffer)))
          ;; The buffer slot's car should be a buffer, not a string;
          ;; specifically the live buffer we got back from get-buffer.
          (expect (buffer-live-p (car buf-cell)) :to-be t)
          (expect (car buf-cell) :to-equal live-buf))))))

(describe "workspace--capture-frameset"
  (before-each
    (clrhash workspace--registry)
    (let ((tabs (frame-parameter nil 'tabs)))
      (when (> (length tabs) 1)
        (dotimes (_ (1- (length tabs)))
          (tab-bar-close-tab 2))))
    ;; Stub workspace-scaffold + tmpdir for workspaces-default-parent-directory
    ;; so workspace-new is filesystem-isolated (cycle-3 wired scaffold pipeline).
    (spy-on 'workspace-scaffold :and-call-fake
            (lambda (home _name &rest _) (make-directory home t) home))
    (setq layouts-spec--parent-dir
          (make-temp-file "ws-layouts-spec-" t)
          workspaces-default-parent-directory layouts-spec--parent-dir))
  (after-each (layouts-spec--cleanup))

  (it "embeds a workspace-buffer struct in every captured leaf"
    (workspace-new "alpha")
    (delete-other-windows)
    (split-window-right)
    (let* ((state (workspace--capture-frameset))
           (visits 0))
      (workspace--window-state-walk-leaves
       state
       (lambda (leaf)
         (pcase-let* ((`(leaf . ,attrs) leaf)
                      ((map parameters) attrs))
           (when (map-elt parameters 'workspace-buffer)
             (cl-incf visits)))
         leaf))
      ;; Two windows after the split → at least two leaves with a
      ;; workspace-buffer payload.
      (expect visits :to-be-weakly-greater-than 2))))

(describe "workspace--autosave-current-layout slot routing"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it ":working-state writes :working-state and leaves :saved-state untouched"
    (workspace-new "alpha")
    (let* ((ws (gethash "alpha" workspace--registry))
           (layout (workspace--group-recent-layout
                    (workspace--find-group ws "home")))
           (saved-before (workspace--layout-saved-state layout)))
      (workspace--autosave-current-layout :working-state)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :not :to-be nil)
        (expect (workspace--layout-saved-state layout)
                :to-equal saved-before))))

  (it ":saved-state writes :saved-state"
    (workspace-new "alpha")
    ;; Add some drift first so we can verify :saved-state changes.
    (workspace--autosave-current-layout :working-state)
    (let* ((ws (gethash "alpha" workspace--registry))
           (layout (workspace--group-recent-layout
                    (workspace--find-group ws "home")))
           (saved-before (workspace--layout-saved-state layout)))
      ;; Force the next capture to differ.
      (sleep-for 1.1)
      (delete-other-windows)
      (split-window-right)
      (workspace--autosave-current-layout :saved-state)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        ;; saved-state differs from before (window count changed).
        (expect (workspace--layout-saved-state layout)
                :not :to-equal saved-before))))

  (it "errors when SLOT is omitted (no default; structural enforcement of autosave-never-writes-saved-state)"
    (workspace-new "alpha")
    ;; The function requires an explicit slot; an omitted slot fires
    ;; cl-assert.  This is the structural fix for the v1 MVP gap D8
    ;; "tab-switch autosave clobbers explicit save" failure mode: an
    ;; autosave site that forgets to pass :working-state cannot
    ;; silently fall through to :saved-state.  See on-touch architect
    ;; finding arch-cycle-20260524-200631-on-touch-two-state-layout-1.
    ;; No-arg call → wrong-number-of-arguments (function signature change
    ;; from `&optional slot' to required slot is structural, not runtime).
    (let ((caught-no-arg
           (condition-case _ (workspace--autosave-current-layout) (error 'caught))))
      (expect caught-no-arg :to-equal 'caught))
    ;; Off-vocabulary slot → cl-assertion-failed via the
    ;; workspace--state-slot-p predicate at function entry.
    (let ((caught-bogus
           (condition-case _ (workspace--autosave-current-layout :bogus) (error 'caught))))
      (expect caught-bogus :to-equal 'caught))))

(describe "Explicit workspace-save clears :working-state"
  ;; register/invariant/explicit-save-clears-working-state — pinned for
  ;; every explicit-save variant (workspace-save, workspace-save-layout,
  ;; workspace-new home stamp).  workspace-switch-layout is navigation,
  ;; not an explicit-save variant per design.md §D4 — it does NOT clear
  ;; :working-state and is intentionally out of scope here.
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "workspace-save clears :working-state on the affected layout"
    ;; Stub the disk-flush so the test doesn't write.
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      ;; Accumulate :working-state drift.
      (workspace--autosave-current-layout :working-state)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :not :to-be nil))
      ;; Explicit save clears the drift.
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (workspace--layout-working-state layout) :to-be nil)
        (expect (workspace--layout-saved-state layout) :not :to-be nil))))

  (it "workspace-save-layout NAME leaves the named layout's :working-state nil"
    ;; The named-layout save path constructs a fresh layout via
    ;; workspace--layout-make (which pins :working-state to nil at
    ;; construction).  If a future refactor switches to preserving a
    ;; passed-in :working-state, this scenario catches the regression.
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      ;; Stamp a layout named "magit" with prior :working-state drift
      ;; pre-populated via the autosave path against the recent layout
      ;; first, then create the named layout.
      (workspace--autosave-current-layout :working-state)
      (workspace-save-layout "magit")
      (let* ((ws (gethash "alpha" workspace--registry))
             (magit (workspace--find-group ws "magit"))
             (layout (workspace--group-recent-layout magit)))
        (expect (workspace--layout-saved-state layout) :not :to-be nil)
        (expect (workspace--layout-working-state layout) :to-be nil))))

  (it "workspace-new home stamp leaves the home layout's :working-state nil"
    ;; The home-group construction path at workspace-new time uses
    ;; workspace--layout-make → :working-state nil from the start.
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      (let* ((ws (gethash "alpha" workspace--registry))
             (home (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout home)))
        (expect (workspace--layout-saved-state layout) :not :to-be nil)
        (expect (workspace--layout-working-state layout) :to-be nil)))))

(describe "workspace-switch-layout routes outgoing capture to :working-state"
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "writes the outgoing layout's :working-state, not :saved-state"
    (workspace-new "alpha")
    (workspace-save-layout "magit")
    ;; alpha's recent is "magit"; switch to home should capture the
    ;; current frame into magit's :working-state.
    (let* ((ws (gethash "alpha" workspace--registry))
           (magit (workspace--find-group ws "magit"))
           (layout-before (workspace--group-recent-layout magit))
           (saved-before (workspace--layout-saved-state layout-before)))
      (expect (workspace--layout-working-state layout-before) :to-be nil)
      (workspace-switch-layout "home")
      (let* ((ws (gethash "alpha" workspace--registry))
             (magit (workspace--find-group ws "magit"))
             (layout-after (workspace--group-recent-layout magit)))
        (expect (workspace--layout-working-state layout-after)
                :not :to-be nil)
        ;; :saved-state untouched.
        (expect (workspace--layout-saved-state layout-after)
                :to-equal saved-before)))))

(describe "layout-v2-plist producer shape equivalence"
  ;; All three explicit-save variants — workspace-save,
  ;; workspace-save-layout, and workspace--capture-home-layout — funnel
  ;; through workspace--autosave-current-layout :saved-state, the
  ;; canonical layout-construction helper.  Today :etc is universally
  ;; nil (design.md §D7 :buffer-files consolidation deferred); tomorrow
  ;; when :etc carries content (design.md §D5 per-layout-group git
  ;; observation), divergent producers would silently wipe the field.
  ;; These specs pin :etc round-trip across the three variants so the
  ;; latent fragmentation cannot regress.  Resolves register/shape/
  ;; layout-v2-plist producer_fragmentation_note.
  (before-each (layouts-spec--reset))
  (after-each (layouts-spec--cleanup))

  (it "workspace-save preserves :etc on the recent layout (variant 1)"
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      ;; Inject a known :etc value via direct registry mutation.
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "home"))
             (layout (workspace--group-recent-layout group)))
        (plist-put layout :etc '((sentinel . cross-producer-test))))
      (workspace-save)
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (plist-get layout :etc)
                :to-equal '((sentinel . cross-producer-test))))))

  (it "workspace-save-layout preserves :etc on re-save (variant 2)"
    ;; The latent divergence the funnel fixes: pre-funnel,
    ;; workspace-save-layout constructed a fresh layout via
    ;; workspace--layout-make + upsert, wiping any prior :etc on the
    ;; same-named group.  Post-funnel, the canonical :saved-state path
    ;; copies the existing layout and preserves :etc.
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      ;; Create magit (initial save).
      (workspace-save-layout "magit")
      ;; Inject :etc on the named layout via direct registry mutation.
      (let* ((ws (gethash "alpha" workspace--registry))
             (group (workspace--find-group ws "magit"))
             (layout (workspace--group-recent-layout group)))
        (plist-put layout :etc '((sentinel . cross-producer-test))))
      ;; Re-save magit; :etc must survive.
      (workspace-save-layout "magit")
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "magit"))))
        (expect (plist-get layout :etc)
                :to-equal '((sentinel . cross-producer-test))))))

  (it "workspace--capture-home-layout produces a shape-equivalent layout (variant 3)"
    ;; A brand-new workspace has nothing to preserve, but the funnel
    ;; ensures shape parity: same keys, same nil-initialised :etc and
    ;; :working-state, as the other two variants on first save.
    (cl-letf (((symbol-function 'workspace--flush-state) (lambda () nil)))
      (workspace-new "alpha")
      (let* ((ws (gethash "alpha" workspace--registry))
             (layout (workspace--group-recent-layout
                      (workspace--find-group ws "home"))))
        (expect (plist-member layout :timestamp) :to-be-truthy)
        (expect (plist-member layout :saved-state) :to-be-truthy)
        (expect (plist-member layout :working-state) :to-be-truthy)
        (expect (plist-member layout :etc) :to-be-truthy)
        (expect (plist-get layout :working-state) :to-be nil)
        (expect (plist-get layout :etc) :to-be nil)
        (expect (plist-get layout :saved-state) :not :to-be nil)))))

;;; ------------------------------------------------------------------
;;; Change workspaces-programmatic-layouts (design.md § Seams, § Scenario
;;; coverage).  Specs marked `xit' are pending: their bodies drive
;;; functions the change introduces (`workspace-add-layout',
;;; `workspace-remove-layout', the metadata accessors, recipe expansion)
;;; and run once an Implementor promotes them.  Hooks and helpers use only
;;; what HEAD defines, because buttercup runs the hooks of a pending spec.
;;; ------------------------------------------------------------------

(defvar layouts-spec--state-dir nil
  "Per-test directory the state file is redirected to, or nil.")

(defvar layouts-spec--files-dir nil
  "Per-test directory holding the files recipe panes name, or nil.")

(defvar layouts-spec--saved-state-override nil
  "`workspace-state-directory-override' as it was before the test.")

(defun layouts-spec--programmatic-reset ()
  "Reset as `layouts-spec--reset' does; isolate the state file; make a pane-file dir."
  (layouts-spec--reset)
  (delete-other-windows)
  (setq layouts-spec--saved-state-override workspace-state-directory-override
        layouts-spec--state-dir (make-temp-file "ws-layouts-state-" t)
        workspace-state-directory-override layouts-spec--state-dir
        layouts-spec--files-dir (file-name-as-directory
                                 (file-truename
                                  (make-temp-file "ws-layouts-files-" t)))))

(defun layouts-spec--programmatic-cleanup ()
  "Undo `layouts-spec--programmatic-reset', killing buffers in its directories."
  (setq workspace-state-directory-override layouts-spec--saved-state-override)
  (dolist (dir (list layouts-spec--state-dir layouts-spec--files-dir))
    (when (and dir (file-directory-p dir))
      ;; String comparison only: another spec's buffer may have a remote
      ;; `default-directory', which a file operation would try to reach.
      (let ((spellings (list (file-name-as-directory dir)
                             (file-name-as-directory (file-truename dir)))))
        (dolist (buffer (buffer-list))
          (let ((file (buffer-file-name buffer))
                (default (buffer-local-value 'default-directory buffer)))
            (when (seq-some (lambda (prefix)
                              (or (and (stringp file) (string-prefix-p prefix file))
                                  (and (stringp default)
                                       (string-prefix-p prefix default))))
                            spellings)
              (with-current-buffer buffer (set-buffer-modified-p nil))
              (kill-buffer buffer)))))
      (delete-directory dir t)))
  (setq layouts-spec--state-dir nil
        layouts-spec--files-dir nil)
  (layouts-spec--cleanup))

(defun layouts-spec--write-file (name &optional lines)
  "Create NAME in the pane-file dir holding LINES numbered lines (default 200).
Return its absolute name."
  (let ((path (expand-file-name name layouts-spec--files-dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (dotimes (i (or lines 200))
        (insert (format "line %d\n" (1+ i)))))
    path))

(defun layouts-spec--drain-timers ()
  "Let the deferred `window-state-put' of `workspace--restore-frameset' run."
  (sit-for 0)
  (sit-for 0)
  (accept-process-output nil 0.05))

(defun layouts-spec--windows ()
  "The selected frame's windows, left to right and top to bottom."
  (window-list nil 'no-mini (frame-first-window)))

(defun layouts-spec--window-file (window)
  "Truename of the file WINDOW's buffer visits, or nil."
  (let ((file (buffer-file-name (window-buffer window))))
    (and file (file-truename file))))

(defun layouts-spec--window-line (window)
  "Line number of WINDOW's point."
  (with-selected-window window
    (line-number-at-pos)))

(defun layouts-spec--count-leaves (form)
  "Count `leaf' symbols in window-state FORM."
  (let ((count 0))
    (cl-labels ((walk (x)
                  (cond ((eq x 'leaf) (cl-incf count))
                        ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk form))
    count))

(defun layouts-spec--layout (ws-name layout-name)
  "The layout named LAYOUT-NAME of workspace WS-NAME in the registry, or nil."
  (let* ((ws (gethash ws-name workspace--registry))
         (group (and ws (workspace--find-group ws layout-name))))
    (and group (workspace--group-recent-layout group))))

(defun layouts-spec--put-layout (ws-name layout-name layout)
  "Store LAYOUT as LAYOUT-NAME of WS-NAME through the pure data layer."
  (puthash ws-name
           (workspace--upsert-group (gethash ws-name workspace--registry)
                                    layout-name layout)
           workspace--registry))

(defun layouts-spec--on-disk-layout-names (ws-name)
  "Layout names of WS-NAME in the state file on disk."
  (let* ((state (workspace--read-state))
         (ws (seq-find (lambda (w) (equal (workspace--name w) ws-name))
                       (plist-get state :workspaces))))
    (mapcar #'workspace--group-name (workspace--layout-groups ws))))

(defun layouts-spec--surroundings ()
  "What a programmatic write must leave alone, as a plist."
  (list :frame (selected-frame)
        :tab (workspace--tab-name (workspace--frame-current-tab))
        :tabs (mapcar #'workspace--tab-name (frame-parameter nil 'tabs))
        :windows (current-window-configuration)
        :buffers (buffer-list)))

(defun layouts-spec--expect-surroundings-unchanged (before)
  "Expect the frame, tabs, windows and buffers to be as BEFORE recorded them."
  (let ((after (layouts-spec--surroundings)))
    (expect (plist-get after :frame) :to-be (plist-get before :frame))
    (expect (plist-get after :tab) :to-equal (plist-get before :tab))
    (expect (plist-get after :tabs) :to-equal (plist-get before :tabs))
    (expect (compare-window-configurations (plist-get before :windows)
                                           (plist-get after :windows))
            :to-be-truthy)
    (expect (seq-difference (plist-get after :buffers)
                            (plist-get before :buffers))
            :to-equal nil)))

(defun layouts-spec--refusal (thunk)
  "Call THUNK; return the message of the `user-error' it signals, else nil."
  (condition-case err
      (progn (funcall thunk) nil)
    (user-error (error-message-string err))))

(defun layouts-spec--two-window-state ()
  "A captured window-state of two side-by-side windows; the frame is left as one."
  (delete-other-windows)
  (split-window-right)
  (prog1 (workspace--capture-frameset)
    (delete-other-windows)))

(describe "Programmatic layout addition (seam/programmatic-layout-writes)"
  (before-each (layouts-spec--programmatic-reset))
  (after-each (layouts-spec--programmatic-cleanup))

  (it "adds a layout to the user's workspace without disturbing anything (Scenario: workspaces § Adding a layout does not disturb the user)"
    (workspace-new "code")
    (let* ((state (layouts-spec--two-window-state))
           (recent-before (workspace--recent-group
                           (gethash "code" workspace--registry)))
           (before (layouts-spec--surroundings)))
      (spy-on 'workspace--flush-state :and-call-through)
      (workspace-add-layout "code" "demo" state :origin 'agent)
      (let ((demo (layouts-spec--layout "code" "demo")))
        (expect demo :not :to-be nil)
        (expect (workspace--layout-saved-state demo) :to-equal state)
        (expect (workspace--layout-working-state demo) :to-be nil)
        (expect (workspace--layout-origin demo) :to-be 'agent))
      (expect (workspace--recent-group (gethash "code" workspace--registry))
              :to-equal recent-before)
      (layouts-spec--expect-surroundings-unchanged before)
      (expect 'workspace--flush-state :to-have-been-called-times 1)
      (expect (layouts-spec--on-disk-layout-names "code") :to-contain "demo")))

  (it "adds a layout to a workspace that has no tab, opening none (Scenario: workspaces § Adding a layout to a workspace without a tab)"
    (workspace-new "notes")
    (tab-bar-select-tab 1)
    (tab-bar-close-tab (workspace--tab-index-for "notes"))
    (expect (workspace--tab-index-for "notes") :to-be nil)
    (let* ((state (layouts-spec--two-window-state))
           (before (layouts-spec--surroundings)))
      (workspace-add-layout "notes" "review" state :origin 'agent)
      (expect (workspace--layout-saved-state (layouts-spec--layout "notes" "review"))
              :to-equal state)
      (expect (workspace--tab-index-for "notes") :to-be nil)
      (layouts-spec--expect-surroundings-unchanged before)))

  (it "refuses the reserved layout name `home', leaving `home' unchanged (Scenario: workspaces § The home layout cannot be added programmatically)"
    (workspace-new "code")
    (let* ((home-before (copy-tree (layouts-spec--layout "code" "home")))
           (state (layouts-spec--two-window-state))
           (message (layouts-spec--refusal
                     (lambda ()
                       (workspace-add-layout "code" "home" state :origin 'agent)))))
      (expect message :to-match "home")
      (expect message :to-match "reserved")
      (expect (layouts-spec--layout "code" "home") :to-equal home-before))))

(describe "Protection of user layouts from programmatic writes (seam/write-protection)"
  (before-each (layouts-spec--programmatic-reset))
  (after-each (layouts-spec--programmatic-cleanup))

  (it "refuses to overwrite a user layout, which stays as it was (Scenario: workspaces § User layout is never overwritten)"
    (workspace-new "code")
    ;; The user's own layout, saved with the user's command.
    (workspace-save-layout "review")
    (workspace-switch-layout "home")
    (layouts-spec--drain-timers)
    (let* ((before (copy-tree (layouts-spec--layout "code" "review")))
           (state (layouts-spec--two-window-state))
           (message (layouts-spec--refusal
                     (lambda ()
                       (workspace-add-layout "code" "review" state :origin 'agent)))))
      (expect message :to-match "review")
      (expect message :to-match "belongs to the user")
      (expect (layouts-spec--layout "code" "review") :to-equal before)))

  (it "refuses to replace an agent layout that is on screen (Scenario: workspaces § On-screen agent layout is not replaced)"
    (workspace-new "code")
    (let ((state (layouts-spec--two-window-state)))
      (workspace-add-layout "code" "demo" state :origin 'agent)
      (workspace-switch-layout "demo")
      (layouts-spec--drain-timers)
      (expect (workspace-layout-on-screen-p "code" "demo") :to-be-truthy)
      (let* ((before (copy-tree (layouts-spec--layout "code" "demo")))
             (message (layouts-spec--refusal
                       (lambda ()
                         (workspace-add-layout "code" "demo" state :origin 'agent)))))
        (expect message :to-match "demo")
        (expect message :to-match "on screen")
        (expect (layouts-spec--layout "code" "demo") :to-equal before))))

  (it "replaces an off-screen agent layout, discarding its working state (Scenario: workspaces § Off-screen agent layout is replaced)"
    (workspace-new "code")
    (workspace-add-layout "code" "demo" (layouts-spec--two-window-state) :origin 'agent)
    (workspace-switch-layout "demo")
    (layouts-spec--drain-timers)
    (split-window-below)
    ;; Leaving `demo' autosaves the drift into its :working-state.
    (workspace-switch-layout "home")
    (layouts-spec--drain-timers)
    (expect (workspace--layout-working-state (layouts-spec--layout "code" "demo"))
            :not :to-be nil)
    (expect (workspace-layout-on-screen-p "code" "demo") :to-be nil)
    (delete-other-windows)
    (split-window-below)
    (let ((new (prog1 (workspace--capture-frameset) (delete-other-windows))))
      (workspace-add-layout "code" "demo" new :origin 'agent)
      (let ((demo (layouts-spec--layout "code" "demo")))
        (expect (workspace--layout-saved-state demo) :to-equal new)
        (expect (workspace--layout-working-state demo) :to-be nil)
        (expect (workspace--layout-origin demo) :to-be 'agent)))))

(describe "Programmatic layout removal (seam/programmatic-layout-writes, seam/write-protection)"
  (before-each (layouts-spec--programmatic-reset))
  (after-each (layouts-spec--programmatic-cleanup))

  (it "removes an off-screen agent layout without disturbing anything (Scenario: workspaces § Agent layout removed when its artifact closes)"
    (workspace-new "code")
    (workspace-add-layout "code" "ask/a1" (layouts-spec--two-window-state)
                          :origin 'agent :source "a1")
    (let ((recent-before (workspace--recent-group
                          (gethash "code" workspace--registry)))
          (before (layouts-spec--surroundings)))
      (spy-on 'workspace--flush-state :and-call-through)
      (workspace-remove-layout "code" "ask/a1")
      (expect (layouts-spec--layout "code" "ask/a1") :to-be nil)
      (expect (workspace--recent-group (gethash "code" workspace--registry))
              :to-equal recent-before)
      (layouts-spec--expect-surroundings-unchanged before)
      (expect 'workspace--flush-state :to-have-been-called-times 1)
      (expect (layouts-spec--on-disk-layout-names "code") :not :to-contain "ask/a1")))

  (it "refuses to remove a user layout, which stays as it was (Scenario: workspaces § User layout cannot be removed programmatically)"
    (workspace-new "code")
    (workspace-save-layout "review")
    (workspace-switch-layout "home")
    (layouts-spec--drain-timers)
    (let* ((before (copy-tree (layouts-spec--layout "code" "review")))
           (message (layouts-spec--refusal
                     (lambda () (workspace-remove-layout "code" "review")))))
      (expect message :to-match "review")
      (expect message :to-match "belongs to the user")
      (expect (layouts-spec--layout "code" "review") :to-equal before)))

  (it "refuses to remove a layout that is on screen (Scenario: workspaces § On-screen layout is not removed)"
    (workspace-new "code")
    (workspace-add-layout "code" "ask/a1" (layouts-spec--two-window-state)
                          :origin 'agent :source "a1")
    (workspace-switch-layout "ask/a1")
    (layouts-spec--drain-timers)
    (let ((message (layouts-spec--refusal
                    (lambda () (workspace-remove-layout "code" "ask/a1")))))
      (expect message :to-match "ask/a1")
      (expect message :to-match "on screen")
      (expect (layouts-spec--layout "code" "ask/a1") :not :to-be nil)))

  (it "reassigns the recent pointer as `workspace-delete-layout' does when the removed layout was recent (seam/programmatic-layout-writes)"
    (workspace-new "code")
    (workspace-add-layout "code" "ask/a1" (layouts-spec--two-window-state)
                          :origin 'agent :source "a1")
    (workspace-switch-layout "ask/a1")
    (layouts-spec--drain-timers)
    ;; Recent, but with its tab closed it is not on screen.
    (tab-bar-select-tab 1)
    (tab-bar-close-tab (workspace--tab-index-for "code"))
    (workspace-remove-layout "code" "ask/a1")
    (expect (workspace--recent-group (gethash "code" workspace--registry))
            :to-equal "home")))

(describe "Explicit saves take ownership of agent layouts (seam/explicit-save-ownership)"
  (before-each (layouts-spec--programmatic-reset))
  (after-each (layouts-spec--programmatic-cleanup))

  (it "makes the recent agent layout the user's on `workspace-save', keeping its other metadata (Scenario: workspaces § Explicit save takes ownership)"
    (workspace-new "code")
    (layouts-spec--put-layout
     "code" "demo"
     (workspace--layout-with-metadata
      (workspace--layout-make (layouts-spec--two-window-state))
      :origin 'agent :description "Demo" :source "change-x"))
    (workspace-switch-layout "demo")
    (layouts-spec--drain-timers)
    (delete-other-windows)
    (split-window-below)
    (split-window-right)
    (workspace-save)
    (let ((demo (layouts-spec--layout "code" "demo")))
      (expect (workspace--layout-origin demo) :to-be 'user)
      (expect (workspace--layout-description demo) :to-equal "Demo")
      (expect (workspace--layout-source demo) :to-equal "change-x")
      ;; A captured window-state of the current frame: three windows.
      (expect (layouts-spec--count-leaves (workspace--layout-saved-state demo))
              :to-equal 3)))

  (it "makes an agent layout the user's when `workspace-save-layout' saves over its name (Scenario: workspaces § Saving under an agent layout's name takes ownership)"
    (workspace-new "code")
    (layouts-spec--put-layout
     "code" "demo"
     (workspace--layout-with-metadata
      (workspace--layout-make (layouts-spec--two-window-state))
      :origin 'agent))
    (workspace-save-layout "demo")
    (expect (workspace--layout-origin (layouts-spec--layout "code" "demo"))
            :to-be 'user))

  (it "replaces a recipe :saved-state with a captured window-state on `workspace-save' (Scenario: workspaces § Explicit save replaces the recipe)"
    ;; HEAD satisfies this by construction: the explicit save overwrites
    ;; :saved-state without reading it.  The recipe is made the recent
    ;; layout but not applied, which the save does not depend on.
    (workspace-new "code")
    (let ((recipe `(:recipe :split columns
                            :panes ((:file ,(layouts-spec--write-file "a.org") :line 40)
                                    (:file ,(layouts-spec--write-file "design.md"))))))
      (layouts-spec--put-layout "code" "demo" (workspace--layout-make recipe))
      (workspace--update-recent-group "code" "demo")
      (delete-other-windows)
      (split-window-right)
      (workspace-save)
      (let ((saved (workspace--layout-saved-state (layouts-spec--layout "code" "demo"))))
        (expect saved :not :to-equal recipe)
        (expect (car-safe saved) :not :to-be :recipe)
        (expect (layouts-spec--count-leaves saved) :to-equal 2)))))

(describe "Recipe layouts are built when applied (seam/recipe-apply)"
  (before-each (layouts-spec--programmatic-reset))
  (after-each (layouts-spec--programmatic-cleanup))

  (it "builds two side-by-side panes, point on the given line (Scenario: workspaces § Switching to a recipe layout builds it)"
    (workspace-new "code")
    (let ((a (layouts-spec--write-file "a.org"))
          (design (layouts-spec--write-file "design.md")))
      (layouts-spec--put-layout
       "code" "demo"
       (workspace--layout-make
        `(:recipe :split columns :panes ((:file ,a :line 40) (:file ,design)))))
      (workspace-switch-layout "demo")
      (layouts-spec--drain-timers)
      (let ((windows (layouts-spec--windows)))
        (expect (length windows) :to-equal 2)
        (expect (mapcar #'layouts-spec--window-file windows)
                :to-equal (list a design))
        (expect (window-top-line (nth 0 windows))
                :to-equal (window-top-line (nth 1 windows)))
        (expect (window-left-column (nth 0 windows))
                :to-be-less-than (window-left-column (nth 1 windows)))
        (expect (abs (- (window-total-width (nth 0 windows))
                        (window-total-width (nth 1 windows))))
                :to-be-less-than 2)
        (expect (layouts-spec--window-line (nth 0 windows)) :to-equal 40))))

  (it "builds a main pane beside a stack of two (Scenario: workspaces § Nested recipe builds a main pane beside a stack)"
    (workspace-new "code")
    (let ((ask (layouts-spec--write-file "ask.org"))
          (scaffold (layouts-spec--write-file "scaffold.org"))
          (design (layouts-spec--write-file "design.md")))
      (layouts-spec--put-layout
       "code" "ask"
       (workspace--layout-make
        `(:recipe :split columns
                  :panes ((:file ,ask)
                          (:split rows
                                  :panes ((:file ,scaffold :line 157)
                                          (:file ,design)))))))
      (workspace-switch-layout "ask")
      (layouts-spec--drain-timers)
      (let ((windows (layouts-spec--windows)))
        (expect (length windows) :to-equal 3)
        (expect (mapcar #'layouts-spec--window-file windows)
                :to-equal (list ask scaffold design))
        ;; `ask.org' on the left; the stack on the right, scaffold above.
        (expect (window-left-column (nth 0 windows))
                :to-be-less-than (window-left-column (nth 1 windows)))
        (expect (window-left-column (nth 1 windows))
                :to-equal (window-left-column (nth 2 windows)))
        (expect (window-top-line (nth 1 windows))
                :to-be-less-than (window-top-line (nth 2 windows)))
        (expect (layouts-spec--window-line (nth 1 windows)) :to-equal 157))))

  (it "shows a placeholder naming a missing file and builds the other panes (Scenario: workspaces § Missing file does not break the layout)"
    (workspace-new "code")
    (let ((missing (expand-file-name "deleted.org" layouts-spec--files-dir))
          (present (layouts-spec--write-file "present.org")))
      (layouts-spec--put-layout
       "code" "demo"
       (workspace--layout-make
        `(:recipe :split columns :panes ((:file ,missing) (:file ,present)))))
      (workspace-switch-layout "demo")
      (layouts-spec--drain-timers)
      (let ((windows (layouts-spec--windows)))
        (expect (length windows) :to-equal 2)
        (expect (find-buffer-visiting missing) :to-be nil)
        (expect (with-current-buffer (window-buffer (nth 0 windows))
                  (buffer-string))
                :to-match (regexp-quote missing))
        (expect (layouts-spec--window-file (nth 1 windows)) :to-equal present)))))

(provide 'layouts-spec)
;;; layouts-spec.el ends here
