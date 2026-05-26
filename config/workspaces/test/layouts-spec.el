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
  (setq workspaces-default-parent-directory
        (make-temp-file "ws-layouts-spec-" t)))

(describe "workspace-save-layout"
  (before-each (layouts-spec--reset))

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
    (setq workspaces-default-parent-directory
          (make-temp-file "ws-layouts-spec-" t)))

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

(provide 'layouts-spec)
;;; layouts-spec.el ends here
