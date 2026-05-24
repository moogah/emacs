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
  (load (expand-file-name "../layouts.el"           dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defun layouts-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2)))))

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
          (tab-bar-close-tab 2)))))

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

(provide 'layouts-spec)
;;; layouts-spec.el ends here
