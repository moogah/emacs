;;; broken-home-runtime-spec.el --- Broken-home activation guards + re-anchor -*- lexical-binding: t; -*-

;; Pinned register entries:
;;   register/invariant/broken-tag-runtime-only (clear path via helper)
;;   register/invariant/home-required-no-floating-workspaces (absolute-path arm)
;;   register/invariant/registry-name-equals-basename (rename-on-re-anchor)
;;   register/shape/workspace-plist-v3
;;   register/vocabulary/workspace-broken-disposition (refuse on switch/restore;
;;     permit on re-anchor; error messages name remediation commands)
;;
;; Covers the command-layer broken-state contract (design.md §D5, §D6):
;;
;;   - `workspace-switch'   refuses broken with user-error naming
;;     remediation commands.
;;   - `workspace-restore'  refuses broken with the same message.
;;   - `workspace-re-anchor' has four branches:
;;       (1) same-basename happy path → home updated, broken cleared.
;;       (2) different-basename rename → registry key swapped; tab
;;           label updated when a live tab exists.
;;       (3) rename-collision → user-error; registry unchanged.
;;       (4) non-existent target → user-error; registry unchanged.
;;   - Persistence deserializer skips relative-path :home entries
;;     (cycle-2 architect finding arch-cycle-20260525-213500-04).

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
  ;; workspaces.el defines workspace-re-anchor; load it directly,
  ;; bypassing jf/load-module to avoid the parent's startup-restore
  ;; side effect in batch mode.
  (let ((workspaces-el (expand-file-name "../workspaces.el" dir)))
    ;; The parent file has a top-level `(jf/load-module ...)` cascade
    ;; and a `(when (fboundp 'workspace--restore) (workspace--restore))`
    ;; call; both are inert in a test process where the submodules are
    ;; already loaded directly above and `jf/load-module' is stubbed.
    (unless (fboundp 'jf/load-module)
      (defalias 'jf/load-module
        (lambda (path) (when (file-exists-p path) (load path nil t)))))
    (unless (boundp 'jf/emacs-dir)
      (defvar jf/emacs-dir (expand-file-name "../../.." dir)))
    (load workspaces-el nil t)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar broken-home-runtime-spec--tmp-dir nil)
(defvar broken-home-runtime-spec--captured-rename nil
  "List of (NEW-NAME . TAB-INDEX) from spy on `tab-bar-rename-tab'.")

(defun broken-home-runtime-spec--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq broken-home-runtime-spec--captured-rename nil)
  (setq broken-home-runtime-spec--tmp-dir
        (make-temp-file "ws-broken-runtime-" t)))

(defun broken-home-runtime-spec--cleanup ()
  (when (and broken-home-runtime-spec--tmp-dir
             (file-directory-p broken-home-runtime-spec--tmp-dir))
    (delete-directory broken-home-runtime-spec--tmp-dir t)))

(defmacro broken-home-runtime-spec--with-state-file (&rest body)
  "Redirect persistence I/O at the tmp dir for the duration of BODY."
  (declare (indent 0))
  `(let ((tmp broken-home-runtime-spec--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun broken-home-runtime-spec--write-raw (form)
  (make-directory (workspace--state-directory) t)
  (with-temp-file (workspace--state-file)
    (let ((print-length nil)
          (print-level nil))
      (prin1 form (current-buffer)))))

(defun broken-home-runtime-spec--make-broken (name)
  "Insert a broken workspace named NAME into the registry; return it."
  (let* ((home (file-name-as-directory
                (expand-file-name (concat name "-missing")
                                  broken-home-runtime-spec--tmp-dir)))
         ;; The directory does NOT exist; that is the precondition.
         (ws (workspace--mark-broken (workspace--make name home))))
    (puthash name ws workspace--registry)
    ws))

(describe "workspace-switch refuses broken workspaces"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "signals user-error naming the missing path and remediation commands"
    (broken-home-runtime-spec--make-broken "broken-ws")
    (let* ((tab-count-before (length (frame-parameter nil 'tabs)))
           (err (condition-case e
                    (progn (workspace-switch "broken-ws") nil)
                  (user-error e)))
           (msg (when err (error-message-string err))))
      (expect err :not :to-be nil)
      ;; The error names the workspace.
      (expect msg :to-match "broken-ws")
      ;; The error explicitly names BOTH remediation commands.
      (expect msg :to-match "workspace-re-anchor")
      (expect msg :to-match "workspace-purge")
      ;; The error mentions the broken-state semantics.
      (expect msg :to-match (regexp-quote "no longer exists"))
      ;; No tab was created or selected as a side effect.
      (expect (length (frame-parameter nil 'tabs))
              :to-equal tab-count-before))))

(describe "workspace-restore refuses broken workspaces"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "signals user-error naming the missing path and remediation commands; no tab created"
    (broken-home-runtime-spec--make-broken "broken-ws")
    (let* ((tab-count-before (length (frame-parameter nil 'tabs)))
           (err (condition-case e
                    (progn (workspace-restore "broken-ws") nil)
                  (user-error e)))
           (msg (when err (error-message-string err))))
      (expect err :not :to-be nil)
      (expect msg :to-match "broken-ws")
      (expect msg :to-match "workspace-re-anchor")
      (expect msg :to-match "workspace-purge")
      (expect (length (frame-parameter nil 'tabs))
              :to-equal tab-count-before))))

(describe "workspace-re-anchor happy path (same basename)"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "updates :home, clears :broken, flushes persistence"
    (broken-home-runtime-spec--with-state-file
      (broken-home-runtime-spec--make-broken "foo")
      (let* ((new-home (file-name-as-directory
                        (expand-file-name "foo"
                                          broken-home-runtime-spec--tmp-dir))))
        (make-directory new-home t)
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) new-home)))
          (let ((spy-calls 0))
            (cl-letf (((symbol-function 'workspace--flush-state)
                       (lambda (&rest _) (cl-incf spy-calls))))
              (call-interactively
               (lambda ()
                 (interactive)
                 (workspace-re-anchor "foo" new-home))))
            ;; Persistence flush was invoked exactly once.
            (expect spy-calls :to-equal 1)))
        (let ((foo (gethash "foo" workspace--registry)))
          (expect foo :not :to-be nil)
          (expect (workspace--home foo) :to-equal new-home)
          (expect (workspace--broken-p foo) :to-be nil))))))

(describe "workspace-re-anchor renames the registry key on basename change"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "removes old key, inserts under new basename, updates tab label when live tab exists"
    (broken-home-runtime-spec--with-state-file
      (broken-home-runtime-spec--make-broken "foo")
      (let* ((new-home (file-name-as-directory
                        (expand-file-name "renamed"
                                          broken-home-runtime-spec--tmp-dir))))
        (make-directory new-home t)
        (cl-letf (((symbol-function 'workspace--flush-state)
                   (lambda (&rest _) nil))
                  ;; tab-idx is nil for this case (no live tab), so the
                  ;; rename-tab branch should NOT fire.  Spy returns no
                  ;; assertion target; we just verify it is not called.
                  ((symbol-function 'tab-bar-rename-tab)
                   (lambda (&rest args)
                     (push args broken-home-runtime-spec--captured-rename))))
          (workspace-re-anchor "foo" new-home))
        ;; Old key is gone.
        (expect (gethash "foo" workspace--registry) :to-be nil)
        ;; New key is present and points at new home.
        (let ((renamed (gethash "renamed" workspace--registry)))
          (expect renamed :not :to-be nil)
          (expect (workspace--home renamed) :to-equal new-home)
          (expect (workspace--broken-p renamed) :to-be nil)
          (expect (workspace--name renamed) :to-equal "renamed")
          ;; Both the registry KEY and the plist :name slot equal the
          ;; new basename, in lockstep, per
          ;; register/invariant/registry-name-equals-basename (which
          ;; pins identity on the plist accessor, not on the registry
          ;; key).  Without this, the rename would not survive a save/
          ;; restore cycle: the serializer writes (workspace--name ws)
          ;; and the deserializer puthashes under the same.
          )
        ;; No live tab existed for "foo", so tab-bar-rename-tab MUST
        ;; NOT have been called.
        (expect broken-home-runtime-spec--captured-rename :to-equal nil))))

  (it "updates the live tab label when one exists"
    (broken-home-runtime-spec--with-state-file
      (broken-home-runtime-spec--make-broken "foo")
      ;; Manually create a tab named "foo" so workspace--tab-index-for
      ;; finds it; the broken-state guard on workspace-switch would
      ;; otherwise block us from materializing one.
      (tab-bar-new-tab)
      (tab-bar-rename-tab "foo")
      (let* ((new-home (file-name-as-directory
                        (expand-file-name "renamed"
                                          broken-home-runtime-spec--tmp-dir))))
        (make-directory new-home t)
        (cl-letf (((symbol-function 'workspace--flush-state)
                   (lambda (&rest _) nil)))
          (workspace-re-anchor "foo" new-home))
        ;; The tab now exists under the new label.
        (let ((tab-names (mapcar (lambda (tab)
                                   (cdr (assq 'name tab)))
                                 (frame-parameter nil 'tabs))))
          (expect (member "renamed" tab-names) :to-be-truthy)
          (expect (member "foo" tab-names) :to-be nil))))))

(describe "workspace-re-anchor refuses to clobber an existing workspace"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "signals user-error when the new basename collides with a healthy registry entry"
    (broken-home-runtime-spec--with-state-file
      (broken-home-runtime-spec--make-broken "foo")
      ;; Pre-populate a healthy "bar" workspace.
      (let* ((bar-home (file-name-as-directory
                        (expand-file-name "bar"
                                          broken-home-runtime-spec--tmp-dir))))
        (make-directory bar-home t)
        (puthash "bar"
                 (workspace--make "bar" bar-home)
                 workspace--registry))
      (let* ((collision-home (file-name-as-directory
                              (expand-file-name "bar"
                                                broken-home-runtime-spec--tmp-dir)))
             (err (condition-case e
                      (progn (workspace-re-anchor "foo" collision-home) nil)
                    (user-error e)))
             (msg (when err (error-message-string err))))
        (expect err :not :to-be nil)
        ;; The error names the colliding new name.
        (expect msg :to-match "bar")
        (expect msg :to-match "already exists"))
      ;; Registry unchanged: foo still broken, bar still healthy.
      (let ((foo (gethash "foo" workspace--registry))
            (bar (gethash "bar" workspace--registry)))
        (expect foo :not :to-be nil)
        (expect (workspace--broken-p foo) :to-be t)
        (expect bar :not :to-be nil)
        (expect (workspace--broken-p bar) :to-be nil)))))

(describe "workspace-re-anchor refuses a non-existent target"
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "signals user-error when the new home directory does not exist"
    (broken-home-runtime-spec--with-state-file
      (broken-home-runtime-spec--make-broken "foo")
      (let* ((nonexistent (file-name-as-directory
                           (expand-file-name "no-such-dir"
                                             broken-home-runtime-spec--tmp-dir)))
             (err (condition-case e
                      (progn (workspace-re-anchor "foo" nonexistent) nil)
                    (user-error e)))
             (msg (when err (error-message-string err))))
        (expect err :not :to-be nil)
        (expect msg :to-match "Not a directory")
        (expect msg :to-match (regexp-quote nonexistent)))
      ;; Registry unchanged: foo still broken at its original path.
      (let ((foo (gethash "foo" workspace--registry)))
        (expect foo :not :to-be nil)
        (expect (workspace--broken-p foo) :to-be t))))

  (it "signals user-error when no workspace by that name exists"
    (broken-home-runtime-spec--with-state-file
      (let ((err (condition-case e
                     (progn (workspace-re-anchor "nonexistent" "/tmp/") nil)
                   (user-error e))))
        (expect err :not :to-be nil)
        (expect (error-message-string err) :to-match "No workspace named")))))

(describe "Persistence deserializer rejects relative-path :home"
  ;; Architect finding arch-cycle-20260525-213500-04 (invariant-gap on
  ;; register/invariant/home-required-no-floating-workspaces): the
  ;; invariant requires :home be absolute.  Code path: the
  ;; deserializer filter at workspace--deserialize-state.
  (before-each (broken-home-runtime-spec--reset))
  (after-each (broken-home-runtime-spec--cleanup))

  (it "skips a persisted entry whose :home is a relative path and names it in *Messages*"
    (broken-home-runtime-spec--with-state-file
      (let ((messages-before (with-current-buffer "*Messages*"
                               (buffer-string))))
        (broken-home-runtime-spec--write-raw
         '(:version 3
                    :workspaces ((:name "rel-path-ws"
                                        :home "relative/path/"
                                        :recent-layout-group nil
                                        :buffer-files nil
                                        :layout-groups nil))))
        (workspace--restore)
        ;; Workspace was NOT inserted into the registry.
        (expect (gethash "rel-path-ws" workspace--registry) :to-be nil)
        ;; The notice names the offending entry and value.
        (let ((messages-after (with-current-buffer "*Messages*"
                                (buffer-string))))
          (expect (substring messages-after (length messages-before))
                  :to-match "not absolute")
          (expect (substring messages-after (length messages-before))
                  :to-match "rel-path-ws")
          (expect (substring messages-after (length messages-before))
                  :to-match "relative/path/"))))))

(provide 'broken-home-runtime-spec)
;;; broken-home-runtime-spec.el ends here
