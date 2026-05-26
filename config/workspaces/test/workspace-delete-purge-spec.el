;;; workspace-delete-purge-spec.el --- Tests for workspace-delete + workspace-purge -*- lexical-binding: t; -*-

;; Pinned register entries:
;;   register/shape/workspace-plist-v3   (reconciled; load-bearing)
;;   register/invariant/broken-tag-runtime-only   (confirmed; load-bearing)
;;   register/invariant/registry-name-equals-basename   (reconciled; load-bearing)
;;   register/vocabulary/workspace-broken-disposition   (confirmed; load-bearing)
;;
;; Covers cycle-4 `workspace-delete-and-purge' task:
;;
;;   - `workspace-delete' is unregister-only (removes registry entry,
;;     closes live tab, flushes persistence; :home untouched on disk).
;;     `user-error' when NAME is not registered.
;;   - `workspace-purge' is destructive: yes-or-no-p confirmation,
;;     unregister-first, then `delete-directory ... t'.
;;   - Scope safeguard: refuses when :home is outside
;;     `workspaces-default-parent-directory' unless `current-prefix-arg'
;;     is set; the yes-or-no-p prompt MUST NOT fire when the safeguard
;;     trips (no destructive prompt before deliberate opt-in).
;;   - Cancellation at the prompt leaves registry + filesystem untouched.
;;   - Broken-home case: purge removes the registry entry; no error from
;;     the absent path; no attempt to call `delete-directory'.
;;
;; Pattern conventions mirror `workspace-new-default-spec.el' (cycle-3
;; reference): per-test tmp dir under `make-temp-file ... t', registry
;; cleared in `before-each', tmp tree torn down in `after-each',
;; defcustom mutation restored implicitly by re-`setq' in the next
;; before-each.  `workspace--flush-state' is stubbed so the test
;; process never writes to the real state file.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "../data-model.el" dir))
  (load (expand-file-name "../home-org.el"   dir))
  (load (expand-file-name "../scaffold.el"   dir))
  (load (expand-file-name "../tabs.el"       dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir))
  ;; workspaces.el defines workspace-delete / workspace-purge plus the
  ;; keybindings.  Stub jf/load-module + jf/emacs-dir so the cascade at
  ;; the top of workspaces.el is inert (submodules are already loaded
  ;; above) and the workspace--restore call at the bottom is harmless.
  (let ((workspaces-el (expand-file-name "../workspaces.el" dir)))
    (unless (fboundp 'jf/load-module)
      (defalias 'jf/load-module
        (lambda (path) (when (file-exists-p path) (load path nil t)))))
    (unless (boundp 'jf/emacs-dir)
      (defvar jf/emacs-dir (expand-file-name "../../.." dir)))
    (load workspaces-el nil t)))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar wdp-spec--tmp-parent nil
  "Per-test temp dir used as `workspaces-default-parent-directory'.")

(defvar wdp-spec--tmp-external nil
  "Per-test external dir (sibling of the default parent) for the
scope-safeguard scenarios.  Workspaces anchored under this tree are
deliberately outside the default-parent dir, so the safeguard fires.")

(defun wdp-spec--reset ()
  "Reset registry, tab-bar, and per-test tmp dirs."
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq wdp-spec--tmp-parent (make-temp-file "ws-purge-default-" t)
        wdp-spec--tmp-external (make-temp-file "ws-purge-external-" t)
        workspaces-default-parent-directory wdp-spec--tmp-parent))

(defun wdp-spec--cleanup ()
  "Tear down per-test tmp dirs if they still exist."
  (dolist (dir (list wdp-spec--tmp-parent wdp-spec--tmp-external))
    (when (and dir (file-directory-p dir))
      (delete-directory dir t))))

(defun wdp-spec--make-workspace (name home &optional broken)
  "Insert workspace NAME at HOME into the registry; return the plist.
When BROKEN is non-nil, mark the entry broken (`:home' may be absent)."
  (let* ((ws (workspace--make name home))
         (ws (if broken (workspace--mark-broken ws) ws)))
    (puthash name ws workspace--registry)
    ws))

(defun wdp-spec--seed-home (home)
  "Create HOME on disk with a home.org file and a sessions/ subdir.
Returns HOME for chaining.  Mirrors the post-scaffold shape so the
delete-leaves-files-on-disk assertions are meaningful."
  (make-directory home t)
  (with-temp-file (expand-file-name "home.org" home)
    (insert "#+TITLE: " (file-name-nondirectory (directory-file-name home)) "\n"))
  (make-directory (expand-file-name "sessions" home) t)
  home)

(describe "workspace-delete happy path"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "removes registry entry, closes live tab, leaves :home untouched on disk"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent)))
           (home-org (expand-file-name "home.org" home)))
      (wdp-spec--make-workspace "myproj" home)
      ;; Materialise a live tab so we can verify the close.
      (tab-bar-new-tab)
      (tab-bar-rename-tab "myproj")
      (let ((tabs-before (length (frame-parameter nil 'tabs))))
        (workspace-delete "myproj")
        ;; Registry no longer contains the entry.
        (expect (gethash "myproj" workspace--registry) :to-be nil)
        ;; Tab was closed.
        (expect (length (frame-parameter nil 'tabs))
                :to-equal (1- tabs-before))
        ;; Home directory still on disk, with home.org content intact.
        (expect (file-directory-p home) :to-be t)
        (expect (file-exists-p home-org) :to-be t)
        (expect (file-directory-p (expand-file-name "sessions" home))
                :to-be t)
        ;; Persistence was flushed exactly once.
        (expect 'workspace--flush-state :to-have-been-called-times 1))))

  (it "operates correctly when no live tab exists (registry-only entry)"
    (let ((home (wdp-spec--seed-home
                 (expand-file-name "ghost" wdp-spec--tmp-parent))))
      (wdp-spec--make-workspace "ghost" home)
      ;; No tab created.
      (workspace-delete "ghost")
      (expect (gethash "ghost" workspace--registry) :to-be nil)
      ;; Home directory still on disk.
      (expect (file-directory-p home) :to-be t)
      (expect 'workspace--flush-state :to-have-been-called-times 1))))

(describe "workspace-delete user-error"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "signals user-error when NAME is not registered; no persistence flush"
    (let* ((err (condition-case e
                    (progn (workspace-delete "no-such-ws") nil)
                  (user-error e)))
           (msg (when err (error-message-string err))))
      (expect err :not :to-be nil)
      (expect msg :to-match "no-such-ws")
      ;; No state mutation occurred.
      (expect 'workspace--flush-state :not :to-have-been-called))))

(describe "workspace-purge happy path"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "unregisters, closes tab, deletes :home recursively after confirmation"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent))))
      (wdp-spec--make-workspace "myproj" home)
      (tab-bar-new-tab)
      (tab-bar-rename-tab "myproj")
      (let ((tabs-before (length (frame-parameter nil 'tabs))))
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (workspace-purge "myproj"))
        (expect (gethash "myproj" workspace--registry) :to-be nil)
        (expect (length (frame-parameter nil 'tabs))
                :to-equal (1- tabs-before))
        ;; Home directory is gone.
        (expect (file-exists-p home) :to-be nil)
        (expect 'workspace--flush-state :to-have-been-called-times 1)))))

(describe "workspace-purge cancellation"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "signals 'Cancelled', leaves registry and filesystem untouched"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent)))
           err msg)
      (wdp-spec--make-workspace "myproj" home)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (setq err (condition-case e
                      (progn (workspace-purge "myproj") nil)
                    (user-error e))
              msg (when err (error-message-string err))))
      (expect err :not :to-be nil)
      (expect msg :to-match "[Cc]ancelled")
      ;; Registry still has the entry.
      (expect (gethash "myproj" workspace--registry) :not :to-be nil)
      ;; Home directory still on disk.
      (expect (file-directory-p home) :to-be t)
      (expect (file-exists-p (expand-file-name "home.org" home)) :to-be t)
      ;; No persistence write happened on the cancel path.
      (expect 'workspace--flush-state :not :to-have-been-called))))

(describe "workspace-purge scope safeguard"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "refuses external :home without prefix arg; no yes-or-no-p prompt fires"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "external-proj"
                                    wdp-spec--tmp-external)))
           (prompt-calls 0)
           err msg)
      (wdp-spec--make-workspace "external-proj" home)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompt-calls) t)))
        (let ((current-prefix-arg nil))
          (setq err (condition-case e
                        (progn (workspace-purge "external-proj") nil)
                      (user-error e))
                msg (when err (error-message-string err)))))
      (expect err :not :to-be nil)
      (expect msg :to-match (regexp-quote home))
      (expect msg :to-match "Refusing to purge")
      ;; Safeguard fired BEFORE the destructive prompt — critical for
      ;; the two-step opt-in (design.md §D8).
      (expect prompt-calls :to-equal 0)
      ;; Registry untouched.
      (expect (gethash "external-proj" workspace--registry) :not :to-be nil)
      ;; Home directory untouched.
      (expect (file-directory-p home) :to-be t)
      (expect 'workspace--flush-state :not :to-have-been-called)))

  (it "with prefix arg + yes-or-no-p t, proceeds and deletes the external dir"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "external-proj"
                                    wdp-spec--tmp-external))))
      (wdp-spec--make-workspace "external-proj" home)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        ;; One C-u → '(4); any non-nil value passes the safeguard.
        (let ((current-prefix-arg '(4)))
          (workspace-purge "external-proj")))
      (expect (gethash "external-proj" workspace--registry) :to-be nil)
      (expect (file-exists-p home) :to-be nil)
      (expect 'workspace--flush-state :to-have-been-called-times 1))))

(describe "workspace-purge broken-home"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "removes registry entry; no delete-directory call against missing path"
    (let* ((missing (file-name-as-directory
                     (expand-file-name "vanished" wdp-spec--tmp-parent)))
           (delete-calls 0))
      ;; Precondition: directory does NOT exist on disk.
      (expect (file-exists-p missing) :to-be nil)
      (wdp-spec--make-workspace "vanished" missing t)
      ;; The broken entry's :home is inside the default parent, so the
      ;; safeguard does not fire and no prefix arg is required.  Spy on
      ;; delete-directory so we can confirm it is NOT called for a path
      ;; that does not exist — the (file-directory-p home) guard inside
      ;; `workspace-purge' is what protects this case.
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'delete-directory)
                 (lambda (&rest _) (cl-incf delete-calls))))
        (workspace-purge "vanished"))
      (expect (gethash "vanished" workspace--registry) :to-be nil)
      (expect delete-calls :to-equal 0)
      (expect 'workspace--flush-state :to-have-been-called-times 1))))

(describe "workspace-delete and workspace-purge keybindings"
  (it "C-x w D is bound to workspace-delete"
    (expect (key-binding (kbd "C-x w D"))
            :to-equal #'workspace-delete))
  (it "C-x w P is bound to workspace-purge"
    (expect (key-binding (kbd "C-x w P"))
            :to-equal #'workspace-purge)))

(provide 'workspace-delete-purge-spec)
;;; workspace-delete-purge-spec.el ends here
