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
  ;; integrations.el defines `workspace--dispatch-purge-integrations',
  ;; which `workspace-purge' now calls before `delete-directory'.  It
  ;; depends only on workspace-data-model (loaded above).
  (load (expand-file-name "../integrations.el" dir))
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
    (load workspaces-el nil t))
  ;; The fake remote (TRAMP) handler; a core helper, not the
  ;; portable-bookmarks fixtures.
  (load (expand-file-name "workspaces-test-helpers.el" dir)))

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

(defun wdp-spec--purge-in-scope (name)
  "Purge NAME with no prefix arg and confirmation granted; return the error or nil.
Integration teardown is stubbed so only the safeguard and the delete run."
  (spy-on 'workspace--dispatch-purge-integrations :and-return-value nil)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (let ((current-prefix-arg nil))
      (condition-case e
          (progn (workspace-purge name) nil)
        (user-error e)))))

(describe "workspace-purge scope safeguard decides containment by file identity"
  ;; The safeguard asks core's ONE containment predicate
  ;; (`workspace--path-within-p'), never `file-in-directory-p', which
  ;; compares spellings case-sensitively and ends in `file-equal-p'.  Each
  ;; spec below was a spurious "Refusing to purge" under the old test.
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "purges a home under the default parent while the parent is written to between reads"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent)))
           (parent wdp-spec--tmp-parent)
           (real-file-attributes (symbol-function 'file-attributes))
           (writes 0)
           (control nil)
           (err nil))
      (wdp-spec--make-workspace "myproj" home)
      ;; Every `file-attributes' read is followed by a real write into the
      ;; parent, so its modification time moves between two reads.
      (cl-letf (((symbol-function 'file-attributes)
                 (lambda (file &optional id-format)
                   (prog1 (funcall real-file-attributes file id-format)
                     (setq writes (1+ writes))
                     (write-region "" nil
                                   (expand-file-name
                                    (format "wdp-write-%d" writes) parent)
                                   nil 'silent)))))
        ;; Control: the old test refuses under this write pattern.
        (setq control (file-in-directory-p home parent))
        (setq err (wdp-spec--purge-in-scope "myproj")))
      (expect control :to-be nil)
      (expect err :to-be nil)
      (expect writes :to-be-greater-than 2)
      (expect (gethash "myproj" workspace--registry) :to-be nil)
      (expect (file-exists-p home) :to-be nil)))

  (it "purges a home under a differently cased spelling of the default parent"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent))))
      (assume (file-name-case-insensitive-p wdp-spec--tmp-parent)
              "case-sensitive filesystem")
      (setq workspaces-default-parent-directory
            (expand-file-name
             (upcase (file-name-nondirectory
                      (directory-file-name wdp-spec--tmp-parent)))
             (file-name-directory (directory-file-name wdp-spec--tmp-parent))))
      (expect (file-directory-p workspaces-default-parent-directory) :to-be t)
      ;; Control: the spelling comparison misses it.
      (expect (file-in-directory-p home workspaces-default-parent-directory)
              :to-be nil)
      (wdp-spec--make-workspace "myproj" home)
      (expect (wdp-spec--purge-in-scope "myproj") :to-be nil)
      (expect (gethash "myproj" workspace--registry) :to-be nil)
      (expect (file-exists-p home) :to-be nil)))

  (it "still refuses a vanished home under an external tree without a prefix arg"
    ;; A missing :home is judged by its nearest existing ancestor: under
    ;; the parent it is in scope (the broken-home spec below), under an
    ;; external tree it is not.
    (let* ((missing (file-name-as-directory
                     (expand-file-name "vanished" wdp-spec--tmp-external)))
           (err nil))
      (expect (file-exists-p missing) :to-be nil)
      (wdp-spec--make-workspace "vanished" missing t)
      (setq err (wdp-spec--purge-in-scope "vanished"))
      (expect err :not :to-be nil)
      (expect (error-message-string err) :to-match "Refusing to purge")
      (expect (gethash "vanished" workspace--registry) :not :to-be nil)
      (expect 'workspace--flush-state :not :to-have-been-called))))

(defmacro wdp-spec--recording-deletes (var &rest body)
  "Run BODY with `delete-directory' recording its arguments onto VAR.
Nothing is deleted."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'delete-directory)
              (lambda (&rest args) (push args ,var) nil)))
     ,@body))

(describe "workspace-purge scope is STRICT: the default parent itself needs C-u"
  ;; The containment predicate is reflexive (a path is within itself).  A
  ;; workspace anchored AT the default parent must not pass the safeguard:
  ;; one "yes" would recursively delete every workspace below it.
  ;; Every purge here runs inside `wdp-spec--recording-deletes', which
  ;; records `delete-directory' calls instead of deleting, so even with
  ;; the production change reverted nothing is deleted.  (A scoped
  ;; `cl-letf', not a spy: a spy would still be active in `after-each'
  ;; and stop the temp-dir cleanup.)
  :var (deleted)
  (before-each
    (wdp-spec--reset)
    (setq deleted nil)
    (spy-on 'workspace--flush-state :and-return-value nil)
    (spy-on 'workspace--dispatch-purge-integrations :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "is out of scope at the predicate, for the parent and a symlinked spelling of it"
    (let ((link (expand-file-name "link-to-parent" wdp-spec--tmp-external)))
      (make-symbolic-link (directory-file-name wdp-spec--tmp-parent) link)
      (expect (workspace--purge-in-scope-p wdp-spec--tmp-parent) :to-be nil)
      (expect (workspace--purge-in-scope-p
               (directory-file-name wdp-spec--tmp-parent))
              :to-be nil)
      (expect (workspace--purge-in-scope-p (file-name-as-directory link))
              :to-be nil)
      ;; A proper descendant is still in scope, present or vanished.
      (let ((child (wdp-spec--seed-home
                    (expand-file-name "child" wdp-spec--tmp-parent))))
        (expect (workspace--purge-in-scope-p child) :to-be-truthy))
      (expect (workspace--purge-in-scope-p
               (expand-file-name "vanished/" wdp-spec--tmp-parent))
              :to-be-truthy)))

  (it "refuses a workspace anchored at the default parent without C-u; nothing is prompted or deleted"
    (let* ((parent-home (file-name-as-directory wdp-spec--tmp-parent))
           (sibling (wdp-spec--seed-home
                     (expand-file-name "sibling" wdp-spec--tmp-parent)))
           (prompt-calls 0)
           (err nil))
      (wdp-spec--make-workspace "parent" parent-home)
      (wdp-spec--make-workspace "sibling" sibling)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (cl-incf prompt-calls) t)))
        (let ((current-prefix-arg nil))
          (wdp-spec--recording-deletes deleted
            (setq err (condition-case e
                          (progn (workspace-purge "parent") nil)
                        (user-error e))))))
      (expect err :not :to-be nil)
      (expect (error-message-string err) :to-match "Refusing to purge")
      (expect prompt-calls :to-equal 0)
      (expect deleted :to-be nil)
      (expect 'workspace--flush-state :not :to-have-been-called)
      (expect (gethash "parent" workspace--registry) :not :to-be nil)
      (expect (file-directory-p sibling) :to-be t)))

  (it "refuses a symlinked spelling of the default parent without C-u"
    (let* ((link (expand-file-name "link-to-parent" wdp-spec--tmp-external))
           (err nil))
      (make-symbolic-link (directory-file-name wdp-spec--tmp-parent) link)
      (wdp-spec--make-workspace "link-to-parent" (file-name-as-directory link))
      (wdp-spec--recording-deletes deleted
        (setq err (wdp-spec--purge-in-scope "link-to-parent")))
      (expect err :not :to-be nil)
      (expect (error-message-string err) :to-match "Refusing to purge")
      (expect deleted :to-be nil)
      (expect (gethash "link-to-parent" workspace--registry) :not :to-be nil)))

  (it "with C-u, the parent is purgeable like any external home"
    (let ((parent-home (file-name-as-directory wdp-spec--tmp-parent)))
      (wdp-spec--make-workspace "parent" parent-home)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (let ((current-prefix-arg '(4)))
          (wdp-spec--recording-deletes deleted
            (workspace-purge "parent"))))
      (expect (gethash "parent" workspace--registry) :to-be nil)
      (expect deleted :to-equal (list (list parent-home t))))))

(describe "workspace-purge broken-home"
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "no-ops on filesystem when :home no longer exists"
    ;; Observable end-state contract for the broken-home purge path
    ;; (register/vocabulary/workspace-broken-disposition: purge is
    ;; :allowed on broken — registry-only cleanup, no filesystem
    ;; deletion to perform).  Asserts the BEHAVIOUR, not the call
    ;; shape: any implementation that completes without error, leaves
    ;; the registry clean, and does not touch the parent directory
    ;; satisfies the contract.
    (let* ((missing (file-name-as-directory
                     (expand-file-name "vanished" wdp-spec--tmp-parent))))
      ;; Precondition: :home does NOT exist on disk; parent does.
      (expect (file-exists-p missing) :to-be nil)
      (expect (file-directory-p wdp-spec--tmp-parent) :to-be t)
      (wdp-spec--make-workspace "vanished" missing t)
      ;; :home is inside the default parent, so the safeguard does
      ;; not fire and no prefix arg is required.  yes-or-no-p stubbed
      ;; to t — the implicit assertion is that `workspace-purge'
      ;; completes without signalling (Buttercup would raise).
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (workspace-purge "vanished"))
      ;; Observable end-state:
      ;; - registry entry gone
      (expect (gethash "vanished" workspace--registry) :to-be nil)
      ;; - parent directory untouched (no over-broad deletion)
      (expect (file-directory-p wdp-spec--tmp-parent) :to-be t)
      ;; - persistence flushed exactly once
      (expect 'workspace--flush-state :to-have-been-called-times 1))))

(describe "workspace-purge integration teardown dispatch"
  ;; `workspace-purge' fires the integration `:on-purge' surface via
  ;; `workspace--dispatch-purge-integrations' BEFORE recursively
  ;; deleting :home, so integrations can clean up resources anchored
  ;; under the home tree (e.g. git worktrees) while the directory still
  ;; exists (design.md §D3).  The dispatch is ADDITIVE and NEVER
  ;; LOAD-BEARING (register/boundary/workspace-integration-registry): a
  ;; (failed . REASON) outcome is surfaced but never aborts the purge,
  ;; and `workspace-purge' ignores the returned alist for control flow
  ;; (register/vocabulary/workspace-integration-outcome).  Teardown
  ;; fires on purge ONLY — `workspace-delete' keeps :home and its
  ;; worktrees, so it dispatches nothing.
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil))
  (after-each (wdp-spec--cleanup))

  (it "calls dispatch once with NAME + HOME, BEFORE delete-directory, on purge"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent)))
           (order nil)
           ;; `workspace--make' normalises :home to a directory name; the
           ;; dispatch receives the stored (normalised) value, so assert
           ;; against that rather than the raw seed path.
           (stored-home (workspace--home
                         (wdp-spec--make-workspace "myproj" home))))
      ;; Record relative ordering: dispatch must run while :home still
      ;; exists, and delete-directory after.  Both are spied so the
      ;; ORDER assertion proves dispatch precedes deletion.
      (spy-on 'workspace--dispatch-purge-integrations
              :and-call-fake
              (lambda (&rest _)
                ;; The home tree is still on disk at dispatch time.
                (expect (file-directory-p home) :to-be t)
                (push 'dispatch order)
                nil))
      (spy-on 'delete-directory
              :and-call-fake
              (lambda (&rest _) (push 'delete order) nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (workspace-purge "myproj"))
      ;; Dispatch called exactly once.
      (expect 'workspace--dispatch-purge-integrations
              :to-have-been-called-times 1)
      ;; With the workspace NAME and its on-disk :home, context `purge'.
      (expect 'workspace--dispatch-purge-integrations
              :to-have-been-called-with "myproj" stored-home 'purge)
      ;; And BEFORE the recursive delete (order is reversed push list).
      (expect (nreverse order) :to-equal '(dispatch delete))))

  (it "does NOT abort the purge when a handler reports (failed . REASON)"
    ;; Additive contract: a failed teardown outcome is surfaced but the
    ;; purge still completes — :home is deleted regardless.
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "myproj" wdp-spec--tmp-parent))))
      (wdp-spec--make-workspace "myproj" home)
      (spy-on 'workspace--dispatch-purge-integrations
              :and-return-value '((some-integration . (failed . "boom"))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (workspace-purge "myproj"))
      ;; Purge completed: dispatch ran, registry clean, home deleted.
      (expect 'workspace--dispatch-purge-integrations
              :to-have-been-called-times 1)
      (expect (gethash "myproj" workspace--registry) :to-be nil)
      (expect (file-exists-p home) :to-be nil)
      (expect 'workspace--flush-state :to-have-been-called-times 1)))

  (it "does NOT dispatch when :home is already absent (broken-home purge)"
    ;; Nothing to tear down when :home is gone — the home-exists guard
    ;; gates the dispatch alongside delete-directory.
    (let* ((missing (file-name-as-directory
                     (expand-file-name "vanished" wdp-spec--tmp-parent))))
      (expect (file-exists-p missing) :to-be nil)
      (wdp-spec--make-workspace "vanished" missing t)
      (spy-on 'workspace--dispatch-purge-integrations)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (workspace-purge "vanished"))
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)
      (expect (gethash "vanished" workspace--registry) :to-be nil)))

  (it "does NOT dispatch on workspace-delete (home + worktrees left intact)"
    ;; `workspace-delete' is the non-destructive command: it never
    ;; deletes :home and so must never fire :on-purge teardown.
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "keepme" wdp-spec--tmp-parent))))
      (wdp-spec--make-workspace "keepme" home)
      (spy-on 'workspace--dispatch-purge-integrations)
      (workspace-delete "keepme")
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)
      ;; Home untouched on disk.
      (expect (file-directory-p home) :to-be t))))

(describe "workspace-delete and workspace-purge keybindings"
  ;; C-x w was promoted to the `workspace-menu' transient (the chord
  ;; bindings `C-x w D' / `C-x w P' were retired in favour of the menu's
  ;; D / P suffixes).  The commands stay reachable through the menu and
  ;; via M-x.
  (it "C-x w opens the workspace-menu transient"
    (expect (key-binding (kbd "C-x w"))
            :to-equal #'workspace-menu))
  (it "workspace-delete remains an interactive command (M-x reachable)"
    (expect (commandp #'workspace-delete) :to-be-truthy))
  (it "workspace-purge remains an interactive command (M-x reachable)"
    (expect (commandp #'workspace-purge) :to-be-truthy)))

(provide 'workspace-delete-purge-spec)
;;; workspace-delete-purge-spec.el ends here

;;; Remote homes (unsupported; user decision 2026-09-27): purge never
;;; contacts the host.  The core fake remote handler
;;; (`workspaces-test-with-fake-remote', workspaces-test-helpers.el) plays
;;; TRAMP with its strict default: `file-remote-p' answers from the name
;;; and ANY other operation, file-NAME operations included, is recorded
;;; and signals, so a purge that stats, tears down or deletes the remote
;;; home fails.

(defconst wdp-spec--remote-prefix "/wdp-remote:"
  "Prefix of the stand-in remote file names below.")

(describe "workspace-purge on a remote :home"
  :var (remote-home prompts)
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil)
    (spy-on 'workspace--dispatch-purge-integrations)
    (setq prompts nil
          remote-home (concat wdp-spec--remote-prefix "/srv/r/"))
    (wdp-spec--make-workspace "r" remote-home t))
  (after-each (wdp-spec--cleanup))

  (it "unregisters after one unregister-only confirmation, with or without C-u, and never contacts the host"
    (dolist (prefix '(nil (4)))
      (clrhash workspace--registry)
      (wdp-spec--make-workspace "r" remote-home t)
      (setq prompts nil)
      (workspaces-test-with-fake-remote wdp-spec--remote-prefix
        (let ((current-prefix-arg prefix)
              (deletes nil))
          (wdp-spec--recording-deletes deletes
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (prompt) (push prompt prompts) t)))
              (workspace-purge "r"))
            (expect deletes :to-equal nil))))
      (expect (gethash "r" workspace--registry) :to-be nil)
      (expect (length prompts) :to-equal 1)
      (expect (car prompts) :to-match "Unregister workspace r")
      (expect (car prompts) :not :to-match "Recursively delete")
      (expect workspaces-test-fake-remote-touches :to-equal nil)
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)))

  (it "cancelling the confirmation leaves the entry registered"
    (workspaces-test-with-fake-remote wdp-spec--remote-prefix
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (expect (workspace-purge "r") :to-throw 'user-error)))
    (expect (gethash "r" workspace--registry) :not :to-be nil)
    (expect workspaces-test-fake-remote-touches :to-equal nil)))

(describe "workspace-purge re-checks scope after the confirmation prompt"
  ;; arch-on-touch-1790453714-69: scope is decided before the prompt and
  ;; the delete happens after it.  If the home leaves the default parent
  ;; while the prompt is open, nothing is torn down or deleted.
  (before-each
    (wdp-spec--reset)
    (spy-on 'workspace--flush-state :and-return-value nil)
    (spy-on 'workspace--dispatch-purge-integrations))
  (after-each (wdp-spec--cleanup))

  (it "does not tear down or delete when the home left scope during the prompt"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "moving" wdp-spec--tmp-parent)))
           (deleted nil)
           (in-scope t))
      (wdp-spec--make-workspace "moving" home)
      (cl-letf (((symbol-function 'workspace--purge-in-scope-p)
                 (lambda (&rest _) in-scope))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq in-scope nil) t)))
        (let ((current-prefix-arg nil))
          (wdp-spec--recording-deletes deleted
            (expect (workspace-purge "moving") :to-throw 'user-error))))
      (expect deleted :to-be nil)
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)
      (expect (file-directory-p home) :to-be t)))

  ;; Scenario: workspaces § Scope is re-checked after the prompt
  (it "leaves the workspace registered, its tab open and its home on disk when the home left scope during the prompt"
    (let* ((home (wdp-spec--seed-home
                  (expand-file-name "moving" wdp-spec--tmp-parent)))
           (deleted nil)
           (in-scope t)
           tabs-before)
      (wdp-spec--make-workspace "moving" home)
      (tab-bar-new-tab)
      (tab-bar-rename-tab "moving")
      (setq tabs-before (length (frame-parameter nil 'tabs)))
      (cl-letf (((symbol-function 'workspace--purge-in-scope-p)
                 (lambda (&rest _) in-scope))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (setq in-scope nil) t)))
        (let ((current-prefix-arg nil))
          (wdp-spec--recording-deletes deleted
            (expect (workspace-purge "moving") :to-throw 'user-error))))
      (expect deleted :to-be nil)
      (expect 'workspace--dispatch-purge-integrations :not :to-have-been-called)
      (expect (file-directory-p home) :to-be t)
      ;; Nothing irreversible happened before the decision: the user asked
      ;; for a purge and got neither a purge nor a delete.
      (expect (gethash "moving" workspace--registry) :not :to-be nil)
      (expect (length (frame-parameter nil 'tabs)) :to-equal tabs-before)
      (expect (workspace--tab-index-for "moving") :not :to-be nil)
      (expect 'workspace--flush-state :not :to-have-been-called))))
