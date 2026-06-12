;;; scaffold-leave-partial-on-failure.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/scaffold-leave-partial-on-failure
;; generated-at: 2026-05-25T21:35:00Z
;; license: implementor-may-revise
;;
;; Invariant: on any failure of any stage of `workspace-scaffold' (see
;; register/boundary/workspace-scaffold-pipeline), the package
;; SHALL NOT delete partial filesystem state and SHALL NOT mutate the
;; registry. The failure is signalled via `user-error' naming the path.
;;
;; Enforcement mechanism: test (behavioural — there is no cheap
;; structural-audit for "did this function call delete-directory in
;; its error path?"). The cycle-2 Implementor of scaffold-module is
;; expected to satisfy these assertions by writing the failure-path
;; specs into config/workspaces/test/scaffold-spec.el. This scaffold
;; file is the canonical contract; the production spec file MAY
;; lift the assertion shapes from here.

(require 'buttercup)
(require 'cl-lib)

(defmacro scaffold-leave-partial--with-tmp-home (binding &rest body)
  "Bind BINDING to a fresh tmp dir; delete on exit."
  (declare (indent 1))
  `(let ((,(car binding) (make-temp-file "ws-scaffold-fail-" t)))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,(car binding))
         (delete-directory ,(car binding) t)))))

(describe "Invariant: scaffold-leave-partial-on-failure"

  (it "mid-pipeline git-commit failure leaves home.org and sessions/ in place"
    ;; Stage 6 (git commit) is the latest gated stage. Force it to
    ;; fail via cl-letf; assert the pre-stage-6 filesystem state
    ;; (.git/, home.org, sessions/, sessions/<date>-initial.org) is
    ;; all preserved after the user-error signal.
    (error "TODO: implement mid-pipeline-failure case — \
(scaffold-leave-partial--with-tmp-home (home) \
  (cl-letf* ((original-git (symbol-function 'workspace--scaffold-git)) \
             ((symbol-function 'workspace--scaffold-git) \
              (lambda (home &rest args) \
                (if (and args (member \"commit\" args)) \
                    (user-error \"git commit failed (simulated)\") \
                  (apply original-git home args))))) \
    (expect (workspace-scaffold home \"alpha\" :init-and-commit? t) \
            :to-throw 'user-error) \
    (expect (file-directory-p home) :to-be-truthy) \
    (expect (file-directory-p (expand-file-name \".git\" home)) :to-be-truthy) \
    (expect (file-exists-p (expand-file-name \"home.org\" home)) :to-be-truthy) \
    (expect (file-directory-p (expand-file-name \"sessions\" home)) :to-be-truthy) \
    (let ((sessions (expand-file-name \"sessions\" home))) \
      (expect (directory-files sessions nil \"-initial\\\\.org\\\\'\") \
              :not :to-be nil))))"))

  (it "mid-pipeline failure does NOT mutate workspace--registry"
    ;; The caller-side contract: even though workspace-scaffold itself
    ;; doesn't touch the registry, this test pins that no registry
    ;; entry exists for the failed scaffold target.
    (error "TODO: implement no-registry-mutation assertion — \
(scaffold-leave-partial--with-tmp-home (home) \
  (let ((name (file-name-nondirectory (directory-file-name home))) \
        (registry-before (and (boundp 'workspace--registry) \
                              (copy-hash-table workspace--registry)))) \
    (cl-letf (((symbol-function 'workspace--scaffold-git) \
               (lambda (_h &rest _a) (user-error \"git failed\")))) \
      (ignore (condition-case nil \
                  (workspace-scaffold home name :init-and-commit? t) \
                (user-error nil)))) \
    (when registry-before \
      (expect (hash-table-count workspace--registry) \
              :to-equal (hash-table-count registry-before))) \
    (when registry-before \
      (expect (gethash name workspace--registry) :to-be nil))))"))

  (it "the user-error message names the HOME path and the failing step"
    ;; The remedy-hint contract — the error message must give the
    ;; user enough to act. Assert the path is in the message; assert
    ;; either the failing git arg or the failing stage name is named.
    (error "TODO: implement remedy-hint assertion — \
(scaffold-leave-partial--with-tmp-home (home) \
  (cl-letf (((symbol-function 'workspace--scaffold-git) \
             (lambda (_h &rest args) \
               (user-error \"git %s failed\" (mapconcat #'identity args \" \"))))) \
    (condition-case err \
        (workspace-scaffold home \"alpha\" :init-and-commit? t) \
      (user-error \
       (expect (cadr err) :to-match (regexp-quote home)))))) \
NOTE: depending on how workspace-scaffold packages the inner \
user-error, the outer message may or may not include the path. \
If the Implementor chooses to re-signal with the path appended, \
this assertion passes. If they let the inner signal propagate \
verbatim, the assertion may need adjustment — record in ## Discoveries."))

  (it "no delete-directory or delete-file primitive appears in scaffold.el's body"
    ;; Structural assertion. If the Implementor later adds an
    ;; auto-cleanup path (e.g. on git-init failure), this catches it.
    (error "TODO: implement no-delete-primitive lint — \
(let* ((repo-root (let ((here (file-name-directory \
                               (or load-file-name buffer-file-name)))) \
                     (expand-file-name \"../../../../../\" here))) \
       (scaffold-el (expand-file-name \"config/workspaces/scaffold.el\" repo-root))) \
  (when (file-readable-p scaffold-el) \
    (with-temp-buffer \
      (insert-file-contents scaffold-el) \
      (dolist (sym '(delete-directory delete-file)) \
        (goto-char (point-min)) \
        (expect (re-search-forward (concat \"(\" (symbol-name sym) \"\\\\b\") nil t) \
                :to-be nil))))). \
The file might not exist yet at this scaffold's run time; the \
file-readable-p guard makes the assertion vacuously pass until \
scaffold-module lands. Once it lands, the lint becomes real.")))

(provide 'scaffold-leave-partial-on-failure)
;;; scaffold-leave-partial-on-failure.el ends here
