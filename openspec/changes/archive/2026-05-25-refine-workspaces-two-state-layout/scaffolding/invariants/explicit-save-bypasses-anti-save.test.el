;;; explicit-save-bypasses-anti-save.test.el --- invariant pin -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/explicit-save-bypasses-anti-save
;; generated-at: 2026-05-24T20:06:31Z
;; reconciled-at: 2026-05-25T08:33:15Z
;; license: implementor-may-revise
;;
;; Status: REAL after cycle-2 anti-save-predicates task landed.  The
;; three scenarios pin the invariant that workspace-save proceeds
;; unconditionally while autosave triggers honour
;; workspace-anti-save-predicates.  The shipped behavioral coverage
;; lives in `config/workspaces/test/anti-save-spec.el'; this file
;; replays the same fixture against the boundary so a future
;; refactor that collapses workspace-save and autosave into one
;; helper breaks here too.

(require 'buttercup)
(require 'cl-lib)
(require 'tab-bar)
(require 'frameset)

;; Load the workspaces stack relative to the repo root.  This scaffold
;; is run by buttercup as part of the same `make test` sweep that runs
;; the in-tree spec, so we resolve paths from the project root.
(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../../../../" here))
       (mod  (lambda (p) (expand-file-name p root))))
  (load (funcall mod "config/workspaces/data-model.el"))
  (load (funcall mod "config/workspaces/tabs.el"))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (funcall mod "config/workspaces/buffer-membership.el"))
  (load (funcall mod "config/workspaces/layouts.el"))
  (load (funcall mod "config/workspaces/persistence.el"))
  (load (funcall mod "config/workspaces/workspaces.el")))

(defvar workspace-home-builder #'workspace-default-home-builder)

(defvar explicit-save-bypass--tmp-dir nil)

(defun explicit-save-bypass--reset ()
  (clrhash workspace--registry)
  (let ((tabs (frame-parameter nil 'tabs)))
    (when (> (length tabs) 1)
      (dotimes (_ (1- (length tabs)))
        (tab-bar-close-tab 2))))
  (setq explicit-save-bypass--tmp-dir
        (make-temp-file "ws-bypass-" t)))

(defun explicit-save-bypass--cleanup ()
  (when (and explicit-save-bypass--tmp-dir
             (file-directory-p explicit-save-bypass--tmp-dir))
    (delete-directory explicit-save-bypass--tmp-dir t)))

(defmacro explicit-save-bypass--with-state-file (&rest body)
  (declare (indent 0))
  `(let ((tmp explicit-save-bypass--tmp-dir))
     (cl-letf (((symbol-function 'workspace--state-directory)
                (lambda () (file-name-as-directory tmp)))
               ((symbol-function 'workspace--state-file)
                (lambda () (expand-file-name "workspaces.eld"
                                             (file-name-as-directory tmp)))))
       ,@body)))

(defun explicit-save-bypass--layout (ws-name)
  (let* ((ws (gethash ws-name workspace--registry))
         (group-name (workspace--recent-group ws))
         (group (and group-name (workspace--find-group ws group-name))))
    (and group (workspace--group-recent-layout group))))

(describe "Invariant: explicit-save-bypasses-anti-save"
  (before-each (explicit-save-bypass--reset))
  (after-each (explicit-save-bypass--cleanup))

  (it "workspace-save proceeds even when an always-non-nil predicate is registered"
    (explicit-save-bypass--with-state-file
      (workspace-new "alpha")
      (let ((workspace-anti-save-predicates (list (lambda () t))))
        (workspace-save))
      (let ((layout (explicit-save-bypass--layout "alpha")))
        (expect (workspace--layout-saved-state layout) :not :to-be nil))
      (expect (file-exists-p (workspace--state-file)) :to-be-truthy)))

  (it "tab-switch autosave IS suppressed by the same predicate"
    (explicit-save-bypass--with-state-file
      (workspace-new "alpha")
      (workspace-save)
      (let ((saved-before (workspace--layout-saved-state
                           (explicit-save-bypass--layout "alpha")))
            (workspace-anti-save-predicates (list (lambda () t))))
        (delete-other-windows)
        (split-window-right)
        (tab-bar-select-tab 1)
        (let ((layout (explicit-save-bypass--layout "alpha")))
          (expect (workspace--layout-working-state layout) :to-be nil)
          (expect (workspace--layout-saved-state layout)
                  :to-equal saved-before)))))

  (it "workspace-save's entry point does not call run-hook-with-args-until-success"
    ;; Structural assertion: grep the tangled persistence.el for the
    ;; predicate-consultation form and confirm it does NOT appear
    ;; inside the body of `workspace-save'.  The acceptable shape is
    ;; (defun workspace-save ... <no run-hook... predicates> ...).
    (let* ((here (file-name-directory (or load-file-name buffer-file-name)))
           (root (expand-file-name "../../../../" here))
           (file (expand-file-name "config/workspaces/persistence.el" root))
           (form-re
            (rx "(run-hook-with-args-until-success"
                (one-or-more (or space "\n"))
                "'workspace-anti-save-predicates")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Anchor on the open paren after the name so `\b` does not
        ;; silently match `workspace-save-state' (which sits earlier
        ;; in the file).  Without this anchor the structural
        ;; assertion ran against the wrong defun and was a no-op.
        (re-search-forward "^(defun workspace-save +(")
        (let ((body-start (point))
              (body-end (save-excursion
                          (goto-char (match-beginning 0))
                          (forward-sexp)
                          (point))))
          (goto-char body-start)
          (expect (re-search-forward form-re body-end t) :to-be nil))))))

(provide 'explicit-save-bypasses-anti-save.test)
;;; explicit-save-bypasses-anti-save.test.el ends here
