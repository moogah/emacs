;;; idle-tick-no-op-off-workspace.test.el --- reconciled invariant -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/idle-tick-no-op-off-workspace
;; generated-at: 2026-05-25T00:30:00Z
;; reconciled-at: 2026-05-25T08:33:00Z
;; reconciled-by: task/idle-save-mode (cycle 2)
;; license: implementor-may-revise
;;
;; The enforcement-locus check for the idle-tick callback:
;; (workspace--current-name) → if nil, return without touching
;; workspace--autosave-current-layout. Cited design.md section: §D6
;; ("Idle save is specifically the 'crash safety' net, not the
;; primary persistence trigger") — the tick is not authoritative on
;; "is there a workspace to save"; it must check.
;;
;; The four scenarios below are also covered in the shipped
;; behavioral spec at
;; config/workspaces/test/workspaces-mode-spec.el (the
;; `workspaces-mode--idle-tick' describe block). This file is the
;; speculation-side mirror: it is intentionally
;; load-the-module-and-assert with no fixture machinery, so a future
;; reader can read it in isolation as the canonical statement of the
;; invariant.

(require 'buttercup)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root
        ;; .../openspec/changes/<change>/scaffolding/invariants/this.el
        ;; → climb five levels to repo root.
        (file-name-as-directory
         (expand-file-name "../../../../.."
                           (file-name-directory this-file)))))
  (load (expand-file-name "config/workspaces/data-model.el"        repo-root))
  (load (expand-file-name "config/workspaces/tabs.el"              repo-root))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "config/workspaces/buffer-membership.el" repo-root))
  (load (expand-file-name "config/workspaces/layouts.el"           repo-root))
  (load (expand-file-name "config/workspaces/persistence.el"       repo-root))
  (load (expand-file-name "config/workspaces/workspaces-mode.el"   repo-root)))

(defvar idle-tick-invariant--source-path
  (let ((this-file (or load-file-name buffer-file-name)))
    (expand-file-name "config/workspaces/workspaces-mode.el"
                      (file-name-as-directory
                       (expand-file-name "../../../../.."
                                         (file-name-directory this-file)))))
  "Absolute path to the tangled workspaces-mode.el (for the
structural scenario).  Captured at load time.")

(describe "Invariant: idle-tick-no-op-off-workspace"

  (it "workspaces-mode--idle-tick does not call workspace--autosave-current-layout when (workspace--current-name) is nil"
    (let ((autosave-calls 0))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () nil))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (&rest _) (cl-incf autosave-calls))))
        (workspaces-mode--idle-tick)
        (expect autosave-calls :to-equal 0))))

  (it "workspaces-mode--idle-tick DOES call workspace--autosave-current-layout :working-state when a current workspace name exists"
    (let ((calls nil))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (slot) (push slot calls))))
        (workspaces-mode--idle-tick)
        (expect (length calls) :to-equal 1)
        (expect (car calls) :to-equal :working-state))))

  (it "workspaces-mode--idle-tick passes :working-state explicitly, never :saved-state and never a default"
    (let ((slot-arg 'unset))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (slot) (setq slot-arg slot))))
        (workspaces-mode--idle-tick)
        (expect (eq slot-arg :working-state) :to-be t)
        (expect (eq slot-arg :saved-state)   :to-be nil))))

  (it "the (when (workspace--current-name) ...) guard is the FIRST form of the callback body"
    (let* ((form
            (with-temp-buffer
              (insert-file-contents idle-tick-invariant--source-path)
              (goto-char (point-min))
              (let (result f)
                (while (and (not result)
                            (setq f (ignore-errors (read (current-buffer)))))
                  (when (and (consp f)
                             (eq (car f) 'defun)
                             (eq (cadr f) 'workspaces-mode--idle-tick))
                    (setq result f)))
                result)))
           (body (cdddr form))
           (first-form (cl-loop for f in body
                                unless (or (stringp f)
                                           (and (consp f)
                                                (memq (car f)
                                                      '(declare interactive))))
                                return f)))
      (expect form :not :to-be nil)
      (expect (and (consp first-form) (eq (car first-form) 'when)) :to-be t)
      (expect (cadr first-form) :to-equal '(workspace--current-name)))))

(provide 'idle-tick-no-op-off-workspace.test)
;;; idle-tick-no-op-off-workspace.test.el ends here
