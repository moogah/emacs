;;; workspaces-mode-spec.el --- Behavioral tests for workspaces-mode -*- lexical-binding: t; -*-

;; Covers the five task-body scenarios for the opt-in idle-save mode
;; and the four scaffolded scenarios from
;; openspec/changes/refine-workspaces-two-state-layout/scaffolding/
;; invariants/idle-tick-no-op-off-workspace.test.el — the scaffold's
;; speculated invariants are reconciled here as passing assertions per
;; the scaffolding contract's `license: implementor-may-revise'.

(require 'buttercup)
(require 'cl-lib)

(defvar workspaces-mode-spec--src-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this spec file; captured at load time so
the structural-shape test can locate workspaces-mode.el even after
load completes (when `load-file-name' has gone nil).")

(let ((dir workspaces-mode-spec--src-dir))
  (load (expand-file-name "../data-model.el"     dir))
  (load (expand-file-name "../tabs.el"           dir))
  (unless (featurep 'bufferlo)
    (defalias 'bufferlo-mode (lambda (&optional _) nil))
    (provide 'bufferlo))
  (load (expand-file-name "../buffer-membership.el" dir))
  (load (expand-file-name "../layouts.el"           dir))
  (load (expand-file-name "../persistence.el"       dir))
  (load (expand-file-name "../workspaces-mode.el"   dir)))

(defvar workspace-home-builder #'workspace-default-home-builder)

;; Sentinel passed into our mock run-with-idle-timer so disable-path
;; tests have a non-nil timer object to cancel.
(defvar workspaces-mode-spec--fake-timer-sentinel
  '(fake-timer))

(defun workspaces-mode-spec--reset ()
  "Ensure the mode is OFF and the timer slot is empty before each test.
Force-cancels any sentinel timer left by a previous test without
calling real `cancel-timer' (the sentinel is not a real timerp)."
  (cl-letf (((symbol-function 'cancel-timer) (lambda (_) nil)))
    (when workspaces-mode (workspaces-mode -1)))
  (setq workspaces-mode--idle-timer nil))

(describe "workspaces-mode toggle"
  (before-each (workspaces-mode-spec--reset))
  (after-each  (workspaces-mode-spec--reset))

  (it "registers an idle timer with `workspaces-mode-idle-frequency' and the tick callback"
    (let ((captured nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (secs repeat fn &rest _args)
                   (setq captured (list secs repeat fn))
                   workspaces-mode-spec--fake-timer-sentinel)))
        (let ((workspaces-mode-idle-frequency 60))
          (workspaces-mode 1))
        (expect (nth 0 captured) :to-equal 60)
        ;; Second arg is the repeat flag — must be non-nil so the timer
        ;; re-fires on every idle interval, not just once per idle.
        (expect (nth 1 captured) :not :to-be nil)
        (expect (nth 2 captured) :to-equal #'workspaces-mode--idle-tick))))

  (it "cancels the stored timer on disable"
    (let ((cancel-args nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _)
                   workspaces-mode-spec--fake-timer-sentinel))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancel-args))))
        (workspaces-mode 1)
        (expect workspaces-mode--idle-timer
                :to-equal workspaces-mode-spec--fake-timer-sentinel)
        (workspaces-mode -1)
        (expect cancel-args
                :to-equal (list workspaces-mode-spec--fake-timer-sentinel))
        (expect workspaces-mode--idle-timer :to-be nil))))

  (it "leaves the timer slot nil when `workspaces-mode-idle-frequency' is nil"
    (let ((scheduled nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest args)
                   (setq scheduled args)
                   workspaces-mode-spec--fake-timer-sentinel)))
        (let ((workspaces-mode-idle-frequency nil))
          (workspaces-mode 1))
        ;; Disabled idle-frequency: mode is "on" but no timer was
        ;; installed.  The explicit save and tab-switch autosave keep
        ;; running independently.
        (expect scheduled :to-be nil)
        (expect workspaces-mode--idle-timer :to-be nil)))))

(describe "workspaces-mode--idle-tick"
  (before-each (workspaces-mode-spec--reset))
  (after-each  (workspaces-mode-spec--reset))

  ;; --- Scaffolded scenarios (reconciled here) ----------------------

  (it "does not call workspace--autosave-current-layout when (workspace--current-name) is nil"
    ;; Scaffold scenario 1: idle-tick-no-op-off-workspace.
    (let ((autosave-calls 0))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () nil))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (&rest _) (cl-incf autosave-calls))))
        (workspaces-mode--idle-tick)
        (expect autosave-calls :to-equal 0))))

  (it "calls workspace--autosave-current-layout exactly once with :working-state, then flushes synchronously"
    ;; Scaffold scenario 2: positive-path companion to scenario 1.
    ;; Also pins the synchronous flush: the idle tick must write to disk
    ;; itself (no debounce) once the capture is taken.
    (let ((calls nil)
          (flush-calls 0))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (slot) (push slot calls)))
                ((symbol-function 'workspace--flush-state)
                 (lambda (&rest _) (cl-incf flush-calls))))
        (workspaces-mode--idle-tick)
        (expect (length calls) :to-equal 1)
        (expect (car calls) :to-equal :working-state)
        (expect flush-calls :to-equal 1))))

  (it "passes :working-state explicitly (eq, not just present) — never :saved-state, never a default"
    ;; Scaffold scenario 3: pin the keyword by eq to catch an off-by-
    ;; one keyword that would silently corrupt the saved baseline.
    (let ((slot-arg 'unset))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (slot) (setq slot-arg slot)))
                ((symbol-function 'workspace--flush-state)
                 (lambda (&rest _) nil)))
        (workspaces-mode--idle-tick)
        (expect (eq slot-arg :working-state) :to-be t)
        (expect (eq slot-arg :saved-state)   :to-be nil))))

  (it "guards on (workspace--current-name) as the FIRST form of the callback body"
    ;; Scaffold scenario 4: structural assertion.  Reading the SOURCE
    ;; (not the byte-compiled function cell) is intentional: it
    ;; matches the scaffold's prescription ("Read the tangled config/
    ;; workspaces/workspaces-mode.el via find-file") and is immune to
    ;; byte-compilation reshaping the function body.
    (let* ((src (expand-file-name "../workspaces-mode.el"
                                  workspaces-mode-spec--src-dir))
           (form
            (with-temp-buffer
              (insert-file-contents src)
              (goto-char (point-min))
              ;; Use distinct names (`f' as the scratch read, `result'
              ;; as the accumulator) so we do not `setq' through a
              ;; lexical binding under construction in the outer
              ;; `let*'.
              (let (result f)
                (while (and (not result)
                            (setq f (ignore-errors
                                      (read (current-buffer)))))
                  (when (and (consp f)
                             (eq (car f) 'defun)
                             (eq (cadr f) 'workspaces-mode--idle-tick))
                    (setq result f)))
                result)))
           ;; defun shape: (defun NAME ARGS [DOCSTRING] [DECL] BODY...)
           (body (cdddr form))
           (first-form (cl-loop for f in body
                                unless (or (stringp f)
                                           (and (consp f)
                                                (memq (car f)
                                                      '(declare interactive))))
                                return f)))
      (expect form :not :to-be nil)
      (expect (and (consp first-form) (eq (car first-form) 'when)) :to-be t)
      (expect (cadr first-form) :to-equal '(workspace--current-name))))

  ;; --- Task-body scenario 5: anti-save predicates regression -------

  (it "skips autosave when an anti-save predicate returns non-nil"
    ;; Stage-1 entry-point #4 of register/boundary/autosave-guard-
    ;; pipeline.  The (unless run-hook-with-args-until-success ...)
    ;; wrap nests inside the current-name guard.  If any predicate in
    ;; `workspace-anti-save-predicates' returns non-nil, the idle tick
    ;; MUST NOT call `workspace--autosave-current-layout'.
    (let ((calls 0))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (_slot) (cl-incf calls)))
                (workspace-anti-save-predicates (list (lambda () t))))
        (workspaces-mode--idle-tick)
        (expect calls :to-equal 0)))
    ;; Negative-path companion: when every predicate returns nil, the
    ;; idle tick proceeds to call the autosave.  Pins the wrap shape
    ;; from both directions.
    (let ((calls 0))
      (cl-letf (((symbol-function 'workspace--current-name)
                 (lambda () "alpha"))
                ((symbol-function 'workspace--autosave-current-layout)
                 (lambda (_slot) (cl-incf calls)))
                ((symbol-function 'workspace--flush-state)
                 (lambda (&rest _) nil))
                (workspace-anti-save-predicates (list (lambda () nil))))
        (workspaces-mode--idle-tick)
        (expect calls :to-equal 1)))))

(provide 'workspaces-mode-spec)
;;; workspaces-mode-spec.el ends here
