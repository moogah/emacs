;;; autosave-guard-pipeline.el --- speculated boundary -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/autosave-guard-pipeline
;; generated-at: 2026-05-24T20:06:31Z
;; license: implementor-may-revise
;;
;; Boundary shell: the autosave guard pipeline that every autosave
;; trigger funnels through. Explicit-save bypasses stage 1 and enters
;; at stage 2 (the bypass is the structural enforcement of register/
;; invariant/explicit-save-bypasses-anti-save).

;; Stage 1: anti-save-check
(defun workspace--pipeline-anti-save-check (_trigger)
  "Stage 1: consult `workspace-anti-save-predicates'. Return `halt' if
any predicate returns non-nil, else `continue'. Explicit-save callers
MUST NOT enter this stage.

Speculated implementation:
  (if (run-hook-with-args-until-success 'workspace-anti-save-predicates)
      'halt
    'continue)"
  (error "TODO: implement workspace--pipeline-anti-save-check — \
return 'halt when any anti-save predicate fires, else 'continue. The TRIGGER \
argument is for logging only; routing does not depend on it."))

;; Stage 2: capture (shared with explicit-save)
(defun workspace--pipeline-autosave-capture ()
  "Stage 2: capture the current frame's window-state into the same
enriched shape produced by the explicit-save capture path.

Speculated: delegate to the existing capture function so the captured
form is structurally indistinguishable between explicit-save and
autosave entry points (this is what makes the trigger → slot mapping
the only per-trigger decision)."
  (error "TODO: implement workspace--pipeline-autosave-capture — \
call the shared workspace--capture-frameset on (selected-frame) and \
return the enriched window-state form."))

;; Stage 3: slot-route
(defun workspace--pipeline-slot-route (captured trigger)
  "Stage 3: route CAPTURED into the layout slot selected by TRIGGER.

Speculation: explicit-save (TRIGGER = 'explicit-save) writes
:saved-state AND clears :working-state. Every other trigger writes
:working-state and leaves :saved-state untouched (the structural
enforcement of register/invariant/autosave-never-writes-saved-state).

The slot lookup MUST go through `workspace--slot-for-trigger' (the
canonical mapping function in scaffolding/vocabularies/
workspace-state-slot.el). Inlining a pcase here is a duplication bug."
  (ignore captured trigger)
  (error "TODO: implement workspace--pipeline-slot-route — \
(let ((slot (workspace--slot-for-trigger trigger))) \
  (cond ((eq slot :saved-state) \
         ;; write CAPTURED to :saved-state AND nil out :working-state \
         ...) \
        ((eq slot :working-state) \
         ;; write CAPTURED to :working-state ONLY \
         ...)))"))

;; Stage 4: flush
(defun workspace--pipeline-flush (trigger)
  "Stage 4: write the registry to disk. Synchronous for explicit-save
and kill-emacs; debounced for tab-switch and idle-timer (via
workspace-save-state's existing debounce)."
  (ignore trigger)
  (error "TODO: implement workspace--pipeline-flush — \
synchronous for TRIGGER in (explicit-save save-layout kill-emacs new-home-stamp); \
debounced via workspace-save-state for TRIGGER in (tab-switch idle switch-layout)."))

;; Composed entry point — autosave triggers call this; explicit-save
;; calls capture/slot-route/flush directly (bypassing stage 1).
(defun workspace--autosave-pipeline (trigger)
  "Run the full guard pipeline for TRIGGER. Returns one of `halted',
`saved', or signals on stage error."
  (pcase (workspace--pipeline-anti-save-check trigger)
    ('halt
     'halted)
    ('continue
     (let* ((captured (workspace--pipeline-autosave-capture)))
       (workspace--pipeline-slot-route captured trigger)
       (workspace--pipeline-flush trigger)
       'saved))
    (other
     (error "workspace--pipeline-anti-save-check returned %S; \
expected 'halt or 'continue" other))))

(provide 'autosave-guard-pipeline)
;;; autosave-guard-pipeline.el ends here
