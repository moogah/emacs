;;; backend-invocation-spec.el --- Buttercup tests for gptel-chat FSM handler chain -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for the chained FSM handlers delivered by task
;; `fsm-handlers' in the `gptel-chat-mode' OpenSpec change.  The
;; handler alist `gptel-chat--fsm-handlers' wires five UI-only
;; handlers onto upstream's request FSM so chat-mode buffers can
;; surface lifecycle state (waiting / streaming / tool-running /
;; done / error) via the buffer-local `gptel-chat--lifecycle-state'
;; without cracking open `gptel--fsm-last' directly.
;;
;; Two invariants under test (design.md §Decision 3):
;;
;;   1. Our handler chains BEFORE upstream's handler for WAIT and
;;      TOOL — UI state updates precede the transition-driving
;;      logic that would otherwise fire the network request or run
;;      tool calls.
;;
;;   2. Our handlers are UI-only: they never call
;;      `gptel--fsm-transition' themselves.  Transitions are driven
;;      solely by upstream's chained handlers (`gptel--handle-wait',
;;      `gptel--handle-tool-use') and by the curl/url response
;;      pipeline (for TYPE/DONE/ERRS entry).
;;
;; Approach: we build a real `gptel-fsm' struct populated with our
;; handler alist, spy on upstream's chained handlers AND on
;; `gptel--fsm-transition' itself (to isolate our handlers from any
;; real side effects), then call `gptel--fsm-transition' to drive
;; scripted entry into each state.  Assertions cover (a) the
;; lifecycle indicator transitions, (b) our handlers run, (c) they
;; run before the chained upstream handlers, and (d) they never
;; invoke `gptel--fsm-transition' directly.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/send/;
;; two levels up is .../config/gptel/chat/, which holds `send.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel)
(require 'gptel-chat-send)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-send-test--buffer nil
  "Scratch chat buffer used by the FSM-handler specs.")

(defun gptel-chat-send-test--setup ()
  "Create a fresh request buffer for each example."
  (setq gptel-chat-send-test--buffer
        (generate-new-buffer " *gptel-chat-send-test*")))

(defun gptel-chat-send-test--cleanup ()
  "Tear down the fixture buffer after each example."
  (when (buffer-live-p gptel-chat-send-test--buffer)
    (kill-buffer gptel-chat-send-test--buffer))
  (setq gptel-chat-send-test--buffer nil))

(defun gptel-chat-send-test--make-fsm ()
  "Build a `gptel-fsm' wired with our handler alist.
The INFO slot carries `:buffer' pointing at the fixture buffer so
the UI handlers have somewhere to deposit the lifecycle symbol."
  (gptel-make-fsm
   :handlers gptel-chat--fsm-handlers
   :info     (list :buffer gptel-chat-send-test--buffer)))

(defun gptel-chat-send-test--lifecycle ()
  "Return the lifecycle indicator set in the fixture buffer."
  (buffer-local-value 'gptel-chat--lifecycle-state
                      gptel-chat-send-test--buffer))


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat FSM handlers (task fsm-handlers)"

  (before-each
    (gptel-chat-send-test--setup)
    ;; Stub upstream's chained handlers for WAIT / TOOL so entering
    ;; those states does not fire a network request or execute tool
    ;; calls.  We deliberately do NOT stub `gptel--handle-post' here
    ;; -- it is cheap and has no external side effects (it just
    ;; iterates the caller-supplied :post list on info), and the
    ;; `:post hook chaining on terminal states' specs need the real
    ;; implementation to run.  Individual specs that need to observe
    ;; ordering stub it locally.  Also spy on the transition
    ;; primitive itself so we can assert our handlers never invoke
    ;; it.
    (spy-on 'gptel--handle-wait)
    (spy-on 'gptel--handle-tool-use)
    (spy-on 'gptel--fsm-transition :and-call-through))

  (after-each (gptel-chat-send-test--cleanup))


  (describe "handler alist shape"

    (it "chains our WAIT handler BEFORE upstream's gptel--handle-wait"
      (let ((entry (alist-get 'WAIT gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-wait)
        (expect (cadr entry) :to-equal #'gptel--handle-wait)))

    (it "chains our TOOL handler BEFORE upstream's gptel--handle-tool-use"
      (let ((entry (alist-get 'TOOL gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-tool)
        (expect (cadr entry) :to-equal #'gptel--handle-tool-use)))

    (it "lists a lone UI handler for TYPE (no upstream handler in the chain)"
      (let ((entry (alist-get 'TYPE gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-type)
        (expect (length entry) :to-equal 1)))

    (it "chains our DONE handler BEFORE upstream's gptel--handle-post"
      (let ((entry (alist-get 'DONE gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-done)
        (expect (cadr entry) :to-equal #'gptel--handle-post)))

    (it "chains our ERRS handler BEFORE upstream's gptel--handle-post"
      (let ((entry (alist-get 'ERRS gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-errs)
        (expect (cadr entry) :to-equal #'gptel--handle-post)))

    (it "chains our ABRT handler BEFORE upstream's gptel--handle-post"
      (let ((entry (alist-get 'ABRT gptel-chat--fsm-handlers)))
        (expect (car entry)  :to-equal #'gptel-chat--on-abrt)
        (expect (cadr entry) :to-equal #'gptel--handle-post))))


  (describe "lifecycle indicator transitions"

    (it "sets `waiting' on entry to WAIT"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'gptel-chat--on-wait :and-call-through)
        (gptel--fsm-transition fsm 'WAIT)
        (expect 'gptel-chat--on-wait :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'waiting)))

    (it "sets `streaming' on entry to TYPE"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'gptel-chat--on-type :and-call-through)
        (gptel--fsm-transition fsm 'TYPE)
        (expect 'gptel-chat--on-type :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'streaming)))

    (it "sets `tool-running' on entry to TOOL"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'gptel-chat--on-tool :and-call-through)
        (gptel--fsm-transition fsm 'TOOL)
        (expect 'gptel-chat--on-tool :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'tool-running)))

    (it "clears the indicator on entry to DONE"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        ;; Seed a non-nil value so "clear" is observable.
        (with-current-buffer gptel-chat-send-test--buffer
          (setq gptel-chat--lifecycle-state 'streaming))
        (spy-on 'gptel-chat--on-done :and-call-through)
        (gptel--fsm-transition fsm 'DONE)
        (expect 'gptel-chat--on-done :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal nil)))

    (it "sets `error' and logs a message on entry to ERRS"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'gptel-chat--on-errs :and-call-through)
        (spy-on 'message)
        (gptel--fsm-transition fsm 'ERRS)
        (expect 'gptel-chat--on-errs :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'error)
        (expect 'message :to-have-been-called)))

    (it "sets `aborted' and logs a message on entry to ABRT"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        ;; Seed a non-nil busy value so "abort transition clears busy
        ;; state" is observable.
        (with-current-buffer gptel-chat-send-test--buffer
          (setq gptel-chat--lifecycle-state 'streaming))
        (spy-on 'gptel-chat--on-abrt :and-call-through)
        (spy-on 'message)
        (gptel--fsm-transition fsm 'ABRT)
        (expect 'gptel-chat--on-abrt :to-have-been-called)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'aborted)
        (expect 'message :to-have-been-called)))

    (it "walks a WAIT -> TYPE -> TOOL -> DONE sequence and ends idle"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (gptel--fsm-transition fsm 'WAIT)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'waiting)
        (gptel--fsm-transition fsm 'TYPE)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'streaming)
        (gptel--fsm-transition fsm 'TOOL)
        (expect (gptel-chat-send-test--lifecycle) :to-equal 'tool-running)
        (gptel--fsm-transition fsm 'DONE)
        (expect (gptel-chat-send-test--lifecycle) :to-equal nil))))


  (describe "chaining order (our handler runs before upstream's)"

    (it "fires gptel-chat--on-wait strictly before gptel--handle-wait"
      (let ((fsm (gptel-chat-send-test--make-fsm))
            (call-log '()))
        (spy-on 'gptel-chat--on-wait
                :and-call-fake
                (lambda (_fsm) (push 'ours call-log)))
        ;; Re-spy upstream with a logger; before-each already installed
        ;; a plain stub — replace it so ordering is observable.
        (spy-on 'gptel--handle-wait
                :and-call-fake
                (lambda (_fsm) (push 'upstream call-log)))
        (gptel--fsm-transition fsm 'WAIT)
        (expect (nreverse call-log) :to-equal '(ours upstream))))

    (it "fires gptel-chat--on-tool strictly before gptel--handle-tool-use"
      (let ((fsm (gptel-chat-send-test--make-fsm))
            (call-log '()))
        (spy-on 'gptel-chat--on-tool
                :and-call-fake
                (lambda (_fsm) (push 'ours call-log)))
        (spy-on 'gptel--handle-tool-use
                :and-call-fake
                (lambda (_fsm) (push 'upstream call-log)))
        (gptel--fsm-transition fsm 'TOOL)
        (expect (nreverse call-log) :to-equal '(ours upstream))))

    (it "fires gptel-chat--on-done strictly before gptel--handle-post"
      (let ((fsm (gptel-chat-send-test--make-fsm))
            (call-log '()))
        (spy-on 'gptel-chat--on-done
                :and-call-fake
                (lambda (_fsm) (push 'ours call-log)))
        (spy-on 'gptel--handle-post
                :and-call-fake
                (lambda (_fsm) (push 'upstream call-log)))
        (gptel--fsm-transition fsm 'DONE)
        (expect (nreverse call-log) :to-equal '(ours upstream))))

    (it "fires gptel-chat--on-errs strictly before gptel--handle-post"
      (let ((fsm (gptel-chat-send-test--make-fsm))
            (call-log '()))
        (spy-on 'gptel-chat--on-errs
                :and-call-fake
                (lambda (_fsm) (push 'ours call-log)))
        (spy-on 'gptel--handle-post
                :and-call-fake
                (lambda (_fsm) (push 'upstream call-log)))
        (spy-on 'message)               ; silence the ERRS log
        (gptel--fsm-transition fsm 'ERRS)
        (expect (nreverse call-log) :to-equal '(ours upstream))))

    (it "fires gptel-chat--on-abrt strictly before gptel--handle-post"
      (let ((fsm (gptel-chat-send-test--make-fsm))
            (call-log '()))
        (spy-on 'gptel-chat--on-abrt
                :and-call-fake
                (lambda (_fsm) (push 'ours call-log)))
        (spy-on 'gptel--handle-post
                :and-call-fake
                (lambda (_fsm) (push 'upstream call-log)))
        (spy-on 'message)               ; silence the ABRT log
        (gptel--fsm-transition fsm 'ABRT)
        (expect (nreverse call-log) :to-equal '(ours upstream)))))


  (describe "UI-only invariant: handlers do not drive transitions"

    (it "gptel-chat--on-wait does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (gptel-chat--on-wait fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called)))

    (it "gptel-chat--on-type does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (gptel-chat--on-type fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called)))

    (it "gptel-chat--on-tool does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (gptel-chat--on-tool fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called)))

    (it "gptel-chat--on-done does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (gptel-chat--on-done fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called)))

    (it "gptel-chat--on-errs does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'message)                 ; silence the side-effect log
        (gptel-chat--on-errs fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called)))

    (it "gptel-chat--on-abrt does not call gptel--fsm-transition"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (spy-on 'message)                 ; silence the side-effect log
        (gptel-chat--on-abrt fsm)
        (expect 'gptel--fsm-transition :not :to-have-been-called))))


  (describe "handler robustness"

    (it "is a silent no-op when the request buffer has been killed"
      (let ((fsm (gptel-chat-send-test--make-fsm)))
        (kill-buffer gptel-chat-send-test--buffer)
        (expect (gptel-chat--on-wait fsm) :not :to-throw)))

    (it "is a silent no-op when :buffer is missing from info"
      (let ((fsm (gptel-make-fsm
                  :handlers gptel-chat--fsm-handlers
                  :info     nil)))
        (expect (gptel-chat--on-done fsm) :not :to-throw))))


  (describe ":post hook chaining on terminal states"
    ;; Upstream's `gptel--handle-post' is the mechanism that runs any
    ;; caller-supplied `:post' functions on the request info plist
    ;; (`gptel-request.el':1754-1758).  Our handler alist chains it on
    ;; DONE / ERRS / ABRT so callers that rely on `:post' — future
    ;; session export, budget tracking, activities integration — do
    ;; not silently lose their hook.
    ;;
    ;; These specs run the real `gptel--handle-post' (the enclosing
    ;; before-each does not stub it) and observe the caller-supplied
    ;; post hook actually firing on each terminal state.

    (it "fires :post callback on DONE"
      (let* ((fired nil)
             (post-fn (lambda (_info) (setq fired t)))
             (fsm (gptel-make-fsm
                   :handlers gptel-chat--fsm-handlers
                   :info (list :buffer gptel-chat-send-test--buffer
                               :post (list post-fn)))))
        (gptel--fsm-transition fsm 'DONE)
        (expect fired :to-be t)))

    (it "fires :post callback on ERRS"
      (let* ((fired nil)
             (post-fn (lambda (_info) (setq fired t)))
             (fsm (gptel-make-fsm
                   :handlers gptel-chat--fsm-handlers
                   :info (list :buffer gptel-chat-send-test--buffer
                               :post (list post-fn)))))
        (spy-on 'message)               ; silence the ERRS log
        (gptel--fsm-transition fsm 'ERRS)
        (expect fired :to-be t)))

    (it "fires :post callback on ABRT"
      (let* ((fired nil)
             (post-fn (lambda (_info) (setq fired t)))
             (fsm (gptel-make-fsm
                   :handlers gptel-chat--fsm-handlers
                   :info (list :buffer gptel-chat-send-test--buffer
                               :post (list post-fn)))))
        (spy-on 'message)               ; silence the ABRT log
        (gptel--fsm-transition fsm 'ABRT)
        (expect fired :to-be t))))


  (describe "send-guard idle-state contract"
    ;; Delivered by task `send-command': `gptel-chat-send' rejects new
    ;; sends while a request is in flight, and does NOT reject on idle
    ;; or terminal states.  Source of truth is the buffer-local
    ;; `gptel--fsm-last' (design.md §Decision 11).
    ;;
    ;; Scope here: exercise the guard across the six terminal FSM
    ;; states.  DONE / ERRS / ABRT must be idle so a completed,
    ;; errored, or user-aborted request does not wedge the buffer;
    ;; WAIT / TYPE / TOOL must block a second concurrent send.  Each
    ;; spec seeds `gptel--fsm-last' with a real `gptel-fsm' whose
    ;; state is set directly via `setf' (no handler chain needed —
    ;; the guard only reads `gptel-fsm-state').
    ;;
    ;; Each spec also installs a populated `#+begin_user' block with
    ;; point on its body so that, on the idle path, `gptel-chat-send'
    ;; reaches the real `gptel-request' call.  That call is stubbed
    ;; out via `spy-on' so no network traffic occurs.

    (defun gptel-chat-send-test--populate-user-block ()
      "Insert a populated user block into the fixture and leave point in its body."
      (with-current-buffer gptel-chat-send-test--buffer
        (erase-buffer)
        (insert "#+begin_user\nhello model\n#+end_user\n")
        (goto-char (point-min))
        (forward-line 1)))        ; land on the body line

    (defun gptel-chat-send-test--seed-state (state)
      "Set `gptel--fsm-last' in the fixture buffer to a fresh FSM in STATE."
      (with-current-buffer gptel-chat-send-test--buffer
        (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
          (setf (gptel-fsm-state fsm) state)
          (setq gptel--fsm-last fsm))))

    (describe "idle states permit send"
      ;; DONE / ERRS / ABRT — the three terminal states — are all
      ;; idle.  `gptel-chat-send' must proceed to `gptel-request'
      ;; without signalling.  The guard also treats a fresh FSM
      ;; state of `INIT' as idle (not explicitly tested here; covered
      ;; by the "nil gptel--fsm-last" case below where no FSM exists).

      (dolist (state '(DONE ERRS ABRT))
        (it (format "proceeds when gptel--fsm-last is in %s" state)
          (gptel-chat-send-test--populate-user-block)
          (gptel-chat-send-test--seed-state state)
          (spy-on 'gptel-request :and-return-value nil)
          (with-current-buffer gptel-chat-send-test--buffer
            (expect (gptel-chat-send) :not :to-throw))
          (expect 'gptel-request :to-have-been-called)))

      (it "proceeds when gptel--fsm-last is nil (no prior request)"
        (gptel-chat-send-test--populate-user-block)
        (with-current-buffer gptel-chat-send-test--buffer
          (setq gptel--fsm-last nil))
        (spy-on 'gptel-request :and-return-value nil)
        (with-current-buffer gptel-chat-send-test--buffer
          (expect (gptel-chat-send) :not :to-throw))
        (expect 'gptel-request :to-have-been-called)))

    (describe "in-flight states raise user-error"
      ;; WAIT / TYPE / TOOL are the three in-flight states; a second
      ;; `gptel-chat-send' while the FSM sits in any of them must
      ;; raise `user-error' and MUST NOT call `gptel-request'.

      (dolist (state '(WAIT TYPE TOOL))
        (it (format "rejects send when gptel--fsm-last is in %s" state)
          (gptel-chat-send-test--populate-user-block)
          (gptel-chat-send-test--seed-state state)
          (spy-on 'gptel-request)
          (with-current-buffer gptel-chat-send-test--buffer
            (expect (gptel-chat-send) :to-throw 'user-error))
          (expect 'gptel-request :not :to-have-been-called)))))


  (describe "gptel-chat--state accessor"

    (it "returns nil when no request has run in this buffer"
      (with-current-buffer gptel-chat-send-test--buffer
        ;; `gptel--fsm-last' is a defvar-local initialised to nil.
        (setq gptel--fsm-last nil)
        (expect (gptel-chat--state) :to-equal nil)))

    (it "returns the current state from the buffer-local gptel--fsm-last"
      (with-current-buffer gptel-chat-send-test--buffer
        (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
          ;; Drive to TYPE directly via setf on the struct slot to
          ;; avoid invoking handler chains in this accessor test.
          (setf (gptel-fsm-state fsm) 'TYPE)
          (setq gptel--fsm-last fsm)
          (expect (gptel-chat--state) :to-equal 'TYPE))))))


(provide 'backend-invocation-spec)

;;; backend-invocation-spec.el ends here
