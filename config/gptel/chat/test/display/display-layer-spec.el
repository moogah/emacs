;;; display-layer-spec.el --- Buttercup tests for gptel-chat display layer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-display' — the overlay-based role
;; distinction layer and its toggle command.
;;
;; Coverage (from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md
;; §"Display-layer role distinction"):
;;   - Display layer is active by default (overlays on user + assistant
;;     block bodies with role-specific faces).
;;   - Display layer can be toggled off (overlays removed, buffer text
;;     unchanged).
;;   - Toggle back on → overlays reappear.
;;   - Buffer text is never modified (buffer-substring-no-properties
;;     equals the on-disk string).
;;
;; All tests drive `gptel-chat-mode' directly so the hook-installed
;; activation path (debounced after-change + initial refresh) is
;; exercised end-to-end.  No mocking of overlays or parser.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/display/;
;; two levels up is .../config/gptel/chat/, which holds `display.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-display)
(require 'gptel-chat-send)


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-display-test--two-turns
  (concat "#+begin_user\n"
          "Hello there.\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "General Kenobi.\n"
          "#+end_assistant\n")
  "Minimal buffer content with one user turn and one assistant turn.")

(defun gptel-chat-display-test--display-overlays (&optional buffer)
  "Return the list of `gptel-chat-display'-tagged overlays in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cl-remove-if-not
     (lambda (ov) (overlay-get ov 'gptel-chat-display))
     (overlays-in (point-min) (point-max)))))

(defun gptel-chat-display-test--overlays-with-face (face &optional buffer)
  "Return `gptel-chat-display' overlays in BUFFER whose `face' property equals FACE."
  (cl-remove-if-not
   (lambda (ov) (eq (overlay-get ov 'face) face))
   (gptel-chat-display-test--display-overlays buffer)))

(defmacro gptel-chat-display-test--with-chat-buffer (content &rest body)
  "Create a temp buffer populated with CONTENT in `gptel-chat-mode' and run BODY.

The buffer is killed on exit.  Activation runs the real
`gptel-chat-mode-hook', which installs the display layer's
after-change hook and runs the initial overlay refresh
synchronously."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *gptel-chat-display-test*")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,content)
           (goto-char (point-min))
           (gptel-chat-mode)
           ,@body)
       (kill-buffer buf))))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat-display: default activation"

  (it "installs overlays on both user and assistant block bodies"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let ((user-ovs (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-user-face))
           (asst-ovs (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-assistant-face)))
       (expect (length user-ovs) :to-equal 1)
       (expect (length asst-ovs) :to-equal 1))))

  (it "overlays cover block bodies but NOT delimiter lines"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let* ((user-ov (car (gptel-chat-display-test--overlays-with-face
                           'gptel-chat-user-face)))
            (user-text (buffer-substring-no-properties
                        (overlay-start user-ov)
                        (overlay-end user-ov))))
       ;; The body text is exactly "Hello there.\n" — no delimiters.
       (expect user-text :to-equal "Hello there.\n")
       (expect user-text :not :to-match "#\\+begin_user")
       (expect user-text :not :to-match "#\\+end_user"))
     (let* ((asst-ov (car (gptel-chat-display-test--overlays-with-face
                           'gptel-chat-assistant-face)))
            (asst-text (buffer-substring-no-properties
                        (overlay-start asst-ov)
                        (overlay-end asst-ov))))
       (expect asst-text :to-equal "General Kenobi.\n")
       (expect asst-text :not :to-match "#\\+begin_assistant")
       (expect asst-text :not :to-match "#\\+end_assistant"))))

  (it "does not modify buffer text (buffer-substring-no-properties matches input)"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect (buffer-substring-no-properties (point-min) (point-max))
             :to-equal gptel-chat-display-test--two-turns)))

  (it "starts with `gptel-chat--display-enabled' set to t"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect gptel-chat--display-enabled :to-be-truthy))))


(describe "gptel-chat-display: overlay tagging"

  (it "every installed overlay carries the `gptel-chat-display' property"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let ((ovs (overlays-in (point-min) (point-max))))
       ;; Every overlay this module installs must be tagged, so
       ;; `remove-overlays ... gptel-chat-display t' can target them.
       (dolist (ov (cl-remove-if-not
                    (lambda (o) (memq (overlay-get o 'face)
                                      '(gptel-chat-user-face
                                        gptel-chat-assistant-face)))
                    ovs))
         (expect (overlay-get ov 'gptel-chat-display) :to-be-truthy))))))


(describe "gptel-chat-toggle-display-layer"

  (it "removes all gptel-chat-display overlays when toggled off"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 2)
     (gptel-chat-toggle-display-layer)
     (expect gptel-chat--display-enabled :to-equal nil)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0)))

  (it "keeps buffer text unchanged when toggled off"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (gptel-chat-toggle-display-layer)
     (expect (buffer-substring-no-properties (point-min) (point-max))
             :to-equal gptel-chat-display-test--two-turns)))

  (it "reinstalls overlays when toggled back on"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     ;; off
     (gptel-chat-toggle-display-layer)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0)
     ;; on
     (gptel-chat-toggle-display-layer)
     (expect gptel-chat--display-enabled :to-be-truthy)
     (expect (length (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-user-face))
             :to-equal 1)
     (expect (length (gptel-chat-display-test--overlays-with-face
                      'gptel-chat-assistant-face))
             :to-equal 1))))


(describe "gptel-chat--refresh-overlays: idempotence and robustness"

  (it "is idempotent — repeated calls do not stack overlays"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (gptel-chat--refresh-overlays)
     (gptel-chat--refresh-overlays)
     (gptel-chat--refresh-overlays)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 2)))

  (it "treats a mid-edit unclosed block as a no-op (no overlays, no error)"
    ;; A buffer containing only `#+begin_user' (no close) is the kind of
    ;; state the debounced refresh can land on between keystrokes.  The
    ;; refresher must swallow the parser's `user-error' so typing is not
    ;; interrupted.
    (gptel-chat-display-test--with-chat-buffer
     "#+begin_user\nhalf-typed\n"
     (expect (gptel-chat--refresh-overlays) :not :to-throw)
     (expect (length (gptel-chat-display-test--display-overlays))
             :to-equal 0))))



;;; After-change scheduling + streaming guard -----------------------------

(describe "gptel-chat-display: after-change debounced scheduling"

  (it "schedules exactly one refresh after N rapid inserts (debounce collapses)"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     ;; Clear any pending timer from activation.
     (when (timerp gptel-chat--display-refresh-timer)
       (cancel-timer gptel-chat--display-refresh-timer)
       (setq gptel-chat--display-refresh-timer nil))
     (spy-on 'gptel-chat--refresh-overlays :and-call-through)
     ;; Three rapid inserts in the user body.
     (save-excursion
       (goto-char (point-min))
       (forward-line 1) ;; inside user body
       (insert "a")
       (insert "b")
       (insert "c"))
     ;; The scheduler installs (or re-installs) a single pending timer
     ;; for all three inserts — earlier timers are cancelled in favour
     ;; of the latest.  No refresh has fired yet because the idle
     ;; delay has not elapsed.
     (let ((pending gptel-chat--display-refresh-timer))
       (expect (timerp pending) :to-be-truthy)
       (expect 'gptel-chat--refresh-overlays :not :to-have-been-called)
       ;; Idle timers do not advance under batch-mode `sit-for'; invoke
       ;; the timer's own function directly to simulate its firing.
       ;; That matches what the Emacs timer loop would do.
       (timer-event-handler pending))
     ;; Exactly one refresh call from the debounced path.
     (expect 'gptel-chat--refresh-overlays :to-have-been-called)
     (expect (spy-calls-count 'gptel-chat--refresh-overlays)
             :to-equal 1)
     ;; After the timer fires it clears its own handle.
     (expect gptel-chat--display-refresh-timer :to-equal nil)))

  (it "does NOT schedule refresh while lifecycle-state is `streaming'"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     ;; Clear any pending timer from activation.
     (when (timerp gptel-chat--display-refresh-timer)
       (cancel-timer gptel-chat--display-refresh-timer)
       (setq gptel-chat--display-refresh-timer nil))
     (spy-on 'gptel-chat--refresh-overlays)
     (let ((gptel-chat--lifecycle-state 'streaming))
       (gptel-chat--display-schedule-refresh))
     ;; No timer scheduled, no refresh fired.
     (expect gptel-chat--display-refresh-timer :to-equal nil)
     (expect 'gptel-chat--refresh-overlays :not :to-have-been-called)))

  (it "does NOT schedule refresh while lifecycle-state is `tool-running'"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (when (timerp gptel-chat--display-refresh-timer)
       (cancel-timer gptel-chat--display-refresh-timer)
       (setq gptel-chat--display-refresh-timer nil))
     (spy-on 'gptel-chat--refresh-overlays)
     (let ((gptel-chat--lifecycle-state 'tool-running))
       (gptel-chat--display-schedule-refresh))
     (expect gptel-chat--display-refresh-timer :to-equal nil)
     (expect 'gptel-chat--refresh-overlays :not :to-have-been-called)))

  (it "resumes scheduling once lifecycle-state clears (nil)"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (when (timerp gptel-chat--display-refresh-timer)
       (cancel-timer gptel-chat--display-refresh-timer)
       (setq gptel-chat--display-refresh-timer nil))
     (let ((gptel-chat--lifecycle-state nil))
       (gptel-chat--display-schedule-refresh))
     ;; With lifecycle-state back to idle, the scheduler installs a
     ;; pending timer as normal.
     (expect (timerp gptel-chat--display-refresh-timer) :to-be-truthy)
     (cancel-timer gptel-chat--display-refresh-timer)
     (setq gptel-chat--display-refresh-timer nil))))


(describe "gptel-chat-display: DONE-handler refresh"

  (it "fires exactly one refresh on `gptel-chat--on-done' entry"
    (gptel-chat-display-test--with-chat-buffer
     gptel-chat-display-test--two-turns
     (let ((target-buf (current-buffer)))
       ;; Build an FSM whose info points at this buffer so the advice
       ;; can locate us.
       (let ((fsm (gptel-make-fsm
                   :handlers gptel-chat--fsm-handlers
                   :info     (list :buffer target-buf))))
         (spy-on 'gptel-chat--refresh-overlays :and-call-through)
         ;; Simulate the upstream transition into DONE; the chained
         ;; `gptel-chat--on-done' runs our advice which calls
         ;; `gptel-chat--refresh-overlays' once.  Stub upstream's
         ;; `gptel--handle-post' so no :post list iteration interferes.
         (spy-on 'gptel--handle-post)
         (gptel--fsm-transition fsm 'DONE)
         (expect 'gptel-chat--refresh-overlays :to-have-been-called)
         (expect (spy-calls-count 'gptel-chat--refresh-overlays)
                 :to-equal 1)))))

  (it "is a no-op when the FSM's buffer was killed before DONE"
    ;; The helper guards on `buffer-live-p'; a dead buffer must not
    ;; signal (e.g. `wrong-type-argument' from `with-current-buffer').
    (let* ((buf (generate-new-buffer " *gptel-chat-display-test-dead*"))
           (fsm (gptel-make-fsm
                 :handlers gptel-chat--fsm-handlers
                 :info     (list :buffer buf))))
      (kill-buffer buf)
      (expect (gptel-chat--display-refresh-on-done fsm)
              :not :to-throw))))


(describe "gptel-chat-display: kill-buffer timer cleanup"

  (it "cancels the pending idle timer when the buffer is killed"
    (let* ((buf (generate-new-buffer " *gptel-chat-display-test-kill*"))
           pending-timer)
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert gptel-chat-display-test--two-turns)
              (gptel-chat-mode)
              ;; Schedule a refresh by inserting something; the timer
              ;; sits in `timer-list' until either it fires or we
              ;; cancel it.
              (goto-char (point-min))
              (forward-line 1)
              (insert "x")
              (setq pending-timer gptel-chat--display-refresh-timer)
              (expect (timerp pending-timer) :to-be-truthy)
              (expect (memq pending-timer timer-idle-list) :to-be-truthy))
            ;; Kill the buffer; `kill-buffer-hook' must cancel the
            ;; pending timer so it no longer sits in `timer-idle-list'.
            (kill-buffer buf)
            (expect (memq pending-timer timer-idle-list) :to-equal nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        ;; Defensive: if the hook was somehow skipped, clean up.
        (when (and (timerp pending-timer)
                   (memq pending-timer timer-idle-list))
          (cancel-timer pending-timer))))))


;;; display-layer-spec.el ends here
(provide 'gptel-chat-display-layer-spec)
