;;; regenerate-in-flight-spec.el --- In-flight guard and atomicity for gptel-chat-regenerate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs covering the in-flight guard and atomic-change-group
;; wrapping added to `gptel-chat-regenerate' by task
;; `nav-regenerate-in-flight-guard'.
;;
;; Coverage:
;;   - With `gptel-chat--lifecycle-state' bound to `streaming' (and to
;;     `waiting' and `tool-running'), calling `gptel-chat-regenerate'
;;     signals `user-error', does not delete any buffer text, and does
;;     not call `gptel-chat-send'.
;;   - With `gptel-chat--lifecycle-state' nil (steady state) the
;;     command proceeds: it deletes the trailing assistant block and
;;     invokes `gptel-chat-send' (spied on).
;;   - Terminal-but-non-nil states `error' and `aborted' are treated as
;;     idle — the command proceeds so the user can regenerate after a
;;     failed or aborted request (mirrors the send-guard's idle-state
;;     contract in send.org).
;;   - When `gptel-chat-send' signals an error mid-regenerate, a single
;;     `undo' (i.e. `C-/') restores the pre-regenerate buffer thanks to
;;     `atomic-change-group'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-parser)
(require 'gptel-chat-nav)

;; `gptel-chat-send' is owned by a sibling task; provide a batch-safe
;; default so `spy-on' has a symbol to replace.
(unless (fboundp 'gptel-chat-send)
  (defun gptel-chat-send ()
    "Stub for batch-context testing; replaced by `spy-on' in each spec."
    nil))

;; The guard reads `gptel-chat--lifecycle-state'.  It is defined in
;; send.el as a buffer-local defvar, but this spec does not load
;; send.el (its dependency graph — gptel, fsm-handlers — is heavier
;; than the guard under test needs).  Declare a top-level default so
;; buffer-local bindings behave deterministically in batch.
(defvar-local gptel-chat--lifecycle-state nil
  "Mirror of send.el's lifecycle indicator, for batch tests.")


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-regen-inflight-test--completed
  (concat "#+begin_user\n"
          "What's the capital of France?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Paris.\n"
          "#+end_assistant\n")
  "Buffer ending in a completed assistant response.")

(defmacro gptel-chat-regen-inflight-test--with-buffer (content &rest body)
  "Insert CONTENT into a fresh temp buffer and evaluate BODY there.

Uses `with-temp-buffer' with `buffer-enable-undo' so
`atomic-change-group' / `undo' behave as in a live buffer."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (buffer-enable-undo)
     (insert ,content)
     (goto-char (point-min))
     ,@body))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat-regenerate in-flight guard"

  (describe "when a request is in flight"
    (dolist (state '(waiting streaming tool-running))
      (it (format "signals user-error and does not mutate buffer or send (state=%s)" state)
        (spy-on 'gptel-chat-send :and-return-value nil)
        (gptel-chat-regen-inflight-test--with-buffer
            gptel-chat-regen-inflight-test--completed
          (setq gptel-chat--lifecycle-state state)
          (let ((before (buffer-string)))
            (expect (gptel-chat-regenerate) :to-throw 'user-error)
            ;; Buffer must be untouched (assistant block intact).
            (expect (buffer-string) :to-equal before)
            (expect (buffer-string) :to-match "#\\+begin_assistant")
            (expect (buffer-string) :to-match "Paris\\.")
            ;; Send must NOT have been called.
            (expect 'gptel-chat-send :not :to-have-been-called))))))

  (describe "when the buffer is idle (lifecycle-state nil)"
    (it "proceeds to delete the assistant block and call gptel-chat-send"
      (spy-on 'gptel-chat-send :and-return-value nil)
      (gptel-chat-regen-inflight-test--with-buffer
          gptel-chat-regen-inflight-test--completed
        (setq gptel-chat--lifecycle-state nil)
        (gptel-chat-regenerate)
        (expect (buffer-string) :not :to-match "#\\+begin_assistant")
        (expect (buffer-string) :not :to-match "Paris\\.")
        (expect 'gptel-chat-send :to-have-been-called))))

  (describe "when the previous request terminated non-successfully"
    (dolist (state '(error aborted))
      (it (format "treats %s as idle and proceeds" state)
        (spy-on 'gptel-chat-send :and-return-value nil)
        (gptel-chat-regen-inflight-test--with-buffer
            gptel-chat-regen-inflight-test--completed
          (setq gptel-chat--lifecycle-state state)
          (gptel-chat-regenerate)
          (expect (buffer-string) :not :to-match "#\\+begin_assistant")
          (expect 'gptel-chat-send :to-have-been-called))))))


(describe "gptel-chat-regenerate atomic-change-group"

  (it "undoes the delete when gptel-chat-send signals an error"
    ;; Force `gptel-chat-send' to error AFTER the assistant block is
    ;; deleted.  `atomic-change-group' should roll the buffer back as
    ;; the error unwinds through it; a single `undo' (the user's
    ;; `C-/') then restores the pre-regenerate state.
    ;;
    ;; Note: `atomic-change-group' cancels its own change group when
    ;; the body errors non-locally, which rolls the change back
    ;; directly (no user undo needed).  Either way the observable
    ;; contract is: after the error, the assistant block is restored.
    (spy-on 'gptel-chat-send
            :and-call-fake
            (lambda () (error "simulated send failure")))
    (gptel-chat-regen-inflight-test--with-buffer
        gptel-chat-regen-inflight-test--completed
      (setq gptel-chat--lifecycle-state nil)
      (let ((before (buffer-string)))
        (expect (gptel-chat-regenerate) :to-throw 'error)
        ;; After the error unwinds, the buffer should be back to its
        ;; pre-regenerate state (atomic-change-group cancellation on
        ;; non-local exit).
        (expect (buffer-string) :to-equal before)
        (expect (buffer-string) :to-match "#\\+begin_assistant")
        (expect (buffer-string) :to-match "Paris\\.")))))


(provide 'gptel-chat-regenerate-in-flight-spec)

;;; regenerate-in-flight-spec.el ends here
