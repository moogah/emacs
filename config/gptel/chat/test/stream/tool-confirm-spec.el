;;; tool-confirm-spec.el --- Buttercup tests for chat-mode :confirm tool UI -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises the chat-mode tool-call confirmation UI introduced by
;; `config/gptel/chat/tool-confirm.org': the overlay creator
;; `gptel-chat--display-tool-confirm', and the accept/reject handlers
;; that operate on it.  Covers the unit-level contract (overlay
;; properties, continuation dispatch, overlay teardown) AND the
;; integration with `gptel-chat-stream-callback' so that a synthetic
;; `(tool-call . CALLS)' event surfaces the overlay end-to-end.
;;
;; The bug this UI fixes is described in
;; `.tasks/chat-mode-tool-confirm-ui-missing.md' (predecessor in
;; `.tasks/`): without the overlay, a `:confirm t' tool registered
;; through upstream gptel hangs the chat-mode FSM forever because
;; the stream callback ignores the third element of each pending
;; call (the `process-tool-result' continuation) and offers no UI
;; for the user to invoke it.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
;; `gptel-make-tool' / `gptel-tool-name' / `gptel-tool-function' /
;; `gptel-tool-async' — needed for the spec to build synthetic tool
;; specs and for the accept handler to dispatch them.
(require 'gptel)

;; Load the modules under test from the co-located source directory.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-tool-confirm)
(require 'gptel-chat-stream)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-tool-confirm-test--buffer nil
  "Scratch buffer for tool-confirm tests.")

(defvar gptel-chat-tool-confirm-test--marker nil
  "Live advance insertion marker for the active assistant block.")

(defun gptel-chat-tool-confirm-test--fresh-buffer ()
  "Create a fresh scratch buffer with an empty assistant body.
Returns an advance marker (insertion-type t) positioned at the start
of the assistant body — where the stream callback would route its
inserts."
  (setq gptel-chat-tool-confirm-test--buffer
        (generate-new-buffer " *gptel-chat-tool-confirm-test*"))
  (with-current-buffer gptel-chat-tool-confirm-test--buffer
    (insert "#+begin_user\nhello\n#+end_user\n#+begin_assistant\n")
    (setq gptel-chat-tool-confirm-test--marker
          (copy-marker (point-max) t)))
  gptel-chat-tool-confirm-test--marker)

(defun gptel-chat-tool-confirm-test--buffer-string ()
  "Return the current contents of the scratch test buffer."
  (with-current-buffer gptel-chat-tool-confirm-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-chat-tool-confirm-test--cleanup ()
  (when (buffer-live-p gptel-chat-tool-confirm-test--buffer)
    (kill-buffer gptel-chat-tool-confirm-test--buffer))
  (setq gptel-chat-tool-confirm-test--buffer nil
        gptel-chat-tool-confirm-test--marker nil))

(defun gptel-chat-tool-confirm-test--sync-tool (name result-fn)
  "Build a sync `gptel-tool' struct named NAME whose function returns RESULT-FN.
RESULT-FN is invoked with the model-supplied arg values (apply'd) at
accept time; whatever it returns becomes the tool result string."
  (gptel-make-tool
   :name name
   :function result-fn
   :description (format "test tool %s" name)
   :args nil
   :category "test"
   :confirm t))

(defun gptel-chat-tool-confirm-test--async-tool (name)
  "Build an async `gptel-tool' struct named NAME whose function records its callback.
The function captures the continuation upstream supplies as its
FIRST argument; the test inspects that capture via the
`callback-cell' shared closure cell."
  (gptel-make-tool
   :name name
   :function (lambda (cb &rest _args)
               ;; Stash the continuation so the test can verify it
               ;; was passed in unmodified.
               (setq gptel-chat-tool-confirm-test--async-cb cb))
   :description (format "test async tool %s" name)
   :args nil
   :async t
   :category "test"
   :confirm t))

(defvar gptel-chat-tool-confirm-test--async-cb nil
  "Captured continuation from the most-recent async tool dispatch.")

(defun gptel-chat-tool-confirm-test--find-confirm-overlay ()
  "Return the (single) chat-mode tool-confirm overlay in the test buffer, or nil."
  (with-current-buffer gptel-chat-tool-confirm-test--buffer
    (cl-find-if (lambda (ov)
                  (overlay-get ov 'gptel-chat-tool-confirm))
                (overlays-in (point-min) (point-max)))))


;;; gptel-chat--display-tool-confirm — overlay properties --------------------

(describe "gptel-chat--display-tool-confirm"

  (before-each (gptel-chat-tool-confirm-test--fresh-buffer))
  (after-each  (gptel-chat-tool-confirm-test--cleanup))

  (it "creates an overlay carrying the CALLS list on the `gptel-tool' property"
    (let* ((tool (gptel-chat-tool-confirm-test--sync-tool
                  "read_file" (lambda (&rest _) "")))
           (cont (lambda (_) nil))
           (calls `((,tool (:path "/x") ,cont)))
           (start (marker-position
                   gptel-chat-tool-confirm-test--marker))
           (ov (gptel-chat--display-tool-confirm
                calls start
                gptel-chat-tool-confirm-test--marker)))
      (expect (overlayp ov) :to-be-truthy)
      (expect (overlay-buffer ov)
              :to-be gptel-chat-tool-confirm-test--buffer)
      (expect (overlay-get ov 'gptel-tool) :to-equal calls)
      (expect (overlay-get ov 'gptel-chat-tool-confirm) :to-be t)
      (expect (overlay-get ov 'keymap)
              :to-be gptel-chat-tool-call-actions-map)
      (expect (overlay-get ov 'before-string) :not :to-be nil)))

  (it "leaves buffer-substring-no-properties unchanged"
    ;; `before-string' overlays surface as displayed text but are not
    ;; part of the underlying buffer string — preserving the on-disk
    ;; roundtrip invariant the chat-mode buffer format relies on.
    (let* ((tool (gptel-chat-tool-confirm-test--sync-tool
                  "noop" (lambda (&rest _) "")))
           (cont (lambda (_) nil))
           (start (marker-position
                   gptel-chat-tool-confirm-test--marker))
           (before (gptel-chat-tool-confirm-test--buffer-string)))
      (gptel-chat--display-tool-confirm
       `((,tool nil ,cont))
       start gptel-chat-tool-confirm-test--marker)
      (expect (gptel-chat-tool-confirm-test--buffer-string)
              :to-equal before))))


;;; gptel-chat--accept-tool-calls — sync + async dispatch ------------------

(describe "gptel-chat--accept-tool-calls"

  (before-each
    (gptel-chat-tool-confirm-test--fresh-buffer)
    (setq gptel-chat-tool-confirm-test--async-cb nil))
  (after-each (gptel-chat-tool-confirm-test--cleanup))

  (it "invokes each sync continuation with the tool's return value"
    (let* (received-results
           (tool-a (gptel-chat-tool-confirm-test--sync-tool
                    "a" (lambda (&rest _) "result-a")))
           (tool-b (gptel-chat-tool-confirm-test--sync-tool
                    "b" (lambda (&rest _) "result-b")))
           (cb-a   (lambda (r) (push (cons "a" r) received-results)))
           (cb-b   (lambda (r) (push (cons "b" r) received-results)))
           (calls  `((,tool-a (:x 1) ,cb-a)
                     (,tool-b (:y 2) ,cb-b)))
           (ov (gptel-chat--display-tool-confirm
                calls
                (marker-position gptel-chat-tool-confirm-test--marker)
                gptel-chat-tool-confirm-test--marker)))
      (gptel-chat--accept-tool-calls calls ov)
      ;; Both continuations fired with their tool's return string.
      (expect (sort received-results
                    (lambda (l r) (string< (car l) (car r))))
              :to-equal '(("a" . "result-a") ("b" . "result-b")))
      ;; Overlay torn down.
      (expect (overlay-buffer ov) :to-be nil)))

  (it "converts a synchronous tool error into a stringified result"
    ;; `gptel--accept-tool-calls' wraps tool execution in
    ;; `condition-case' to keep the loop alive when one tool errors;
    ;; we mirror that contract.  The continuation must still fire
    ;; with SOME string so the FSM advances.
    (let* (received
           (tool (gptel-chat-tool-confirm-test--sync-tool
                  "boom" (lambda (&rest _) (error "kaboom"))))
           (cb   (lambda (r) (setq received r)))
           (calls `((,tool nil ,cb)))
           (ov (gptel-chat--display-tool-confirm
                calls
                (marker-position gptel-chat-tool-confirm-test--marker)
                gptel-chat-tool-confirm-test--marker)))
      (gptel-chat--accept-tool-calls calls ov)
      (expect (stringp received) :to-be-truthy)
      (expect received :to-match "kaboom")
      (expect (overlay-buffer ov) :to-be nil)))

  (it "passes the continuation as the first argument to an async tool"
    ;; Async tools call their continuation themselves (eventually);
    ;; the accept handler must NOT funcall it directly — it just
    ;; invokes the tool function with the continuation prepended.
    (let* ((tool (gptel-chat-tool-confirm-test--async-tool "downloader"))
           (cb   (lambda (_r) nil))
           (calls `((,tool (:url "http://x") ,cb)))
           (ov (gptel-chat--display-tool-confirm
                calls
                (marker-position gptel-chat-tool-confirm-test--marker)
                gptel-chat-tool-confirm-test--marker)))
      (gptel-chat--accept-tool-calls calls ov)
      ;; Async fixture captured the SAME function we passed in.
      (expect gptel-chat-tool-confirm-test--async-cb :to-be cb)
      (expect (overlay-buffer ov) :to-be nil)))

  (it "signals when invoked programmatically with no pending calls"
    (expect (gptel-chat--accept-tool-calls nil nil) :to-throw)))


;;; gptel-chat--reject-tool-calls — deny-string dispatch -------------------

(describe "gptel-chat--reject-tool-calls"

  (before-each (gptel-chat-tool-confirm-test--fresh-buffer))
  (after-each  (gptel-chat-tool-confirm-test--cleanup))

  (it "funcalls each continuation with `gptel-chat-tool-reject-message'"
    ;; The deny string is what makes chat-mode's reject diverge from
    ;; upstream's: feeding it back through the continuation advances
    ;; the FSM (vs. upstream's abandon-and-resume-via-gptel-menu).
    (let* (received-results
           (tool-a (gptel-chat-tool-confirm-test--sync-tool
                    "a" (lambda (&rest _) "should-not-run")))
           (tool-b (gptel-chat-tool-confirm-test--sync-tool
                    "b" (lambda (&rest _) "should-not-run")))
           (cb-a   (lambda (r) (push (cons "a" r) received-results)))
           (cb-b   (lambda (r) (push (cons "b" r) received-results)))
           (calls  `((,tool-a nil ,cb-a) (,tool-b nil ,cb-b)))
           (ov (gptel-chat--display-tool-confirm
                calls
                (marker-position gptel-chat-tool-confirm-test--marker)
                gptel-chat-tool-confirm-test--marker)))
      (gptel-chat--reject-tool-calls calls ov)
      (expect (sort received-results
                    (lambda (l r) (string< (car l) (car r))))
              :to-equal `(("a" . ,gptel-chat-tool-reject-message)
                          ("b" . ,gptel-chat-tool-reject-message)))
      (expect (overlay-buffer ov) :to-be nil)))

  (it "signals when invoked programmatically with no pending calls"
    (expect (gptel-chat--reject-tool-calls nil nil) :to-throw)))


;;; Stream-callback integration --------------------------------------------

(describe "gptel-chat-stream-callback tool-call → confirm overlay"

  (before-each (gptel-chat-tool-confirm-test--fresh-buffer))
  (after-each  (gptel-chat-tool-confirm-test--cleanup))

  (it "places a confirmation overlay on a (tool-call . CALLS) event"
    ;; End-to-end wiring: feeding a synthetic upstream `tool-call'
    ;; event into the stream callback must (a) render the empty
    ;; `#+begin_tool' block(s) — unchanged from prior behavior —
    ;; AND (b) drop a `gptel-chat-tool-confirm' overlay carrying
    ;; the pending CALLS so the user can act on them.
    (let* ((cb (gptel-chat-stream-callback
                gptel-chat-tool-confirm-test--marker))
           (tool (gptel-chat-tool-confirm-test--sync-tool
                  "read_file" (lambda (&rest _) "")))
           (cont (lambda (_) nil))
           (calls `((,tool (:path "/x") ,cont))))
      (funcall cb `(tool-call . ,calls) nil)
      ;; The empty block was rendered (existing contract).
      (expect (gptel-chat-tool-confirm-test--buffer-string)
              :to-match "#\\+begin_tool (read_file :path \"/x\")")
      ;; A confirm overlay exists carrying the calls.
      (let ((ov (gptel-chat-tool-confirm-test--find-confirm-overlay)))
        (expect (overlayp ov) :to-be-truthy)
        (expect (overlay-get ov 'gptel-tool) :to-equal calls))))

  (it "the overlay's stored CALLS round-trip through accept → tool-result fill"
    ;; The end-to-end happy path: accept the overlay → the sync
    ;; tool's function fires → its return value is fed to the
    ;; continuation → upstream would emit `(tool-result . ...)' →
    ;; we simulate that emission directly here (the synthetic
    ;; continuation is the test's own closure, not upstream's real
    ;; one) and assert the empty block fills with the result.
    (let* ((cb (gptel-chat-stream-callback
                gptel-chat-tool-confirm-test--marker))
           (tool (gptel-chat-tool-confirm-test--sync-tool
                  "read_file" (lambda (&rest _) "FILE-CONTENTS")))
           ;; Synthetic continuation: drive the next stream-callback
           ;; event when invoked, mirroring what upstream's
           ;; PROCESS-TOOL-RESULT does after a tool returns.
           (cont (lambda (result)
                   (funcall cb `(tool-result . ((,tool (:path "/x")
                                                       ,result))) nil)))
           (calls `((,tool (:path "/x") ,cont))))
      (funcall cb `(tool-call . ,calls) nil)
      (let ((ov (gptel-chat-tool-confirm-test--find-confirm-overlay)))
        (with-current-buffer gptel-chat-tool-confirm-test--buffer
          (gptel-chat--accept-tool-calls calls ov)))
      ;; Block filled with the tool's return string.
      (expect (gptel-chat-tool-confirm-test--buffer-string)
              :to-match "FILE-CONTENTS")
      ;; Overlay torn down.
      (expect (gptel-chat-tool-confirm-test--find-confirm-overlay)
              :to-be nil)))

  (it "the overlay's stored CALLS round-trip through reject → deny-string fill"
    ;; Reject mirror: invoking the reject handler funcalls the
    ;; continuation with the deny string, simulating the FSM
    ;; advancing with the rejection as the tool's result.
    (let* ((cb (gptel-chat-stream-callback
                gptel-chat-tool-confirm-test--marker))
           (tool (gptel-chat-tool-confirm-test--sync-tool
                  "read_file" (lambda (&rest _) "should-not-run")))
           (cont (lambda (result)
                   (funcall cb `(tool-result . ((,tool (:path "/x")
                                                       ,result))) nil)))
           (calls `((,tool (:path "/x") ,cont))))
      (funcall cb `(tool-call . ,calls) nil)
      (let ((ov (gptel-chat-tool-confirm-test--find-confirm-overlay)))
        (with-current-buffer gptel-chat-tool-confirm-test--buffer
          (gptel-chat--reject-tool-calls calls ov)))
      ;; The deny string filled the block.
      (expect (gptel-chat-tool-confirm-test--buffer-string)
              :to-match (regexp-quote gptel-chat-tool-reject-message))
      ;; Overlay torn down.
      (expect (gptel-chat-tool-confirm-test--find-confirm-overlay)
              :to-be nil))))


(provide 'tool-confirm-spec)

;;; tool-confirm-spec.el ends here
