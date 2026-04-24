;;; send-command-spec.el --- Buttercup tests for `gptel-chat-send' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the interactive command `gptel-chat-send'
;; delivered by task `send-command' in the `gptel-chat-mode' OpenSpec
;; change.
;;
;; The command is the `C-c C-c' entry point for chat-mode.  Its job
;; is narrow (design.md §Decision 11, architecture.md
;; §`gptel-chat-send'):
;;
;;   1. Reject a new send if a prior request is still in flight (the
;;      buffer-local `gptel--fsm-last' is in WAIT / TYPE / TOOL).
;;   2. Reject a send from inside an `#+begin_assistant' block.
;;   3. Treat an empty / whitespace-only user block as a no-op (a
;;      user-visible `message', no `user-error').
;;   4. On success: parse the buffer, build the `:prompt' message
;;      list, open a fresh `#+begin_assistant' block after the
;;      user's `#+end_user', and invoke `gptel-request' with
;;      `:prompt' / `:stream' / `:callback' / `:fsm'.
;;
;; Scope and mocking: the real `gptel-request' is stubbed with
;; `spy-on' so these tests do not touch the network.  The tests
;; assert on the KEYWORD ARGUMENTS the command passes to
;; `gptel-request', not on what upstream does with them — upstream's
;; behaviour is covered by gptel's own test suite.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/send/;
;; two levels up is .../config/gptel/chat/, which holds `send.el' and
;; its siblings.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel)
(require 'gptel-chat-parser)
(require 'gptel-chat-nav)
(require 'gptel-chat-stream)
(require 'gptel-chat-send)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-send-cmd-test--buffer nil
  "Scratch buffer for `gptel-chat-send' specs.")

(defun gptel-chat-send-cmd-test--setup (content &optional point-finder)
  "Install CONTENT into a fresh buffer; leave point per POINT-FINDER.
POINT-FINDER defaults to placing point at `point-min'.  It is
invoked inside the buffer and is free to call `re-search-forward',
`goto-char', etc."
  (setq gptel-chat-send-cmd-test--buffer
        (generate-new-buffer " *gptel-chat-send-cmd-test*"))
  (with-current-buffer gptel-chat-send-cmd-test--buffer
    (insert content)
    (goto-char (point-min))
    (when point-finder (funcall point-finder)))
  gptel-chat-send-cmd-test--buffer)

(defun gptel-chat-send-cmd-test--cleanup ()
  (when (buffer-live-p gptel-chat-send-cmd-test--buffer)
    (kill-buffer gptel-chat-send-cmd-test--buffer))
  (setq gptel-chat-send-cmd-test--buffer nil))

(defun gptel-chat-send-cmd-test--buffer-string ()
  (with-current-buffer gptel-chat-send-cmd-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;;; Spec ---------------------------------------------------------------------

(describe "gptel-chat-send (task send-command)"

  (before-each
    (setq gptel-chat-send-cmd-test--buffer nil))

  (after-each
    (gptel-chat-send-cmd-test--cleanup))


  (describe "precondition: in-flight guard"
    ;; Covered in detail by the parametrized specs in
    ;; `backend-invocation-spec.el' ("send-guard idle-state
    ;; contract").  Here we pin the single happy-path case —
    ;; `gptel--fsm-last' nil — for locality.

    (it "does not consult gptel-request when in-flight"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\nhi\n#+end_user\n"
       (lambda () (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (let ((fsm (gptel-make-fsm :info (list :buffer (current-buffer)))))
          (setf (gptel-fsm-state fsm) 'WAIT)
          (setq gptel--fsm-last fsm))
        (spy-on 'gptel-request)
        (expect (gptel-chat-send) :to-throw 'user-error)
        (expect 'gptel-request :not :to-have-been-called))))


  (describe "precondition: point location"

    (it "signals user-error when point is inside an assistant block"
      (gptel-chat-send-cmd-test--setup
       (concat "#+begin_user\nhi\n#+end_user\n"
               "#+begin_assistant\nsome reply\n#+end_assistant\n")
       (lambda ()
         (re-search-forward "^some reply$")
         (beginning-of-line)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request)
        (expect (gptel-chat-send) :to-throw 'user-error)
        (expect 'gptel-request :not :to-have-been-called)))

    (it "prints a message and returns nil when the current user block is empty"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\n\n#+end_user\n"
       (lambda ()
         ;; Land inside the empty user body (the blank line).
         (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request)
        (spy-on 'message)
        (expect (gptel-chat-send) :to-equal nil)
        (expect 'message :to-have-been-called)
        (expect 'gptel-request :not :to-have-been-called)))

    (it "prints a message when no user block exists"
      (gptel-chat-send-cmd-test--setup
       "* Just a heading, no turns\n")
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request)
        (spy-on 'message)
        (expect (gptel-chat-send) :to-equal nil)
        (expect 'message :to-have-been-called)
        (expect 'gptel-request :not :to-have-been-called)))

    (it "falls back to the last user block when point is between blocks"
      ;; Classic append-flow layout: streaming just closed an
      ;; assistant block and appended a fresh user block; point sits
      ;; on the body line of the new user block (which is populated
      ;; by the user typing in it).
      (gptel-chat-send-cmd-test--setup
       (concat "#+begin_user\nfirst\n#+end_user\n"
               "#+begin_assistant\nfirst reply\n#+end_assistant\n"
               "#+begin_user\nsecond\n#+end_user\n")
       (lambda ()
         ;; Park point OUTSIDE any block — after the final
         ;; `#+end_user'.  The resolver must then fall back to the
         ;; last user block in the buffer ("second") and send it.
         (goto-char (point-max))))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (expect (gptel-chat-send) :not :to-throw)
        (expect 'gptel-request :to-have-been-called))))


  (describe "gptel-request invocation"

    (it "calls gptel-request once on a valid single-turn buffer"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\nWhat's the capital of France?\n#+end_user\n"
       (lambda () (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (gptel-chat-send)
        (expect 'gptel-request :to-have-been-called-times 1)))

    (it "passes :stream t and a live callback closure"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\nhi\n#+end_user\n"
       (lambda () (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (gptel-chat-send)
        (let* ((args (spy-calls-args-for 'gptel-request 0))
               (keys (cdr args)))
          ;; First positional arg is the prompt message list.
          (expect (car args) :to-be-truthy)
          ;; :stream t
          (expect (plist-get keys :stream) :to-be t)
          ;; :callback is a function (closure from
          ;; `gptel-chat--stream-callback').
          (expect (functionp (plist-get keys :callback)) :to-be-truthy)
          ;; :buffer is NOT passed — upstream defaults to
          ;; current-buffer, which is what chat-mode wants
          ;; (design Decision 11 / send.org invocation).
          (expect (plist-member keys :buffer) :to-be nil))))

    (it "passes :fsm built with gptel-chat--fsm-handlers"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\nhi\n#+end_user\n"
       (lambda () (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (gptel-chat-send)
        (let* ((args (spy-calls-args-for 'gptel-request 0))
               (fsm  (plist-get (cdr args) :fsm)))
          (expect (gptel-fsm-p fsm) :to-be-truthy)
          (expect (gptel-fsm-handlers fsm)
                  :to-equal gptel-chat--fsm-handlers))))

    (it "passes a :prompt derived from gptel-chat--turns-to-messages"
      (gptel-chat-send-cmd-test--setup
       (concat "#+begin_user\nhello\n#+end_user\n"
               "#+begin_assistant\nhi back\n#+end_assistant\n"
               "#+begin_user\ngoodbye\n#+end_user\n")
       (lambda ()
         ;; Place point inside the last user block.
         (re-search-forward "^goodbye$")
         (beginning-of-line)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (gptel-chat-send)
        (let* ((args   (spy-calls-args-for 'gptel-request 0))
               (prompt (car args)))
          ;; The converter yields one cons per turn; we expect the
          ;; three-turn conversation to round-trip into three conses
          ;; in document order.  Exact text content is exercised by
          ;; `parser' tests; here we pin shape and ordering.
          (expect (length prompt) :to-equal 3)
          (expect (car (nth 0 prompt)) :to-equal 'prompt)
          (expect (car (nth 1 prompt)) :to-equal 'response)
          (expect (car (nth 2 prompt)) :to-equal 'prompt))))

    (it "opens a #+begin_assistant block after the user's #+end_user"
      (gptel-chat-send-cmd-test--setup
       "#+begin_user\nhi\n#+end_user\n"
       (lambda () (forward-line 1)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request :and-return-value nil)
        (gptel-chat-send))
      ;; The assistant block header must be present and follow the
      ;; closing user delimiter in document order.  The block is NOT
      ;; pre-closed here — the stream callback / close helpers append
      ;; `#+end_assistant' on completion / error / abort.
      (let ((content (gptel-chat-send-cmd-test--buffer-string)))
        (expect (string-match-p
                 "#\\+end_user\n+#\\+begin_assistant\n"
                 content)
                :to-be-truthy))))


  (describe "error propagation"

    (it "lets parse errors from gptel-chat--parse-buffer surface"
      ;; An unclosed `#+begin_tool' inside an assistant block is one
      ;; of the parser's `user-error' shapes.  `gptel-chat-send'
      ;; should not swallow it.
      (gptel-chat-send-cmd-test--setup
       (concat "#+begin_user\nhi\n#+end_user\n"
               "#+begin_assistant\n#+begin_tool (run :arg 1)\n"
               ;; Missing `#+end_tool' — unclosed.
               "#+end_assistant\n"
               "#+begin_user\nagain\n#+end_user\n")
       (lambda ()
         (re-search-forward "^again$")
         (beginning-of-line)))
      (with-current-buffer gptel-chat-send-cmd-test--buffer
        (spy-on 'gptel-request)
        (expect (gptel-chat-send) :to-throw 'user-error)
        (expect 'gptel-request :not :to-have-been-called)))))


(provide 'send-command-spec)

;;; send-command-spec.el ends here
