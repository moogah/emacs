;;; send-and-completion-spec.el --- Persistent-agent send + completion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the persistent-agent send/completion pipeline.
;;
;; Pins four pieces of the contract introduced by the
;; `persistent-agent-rebuild' OpenSpec change:
;;
;;   1. `gptel-request' invocation shape — messages list, stream
;;      callback, parent-overlay context, composed FSM struct.
;;
;;   2. FSM handler composition — agent overlay/terminal handlers run
;;      ahead of chat-mode lifecycle handlers, which run ahead of
;;      upstream's transition-driving handlers.  See
;;      openspec/changes/persistent-agent-rebuild/architecture.md
;;      §Interfaces "FSM handler composition".
;;
;;   3. Final-text extraction — the trailing text segment of the last
;;      assistant turn, with the documented empty-string fallback
;;      (design.md §Decisions 2).
;;
;;   4. Stream-callback wiring — the `:callback' value handed to
;;      `gptel-request' is the closure produced by
;;      `gptel-chat-stream-callback' (no custom callback in this
;;      module).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Sibling helpers file lives in the same directory.  When the runner
;; discovers and loads it first (alphabetical order: `helpers-spec.el'
;; precedes `send-and-completion-spec.el'), the feature is already
;; provided.  When this spec is loaded in isolation, fall back to an
;; explicit path-based load — guarded by `featurep' so re-running the
;; full directory does not duplicate-execute the helpers' own smoke
;; tests.
(unless (featurep 'jf-persistent-agent-test-helpers)
  (load (expand-file-name
         "helpers-spec.el"
         (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(require 'jf-persistent-agent-test-helpers)
(require 'gptel)
(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-send)        ; gptel-chat-fsm-handlers + lifecycle handlers
(require 'gptel-chat-stream)      ; gptel-chat-stream-callback
(require 'gptel-persistent-agent)


;;; Content builders --------------------------------------------------------

(defconst jf/pa-send-test--two-turn-with-trailing-text
  (concat "#+begin_user\n"
          "Q\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "intermediate\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Q2\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Sure.\n"
          "\n"
          "#+begin_tool (some_tool :arg \"x\")\n"
          "(:name \"some_tool\" :args (:arg \"x\"))\n"
          "\n"
          "result\n"
          "#+end_tool\n"
          "\n"
          "Final answer.\n"
          "#+end_assistant\n")
  "Multi-turn buffer where the last assistant has tool block + trailing text.")

(defconst jf/pa-send-test--tool-only-assistant
  (concat "#+begin_user\n"
          "Q\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "#+begin_tool (some_tool)\n"
          "(:name \"some_tool\" :args nil)\n"
          "\n"
          "result\n"
          "#+end_tool\n"
          "#+end_assistant\n")
  "Buffer whose last assistant turn contains only a tool block (no text).")


;;; Helpers -----------------------------------------------------------------

(defun jf/pa-send-test--make-chat-buffer (content)
  "Return a fresh `gptel-chat-mode' buffer populated with CONTENT.
Caller is responsible for killing the returned buffer."
  (let ((buf (generate-new-buffer " *pa-send-test-chat*")))
    (with-current-buffer buf
      (insert content)
      (goto-char (point-min))
      (gptel-chat-mode))
    buf))

(defun jf/pa-send-test--captured-call (captured)
  "Extract the last captured `gptel-request' invocation from CAPTURED.
CAPTURED is the list pushed onto by
`jf/persistent-agent-test--with-mock-gptel-request' — its CAR is the
most recent invocation in the form `(:prompt PROMPT . PLIST)'.

Returns a plist with `:prompt' (the first positional arg to
`gptel-request') and `:args' (the keyword-arg plist that followed)."
  (let* ((entry (car captured))
         (payload (cdr entry)))               ; (PROMPT . PLIST)
    (list :prompt (car payload)
          :args  (cdr payload))))


;;; Specs -------------------------------------------------------------------

(describe "gptel-request invocation shape"
  (it "issues a gptel-request with messages, stream-callback, and composed FSM"
    ;; Scenario: openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
    ;;           §"Execution lifecycle" → "gptel-request invocation shape"
    (jf/persistent-agent-test--with-mock-parent-session
      (jf/persistent-agent-test--with-mock-preset 'test-preset
        (let ((captured nil))
          (jf/persistent-agent-test--with-mock-gptel-request captured
            (jf/gptel-persistent-agent--task
             #'ignore "test-preset" "demo task" "do the thing" nil))
          ;; Exactly one gptel-request invocation captured.
          (expect (length captured) :to-equal 1)
          (let* ((invocation (jf/pa-send-test--captured-call captured))
                 (prompt     (plist-get invocation :prompt))
                 (args       (plist-get invocation :args)))
            ;; First positional: the messages list (not nil, not a string).
            (expect (listp prompt) :to-be-truthy)
            (expect (stringp prompt) :to-be nil)
            (expect prompt :not :to-be nil)
            ;; Round-trip: drawer + chat-mode parser emits the user prompt
            ;; as `(prompt . STR)' where STR matches "do the thing" after
            ;; trimming the newline the on-disk drawer template wraps it
            ;; in.
            (let ((user-entry (cl-find-if
                               (lambda (msg)
                                 (and (consp msg) (eq (car msg) 'prompt)))
                               prompt)))
              (expect user-entry :not :to-be nil)
              (expect (string-trim (cdr user-entry))
                      :to-equal "do the thing"))
            ;; :stream t.
            (expect (plist-get args :stream) :to-be t)
            ;; :callback is a function (the chat-mode stream-callback closure).
            (expect (functionp (plist-get args :callback)) :to-be-truthy)
            ;; :context is the parent overlay.
            (expect (overlayp (plist-get args :context)) :to-be-truthy)
            ;; :fsm is a `gptel-fsm' struct.
            (expect (gptel-fsm-p (plist-get args :fsm)) :to-be-truthy)))))))


(describe "FSM handler composition"
  (it "composes WAIT/TOOL/DONE/ERRS/ABRT atop chat-mode lifecycle and upstream"
    ;; Scenarios: openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
    ;;            §"Execution lifecycle" → "WAIT state updates the parent overlay"
    ;;            §"Execution lifecycle" → "TOOL state updates the parent overlay
    ;;                                       with cumulative count"
    (jf/persistent-agent-test--with-mock-parent-session
      (jf/persistent-agent-test--with-mock-preset 'test-preset
        (let ((captured nil))
          (jf/persistent-agent-test--with-mock-gptel-request captured
            (jf/gptel-persistent-agent--task
             #'ignore "test-preset" "demo task" "do the thing" nil))
          (let* ((invocation (jf/pa-send-test--captured-call captured))
                 (fsm        (plist-get (plist-get invocation :args) :fsm))
                 (handlers   (gptel-fsm-handlers fsm))
                 (wait-chain (cdr (assq 'WAIT handlers)))
                 (tool-chain (cdr (assq 'TOOL handlers)))
                 (done-chain (cdr (assq 'DONE handlers)))
                 (errs-chain (cdr (assq 'ERRS handlers)))
                 (abrt-chain (cdr (assq 'ABRT handlers))))
            ;; WAIT: agent overlay updater → chat lifecycle → upstream transition.
            (expect wait-chain :to-equal
                    (list #'jf/gptel-persistent-agent--indicate-wait
                          #'gptel-chat--on-wait
                          #'gptel--handle-wait))
            ;; TOOL: agent overlay updater → chat lifecycle → upstream transition.
            (expect tool-chain :to-equal
                    (list #'jf/gptel-persistent-agent--indicate-tool-call
                          #'gptel-chat--on-tool
                          #'gptel--handle-tool-use))
            ;; Terminal states: agent's done/errs/abrt closure runs first, then
            ;; chat-mode lifecycle, then upstream's post hook.  The first
            ;; element is a closure (function value, not a symbol).
            (expect (length done-chain) :to-equal 3)
            (expect (functionp (nth 0 done-chain)) :to-be-truthy)
            (expect (symbolp  (nth 0 done-chain)) :to-be nil)
            (expect (nth 1 done-chain) :to-equal #'gptel-chat--on-done)
            (expect (nth 2 done-chain) :to-equal #'gptel--handle-post)

            (expect (length errs-chain) :to-equal 3)
            (expect (functionp (nth 0 errs-chain)) :to-be-truthy)
            (expect (symbolp  (nth 0 errs-chain)) :to-be nil)
            (expect (nth 1 errs-chain) :to-equal #'gptel-chat--on-errs)
            (expect (nth 2 errs-chain) :to-equal #'gptel--handle-post)

            (expect (length abrt-chain) :to-equal 3)
            (expect (functionp (nth 0 abrt-chain)) :to-be-truthy)
            (expect (symbolp  (nth 0 abrt-chain)) :to-be nil)
            (expect (nth 1 abrt-chain) :to-equal #'gptel-chat--on-abrt)
            (expect (nth 2 abrt-chain) :to-equal #'gptel--handle-post)))))))


(describe "Final-text extraction"
  (it "returns the last assistant text segment when present"
    ;; Scenario: openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
    ;;           §"Parent-child communication" → "DONE returns the final
    ;;           assistant text segment"
    (let ((buf (jf/pa-send-test--make-chat-buffer
                jf/pa-send-test--two-turn-with-trailing-text)))
      (unwind-protect
          (let ((text (jf/gptel-persistent-agent--extract-final-text buf)))
            (expect (stringp text) :to-be-truthy)
            ;; Parser preserves the segment's content; assert on the
            ;; trimmed shape so leading/trailing whitespace inside the
            ;; segment doesn't perturb the test.
            (expect (string-trim text) :to-equal "Final answer."))
        (when (buffer-live-p buf) (kill-buffer buf)))))

  (it "returns the empty string when the last assistant has no text segment"
    ;; Decision 2 (design.md §"Decisions" 2): empty-text fallback returns
    ;; "" — not nil, not an error.
    (let ((buf (jf/pa-send-test--make-chat-buffer
                jf/pa-send-test--tool-only-assistant)))
      (unwind-protect
          (let ((text (jf/gptel-persistent-agent--extract-final-text buf)))
            (expect text :to-equal ""))
        (when (buffer-live-p buf) (kill-buffer buf)))))

  (it "DONE handler invokes main-cb exactly once with the final text"
    ;; Scenario: openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
    ;;           §"Parent-child communication" → "DONE returns the final
    ;;           assistant text segment" (call-count piece)
    (let* ((calls nil)
           (main-cb (lambda (text) (push text calls)))
           (agent-buf (jf/pa-send-test--make-chat-buffer
                       jf/pa-send-test--two-turn-with-trailing-text))
           (overlay-buf (generate-new-buffer " *pa-send-test-overlay*"))
           (overlay (with-current-buffer overlay-buf
                      (insert "anchor")
                      (make-overlay (point-min) (point-max))))
           (handler (jf/gptel-persistent-agent--make-on-done main-cb agent-buf))
           (fsm (gptel-make-fsm :info (list :context overlay))))
      (unwind-protect
          (progn
            (funcall handler fsm)
            (expect (length calls) :to-equal 1)
            (expect (string-trim (car calls)) :to-equal "Final answer.")
            ;; Overlay struct still exists, but its buffer slot is nil
            ;; (delete-overlay detaches it without freeing the struct).
            (expect (overlayp overlay) :to-be-truthy)
            (expect (overlay-buffer overlay) :to-be nil))
        (when (buffer-live-p agent-buf)   (kill-buffer agent-buf))
        (when (buffer-live-p overlay-buf) (kill-buffer overlay-buf))))))


(describe "Stream callback wiring"
  (it "uses chat-mode's public stream callback (a closure, not a symbol)"
    ;; Scenario: openspec/changes/persistent-agent-rebuild/specs/persistent-agent/spec.md
    ;;           §"Execution lifecycle" → "gptel-request invocation shape"
    ;;           (stream-callback piece)
    (jf/persistent-agent-test--with-mock-parent-session
      (jf/persistent-agent-test--with-mock-preset 'test-preset
        (let ((captured nil))
          (jf/persistent-agent-test--with-mock-gptel-request captured
            (jf/gptel-persistent-agent--task
             #'ignore "test-preset" "demo task" "do the thing" nil))
          (let* ((invocation (jf/pa-send-test--captured-call captured))
                 (callback   (plist-get (plist-get invocation :args) :callback)))
            ;; The callback is a function value (closure or compiled
            ;; function).  It is not a bare symbol — the agent does not
            ;; pass `gptel-chat-stream-callback' itself, but the closure
            ;; that function returns.
            (expect (functionp callback) :to-be-truthy)
            (expect (symbolp callback) :to-be nil)
            ;; gptel-chat-stream-callback returns a NEW closure on each
            ;; call, so the captured value is not `eq' to the function
            ;; symbol.
            (expect (eq callback #'gptel-chat-stream-callback) :to-be nil)))))))

(provide 'jf-persistent-agent-send-and-completion-spec)
;;; send-and-completion-spec.el ends here
