;;; multi-round-tool-use-spec.el --- Buttercup tests for gptel-chat multi-round tool-use t-signal gating -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises the multi-round tool-use gate on the `t' (HTTP success)
;; arm of `gptel-chat--stream-callback' (design.md §Decision 10).
;;
;; Upstream's `gptel-curl--stream-cleanup' calls
;; `(funcall callback t info)' on every HTTP success — once per
;; round-trip.  For a multi-round tool-use turn the sequence is:
;;   Request-1 streaming text
;;   → `t' (with :tool-use)            ← must NOT close block
;;   → (tool-call . ...)
;;   → (tool-result . ...)
;;   → Request-2 streaming text
;;   → `t' (no :tool-use)              ← close block here only
;;
;; If the first `t' closed the assistant block, the subsequent
;; tool-result events and Request-2 text would land after
;; `#+end_assistant' and corrupt the buffer.  The canonical pattern
;; `(unless (plist-get info :tool-use) …)' (see
;; `persistent-agent.org') is applied here.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel)

;; Load the module under test from the co-located source directory.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-stream)


;;; Fixtures -----------------------------------------------------------------

(defun gptel-chat-multi-round-test--tool (name)
  "Build a minimal `gptel-tool' struct with NAME for test fixtures.
Upstream emits tool-call / tool-result events as 3-lists
`(TOOL-STRUCT ARGS CB-OR-RESULT)'; only the struct's name slot is
consulted by the stream callback."
  (gptel-make-tool
   :name name
   :function #'ignore
   :description (format "test tool %s" name)
   :args nil
   :category "test"))

(defvar gptel-chat-multi-round-test--buffer nil
  "Scratch buffer for multi-round tool-use tests.")

(defvar gptel-chat-multi-round-test--marker nil
  "Advance insertion marker at the end of the open assistant block.")

(defun gptel-chat-multi-round-test--fresh-buffer ()
  "Create a fresh scratch buffer containing a single open assistant block.
Returns an advance marker at the end of the assistant body."
  (setq gptel-chat-multi-round-test--buffer
        (generate-new-buffer " *gptel-chat-multi-round-test*"))
  (with-current-buffer gptel-chat-multi-round-test--buffer
    (insert "#+begin_user\nhello\n#+end_user\n#+begin_assistant\n")
    (setq gptel-chat-multi-round-test--marker
          (copy-marker (point-max) t)))
  gptel-chat-multi-round-test--marker)

(defun gptel-chat-multi-round-test--buffer-string ()
  "Return the current contents of the scratch test buffer."
  (with-current-buffer gptel-chat-multi-round-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-chat-multi-round-test--count-occurrences (needle)
  "Return the number of times NEEDLE appears in the test buffer."
  (with-current-buffer gptel-chat-multi-round-test--buffer
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward needle nil t)
          (cl-incf count))
        count))))

(defun gptel-chat-multi-round-test--cleanup ()
  (when (buffer-live-p gptel-chat-multi-round-test--buffer)
    (kill-buffer gptel-chat-multi-round-test--buffer))
  (setq gptel-chat-multi-round-test--buffer nil
        gptel-chat-multi-round-test--marker nil))


;;; Specs --------------------------------------------------------------------

(describe "gptel-chat--stream-callback multi-round tool-use"

  (before-each
    (gptel-chat-multi-round-test--fresh-buffer))

  (after-each
    (gptel-chat-multi-round-test--cleanup))

  (describe "intermediate `t' with :tool-use set (mid-turn)"

    (it "does NOT close the assistant block on the first `t'"
      ;; Upstream fires `t' at the end of every HTTP round-trip.
      ;; Before the first `t' here, Request-1 has finished streaming
      ;; text and is about to emit tool-call → tool-result → Request-2.
      ;; Upstream's FSM sets :tool-use on INFO when the request is
      ;; marked tool-use (pre-handle-tool-use).  On this mid-turn `t'
      ;; the callback must flush holdback but leave
      ;; `#+begin_assistant' open so subsequent events land inside.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        (funcall cb "Thinking about it.\n" '(:tool-use t))
        (funcall cb t '(:tool-use t)))
      (expect (gptel-chat-multi-round-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "Thinking about it.\n"))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 0))

    (it "flushes a trailing partial line on the mid-turn `t' (no newline added)"
      ;; The holdback must still be drained on every `t' so text is
      ;; not lost; only the close-and-append sequence is gated.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        (funcall cb "partial-no-newline" '(:tool-use t))
        (funcall cb t '(:tool-use t)))
      ;; Partial is committed, block is still open (no trailing \n
      ;; after the partial, no `#+end_assistant').
      (expect (gptel-chat-multi-round-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "partial-no-newline"))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 0)))

  (describe "full multi-round flow: text → t(tool-use) → tool-call → tool-result → text → t(done)"

    (it "produces exactly one #+end_assistant, after the final `t'"
      ;; Scripted sequence mirroring the upstream FSM:
      ;;   Request-1 streams text
      ;;   Request-1 completes with `t' (INFO :tool-use t)
      ;;   tool-call event
      ;;   tool-result event
      ;;   Request-2 streams text
      ;;   Request-2 completes with `t' (INFO :tool-use unset) ← DONE
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        ;; Request-1 streaming text (no :tool-use on chunks — upstream
        ;; only marks :tool-use on the terminal `t' for the tool-use
        ;; turn).
        (funcall cb "Let me check.\n" nil)
        ;; Request-1 completes; FSM has marked this request as tool-use.
        (funcall cb t '(:tool-use t))
        ;; Tool call and its result (upstream's 3-list shape).
        (funcall cb `(tool-call . ((,(gptel-chat-multi-round-test--tool "read_file")
                                    (:path "/a")
                                    ,#'ignore)))
                 nil)
        (funcall cb `(tool-result . ((,(gptel-chat-multi-round-test--tool "read_file")
                                      (:path "/a")
                                      "file contents")))
                 nil)
        ;; Request-2 streams the assistant's final answer.
        (funcall cb "Done reading.\n" nil)
        ;; Request-2 completes; final turn, no :tool-use.
        (funcall cb t nil))
      ;; Expected buffer: one open-assistant header, the Request-1
      ;; prose, the rendered tool block, the Request-2 prose, and
      ;; exactly one `#+end_assistant' close.
      (expect (gptel-chat-multi-round-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "Let me check.\n"
                      "#+begin_tool (read_file :path \"/a\")\n"
                      "file contents\n"
                      "#+end_tool\n"
                      "Done reading.\n"
                      "#+end_assistant\n"
                      "\n#+begin_user\n\n#+end_user\n"))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 1))

    (it "handles two tool-use rounds before the final `t'"
      ;; A harder case: the assistant makes two consecutive tool-use
      ;; rounds before its final turn.  The block must stay open
      ;; across BOTH intermediate `t' signals.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        ;; Round 1: text → t(:tool-use t) → call → result.
        (funcall cb "Round1 prose\n" nil)
        (funcall cb t '(:tool-use t))
        (funcall cb `(tool-call . ((,(gptel-chat-multi-round-test--tool "t1")
                                    (:k 1)
                                    ,#'ignore)))
                 nil)
        (funcall cb `(tool-result . ((,(gptel-chat-multi-round-test--tool "t1")
                                      (:k 1)
                                      "r1")))
                 nil)
        ;; Round 2: text → t(:tool-use t) → call → result.
        (funcall cb "Round2 prose\n" nil)
        (funcall cb t '(:tool-use t))
        (funcall cb `(tool-call . ((,(gptel-chat-multi-round-test--tool "t2")
                                    (:k 2)
                                    ,#'ignore)))
                 nil)
        (funcall cb `(tool-result . ((,(gptel-chat-multi-round-test--tool "t2")
                                      (:k 2)
                                      "r2")))
                 nil)
        ;; Final turn: text → t(no :tool-use).
        (funcall cb "Final answer.\n" nil)
        (funcall cb t nil))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 1)
      (expect (gptel-chat-multi-round-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "Round1 prose\n"
                      "#+begin_tool (t1 :k 1)\nr1\n#+end_tool\n"
                      "Round2 prose\n"
                      "#+begin_tool (t2 :k 2)\nr2\n#+end_tool\n"
                      "Final answer.\n"
                      "#+end_assistant\n"
                      "\n#+begin_user\n\n#+end_user\n"))))

  (describe "single-turn (no tool-use) still closes on `t'"

    (it "closes block and appends a fresh user block when :tool-use is absent"
      ;; Regression guard: gating on :tool-use must not break the
      ;; common single-turn path where upstream fires exactly one `t'
      ;; with no :tool-use flag set.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        (funcall cb "One-shot answer.\n" nil)
        (funcall cb t nil))
      (expect (gptel-chat-multi-round-test--buffer-string)
              :to-equal
              (concat "#+begin_user\nhello\n#+end_user\n"
                      "#+begin_assistant\n"
                      "One-shot answer.\n"
                      "#+end_assistant\n"
                      "\n#+begin_user\n\n#+end_user\n"))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 1))

    (it "closes block even when INFO is nil (no plist at all)"
      ;; Defensive: `(plist-get nil :tool-use)' returns nil, so a
      ;; nil INFO must behave identically to an INFO with no
      ;; :tool-use flag.  Pins that contract.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        (funcall cb "Hello.\n" nil)
        (funcall cb t nil))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 1))

    (it "closes block when INFO has :tool-use explicitly nil"
      ;; Another shape upstream could plausibly send: the plist
      ;; carries :tool-use but its value is nil (flag cleared).
      ;; Must still close.
      (let ((cb (gptel-chat--stream-callback
                 gptel-chat-multi-round-test--marker)))
        (funcall cb "Hello.\n" nil)
        (funcall cb t '(:tool-use nil)))
      (expect (gptel-chat-multi-round-test--count-occurrences
               "#+end_assistant")
              :to-equal 1))))


(provide 'multi-round-tool-use-spec)

;;; multi-round-tool-use-spec.el ends here
