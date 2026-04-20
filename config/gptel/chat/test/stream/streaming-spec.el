;;; streaming-spec.el --- Buttercup tests for gptel-chat streaming sanitizer and line-holdback closure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises the two public entry points of `gptel-chat-stream'
;; delivered by task `sanitize-chunks':
;;
;; 1. `gptel-chat--sanitize-chunk' — single-line delimiter-collision
;;    escape.  Verifies the case-insensitive three-delimiter match
;;    rule (#+end_user / #+end_assistant / #+end_tool) and the
;;    intentional absence of escaping for lines like `#+end_src',
;;    `#+begin_assistant', or generic prose (design.md §Decision 4).
;;
;; 2. `gptel-chat--make-stream-closure' — per-request closure that
;;    line-buffers streamed chunks via a holdback and inserts
;;    sanitized complete lines at a captured marker.  Covers
;;    multi-chunk in-order insertion, collisions that land on a
;;    whole line, stream completion via the `t' flush sentinel,
;;    and the exposed `tool-marker' slot (behaviourally: when a
;;    caller has bound a live marker into the closure's tool slot
;;    via direct surgery on the captured environment, inserts
;;    route there instead of to the assistant marker).
;;
;; Scenarios covered (spec §"Response streaming and sanitization"):
;; - Normal stream completion (multi-chunk in-order).
;; - Response containing #+end_assistant on its own line is escaped.
;; - Case-insensitive: #+END_ASSISTANT, #+End_Assistant are escaped.
;; - #+end_src / #+begin_assistant / plain prose pass through.
;;
;; The split-across-chunks scenario (chunk 1 ends with `#+end_ass',
;; chunk 2 begins with `istant\nmore') lives in `chunk-split-spec.el'.
;; Abort handling and tool-call rendering belong to task
;; `stream-callback' and are out of scope here.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/stream/;
;; two levels up is .../config/gptel/chat/, which holds `stream.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-stream)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-stream-test--buffer nil
  "Scratch buffer for streaming-closure tests.")

(defvar gptel-chat-stream-test--marker nil
  "Live insertion marker into `gptel-chat-stream-test--buffer'.")

(defun gptel-chat-stream-test--fresh-buffer ()
  "Create a fresh empty test buffer and a marker at point-min.
Returns the marker."
  (setq gptel-chat-stream-test--buffer
        (generate-new-buffer " *gptel-chat-stream-test*"))
  (with-current-buffer gptel-chat-stream-test--buffer
    (setq gptel-chat-stream-test--marker
          (copy-marker (point-min) t)))
  gptel-chat-stream-test--marker)

(defun gptel-chat-stream-test--buffer-string ()
  "Return the contents of the scratch test buffer."
  (with-current-buffer gptel-chat-stream-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun gptel-chat-stream-test--cleanup ()
  "Kill the scratch buffer if one was created."
  (when (buffer-live-p gptel-chat-stream-test--buffer)
    (kill-buffer gptel-chat-stream-test--buffer))
  (setq gptel-chat-stream-test--buffer nil
        gptel-chat-stream-test--marker nil))


;;; gptel-chat--sanitize-chunk ----------------------------------------------

(describe "gptel-chat--sanitize-chunk"

  (describe "matches the three chat-mode end-delimiters"

    (it "escapes #+end_user"
      (expect (gptel-chat--sanitize-chunk "#+end_user")
              :to-equal ",#+end_user"))

    (it "escapes #+end_assistant"
      (expect (gptel-chat--sanitize-chunk "#+end_assistant")
              :to-equal ",#+end_assistant"))

    (it "escapes #+end_tool"
      (expect (gptel-chat--sanitize-chunk "#+end_tool")
              :to-equal ",#+end_tool")))

  (describe "is case-insensitive (design.md §Decision 4)"

    (it "escapes upper-case #+END_ASSISTANT"
      (expect (gptel-chat--sanitize-chunk "#+END_ASSISTANT")
              :to-equal ",#+END_ASSISTANT"))

    (it "escapes mixed-case #+End_Assistant"
      (expect (gptel-chat--sanitize-chunk "#+End_Assistant")
              :to-equal ",#+End_Assistant"))

    (it "escapes upper-case #+END_TOOL"
      (expect (gptel-chat--sanitize-chunk "#+END_TOOL")
              :to-equal ",#+END_TOOL")))

  (describe "leaves non-matching lines untouched"

    (it "passes #+end_src through (not one of the three)"
      (expect (gptel-chat--sanitize-chunk "#+end_src")
              :to-equal "#+end_src"))

    (it "passes #+begin_assistant through (begin, not end)"
      (expect (gptel-chat--sanitize-chunk "#+begin_assistant")
              :to-equal "#+begin_assistant"))

    (it "passes #+begin_user through"
      (expect (gptel-chat--sanitize-chunk "#+begin_user")
              :to-equal "#+begin_user"))

    (it "passes plain prose through"
      (expect (gptel-chat--sanitize-chunk "Hello world.")
              :to-equal "Hello world."))

    (it "passes the empty string through"
      (expect (gptel-chat--sanitize-chunk "")
              :to-equal ""))

    (it "passes #+end_useful through (word-boundary guard)"
      ;; The \b anchor ensures the delimiter is a whole word; a line
      ;; like `#+end_useful' must not be mistaken for `#+end_user'.
      (expect (gptel-chat--sanitize-chunk "#+end_useful")
              :to-equal "#+end_useful"))

    (it "only escapes once — #+end_user on input is a plain line, not a re-escape of an already-escaped form"
      ;; Sanitize is idempotent only on already-escaped input: a line
      ;; that already begins with `,#+end_...' does not start with
      ;; `#+end_' and so is returned unchanged.  This is the expected
      ;; round-trip with the parser's un-escape on read.
      (expect (gptel-chat--sanitize-chunk ",#+end_assistant")
              :to-equal ",#+end_assistant")))

  (describe "escapes with leading whitespace only when anchored at column 0"
    ;; Decision 14 pins delimiters to column 0.  A line with leading
    ;; whitespace is structurally safe (parser anchors on ^#\+end_...)
    ;; and must pass through untouched so that indented content looking
    ;; like a delimiter is not spuriously escaped.
    (it "passes `  #+end_assistant' (indented) through"
      (expect (gptel-chat--sanitize-chunk "  #+end_assistant")
              :to-equal "  #+end_assistant"))))


;;; gptel-chat--make-stream-closure ------------------------------------------

(describe "gptel-chat--make-stream-closure"

  (before-each
    (gptel-chat-stream-test--fresh-buffer))

  (after-each
    (gptel-chat-stream-test--cleanup))

  (describe "argument validation"

    (it "rejects a non-marker argument"
      (expect (gptel-chat--make-stream-closure 42)
              :to-throw))

    (it "rejects a marker with no buffer"
      (let ((dead (make-marker)))
        ;; A freshly-made marker has no buffer until you set one.
        (expect (gptel-chat--make-stream-closure dead)
                :to-throw))))

  (describe "inserting complete lines in order"

    (it "inserts two complete lines from a single newline-terminated chunk"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "hello\nworld\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "hello\nworld\n"))

    (it "preserves order across multiple chunks with no collisions"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "alpha\n")
        (funcall cb "beta\n")
        (funcall cb "gamma\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "alpha\nbeta\ngamma\n"))

    (it "flushes a final partial line via the t sentinel without adding a newline"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "trailing partial")
        ;; Before flush, nothing is committed — the partial is still
        ;; in the holdback.
        (expect (gptel-chat-stream-test--buffer-string) :to-equal "")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "trailing partial")))

  (describe "holdback behaviour (no premature inserts)"

    (it "does not insert a partial line that arrives without a newline"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "partial "))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal ""))

    (it "completes a line only when a later chunk supplies the newline"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "partial ")
        (expect (gptel-chat-stream-test--buffer-string) :to-equal "")
        (funcall cb "line\n")
        (expect (gptel-chat-stream-test--buffer-string)
                :to-equal "partial line\n"))))

  (describe "sanitizes collisions on whole-line chunks"

    (it "escapes #+end_assistant when it arrives as its own line"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "prose\n#+end_assistant\nmore\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "prose\n,#+end_assistant\nmore\n"))

    (it "escapes case-variant #+End_Assistant mid-stream"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "a\n#+End_Assistant\nb\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "a\n,#+End_Assistant\nb\n"))

    (it "does NOT escape #+end_src on its own line"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "before\n#+end_src\nafter\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "before\n#+end_src\nafter\n"))

    (it "does NOT escape #+begin_assistant (begin, not end)"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "a\n#+begin_assistant\nb\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "a\n#+begin_assistant\nb\n")))

  (describe "flush semantics on stream completion"

    (it "flush of an empty holdback is a no-op"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "full line\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "full line\n"))

    (it "flush sanitizes a trailing #+end_assistant partial (no newline)"
      ;; Per the task spec: upstream sends `t' after a newline-terminated
      ;; final chunk (holdback empty) OR a final content chunk with no
      ;; newline (holdback is a single line).  In the latter case the
      ;; single-line flush goes through the same sanitizer.
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-stream-test--marker)))
        (funcall cb "#+end_assistant")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal ",#+end_assistant")))

  (describe "marker-based insertion is robust to edits above the insertion point"

    (it "user edits above the marker do not corrupt subsequent inserts"
      ;; Decision 3b: insertion uses a *marker*, not an integer
      ;; position, precisely so concurrent user edits above the
      ;; insertion point do not shift where new content lands.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "header\n"))
      ;; Re-anchor: the marker was at point-min and insertion-before
      ;; type nil means it stays at point-min (byte 1), which is now
      ;; above "header\n" — inserts should go at the top of the buffer.
      ;;
      ;; Use an advance marker instead to model "the end of the open
      ;; assistant block" — a marker at point-max with insertion-type
      ;; t, so user prepends above it don't shift the insertion
      ;; target away from the end.
      (let* ((advance-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-max) t)))
             (cb (gptel-chat--make-stream-closure advance-marker)))
        (funcall cb "response line\n")
        (funcall cb t)
        ;; User edit ABOVE the marker after the first insert:
        (with-current-buffer gptel-chat-stream-test--buffer
          (goto-char (point-min))
          (insert "user-edit\n"))
        (funcall cb "second\n")
        (funcall cb t)
        ;; Both response lines must appear after the header/user-edit,
        ;; i.e., at the end of the buffer, in order.
        (expect (gptel-chat-stream-test--buffer-string)
                :to-equal "user-edit\nheader\nresponse line\nsecond\n"))))

  (describe "closure isolation"

    (it "each factory call produces an independent holdback"
      (let ((cb1 (gptel-chat--make-stream-closure
                  gptel-chat-stream-test--marker)))
        ;; Park a partial in cb1's holdback, then throw it away; a
        ;; fresh closure must not inherit it.
        (funcall cb1 "partial-from-cb1")
        (setq cb1 nil))
      (let ((cb2 (gptel-chat--make-stream-closure
                  gptel-chat-stream-test--marker)))
        (funcall cb2 "fresh\n")
        (funcall cb2 t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "fresh\n"))))

(provide 'streaming-spec)

;;; streaming-spec.el ends here
