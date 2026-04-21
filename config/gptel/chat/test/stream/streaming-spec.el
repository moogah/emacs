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
;; 2. `gptel-chat--make-stream-inserter' — per-request factory that
;;    returns a `gptel-chat-stream' cl-struct handle.  The handle's
;;    `insert' slot is the line-buffered chunk processor; its
;;    `set-tool-marker' / `clear-tool-marker' slots expose the
;;    tool-routing override so callers (notably the later
;;    `stream-callback' task) can flip routing without `cl-letf'
;;    surgery on captured state (design.md §Decision 3b).  Covers
;;    multi-chunk in-order insertion, collisions that land on a
;;    whole line, stream completion via the `t' flush sentinel,
;;    and the typed struct surface itself (predicate, setter
;;    validation, setter-routes-inserts, clearer-restores-routing).
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


;;; gptel-chat--make-stream-inserter ------------------------------------------

(describe "gptel-chat--make-stream-inserter"

  (before-each
    (gptel-chat-stream-test--fresh-buffer))

  (after-each
    (gptel-chat-stream-test--cleanup))

  (describe "argument validation"

    (it "rejects a non-marker argument"
      (expect (gptel-chat--make-stream-inserter 42)
              :to-throw))

    (it "rejects a marker with no buffer"
      (let ((dead (make-marker)))
        ;; A freshly-made marker has no buffer until you set one.
        (expect (gptel-chat--make-stream-inserter dead)
                :to-throw))))

  (describe "inserting complete lines in order"

    (it "inserts two complete lines from a single newline-terminated chunk"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "hello\nworld\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "hello\nworld\n"))

    (it "preserves order across multiple chunks with no collisions"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "alpha\n")
        (funcall cb "beta\n")
        (funcall cb "gamma\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "alpha\nbeta\ngamma\n"))

    (it "flushes a final partial line via the t sentinel without adding a newline"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "trailing partial")
        ;; Before flush, nothing is committed — the partial is still
        ;; in the holdback.
        (expect (gptel-chat-stream-test--buffer-string) :to-equal "")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "trailing partial")))

  (describe "holdback behaviour (no premature inserts)"

    (it "does not insert a partial line that arrives without a newline"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "partial "))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal ""))

    (it "completes a line only when a later chunk supplies the newline"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "partial ")
        (expect (gptel-chat-stream-test--buffer-string) :to-equal "")
        (funcall cb "line\n")
        (expect (gptel-chat-stream-test--buffer-string)
                :to-equal "partial line\n"))))

  (describe "sanitizes collisions on whole-line chunks"

    (it "escapes #+end_assistant when it arrives as its own line"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "prose\n#+end_assistant\nmore\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "prose\n,#+end_assistant\nmore\n"))

    (it "escapes case-variant #+End_Assistant mid-stream"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "a\n#+End_Assistant\nb\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "a\n,#+End_Assistant\nb\n"))

    (it "does NOT escape #+end_src on its own line"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "before\n#+end_src\nafter\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "before\n#+end_src\nafter\n"))

    (it "does NOT escape #+begin_assistant (begin, not end)"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "a\n#+begin_assistant\nb\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "a\n#+begin_assistant\nb\n")))

  (describe "flush semantics on stream completion"

    (it "flush of an empty holdback is a no-op"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "full line\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "full line\n"))

    (it "flush sanitizes a trailing #+end_assistant partial (no newline)"
      ;; Per the task spec: upstream sends `t' after a newline-terminated
      ;; final chunk (holdback empty) OR a final content chunk with no
      ;; newline (holdback is a single line).  In the latter case the
      ;; single-line flush goes through the same sanitizer.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
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
             (handle (gptel-chat--make-stream-inserter advance-marker))
             (cb (gptel-chat-stream-insert handle)))
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
      (let* ((handle1 (gptel-chat--make-stream-inserter
                       gptel-chat-stream-test--marker))
             (cb1 (gptel-chat-stream-insert handle1)))
        ;; Park a partial in cb1's holdback, then throw it away; a
        ;; fresh closure must not inherit it.
        (funcall cb1 "partial-from-cb1")
        (setq cb1 nil
              handle1 nil))
      (let* ((handle2 (gptel-chat--make-stream-inserter
                       gptel-chat-stream-test--marker))
             (cb2 (gptel-chat-stream-insert handle2)))
        (funcall cb2 "fresh\n")
        (funcall cb2 t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "fresh\n"))))


;;; gptel-chat-stream handle API --------------------------------------------
;;
;; These specs exercise the typed `cl-defstruct' surface introduced by
;; task `expose-tool-marker-setter'.  They cover the minimum public
;; shape — predicate + three function slots — needed by the downstream
;; `stream-callback' task to wire tool-call/tool-result events without
;; `cl-letf' surgery on the captured closure environment.  Exhaustive
;; routing-correctness tests (what happens to text across many
;; set/clear toggles) are owned by task `tool-marker-routing-tests'.

(describe "gptel-chat-stream handle API"

  (before-each
    (gptel-chat-stream-test--fresh-buffer))

  (after-each
    (gptel-chat-stream-test--cleanup))

  (describe "factory return type"

    (it "returns a gptel-chat-stream struct"
      (let ((handle (gptel-chat--make-stream-inserter
                     gptel-chat-stream-test--marker)))
        (expect (gptel-chat-stream-p handle) :to-be-truthy)))

    (it "populates all three function slots"
      (let ((handle (gptel-chat--make-stream-inserter
                     gptel-chat-stream-test--marker)))
        (expect (functionp (gptel-chat-stream-insert handle))
                :to-be-truthy)
        (expect (functionp (gptel-chat-stream-set-tool-marker handle))
                :to-be-truthy)
        (expect (functionp (gptel-chat-stream-clear-tool-marker handle))
                :to-be-truthy))))

  (describe "set-tool-marker slot"

    (it "rejects a non-marker argument"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (setter (gptel-chat-stream-set-tool-marker handle)))
        (expect (funcall setter 42) :to-throw)))

    (it "rejects a marker with no buffer"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (setter (gptel-chat-stream-set-tool-marker handle))
             (dead (make-marker)))
        (expect (funcall setter dead) :to-throw)))

    (it "routes subsequent inserts to the tool marker"
      ;; Decision 3b: after set-tool-marker, insert writes land at the
      ;; tool marker, not the assistant marker.  Set up two distinct
      ;; markers in the same buffer with a sentinel between them so
      ;; routing is directly observable from the final buffer text.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "SENTINEL\n"))
      (let* ((assistant-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-min) t)))
             (tool-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-max) t)))
             (handle (gptel-chat--make-stream-inserter assistant-marker))
             (cb (gptel-chat-stream-insert handle))
             (set-tool (gptel-chat-stream-set-tool-marker handle)))
        ;; First insert goes to the assistant marker (before SENTINEL).
        (funcall cb "assistant-text\n")
        ;; Flip routing to the tool marker.
        (funcall set-tool tool-marker)
        ;; Second insert now goes to the tool marker (after SENTINEL).
        (funcall cb "tool-text\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "assistant-text\nSENTINEL\ntool-text\n")))

  (describe "clear-tool-marker slot"

    (it "routes subsequent inserts back to the assistant marker"
      ;; After clear-tool-marker, insert writes return to the
      ;; assistant marker.  Same two-marker layout as above.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "SENTINEL\n"))
      (let* ((assistant-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-min) t)))
             (tool-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-max) t)))
             (handle (gptel-chat--make-stream-inserter assistant-marker))
             (cb (gptel-chat-stream-insert handle))
             (set-tool (gptel-chat-stream-set-tool-marker handle))
             (clear-tool (gptel-chat-stream-clear-tool-marker handle)))
        (funcall set-tool tool-marker)
        (funcall cb "in-tool\n")
        (funcall clear-tool)
        (funcall cb "back-to-assistant\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "back-to-assistant\nSENTINEL\nin-tool\n"))

    (it "is a no-op when no tool marker has been set"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle))
             (clear-tool (gptel-chat-stream-clear-tool-marker handle)))
        (funcall clear-tool)
        (funcall cb "hello\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "hello\n"))))


(provide 'streaming-spec)

;;; streaming-spec.el ends here
