;;; streaming-spec.el --- Buttercup tests for gptel-chat streaming indenter and line-holdback closure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Exercises the two public entry points of `gptel-chat-stream':
;;
;; 1. `gptel-chat--sanitize-chunk' — a per-line body indenter (change
;;    `gptel-chat-heading-scoping', design.md §Decisions 1 and 6).  A
;;    non-blank line is returned with `gptel-chat--body-indent' leading
;;    spaces prepended so streamed assistant content sits inside the
;;    indented chat-block body, off column 0 where org's structural
;;    scanners (the heading regex, the special-block closers, drawers,
;;    keywords) are anchored.  A blank line is returned unchanged.  An
;;    embedded newline signals an error.  This single mechanism
;;    supersedes the two per-token escapes of the earlier design (the
;;    `,#+end_*' delimiter escape and the `*' heading escape).
;;
;; 2. `gptel-chat--make-stream-inserter' — per-request factory that
;;    returns a `gptel-chat-stream' cl-struct handle.  The handle's
;;    `insert' slot is the line-buffered chunk processor; its
;;    `set-tool-marker' / `clear-tool-marker' slots expose the
;;    tool-routing override so callers (notably `stream-callback') can
;;    flip routing without `cl-letf' surgery on captured state
;;    (design.md §Decision 3b).  Covers multi-chunk in-order
;;    insertion, the body indenter applied per line, stream completion
;;    via the `t' flush sentinel, and the typed struct surface itself
;;    (predicate, setter validation, setter-routes-inserts,
;;    clearer-restores-routing).
;;
;; Scenarios covered (spec §"Response streaming and sanitization"):
;; - Normal stream completion (multi-chunk in-order, each line
;;   indented by the body width).
;; - A streamed `#+end_assistant' line is indented (body content, not
;;   a real closer).
;; - A streamed `* Heading' line is indented (body content, not a
;;   document heading).
;; - Blank lines pass through unchanged.
;;
;; The split-across-chunks scenario (chunk 1 ends with `* He', chunk 2
;; begins with `ading\nmore') lives in `chunk-split-spec.el'.  Abort
;; handling and tool-call rendering belong to `tool-call-spec.el'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/stream/;
;; two levels up is .../config/gptel/chat/, which holds `stream.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; `gptel-chat-mode' owns the `gptel-chat-content-indentation' defcustom
;; and the `gptel-chat--body-indent' accessor consulted by
;; `gptel-chat--sanitize-chunk' for the body indent.  In production
;; `mode.el' loads before `stream.el'; loading it here mirrors that
;; order so the indenter specs see the configured default rather than
;; the accessor's standalone fallback.
(require 'gptel-chat-mode)
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


;;; gptel-chat--sanitize-chunk: per-line body indenter ----------------------
;;
;; design.md §Decisions 1 and 6: `gptel-chat--sanitize-chunk' is a
;; line-level helper that indents a complete line (no embedded `\n')
;; by `gptel-chat--body-indent' leading spaces.  Indenting every
;; streamed line off column 0 is what keeps org's structural scanners
;; — the heading regex `^\*+ ', the special-block closers `^#\+end_…',
;; drawers, keywords — from reading assistant output as document
;; structure.  A blank line is returned unchanged so no trailing
;; whitespace is introduced.

(describe "gptel-chat--sanitize-chunk: per-line body indenter"

  (describe "indents a non-blank line by the body width"

    (it "prepends the body indent to plain prose (default width 2)"
      (expect (gptel-chat--sanitize-chunk "Hello world.")
              :to-equal "  Hello world."))

    (it "indents a heading-shaped line off column 0"
      ;; A `* Heading' at column 0 inside a chat-block body would be
      ;; absorbed by org's heading scanner; the indenter moves it off
      ;; column 0 so it reads as body content.
      (expect (gptel-chat--sanitize-chunk "* Heading")
              :to-equal "  * Heading"))

    (it "indents a deep `*** Deep' line the same way (count-independent)"
      ;; The indent does not scale with star count; any leading
      ;; whitespace breaks `^\*+ ' regardless of nesting depth.
      (expect (gptel-chat--sanitize-chunk "*** Deep")
              :to-equal "  *** Deep"))

    (it "indents a six-star `****** Even deeper' line"
      (expect (gptel-chat--sanitize-chunk "****** Even deeper")
              :to-equal "  ****** Even deeper"))

    (it "indents an end-delimiter-shaped line off column 0"
      ;; A streamed `#+end_assistant' line is body content; indenting
      ;; it means the parser's column-0-anchored closer regex never
      ;; matches it.
      (expect (gptel-chat--sanitize-chunk "#+end_assistant")
              :to-equal "  #+end_assistant"))

    (it "indents a `#+end_tool'-shaped line off column 0"
      (expect (gptel-chat--sanitize-chunk "#+end_tool")
              :to-equal "  #+end_tool"))

    (it "indents a `#+begin_src'-shaped line off column 0"
      (expect (gptel-chat--sanitize-chunk "#+begin_src emacs-lisp")
              :to-equal "  #+begin_src emacs-lisp"))

    (it "indents an org-emphasis or list-bullet line like ordinary prose"
      ;; The indenter is unconditional — it does not discriminate by
      ;; line shape.  An `*emphasis*' line and a `- list' bullet are
      ;; body content and take the body indent like any other prose;
      ;; neither is an org structural token at column 0.
      (expect (gptel-chat--sanitize-chunk "*emphasis* in a sentence")
              :to-equal "  *emphasis* in a sentence")
      (expect (gptel-chat--sanitize-chunk "- a list item")
              :to-equal "  - a list item")))

  (describe "leaves blank lines unchanged"

    (it "returns the empty string unchanged (no trailing whitespace)"
      (expect (gptel-chat--sanitize-chunk "")
              :to-equal ""))

    (it "returns a whitespace-only line unchanged"
      ;; A line of only spaces / tabs is blank; the indenter must not
      ;; prepend more whitespace to it.
      (expect (gptel-chat--sanitize-chunk "   ")
              :to-equal "   ")
      (expect (gptel-chat--sanitize-chunk "\t")
              :to-equal "\t")))

  (describe "honours `gptel-chat-content-indentation'"

    (it "uses 1 leading space when the defcustom is 1"
      (let ((gptel-chat-content-indentation 1))
        (expect (gptel-chat--sanitize-chunk "* Heading")
                :to-equal " * Heading")))

    (it "uses 2 leading spaces by default (org-edit-src parity)"
      (let ((gptel-chat-content-indentation 2))
        (expect (gptel-chat--sanitize-chunk "* Heading")
                :to-equal "  * Heading")))

    (it "uses 4 leading spaces when the defcustom is set to 4"
      (let ((gptel-chat-content-indentation 4))
        (expect (gptel-chat--sanitize-chunk "* Heading")
                :to-equal "    * Heading")))

    (it "clamps a 0-width defcustom up to the floor of 1"
      ;; `gptel-chat--body-indent' clamps any value below 1 up to 1 —
      ;; a 0-width indent would not move content off column 0.
      (let ((gptel-chat-content-indentation 0))
        (expect (gptel-chat--sanitize-chunk "* Heading")
                :to-equal " * Heading"))))

  (describe "is NOT idempotent — re-indenting stacks indentation"
    ;; The indenter is unconditional: it prepends the body width to
    ;; every non-blank line, every call.  Idempotence is a property of
    ;; the streaming path (each line is processed exactly once, never
    ;; re-fed), not of the function itself.

    (it "indenting an already-indented line adds another body width"
      (let* ((once  (gptel-chat--sanitize-chunk "* Heading"))
             (twice (gptel-chat--sanitize-chunk once)))
        (expect once  :to-equal "  * Heading")
        (expect twice :to-equal "    * Heading"))))

  (describe "enforces its single-line contract"
    ;; The indenter is a line-level helper; misuse with an embedded
    ;; newline would only indent the prefix up to the first `\n',
    ;; re-introducing the split-chunk bug the holdback exists to
    ;; prevent.  The guard turns that silent misuse into a loud
    ;; failure.  Each negative spec pins the error *message*
    ;; (Buttercup's `:to-throw' compares signal args with `equal').

    (it "signals when LINE contains an embedded newline"
      (expect (gptel-chat--sanitize-chunk "first\nsecond")
              :to-throw 'error '("LINE must not contain newlines")))

    (it "signals when LINE begins with a newline"
      (expect (gptel-chat--sanitize-chunk "\nleading")
              :to-throw 'error '("LINE must not contain newlines")))

    (it "signals when LINE ends with a newline"
      (expect (gptel-chat--sanitize-chunk "trailing\n")
              :to-throw 'error '("LINE must not contain newlines")))

    (it "signals when LINE contains multiple newlines"
      (expect (gptel-chat--sanitize-chunk "a\nb\nc")
              :to-throw 'error '("LINE must not contain newlines")))

    (it "signals when LINE is a bare newline"
      (expect (gptel-chat--sanitize-chunk "\n")
              :to-throw 'error '("LINE must not contain newlines")))))


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
                :to-throw)))

    (it "rejects a marker with insertion-type nil"
      ;; The factory requires an advance marker (insertion-type t) so
      ;; that inserts at the marker push it forward and successive line
      ;; inserts land in order.  Callers that pass a default-type
      ;; marker would otherwise get reversed-order output; the guard
      ;; fails loudly at construction instead.
      (let ((default-type-marker
             (with-current-buffer gptel-chat-stream-test--buffer
               ;; `copy-marker' without the second argument gives
               ;; insertion-type nil — the silent-bug case.
               (copy-marker (point-min)))))
        (expect (marker-insertion-type default-type-marker)
                :to-equal nil)
        (expect (gptel-chat--make-stream-inserter default-type-marker)
                :to-throw)))

    (it "accepts a marker with insertion-type t"
      ;; Positive case: the fixture marker is built with
      ;; `(copy-marker ... t)' so it has insertion-type t and must be
      ;; accepted.  Guards against an over-eager check that rejects
      ;; the valid shape.
      (expect (marker-insertion-type gptel-chat-stream-test--marker)
              :to-equal t)
      (let ((handle (gptel-chat--make-stream-inserter
                     gptel-chat-stream-test--marker)))
        (expect (gptel-chat-stream-p handle) :to-be-truthy))))

  (describe "inserting complete lines in order, each indented"

    (it "inserts two complete lines from a single newline-terminated chunk"
      ;; Each complete line is indented by the body width before
      ;; insertion; delimiter framing is the caller's job.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "hello\nworld\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  hello\n  world\n"))

    (it "preserves order across multiple chunks"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "alpha\n")
        (funcall cb "beta\n")
        (funcall cb "gamma\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  alpha\n  beta\n  gamma\n"))

    (it "leaves blank lines blank between indented content lines"
      ;; A blank line passes through the indenter unchanged — no
      ;; trailing whitespace is introduced.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "first\n\nsecond\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  first\n\n  second\n"))

    (it "flushes a final partial line via the t sentinel without adding a newline"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "trailing partial")
        ;; Before flush, nothing is committed — the partial is still
        ;; in the holdback.
        (expect (gptel-chat-stream-test--buffer-string) :to-equal "")
        (funcall cb t))
      ;; The flushed partial is indented like any other content line.
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  trailing partial")))

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
                :to-equal "  partial line\n"))))

  (describe "indents structural-token lines on whole-line chunks"

    (it "indents `#+end_assistant' when it arrives as its own line"
      ;; A streamed `#+end_assistant' line is body content, not a real
      ;; closer; indenting it keeps the containing block well-formed.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "prose\n#+end_assistant\nmore\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  prose\n  #+end_assistant\n  more\n"))

    (it "indents a `* Heading' line that arrives as its own chunk"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "* Heading\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  * Heading\n"))

    (it "indents a mixed chunk: heading, end-delimiter, and plain prose"
      ;; Each line goes through the per-line indenter; every non-blank
      ;; line lands off column 0 regardless of its shape.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "* Heading\n#+end_assistant\nplain text\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal
              "  * Heading\n  #+end_assistant\n  plain text\n"))

    (it "honours `gptel-chat-content-indentation' when streaming"
      ;; Customising the defcustom changes the streamed indent width.
      (let ((gptel-chat-content-indentation 4))
        (let* ((handle (gptel-chat--make-stream-inserter
                        gptel-chat-stream-test--marker))
               (cb (gptel-chat-stream-insert handle)))
          (funcall cb "* Heading\n")
          (funcall cb t)))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "    * Heading\n")))

  (describe "rejects chunk values that are neither string nor t sentinel"
    ;; Contract (design.md §Decision 10): the insert slot's dispatch
    ;; recognises exactly two chunk shapes — a string chunk or the
    ;; `t' flush sentinel.  Any other value (including nil, a
    ;; symbol, or a cons cell like `(tool-call . _)') is a caller
    ;; bug and must signal loudly so drift from upstream's protocol
    ;; does not silently drop response data.

    (it "signals on a non-string symbol chunk (e.g. 'abort)"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (expect (funcall cb 'abort) :to-throw 'error)))

    (it "signals on nil"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (expect (funcall cb nil) :to-throw 'error)))

    (it "signals on a cons cell (e.g. '(tool-call . _))"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (expect (funcall cb '(tool-call . dummy)) :to-throw 'error))))

  (describe "flush semantics on stream completion"

    (it "flush of an empty holdback is a no-op"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "full line\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  full line\n"))

    (it "flush indents a trailing #+end_assistant partial (no newline)"
      ;; Per the task spec: upstream sends `t' after a newline-terminated
      ;; final chunk (holdback empty) OR a final content chunk with no
      ;; newline (holdback is a single line).  In the latter case the
      ;; single-line flush goes through the same per-line indenter.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "#+end_assistant")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  #+end_assistant")))

  (describe "marker-based insertion is robust to edits above the insertion point"

    (it "user edits above the marker do not corrupt subsequent inserts"
      ;; Decision 3b: insertion uses a *marker*, not an integer
      ;; position, precisely so concurrent user edits above the
      ;; insertion point do not shift where new content lands.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "header\n"))
      ;; Use an advance marker at point-max to model "the end of the
      ;; open assistant block" — a marker with insertion-type t, so
      ;; user prepends above it don't shift the insertion target away
      ;; from the end.
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
        ;; i.e., at the end of the buffer, in order, each indented.
        (expect (gptel-chat-stream-test--buffer-string)
                :to-equal
                "user-edit\nheader\n  response line\n  second\n"))))

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
              :to-equal "  fresh\n"))))


;;; gptel-chat-stream handle API --------------------------------------------
;;
;; These specs exercise the typed `cl-defstruct' surface.  They cover
;; the minimum public shape — predicate + three function slots —
;; needed by `stream-callback' to wire tool-call/tool-result events
;; without `cl-letf' surgery on the captured closure environment.

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
      ;; Both inserted lines are indented by the body width; only the
      ;; pre-existing SENTINEL line is at column 0.
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  assistant-text\nSENTINEL\n  tool-text\n")))

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
              :to-equal
              "  back-to-assistant\nSENTINEL\n  in-tool\n"))

    (it "is a no-op when no tool marker has been set"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (cb (gptel-chat-stream-insert handle))
             (clear-tool (gptel-chat-stream-clear-tool-marker handle)))
        (funcall clear-tool)
        (funcall cb "hello\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "  hello\n"))))


;;; gptel-chat--stream-active-marker ---------------------------------------
;;
;; Direct unit tests for the helper that picks the active insertion
;; target from (INSERTION-MARKER, TOOL-MARKER).  These specs invoke it
;; directly so all three input states (nil tool-marker, live
;; tool-marker, dead tool-marker) are pinned independently of the
;; closure wiring.

(describe "gptel-chat--stream-active-marker"

  (before-each
    (gptel-chat-stream-test--fresh-buffer))

  (after-each
    (gptel-chat-stream-test--cleanup))

  (it "returns insertion-marker when tool-marker is nil"
    (let ((insertion gptel-chat-stream-test--marker))
      (expect (gptel-chat--stream-active-marker insertion nil)
              :to-be insertion)))

  (it "returns tool-marker when it is live"
    (let* ((insertion gptel-chat-stream-test--marker)
           (tool (with-current-buffer gptel-chat-stream-test--buffer
                   (copy-marker (point-max) t))))
      (expect (gptel-chat--stream-active-marker insertion tool)
              :to-be tool)))

  (it "falls back to insertion-marker when tool-marker has been cleared with set-marker nil"
    ;; A marker whose buffer has been cleared via (set-marker M nil)
    ;; is no longer "live" in the helper's sense — (marker-buffer M)
    ;; is nil — so routing must fall back to the assistant marker.
    (let* ((insertion gptel-chat-stream-test--marker)
           (tool (with-current-buffer gptel-chat-stream-test--buffer
                   (copy-marker (point-max) t))))
      (set-marker tool nil)
      (expect (marker-buffer tool) :to-be nil)
      (expect (gptel-chat--stream-active-marker insertion tool)
              :to-be insertion))))


;;; tool-marker routing through the inserter --------------------------------
;;
;; Integration-level specs that exercise tool-marker routing through
;; the public struct surface.  These complement the handle-API specs
;; above by covering the case where the *same* setter lambda is
;; called more than once on the same closure instance (the "reroute"
;; scenario) and by pinning the input-validation contract on a
;; `nil' argument.

(describe "gptel-chat-stream tool-marker routing through the inserter"

  (before-each
    (gptel-chat-stream-test--fresh-buffer))

  (after-each
    (gptel-chat-stream-test--cleanup))

  (describe "reroute: set → insert → set-to-a-different-marker → insert"

    (it "routes the second insert to the new tool marker, not the first"
      ;; Pins that `setq tool-marker …' in the setter lambda works on
      ;; the *second* call of the same closure instance, not just the
      ;; first.  Layout: three distinct anchor points in the buffer,
      ;; separated by sentinels, each with its own advance marker.
      ;; Each insert lands at the marker active at the time of the
      ;; call, indented by the body width.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "S1\nS2\n"))
      (let* ((assistant-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-min) t)))
             (tool-marker-1
              (with-current-buffer gptel-chat-stream-test--buffer
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "S1\n")
                  (copy-marker (point) t))))
             (tool-marker-2
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-max) t)))
             (handle (gptel-chat--make-stream-inserter assistant-marker))
             (cb (gptel-chat-stream-insert handle))
             (set-tool (gptel-chat-stream-set-tool-marker handle)))
        ;; First: route to tool-marker-1 and insert.
        (funcall set-tool tool-marker-1)
        (funcall cb "first-tool\n")
        ;; Second: re-route to tool-marker-2 on the SAME closure and
        ;; insert again.  If the setter's `setq' only took effect
        ;; the first time, this text would land at tool-marker-1.
        (funcall set-tool tool-marker-2)
        (funcall cb "second-tool\n")
        (funcall cb t))
      ;; Expected layout — inserted lines indented, sentinels not:
      ;;   S1\n               ← original sentinel 1
      ;;     first-tool\n     ← inserted at tool-marker-1
      ;;   S2\n               ← original sentinel 2
      ;;     second-tool\n    ← inserted at tool-marker-2
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal "S1\n  first-tool\nS2\n  second-tool\n"))

    (it "after rerouting, clear-tool-marker still returns routing to the assistant marker"
      ;; A follow-on check: once the setter has been called more
      ;; than once, the clearer must still restore the assistant
      ;; marker as the routing target.
      (with-current-buffer gptel-chat-stream-test--buffer
        (insert "S1\n"))
      (let* ((assistant-marker
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-min) t)))
             (tool-marker-1
              (with-current-buffer gptel-chat-stream-test--buffer
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "S1\n")
                  (copy-marker (point) t))))
             (tool-marker-2
              (with-current-buffer gptel-chat-stream-test--buffer
                (copy-marker (point-max) t)))
             (handle (gptel-chat--make-stream-inserter assistant-marker))
             (cb (gptel-chat-stream-insert handle))
             (set-tool (gptel-chat-stream-set-tool-marker handle))
             (clear-tool (gptel-chat-stream-clear-tool-marker handle)))
        (funcall set-tool tool-marker-1)
        (funcall cb "at-tm1\n")
        (funcall set-tool tool-marker-2)
        (funcall cb "at-tm2\n")
        (funcall clear-tool)
        (funcall cb "back-to-assistant\n")
        (funcall cb t))
      (expect (gptel-chat-stream-test--buffer-string)
              :to-equal
              "  back-to-assistant\nS1\n  at-tm1\n  at-tm2\n")))

  (describe "set-tool-marker input validation"

    (it "signals an error when called with nil"
      ;; Contract: clearing the routing override is
      ;; `clear-tool-marker''s job.  Passing nil to the setter is a
      ;; caller bug and must raise.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-stream-test--marker))
             (setter (gptel-chat-stream-set-tool-marker handle)))
        (expect (funcall setter nil) :to-throw)))))


;;; gptel-chat-stream-callback terminal paths -----------------------------
;;
;; These specs exercise the completion / abort / error branches of
;; `gptel-chat-stream-callback' (design.md §Decision 10).  The
;; callback closes the active assistant block, optionally records a
;; visible marker for abort/error, and appends a fresh empty
;; `#+begin_user' / `#+end_user' block with point positioned on the
;; body line inside.
;;
;; Streamed assistant content is indented by the body width; the
;; `#+end_assistant' closer and the appended `#+begin_user' /
;; `#+end_user' delimiters stay at column 0.  The body line of the
;; appended user block is indented to the body width (design.md
;; §Decision 6).
;;
;; Also asserts the bypass of `gptel-post-response-functions' and
;; `gptel-pre-response-hook'.

(describe "gptel-chat-stream-callback terminal paths"

  (let (test-buffer test-marker)

    (cl-labels
        ((fresh-buffer ()
           (setq test-buffer
                 (generate-new-buffer " *gptel-chat-stream-callback-test*"))
           (with-current-buffer test-buffer
             (insert "#+begin_user\nhi\n#+end_user\n#+begin_assistant\n")
             (setq test-marker (copy-marker (point-max) t)))
           test-marker)
         (buffer-string-no-props ()
           (with-current-buffer test-buffer
             (buffer-substring-no-properties (point-min) (point-max))))
         (body-pad ()
           ;; The body-width pad inserted on the appended user block's
           ;; blank line by `gptel-chat--stream-close-assistant'.
           (make-string (gptel-chat--body-indent) ?\s))
         (cleanup ()
           (when (buffer-live-p test-buffer)
             (kill-buffer test-buffer))
           (setq test-buffer nil test-marker nil)))

      (before-each (fresh-buffer))
      (after-each  (cleanup))

      (describe "normal completion (response `t')"

        (it "flushes holdback, closes block, appends a fresh user block"
          ;; Streamed text is indented by the body width; the
          ;; `#+end_assistant' closer and the appended user-block
          ;; delimiters stay at column 0; the new user block's body
          ;; line carries the body indent.
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb "Hello.\n" nil)
            (funcall cb t nil))
          (expect (buffer-string-no-props)
                  :to-equal
                  (concat "#+begin_user\nhi\n#+end_user\n"
                          "#+begin_assistant\n"
                          "  Hello.\n"
                          "#+end_assistant\n"
                          "\n#+begin_user\n" (body-pad) "\n#+end_user\n")))

        (it "flushes a trailing partial line before closing"
          ;; Upstream may send the completion `t' after a final chunk
          ;; that has no trailing newline.  The holdback-aware inserter
          ;; flushes that partial (indented) without adding a newline;
          ;; the callback then adds one before `#+end_assistant'.
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb "trailing partial" nil)
            (funcall cb t nil))
          (expect (buffer-string-no-props)
                  :to-equal
                  (concat "#+begin_user\nhi\n#+end_user\n"
                          "#+begin_assistant\n"
                          "  trailing partial\n"
                          "#+end_assistant\n"
                          "\n#+begin_user\n" (body-pad) "\n#+end_user\n")))

        (it "positions point on the body line inside the new user block"
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb "Done.\n" nil)
            (funcall cb t nil))
          ;; Point should be on the body line inside the appended
          ;; `#+begin_user' block.  `gptel-chat--stream-close-assistant'
          ;; runs `indent-line-to' on that line, which leaves point at
          ;; the end of the body-width indentation (past the spaces,
          ;; before the line's `\n' terminator).
          (with-current-buffer test-buffer
            (expect (current-column)
                    :to-equal (gptel-chat--body-indent))
            ;; The body line's content is exactly the body-width pad.
            (expect (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))
                    :to-equal (body-pad))
            ;; Backward-looking sanity: the line immediately before
            ;; point is `#+begin_user' at column 0.
            (forward-line -1)
            (expect (buffer-substring-no-properties
                     (point) (line-end-position))
                    :to-equal "#+begin_user"))))

      (describe "user abort (response `abort')"

        (it "closes block with interruption marker and appends a user block"
          ;; Spec §"Stream abort": the assistant block is closed with
          ;; `#+end_assistant' and a visible marker records that the
          ;; response was interrupted.  The marker line is inserted
          ;; verbatim by `gptel-chat--stream-close-assistant' (not
          ;; through the per-line indenter).
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb "Partial before abort.\n" nil)
            (funcall cb 'abort nil))
          (expect (buffer-string-no-props)
                  :to-equal
                  (concat "#+begin_user\nhi\n#+end_user\n"
                          "#+begin_assistant\n"
                          "  Partial before abort.\n"
                          gptel-chat--stream-abort-marker "\n"
                          "#+end_assistant\n"
                          "\n#+begin_user\n" (body-pad) "\n#+end_user\n")))

        (it "closes cleanly when no prior content has been streamed"
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb 'abort nil))
          (expect (buffer-string-no-props)
                  :to-equal
                  (concat "#+begin_user\nhi\n#+end_user\n"
                          "#+begin_assistant\n"
                          gptel-chat--stream-abort-marker "\n"
                          "#+end_assistant\n"
                          "\n#+begin_user\n" (body-pad) "\n#+end_user\n"))))

      (describe "network / API error (response `nil')"

        (it "closes block with error marker and appends a user block"
          (let ((cb (gptel-chat-stream-callback test-marker)))
            (funcall cb "Some content.\n" nil)
            (funcall cb nil nil))
          (expect (buffer-string-no-props)
                  :to-equal
                  (concat "#+begin_user\nhi\n#+end_user\n"
                          "#+begin_assistant\n"
                          "  Some content.\n"
                          gptel-chat--stream-error-marker "\n"
                          "#+end_assistant\n"
                          "\n#+begin_user\n" (body-pad) "\n#+end_user\n"))))

      (describe "upstream response hooks are bypassed"

        (it "does NOT invoke `gptel-post-response-functions' on completion"
          ;; Decision 10: those hooks assume gptel-mode's
          ;; prompt/response-prefix conventions.  Chat-mode does not
          ;; use them; the callback must not run them.
          (let ((gptel-post-response-functions nil)
                (call-count 0))
            (add-hook 'gptel-post-response-functions
                      (lambda (&rest _) (cl-incf call-count)))
            (let ((cb (gptel-chat-stream-callback test-marker)))
              (funcall cb "text\n" nil)
              (funcall cb t nil))
            (expect call-count :to-equal 0)))

        (it "does NOT invoke `gptel-pre-response-hook' on any path"
          (let ((gptel-pre-response-hook nil)
                (call-count 0))
            (add-hook 'gptel-pre-response-hook
                      (lambda (&rest _) (cl-incf call-count)))
            (let ((cb (gptel-chat-stream-callback test-marker)))
              (funcall cb "text\n" nil)
              (funcall cb t nil))
            (expect call-count :to-equal 0)))

        (it "does NOT invoke `gptel-post-response-functions' on abort"
          (let ((gptel-post-response-functions nil)
                (call-count 0))
            (add-hook 'gptel-post-response-functions
                      (lambda (&rest _) (cl-incf call-count)))
            (let ((cb (gptel-chat-stream-callback test-marker)))
              (funcall cb 'abort nil))
            (expect call-count :to-equal 0)))

        (it "does NOT invoke `gptel-post-response-functions' on error"
          (let ((gptel-post-response-functions nil)
                (call-count 0))
            (add-hook 'gptel-post-response-functions
                      (lambda (&rest _) (cl-incf call-count)))
            (let ((cb (gptel-chat-stream-callback test-marker)))
              (funcall cb nil nil))
            (expect call-count :to-equal 0)))))))


(provide 'streaming-spec)

;;; streaming-spec.el ends here
