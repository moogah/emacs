;;; stream.el --- GPTEL Chat-Mode Streaming -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Implements the per-chunk text hygiene layer for `gptel-chat-mode':
;; line-level delimiter-collision sanitization and a per-request
;; closure that line-buffers incoming chunks.  The protocol-level
;; callback that invokes this closure is wired in by the
;; `stream-callback' task.

;;; Code:

(require 'cl-lib)

;; Line-level sanitizer

;; =gptel-chat--sanitize-chunk= is intentionally a *line-level* helper —
;; it takes a complete line (no embedded =\n=) and returns either the
;; input unchanged or a version with a single leading =,= prepended.

;; Match rule (case-insensitive, to mirror org's own delimiter matching):

;; : ^#\+end_\(user\|assistant\|tool\)\b

;; Only those three forms are escaped. Lines like =#+end_src=,
;; =#+begin_assistant=, or prose beginning with a colon pass through
;; untouched. Design.md §Decision 4 covers the reasoning: we deliberately
;; do **not** use =org-escape-code-in-string= because its broader escape
;; (headings, all block delimiters) clutters assistant content with
;; spurious commas on benign lines.

;; Callers that receive multi-line chunks must split first — the
;; streaming closure below does that via a holdback.


;; [[file:stream.org::*Line-level sanitizer][Line-level sanitizer:1]]
(defconst gptel-chat--end-delimiter-regexp
  "^#\\+end_\\(user\\|assistant\\|tool\\)\\b"
  "Regexp matching the three chat-mode block-closing delimiters.
Case-insensitive via `case-fold-search'.  Used by
`gptel-chat--sanitize-chunk' to identify lines that would prematurely
close a containing block if inserted verbatim.")

(defun gptel-chat--sanitize-chunk (line)
  "Return LINE with `,' prepended if it would close a chat-mode block.
LINE must be a complete single line with no embedded newline.  A line
matches when it begins with `#+end_user', `#+end_assistant', or
`#+end_tool' (case-insensitive).  All other lines — including
`#+end_src', `#+begin_assistant', and ordinary prose — pass through
unchanged.

Callers with multi-line input must split on newlines first and call
this function per line; `gptel-chat--make-stream-closure' does that
via a one-line holdback."
  (let ((case-fold-search t))
    (if (string-match-p gptel-chat--end-delimiter-regexp line)
        (concat "," line)
      line)))
;; Line-level sanitizer:1 ends here

;; The =gptel-chat-stream= struct

;; The factory returns a =cl-defstruct= record. =cl-defstruct=
;; auto-generates =make-gptel-chat-stream=, =gptel-chat-stream-p=, and
;; per-slot accessors =gptel-chat-stream-insert=,
;; =gptel-chat-stream-set-tool-marker=,
;; =gptel-chat-stream-clear-tool-marker=. Callers invoke operations by
;; funcalling an accessor:

;; : (funcall (gptel-chat-stream-insert handle) "chunk\n")
;; : (funcall (gptel-chat-stream-set-tool-marker handle) tool-marker)
;; : (funcall (gptel-chat-stream-clear-tool-marker handle))


;; [[file:stream.org::*The =gptel-chat-stream= struct][The =gptel-chat-stream= struct:1]]
(cl-defstruct (gptel-chat-stream (:copier nil))
  "Per-send stream handle for `gptel-chat-mode' response streaming.
Created by `gptel-chat--make-stream-closure'.  Text-processing state
(insertion marker, line holdback, current tool-marker value) lives
inside the closures bound to the function slots; only the routing
*operations* are exposed here.

Slots:

- INSERT: function of one argument.  Pass a string chunk for normal
  text streaming or the sentinel `t' to flush any trailing
  holdback on stream completion.  See
  `gptel-chat--make-stream-closure' for semantics.
- SET-TOOL-MARKER: function of one argument (a live marker).
  After calling, subsequent INSERT calls route text to the given
  marker instead of the assistant insertion marker.  Intended to
  be called on upstream `tool-call' events.
- CLEAR-TOOL-MARKER: zero-argument function.  Clears the routing
  override so subsequent INSERT calls route back to the assistant
  insertion marker.  Intended to be called on `tool-result'
  events."
  insert
  set-tool-marker
  clear-tool-marker)
;; The =gptel-chat-stream= struct:1 ends here

;; Helper: pick the active insertion target

;; Factored out for testability and for the =tool-marker= semantics to
;; be visible on their own.


;; [[file:stream.org::*Helper: pick the active insertion target][Helper: pick the active insertion target:1]]
(defun gptel-chat--stream-active-marker (insertion-marker tool-marker)
  "Return the marker that receives the next insert.
Prefer TOOL-MARKER when it is a live marker pointing into a buffer;
otherwise return INSERTION-MARKER.  A marker is considered live when
it is a marker object with a non-nil buffer."
  (if (and (markerp tool-marker)
           (marker-buffer tool-marker))
      tool-marker
    insertion-marker))
;; Helper: pick the active insertion target:1 ends here

;; Helper: insert one sanitized line at a marker

;; Single-statement insert so the closure body stays small and the line
;; contract (sanitize, then append =\n=) is explicit.


;; [[file:stream.org::*Helper: insert one sanitized line at a marker][Helper: insert one sanitized line at a marker:1]]
(defun gptel-chat--stream-insert-line (marker line)
  "Insert sanitized LINE followed by a newline at MARKER.
LINE is a single complete line with no embedded newline.  MARKER
must have insertion-type t (an \"advance\" marker) so that
inserting at it pushes the marker forward and subsequent inserts
append after it; the factory `gptel-chat--make-stream-closure'
enforces that contract at construction."
  (let ((sanitized (gptel-chat--sanitize-chunk line)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (insert sanitized "\n")))))
;; Helper: insert one sanitized line at a marker:1 ends here

;; Helper: insert the holdback flush (no trailing newline)

;; The flush path is asymmetric with the per-line path: on stream
;; completion we want the final content but not an extra newline — the
;; caller appends =#+end_assistant= on its own line. Factored out so the
;; asymmetry is explicit rather than living inside an =if= in the main
;; closure.


;; [[file:stream.org::*Helper: insert the holdback flush (no trailing newline)][Helper: insert the holdback flush (no trailing newline):1]]
(defun gptel-chat--stream-insert-flush (marker line)
  "Insert sanitized LINE at MARKER without a trailing newline.
Used for the stream-completion holdback flush.  If LINE is empty,
do nothing."
  (unless (string-empty-p line)
    (let ((sanitized (gptel-chat--sanitize-chunk line)))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (insert sanitized))))))
;; Helper: insert the holdback flush (no trailing newline):1 ends here

;; The closure factory


;; [[file:stream.org::*The closure factory][The closure factory:1]]
(defun gptel-chat--make-stream-closure (insertion-marker)
  "Return a `gptel-chat-stream' handle for streaming assistant text.
INSERTION-MARKER must be a live Emacs marker pointing inside the
buffer that should receive assistant output, and it must have
insertion-type t (an \"advance\" marker — see
`set-marker-insertion-type').  With insertion-type nil (the
default for `make-marker' and `copy-marker' without the second
argument), the marker stays put when text is inserted at its
position, so successive line inserts would land in reverse
order.  The factory signals an error rather than silently
coercing the shape, so callers that construct the wrong kind of
marker fail loudly at construction rather than producing
garbled buffer output downstream.

Three slots of per-request state are captured inside the closures
bound to the returned struct's function slots:

- INSERTION-MARKER (the argument) — where assistant text goes by
  default.
- HOLDBACK — string carry-over for a trailing partial line.
  Internal; never exposed on the struct.
- TOOL-MARKER — a marker (initially nil) used for nested tool-block
  inserts.  When set to a live marker, the insert closure writes
  there instead of INSERTION-MARKER.  Callers mutate this slot
  indirectly via the struct's SET-TOOL-MARKER and
  CLEAR-TOOL-MARKER accessors — the raw value is never exposed.

The returned struct exposes three operations:

- INSERT (funcalled with one argument):

  - a STRING chunk: concatenate with HOLDBACK, split at `\\n',
    sanitize each complete line via `gptel-chat--sanitize-chunk',
    insert each at the active marker followed by `\\n'.  The
    trailing partial becomes the new HOLDBACK.
  - t: flush.  Insert the sanitized HOLDBACK (if any) at the
    active marker with no trailing newline, then clear HOLDBACK.

  Returns nil.

- SET-TOOL-MARKER (funcalled with one argument, a live marker):
  set TOOL-MARKER to the given marker.  Subsequent INSERT calls
  route text to that marker instead of INSERTION-MARKER.
  Intended to be called by `stream-callback' on `tool-call'
  events.  Signals an error if passed a non-marker or a marker
  with no buffer.

- CLEAR-TOOL-MARKER (funcalled with zero arguments): reset
  TOOL-MARKER to nil so subsequent INSERT calls route back to
  INSERTION-MARKER.  Intended to be called on `tool-result'
  events.

Per design.md §Decision 3b, packaging per-send state as a
cl-struct handle (rather than as a bare lambda or a caller-owned
mutable cell) keeps the text-processing state scoped to the send
while giving callers a typed, external API for toggling tool
routing — no `cl-letf' surgery on captured variables."
  (unless (and (markerp insertion-marker)
               (marker-buffer insertion-marker))
    (error "INSERTION-MARKER must be a live marker"))
  (unless (marker-insertion-type insertion-marker)
    (error "INSERTION-MARKER must have insertion-type t \
(advance marker); got insertion-type nil"))
  (let ((holdback "")
        (tool-marker nil))
    (make-gptel-chat-stream
     :insert
     (lambda (chunk)
       (cond
        ;; Flush sentinel: insert any remaining holdback without a
        ;; trailing newline and clear the holdback.
        ((eq chunk t)
         (let ((active (gptel-chat--stream-active-marker
                        insertion-marker tool-marker)))
           (gptel-chat--stream-insert-flush active holdback))
         (setq holdback "")
         nil)
        ;; Text chunk: prepend holdback, split at newlines, complete
        ;; lines go through the sanitizer + insert; trailing partial
        ;; becomes the new holdback.
        ((stringp chunk)
         (let* ((combined (concat holdback chunk))
                (parts (split-string combined "\n"))
                (new-holdback (car (last parts)))
                (complete-lines (butlast parts))
                (active (gptel-chat--stream-active-marker
                         insertion-marker tool-marker)))
           (dolist (line complete-lines)
             (gptel-chat--stream-insert-line active line))
           (setq holdback (or new-holdback "")))
         nil)
        ;; Anything else is a caller bug; surface it loudly rather
        ;; than silently drop.
        (t
         (error "Unexpected chunk %S" chunk))))
     :set-tool-marker
     (lambda (marker)
       (unless (and (markerp marker) (marker-buffer marker))
         (error "set-tool-marker: MARKER must be a live marker"))
       (setq tool-marker marker)
       nil)
     :clear-tool-marker
     (lambda ()
       (setq tool-marker nil)
       nil))))
;; The closure factory:1 ends here

;; Provide


;; [[file:stream.org::*Provide][Provide:1]]
;; TODO(gptel-chat-mode): wire this closure into a
;; `gptel-chat--install-stream-callback' that dispatches on
;; gptel-request response shapes (string / tool-call / tool-result /
;; t / nil / 'abort) per design.md §Decision 10.  Owned by the
;; `stream-callback' task.

(provide 'gptel-chat-stream)

;;; stream.el ends here
;; Provide:1 ends here
