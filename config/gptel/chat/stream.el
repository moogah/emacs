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

;; `gptel-chat--body-indent' (the body indent width) and the
;; `gptel-chat-content-indentation' defcustom it reads are owned by
;; `gptel-chat-mode' (config/gptel/chat/mode.el) — see chat.org for
;; load order.  Production loads `mode.el' before `stream.el'.  The
;; accessor reads the defcustom through `bound-and-true-p' with a
;; fallback of 2, so `gptel-chat--sanitize-chunk' is safe to call
;; even when the variable is unbound.
;;
;; `gptel-chat--indenting' is `gptel-chat-mode''s re-entry guard for
;; its `after-change' paste recorder; the streaming callback binds it
;; so streamed inserts — already indented by
;; `gptel-chat--sanitize-chunk' — are not recorded as pasted regions.
;; The bare `defvar' declares it special here so the `let' binding is
;; dynamic regardless of load or compile order.  These `defvar' /
;; `declare-function' forms also silence byte-compiler warnings.
(defvar gptel-chat-content-indentation)
(defvar gptel-chat--indenting)
(declare-function gptel-chat--body-indent "gptel-chat-mode" ())

;; Line-level body indenter

;; =gptel-chat--sanitize-chunk= is a *line-level* helper — it takes a
;; complete line (no embedded =\n=) and returns it indented by the
;; chat-block body width, so streamed assistant content lands inside the
;; indented chat-block body rather than at column 0.

;; Indenting every streamed line off column 0 is what keeps org's
;; structural scanners — the heading regex =^\*+ =, the special-block
;; closers =^#\+end_…=, drawers, keywords — from reading assistant
;; output as document structure. It supersedes the two per-token escapes
;; of the earlier design (the =,#+end_*= delimiter escape and the =*=
;; heading escape): a uniformly indented body neutralises *every*
;; column-0 collision at once (=gptel-chat-heading-scoping= design.md
;; Decisions 1, 4).

;; A blank line (empty or whitespace-only) is returned unchanged — we do
;; not introduce trailing whitespace. The send-path =gptel-chat--dedent=
;; strips the common indentation back off before the content reaches the
;; model; the round-trip is robust against changes to
;; =gptel-chat-content-indentation= because the dedent measures rather
;; than assumes (design.md Decision 3).

;; Callers that receive multi-line chunks must split first — the
;; streaming closure below does that via a holdback.


;; [[file:stream.org::*Line-level body indenter][Line-level body indenter:1]]
(defun gptel-chat--sanitize-chunk (line)
  "Return LINE indented by the chat-block body indent.
LINE must be a complete single line with no embedded newline.  A
non-blank LINE is returned with `gptel-chat--body-indent' leading
spaces prepended, so streamed assistant content sits inside the
indented chat-block body — off column 0, where org's structural
scanners (the heading regex, the special-block closers, drawers,
keywords) are anchored.  A blank LINE (empty or whitespace-only) is
returned unchanged so no trailing whitespace is introduced.

The send-path `gptel-chat--dedent' strips the indentation back off
before the content reaches the model; the round-trip is robust
against changes to `gptel-chat-content-indentation' because the
dedent measures the indentation rather than assuming a width.

`gptel-chat--body-indent' is owned by `gptel-chat-mode'
\(`config/gptel/chat/mode.el'), which loads before this module per
`config/gptel/chat/chat.org'.  It reads `gptel-chat-content-indentation'
through `bound-and-true-p' with a fallback of 2, so this function is
safe to call even when the defcustom has not been customised.

Signals an error with message \"LINE must not contain newlines\" when
LINE contains an embedded `\\n'.  Callers with multi-line input must
split on newlines first and call this function per line;
`gptel-chat--make-stream-inserter' does that via a one-line holdback."
  (when (string-match-p "\n" line)
    (error "LINE must not contain newlines"))
  (if (string-match-p "\\`[ \t]*\\'" line)
      line
    (concat (make-string (gptel-chat--body-indent) ?\s) line)))
;; Line-level body indenter:1 ends here

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
Created by `gptel-chat--make-stream-inserter'.  Text-processing state
(insertion marker, line holdback, current tool-marker value) lives
inside the closures bound to the function slots; only the routing
*operations* are exposed here.

Slots:

- INSERT: function of one argument.  Pass a string chunk for normal
  text streaming or the sentinel `t' to flush any trailing
  holdback on stream completion.  See
  `gptel-chat--make-stream-inserter' for semantics.
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
append after it; the factory `gptel-chat--make-stream-inserter'
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
do nothing.

MARKER must have insertion-type t (an \"advance\" marker — see
`set-marker-insertion-type'); the factory
`gptel-chat--make-stream-inserter' enforces that contract at
construction.  With insertion-type nil a flush at the marker would
leave the marker sitting before the inserted text, so a subsequent
close-block insert from the callback would land before rather than
after the flushed content."
  (unless (string-empty-p line)
    (let ((sanitized (gptel-chat--sanitize-chunk line)))
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (insert sanitized))))))
;; Helper: insert the holdback flush (no trailing newline):1 ends here

;; The inserter factory


;; [[file:stream.org::*The inserter factory][The inserter factory:1]]
(defun gptel-chat--make-stream-inserter (insertion-marker)
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
;; The inserter factory:1 ends here

;; Helper: format a tool-block header

;; The header contract is =#+begin_tool (<name> <plist...>)= (see
;; design.md Decision 10 and =gptel-chat--parse-tool-header=): a single
;; sexp whose =car= is the tool-name symbol and whose =cdr= is the
;; model-supplied arguments plist itself (no wrapper key). The parser
;; returns =(cdr parsed)= verbatim, so the buffer text round-trips
;; into the same plist downstream consumers (=segment-to-messages=,
;; =gptel--parse-list=) feed to the model as JSON arguments.


;; [[file:stream.org::*Helper: format a tool-block header][Helper: format a tool-block header:1]]
(defun gptel-chat--stream-format-tool-header (name args)
  "Format a `#+begin_tool' header line.
NAME is a string — the tool name extracted via `gptel-tool-name'
from the TOOL-STRUCT upstream passes in the tool-call 3-list.
ARGS is a plist (model-supplied tool arguments) emitted as the
plist tail of the header sexp via `prin1-to-string', so the
parser's `read-from-string' round-trips it intact and the
parser's `:args' segment field IS the model's args (no
unwrapping needed downstream).  Returns the full header line
without a trailing newline."
  (format "#+begin_tool %s"
          (prin1-to-string (cons (intern name) args))))
;; Helper: format a tool-block header:1 ends here

;; Helper: insert a sanitized multi-line block at a marker

;; Tool results arrive as a single string (not a stream), so they do
;; not go through the holdback-aware inserter. Each line is individually
;; indented by =gptel-chat--sanitize-chunk= so the result sits inside
;; the indented tool body — a result line that itself reads as
;; =#+end_tool= or an org heading is moved off column 0 and is inert.


;; [[file:stream.org::*Helper: insert a sanitized multi-line block at a marker][Helper: insert a sanitized multi-line block at a marker:1]]
(defun gptel-chat--stream-insert-sanitized-block (marker text)
  "Insert TEXT at MARKER with per-line body indentation.
TEXT may contain embedded newlines; each line is individually
indented by `gptel-chat--sanitize-chunk', then reassembled with
its original line separators preserved.  The inserted payload
always ends in exactly one newline so a following closer delimiter
lands on its own line:

- TEXT nil or empty  → a single newline (empty body line).
- TEXT ends in `\\n'  → preserved as-is (already line-terminated).
- TEXT lacks `\\n'    → a trailing newline is appended.

Rationale: tool results may be arbitrarily shaped (with or without
a trailing newline), and the calling layout relies on exactly one
`\\n' between the last content line and the closer delimiter."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (cond
       ((or (null text) (string-empty-p text))
        (insert "\n"))
       (t
        (let* ((lines (split-string text "\n"))
               ;; If TEXT ends in `\\n' the split produces a trailing
               ;; empty element; dropping it avoids emitting a spurious
               ;; blank line, then the explicit terminator below gives
               ;; us exactly one trailing newline regardless of input
               ;; shape.
               (effective (if (and (> (length lines) 1)
                                   (string-empty-p
                                    (car (last lines))))
                              (butlast lines)
                            lines))
               (sanitized (mapconcat #'gptel-chat--sanitize-chunk
                                     effective "\n")))
          (insert sanitized "\n")))))))
;; Helper: insert a sanitized multi-line block at a marker:1 ends here

;; Helper: open a tool block and return its result marker

;; Inserts both delimiters (=#+begin_tool ...= and =#+end_tool=)
;; immediately with an empty body, and returns an advance marker
;; positioned on the blank body line between them. Opening-with-closer
;; matters for multi-call events: if the opener inserted only the
;; header and left the close to =gptel-chat--stream-close-tool-block=,
;; the first call's result marker and the assistant =insertion-marker=
;; would sit at the same buffer position, and inserting the second
;; call's header would push both markers forward together — corrupting
;; the FIFO. Closing both delimiters up-front separates the two markers
;; by the =#+end_tool= line, so subsequent work at =insertion-marker=
;; does not disturb prior result markers.

;; The implementation performs the =#+end_tool= insert BEFORE
;; building the result-marker so the result-marker does not ride the
;; `#+end_tool= insertion forward. We record the body position
;; (an integer from =(point)=) after writing the header, then insert
;; the closer (which advances =insertion-marker= past the whole
;; block), then construct a fresh advance marker at the recorded
;; integer position. The result-marker ends up between the two
;; delimiters while =insertion-marker= ends up past them — so
;; subsequent inserts at =insertion-marker= (a second sibling tool
;; block, or streamed assistant text) do not disturb prior tool
;; markers.


;; [[file:stream.org::*Helper: open a tool block and return its result marker][Helper: open a tool block and return its result marker:1]]
(defun gptel-chat--stream-open-tool-block (insertion-marker tool-spec args)
  "Open a nested `#+begin_tool' block at INSERTION-MARKER.
TOOL-SPEC is a `gptel-tool' struct (upstream's
`gptel-request.el:1308' cl-defstruct) whose `name' slot is
extracted via `gptel-tool-name'.  ARGS is the plist (or list)
of model-supplied tool arguments carried as the second element
of each tool-call / tool-result 3-list
\(see `gptel-request.el:1812-1827' for upstream's contract\).

Inserts both `#+begin_tool ...' and matching `#+end_tool'
delimiters with an empty body, and returns an advance marker
\(insertion-type t\) positioned on the empty body line between
them.  The caller feeds the result text into that marker via
`gptel-chat--stream-close-tool-block'; the closing delimiter is
already in place.

Marker construction is ordered so the result-marker is not
co-located with INSERTION-MARKER at any moment when a shared-position
insert happens — see the §\"Helper: open a tool block\" discussion
above."
  (let* ((name (or (and tool-spec (gptel-tool-name tool-spec)) ""))
         (header (gptel-chat--stream-format-tool-header name args))
         (buf (marker-buffer insertion-marker)))
    (with-current-buffer buf
      (save-excursion
        (goto-char insertion-marker)
        (insert header "\n")
        ;; Record the integer position of the result-body line,
        ;; then insert the closer.  Because `body-pos' is a plain
        ;; integer (not a marker), the `#+end_tool' insert does
        ;; not carry it forward; `insertion-marker' (insertion-type
        ;; t) advances past the closer while `body-pos' continues
        ;; to refer to the body line.
        (let ((body-pos (point)))
          (insert "#+end_tool\n")
          (copy-marker body-pos t))))))
;; Helper: open a tool block and return its result marker:1 ends here

;; Helper: close a tool block at its result marker

;; On =tool-result=, insert the stringified result at the result-target
;; marker. The =#+end_tool= closer is already in the buffer (inserted
;; by =gptel-chat--stream-open-tool-block=); this helper only fills in
;; the body.


;; [[file:stream.org::*Helper: close a tool block at its result marker][Helper: close a tool block at its result marker:1]]
(defun gptel-chat--stream-close-tool-block (tool-marker result)
  "Insert RESULT at TOOL-MARKER; the `#+end_tool' closer is pre-placed.
RESULT is the third element of upstream's tool-result 3-list
\(see `gptel-request.el:1812-1827'\): typically a string returned
by the tool function, but tool authors may return nil or a
non-string sexp.  A nil RESULT renders as an empty body line; a
non-string sexp is rendered via `prin1-to-string' so the block is
well-formed regardless.  Each line of the rendered form is
sanitized via `gptel-chat--sanitize-chunk' so a result that itself
contains `#+end_tool' or `#+end_assistant' cannot prematurely close
a containing block."
  (let ((text (cond
               ((null result) "")
               ((stringp result) result)
               (t (prin1-to-string result)))))
    (gptel-chat--stream-insert-sanitized-block tool-marker text)))
;; Helper: close a tool block at its result marker:1 ends here

;; Helper: close the assistant block and append a fresh user block

;; Three terminal paths (completion, error, abort) share the same
;; structural operation: ensure the buffer text ends with a newline,
;; append =#+end_assistant= on its own line, then append a fresh empty
;; =#+begin_user= / =#+end_user= block with point positioned on the
;; blank line inside (Decision 8's shell-like append flow). For the
;; error and abort paths, a visible marker line is prepended before the
;; closing delimiter.

;; Completion and error/abort share the closing-and-append sequence
;; so the buffer always ends in a well-formed user-block pending the
;; next send.


;; [[file:stream.org::*Helper: close the assistant block and append a fresh user block][Helper: close the assistant block and append a fresh user block:1]]
(defun gptel-chat--stream-close-assistant (insertion-marker
                                           &optional marker-text)
  "Close the open assistant block at INSERTION-MARKER.
If MARKER-TEXT is non-nil, insert it as a line before
`#+end_assistant'; used by the error and abort paths to record a
visible interruption marker.  Then append a fresh empty
`#+begin_user' / `#+end_user' block and move point to the blank
line inside it (Decision 8).  Always leaves the buffer in a
well-formed state: one closed assistant block followed by one open
(empty) user block."
  (let ((buf (marker-buffer insertion-marker)))
    (with-current-buffer buf
      (save-excursion
        (goto-char insertion-marker)
        ;; Ensure we're at the start of a line before inserting the
        ;; closing delimiter.  If the previous insert left point
        ;; mid-line (e.g., a holdback flush with no trailing newline),
        ;; start a new line first.
        (unless (bolp) (insert "\n"))
        (when marker-text
          (insert marker-text "\n"))
        (insert "#+end_assistant\n\n#+begin_user\n\n#+end_user\n"))
      ;; Position point on the body line inside the new user block.
      ;; The insertion-marker (insertion-type t) has advanced past
      ;; everything we just inserted, so search backward from it for
      ;; the `#+end_user' line and step one line up.  Indent the
      ;; (empty) body line to the body width so the next human turn
      ;; starts off column 0, consistent with the indented region.
      (goto-char insertion-marker)
      (when (search-backward "#+end_user" nil t)
        (forward-line -1)
        (indent-line-to (gptel-chat--body-indent))))))
;; Helper: close the assistant block and append a fresh user block:1 ends here

;; Constants: visible error and abort markers

;; Shared constants so test assertions can reference them without
;; hard-coding literals across files. Plain org comment lines keep the
;; parser happy (comment lines are ignored when walking an assistant
;; body).


;; [[file:stream.org::*Constants: visible error and abort markers][Constants: visible error and abort markers:1]]
(defconst gptel-chat--stream-error-marker
  "# gptel-chat: request failed"
  "Visible marker line inserted at the end of an assistant block on
error (network failure / `nil' response).  See
`gptel-chat-stream-callback'.")

(defconst gptel-chat--stream-abort-marker
  "# gptel-chat: interrupted"
  "Visible marker line inserted at the end of an assistant block on
user abort (`M-x gptel-abort').  See `gptel-chat-stream-callback'.")
;; Constants: visible error and abort markers:1 ends here

;; The callback factory

;; =gptel-chat-stream-callback= is the entry point. Given an advance
;; marker at the end of the open =#+begin_assistant= block, it returns
;; the =:callback= closure for =gptel-request=. The closure ignores
;; its second argument (=info=) except to extract no currently-used
;; context; all per-send state is captured in the closure.

;; Design invariants:

;; 1. The callback's =pcase= dispatcher is the single integration point
;;    with upstream's callback protocol (design.md §Decision 10). If a
;;    future upstream version adds a new response shape, the fix is a
;;    one-file edit here.
;; 2. The callback never calls =gptel--fsm-transition= — state
;;    transitions are driven by the chained upstream handlers
;;    (=gptel--handle-wait=, =gptel--handle-tool-use=). See
;;    =config/gptel/chat/send.org= for the handler chain.
;; 3. The callback never invokes =gptel-post-response-functions= or
;;    =gptel-pre-response-hook=: those hooks assume gptel-mode's
;;    prompt/response-prefix conventions that chat-mode deliberately
;;    does not use (design.md §Decision 10 paragraph "Upstream response
;;    hooks are intentionally bypassed").


;; [[file:stream.org::*The callback factory][The callback factory:1]]
(defun gptel-chat-stream-callback (insertion-marker)
  "Return a `gptel-request' :callback closure for INSERTION-MARKER.
INSERTION-MARKER is an advance marker (insertion-type t) at the
end of an open `#+begin_assistant' block; the returned closure
streams text, renders tool blocks, and closes the block on
completion / error / abort per design.md §Decision 10.

The returned closure has signature `(RESPONSE INFO)' matching
upstream's `gptel-request' :callback contract.  INFO is consulted
on the `t' (HTTP success) arm only: upstream fires `t' once per
HTTP round-trip, so during a multi-round tool-use turn we must
keep the assistant block open until the FINAL `t' (the one with
`:tool-use' unset) so tool results and subsequent request text
land inside the same block.  See `persistent-agent.org' for the
canonical pattern.

State captured in the closure:

- INSERTION-MARKER (the argument): where assistant-level text goes.
- STREAM-HANDLE: the `gptel-chat-stream' built by
  `gptel-chat--make-stream-inserter' — carries line-holdback state
  and the tool-marker routing override.
- PENDING-TOOL-MARKERS: FIFO list of result-target markers, one
  per open tool block awaiting its result.  Pushed on `tool-call'
  events (the `:confirm' path in `gptel--handle-tool-use').  On
  `tool-result', the FIFO is popped first; if empty (the auto-
  approved path, where upstream fires no prior `tool-call'), the
  block is synthesized on the fly from the 3-list's TOOL-SPEC
  and ARGS."
  (unless (and (markerp insertion-marker)
               (marker-buffer insertion-marker))
    (error "INSERTION-MARKER must be a live marker"))
  (unless (marker-insertion-type insertion-marker)
    (error "INSERTION-MARKER must have insertion-type t \
(advance marker); got insertion-type nil"))
  (let* ((stream-handle (gptel-chat--make-stream-inserter
                         insertion-marker))
         (stream-insert (gptel-chat-stream-insert stream-handle))
         (set-tool (gptel-chat-stream-set-tool-marker stream-handle))
         (clear-tool (gptel-chat-stream-clear-tool-marker stream-handle))
         (pending-tool-markers nil))
    (lambda (response info)
      ;; Bind the mode's `after-change' re-entry guard for the whole
      ;; chunk: every streamed insert below is already indented by
      ;; `gptel-chat--sanitize-chunk', so the paste recorder
      ;; (`gptel-chat--indent-inserted-region') must not record these
      ;; inserts as fresh pasted regions to be re-shifted.
      (let ((gptel-chat--indenting t))
        (pcase response
          ;; Text chunk: line-buffered sanitize + insert at active marker.
          ((pred stringp)
           (funcall stream-insert response))
          ;; Reasoning event: v1 ignores.  A later change may render
          ;; into a `#+begin_reasoning' block (design.md §Decision 10).
          (`(reasoning . ,_)
           nil)
          ;; Tool-call event: open one #+begin_tool block per call,
          ;; push each result-target marker onto the FIFO, route
          ;; subsequent streaming to the LAST opened block, and surface
          ;; the confirmation overlay so the user can accept/reject.
          ;;
          ;; Upstream emits each element of CALLS as a 3-list
          ;; `(TOOL-STRUCT ARGS PROCESS-TOOL-RESULT)' — see
          ;; `gptel-request.el:1684-1752' (the `pending-calls' branch)
          ;; and `gptel.el:1801'.  PROCESS-TOOL-RESULT is the continuation
          ;; that fires `(tool-result . ...)' back through this callback
          ;; when invoked.  We hand the full CALLS list to
          ;; `gptel-chat--display-tool-confirm', which stores it on a
          ;; `gptel-tool'-tagged overlay so the accept/reject keymap
          ;; commands can recover it via `get-char-property-and-overlay'.
          ;;
          ;; The auto-approved path NEVER reaches here — upstream's
          ;; `gptel--handle-tool-use' runs auto-approved tools inline and
          ;; emits only `(tool-result . ...)' for them.  So firing the
          ;; confirmation UI on every `tool-call' is safe.
          (`(tool-call . ,calls)
           (let ((overlay-start (marker-position insertion-marker))
                 last-marker)
             (pcase-dolist (`(,tool-spec ,args ,_cb) calls)
               (let ((m (gptel-chat--stream-open-tool-block
                         insertion-marker tool-spec args)))
                 (setq pending-tool-markers
                       (append pending-tool-markers (list m))
                       last-marker m)))
             (when last-marker
               (funcall set-tool last-marker))
             ;; Surface the confirmation overlay over the empty tool
             ;; blocks we just rendered.  Soft `fboundp' guard keeps
             ;; the callback safe during isolated tests that load
             ;; `gptel-chat-stream' without `gptel-chat-tool-confirm';
             ;; in normal init the loader pulls in tool-confirm first.
             (when (fboundp 'gptel-chat--display-tool-confirm)
               (gptel-chat--display-tool-confirm
                calls overlay-start insertion-marker))))
          ;; Tool-result event: for each result, close the matching
          ;; pending block (confirmation path) or synthesize a fresh
          ;; `#+begin_tool' block (auto-approved path — upstream fires
          ;; no prior `(tool-call . ...)' for tools executed inline
          ;; inside `gptel--handle-tool-use', see
          ;; `gptel-request.el:1732-1747').  After all results are
          ;; handled, clear the tool-marker override so subsequent
          ;; streaming routes back to the assistant-level marker.
          ;;
          ;; Upstream emits each element of RESULTS as a 3-list
          ;; `(TOOL-STRUCT ARGS RESULT)' — see `gptel-request.el:1812-1827'
          ;; and `gptel.el:1855' (`cl-loop for (tool args result) in
          ;; tool-results').  We destructure TOOL-SPEC and ARGS (not
          ;; just RESULT) so the synthesize path has everything it
          ;; needs to render the block header.
          (`(tool-result . ,results)
           (pcase-dolist (`(,tool-spec ,args ,result) results)
             (let ((marker (or (pop pending-tool-markers)
                               (gptel-chat--stream-open-tool-block
                                insertion-marker tool-spec args))))
               (gptel-chat--stream-close-tool-block marker result)))
           (unless pending-tool-markers
             (funcall clear-tool)))
          ;; HTTP success (`t'): upstream fires this after every
          ;; request completes — once per round-trip.  For a
          ;; multi-round tool-use turn the sequence is
          ;;   Request-1 text → `t' (with :tool-use) → tool-call
          ;;   → tool-result → Request-2 text → `t' (no :tool-use).
          ;; If we closed the assistant block on the FIRST `t', the
          ;; subsequent tool-result and Request-2 text would land
          ;; after `#+end_assistant' and corrupt the buffer.
          ;; Gate the close-and-append sequence on the final turn
          ;; (null :tool-use); otherwise only flush holdback and
          ;; leave the block open.  Mirrors persistent-agent's
          ;; `(unless (plist-get info :tool-use) …)' pattern.
          ('t
           (funcall stream-insert t)
           (unless (plist-get info :tool-use)
             (gptel-chat--stream-close-assistant insertion-marker)))
          ;; Error / network failure: flush, close with error marker.
          ('nil
           (funcall stream-insert t)
           (gptel-chat--stream-close-assistant
            insertion-marker gptel-chat--stream-error-marker))
          ;; User abort (M-x gptel-abort): flush, close with
          ;; interruption marker.
          ('abort
           (funcall stream-insert t)
           (gptel-chat--stream-close-assistant
            insertion-marker gptel-chat--stream-abort-marker))
          ;; Any other shape is a caller bug or an unexpected
          ;; upstream shape; surface it loudly so drift from the
          ;; upstream protocol does not silently corrupt the buffer.
          (_
           (error "gptel-chat: unexpected response shape %S" response))))
      nil)))
;; The callback factory:1 ends here

;; Provide


;; [[file:stream.org::*Provide][Provide:1]]
(provide 'gptel-chat-stream)

;;; stream.el ends here
;; Provide:1 ends here
