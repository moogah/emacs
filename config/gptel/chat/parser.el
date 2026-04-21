;;; parser.el --- GPTEL Chat-Mode Buffer Parser -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Walks a gptel-chat-mode buffer and emits an ordered turn list.
;; The parser is a small state-machine over re-search-forward calls;
;; it does not use `org-element-parse-buffer' (design.md Decision 1)
;; and is indifferent to org heading depth (design.md Decision 12).
;;
;; Public entry points:
;;   `gptel-chat--parse-buffer'       — buffer -> turn list
;;   `gptel-chat--turns-to-messages'  — turn list -> `gptel-request' :prompt

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Delimiter regexes

;; All three delimiters are pinned to column 0 (design.md Decision 14).
;; Case-folding matches org's own delimiter recognition — =#+END_ASSISTANT=
;; terminates =#+begin_assistant=.


;; [[file:parser.org::*Delimiter regexes][Delimiter regexes:1]]
(defconst gptel-chat--re-begin-turn
  "^#\\+begin_\\(user\\|assistant\\)\\b"
  "Regex for the start of an outer turn block (column 0).")

(defconst gptel-chat--re-end-user
  "^#\\+end_user\\b"
  "Regex for the close of a user block.")

(defconst gptel-chat--re-end-assistant
  "^#\\+end_assistant\\b"
  "Regex for the close of an assistant block.")

(defconst gptel-chat--re-begin-tool
  "^#\\+begin_tool\\b"
  "Regex for the start of a nested tool block inside an assistant block.")

(defconst gptel-chat--re-end-tool
  "^#\\+end_tool\\b"
  "Regex for the close of a nested tool block.")

(defconst gptel-chat--re-inside-assistant
  (concat
   "\\(" gptel-chat--re-end-assistant  "\\)"  ; 1
   "\\|\\(" gptel-chat--re-begin-tool  "\\)"  ; 2
   "\\|\\(" gptel-chat--re-begin-turn  "\\)") ; 3 (group 4 is the inner kind)
  "Union regex used inside an open assistant block.
Groups:
  1. `#+end_assistant' — normal termination.
  2. `#+begin_tool'    — open a nested tool block.
  3. `#+begin_user' or `#+begin_assistant' — invalid: turn-inside-turn.

User-block bodies use a separate, stricter scanner (only `#+end_user')
because Decision 13 allows literal turn delimiters inside user prose.")
;; Delimiter regexes:1 ends here

;; Line-of position helper

;; Error messages quote the line of the offending delimiter. Using
;; =line-number-at-pos= with the buffer's narrow state keeps the number
;; stable even if the caller narrows the buffer for partial parses.


;; [[file:parser.org::*Line-of position helper][Line-of position helper:1]]
(defun gptel-chat--line-of (pos)
  "Return the 1-based line number of POS in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (line-number-at-pos pos))))
;; Line-of position helper:1 ends here

;; Error signalling

;; Shape the three validation errors through a single helper so callers
;; and tests can compare the message text directly.


;; [[file:parser.org::*Error signalling][Error signalling:1]]
(defun gptel-chat--parse-error (kind line)
  "Signal a `user-error' describing a parse failure.
KIND is a keyword: `unclosed-user', `unclosed-assistant',
`unclosed-tool', `tool-block-outside-assistant', or
`turn-inside-turn'. LINE is the 1-based line number of the offending
delimiter."
  (user-error
   "gptel-chat: %s at line %d"
   (pcase kind
     (`unclosed-user                 "unclosed user block")
     (`unclosed-assistant            "unclosed assistant block")
     (`unclosed-tool                 "unclosed tool block")
     (`tool-block-outside-assistant  "tool-block-outside-assistant")
     (`turn-inside-turn              "turn-inside-turn")
     (_ (format "parse error (%s)" kind)))
   line))
;; Error signalling:1 ends here

;; Marker helpers

;; All turn start/end positions are markers (design rationale in the
;; task body). =make-marker= + =set-marker= pins the marker to the
;; buffer so cross-buffer uses are explicit.


;; [[file:parser.org::*Marker helpers][Marker helpers:1]]
(defun gptel-chat--marker-at (pos &optional buffer)
  "Return a fresh marker pointing at POS in BUFFER (default current)."
  (let ((m (make-marker)))
    (set-marker m pos (or buffer (current-buffer)))
    m))
;; Marker helpers:1 ends here

;; Tool-block parsing

;; Inside an open assistant block the parser encounters =#+begin_tool=
;; headers. The header contract is =#+begin_tool (<name> <plist...>)=: a
;; single sexp whose =car= is the tool-name symbol and whose =cdr= is a
;; plist of arguments (the plist keys are opaque to the parser — the
;; stream-writer currently emits =(:args <sexp>)= by convention, but any
;; plist shape round-trips). We capture:

;; - =:name= — first element of the header sexp as a string (or the raw
;;   post-delimiter text if the header fails to parse).
;; - =:args= — =(cdr parsed)= verbatim: the plist tail of the header
;;   sexp (or nil if no parseable sexp / no tail).
;; - =:result= — everything between the header line and =#+end_tool=,
;;   with leading/trailing whitespace trimmed.

;; Parsing is defensive: if the header does not contain a readable sexp
;; (e.g., early drafts or manually-edited tool blocks), store =:name= as
;; the raw header text and leave =:args= nil.


;; [[file:parser.org::*Tool-block parsing][Tool-block parsing:1]]
(defun gptel-chat--parse-tool-header (header-text)
  "Parse HEADER-TEXT (portion of `#+begin_tool' line after the delimiter).

The header contract is `(<name> <plist...>)': a single sexp whose car
is the tool-name symbol and whose cdr is the arguments plist.  :args
is extracted as `(cdr parsed)' verbatim — the plist keys are not
interpreted here.

Returns a cons (NAME . ARGS):
- NAME is a string (symbol-name of the first sexp element, or the
  trimmed raw text if the header does not parse).
- ARGS is the cdr of the parsed sexp (a list) or nil."
  (let ((text (string-trim (or header-text ""))))
    (if (string-empty-p text)
        (cons "" nil)
      (condition-case nil
          (let* ((parsed (car (read-from-string text)))
                 (name (cond
                        ((and (listp parsed) parsed (symbolp (car parsed)))
                         (symbol-name (car parsed)))
                        ((symbolp parsed) (symbol-name parsed))
                        (t text)))
                 (args (when (and (listp parsed) (cdr parsed))
                         (cdr parsed))))
            (cons name args))
        (error (cons text nil))))))
;; Tool-block parsing:1 ends here

;; Text-segment flush

;; While walking the body of an assistant block we accumulate prose
;; between tool blocks. =gptel-chat--flush-text-segment= builds a
;; =(:type text :content STR)= segment when the accumulated range is
;; non-empty. Empty ranges do not emit a segment (they would carry no
;; content into the eventual message list anyway).


;; [[file:parser.org::*Text-segment flush][Text-segment flush:1]]
(defun gptel-chat--flush-text-segment (from to)
  "Return a text segment plist for the range FROM..TO, or nil if empty.
The captured content is verbatim (no trimming) so delimiter-collision
escaping and round-trip behaviour are preserved for downstream tasks."
  (when (> to from)
    (let ((content (buffer-substring-no-properties from to)))
      (unless (string-empty-p content)
        (list :type 'text :content content)))))
;; Text-segment flush:1 ends here

;; Inside an assistant block

;; Walk forward from point, looking only for =#+end_assistant= and
;; nested =#+begin_tool=. Anything else — including literal
;; =#+begin_user=, =#+begin_assistant=, or =#+end_user= — is body
;; content, and the state machine does not react to it.

;; The outer caller passes =body-start= (the position immediately after
;; the =#+begin_assistant= line) and =open-line= (the line number of the
;; =#+begin_assistant= delimiter, used if the block turns out to be
;; unclosed). Returns a plist =(:segments SEGS :body-end POS)= where
;; =:body-end= is the position of the =#+end_assistant= line's start.


;; [[file:parser.org::*Inside an assistant block][Inside an assistant block:1]]
(defun gptel-chat--scan-assistant-body (body-start open-line)
  "Scan the body of an assistant block starting at BODY-START.
OPEN-LINE is the line number of the `#+begin_assistant' delimiter; used
in the unclosed-block error. Returns a plist
  (:segments SEGS :body-end POS)
where POS is the start of the matching `#+end_assistant' line.
Signals `user-error' on unclosed delimiters."
  (goto-char body-start)
  (let ((segments nil)
        (text-from body-start)
        (done nil)
        body-end)
    (while (not done)
      (cond
       ((not (re-search-forward gptel-chat--re-inside-assistant nil t))
        (gptel-chat--parse-error 'unclosed-assistant open-line))
       ;; Group 1 matched: `#+end_assistant'
       ((match-beginning 1)
        (let* ((end-line-start (match-beginning 1)))
          (when-let ((seg (gptel-chat--flush-text-segment
                           text-from end-line-start)))
            (push seg segments))
          (setq body-end end-line-start
                done t)))
       ;; Group 3 matched: `#+begin_user' or `#+begin_assistant' — invalid.
       ((match-beginning 3)
        (gptel-chat--parse-error
         'turn-inside-turn
         (gptel-chat--line-of (match-beginning 3))))
       ;; Group 2 matched: `#+begin_tool'
       ((match-beginning 2)
        (let* ((tool-open-line (gptel-chat--line-of (match-beginning 2)))
               (tool-line-start (match-beginning 2))
               (tool-line-end (line-end-position))
               (header-text (buffer-substring-no-properties
                             (+ tool-line-start (length "#+begin_tool"))
                             tool-line-end))
               (tool-body-start (1+ tool-line-end)))
          ;; Flush text before the tool block.
          (when-let ((seg (gptel-chat--flush-text-segment
                           text-from tool-line-start)))
            (push seg segments))
          ;; Find the matching #+end_tool.
          (goto-char tool-body-start)
          (unless (re-search-forward gptel-chat--re-end-tool nil t)
            (gptel-chat--parse-error 'unclosed-tool tool-open-line))
          (let* ((tool-end-line-start (match-beginning 0))
                 (result-text (string-trim
                               (buffer-substring-no-properties
                                tool-body-start tool-end-line-start)))
                 (parsed (gptel-chat--parse-tool-header header-text))
                 (tool-call (list :type 'tool-call
                                  :name (car parsed)
                                  :args (cdr parsed)
                                  :result result-text)))
            (push tool-call segments)
            ;; Advance past the #+end_tool line.
            (goto-char (line-end-position))
            (setq text-from (min (1+ (point)) (point-max))))))))
    (list :segments (nreverse segments) :body-end body-end)))
;; Inside an assistant block:1 ends here

;; Inside a user block

;; User blocks are opaque. The parser searches only for the matching
;; =#+end_user= — any lines that look like turn-opening or nested
;; =#+begin_tool= delimiters inside a user body are body content
;; (design.md Decision 13).

;; Returns =(:content STR :body-end POS)= where =POS= is the start of
;; the matching =#+end_user= line. Signals =user-error= on an unclosed
;; block.


;; [[file:parser.org::*Inside a user block][Inside a user block:1]]
(defun gptel-chat--scan-user-body (body-start open-line)
  "Scan the body of a user block starting at BODY-START.
OPEN-LINE is the line number of the `#+begin_user' delimiter; used in
the unclosed-block error. Returns a plist
  (:content STR :body-end POS)
where POS is the start of the matching `#+end_user' line."
  (goto-char body-start)
  (unless (re-search-forward gptel-chat--re-end-user nil t)
    (gptel-chat--parse-error 'unclosed-user open-line))
  (let* ((end-line-start (match-beginning 0))
         (content (buffer-substring-no-properties body-start end-line-start)))
    (list :content content :body-end end-line-start)))
;; Inside a user block:1 ends here

;; Outside-turn scan — the top-level state machine

;; Outside any turn block the walker looks for the next turn opener or
;; the first delimiter that is not allowed at this level (a bare
;; =#+begin_tool= or a bare =#+end_*=). Blank lines, org headings,
;; paragraphs, drawers, and =#+keyword:= lines are skipped
;; transparently.

;; The approach: run one =re-search-forward= that matches any of the
;; five delimiters we care about, decide what we just saw, dispatch the
;; relevant helper, and continue.


;; [[file:parser.org::*Outside-turn scan — the top-level state machine][Outside-turn scan — the top-level state machine:1]]
(defconst gptel-chat--re-outer-opener
  (concat
   "\\(" gptel-chat--re-begin-turn   "\\)"  ; 1,2 (outer + kind)
   "\\|\\(" gptel-chat--re-begin-tool "\\)") ; 3
  "Union regex matching turn or tool-block openers at column 0.
Used by the outer walker. Bare `#+end_*' closers at top level are
ignored — they only appear in malformed buffers and the spec does
not require a dedicated error shape for them.")
;; Outside-turn scan — the top-level state machine:1 ends here

;; Buffer parser — public entry point

;; =gptel-chat--parse-buffer= walks the current buffer (or BUFFER, when
;; supplied) and returns a list of turn plists in document order.

;; Behaviour:
;; - Empty buffer → empty list.
;; - Metadata-only / heading-only / prose-only buffer → empty list.
;; - Content between turn blocks is ignored.
;; - Empty blocks are parsed into turns with empty content; downstream
;;   message conversion filters them (task =messages=).
;; - Malformed structure signals =user-error= at the offending line.


;; [[file:parser.org::*Buffer parser — public entry point][Buffer parser — public entry point:1]]
(defun gptel-chat--parse-buffer (&optional buffer)
  "Parse BUFFER (default current) as a `gptel-chat-mode' buffer.

Returns a list of turn plists in document order.  Each element has
shape

  (:role user :content STR :start MARKER :end MARKER)
  (:role assistant
   :segments ((:type text :content STR)
              (:type tool-call :name STR :args SEXP :result STR) ...)
   :start MARKER :end MARKER)

The START and END markers point at the `#+begin_*' delimiter's line
start and at the start of the matching `#+end_*' delimiter line,
respectively.

Signals `user-error' when the buffer is structurally invalid:
- an unclosed `#+begin_user', `#+begin_assistant', or `#+begin_tool';
- a `#+begin_tool' outside any assistant block;
- a `#+begin_user' or `#+begin_assistant' opened inside another
  user/assistant block."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search t)
              (turns nil))
          (goto-char (point-min))
          (while (re-search-forward gptel-chat--re-outer-opener nil t)
            (cond
             ;; Group 1: a turn opener. `kind' is group 2.
             ((match-beginning 1)
              (let* ((open-start (match-beginning 1))
                     (open-line  (gptel-chat--line-of open-start))
                     (kind-str   (match-string 2))
                     ;; Clamp to `point-max' so a `#+begin_user' or
                     ;; `#+begin_assistant' on the buffer's last line
                     ;; without a trailing newline dispatches into the
                     ;; body scanners, which signal the documented
                     ;; `unclosed <kind> block at line N' user-error
                     ;; instead of `args-out-of-range' from `goto-char'.
                     (body-start (min (1+ (line-end-position)) (point-max))))
                (pcase (downcase kind-str)
                  ("user"
                   (let* ((scan (gptel-chat--scan-user-body
                                 body-start open-line))
                          (end-line-start (plist-get scan :body-end))
                          (content (plist-get scan :content)))
                     (push (list :role 'user
                                 :content content
                                 :start (gptel-chat--marker-at open-start)
                                 :end   (gptel-chat--marker-at end-line-start))
                           turns)
                     (goto-char (line-end-position))))
                  ("assistant"
                   (let* ((scan (gptel-chat--scan-assistant-body
                                 body-start open-line))
                          (segments (plist-get scan :segments))
                          (end-line-start (plist-get scan :body-end)))
                     (push (list :role 'assistant
                                 :segments segments
                                 :start (gptel-chat--marker-at open-start)
                                 :end   (gptel-chat--marker-at end-line-start))
                           turns)
                     (goto-char (line-end-position)))))))
             ;; Group 3: `#+begin_tool' outside any assistant block.
             ((match-beginning 3)
              (gptel-chat--parse-error
               'tool-block-outside-assistant
               (gptel-chat--line-of (match-beginning 3))))))
          (nreverse turns))))))
;; Buffer parser — public entry point:1 ends here

;; Comma un-escape (inverse of the stream sanitizer)

;; The streaming sanitizer (=gptel-chat--sanitize-chunk= in =stream.el=)
;; prepends =,= to any body line matching

;; : ^#\+end_\(user\|assistant\|tool\)\b

;; so that assistant output cannot prematurely close a containing block.
;; On the send path, the same transform must be inverted before the text
;; reaches the model: a line =,#+end_assistant= on disk is presented to
;; the model as =#+end_assistant=.

;; =gptel-chat--unescape-end-delimiters= is a pure string → string
;; function that strips a single leading =,= from lines matching the
;; escape pattern (case-insensitive, anchored to beginning of line) and
;; leaves everything else untouched. It is the exact inverse of
;; =gptel-chat--sanitize-chunk= restricted to the three collision
;; delimiters — round-trip tests in =test/parser/escape-round-trip-spec.el=
;; pin that inverse relationship.


;; [[file:parser.org::*Comma un-escape (inverse of the stream sanitizer)][Comma un-escape (inverse of the stream sanitizer):1]]
(defconst gptel-chat--escaped-end-delimiter-regexp
  "^,\\(#\\+end_\\(user\\|assistant\\|tool\\)\\b\\)"
  "Regexp matching a comma-escaped chat-mode closing delimiter line.
Group 1 is the un-escaped delimiter text.  Case-folded matches via
`case-fold-search' so mixed-case delimiters (e.g., `,#+End_User')
round-trip cleanly.")

(defun gptel-chat--unescape-end-delimiters (text)
  "Strip a single leading `,' from any `,#+end_*' line in TEXT.
Inverse of the `,'-prefix escape applied by
`gptel-chat--sanitize-chunk'.  Only the three chat-mode closers
(`#+end_user', `#+end_assistant', `#+end_tool') are un-escaped —
`,#+end_src' and other org delimiters are preserved verbatim.

Lines are identified by `\\n' boundaries; the first line is treated
identically to any other line in TEXT.  Matching is case-insensitive
to mirror the sanitizer.  Returns TEXT unchanged when no escaped
delimiter is present."
  (if (or (null text) (string-empty-p text))
      (or text "")
    (let ((case-fold-search t))
      (replace-regexp-in-string
       "\\(\\`\\|\n\\),\\(#\\+end_\\(user\\|assistant\\|tool\\)\\b\\)"
       "\\1\\2"
       text))))
;; Comma un-escape (inverse of the stream sanitizer):1 ends here

;; Whitespace emptiness test

;; Empty user/assistant turns are skipped when building the message list
;; (spec scenario "Empty blocks are skipped"). "Empty" means zero
;; non-whitespace characters — a block containing only a newline is
;; still empty.


;; [[file:parser.org::*Whitespace emptiness test][Whitespace emptiness test:1]]
(defun gptel-chat--blank-content-p (text)
  "Return non-nil when TEXT is nil, empty, or whitespace-only."
  (or (null text)
      (string-empty-p text)
      (string-match-p "\\`[ \t\n\r]*\\'" text)))
;; Whitespace emptiness test:1 ends here

;; Assistant segment → messages

;; Assistant turns hold a list of segments in document order. Text
;; segments become one =(response . STR)= cons each (after un-escape);
;; tool-call segments become one =(tool . PLIST)= cons each. Empty text
;; segments are dropped — the parser does not emit empty text segments
;; today (see =gptel-chat--flush-text-segment=), but we defend against
;; that shape anyway so future parser tweaks don't silently inject blank
;; assistant messages.

;; Tool calls carry =:name=, =:args=, and =:result= from the parser;
;; upstream backends additionally consume =:id=. The turn list has no
;; =:id= slot — tool-call IDs in on-disk buffers are implicit (the
;; =#+begin_tool (NAME ARGS)= header does not carry one). Each backend's
;; =gptel--parse-list= synthesises a fresh ID when the =:id= key is
;; absent (e.g., =gptel--anthropic-format-tool-id=,
;; =gptel--openai-format-tool-id=), so we simply pass the =:id= through
;; only when the parser has one. Today it never does, so the emitted
;; plist carries only =:name=, =:args=, =:result=.


;; [[file:parser.org::*Assistant segment → messages][Assistant segment → messages:1]]
(defun gptel-chat--segment-to-messages (segment)
  "Convert one assistant SEGMENT into a list of messages.
Returns a list (possibly empty) of cons cells in the advanced
`gptel-request' `:prompt' format.  Text segments yield a single
`(response . STR)' cons (nil if the content is blank); tool-call
segments yield a single `(tool . PLIST)' cons with `:name', `:args',
and `:result' copied from the segment."
  (pcase (plist-get segment :type)
    (`text
     (let ((content (gptel-chat--unescape-end-delimiters
                     (plist-get segment :content))))
       (if (gptel-chat--blank-content-p content)
           nil
         (list (cons 'response content)))))
    (`tool-call
     (let ((call (list :name   (plist-get segment :name)
                       :args   (plist-get segment :args)
                       :result (or (plist-get segment :result) ""))))
       (list (cons 'tool call))))
    (_ nil)))
;; Assistant segment → messages:1 ends here

;; Turn → messages

;; =gptel-chat--turn-to-messages= dispatches on =:role=. User turns emit
;; a single =(prompt . STR)= cons when non-blank; assistant turns emit
;; one message per non-empty segment in document order.


;; [[file:parser.org::*Turn → messages][Turn → messages:1]]
(defun gptel-chat--turn-to-messages (turn)
  "Convert TURN (a parser turn plist) into a list of messages.
See `gptel-chat--turns-to-messages' for the message-shape contract.
Returns nil for an empty user turn or an assistant turn whose
segments are all empty."
  (pcase (plist-get turn :role)
    (`user
     (let ((content (gptel-chat--unescape-end-delimiters
                     (plist-get turn :content))))
       (if (gptel-chat--blank-content-p content)
           nil
         (list (cons 'prompt content)))))
    (`assistant
     (cl-loop for seg in (plist-get turn :segments)
              nconc (gptel-chat--segment-to-messages seg)))
    (_ nil)))
;; Turn → messages:1 ends here

;; Public entry point

;; =gptel-chat--turns-to-messages= is the public converter. It accepts
;; either a buffer's parsed turn list directly, or the result of an
;; earlier call to =gptel-chat--parse-buffer=. Empty input yields an
;; empty message list.


;; [[file:parser.org::*Public entry point][Public entry point:1]]
(defun gptel-chat--turns-to-messages (turns)
  "Convert TURNS into a `gptel-request' `:prompt' message list.

TURNS is the return value of `gptel-chat--parse-buffer' — an ordered
list of turn plists.  Returns an ordered list of cons cells suitable
for passing as `gptel-request''s `:prompt' keyword:

  (prompt . STR)      — a user turn
  (response . STR)    — an assistant text segment
  (tool . PLIST)      — one tool call; PLIST has `:name', `:args',
                        `:result' (the backend synthesises `:id')

User turns with whitespace-only content and assistant turns whose
segments are all empty are skipped.  Assistant segments with tool
calls are emitted as a single `(tool . PLIST)' cons each; the
backend's `gptel--parse-list' method expands each into the wire-
level tool_use / tool_result pair (see Anthropic, OpenAI, Ollama,
Gemini implementations in `runtime/straight/repos/gptel/').

Body lines matching `^,#\\+end_\\(user\\|assistant\\|tool\\)\\b' are
un-escaped (leading `,' stripped) before inclusion — the inverse of
the streaming sanitizer in `gptel-chat-stream'."
  (cl-loop for turn in turns
           nconc (gptel-chat--turn-to-messages turn)))
;; Public entry point:1 ends here

;; Provide


;; [[file:parser.org::*Provide][Provide:1]]
(provide 'gptel-chat-parser)

;;; parser.el ends here
;; Provide:1 ends here
