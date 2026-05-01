;;; chat-block-delimiter-lines-stay-at-column-0.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-block-delimiter-lines-stay-at-column-0
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-block-delimiter-lines-stay-at-column-0"

  (it "is preserved by the streaming sanitizer"
    (error "speculated; not implemented \
\nFixture: `#+begin_assistant' open in a chat-mode buffer. \
\nDrive sanitizer with chunks containing `#+end_assistant', \
`#+begin_tool', `#+end_tool'. (Note: `#+end_*` lines should be \
comma-escaped per the OTHER existing rule, but they remain at column \
0 — the comma sits before the `#'.) \
\nAssert: every delimiter line in the resulting buffer is at column 0. \
No `#+begin_*' or `#+end_*' line has been indented by the heading \
escape."))

  (it "is preserved by the user-typed-heading hook (delimiter-line exclusion)"
    (error "speculated; not implemented \
\nFixture: cursor at column 0 of a `#+begin_user' line. \
\nVerify that `gptel-chat--point-in-block-body-p' returns nil for that \
position. Type `*'. \
\nAssert: NO escape is applied (point is not in a body); the line \
remains a valid `#+begin_user' line."))

  (it "is preserved by the migration-on-read pass"
    (error "speculated; not implemented \
\nFixture: open a session file with `#+begin_user' / `#+end_user' / \
`#+begin_assistant' / `#+end_assistant' delimiters, body containing \
column-0 `*' lines. \
\nActivate `gptel-chat-mode'. \
\nAssert: the four delimiter lines are still at column 0; only body \
content has been indented."))

  (it "rejects an indented opener at parse time (regression guard)"
    (error "speculated; not implemented \
\nFixture: a buffer where `#+begin_user' is at column 1 (indented by \
one space). This is a violation. \
\nRun `gptel-chat-parse-buffer'. \
\nAssert: the indented opener is NOT recognised as a turn boundary — \
chat-mode's regex parser anchors on column 0. This documents WHY the \
delimiter-stays-at-column-0 invariant matters.")))

;;; chat-block-delimiter-lines-stay-at-column-0.test.el ends here
