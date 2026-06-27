;;; chat-block-delimiter-lines-stay-at-column-0.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-block-delimiter-lines-stay-at-column-0
;; generated-at: 2026-05-22 (reframed for chat-block-body-indentation)
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-block-delimiter-lines-stay-at-column-0"

  (it "is preserved by the streaming indenter (outer and tool delimiters)"
    (error "speculated; not implemented \
\nFixture: `#+begin_assistant' open in a chat-mode buffer; stream a \
response that opens a nested `#+begin_tool' block. \
\nAssert: every delimiter line in the resulting buffer — \
`#+begin_assistant', `#+end_assistant', `#+begin_tool', `#+end_tool' \
— is at column 0; only body and tool-result content is indented."))

  (it "is preserved by the paste region-shift"
    (error "speculated; not implemented \
\nFixture: paste a region that straddles a `#+end_user' line. \
\nAssert: the `#+end_user' delimiter stays at column 0; only the \
portion of the region inside the body is shifted."))

  (it "is preserved by the migration-on-read pass"
    (error "speculated; not implemented \
\nFixture: open a session with outer delimiters and a nested \
`#+begin_tool' / `#+end_tool' block, bodies un-indented. \
\nActivate `gptel-chat-mode'. \
\nAssert: all delimiter lines — outer and tool — remain at column 0; \
only body content has been indented."))

  (it "rejects an indented outer opener at parse time (regression guard)"
    (error "speculated; not implemented \
\nFixture: a buffer where `#+begin_user' is indented by one space. \
\nRun `gptel-chat-parse-buffer'. \
\nAssert: the indented opener is NOT recognised as a turn boundary — \
the parser anchors `^#\\\\+begin_' at column 0.  This is what makes \
indentation an escape: a `#+end_user' typed in body prose is indented \
content, not a real closer."))

  (it "renders tool delimiters aligned via display line-prefix only"
    (error "speculated; not implemented \
\nFixture: a buffer with a nested `#+begin_tool' / `#+end_tool' block, \
the display layer active. \
\nAssert: the `#+begin_tool' / `#+end_tool' lines carry a `line-prefix' \
display property of the body width, BUT their buffer text still \
starts at column 0 — `(current-column)' at beginning-of-line, and the \
on-disk text, are unaffected.")))

;;; chat-block-delimiter-lines-stay-at-column-0.test.el ends here
