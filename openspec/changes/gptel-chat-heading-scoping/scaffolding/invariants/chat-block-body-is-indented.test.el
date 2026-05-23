;;; chat-block-body-is-indented.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-block-body-is-indented
;; generated-at: 2026-05-22 (reframed from chat-block-body-no-column-zero-stars)
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-block-body-is-indented"

  (it "holds after streaming-sanitizer insertion"
    (error "speculated; not implemented \
\nFixture: a chat-mode buffer with an open `#+begin_assistant'. \
\nDrive `gptel-chat--sanitize-chunk' with chunk text containing \
`* Heading', a line whose content is `#+end_assistant', and ordinary \
prose. \
\nAssert: every non-blank line strictly between `#+begin_assistant' \
and its matching `#+end_assistant' is indented by at least the body \
width; no body line begins with a column-0 org structural token."))

  (it "holds after typing in a body (indent-line-function + electric-indent)"
    (error "speculated; not implemented \
\nFixture: cursor inside a `#+begin_user' body.  Open a new line (RET) \
and type `* heading'. \
\nAssert: the new line starts at the body indent; the typed `*' is not \
at column 0."))

  (it "holds after paste / yank via after-change region-shift"
    (error "speculated; not implemented \
\nFixture: cursor inside a `#+begin_assistant' body.  Insert via \
`(insert \"* H1\\n  - nested\\n** H2\\n\")'. \
\nAssert: the region is shifted so its least-indented line lands at \
the body indent; the relative indentation of `  - nested' is \
preserved; no body line is at column 0."))

  (it "holds after `gptel-chat-mode' activation on a pre-indentation session"
    (error "speculated; not implemented \
\nFixture: open a session whose `#+begin_assistant' body contains \
`* My Heading' at column 0. \
\nActivate `gptel-chat-mode'. \
\nAssert: that line is now indented by the body width; the buffer is \
marked modified; the body invariant holds."))

  (it "is preserved across the streaming chunk-split case"
    (error "speculated; not implemented \
\nFixture: drive the sanitizer with two adjacent chunks, the boundary \
falling mid-line: `\\n* He' then `ading\\n'. \
\nAssert: the per-line holdback completes the line and indents it \
before insertion; the body invariant holds.")))

;;; chat-block-body-is-indented.test.el ends here
