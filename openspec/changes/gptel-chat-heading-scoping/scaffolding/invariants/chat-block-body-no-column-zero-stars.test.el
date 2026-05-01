;;; chat-block-body-no-column-zero-stars.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-block-body-no-column-zero-stars
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-block-body-no-column-zero-stars"

  (it "holds after streaming-sanitizer insertion of `* Heading' content"
    (error "speculated; not implemented \
\nFixture: a chat-mode buffer with an open `#+begin_assistant`. \
\nDrive `gptel-chat--sanitize-chunk' with chunk text that contains \
`* Heading' / `** Sub' / `*** Deep'. \
\nAssert: scanning every line strictly between `#+begin_assistant' and \
its matching `#+end_assistant', no line matches `^\\\\*+ '."))

  (it "holds after `post-self-insert-hook' typing `*' at column 0"
    (error "speculated; not implemented \
\nFixture: cursor at column 0 inside a `#+begin_user' body. \
\nSimulate self-insert of `*'. \
\nAssert: line text starts with the configured indent prefix \
followed by `*'; the body invariant holds."))

  (it "holds after paste / yank / programmatic insert via `after-change-functions'"
    (error "speculated; not implemented \
\nFixture: cursor inside a `#+begin_assistant' body. \
\nInsert via `(insert \"* H1\\n- list\\n** H2\\n\")'. \
\nAssert: the H1 / H2 lines now begin with the configured indent \
prefix; the `- list' line is unchanged; body invariant holds."))

  (it "holds after `gptel-chat-mode' activation on a pre-escape session"
    (error "speculated; not implemented \
\nFixture: open a session file whose `#+begin_assistant' body contains \
the literal string `* My Heading' at column 0. \
\nActivate `gptel-chat-mode'. \
\nAssert: that line is now ` * My Heading' (configured indent prefix); \
buffer is marked modified; body invariant holds."))

  (it "is preserved across the streaming chunk-split case"
    (error "speculated; not implemented \
\nFixture: drive sanitizer with two adjacent chunks, the boundary \
falling mid-line: `\\n* He' then `ading\\n'. \
\nAssert: the per-line holdback completes the escape before insertion; \
the body invariant holds.")))

;;; chat-block-body-no-column-zero-stars.test.el ends here
