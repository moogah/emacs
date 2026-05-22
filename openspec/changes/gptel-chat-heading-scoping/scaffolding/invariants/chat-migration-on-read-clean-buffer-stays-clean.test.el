;;; chat-migration-on-read-clean-buffer-stays-clean.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-migration-on-read-clean-buffer-stays-clean
;; generated-at: 2026-05-22 (reframed for chat-block-body-indentation)
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-migration-on-read-clean-buffer-stays-clean"

  (it "leaves an already-indented session unmodified after activation"
    (error "speculated; not implemented \
\nFixture: a session file whose chat-block bodies are already indented \
by the body width, with no legacy escape artifacts. \
\nOpen the file; activate `gptel-chat-mode'. \
\nAssert: `(buffer-modified-p)' returns nil."))

  (it "marks the buffer modified ONLY when migration rewrites content"
    (error "speculated; not implemented \
\nFixture: a session whose `#+begin_assistant' body has column-0 \
content (pre-indentation) or legacy `,#+end_*' / 1-space `*' escapes. \
\nActivate `gptel-chat-mode'. \
\nAssert: the body is normalised to the indented form; \
`(buffer-modified-p)' returns t."))

  (it "is idempotent — a migrated buffer re-opened is clean"
    (error "speculated; not implemented \
\nFixture: migrate a buffer, persist it, re-open it. \
\nAssert: the second activation makes no change; \
`(buffer-modified-p)' returns nil."))

  (it "scopes its rewrite to chat-block bodies only"
    (error "speculated; not implemented \
\nFixture: a session with `* Section A' as a TOP-LEVEL document \
heading (outside any chat block) and chat blocks needing migration. \
\nActivate `gptel-chat-mode'. \
\nAssert: `* Section A' is unchanged at column 0 and parses as a real \
headline; only chat-block body content was indented."))

  (it "leaves tool delimiter lines at column 0 during migration"
    (error "speculated; not implemented \
\nFixture: a session whose assistant block has a nested `#+begin_tool' \
/ `#+end_tool' block, bodies un-indented. \
\nActivate `gptel-chat-mode'. \
\nAssert: the `#+begin_tool' / `#+end_tool' lines remain at column 0; \
the tool result and the surrounding prose segments are indented.")))

;;; chat-migration-on-read-clean-buffer-stays-clean.test.el ends here
