;;; chat-migration-on-read-clean-buffer-stays-clean.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-migration-on-read-clean-buffer-stays-clean
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-migration-on-read-clean-buffer-stays-clean"

  (it "leaves a clean session unmodified after `gptel-chat-mode' activation"
    (error "speculated; not implemented \
\nFixture: a session file whose chat blocks contain NO column-0 `*' \
lines (already escaped, or no headings at all). \
\nOpen the file; activate `gptel-chat-mode'. \
\nAssert: `(buffer-modified-p)' returns nil. \
\nAssert: scanning the buffer's chat-block bodies finds no `^\\\\*+ ' \
lines (invariant pre-condition was already satisfied)."))

  (it "marks the buffer modified ONLY when migration actually rewrites content"
    (error "speculated; not implemented \
\nFixture: a session file whose `#+begin_assistant' body contains \
`* Some Heading' at column 0. \
\nActivate `gptel-chat-mode'. \
\nAssert: that line is now ` * Some Heading'; \
`(buffer-modified-p)' returns t."))

  (it "preserves the buffer's prior modified state when migration is a no-op"
    (error "speculated; not implemented \
\nFixture: open a clean session file, manually `(insert \"x\")` to mark \
the buffer modified, then `(undo)' so the change is gone but the \
modified flag would still be t per Emacs semantics — this case is \
about NOT clobbering pre-existing user state. \
\nActivate `gptel-chat-mode'. \
\nAssert: migration sees no `^\\\\*+ ' content; does NOT call \
`set-buffer-modified-p nil' as a side effect. The user's prior \
modified state is preserved."))

  (it "scopes its scan to chat-block bodies only"
    (error "speculated; not implemented \
\nFixture: a session file with `* Section A' as a TOP-LEVEL document \
heading (outside any chat block) AND chat blocks whose bodies contain \
no column-0 `*'. \
\nActivate `gptel-chat-mode'. \
\nAssert: `(buffer-modified-p)' returns nil; `* Section A' is \
unchanged at column 0; `org-element-parse-buffer' parses it as a \
real headline.")))

;;; chat-migration-on-read-clean-buffer-stays-clean.test.el ends here
