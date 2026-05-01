;;; all-write-paths-apply-heading-escape.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/all-write-paths-apply-heading-escape
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: all-write-paths-apply-heading-escape"

  (it "wires the streaming sanitizer to apply the heading escape"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat--sanitize-chunk' (in \
config/gptel/chat/stream.el) must contain a code path that escapes \
`^\\\\*+ ' lines. Mechanism: regex against the file contents OR a \
behavioural test that drives the sanitizer with `* Heading' and \
asserts the output has the escape applied."))

  (it "wires `post-self-insert-hook' for typed `*' at column 0"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' (in \
config/gptel/chat/mode.el) registers a function on \
`post-self-insert-hook' (buffer-local) that calls \
`gptel-chat--escape-typed-heading' (or equivalent). Mechanism: grep \
mode.el for `add-hook' and `post-self-insert-hook' OR activate the \
mode in a fixture and inspect `post-self-insert-hook' for the \
expected function."))

  (it "wires `after-change-functions' for paste / yank / programmatic insert"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' registers a function \
on `after-change-functions' (buffer-local) that calls \
`gptel-chat--escape-inserted-headings' (or equivalent). Mechanism: \
same approach as the post-self-insert-hook check."))

  (it "wires migration-on-read to `gptel-chat-mode' activation"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' calls \
`gptel-chat--migrate-headings' (or equivalent) as part of mode \
activation, after buffer-local setup completes. Mechanism: grep \
mode.el for the call OR activate the mode on a pre-escape fixture \
and assert content has been normalised."))

  (it "fails if a fifth content-introduction path is added without escape"
    (error "speculated; not implemented \
\nMeta-assertion: at end-of-cycle Architect audit, scan \
config/gptel/chat/mode.el and any new chat-mode files for additional \
hook-registration sites (`add-hook', `add-function', \
`advice-add'). Compare against the four authorised entry points. \
\nIf a new site exists and does not call any heading-escape function, \
flag as a `responsibility-leakage' or `interface-drift' finding. \
\nThis test scaffold serves as the hand-off marker — replace with a \
real audit at integrate.")))

;;; all-write-paths-apply-heading-escape.test.el ends here
