;;; all-write-paths-indent-body.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/all-write-paths-indent-body
;; generated-at: 2026-05-22 (reframed from all-write-paths-apply-heading-escape)
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: all-write-paths-indent-body"

  (it "wires the streaming sanitizer to indent each line"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat--sanitize-chunk' (in \
config/gptel/chat/stream.el) indents each complete line of streamed \
content by the body width.  Mechanism: drive the sanitizer with \
`* Heading' and assert the output is indented."))

  (it "wires `after-change-functions' for paste / yank"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' registers a \
buffer-local `after-change-functions' entry \
(`gptel-chat--indent-inserted-region' or equivalent) that shifts an \
inserted region inside a body to the body indent.  Mechanism: grep \
mode.el for `add-hook' + `after-change-functions', OR insert a region \
into a fixture body and assert it is shifted."))

  (it "wires migration-on-read to `gptel-chat-mode' activation"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' calls \
`gptel-chat--migrate-buffer' (or equivalent) as part of activation. \
Mechanism: grep mode.el for the call, OR activate the mode on an \
un-indented fixture and assert the body is indented."))

  (it "wires typing via a buffer-local indent-line-function"
    (error "speculated; not implemented \
\nStructural-audit assertion: `gptel-chat-mode' sets a buffer-local \
`indent-line-function' that indents a body line to the body width and \
a delimiter line to column 0, with electric-indent in effect. \
Mechanism: grep mode.el, OR activate the mode and exercise RET inside \
a body.  NOTE: typing is best-effort (design.md Decision 6), not a \
hard guarantee — the audit treats it accordingly."))

  (it "fails if a fifth content-introduction path is added without indent"
    (error "speculated; not implemented \
\nMeta-assertion: at end-of-cycle Architect audit, scan \
config/gptel/chat/*.el for additional content-introduction sites \
(`add-hook', `insert' wrappers, yank-handlers) that land text in a \
chat-block body.  Compare against the authorised paths. \
\nIf a new site exists and does not leave its content indented, flag \
as a `responsibility-leakage' / `interface-drift' finding. \
\nReplace this scaffold with a real audit at integrate.")))

;;; all-write-paths-indent-body.test.el ends here
