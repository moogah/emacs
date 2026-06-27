;;; chat-body-indent-roundtrip-transparent.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-body-indent-roundtrip-transparent
;; generated-at: 2026-05-22 (reframed from chat-heading-escape-roundtrip-transparent)
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-body-indent-roundtrip-transparent"

  (it "round-trips body content through write-indent + parse-dedent"
    (error "speculated; not implemented \
\nProperty: for any segment S, dedenting the indented form of S yields \
S with its common leading indentation removed — \
`(gptel-chat--dedent (indent-segment S))' equals S modulo a uniform \
leading-indentation shift."))

  (it "round-trips a heading-shaped line so the LLM sees column 0"
    (error "speculated; not implemented \
\nFixture: an `#+begin_assistant' body containing the indented line \
`  * My Heading'. \
\nAssert: `gptel-chat--segment-to-messages' yields message content \
containing `* My Heading' at column 0."))

  (it "preserves intentional nested indentation within a segment"
    (error "speculated; not implemented \
\nFixture: a body segment whose lines carry extra indentation relative \
to the body width (a nested list, indented code). \
\nAssert: the dedent strips only the common minimum indent; the \
relative indentation of the nested content survives."))

  (it "is robust against a changed indentation width"
    (error "speculated; not implemented \
\nFixture: content written under `gptel-chat-content-indentation' = 2, \
read with the value 4. \
\nAssert: the dedent measures actual indentation and still yields \
correctly column-0 content — the round-trip does not depend on the \
current defcustom value."))

  (it "leaves a uniformly-indented segment per org-src-block behaviour"
    (error "speculated; not implemented \
\nFixture: a segment EVERY line of which is indented (no column-0 \
anchor line). \
\nAssert: the dedent strips the common minimum — the outermost indent \
level is removed.  This is the accepted edge case, identical to \
`org-do-remove-indentation' for src blocks.")))

;;; chat-body-indent-roundtrip-transparent.test.el ends here
