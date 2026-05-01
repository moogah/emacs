;;; chat-heading-escape-roundtrip-transparent.test.el --- Invariant scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/invariant/chat-heading-escape-roundtrip-transparent
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise

(require 'buttercup)

(describe "Invariant: chat-heading-escape-roundtrip-transparent"

  (it "round-trips `* Heading' through stream-sanitizer + parser-strip"
    (error "speculated; not implemented \
\nProperty: for any string S of the form `*+ <text>' (one or more `*' \
followed by space and arbitrary text), \
\n  (gptel-chat--unescape-headings (gptel-chat--apply-heading-escape S)) \
\nequals S verbatim."))

  (it "round-trips multi-level headings (`** Sub', `*** Deep')"
    (error "speculated; not implemented \
\nFixture: body containing ` ** Sub-heading' and ` *** Sub-sub-heading'. \
\nAssert: `gptel-chat--segment-to-messages' yields message content \
containing exactly `** Sub-heading' and `*** Sub-sub-heading' (escape \
stripped from each)."))

  (it "is robust against any leading-whitespace amount on the strip side"
    (error "speculated; not implemented \
\nFixture: body containing `   * Heading' (three spaces) and \
`\\t* Tabbed' (one tab). \
\nAssert: parser-strip yields `* Heading' and `* Tabbed' for both lines, \
regardless of `gptel-chat-content-indentation' value."))

  (it "composes independently with the delimiter-collision escape"
    (error "speculated; not implemented \
\nFixture: body containing both `,#+end_assistant' and ` * Heading'. \
\nAssert: parser-strip yields message content with `#+end_assistant' \
(comma stripped) AND `* Heading' (leading space stripped); the two \
escapes do not interfere."))

  (it "leaves non-heading lines unchanged through the round-trip"
    (error "speculated; not implemented \
\nFixture: body containing `text* asterisk' (asterisk not at column 0) \
and `*nospace' (no trailing space). \
\nAssert: round-trip yields identical content; neither line is treated \
as a heading by either stage.")))

;;; chat-heading-escape-roundtrip-transparent.test.el ends here
