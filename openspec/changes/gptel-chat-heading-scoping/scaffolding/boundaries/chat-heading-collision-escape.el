;;; chat-heading-collision-escape.el --- Boundary scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/chat-heading-collision-escape
;; generated-at: 2026-05-01T10:35:02+02:00
;; license: implementor-may-revise
;;
;; This file is the speculative canonical mapping for the chat-heading-
;; collision escape pipeline. The Implementor may rewrite this file when
;; the speculation is wrong, but a rewrite is a SIGNAL at integrate, not
;; a transgression. Document the why in your task's `## Discoveries`.
;;
;; Lives under <change>/scaffolding/ — NOT in src/ and NOT discovered by
;; the test runner. Promotion to a permanent home (or archival /
;; rejection) is decided at integrate.

(error "speculated; not implemented \
\n\nBOUNDARY: chat-heading-collision-escape \
\nStage 1 (write-escape) — wired at four entry points: \
\n  1. gptel-chat--sanitize-chunk        (config/gptel/chat/stream.org) \
\n  2. gptel-chat--escape-typed-heading  (config/gptel/chat/mode.org) \
\n  3. gptel-chat--escape-inserted-headings (config/gptel/chat/mode.org) \
\n  4. gptel-chat--migrate-headings      (config/gptel/chat/mode.org) \
\nStage 2 (parse-strip) — wired in: \
\n  - gptel-chat--unescape-headings, called from \
\n    gptel-chat--segment-to-messages   (config/gptel/chat/parser.org) \
\n\nReplace this `error` form with the canonical mapping function once \
all four producers and both consumers exist and the round-trip is \
exercised end-to-end.")

;; Canonical mapping function (TODO body — Implementor to fill).
;; Signature speculation: takes a region of buffer text and returns
;; the escaped form. Stage 2 is its inverse.
;;
;;   (defun gptel-chat--apply-heading-escape (str)
;;     "Return STR with column-0 `*+ ' lines prefixed by the
;;   configured indent."
;;     ...)
;;
;;   (defun gptel-chat--unescape-headings (str)
;;     "Return STR with leading whitespace stripped from
;;   `^[ \t]+\\*+ ` lines."
;;     ...)

(provide 'chat-heading-collision-escape-scaffold)
;;; chat-heading-collision-escape.el ends here
