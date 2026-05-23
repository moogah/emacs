;;; chat-block-body-indentation.el --- Boundary scaffolding -*- lexical-binding: t; -*-

;; scaffolding-of: register/boundary/chat-block-body-indentation
;; generated-at: 2026-05-22 (reframed from chat-heading-collision-escape)
;; license: implementor-may-revise
;;
;; Speculative canonical mapping for the chat-block body indent/dedent
;; pipeline.  Supersedes the retired chat-heading-collision-escape
;; scaffold (per-`*' escape).  The Implementor may rewrite this file
;; when the speculation is wrong — a rewrite is a SIGNAL at integrate,
;; not a transgression.  Document the why in your task's `## Discoveries'.
;;
;; Lives under <change>/scaffolding/ — NOT in src/ and NOT discovered by
;; the test runner.

(error "speculated; not implemented \
\n\nBOUNDARY: chat-block-body-indentation \
\nStage 1 (write-indent) — content-introduction paths: \
\n  1. gptel-chat--sanitize-chunk          (config/gptel/chat/stream.org) \
\n     per-line indent of streamed assistant content \
\n  2. gptel-chat--indent-inserted-region  (config/gptel/chat/mode.org) \
\n     after-change region-shift for paste / yank \
\n  3. gptel-chat--migrate-buffer          (config/gptel/chat/mode.org) \
\n     mode-activation normalisation of legacy and current content \
\n  4. indent-line-function + electric-indent (config/gptel/chat/mode.org) \
\n     typing affordance (best-effort, not a reactive producer) \
\nStage 2 (parse-dedent) — wired in: \
\n  - gptel-chat--dedent (measure-and-strip a string), called from \
\n    gptel-chat--segment-to-messages and gptel-chat--turn-to-messages \
\n    (config/gptel/chat/parser.org) \
\n\nReplace this `error' form with the canonical mapping once the write \
producers and the dedent consumer exist and the indent/dedent \
round-trip is exercised end-to-end.")

;; Canonical helpers (TODO body — Implementor to fill).
;;
;;   (defun gptel-chat--body-indent ()
;;     "Return the body indent width — `gptel-chat-content-indentation',
;;   floored at 1, defaulting to 2 when unbound.")
;;
;;   (defun gptel-chat--dedent (str)
;;     "Return STR with each line's common minimum leading indentation
;;   removed (the `org-do-remove-indentation' model).  Inverse by intent
;;   of the write-path indent.")

(provide 'chat-block-body-indentation-scaffold)
;;; chat-block-body-indentation.el ends here
