;;; scope-drawer-no-duplication.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/scope-drawer-no-duplication
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise
;;
;; Anchored by the prior corruption incident:
;;   ~/org/roam/20260419111957-gptel_preset_property_corruption.org

(require 'buttercup)

(describe "invariant: scope-drawer-no-duplication"

  (it "exactly one :PROPERTIES: line at point-min after a single add-to-scope"
    (error
     "speculated; not implemented — fixture a chat buffer with a drawer, call jf/gptel-scope--write-pattern-to-drawer once, count occurrences of \":PROPERTIES:\" and assert it is exactly 1"))

  (it "exactly one :PROPERTIES: line after three sequential add-to-scope cycles"
    (error
     "speculated; not implemented — call the writer three times with distinct patterns, save-buffer between each (forcing the before-save-hook chain), assert the buffer contains exactly one ':PROPERTIES:' and one ':END:' at point-min"))

  (it "no :PROPERTIES: drawer appears anywhere except point-min"
    (error
     "speculated; not implemented — after the writer runs, scan the buffer for ':PROPERTIES:' tokens and assert the only match is at point-min (or after a leading whitespace-only prefix)"))

  (it "the buffer's chat-mode #+begin_user / #+end_user blocks are unchanged"
    (error
     "speculated; not implemented — capture the chat-content portion before the writer call, run the writer, capture again, assert string-equal")))

(provide 'scope-drawer-no-duplication)
;;; scope-drawer-no-duplication.test.el ends here
