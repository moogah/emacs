;;; scope-add-pattern-idempotent.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/scope-add-pattern-idempotent
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise

(require 'buttercup)

(describe "invariant: scope-add-pattern-idempotent"

  (it "writing a pattern that already exists leaves the buffer byte-for-byte unchanged"
    (error
     "speculated; not implemented — fixture a buffer with :GPTEL_SCOPE_READ: /a/**, call (jf/gptel-scope--write-pattern-to-drawer buf :read \"/a/**\"), assert (string= before after)"))

  (it "the callback reports :success t with empty :patterns_added on dedup"
    (error
     "speculated; not implemented — capture the writer's callback result; assert (eq t (plist-get cb :success)) and (null (plist-get cb :patterns_added))"))

  (it "writing a not-yet-present pattern returns :patterns_added with that pattern"
    (error
     "speculated; not implemented — fixture an empty :GPTEL_SCOPE_READ:, call writer with /b/**, assert callback's :patterns_added contains exactly \"/b/**\""))

  (it "save-buffer is not called on a no-op write"
    (error
     "speculated; not implemented — spy on save-buffer; assert it was not called when the writer dedups; the user's undo ring should not gain a spurious entry")))

(provide 'scope-add-pattern-idempotent)
;;; scope-add-pattern-idempotent.test.el ends here
