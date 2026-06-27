;;; scope-no-security-key-in-plist.test.el --- Invariant scaffold -*- lexical-binding: t; -*-
;;
;; scaffolding-of: register/invariant/scope-no-security-key-in-plist
;; generated-at: 2026-04-29T11:05:33Z
;; license: implementor-may-revise

(require 'buttercup)

(describe "invariant: scope-no-security-key-in-plist"

  (it "loader output from a complete drawer omits :security at top level"
    (error
     "speculated; not implemented — fixture a complete :PROPERTIES: drawer (read+write+modify+execute+deny+cloud_auth+cloud_providers), call jf/gptel-scope--load-from-buffer, assert (null (plist-member result :security))"))

  (it "loader output from a minimal drawer omits :security at top level"
    (error
     "speculated; not implemented — fixture a drawer with only :GPTEL_SCOPE_READ:, assert (null (plist-member (jf/gptel-scope--load-from-buffer ...) :security))"))

  (it "file-fallback loader output also omits :security"
    (error
     "speculated; not implemented — write a session.org with a drawer to a tmpdir, call jf/gptel-scope--load-from-file, assert (null (plist-member result :security))"))

  (it "the loader's plist has exactly the top-level keys :paths and :cloud"
    (error
     "speculated; not implemented — assert (equal (sort (cl-loop for k in result by #'cddr collect k) #'string<) '(:cloud :paths))")))

(provide 'scope-no-security-key-in-plist)
;;; scope-no-security-key-in-plist.test.el ends here
