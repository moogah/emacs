;;; identity-resolution-spec.el --- Buttercup tests for drawer-first identity resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify drawer-first / basename-fallback resolution of session
;; identity (register/boundary/drawer-first-identity-resolution):
;;
;; - `jf/gptel--resolve-session-id': drawer GPTEL_SESSION_ID wins; absent
;;   => fall back to `jf/gptel--session-id-from-directory' (the directory
;;   basename). When the drawer carries the key, the path is NEVER read
;;   (proven with a fixture whose basename differs from the drawer id).
;;
;; - `jf/gptel--resolve-branch-name': drawer GPTEL_BRANCH wins; absent =>
;;   fall back to the enclosing branches/<branch>/ path segment.
;;
;; - `jf/gptel--session-type': GPTEL_PARENT_SESSION_ID present => `agent';
;;   absent => `branch'. This is the only allowed discriminator of session
;;   type (no path-layout cond), per
;;   register/vocabulary/identity-drawer-keys.
;;
;; The drawer-alist consumed here is exactly the shape produced by
;; `jf/gptel--read-session-drawer-head' / `jf/gptel--scan-session-drawer-keys':
;; an alist keyed by the bare key string with string values, e.g.
;; (("GPTEL_SESSION_ID" . "abc") ("GPTEL_BRANCH" . "main")), or nil when
;; there is no point-min drawer at all (legacy / no-migration grace path).

;;; Code:

(require 'buttercup)
(require 'gptel-session-filesystem)

(describe "jf/gptel--resolve-session-id"

  (it "returns the drawer GPTEL_SESSION_ID when present"
    (let ((drawer '(("GPTEL_SESSION_ID" . "react-refactoring-20260120153042")
                    ("GPTEL_BRANCH" . "main"))))
      (expect (jf/gptel--resolve-session-id
               drawer "/sessions/react-refactoring-20260120153042")
              :to-equal "react-refactoring-20260120153042")))

  (it "ignores the directory basename when the drawer carries the id"
    ;; Independence proof: the drawer id and the directory basename are
    ;; deliberately DIFFERENT. The drawer value must win regardless.
    (let ((drawer '(("GPTEL_SESSION_ID" . "canonical-id-from-drawer"))))
      (expect (jf/gptel--resolve-session-id
               drawer "/sessions/totally-different-basename")
              :to-equal "canonical-id-from-drawer")))

  (it "falls back to the directory basename when the drawer omits the id"
    (let ((drawer '(("GPTEL_BRANCH" . "main"))))
      (expect (jf/gptel--resolve-session-id
               drawer "/sessions/legacy-session-20251201090000")
              :to-equal "legacy-session-20251201090000")))

  (it "falls back to the directory basename when the drawer is nil (legacy)"
    (expect (jf/gptel--resolve-session-id
             nil "/sessions/pre-migration-session/")
            :to-equal "pre-migration-session")))

(describe "jf/gptel--resolve-branch-name"

  (it "returns the drawer GPTEL_BRANCH when present"
    (let ((drawer '(("GPTEL_SESSION_ID" . "s1")
                    ("GPTEL_BRANCH" . "feature-x"))))
      (expect (jf/gptel--resolve-branch-name
               drawer "/sessions/s1/branches/main/session.org")
              :to-equal "feature-x")))

  (it "ignores the path segment when the drawer carries the branch"
    ;; Independence proof: drawer branch differs from the path segment.
    (let ((drawer '(("GPTEL_BRANCH" . "drawer-branch"))))
      (expect (jf/gptel--resolve-branch-name
               drawer "/sessions/s1/branches/path-branch/session.org")
              :to-equal "drawer-branch")))

  (it "falls back to the branches/<branch>/ path segment when the drawer omits it"
    (let ((drawer '(("GPTEL_SESSION_ID" . "s1"))))
      (expect (jf/gptel--resolve-branch-name
               drawer "/sessions/s1/branches/20260128153042-feature/session.org")
              :to-equal "20260128153042-feature")))

  (it "falls back to the path segment when the drawer is nil (legacy)"
    (expect (jf/gptel--resolve-branch-name
             nil "/sessions/s1/branches/main/session.org")
            :to-equal "main"))

  (it "returns nil when neither the drawer nor the path supplies a branch"
    (expect (jf/gptel--resolve-branch-name
             nil "/sessions/s1/no-branch-segment/session.org")
            :to-equal nil)))

(describe "jf/gptel--session-type"

  (it "returns agent when the drawer carries GPTEL_PARENT_SESSION_ID"
    (let ((drawer '(("GPTEL_SESSION_ID" . "agent-id")
                    ("GPTEL_PARENT_SESSION_ID" . "parent-id")
                    ("GPTEL_BRANCH" . "main"))))
      (expect (jf/gptel--session-type drawer) :to-equal 'agent)))

  (it "returns branch when the drawer omits GPTEL_PARENT_SESSION_ID"
    (let ((drawer '(("GPTEL_SESSION_ID" . "s1")
                    ("GPTEL_BRANCH" . "main"))))
      (expect (jf/gptel--session-type drawer) :to-equal 'branch)))

  (it "returns branch for a nil drawer (legacy)"
    (expect (jf/gptel--session-type nil) :to-equal 'branch)))

(provide 'identity-resolution-spec)
;;; identity-resolution-spec.el ends here
