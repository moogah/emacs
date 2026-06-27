;;; discovery-drawer-spec.el --- Buttercup tests for drawer-sourced discovery -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Discovery (`jf/gptel--init-registry') keys the session registry on
;; IDENTITY read from each branch `session.org' point-min `:GPTEL_*:'
;; drawer, NOT on directory names.  Directory traversal still LOCATES the
;; files cheaply; a bounded drawer head-read per branch supplies the
;; authoritative `session-id' / `branch-name'.  See
;; register/boundary/drawer-first-identity-resolution and design.md §D7.
;;
;; Behavioral coverage (real on-disk fixture tree, no mocking of the
;; head-read or resolvers):
;;
;; - Drawer-sourced keys: a fixture whose directory basenames DIFFER
;;   from the drawer ids proves the registry keys on the DRAWER values,
;;   never the path (the whole point of path-independence).
;; - Legacy fallback: a branch whose drawer omits the identity keys (but
;;   still carries some `:GPTEL_*:' key, so it is a recognised session)
;;   falls back to the directory basename / branches/<branch>/ segment.
;; - Skip rule: a branch whose `session.org' carries NO `:GPTEL_' drawer
;;   at all is skipped from the registry (consistent with valid-*
;;   gating); registry count matches the number of valid GPTEL branches.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)
(require 'gptel-session-logging)
(require 'gptel-session-registry)

(defvar discovery-drawer-spec--tempdirs nil
  "Temp dirs created during the suite for unwind-cleanup.")

(defun discovery-drawer-spec--make-root ()
  "Create and track a fresh sessions-root temp dir."
  (let ((dir (make-temp-file "gptel-discovery-drawer-" t)))
    (push dir discovery-drawer-spec--tempdirs)
    dir))

(defun discovery-drawer-spec--cleanup ()
  (dolist (d discovery-drawer-spec--tempdirs)
    (when (file-directory-p d)
      (delete-directory d t)))
  (setq discovery-drawer-spec--tempdirs nil))

(defun discovery-drawer-spec--write-session-org (branch-dir drawer-lines)
  "Write a `session.org' into BRANCH-DIR with DRAWER-LINES.
DRAWER-LINES is a list of `(KEY . VALUE)' pairs emitted as
`:KEY: VALUE' inside a point-min `:PROPERTIES:' drawer.  When
DRAWER-LINES is nil, the file carries plain org content and NO
`:GPTEL_' drawer (the corrupt / partial case)."
  (make-directory branch-dir t)
  (let ((file (expand-file-name jf/gptel-session--context-file branch-dir)))
    (with-temp-file file
      (if drawer-lines
          (progn
            (insert ":PROPERTIES:\n")
            (dolist (kv drawer-lines)
              (insert (format ":%s: %s\n" (car kv) (cdr kv))))
            (insert ":END:\n\n"))
        (insert "* Just an ordinary org file\nNo gptel drawer here.\n")))
    file))

(defun discovery-drawer-spec--make-branch (root session-basename branch-basename
                                                drawer-lines)
  "Create ROOT/SESSION-BASENAME/branches/BRANCH-BASENAME with a session.org.
Returns the branch directory."
  (let ((branch-dir (expand-file-name
                     (format "%s/branches/%s" session-basename branch-basename)
                     root)))
    (discovery-drawer-spec--write-session-org branch-dir drawer-lines)
    branch-dir))

(describe "jf/gptel--init-registry (drawer-sourced discovery)"

  (after-each (discovery-drawer-spec--cleanup))

  (it "keys the registry on the DRAWER identity, not the directory basenames"
    ;; Path-independence proof: every directory basename deliberately
    ;; DIFFERS from the drawer id / branch it carries.
    (let ((jf/gptel-sessions-directory (discovery-drawer-spec--make-root)))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory
       "dir-basename-A" "dir-branch-A"
       '(("GPTEL_SESSION_ID" . "drawer-session-alpha")
         ("GPTEL_BRANCH" . "drawer-branch-main")))
      (jf/gptel--init-registry)
      (expect (jf/gptel-session-find "drawer-session-alpha" "drawer-branch-main")
              :not :to-be nil)
      ;; The path basenames must NOT appear as keys.
      (expect (jf/gptel-session-find "dir-basename-A" "dir-branch-A")
              :to-be nil)))

  (it "stores the located directories as :session-dir / :branch-dir"
    (let ((jf/gptel-sessions-directory (discovery-drawer-spec--make-root)))
      (let ((branch-dir
             (discovery-drawer-spec--make-branch
              jf/gptel-sessions-directory
              "dir-basename-B" "dir-branch-B"
              '(("GPTEL_SESSION_ID" . "drawer-session-beta")
                ("GPTEL_BRANCH" . "drawer-branch-feature")))))
        (jf/gptel--init-registry)
        (let ((plist (jf/gptel-session-find "drawer-session-beta"
                                            "drawer-branch-feature")))
          (expect plist :not :to-be nil)
          (expect (file-equal-p (plist-get plist :branch-dir) branch-dir)
                  :to-be t)
          (expect (file-equal-p
                   (plist-get plist :session-dir)
                   (expand-file-name "dir-basename-B" jf/gptel-sessions-directory))
                  :to-be t)))))

  (it "falls back to basename / segment when the drawer omits identity keys"
    ;; The drawer is present (carries GPTEL_PRESET, so it is a recognised
    ;; session) but omits GPTEL_SESSION_ID / GPTEL_BRANCH.  Identity then
    ;; falls back to the session directory basename and the
    ;; branches/<branch>/ path segment.
    (let ((jf/gptel-sessions-directory (discovery-drawer-spec--make-root)))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory
       "legacy-session-20251201090000" "main"
       '(("GPTEL_PRESET" . "default")))
      (jf/gptel--init-registry)
      (expect (jf/gptel-session-find "legacy-session-20251201090000" "main")
              :not :to-be nil)))

  (it "skips a branch whose session.org carries no GPTEL drawer at all"
    (let ((jf/gptel-sessions-directory (discovery-drawer-spec--make-root)))
      ;; One valid drawer branch + one corrupt (no-drawer) branch under
      ;; the same session.
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory
       "sess-C" "valid-branch"
       '(("GPTEL_SESSION_ID" . "drawer-session-gamma")
         ("GPTEL_BRANCH" . "good")))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory
       "sess-C" "corrupt-branch"
       nil)
      (jf/gptel--init-registry)
      (expect (jf/gptel-session-find "drawer-session-gamma" "good")
              :not :to-be nil)
      ;; The no-drawer branch contributes nothing under any key.
      (expect (jf/gptel--session-count) :to-equal 1)))

  (it "registry count matches the number of valid GPTEL branches"
    (let ((jf/gptel-sessions-directory (discovery-drawer-spec--make-root)))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory "s1" "b1"
       '(("GPTEL_SESSION_ID" . "id-1") ("GPTEL_BRANCH" . "main")))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory "s1" "b2"
       '(("GPTEL_SESSION_ID" . "id-1") ("GPTEL_BRANCH" . "feature")))
      (discovery-drawer-spec--make-branch
       jf/gptel-sessions-directory "s2" "b1"
       '(("GPTEL_SESSION_ID" . "id-2") ("GPTEL_BRANCH" . "main")))
      (jf/gptel--init-registry)
      (expect (jf/gptel--session-count) :to-equal 3))))

(provide 'discovery-drawer-spec)
;;; discovery-drawer-spec.el ends here
