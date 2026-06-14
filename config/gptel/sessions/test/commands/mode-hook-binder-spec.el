;;; mode-hook-binder-spec.el --- Content-addressed session binder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for `jf/gptel--bind-session-buffer', the
;; content-addressed `gptel-chat-mode-hook' binder (design.md §Decision
;; D4; register/invariant/activation-and-identity-are-content-not-path,
;; BINDING half).  Verifies:
;;
;; 1. Activating chat-mode in a signature-bearing buffer (a `:GPTEL_*:'
;;    drawer at point-min) sets the four buffer-local session vars from
;;    drawer-resolved identity, registers the buffer in
;;    `jf/gptel--session-registry', and enables `jf/gptel-autosave-enabled'.
;; 2. A buffer with no `:GPTEL_' drawer (scratch chat) is a no-op: no
;;    registry entry, no session vars.
;; 3. `jf/gptel--branch-dir' equals `(file-name-directory (buffer-file-name))'
;;    — the file's own directory, with no `../..' walk.
;;
;; The binder guards on the buffer CONTENT signature
;; (`jf/gptel--session-signature-p'), which scans the live buffer, so
;; fixtures populate a real buffer with a point-min `:PROPERTIES:' drawer
;; and a `buffer-file-name'.  Identity resolution and the session-dir
;; marker walk read the real path on disk, so fixtures lay the
;; `branches/<branch>/session.org' tree under a temp root.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-bind-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-bind-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-bind-test--registry-keys))

(defun jf-gptel-bind-test--write-session (branch-dir &rest drawer-pairs)
  "Write a `session.org' into BRANCH-DIR carrying DRAWER-PAIRS.
DRAWER-PAIRS is a flat list of (KEY VALUE ...) strings written as
`:KEY: VALUE' drawer lines.  Returns the absolute session.org path."
  (make-directory branch-dir t)
  (let ((session-file (expand-file-name "session.org" branch-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n")
      (cl-loop for (key value) on drawer-pairs by #'cddr
               do (insert (format ":%s: %s\n" key value)))
      (insert ":END:\n"
              "\n"
              "#+begin_user\n"
              "hello\n"
              "#+end_user\n"))
    session-file))

(defun jf-gptel-bind-test--buffer-from-file (session-file)
  "Return a fresh buffer visiting SESSION-FILE's contents.
The buffer has `buffer-file-name' set and the file's content inserted,
so the live-buffer signature scan and on-disk path resolution both work
without activating any major mode (the binder is invoked directly)."
  (let ((buf (generate-new-buffer "session.org")))
    (with-current-buffer buf
      (setq buffer-file-name session-file)
      (insert-file-contents session-file))
    buf))

(describe "jf/gptel--bind-session-buffer"

  (after-each
    (dolist (key jf-gptel-bind-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-bind-test--registry-keys nil))

  (describe "signature-bearing session buffer"

    (it "sets the four buffer-locals from drawer identity, registers, enables autosave"
      (let ((temp-root (make-temp-file "gptel-bind-branch-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "foo-20260420000000/branches/main" temp-root))
                   (session-file
                    (jf-gptel-bind-test--write-session
                     branch-dir
                     "GPTEL_SESSION_ID" "foo-20260420000000"
                     "GPTEL_BRANCH" "main"
                     "GPTEL_PRESET" "default")))
              (setq buf (jf-gptel-bind-test--buffer-from-file session-file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                (jf-gptel-bind-test--register-cleanup
                 "foo-20260420000000" "main")

                ;; Four buffer-local session vars set from the drawer.
                (expect jf/gptel--session-id
                        :to-equal "foo-20260420000000")
                (expect jf/gptel--branch-name :to-equal "main")
                ;; session-dir is the structural `branches/' ancestor;
                ;; NOT truename-normalized, so compare with file-equal-p.
                (expect (file-equal-p
                         jf/gptel--session-dir
                         (expand-file-name
                          "foo-20260420000000" temp-root))
                        :to-be-truthy)
                ;; branch-dir is the file's own directory.
                (expect (file-equal-p jf/gptel--branch-dir branch-dir)
                        :to-be-truthy)

                ;; Registry entry created.
                (let ((key (jf/gptel--registry-key
                            "foo-20260420000000" "main")))
                  (expect (gethash key jf/gptel--session-registry)
                          :to-be-truthy))

                ;; Autosave enabled.
                (expect jf/gptel-autosave-enabled :to-be t)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t)))))

    (it "resolves a legacy session (no GPTEL_BRANCH/GPTEL_SESSION_ID) from the path"
      ;; Drawer carries a :GPTEL_ key (so the signature matches) but
      ;; omits the identity keys; both id and branch fall back to the
      ;; path.  The branch-name fallback regex needs the FILE path's
      ;; trailing `branches/<branch>/' segment — exercising the caller
      ;; obligation in register/boundary/drawer-first-identity-resolution.
      (let ((temp-root (make-temp-file "gptel-bind-legacy-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "legacy-20260101000000/branches/feature-x" temp-root))
                   (session-file
                    (jf-gptel-bind-test--write-session
                     branch-dir "GPTEL_PRESET" "default")))
              (setq buf (jf-gptel-bind-test--buffer-from-file session-file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                (jf-gptel-bind-test--register-cleanup
                 "legacy-20260101000000" "feature-x")
                (expect jf/gptel--session-id
                        :to-equal "legacy-20260101000000")
                (expect jf/gptel--branch-name :to-equal "feature-x")))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

  (describe "branch-dir derivation"

    (it "equals (file-name-directory (buffer-file-name)) — no ../.. walk"
      (let ((temp-root (make-temp-file "gptel-bind-bdir-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "bar-20260420000000/branches/feature-y" temp-root))
                   (session-file
                    (jf-gptel-bind-test--write-session
                     branch-dir
                     "GPTEL_SESSION_ID" "bar-20260420000000"
                     "GPTEL_BRANCH" "feature-y")))
              (setq buf (jf-gptel-bind-test--buffer-from-file session-file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                (jf-gptel-bind-test--register-cleanup
                 "bar-20260420000000" "feature-y")
                (expect (file-equal-p
                         jf/gptel--branch-dir
                         (file-name-directory (buffer-file-name)))
                        :to-be-truthy)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

  (describe "agent session"

    (it "uses branch-dir itself as session-dir (agents do not branch)"
      (let ((temp-root (make-temp-file "gptel-bind-agent-" t))
            (buf nil))
        (unwind-protect
            (let* ((agent-dir
                    (expand-file-name
                     "qux-20260420000000/branches/main/agents/researcher-1"
                     temp-root))
                   (session-file
                    (jf-gptel-bind-test--write-session
                     agent-dir
                     "GPTEL_SESSION_ID" "qux-20260420000000"
                     "GPTEL_PARENT_SESSION_ID" "parent-20260101000000")))
              (setq buf (jf-gptel-bind-test--buffer-from-file session-file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                ;; branch-name resolves from the path's branches/main/
                ;; segment for cleanup keying.
                (jf-gptel-bind-test--register-cleanup
                 "qux-20260420000000" jf/gptel--branch-name)
                ;; For an agent, session-dir == branch-dir (no walk).
                (expect (file-equal-p jf/gptel--session-dir
                                      jf/gptel--branch-dir)
                        :to-be-truthy)
                (expect (file-equal-p jf/gptel--branch-dir agent-dir)
                        :to-be-truthy)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

  (describe "non-session buffer (no :GPTEL_ drawer)"

    (it "is a no-op: no session vars, no registry entry (scratch chat)"
      (let ((buf (generate-new-buffer "*scratch-chat*"))
            (registry-count-before
             (hash-table-count jf/gptel--session-registry)))
        (unwind-protect
            (with-current-buffer buf
              ;; A scratch chat buffer: visiting no session file, no drawer.
              (insert "#+begin_user\nhello\n#+end_user\n")
              (jf/gptel--bind-session-buffer)
              (expect jf/gptel--session-id :to-be nil)
              (expect jf/gptel--session-dir :to-be nil)
              (expect jf/gptel--branch-name :to-be nil)
              (expect jf/gptel--branch-dir :to-be nil)
              (expect jf/gptel-autosave-enabled :to-be nil)
              (expect (hash-table-count jf/gptel--session-registry)
                      :to-equal registry-count-before))
          (kill-buffer buf))))

    (it "is a no-op for a file-visiting org buffer that only mentions :GPTEL_ in prose"
      (let ((temp-root (make-temp-file "gptel-bind-noop-" t))
            (buf nil))
        (unwind-protect
            (let* ((file (expand-file-name "notes.org" temp-root)))
              (with-temp-file file
                (insert "* Notes\n"
                        "Here I mention :GPTEL_SESSION_ID: in prose.\n"))
              (setq buf (jf-gptel-bind-test--buffer-from-file file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                (expect jf/gptel--session-id :to-be nil)
                (expect jf/gptel--branch-dir :to-be nil)
                (expect jf/gptel-autosave-enabled :to-be nil)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t)))))))

(provide 'mode-hook-binder-spec)
;;; mode-hook-binder-spec.el ends here
