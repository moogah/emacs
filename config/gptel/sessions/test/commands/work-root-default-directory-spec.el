;;; work-root-default-directory-spec.el --- Work-root default-directory seam -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the work-root activation seam inside
;; `jf/gptel--bind-session-buffer' (design.md §Decisions D2/D3/D4;
;; register/boundary/work-root-activation-seam).  Verifies the single
;; `setq-local default-directory' the binder performs alongside the four
;; identity vars and before the registry `condition-case':
;;
;; 1. A drawer declaring `:GPTEL_WORK_ROOT: <path>' sets buffer-local
;;    `default-directory' to that path, normalized to an absolute
;;    directory with a trailing separator.
;; 2. A drawer that omits `:GPTEL_WORK_ROOT:' falls back to
;;    `jf/gptel--branch-dir' (the file's own directory) — byte-for-byte
;;    today's `find-file' behavior (a true no-op).
;; 3. A `GPTEL_WORK_ROOT' value without a trailing separator is
;;    normalized to carry one.
;;
;; Fixtures mirror `mode-hook-binder-spec.el': a real buffer carrying a
;; point-min `:PROPERTIES:' drawer with `buffer-file-name' set, so the
;; live-buffer signature scan and on-disk path resolution both work
;; without activating a major mode (the binder is invoked directly).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-workroot-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-workroot-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-workroot-test--registry-keys))

(defun jf-gptel-workroot-test--write-session (branch-dir &rest drawer-pairs)
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

(defun jf-gptel-workroot-test--buffer-from-file (session-file)
  "Return a fresh buffer visiting SESSION-FILE's contents.
The buffer has `buffer-file-name' set and the file's content inserted,
so the live-buffer signature scan and on-disk path resolution both work
without activating any major mode (the binder is invoked directly)."
  (let ((buf (generate-new-buffer "session.org")))
    (with-current-buffer buf
      (setq buffer-file-name session-file)
      (insert-file-contents session-file))
    buf))

(describe "jf/gptel--bind-session-buffer: work-root default-directory seam"

  (after-each
    (dolist (key jf-gptel-workroot-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-workroot-test--registry-keys nil))

  (it "sets default-directory from GPTEL_WORK_ROOT when the drawer declares it"
    (let ((temp-root (make-temp-file "gptel-workroot-set-" t))
          (buf nil))
      (unwind-protect
          (let* ((branch-dir
                  (expand-file-name
                   "wr-20260420000000/branches/main" temp-root))
                 (work-root (expand-file-name "proj" temp-root))
                 (session-file
                  (jf-gptel-workroot-test--write-session
                   branch-dir
                   "GPTEL_SESSION_ID" "wr-20260420000000"
                   "GPTEL_BRANCH" "main"
                   "GPTEL_WORK_ROOT" work-root)))
            (setq buf (jf-gptel-workroot-test--buffer-from-file session-file))
            (with-current-buffer buf
              (jf/gptel--bind-session-buffer)
              (jf-gptel-workroot-test--register-cleanup
               "wr-20260420000000" "main")
              ;; default-directory == work root, with trailing separator.
              (expect default-directory
                      :to-equal (file-name-as-directory work-root))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (file-directory-p temp-root)
          (delete-directory temp-root t)))))

  (it "falls back to branch-dir when the drawer omits GPTEL_WORK_ROOT (no-op)"
    (let ((temp-root (make-temp-file "gptel-workroot-fallback-" t))
          (buf nil))
      (unwind-protect
          (let* ((branch-dir
                  (expand-file-name
                   "wr-20260420000001/branches/main" temp-root))
                 (session-file
                  (jf-gptel-workroot-test--write-session
                   branch-dir
                   "GPTEL_SESSION_ID" "wr-20260420000001"
                   "GPTEL_BRANCH" "main")))
            (setq buf (jf-gptel-workroot-test--buffer-from-file session-file))
            (with-current-buffer buf
              (jf/gptel--bind-session-buffer)
              (jf-gptel-workroot-test--register-cleanup
               "wr-20260420000001" "main")
              ;; default-directory falls back to branch-dir.
              (expect (file-equal-p default-directory
                                    jf/gptel--branch-dir)
                      :to-be-truthy)
              (expect default-directory
                      :to-equal (file-name-as-directory
                                 (expand-file-name jf/gptel--branch-dir)))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (file-directory-p temp-root)
          (delete-directory temp-root t)))))

  (it "normalizes a GPTEL_WORK_ROOT value lacking a trailing separator"
    (let ((temp-root (make-temp-file "gptel-workroot-trailing-" t))
          (buf nil))
      (unwind-protect
          (let* ((branch-dir
                  (expand-file-name
                   "wr-20260420000002/branches/main" temp-root))
                 ;; Value written WITHOUT a trailing slash.
                 (work-root-bare (directory-file-name
                                  (expand-file-name "proj" temp-root)))
                 (session-file
                  (jf-gptel-workroot-test--write-session
                   branch-dir
                   "GPTEL_SESSION_ID" "wr-20260420000002"
                   "GPTEL_BRANCH" "main"
                   "GPTEL_WORK_ROOT" work-root-bare)))
            (setq buf (jf-gptel-workroot-test--buffer-from-file session-file))
            (with-current-buffer buf
              (jf/gptel--bind-session-buffer)
              (jf-gptel-workroot-test--register-cleanup
               "wr-20260420000002" "main")
              ;; Trailing separator present despite bare input.
              (expect (directory-name-p default-directory) :to-be-truthy)
              (expect default-directory
                      :to-equal (file-name-as-directory work-root-bare))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (file-directory-p temp-root)
          (delete-directory temp-root t))))))

(provide 'work-root-default-directory-spec)
;;; work-root-default-directory-spec.el ends here
