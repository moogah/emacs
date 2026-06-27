;;; auto-init-resilience-spec.el --- Session-bind resilience to registry failure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression tests for `jf/gptel--bind-session-buffer' (the
;; content-addressed `gptel-chat-mode-hook' binder) pinning the
;; structural decoupling of identity-binding from the post-bind
;; registry/autosave work: a failure registering the buffer must NOT
;; swallow the four buffer-local session vars.
;;
;; Background: under the legacy find-file-hook auto-init pipeline a
;; broad `condition-case' once wrapped both mode activation and the
;; var-setting, so a failure zeroed out `jf/gptel--session-id' etc. as
;; collateral damage. Downstream consumers (PersistentAgent's
;; parent-session check, scope-validation lookups) then behaved as if
;; the buffer was not a session at all.
;;
;; The content-addressed binder sets the four `setq-local' vars FIRST,
;; then wraps only the registry registration + autosave-enable in a
;; narrow `condition-case'. A `jf/gptel--register-session' failure
;; therefore cannot abort the var-setting. This file pins that
;; invariant against the binder, the way it now reaches a session
;; buffer (`magic-mode-alist' signature -> `gptel-chat-mode' ->
;; `gptel-chat-mode-hook' -> binder).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-bind-resilience-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-bind-resilience-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-bind-resilience-test--registry-keys))

(defun jf-gptel-bind-resilience-test--write-session (branch-dir &rest drawer-pairs)
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

(defun jf-gptel-bind-resilience-test--buffer-from-file (session-file)
  "Return a fresh buffer visiting SESSION-FILE's contents.
The buffer has `buffer-file-name' set and the file's content inserted,
so the live-buffer signature scan and on-disk path resolution both work
without activating any major mode (the binder is invoked directly)."
  (let ((buf (generate-new-buffer "session.org")))
    (with-current-buffer buf
      (setq buffer-file-name session-file)
      (insert-file-contents session-file))
    buf))

(describe "jf/gptel--bind-session-buffer resilience to registry failure"

  (after-each
    (dolist (key jf-gptel-bind-resilience-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-bind-resilience-test--registry-keys nil))

  (describe "when jf/gptel--register-session errors"

    (it "still sets the four buffer-local session vars"
      (let ((temp-root (make-temp-file "gptel-bind-resilience-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "resilience-20260430000000/branches/main" temp-root))
                   (session-file
                    (jf-gptel-bind-resilience-test--write-session
                     branch-dir
                     "GPTEL_SESSION_ID" "resilience-20260430000000"
                     "GPTEL_BRANCH" "main"
                     "GPTEL_PRESET" "default")))
              (setq buf (jf-gptel-bind-resilience-test--buffer-from-file
                         session-file))
              (with-current-buffer buf
                (cl-letf (((symbol-function 'jf/gptel--register-session)
                           (lambda (&rest _) (error "Registry write failed"))))
                  (jf/gptel--bind-session-buffer))
                (jf-gptel-bind-resilience-test--register-cleanup
                 "resilience-20260430000000" "main")

                (expect jf/gptel--session-id
                        :to-equal "resilience-20260430000000")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t)))))

    (it "does not signal out of the binder (the failure is swallowed)"
      (let ((temp-root (make-temp-file "gptel-bind-resilience-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "resilience-20260430000001/branches/main" temp-root))
                   (session-file
                    (jf-gptel-bind-resilience-test--write-session
                     branch-dir
                     "GPTEL_SESSION_ID" "resilience-20260430000001"
                     "GPTEL_BRANCH" "main"
                     "GPTEL_PRESET" "default")))
              (setq buf (jf-gptel-bind-resilience-test--buffer-from-file
                         session-file))
              (with-current-buffer buf
                (cl-letf (((symbol-function 'jf/gptel--register-session)
                           (lambda (&rest _) (error "Registry write failed"))))
                  ;; The narrow condition-case around the registry work
                  ;; must absorb the error; binding the buffer must not
                  ;; raise.
                  (expect (jf/gptel--bind-session-buffer) :not :to-throw))
                (jf-gptel-bind-resilience-test--register-cleanup
                 "resilience-20260430000001" "main")))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

  (describe "negative control: when registry registration succeeds"

    (it "sets the four buffer-local session vars and enables autosave"
      (let ((temp-root (make-temp-file "gptel-bind-resilience-" t))
            (buf nil))
        (unwind-protect
            (let* ((branch-dir
                    (expand-file-name
                     "resilience-20260430000002/branches/main" temp-root))
                   (session-file
                    (jf-gptel-bind-resilience-test--write-session
                     branch-dir
                     "GPTEL_SESSION_ID" "resilience-20260430000002"
                     "GPTEL_BRANCH" "main"
                     "GPTEL_PRESET" "default")))
              (setq buf (jf-gptel-bind-resilience-test--buffer-from-file
                         session-file))
              (with-current-buffer buf
                (jf/gptel--bind-session-buffer)
                (jf-gptel-bind-resilience-test--register-cleanup
                 "resilience-20260430000002" "main")

                (expect jf/gptel--session-id
                        :to-equal "resilience-20260430000002")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)
                (expect jf/gptel-autosave-enabled :to-be t)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t)))))))

(provide 'auto-init-resilience-spec)
;;; auto-init-resilience-spec.el ends here
