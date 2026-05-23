;;; auto-init-resilience-spec.el --- Auto-init resilience to mode/preset failure -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Regression tests for `jf/gptel--auto-init-session-buffer'
;; pinning the structural decoupling of session-detection from
;; preset-application: a failing preset (or any mode-activation
;; error) must NOT swallow the four buffer-local session vars.
;;
;; Background: the drawer-resident-preset path
;; (gptel-scope-in-org-properties Decisions 5/6/9) coupled mode
;; activation to preset application via `gptel-chat-mode-hook'.
;; Pre-fix, a broad `condition-case' wrapped both, so a failing
;; preset zeroed out `jf/gptel--session-id' etc. as collateral
;; damage. Downstream consumers (PersistentAgent's parent-session
;; check, scope-validation lookups) then behaved as if the buffer
;; was not a session at all.
;;
;; Post-fix, the `condition-case' is narrowed: only mode-flip is
;; wrapped; the four `setq-local' calls always run when path-shape
;; + directory validation succeeded. This file pins that
;; invariant.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-auto-init-resilience-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-auto-init-resilience-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-auto-init-resilience-test--registry-keys))

(describe "jf/gptel--auto-init-session-buffer resilience to mode/preset failure"

  (after-each
    (dolist (key jf-gptel-auto-init-resilience-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-auto-init-resilience-test--registry-keys nil))

  (describe "when --ensure-mode-once errors (e.g. preset references missing tool)"

    (it "still sets the four buffer-local session vars"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/resilience-20260430000000/branches/main/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'jf/gptel--ensure-mode-once)
                         (lambda () (error "Cannot find tool 'TodoWrite'")))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-resilience-test--register-cleanup
                 "resilience-20260430000000" "main")

                (expect jf/gptel--session-id
                        :to-equal "resilience-20260430000000")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)))
          (kill-buffer buf))))

    (it "still registers the buffer in the session registry"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/resilience-20260430000001/branches/main/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'jf/gptel--ensure-mode-once)
                         (lambda () (error "Preset application failed")))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-resilience-test--register-cleanup
                 "resilience-20260430000001" "main")

                (let ((key (jf/gptel--registry-key
                            "resilience-20260430000001" "main")))
                  (expect (gethash key jf/gptel--session-registry)
                          :to-be-truthy))))
          (kill-buffer buf))))

    (it "still sets jf/gptel-autosave-enabled"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/resilience-20260430000002/branches/main/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'jf/gptel--ensure-mode-once)
                         (lambda () (error "Preset application failed")))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-resilience-test--register-cleanup
                 "resilience-20260430000002" "main")

                (expect (bound-and-true-p jf/gptel-autosave-enabled)
                        :to-be t)))
          (kill-buffer buf)))))

  (describe "negative control: when --ensure-mode-once succeeds"

    (it "still sets the four buffer-local session vars (existing behaviour preserved)"
      (let ((buf (generate-new-buffer "session.org"))
            (mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/resilience-20260430000003/branches/main/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'jf/gptel--ensure-mode-once)
                         (lambda () (setq mode-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-resilience-test--register-cleanup
                 "resilience-20260430000003" "main")

                (expect mode-called :to-be t)
                (expect jf/gptel--session-id
                        :to-equal "resilience-20260430000003")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)))
          (kill-buffer buf))))))

(provide 'auto-init-resilience-spec)
;;; auto-init-resilience-spec.el ends here
