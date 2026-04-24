;;; session-restoration-spec.el --- Behavioral tests for session restoration (read-side) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests verifying the read-side of session restoration via
;; `jf/gptel--auto-init-session-buffer'.
;;
;; Under the chat-mode rename (Decision 16/18), session files are named
;; `session.org' (not `session.md').  The auto-init hook enables
;; `gptel-chat-mode' as the major mode and does NOT call `(gptel-mode 1)'.
;; It also does NOT call `gptel--save-state' or `gptel--restore-state' —
;; chat-mode's block format is self-describing and metadata.yml is the
;; authoritative preset source.
;;
;; Uses `with-seeded-files' macro from persistence-test-helpers to provide
;; mock file content for reads.  All custom persistence code runs for real;
;; only Emacs primitives (insert-file-contents, file-exists-p, file-directory-p)
;; and upstream gptel functions (gptel--apply-preset, gptel-get-preset,
;; gptel-chat-mode) are mocked.  yaml-parse-string runs for real against
;; seeded YAML content to preserve the round-trip parsing guarantee.
;;
;; Note: `with-seeded-files' only mocks file-exists-p for seeded paths and
;; delegates to real filesystem for others.  Since test paths don't exist on
;; disk, tests that use `with-seeded-files' must also mock file-exists-p
;; in the inner cl-letf to cover validation checks (e.g., session.org
;; existence for valid-branch-directory-p).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'yaml)

;; Ensure test directory is on load-path for require
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'persistence-test-helpers)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(describe "Session restoration (read-side)"

  (describe "Path pattern detection"

    (it "detects branch session from path pattern"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/my-session/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"my-session\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode) (lambda (&optional _) nil))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_target _linkname &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (expect jf/gptel--session-id :to-equal "my-session")
                (expect jf/gptel--branch-name :to-equal "main")))
          (remhash (jf/gptel--registry-key "my-session" "main") jf/gptel--session-registry)
          (kill-buffer buf))))

    (it "detects agent session from path pattern"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/my-session/branches/main/agents/researcher-20260101-task/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"agent-session\"\npreset: \"researcher\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode) (lambda (&optional _) nil))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_target _linkname &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (expect jf/gptel--session-id :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")))
          (kill-buffer buf))))

    (it "ignores non-session .org files"
      (let ((buf (generate-new-buffer "readme.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/home/user/projects/readme.org")
              (jf/gptel--auto-init-session-buffer)
              (expect jf/gptel--session-id :to-be nil))
          (kill-buffer buf))))

    (it "ignores session.org at wrong depth"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/my-session/session.org")
              (jf/gptel--auto-init-session-buffer)
              (expect jf/gptel--session-id :to-be nil))
          (kill-buffer buf)))))

  (describe "New session restoration"

    ;; Former "reads preset from metadata.yml and applies it" spec was
    ;; deleted when metadata.yml ceased to be authoritative — preset
    ;; application is now the drawer's job via gptel-chat-mode hook,
    ;; covered by config/gptel/sessions/test/commands/preset-application-spec.el.

    (it "sets buffer-local session variables"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (let ((metadata-path (expand-file-name
                                    "metadata.yml"
                                    "/sessions/test-sess/branches/main/")))
                (with-seeded-files
                    `((,metadata-path . "session_id: \"test-sess\"\npreset: \"executor\"\n"))
                  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                            ((symbol-function 'file-exists-p) (lambda (_) t))
                            ((symbol-function 'gptel-get-preset)
                             (lambda (_) '((gptel-model . "test"))))
                            ((symbol-function 'gptel--apply-preset)
                             (lambda (_name _setter) nil))
                            ((symbol-function 'gptel-chat-mode) (lambda (&optional _) nil))
                            ((symbol-function 'make-symbolic-link)
                             (lambda (_target _linkname &optional _ok) nil))
                            ((symbol-function 'delete-file)
                             (lambda (_f &optional _trash) nil)))
                    (jf/gptel--auto-init-session-buffer)
                    (expect jf/gptel--session-id :to-equal "test-sess")
                    (expect jf/gptel--session-dir :to-be-truthy)
                    (expect jf/gptel--branch-name :to-equal "main")
                    (expect jf/gptel--branch-dir :to-be-truthy)))))
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf))))

    (it "activates gptel-chat-mode after applying preset"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (let ((metadata-path (expand-file-name
                                    "metadata.yml"
                                    "/sessions/test-sess/branches/main/")))
                (with-seeded-files
                    `((,metadata-path . "session_id: \"test-sess\"\npreset: \"executor\"\n"))
                  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                            ((symbol-function 'file-exists-p) (lambda (_) t))
                            ((symbol-function 'gptel-get-preset)
                             (lambda (_) '((gptel-model . "test"))))
                            ((symbol-function 'gptel--apply-preset)
                             (lambda (_name _setter) nil))
                            ((symbol-function 'gptel-chat-mode)
                             (lambda (&optional _arg)
                               (setq chat-mode-called t)))
                            ((symbol-function 'make-symbolic-link)
                             (lambda (_target _linkname &optional _ok) nil))
                            ((symbol-function 'delete-file)
                             (lambda (_f &optional _trash) nil)))
                    (jf/gptel--auto-init-session-buffer)
                    (expect chat-mode-called :to-be t)))))
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf)))))

  (describe "Registry registration"

    (it "registers session in global registry after initialization"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (let ((metadata-path (expand-file-name
                                    "metadata.yml"
                                    "/sessions/test-sess/branches/main/")))
                (with-seeded-files
                    `((,metadata-path . "session_id: \"test-sess\"\npreset: \"executor\"\n"))
                  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                            ((symbol-function 'file-exists-p) (lambda (_) t))
                            ((symbol-function 'gptel-get-preset)
                             (lambda (_) '((gptel-model . "test"))))
                            ((symbol-function 'gptel--apply-preset)
                             (lambda (_name _setter) nil))
                            ((symbol-function 'gptel-chat-mode) (lambda (&optional _) nil))
                            ((symbol-function 'make-symbolic-link)
                             (lambda (_target _linkname &optional _ok) nil))
                            ((symbol-function 'delete-file)
                             (lambda (_f &optional _trash) nil)))
                    (jf/gptel--auto-init-session-buffer)
                    ;; Check registry has this session
                    (let ((key (jf/gptel--registry-key "test-sess" "main")))
                      (expect (gethash key jf/gptel--session-registry) :to-be-truthy))))))
          ;; Clean up registry
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf)))))

  (describe "Error handling"

    (it "handles missing metadata.yml gracefully for new sessions"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p)
                         (lambda (path)
                           ;; session.org exists (for valid-branch-directory-p)
                           ;; metadata.yml does not
                           (not (string-suffix-p "metadata.yml" path))))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _arg)
                           (setq chat-mode-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_target _linkname &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                ;; Should still activate chat-mode (fallback path)
                (expect chat-mode-called :to-be t)
                ;; Session vars should be set even without metadata
                (expect jf/gptel--session-id :to-equal "test-sess")))
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf))))

    (it "handles corrupt yaml in metadata.yml gracefully"
      (let ((buf (generate-new-buffer "session.org")))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (let ((metadata-path (expand-file-name
                                    "metadata.yml"
                                    "/sessions/test-sess/branches/main/")))
                (with-seeded-files
                    `((,metadata-path . "{{invalid yaml: ["))
                  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                            ((symbol-function 'file-exists-p) (lambda (_) t))
                            ((symbol-function 'gptel-chat-mode) (lambda (&optional _) nil))
                            ((symbol-function 'make-symbolic-link)
                             (lambda (_target _linkname &optional _ok) nil))
                            ((symbol-function 'delete-file)
                             (lambda (_f &optional _trash) nil)))
                    ;; Should not error — gracefully falls back
                    (jf/gptel--auto-init-session-buffer)
                    (expect jf/gptel--session-id :to-equal "test-sess")))))
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf))))

    (it "handles missing preset gracefully"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name "/sessions/test-sess/branches/main/session.org")
              (let ((metadata-path (expand-file-name
                                    "metadata.yml"
                                    "/sessions/test-sess/branches/main/")))
                (with-seeded-files
                    `((,metadata-path . "session_id: \"test-sess\"\npreset: \"nonexistent\"\n"))
                  (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                            ((symbol-function 'file-exists-p) (lambda (_) t))
                            ;; Preset not found
                            ((symbol-function 'gptel-get-preset) (lambda (_) nil))
                            ((symbol-function 'gptel-chat-mode)
                             (lambda (&optional _arg)
                               (setq chat-mode-called t)))
                            ((symbol-function 'make-symbolic-link)
                             (lambda (_target _linkname &optional _ok) nil))
                            ((symbol-function 'delete-file)
                             (lambda (_f &optional _trash) nil)))
                    (jf/gptel--auto-init-session-buffer)
                    ;; Should still activate chat-mode (fallback)
                    (expect chat-mode-called :to-be t)
                    (expect jf/gptel--session-id :to-equal "test-sess")))))
          (remhash (jf/gptel--registry-key "test-sess" "main") jf/gptel--session-registry)
          (kill-buffer buf))))))

(provide 'session-restoration-spec)
;;; session-restoration-spec.el ends here
