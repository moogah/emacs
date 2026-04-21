;;; auto-init-chat-mode-spec.el --- Auto-init hook under chat-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for `jf/gptel--auto-init-session-buffer' after the
;; Decision 16/18 rename.  Verifies:
;;
;; 1. Opening `~/.gptel/sessions/foo-20260420000000/branches/main/session.org'
;;    activates `gptel-chat-mode', sets the five buffer-local session
;;    vars, registers the buffer in the registry, and does NOT call
;;    `gptel-mode'.
;; 2. Agent-path recognition (`.../agents/<name>/session.org').
;; 3. A `.org' file at an unrelated path does NOT fire auto-init; no
;;    session vars are set; no registry entry is created.
;; 4. `gptel--save-state' / `gptel--restore-state' are NOT called during
;;    auto-init.
;;
;; Mocks upstream `gptel-get-preset', `gptel--apply-preset',
;; `gptel-chat-mode', `gptel-mode', `gptel--save-state',
;; `gptel--restore-state', and filesystem primitives so the hook runs
;; against seeded state without touching disk or real package defs.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'yaml)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-metadata)
(require 'gptel-session-registry)
(require 'gptel-session-commands)

(defvar jf-gptel-auto-init-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-auto-init-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-auto-init-test--registry-keys))

(describe "jf/gptel--auto-init-session-buffer under chat-mode"

  (after-each
    (dolist (key jf-gptel-auto-init-test--registry-keys)
      (remhash key jf/gptel--session-registry))
    (setq jf-gptel-auto-init-test--registry-keys nil))

  (describe "branch session path"

    (it "opens *.../branches/main/session.org: activates chat-mode, sets vars, registers"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil)
            (gptel-mode-called nil)
            (save-state-called nil)
            (restore-state-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/foo-20260420000000/branches/main/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"foo-20260420000000\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) (setq chat-mode-called t)))
                        ((symbol-function 'gptel-mode)
                         (lambda (&optional _) (setq gptel-mode-called t)))
                        ((symbol-function 'gptel--save-state)
                         (lambda (&rest _) (setq save-state-called t)))
                        ((symbol-function 'gptel--restore-state)
                         (lambda (&rest _) (setq restore-state-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-test--register-cleanup
                 "foo-20260420000000" "main")

                ;; Chat-mode activated.
                (expect chat-mode-called :to-be t)
                ;; gptel-mode was NOT called.
                (expect gptel-mode-called :to-be nil)
                ;; gptel--save-state / gptel--restore-state NOT called.
                (expect save-state-called :to-be nil)
                (expect restore-state-called :to-be nil)

                ;; Five buffer-local session variables set.
                (expect jf/gptel--session-id :to-equal "foo-20260420000000")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)
                ;; Parent-session-id stays nil for top-level sessions
                ;; (buffer-local default).
                (expect (bound-and-true-p jf/gptel--parent-session-id)
                        :to-be nil)

                ;; Registry entry created.
                (let ((key (jf/gptel--registry-key
                            "foo-20260420000000" "main")))
                  (expect (gethash key jf/gptel--session-registry)
                          :to-be-truthy))))
          (kill-buffer buf)))))

  (describe "agent session path"

    (it "opens *.../agents/<name>/session.org: same wiring with branch-name=main"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil)
            (gptel-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/foo-20260420000000/branches/main/agents/researcher-20260420120000-explore/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"agent-42\"\npreset: \"researcher\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) (setq chat-mode-called t)))
                        ((symbol-function 'gptel-mode)
                         (lambda (&optional _) (setq gptel-mode-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (when jf/gptel--session-id
                  (jf-gptel-auto-init-test--register-cleanup
                   jf/gptel--session-id jf/gptel--branch-name))

                (expect chat-mode-called :to-be t)
                (expect gptel-mode-called :to-be nil)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--session-id :to-be-truthy)))
          (kill-buffer buf)))))

  (describe "non-session .org files"

    (it "does not fire auto-init for ~/notes/chat.org"
      (let ((buf (generate-new-buffer "chat.org"))
            (chat-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name (expand-file-name "~/notes/chat.org"))
              (cl-letf (((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) (setq chat-mode-called t))))
                (jf/gptel--auto-init-session-buffer)
                ;; No session vars set.
                (expect jf/gptel--session-id :to-be nil)
                (expect jf/gptel--session-dir :to-be nil)
                (expect jf/gptel--branch-name :to-be nil)
                (expect jf/gptel--branch-dir :to-be nil)
                ;; Auto-init did not call gptel-chat-mode (that path is
                ;; the caller's responsibility; the hook only fires on
                ;; session-file paths).
                (expect chat-mode-called :to-be nil)
                ;; Registry key absent (sanity: registry entries are
                ;; keyed on session-id/branch-name; nothing was added).
                (expect (hash-table-count jf/gptel--session-registry)
                        :to-equal 0)))
          (kill-buffer buf)))))

  (describe "persistence side-effects are suppressed"

    (it "does not call gptel--save-state or gptel--restore-state for a new session"
      (let ((buf (generate-new-buffer "session.org"))
            (save-state-called nil)
            (restore-state-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-abc/branches/main/session.org")
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-abc\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) nil))
                        ((symbol-function 'gptel--save-state)
                         (lambda (&rest _) (setq save-state-called t)))
                        ((symbol-function 'gptel--restore-state)
                         (lambda (&rest _) (setq restore-state-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-test--register-cleanup "sess-abc" "main")
                (expect save-state-called :to-be nil)
                (expect restore-state-called :to-be nil)))
          (kill-buffer buf))))

    (it "does not call gptel--save-state or gptel--restore-state for an existing session"
      ;; Even when gptel--preset is already set as a buffer-local
      ;; (simulating a property drawer or file-local having already
      ;; been processed by Emacs), auto-init MUST NOT go through the
      ;; upstream restore-state round-trip. metadata.yml is authoritative.
      (let ((buf (generate-new-buffer "session.org"))
            (save-state-called nil)
            (restore-state-called nil)
            (chat-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    "/sessions/sess-xyz/branches/main/session.org")
              (setq-local gptel--preset 'drawer-preset)
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'insert-file-contents)
                         (lambda (f &rest _)
                           (insert "session_id: \"sess-xyz\"\npreset: \"executor\"\n")
                           (list f 0)))
                        ((symbol-function 'gptel-get-preset)
                         (lambda (_) '((gptel-model . "test"))))
                        ((symbol-function 'gptel--apply-preset)
                         (lambda (_name _setter) nil))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) (setq chat-mode-called t)))
                        ((symbol-function 'gptel--save-state)
                         (lambda (&rest _) (setq save-state-called t)))
                        ((symbol-function 'gptel--restore-state)
                         (lambda (&rest _) (setq restore-state-called t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (jf-gptel-auto-init-test--register-cleanup "sess-xyz" "main")
                (expect save-state-called :to-be nil)
                (expect restore-state-called :to-be nil)
                (expect chat-mode-called :to-be t)))
          (kill-buffer buf))))))

(provide 'auto-init-chat-mode-spec)
;;; auto-init-chat-mode-spec.el ends here
