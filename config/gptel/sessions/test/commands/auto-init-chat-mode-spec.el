;;; auto-init-chat-mode-spec.el --- Auto-init hook under chat-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for `jf/gptel--auto-init-session-buffer' after the
;; drawer-authoritative rework (design.md §Decisions 5, 6, 9).
;; Verifies:
;;
;; 1. Opening `~/.gptel/sessions/foo-20260420000000/branches/main/session.org'
;;    activates `gptel-chat-mode', sets the four path-derived
;;    buffer-local session vars, registers the buffer in the registry,
;;    and does NOT call `gptel-mode'.
;; 2. Agent-path recognition (`.../agents/<name>/session.org').
;; 3. A `.org' file at an unrelated path does NOT fire auto-init; no
;;    session vars are set; no registry entry is created.
;; 4. `gptel--save-state' / `gptel--restore-state' are NOT called during
;;    auto-init.
;; 5. `jf/gptel--parent-session-id' is populated from the drawer via
;;    the chat-mode hook (not from `metadata.yml').
;;
;; Fixtures write a `session.org' on disk with a `:PROPERTIES:' drawer
;; (NOT a `metadata.yml') for tests that exercise drawer-driven
;; behaviour.  Unit-level tests that stub `gptel-chat-mode' use
;; in-memory buffers without any on-disk session file.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)
(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel)

(defvar jf-gptel-auto-init-test--registry-keys nil
  "Registry keys to clean up after each example.")

(defun jf-gptel-auto-init-test--register-cleanup (session-id branch-name)
  "Remember (SESSION-ID, BRANCH-NAME) for `after-each' registry cleanup."
  (push (jf/gptel--registry-key session-id branch-name)
        jf-gptel-auto-init-test--registry-keys))

(defun jf-gptel-auto-init-test--write-session-with-drawer
    (branch-dir preset &optional parent-id)
  "Write a `session.org' with a PROPERTIES drawer into BRANCH-DIR.
PRESET is a symbol written as `:GPTEL_PRESET:'.  PARENT-ID, when
non-nil, is written as `:GPTEL_PARENT_SESSION_ID:'.  Returns the
absolute path to the created file."
  (make-directory branch-dir t)
  (let ((session-file (expand-file-name "session.org" branch-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n")
      (insert (format ":GPTEL_PRESET: %s\n" preset))
      (when parent-id
        (insert (format ":GPTEL_PARENT_SESSION_ID: %s\n" parent-id)))
      (insert ":END:\n"
              "\n"
              "#+begin_user\n"
              "hello\n"
              "#+end_user\n"))
    session-file))

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

                ;; Four path-derived buffer-local session variables set.
                (expect jf/gptel--session-id :to-equal "foo-20260420000000")
                (expect jf/gptel--session-dir :to-be-truthy)
                (expect jf/gptel--branch-name :to-equal "main")
                (expect jf/gptel--branch-dir :to-be-truthy)
                ;; Parent-session-id stays nil for top-level sessions
                ;; (drawer not read because gptel-chat-mode is stubbed).
                (expect (bound-and-true-p jf/gptel--parent-session-id)
                        :to-be nil)

                ;; Registry entry created.
                (let ((key (jf/gptel--registry-key
                            "foo-20260420000000" "main")))
                  (expect (gethash key jf/gptel--session-registry)
                          :to-be-truthy))))
          (kill-buffer buf)))))

  (describe "agent session path"

    (it "opens nested */branches/<branch>/agents/<name>/session.org: session-id and branch-name come from the path"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil)
            (gptel-mode-called nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/foo-20260420000000/branches/feature-x/agents/researcher-20260420120000-explore/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
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
                ;; session-id comes from the parent session directory
                ;; (captured by the nested-agent regex), NOT from the
                ;; agent's own directory name.
                (expect jf/gptel--session-id :to-equal "foo-20260420000000")
                ;; branch-name comes from the branches/<branch>/
                ;; segment, NOT hardcoded to "main".
                (expect jf/gptel--branch-name :to-equal "feature-x")))
          (kill-buffer buf))))

    (it "opens flat */agents/<name>/session.org (legacy): session-id from path, branch-name defaults to main"
      (let ((buf (generate-new-buffer "session.org"))
            (chat-mode-called nil)
            (symlink-updated nil))
        (unwind-protect
            (with-current-buffer buf
              (setq buffer-file-name
                    (expand-file-name
                     "~/.gptel/sessions/foo-20260420000000/agents/researcher-20260420120000-explore/session.org"))
              (cl-letf (((symbol-function 'file-directory-p) (lambda (_) t))
                        ((symbol-function 'file-exists-p) (lambda (_) t))
                        ((symbol-function 'gptel-chat-mode)
                         (lambda (&optional _) (setq chat-mode-called t)))
                        ((symbol-function 'jf/gptel--update-current-symlink)
                         (lambda (&rest _) (setq symlink-updated t)))
                        ((symbol-function 'make-symbolic-link)
                         (lambda (_t _l &optional _ok) nil))
                        ((symbol-function 'delete-file)
                         (lambda (_f &optional _trash) nil)))
                (jf/gptel--auto-init-session-buffer)
                (when jf/gptel--session-id
                  (jf-gptel-auto-init-test--register-cleanup
                   jf/gptel--session-id jf/gptel--branch-name))

                (expect chat-mode-called :to-be t)
                (expect jf/gptel--session-id :to-equal "foo-20260420000000")
                (expect jf/gptel--branch-name :to-equal "main")
                ;; Legacy flat layout has no branches/ directory, so
                ;; the auto-init hook MUST NOT invoke
                ;; jf/gptel--update-current-symlink on the agent dir
                ;; (which would create a stray `current ->
                ;; branches/main' dangling symlink).
                (expect symlink-updated :to-be nil)))
          (kill-buffer buf))))

    (it "populates jf/gptel--parent-session-id from drawer for a branch session"
      ;; Under the drawer-authoritative design, parent-session-id is
      ;; written to the session.org drawer and read by the chat-mode
      ;; hook at activation — NOT by auto-init reading metadata.yml.
      ;; Drive this test through the real `find-file' path with a real
      ;; `session.org' on disk so `gptel-chat-mode-hook' fires.
      (let ((temp-root (make-temp-file "gptel-auto-init-parent-" t))
            (buf nil)
            (child-preset 'auto-init-parent-preset))
        (unwind-protect
            (progn
              (gptel-make-preset child-preset :temperature 0.42)
              (let* ((branch-dir
                      (expand-file-name
                       "bar-20260420000000/branches/feature-x" temp-root))
                     (session-file
                      (jf-gptel-auto-init-test--write-session-with-drawer
                       branch-dir child-preset "foo-20260420000000")))
                (setq buf (find-file-noselect session-file))
                (with-current-buffer buf
                  (jf-gptel-auto-init-test--register-cleanup
                   "bar-20260420000000" "feature-x")
                  (expect jf/gptel--session-id
                          :to-equal "bar-20260420000000")
                  (expect jf/gptel--branch-name :to-equal "feature-x")
                  (expect jf/gptel--parent-session-id
                          :to-equal "foo-20260420000000"))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (set-buffer-modified-p nil))
            (kill-buffer buf))
          (setq gptel--known-presets
                (assq-delete-all child-preset gptel--known-presets))
          (when (file-directory-p temp-root)
            (delete-directory temp-root t))))))

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
      ;; (simulating a property drawer having already been processed
      ;; by the chat-mode hook), auto-init MUST NOT go through the
      ;; upstream restore-state round-trip. The drawer is authoritative.
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
