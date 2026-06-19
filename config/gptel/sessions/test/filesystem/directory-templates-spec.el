;;; directory-templates-spec.el --- Buttercup tests for session directory templates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify that session filesystem helpers place the conversation file
;; at `session.org' (not `session.md') for branches and agents. Tests
;; mount on a temporary directory and inspect the on-disk structure
;; directly rather than capturing I/O.
;;
;; Coverage:
;; - `jf/gptel-session--context-file' constant equals "session.org".
;; - `jf/gptel--context-file-path' builds `.../session.org' paths.
;; - Branch layout: `<session>/branches/<branch>/session.org'.
;; - Agent layout: `<session>/branches/<branch>/agents/<agent>/session.org'.
;; - `jf/gptel--valid-branch-directory-p' accepts `session.org' and
;;   rejects a directory whose only conversation file is `session.md'.

;;; Code:

(require 'buttercup)

;; Load production modules via the feature symbols they provide.
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)

(defvar jf-gptel-sessions-test--tempdirs nil
  "List of temporary directories created during tests for cleanup.")

(defun jf-gptel-sessions-test--make-tempdir ()
  "Create a fresh temporary directory and register it for cleanup."
  (let ((dir (make-temp-file "gptel-sessions-filesystem-test-" t)))
    (push dir jf-gptel-sessions-test--tempdirs)
    dir))

(describe "Session filesystem: directory templates use session.org"

  (after-each
    (dolist (dir jf-gptel-sessions-test--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-gptel-sessions-test--tempdirs nil))

  (describe "jf/gptel-session--context-file constant"

    (it "names the conversation file session.org"
      (expect jf/gptel-session--context-file :to-equal "session.org")))

  (describe "jf/gptel--context-file-path"

    (it "builds a path ending in session.org"
      (let* ((branch-dir "/tmp/example/branches/main")
             (path (jf/gptel--context-file-path branch-dir)))
        (expect (file-name-nondirectory path) :to-equal "session.org"))))

  (describe "Branch template: branches/<branch-name>/session.org"

    (it "places the conversation file at branches/main/session.org"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main"))
             (ctx-path (jf/gptel--context-file-path branch-dir)))
        ;; Directory structure exists
        (expect (file-directory-p session-dir) :to-be-truthy)
        (expect (file-directory-p branch-dir) :to-be-truthy)
        ;; Conversation file path ends in session.org under branches/main
        (expect ctx-path :to-match
                "/branches/main/session\\.org\\'")))

    (it "places the conversation file at branches/<named>/session.org for non-main branches"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory
                          session-dir "20260420120000-experiment"))
             (ctx-path (jf/gptel--context-file-path branch-dir)))
        (expect ctx-path :to-match
                "/branches/20260420120000-experiment/session\\.org\\'"))))

  (describe "Agent template: agents/<agent-name>/session.org"

    (it "places agent conversation files at agents/<agent>/session.org"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main"))
             (agent-dir (jf/gptel--create-agent-directory
                         branch-dir "researcher" "explore-api"))
             (ctx-path (jf/gptel--context-file-path agent-dir)))
        (expect (file-directory-p agent-dir) :to-be-truthy)
        (expect ctx-path :to-match "/agents/.*/session\\.org\\'"))))

  (describe "jf/gptel--valid-branch-directory-p"

    (it "returns t when session.org exists in the branch directory"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main")))
        ;; Touch session.org
        (with-temp-file (jf/gptel--context-file-path branch-dir)
          (insert "#+begin_user\n\n#+end_user\n"))
        (expect (jf/gptel--valid-branch-directory-p branch-dir) :to-be-truthy)))

    (it "returns nil when only a legacy session.md file is present"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main")))
        ;; Legacy markdown-era file, not picked up by helpers post-Decision 19.
        (with-temp-file (expand-file-name "session.md" branch-dir)
          (insert "###\n"))
        (expect (jf/gptel--valid-branch-directory-p branch-dir) :not :to-be-truthy))))

  (describe "Sessions root / branches tree integrity"

    (it "builds branches/<branch>/session.org from the sessions root"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (branch-dir (jf/gptel--create-branch-directory session-dir "main"))
             (ctx-path (jf/gptel--context-file-path branch-dir))
             (rel (file-relative-name ctx-path tempdir)))
        ;; Relative path from sessions root should be
        ;;   <session-id>/branches/main/session.org
        (expect rel :to-match
                "\\`demo-20260420000000/branches/main/session\\.org\\'")))

    (it "excludes branches without session.org from find-all-branches-with-agents"
      (let* ((tempdir (jf-gptel-sessions-test--make-tempdir))
             (jf/gptel-sessions-directory tempdir)
             (session-dir (jf/gptel--create-session-directory "demo-20260420000000"))
             (valid-branch (jf/gptel--create-branch-directory session-dir "main"))
             (legacy-branch (jf/gptel--create-branch-directory session-dir "legacy")))
        ;; Valid branch has session.org carrying an identity drawer:
        ;; discovery now reads identity from the point-min :GPTEL_*:
        ;; drawer, so a recognised branch must carry one (a no-drawer
        ;; session.org is skipped as corrupt/partial).
        (with-temp-file (jf/gptel--context-file-path valid-branch)
          (insert ":PROPERTIES:\n"
                  ":GPTEL_SESSION_ID: demo-20260420000000\n"
                  ":GPTEL_BRANCH: main\n"
                  ":END:\n\n#+begin_user\n\n#+end_user\n"))
        ;; Legacy branch has only session.md; it must not leak through
        ;; enumeration (Decision 19: clean break, no migration).
        (with-temp-file (expand-file-name "session.md" legacy-branch)
          (insert "###\n"))
        (let ((names (mapcar (lambda (entry) (plist-get entry :branch-name))
                             (jf/gptel--find-all-branches-with-agents))))
          (expect names :to-equal '("main")))))))

(provide 'directory-templates-spec)
;;; directory-templates-spec.el ends here
