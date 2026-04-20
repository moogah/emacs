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
(require 'cl-lib)

;; Load production modules directly from the sessions source tree so the
;; tests work whether run via run-tests.sh or a fresh Emacs session.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (sessions-dir (expand-file-name "../.." test-dir)))
  (load (expand-file-name "logging.el" sessions-dir) nil t)
  (load (expand-file-name "constants.el" sessions-dir) nil t)
  (load (expand-file-name "filesystem.el" sessions-dir) nil t))

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
      (expect jf/gptel-session--context-file :to-equal "session.org"))

    (it "does not name the conversation file session.md"
      (expect jf/gptel-session--context-file :not :to-equal "session.md")))

  (describe "jf/gptel--context-file-path"

    (it "builds a path ending in session.org"
      (let* ((branch-dir "/tmp/example/branches/main")
             (path (jf/gptel--context-file-path branch-dir)))
        (expect (file-name-nondirectory path) :to-equal "session.org")))

    (it "does not build a path ending in session.md"
      (let* ((branch-dir "/tmp/example/branches/main")
             (path (jf/gptel--context-file-path branch-dir)))
        (expect (file-name-nondirectory path) :not :to-equal "session.md"))))

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
                "/branches/main/session\\.org\\'")
        ;; And explicitly NOT session.md
        (expect ctx-path :not :to-match
                "/session\\.md\\'")))

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
        (expect ctx-path :to-match "/agents/.*/session\\.org\\'")
        (expect ctx-path :not :to-match "/session\\.md\\'"))))

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
                "\\`demo-20260420000000/branches/main/session\\.org\\'")))))

(provide 'directory-templates-spec)
;;; directory-templates-spec.el ends here
