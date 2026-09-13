;;; hidden-agents-dir-spec.el --- Buttercup tests for the hidden .agents/ directory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Sub-agent sessions live under `<branch-dir>/.agents/' (hidden), not
;; `<branch-dir>/agents/'.  The name is spelled once, in
;; `jf/gptel-session--agents-dir', and every path that reaches the
;; directory is composed through `jf/gptel--agents-dir-path'
;; (register/vocabulary/session-tree-directory-names).
;;
;; Scenario mapping (delta specs of change workspace-notes-in-vault):
;; - sessions-persistence § "Hidden agents directory" ->
;;   "Agent directory is hidden": a PersistentAgent created under
;;   `branches/main/' lands at `branches/main/.agents/<preset>-<ts>-<slug>/'
;;   and `jf/gptel--find-all-branches-with-agents' finds it.
;; - sessions-branching § "Agent replication" -> "Copying all agent
;;   directories": replication targets the child's `.agents/'.
;; - register/boundary/session-dir-marker-walk: the ancestor-marker walk
;;   still resolves with the hidden directory in the path.
;;
;; No path helper is stubbed: every test builds a REAL temp session tree
;; through the production helpers and inspects the disk.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)
(require 'gptel-session-branching)

(defvar jf-hidden-agents-test--tempdirs nil
  "Temporary directories created during tests, for cleanup.")

(defun jf-hidden-agents-test--make-tempdir ()
  "Create a fresh temporary directory and register it for cleanup."
  (let ((dir (make-temp-file "gptel-hidden-agents-" t)))
    (push dir jf-hidden-agents-test--tempdirs)
    dir))

(defun jf-hidden-agents-test--write-drawer (file &rest kvs)
  "Write FILE with a point-min `:GPTEL_*:' drawer built from KVS pairs."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (insert ":PROPERTIES:\n")
    (cl-loop for (k v) on kvs by #'cddr
             do (insert (format ":%s: %s\n" k v)))
    (insert ":END:\n\n#+begin_user\n\n#+end_user\n")))

(defun jf-hidden-agents-test--basenames (paths)
  "Return the basenames of PATHS (directories or files)."
  (mapcar (lambda (p) (file-name-nondirectory (directory-file-name p)))
          paths))

(describe "Hidden agents directory (.agents/)"

  (after-each
    (dolist (dir jf-hidden-agents-test--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-hidden-agents-test--tempdirs nil))

  (describe "single source of the name"

    (it "spells the agents directory as .agents in the constant"
      (expect jf/gptel-session--agents-dir :to-equal ".agents"))

    (it "composes <branch-dir>/.agents through jf/gptel--agents-dir-path"
      (expect (jf/gptel--agents-dir-path "/tmp/s/branches/main")
              :to-equal "/tmp/s/branches/main/.agents")))

  (describe "Scenario: Agent directory is hidden"

    (let (tempdir session-dir branch-dir agent-dir)

      (before-each
        (setq tempdir (jf-hidden-agents-test--make-tempdir))
        (let ((jf/gptel-sessions-directory tempdir))
          (setq session-dir (jf/gptel--create-session-directory
                             "demo-20260420000000"))
          (setq branch-dir (jf/gptel--create-branch-directory
                            session-dir "main"))
          (jf-hidden-agents-test--write-drawer
           (jf/gptel--context-file-path branch-dir)
           "GPTEL_SESSION_ID" "demo-20260420000000"
           "GPTEL_BRANCH" "main")
          ;; The real PersistentAgent directory helper, no stubs.
          (setq agent-dir (jf/gptel--create-agent-directory
                           branch-dir "researcher" "explore API"))))

      (it "creates branches/main/.agents/<preset>-<ts>-<slug>/"
        (expect (file-directory-p agent-dir) :to-be t)
        (expect (file-relative-name agent-dir session-dir)
                :to-match
                "\\`branches/main/\\.agents/researcher-[0-9]\\{14\\}-explore-api\\'"))

      (it "places the agent under a directory literally named .agents"
        (expect (file-name-nondirectory
                 (directory-file-name (file-name-directory
                                       (directory-file-name agent-dir))))
                :to-equal ".agents")
        (expect (file-directory-p (jf/gptel--agents-dir-path branch-dir))
                :to-be t))

      (it "is found by jf/gptel--find-all-branches-with-agents"
        (let* ((jf/gptel-sessions-directory tempdir)
               (entries (jf/gptel--find-all-branches-with-agents))
               (main (seq-find (lambda (e)
                                 (equal (plist-get e :branch-name) "main"))
                               entries)))
          (expect (length entries) :to-equal 1)
          (expect (plist-get main :session-id) :to-equal "demo-20260420000000")
          (expect (length (plist-get main :agent-dirs)) :to-equal 1)
          (expect (file-equal-p (car (plist-get main :agent-dirs)) agent-dir)
                  :to-be t)))

      (it "lists agent directories without . / .. or other dot entries"
        (let ((listed (jf/gptel--list-agent-directories branch-dir)))
          (expect (length listed) :to-equal 1)
          (expect (file-equal-p (car listed) agent-dir) :to-be t)
          (expect (seq-some (lambda (name) (string-prefix-p "." name))
                            (jf-hidden-agents-test--basenames listed))
                  :to-be nil)))

      (it "is invisible to a hidden-entry listing of the branch dir, so only the helper reaches it"
        ;; Enumerator rule: the agents directory is never DISCOVERED by
        ;; listing a branch dir with the package's "^[^.]" filter — that
        ;; filter hides it.  The only route is the path helper.
        (expect (member ".agents" (directory-files branch-dir nil "^[^.]"))
                :to-be nil)
        (expect (file-directory-p (jf/gptel--agents-dir-path branch-dir))
                :to-be t))))

  (describe "marker walk from an agent transcript under .agents/"

    (let (tempdir session-dir branch-dir agent-dir agent-file)

      (before-each
        (setq tempdir (jf-hidden-agents-test--make-tempdir))
        (let ((jf/gptel-sessions-directory tempdir))
          (setq session-dir (jf/gptel--create-session-directory
                             "demo-20260420000000"))
          (setq branch-dir (jf/gptel--create-branch-directory
                            session-dir "main"))
          (jf-hidden-agents-test--write-drawer
           (jf/gptel--context-file-path branch-dir)
           "GPTEL_SESSION_ID" "demo-20260420000000"
           "GPTEL_BRANCH" "main")
          (setq agent-dir (jf/gptel--create-agent-directory
                           branch-dir "researcher" "explore API"))
          (setq agent-file (jf/gptel--context-file-path agent-dir))
          (jf-hidden-agents-test--write-drawer
           agent-file
           "GPTEL_SESSION_ID" (file-name-nondirectory
                               (directory-file-name agent-dir))
           "GPTEL_BRANCH" "main"
           "GPTEL_PARENT_SESSION_ID" "demo-20260420000000")))

      (it "resolves the transcript as an agent and returns its own directory"
        (let* ((drawer (jf/gptel--read-session-drawer-head agent-file))
               (type (jf/gptel--session-type drawer)))
          (expect type :to-be 'agent)
          (expect (file-equal-p
                   (jf/gptel--session-dir-from-branch-dir
                    (file-name-directory agent-file) type)
                   agent-dir)
                  :to-be t)))

      (it "still finds the enclosing branches/ ancestor with .agents/ in the path"
        ;; The structural walk is unaffected by a hidden component: from
        ;; <session>/branches/main/.agents/<agent>/ the nearest ancestor
        ;; whose child branches/ encloses the start is <session>.
        (expect (file-equal-p
                 (jf/gptel--session-dir-from-branch-dir
                  (file-name-directory agent-file) 'branch)
                 session-dir)
                :to-be t))

      (it "enumerates the agent from the root after the walk"
        (let* ((jf/gptel-sessions-directory tempdir)
               (agent-dirs (apply #'append
                                  (mapcar (lambda (e) (plist-get e :agent-dirs))
                                          (jf/gptel--find-all-branches-with-agents)))))
          (expect (length agent-dirs) :to-equal 1)
          (expect (file-equal-p (car agent-dirs) agent-dir) :to-be t)))))

  (describe "Scenario: Copying all agent directories"

    (let (tempdir session-dir parent-branch-dir child-branch-dir)

      (before-each
        (setq tempdir (jf-hidden-agents-test--make-tempdir))
        (let ((jf/gptel-sessions-directory tempdir))
          (setq session-dir (jf/gptel--create-session-directory
                             "demo-20260420000000"))
          (setq parent-branch-dir (jf/gptel--create-branch-directory
                                   session-dir "main"))
          (setq child-branch-dir (jf/gptel--create-branch-directory
                                  session-dir "20260420120000-child"))
          (dolist (name '("search-agent" "code-generator"))
            (let ((dir (jf/gptel--create-agent-directory
                        parent-branch-dir name "task")))
              (jf-hidden-agents-test--write-drawer
               (jf/gptel--context-file-path dir)
               "GPTEL_SESSION_ID" (file-name-nondirectory dir)
               "GPTEL_PARENT_SESSION_ID" "demo-20260420000000")))))

      (it "replicates every parent agent into the child's .agents/"
        (jf/gptel--copy-branch-agents parent-branch-dir child-branch-dir)
        (let ((parent-names (jf-hidden-agents-test--basenames
                             (jf/gptel--list-agent-directories parent-branch-dir)))
              (child-names (jf-hidden-agents-test--basenames
                            (jf/gptel--list-agent-directories child-branch-dir))))
          (expect (length parent-names) :to-equal 2)
          (expect (sort (copy-sequence child-names) #'string<)
                  :to-equal (sort (copy-sequence parent-names) #'string<))
          (expect (file-directory-p (expand-file-name ".agents" child-branch-dir))
                  :to-be t)
          (dolist (name child-names)
            (expect (file-exists-p
                     (expand-file-name
                      (concat name "/session.org")
                      (jf/gptel--agents-dir-path child-branch-dir)))
                    :to-be t))))

      (it "does not fail when the parent has no .agents/ (child lists no agents)"
        (let ((jf/gptel-sessions-directory tempdir))
          (let* ((bare-parent (jf/gptel--create-branch-directory
                               session-dir "20260420130000-bare"))
                 (bare-child (jf/gptel--create-branch-directory
                              session-dir "20260420130001-bare-child")))
            (jf/gptel--copy-branch-agents bare-parent bare-child)
            ;; No parent .agents/ -> nothing to copy, no error, child has none.
            (expect (jf/gptel--list-agent-directories bare-child) :to-be nil)))))))

(provide 'hidden-agents-dir-spec)
;;; hidden-agents-dir-spec.el ends here
