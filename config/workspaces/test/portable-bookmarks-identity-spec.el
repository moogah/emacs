;;; portable-bookmarks-identity-spec.el --- Repo identity specs -*- lexical-binding: t; -*-

;; Specs for the git-derived repository identity helpers:
;; `workspace-bookmark--git', `workspace-bookmark--repo-root',
;; `workspace-bookmark--remote-url', `workspace-bookmark--normalize-identity'
;; and the composed `workspace-bookmark--identity'.
;;
;; Two halves, deliberately (design.md D11):
;;
;; * BEHAVIORAL — real `git' subprocesses against throwaway repositories
;;   built by `portable-bookmarks-fixtures.el'.  These exist because the
;;   thing under test IS the translation of git's output; a hand-written
;;   imitation of that output would only test the imitation.  They also
;;   cover the two environment hazards the design calls out: a directory
;;   name containing spaces, and a repository with no remote.
;;
;; * UNIT — the URL normalization table, and the composition, with the git
;;   runner mocked via `cl-letf' scoped to the call under test.  These
;;   cover spellings no fixture can conveniently produce (ports, embedded
;;   credentials, malformed strings).

(require 'buttercup)
(require 'cl-lib)
(require 'bookmark)

(defconst portable-bookmarks-identity-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-identity-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-identity-spec--this-dir))


;;; Unit — URL normalization

(describe "workspace-bookmark--normalize-identity"

  (it "agrees across the three spellings of one repository"
    ;; The contract the whole capability rests on: a repository cloned
    ;; over SSH on one machine and over HTTPS on another must produce one
    ;; identity, or its bookmarks are unresolvable on the other machine.
    (let ((forms '("git@github.com:moogah/emacs.git"
                   "https://github.com/moogah/emacs"
                   "ssh://git@github.com/moogah/emacs.git")))
      (dolist (form forms)
        (expect (cons form (workspace-bookmark--normalize-identity form))
                :to-equal (cons form "github.com/moogah/emacs")))))

  (it "normalizes the accepted spellings to host/path"
    (dolist (case '(("git@github.com:moogah/emacs.git" . "github.com/moogah/emacs")
                    ("https://github.com/moogah/emacs" . "github.com/moogah/emacs")
                    ("ssh://git@github.com/moogah/emacs.git" . "github.com/moogah/emacs")
                    ("git://github.com/moogah/emacs.git" . "github.com/moogah/emacs")
                    ("http://github.com/moogah/emacs.git" . "github.com/moogah/emacs")
                    ;; trailing slash, with and without the .git suffix
                    ("https://github.com/moogah/emacs/" . "github.com/moogah/emacs")
                    ("https://github.com/moogah/emacs.git/" . "github.com/moogah/emacs")
                    ;; surrounding whitespace, as `git config' output can carry
                    ("  git@github.com:moogah/emacs.git  " . "github.com/moogah/emacs")
                    ;; host case is insignificant
                    ("https://GitHub.COM/moogah/emacs" . "github.com/moogah/emacs")
                    ("git@GITHUB.com:moogah/emacs.git" . "github.com/moogah/emacs")
                    ;; a non-forge host with no dot is still a host in scp form
                    ("gitserver:team/repo.git" . "gitserver/team/repo")
                    ("ssh://git@gitserver/team/repo.git" . "gitserver/team/repo")
                    ;; scp form with an absolute path
                    ("git@example.com:/srv/git/repo.git" . "example.com/srv/git/repo")
                    ;; deep paths (self-hosted GitLab subgroups)
                    ("https://gitlab.example.com/grp/sub/repo.git"
                     . "gitlab.example.com/grp/sub/repo")))
      (expect (cons (car case)
                    (workspace-bookmark--normalize-identity (car case)))
              :to-equal case)))

  (it "preserves path case while downcasing the host"
    ;; Downcasing the path would let two DIFFERENT repositories on a
    ;; case-sensitive host collide into one identity, which is the
    ;; open-the-wrong-file failure; keeping case can at worst fail to
    ;; recognise one repository, which degrades safely.
    (expect (workspace-bookmark--normalize-identity
             "git@GitHub.com:Moogah/Emacs.git")
            :to-equal "github.com/Moogah/Emacs")
    (expect (workspace-bookmark--normalize-identity
             "git@github.com:moogah/emacs.git")
            :not :to-equal
            (workspace-bookmark--normalize-identity
             "git@github.com:Moogah/Emacs.git")))

  (it "drops a port when the URL carries a scheme"
    ;; A port is transport, not identity: the ssh:// and https:// clones
    ;; of one self-hosted repository must still agree.
    (expect (workspace-bookmark--normalize-identity
             "ssh://git@gitlab.example.com:2222/team/repo.git")
            :to-equal "gitlab.example.com/team/repo")
    (expect (workspace-bookmark--normalize-identity
             "ssh://git@gitlab.example.com:2222/team/repo.git")
            :to-equal
            (workspace-bookmark--normalize-identity
             "https://gitlab.example.com/team/repo.git")))

  (it "treats digits after the colon as a path in scp-like syntax"
    ;; Not a port: git's scp-like form has no port, so `2222' here is a
    ;; directory name and must survive into the identity.  This asymmetry
    ;; with the ssh:// case above is git's, not ours.
    (expect (workspace-bookmark--normalize-identity
             "git@gitlab.example.com:2222/team/repo.git")
            :to-equal "gitlab.example.com/2222/team/repo"))

  (it "strips embedded credentials"
    ;; The identity is persisted to the user's bookmark file; a token or
    ;; password must not travel with it.
    (dolist (case '(("https://ghp_ABC123@github.com/moogah/emacs.git"
                     . "github.com/moogah/emacs")
                    ("https://moogah:s3cret@github.com/moogah/emacs.git"
                     . "github.com/moogah/emacs")))
      (expect (cons (car case)
                    (workspace-bookmark--normalize-identity (car case)))
              :to-equal case))
    (expect (workspace-bookmark--normalize-identity
             "https://ghp_ABC123@github.com/moogah/emacs.git")
            :not :to-match "ghp_ABC123"))

  (it "returns nil for anything that does not name a host"
    (dolist (input '(nil
                     ""
                     "   "
                     "not a url"
                     ;; local remotes: real, and deliberately not portable
                     "/Users/jefffarr/src/repo.git"
                     "../sibling"
                     "sibling/repo"
                     "file:///srv/git/repo.git"
                     ;; host with no path
                     "https://github.com"
                     "https://github.com/"
                     "git@github.com:"
                     "git@github.com:/"))
      (expect (cons input (workspace-bookmark--normalize-identity input))
              :to-equal (cons input nil))))

  (it "returns nil for a non-string argument"
    (expect (workspace-bookmark--normalize-identity 42) :to-be nil)
    (expect (workspace-bookmark--normalize-identity '("url")) :to-be nil)))


;;; Unit — remote selection and composition

(describe "workspace-bookmark--remote-url"

  (it "prefers origin without consulting the remote list"
    (let (calls)
      (cl-letf (((symbol-function 'workspace-bookmark--git)
                 (lambda (_dir &rest args)
                   (push args calls)
                   (when (equal args '("config" "--get" "remote.origin.url"))
                     "git@github.com:moogah/emacs.git"))))
        (expect (workspace-bookmark--remote-url "/anywhere")
                :to-equal "git@github.com:moogah/emacs.git")
        (expect calls :to-equal '(("config" "--get" "remote.origin.url"))))))

  (it "falls back to the first remote git lists"
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (_dir &rest args)
                 (cond ((equal args '("remote")) "upstream\nfork")
                       ((equal args '("config" "--get" "remote.upstream.url"))
                        "https://github.com/moogah/emacs.git")))))
      (expect (workspace-bookmark--remote-url "/anywhere")
              :to-equal "https://github.com/moogah/emacs.git")))

  (it "returns nil when the repository has no remote"
    (cl-letf (((symbol-function 'workspace-bookmark--git) (lambda (&rest _) nil)))
      (expect (workspace-bookmark--remote-url "/anywhere") :to-be nil))))

(describe "workspace-bookmark--identity"

  (it "returns nil without asking for a remote when there is no repository"
    (cl-letf (((symbol-function 'workspace-bookmark--repo-root) (lambda (_) nil))
              ((symbol-function 'workspace-bookmark--remote-url)
               (lambda (_) (error "must not be consulted"))))
      (expect (workspace-bookmark--identity "/anywhere") :to-be nil)))

  (it "returns nil when the repository has no remote"
    (cl-letf (((symbol-function 'workspace-bookmark--repo-root)
               (lambda (_) "/root/"))
              ((symbol-function 'workspace-bookmark--remote-url) (lambda (_) nil)))
      (expect (workspace-bookmark--identity "/anywhere") :to-be nil)))

  (it "asks for the remote at the repository root, not at the starting path"
    (let (asked)
      (cl-letf (((symbol-function 'workspace-bookmark--repo-root)
                 (lambda (_) "/root/"))
                ((symbol-function 'workspace-bookmark--remote-url)
                 (lambda (dir)
                   (setq asked dir)
                   "git@github.com:moogah/emacs.git")))
        (expect (workspace-bookmark--identity "/root/deep/file.el")
                :to-equal "github.com/moogah/emacs")
        (expect asked :to-equal "/root/")))))


;;; Behavioral — real git

(describe "repository identity against real git repositories"

  (before-each (workspace-bookmark-fixtures-setup))
  (after-each (workspace-bookmark-fixtures-teardown))

  (it "gives SSH and HTTPS checkouts of one repository the same identity"
    (let ((ssh (workspace-bookmark-fixtures-make-repo
                :remote "git@github.com:moogah/emacs.git"))
          (https (workspace-bookmark-fixtures-make-repo
                  :remote "https://github.com/moogah/emacs")))
      (expect (workspace-bookmark--identity ssh)
              :to-equal "github.com/moogah/emacs")
      (expect (workspace-bookmark--identity https)
              :to-equal (workspace-bookmark--identity ssh))))

  (it "gives a repository with no remote no identity"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      ;; The repository is real — only the identity is absent.
      (expect (workspace-bookmark--repo-root repo) :to-equal repo)
      (expect (workspace-bookmark--remote-url repo) :to-be nil)
      (expect (workspace-bookmark--identity repo) :to-be nil)))

  (it "gives a directory outside any repository no identity"
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-norepo-")))
      (expect (workspace-bookmark--repo-root dir) :to-be nil)
      (expect (workspace-bookmark--identity dir) :to-be nil)))

  (it "uses the only remote when it is not named origin"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (workspace-bookmark-fixtures-git
       repo "remote" "add" "upstream" "git@github.com:moogah/emacs.git")
      (expect (workspace-bookmark--identity repo)
              :to-equal "github.com/moogah/emacs")))

  (it "prefers origin when several remotes exist"
    ;; `git remote' lists alphabetically, so `alt' comes first: the result
    ;; proves the origin preference rather than list order.
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:moogah/emacs.git")))
      (workspace-bookmark-fixtures-git
       repo "remote" "add" "alt" "https://example.com/someone/other.git")
      (expect (workspace-bookmark--remote-url repo)
              :to-equal "git@github.com:moogah/emacs.git")
      (expect (workspace-bookmark--identity repo)
              :to-equal "github.com/moogah/emacs")))

  (it "resolves the repository root from a subdirectory and from a file"
    (let* ((repo (workspace-bookmark-fixtures-make-repo
                  :remote "git@github.com:moogah/emacs.git"
                  :file "src/deep/thing.el"))
           (subdir (expand-file-name "src/deep" repo))
           (file (expand-file-name "src/deep/thing.el" repo)))
      (expect (workspace-bookmark--repo-root subdir) :to-equal repo)
      (expect (workspace-bookmark--repo-root file) :to-equal repo)
      (expect (workspace-bookmark--identity file)
              :to-equal "github.com/moogah/emacs")))

  (it "gives a linked worktree the identity of its parent repository"
    ;; A linked worktree has no remote config of its own; it reads the
    ;; shared one.  This is the arrangement the whole capability targets.
    (let* ((repo (workspace-bookmark-fixtures-make-repo
                  :remote "git@github.com:moogah/emacs.git"))
           (parent (workspace-bookmark-fixtures-temp-dir "wsbm-wt-"))
           (worktree (workspace-bookmark-fixtures-add-worktree
                      repo (expand-file-name "linked" parent))))
      (expect (workspace-bookmark--repo-root worktree) :to-equal worktree)
      (expect (workspace-bookmark--identity worktree)
              :to-equal "github.com/moogah/emacs")))

  (it "works in a repository whose directory name contains spaces"
    ;; ~/src/Archived Repos/ is real on this machine.  No shell is
    ;; involved, so there is nothing to quote — this spec is what keeps it
    ;; that way.
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:moogah/emacs.git"
                 :prefix "wsbm Archived Repos ")))
      (expect (string-match-p " " repo) :to-be-truthy)
      (expect (workspace-bookmark--identity repo)
              :to-equal "github.com/moogah/emacs"))))


;;; Behavioral — the git runner degrades instead of signalling

(describe "workspace-bookmark--git"

  (before-each (workspace-bookmark-fixtures-setup))
  (after-each (workspace-bookmark-fixtures-teardown))

  (it "returns nil when the directory does not exist"
    (expect (workspace-bookmark--git
             "/wsbm-definitely-not-here/deeper" "rev-parse" "--show-toplevel")
            :to-be nil))

  (it "returns nil when git exits non-zero"
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-nogit-")))
      (expect (workspace-bookmark--git dir "rev-parse" "--show-toplevel")
              :to-be nil)))

  (it "returns nil for an unknown subcommand"
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-badcmd-")))
      (expect (workspace-bookmark--git dir "not-a-real-subcommand") :to-be nil)))

  (it "returns nil rather than signalling when git cannot be run"
    ;; The whole point of the nil contract: these helpers run inside
    ;; bookmark creation and bookmark jumps.
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-nobinary-")))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _) (signal 'file-missing '("git")))))
        (expect (workspace-bookmark--git dir "rev-parse" "--show-toplevel")
                :to-be nil))))

  (it "returns nil for a nil or non-string directory"
    (expect (workspace-bookmark--git nil "rev-parse") :to-be nil)
    (expect (workspace-bookmark--git 42 "rev-parse") :to-be nil))

  (it "returns trimmed stdout on success"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (expect (workspace-bookmark--git repo "rev-parse" "--is-inside-work-tree")
              :to-equal "true"))))

(provide 'portable-bookmarks-identity-spec)
;;; portable-bookmarks-identity-spec.el ends here
