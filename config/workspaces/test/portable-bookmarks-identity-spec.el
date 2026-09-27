;;; portable-bookmarks-identity-spec.el --- Repo identity specs -*- lexical-binding: t; -*-

;; Specs for the git-derived repository identity helpers:
;; `workspace-bookmark--git', `workspace-bookmark--remote-url',
;; `workspace-bookmark--normalize-identity' and
;; `workspace-bookmark--root-identity'.  The behavioral specs ask "which
;; checkout, which repository is DIR?" the way the stamp seam and
;; discovery ask it — DIR's position names the root, the root has the
;; identity — through `workspace-bookmark-fixtures-root-of' and
;; `workspace-bookmark-fixtures-identity-of'.
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
                   (when (equal args '("remote" "get-url" "origin"))
                     "git@github.com:moogah/emacs.git"))))
        (expect (workspace-bookmark--remote-url "/anywhere")
                :to-equal "git@github.com:moogah/emacs.git")
        (expect calls :to-equal '(("remote" "get-url" "origin"))))))

  (it "asks git for the EFFECTIVE url, never for the raw config value"
    ;; Pins the command itself, because the two disagree whenever
    ;; `url.*.insteadOf' is set and the raw value yields a fabricated host.
    (let (calls)
      (cl-letf (((symbol-function 'workspace-bookmark--git)
                 (lambda (_dir &rest args) (push args calls) nil)))
        (workspace-bookmark--remote-url "/anywhere")
        (expect (seq-filter (lambda (a) (member "config" a)) calls)
                :to-be nil))))

  (it "falls back to a SOLE non-origin remote"
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (_dir &rest args)
                 (cond ((equal args '("remote")) "upstream")
                       ((equal args '("remote" "get-url" "upstream"))
                        "https://github.com/moogah/emacs.git")))))
      (expect (workspace-bookmark--remote-url "/anywhere")
              :to-equal "https://github.com/moogah/emacs.git")))

  (it "returns nil when several remotes exist and none is origin"
    ;; NOT the alphabetically-first one: `git remote' is sorted, so choosing
    ;; first would make a persisted identity depend on remote names, and
    ;; adding a remote would orphan every stamp already written.
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (_dir &rest args)
                 (cond ((equal args '("remote" "get-url" "origin")) nil)
                       ((equal args '("remote")) "fork\nupstream")
                       (t "https://github.com/moogah/emacs.git")))))
      (expect (workspace-bookmark--remote-url "/anywhere") :to-be nil)))

  (it "returns nil when the repository has no remote"
    (cl-letf (((symbol-function 'workspace-bookmark--git) (lambda (&rest _) nil)))
      (expect (workspace-bookmark--remote-url "/anywhere") :to-be nil))))

(describe "workspace-bookmark--root-identity"

  (it "returns nil when the repository has no remote"
    (cl-letf (((symbol-function 'workspace-bookmark--remote-url) (lambda (_) nil)))
      (expect (workspace-bookmark--root-identity "/root/") :to-be nil)))

  (it "asks for the remote at ROOT and normalizes it"
    (let (asked)
      (cl-letf (((symbol-function 'workspace-bookmark--remote-url)
                 (lambda (dir)
                   (setq asked dir)
                   "git@github.com:moogah/emacs.git")))
        (expect (workspace-bookmark--root-identity "/root/")
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
      (expect (workspace-bookmark-fixtures-identity-of ssh)
              :to-equal "github.com/moogah/emacs")
      (expect (workspace-bookmark-fixtures-identity-of https)
              :to-equal (workspace-bookmark-fixtures-identity-of ssh))))

  (it "gives a repository with no remote no identity"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      ;; The repository is real — only the identity is absent.
      (expect (workspace-bookmark-fixtures-root-of repo) :to-equal repo)
      (expect (workspace-bookmark--remote-url repo) :to-be nil)
      (expect (workspace-bookmark-fixtures-identity-of repo) :to-be nil)))

  (it "gives a directory outside any repository no identity"
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-norepo-")))
      (expect (workspace-bookmark-fixtures-root-of dir) :to-be nil)
      (expect (workspace-bookmark-fixtures-identity-of dir) :to-be nil)))

  (it "uses the only remote when it is not named origin"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (workspace-bookmark-fixtures-git
       repo "remote" "add" "upstream" "git@github.com:moogah/emacs.git")
      (expect (workspace-bookmark-fixtures-identity-of repo)
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
      (expect (workspace-bookmark-fixtures-identity-of repo)
              :to-equal "github.com/moogah/emacs")))

  (it "resolves the repository root from a subdirectory and from a file"
    (let* ((repo (workspace-bookmark-fixtures-make-repo
                  :remote "git@github.com:moogah/emacs.git"
                  :file "src/deep/thing.el"))
           (subdir (expand-file-name "src/deep" repo))
           (file (expand-file-name "src/deep/thing.el" repo)))
      (expect (workspace-bookmark-fixtures-root-of subdir) :to-equal repo)
      (expect (workspace-bookmark-fixtures-root-of file) :to-equal repo)
      (expect (workspace-bookmark-fixtures-identity-of file)
              :to-equal "github.com/moogah/emacs")))

  (it "gives a linked worktree the identity of its parent repository"
    ;; A linked worktree has no remote config of its own; it reads the
    ;; shared one.  This is the arrangement the whole capability targets.
    (let* ((repo (workspace-bookmark-fixtures-make-repo
                  :remote "git@github.com:moogah/emacs.git"))
           (parent (workspace-bookmark-fixtures-temp-dir "wsbm-wt-"))
           (worktree (workspace-bookmark-fixtures-add-worktree
                      repo (expand-file-name "linked" parent))))
      (expect (workspace-bookmark-fixtures-root-of worktree) :to-equal worktree)
      (expect (workspace-bookmark-fixtures-identity-of worktree)
              :to-equal "github.com/moogah/emacs")))

  (it "works in a repository whose directory name contains spaces"
    ;; ~/src/Archived Repos/ is real on this machine.  No shell is
    ;; involved, so there is nothing to quote — this spec is what keeps it
    ;; that way.
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:moogah/emacs.git"
                 :prefix "wsbm Archived Repos ")))
      (expect (string-match-p " " repo) :to-be-truthy)
      (expect (workspace-bookmark-fixtures-identity-of repo)
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

  (it "returns stdout without its line terminator on success"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (expect (workspace-bookmark--git repo "rev-parse" "--is-inside-work-tree")
              :to-equal "true"))))


;;; Wave-3 review fix-ups: the runner answers about DIR and nothing else
;;
;; On-touch audit findings F2 and F3, review findings A1 (checkout
;; discovery) and A2 (stamp seam).

(describe "workspace-bookmark--git output, missing paths and environment"

  (before-each (workspace-bookmark-fixtures-setup))
  (after-each (workspace-bookmark-fixtures-teardown))

  (it "removes only git's line terminator, keeping whitespace that belongs to the output"
    (let ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-ws-")))
      (dolist (case '(("  padded path \n" . "  padded path ")
                      ("two\nlines\n" . "two\nlines")
                      ("\n\n" . "\n")
                      ("\n" . nil)
                      ("no terminator" . "no terminator")))
        (cl-letf (((symbol-function 'process-file)
                   (lambda (&rest _) (insert (car case)) 0)))
          (expect (cons (car case) (workspace-bookmark--git dir "x"))
                  :to-equal case)))))

  (it "keeps a trailing space in a checkout root"
    (let* ((parent (workspace-bookmark-fixtures-temp-dir "wsbm-space-"))
           (repo (expand-file-name "proj " parent)))
      (make-directory repo)
      (workspace-bookmark-fixtures-git repo "init" "--quiet")
      (expect (workspace-bookmark--git repo "rev-parse" "--show-toplevel")
              :to-equal repo)
      (expect (workspace-bookmark-fixtures-root-of repo)
              :to-equal (file-name-as-directory repo))))

  (it "returns nil for a missing path inside a repository instead of asking its parent"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (dolist (missing (list (expand-file-name "no-such-dir" repo)
                             (expand-file-name "no-such-dir/" repo)
                             (expand-file-name "no-such-file.el" repo)))
        (expect (cons missing (workspace-bookmark--git
                               missing "rev-parse" "--show-toplevel"))
                :to-equal (list missing)))
      ;; An EXISTING file still resolves to its directory.
      (expect (workspace-bookmark--git (expand-file-name "README.md" repo)
                                       "rev-parse" "--show-toplevel")
              :to-equal (directory-file-name repo))))

  (it "answers about DIR's repository whatever GIT_* variables name another"
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:acme/one.git"))
          (other (workspace-bookmark-fixtures-make-repo
                  :remote "https://gitlab.com/other/two.git")))
      ;; Asked from a subdirectory, so that a ceiling at the root would
      ;; stop git's upward search before it finds the repository.
      (make-directory (expand-file-name "sub" repo))
      (dolist (env (list (list (concat "GIT_DIR=" (expand-file-name ".git" other)))
                         (list (concat "GIT_DIR=" (expand-file-name ".git" other))
                               (concat "GIT_WORK_TREE=" (directory-file-name other)))
                         (list (concat "GIT_COMMON_DIR=" (expand-file-name ".git" other)))
                         (list (concat "GIT_CEILING_DIRECTORIES="
                                       (directory-file-name repo)))))
        (let ((process-environment (append env process-environment)))
          (expect (list env (workspace-bookmark-fixtures-root-of
                             (expand-file-name "sub/" repo)))
                  :to-equal (list env repo))
          (expect (list env (workspace-bookmark-fixtures-identity-of
                             (expand-file-name "sub/" repo)))
                  :to-equal (list env "github.com/acme/one"))))))

  (it "does not change the caller's process-environment"
    (let* ((repo (workspace-bookmark-fixtures-make-repo))
           (process-environment (cons "GIT_DIR=/wsbm/elsewhere/.git"
                                      process-environment))
           (before (copy-sequence process-environment)))
      (workspace-bookmark--git repo "rev-parse" "--show-toplevel")
      (expect process-environment :to-equal before))))


;;; Review fix-ups (merge 0f513ca6 -> review findings)
;;
;; Each describe below pins a defect found in review or on-touch audit. All
;; three defects produced a CONFIDENTLY WRONG answer rather than a missing
;; one, which is the class `register/invariant/no-silent-wrong-file-open'
;; exists to prevent.

(describe "workspace-bookmark--git rejects remote paths (review finding 1, blocking)"

  ;; These MUST NOT be mocked. The defect was that `file-directory-p' sat
  ;; outside the `condition-case', so a real TRAMP name signalled before the
  ;; error barrier was entered -- measured: /nosuchmethod: -> user-error,
  ;; /ssh:nonexistent.invalid: -> file-error. A spec that stubs `process-file'
  ;; only covers the guarded region and cannot see this.

  (it "returns nil instead of signalling on an unknown TRAMP method"
    (expect (workspace-bookmark--git "/nosuchmethod:host:/tmp"
                                     "rev-parse" "--show-toplevel")
            :to-be nil))

  (it "returns nil instead of signalling on an unreachable ssh host"
    (expect (workspace-bookmark--git "/ssh:nonexistent.invalid:/tmp"
                                     "rev-parse" "--show-toplevel")
            :to-be nil))

  (it "never runs a subprocess for a remote path"
    ;; The harm is not only the signal. `process-file' is TRAMP-aware, so a
    ;; REACHABLE host would run git remotely and stamp the remote checkout's
    ;; identity onto a local-looking bookmark. Assert no process is attempted.
    (let ((called nil))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _) (setq called t) 0)))
        (workspace-bookmark--git "/ssh:build-box:/srv/emacs" "rev-parse"))
      (expect called :to-be nil)))

  (it "still works for an ordinary local repository"
    (workspace-bookmark-fixtures-setup)
    (unwind-protect
        (let ((repo (workspace-bookmark-fixtures-make-repo)))
          (expect (workspace-bookmark--git repo "rev-parse" "--is-inside-work-tree")
                  :to-equal "true"))
      (workspace-bookmark-fixtures-teardown))))

(describe "workspace-bookmark--remote-url reads the EFFECTIVE url (review finding 2)"

  (before-each (workspace-bookmark-fixtures-setup))
  (after-each (workspace-bookmark-fixtures-teardown))

  (it "expands url.*.insteadOf rather than reading the raw config value"
    ;; `git config --get remote.origin.url' returns the raw `gh:...' spelling,
    ;; which normalizes to the fabricated host `gh' -- an identity that exists
    ;; only in one machine's gitconfig, which is precisely the cross-machine
    ;; breakage this capability exists to prevent.
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "gh:moogah/emacs.git")))
      (workspace-bookmark--git repo "config"
                               "url.git@github.com:.insteadOf" "gh:")
      (expect (workspace-bookmark--remote-url repo)
              :to-equal "git@github.com:moogah/emacs.git")
      (expect (workspace-bookmark-fixtures-identity-of repo)
              :to-equal "github.com/moogah/emacs")))

  (it "uses a sole non-origin remote"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (workspace-bookmark--git repo "remote" "add" "upstream"
                               "git@github.com:moogah/emacs.git")
      (expect (workspace-bookmark-fixtures-identity-of repo)
              :to-equal "github.com/moogah/emacs")))

  (it "degrades to nil rather than guessing when several remotes exist and none is origin"
    ;; `git remote' lists alphabetically, so picking the FIRST would make the
    ;; identity depend on remote NAMES: adding `backup' to a repo whose only
    ;; remote was `upstream' would silently re-key every persisted stamp.
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (workspace-bookmark--git repo "remote" "add" "upstream"
                               "git@github.com:moogah/emacs.git")
      (workspace-bookmark--git repo "remote" "add" "backup"
                               "git@example.com:mirror/emacs.git")
      (expect (workspace-bookmark--remote-url repo) :to-be nil)
      (expect (workspace-bookmark-fixtures-identity-of repo) :to-be nil)))

  (it "still prefers origin when origin exists alongside others"
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:moogah/emacs.git")))
      (workspace-bookmark--git repo "remote" "add" "backup"
                               "git@example.com:mirror/emacs.git")
      (expect (workspace-bookmark-fixtures-identity-of repo)
              :to-equal "github.com/moogah/emacs"))))

(describe "normalization spellings closed after the on-touch probe"

  (it "strips a trailing .git in any case"
    (expect (workspace-bookmark--normalize-identity
             "git@github.com:moogah/emacs.GIT")
            :to-equal "github.com/moogah/emacs"))

  (it "collapses interior runs of slashes"
    (expect (workspace-bookmark--normalize-identity
             "https://github.com//moogah//emacs.git")
            :to-equal "github.com/moogah/emacs"))

  (it "agrees across every spelling of one repository"
    (let ((spellings '("git@github.com:moogah/emacs.git"
                       "git@github.com:moogah/emacs.GIT"
                       "https://github.com/moogah/emacs"
                       "https://github.com/moogah/emacs.git/"
                       "https://github.com//moogah//emacs.git"
                       "ssh://git@github.com/moogah/emacs.git"
                       "https://ghp_TOKEN@github.com/moogah/emacs.git"
                       "git@GITHUB.com:moogah/emacs.git")))
      (expect (delete-dups
               (mapcar #'workspace-bookmark--normalize-identity spellings))
              :to-equal '("github.com/moogah/emacs")))))

(provide 'portable-bookmarks-identity-spec)
;;; portable-bookmarks-identity-spec.el ends here
