;;; git-worktrees-spec.el --- Tests for workspace git-worktrees integration -*- lexical-binding: t; -*-

;; Covers the repo-candidate seam of the git-worktrees consumer module
;; (`jf/workspace--worktree-repo-candidates').  It merges git-derived
;; discovery and magit's repository list (git-derived first), de-dupes by
;; file identity (`workspace--same-file-p'), excludes non-git directories, offers only canonical checkouts,
;; and never consults projectile or project.el.  The read-repo helper's
;; free-form entry and `read-directory-name' floor reach a repo that no
;; source suggests.
;;
;; Magit's list, and the ignored projectile / project.el lists, are STUBBED
;; via `cl-letf' (magit, projectile and project.el are never really
;; required), so the tests are deterministic and do not depend on those
;; packages being installed.
;;
;; The unit block spies the git-derived source and the classifier.  The
;; "git-derived repo candidates" block builds throwaway repositories with
;; `portable-bookmarks-fixtures.el' and drives the real portable-bookmarks
;; discovery helpers against a fresh workspace registry.  Those specs assert
;; only what the candidate list contains, so they do not pin how the helpers
;; decide what is in view or who owns a checkout.

(require 'buttercup)
(require 'cl-lib)

(defconst git-worktrees-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

;; The registration-gating describe block drives
;; `workspace-register-integration' / inspects `workspace--integrations'
;; directly, so make the registry available regardless of spec load order.
;; integrations.el depends on `workspace--sessions-dir' from data-model.el.
(load (expand-file-name "../data-model.el" git-worktrees-spec--this-dir))
(load (expand-file-name "../integrations.el" git-worktrees-spec--this-dir))
(load (expand-file-name "../git-worktrees.el" git-worktrees-spec--this-dir))

;; The git-derived source and the classifier reuse portable-bookmarks'
;; discovery helpers when they are bound.  git-worktrees.el never loads that
;; module, so the spec does, together with the real-git fixtures its own
;; suite uses.
(load (expand-file-name "portable-bookmarks-fixtures.el"
                        git-worktrees-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        git-worktrees-spec--this-dir))

(defvar jf/emacs-dir)
(defvar projectile-known-projects)

(defun git-worktrees-spec--only-features (features)
  "Return a `featurep' replacement for projectile, magit and project.
FEATURES lists which of those to report as loaded; every other feature
answers as the real `featurep' does."
  (let ((real (symbol-function 'featurep)))
    (lambda (feature &optional subfeature)
      (if (memq feature '(projectile magit project))
          (and (memq feature features) t)
        (funcall real feature subfeature)))))

(defmacro git-worktrees-spec--with-sources (features magit-repos &rest body)
  "Run BODY with only FEATURES of projectile/magit/project loaded.
`magit-list-repos' returns MAGIT-REPOS.  Projectile's and project.el's
lists are EMPTY unless BODY binds them."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'featurep)
              (git-worktrees-spec--only-features ,features))
             ((symbol-function 'magit-list-repos)
              (let ((repos ,magit-repos)) (lambda () repos)))
             ((symbol-function 'project-known-project-roots)
              (lambda () nil)))
     (let ((projectile-known-projects nil))
       ,@body)))

(describe "workspace git-worktrees repo-candidate seam"

  ;; By default every probed directory is a real git repo, the git-derived
  ;; source is empty, and portable-bookmarks' public classifier admits
  ;; every candidate it is handed, so these specs isolate source merging.
  ;; Individual specs override each.
  (before-each
    (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
            (lambda (dir) dir))
    (spy-on 'jf/workspace--worktree-git-derived-candidates
            :and-return-value nil)
    (spy-on 'workspace-bookmark-canonical-checkouts-in-view
            :and-call-fake (lambda (&optional dirs) dirs)))

  (describe "jf/workspace--worktree-repo-candidates"

    (it "merges git-derived and magit candidates, git-derived first"
      (spy-on 'jf/workspace--worktree-git-derived-candidates
              :and-return-value '("/repos/git-a/"))
      (git-worktrees-spec--with-sources '(magit) '("/repos/magit-b/")
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal '("/repos/git-a/" "/repos/magit-b/"))))

    (it "excludes directories that are not git repositories"
      (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
              (lambda (dir) (string-prefix-p "/repos/real/" dir)))
      (git-worktrees-spec--with-sources '(magit)
          '("/repos/real/" "/tmp/not-a-repo/")
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal '("/repos/real/"))))

    (it "returns nil when no source is present"
      (git-worktrees-spec--with-sources nil '("/repos/unused/")
        (expect (jf/workspace--worktree-repo-candidates) :to-equal nil)))

    (it "ignores projectile and project.el even when loaded and populated"
      (git-worktrees-spec--with-sources '(projectile magit project) nil
        (let ((projectile-known-projects '("/repos/projectile/")))
          (cl-letf (((symbol-function 'project-known-project-roots)
                     (lambda () '("/repos/project-el/"))))
            (expect (jf/workspace--worktree-repo-candidates)
                    :to-equal nil)))))

    (it "offers only the candidates the classifier calls canonical"
      (spy-on 'workspace-bookmark-canonical-checkouts-in-view :and-call-fake
              (lambda (&optional dirs)
                (seq-filter (lambda (dir) (equal dir "/repos/main/")) dirs)))
      (git-worktrees-spec--with-sources '(magit)
          '("/repos/owned/" "/repos/main/" "/repos/other/")
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal '("/repos/main/"))))

    (it "passes magit candidates through when the classifier is unbound"
      (git-worktrees-spec--with-sources '(magit) '("/repos/a/" "/repos/b/")
        (cl-letf (((symbol-function
                    'workspace-bookmark-canonical-checkouts-in-view)
                   nil))
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal '("/repos/a/" "/repos/b/")))))

    (it "classifies both sources inside ONE discovery call, and leaves no memo behind"
      (let ((memos nil))
        (spy-on 'workspace-bookmark-call-with-discovery-memo :and-call-through)
        (spy-on 'jf/workspace--worktree-git-derived-candidates :and-call-fake
                (lambda ()
                  (push workspace-bookmark--discovery-memo memos)
                  '("/repos/git-a/")))
        (spy-on 'workspace-bookmark-canonical-checkouts-in-view :and-call-fake
                (lambda (&optional dirs)
                  (push workspace-bookmark--discovery-memo memos)
                  dirs))
        (git-worktrees-spec--with-sources '(magit) '("/repos/magit-b/")
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal '("/repos/git-a/" "/repos/magit-b/")))
        (expect 'workspace-bookmark-call-with-discovery-memo
                :to-have-been-called-times 1)
        (expect (length memos) :to-equal 2)
        (expect (hash-table-p (car memos)) :to-be-truthy)
        (expect (eq (car memos) (cadr memos)) :to-be-truthy)
        (expect workspace-bookmark--discovery-memo :to-be nil))))

  (describe "jf/workspace--worktree-read-repo"

    (it "reaches a free-form repo via the read-directory-name floor"
      ;; No candidates; the completing-read returns a path that is not an
      ;; existing directory, so the read-directory-name floor supplies the
      ;; final answer.
      (spy-on 'jf/workspace--worktree-repo-candidates :and-return-value nil)
      (spy-on 'completing-read :and-return-value "/does/not/exist/")
      (spy-on 'read-directory-name :and-return-value "/repos/floor/")
      (expect (jf/workspace--worktree-read-repo)
              :to-equal (expand-file-name "/repos/floor/"))
      (expect 'read-directory-name :to-have-been-called))

    (it "accepts a typed path to a repo that no source suggests"
      ;; require-match is nil, so a directory missing from the candidates
      ;; is returned as typed, without the floor prompt.
      (let ((typed (file-name-as-directory temporary-file-directory)))
        (spy-on 'jf/workspace--worktree-repo-candidates
                :and-return-value '("/repos/suggested/"))
        (spy-on 'completing-read :and-return-value typed)
        (spy-on 'read-directory-name)
        (expect (jf/workspace--worktree-read-repo)
                :to-equal (expand-file-name typed))
        (expect (nth 3 (spy-calls-args-for 'completing-read 0)) :to-be nil)
        (expect 'read-directory-name :not :to-have-been-called)))

    (it "treats empty or blank input as no choice and reaches the floor without probing default-directory"
      ;; worktree-read-repo-empty-input: RET on empty input makes
      ;; `completing-read' return "", and (expand-file-name "") is
      ;; `default-directory', which exists, so the floor was never shown.
      ;; Under a remote `default-directory' the probe also opened a
      ;; connection; the fake remote handler refuses every filesystem
      ;; operation, so any probe of it is recorded as a touch.
      (spy-on 'jf/workspace--worktree-repo-candidates :and-return-value nil)
      (spy-on 'read-directory-name :and-return-value "/repos/floor/")
      (dolist (blank '("" "   "))
        (spy-on 'completing-read :and-return-value blank)
        (let ((raised nil) (result nil))
          (workspaces-test-with-fake-remote ("/ssh:" :allow (expand-file-name))
            (let ((default-directory "/ssh:nohost.invalid:/home/"))
              (condition-case err
                  (setq result (jf/workspace--worktree-read-repo))
                (error (setq raised err)))))
          (expect raised :to-be nil)
          (expect workspaces-test-fake-remote-touches :to-be nil)
          (expect result :to-equal "/repos/floor/")))
      (expect 'read-directory-name :to-have-been-called-times 2))))

;; A remote (TRAMP) entry in magit's list must never touch the network.
;; Real TRAMP is never reached: "/ssh:" names are served by the core fake
;; remote handler (`workspaces-test-with-fake-remote',
;; workspaces-test-helpers.el, loaded by the portable-bookmarks fixtures)
;; with its strict default.  It answers `file-remote-p' from the name, as
;; TRAMP does without its CONNECTED argument, and signals `file-error' for
;; every other operation, standing in for a connection attempt to an
;; unreachable host.  It records every operation it is asked.
(describe "remote (TRAMP) magit entries"
  ;; ownership-and-picker-polish (worktree-candidates-git-first review).
  ;; The git-repository filter probed a remote entry that classification
  ;; had passed through, so an unreachable host raised `file-error' out of
  ;; the candidate list and the prompt was never reached.

  (before-each
    (spy-on 'jf/workspace--worktree-git-derived-candidates
            :and-return-value nil))

  (it "keeps local magit candidates and offers no remote one"
    (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
            (lambda (dir) dir))
    (workspaces-test-with-fake-remote "/ssh:"
      (git-worktrees-spec--with-sources '(magit)
          '("/repos/local/" "/ssh:nohost.invalid:/a/")
        (cl-letf (((symbol-function
                    'workspace-bookmark-canonical-checkouts-in-view)
                   nil))
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal '("/repos/local/"))))
      (expect (workspaces-test-fake-remote-operations)
              :to-equal '(file-remote-p))))

  (it "never hands a remote entry to the classifier"
    ;; ownership-and-picker-polish review: with portable-bookmarks loaded,
    ;; magit's entries reach the classifier before de-duplication.  The
    ;; classifier here touches the filesystem for every entry it is given,
    ;; so a remote entry reaching it would trip the fake handler.
    (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
            (lambda (dir) dir))
    (let ((classified nil))
      (workspaces-test-with-fake-remote "/ssh:"
        (git-worktrees-spec--with-sources '(magit)
            '("/repos/local/" "/ssh:nohost.invalid:/a/")
          (cl-letf (((symbol-function
                      'workspace-bookmark-canonical-checkouts-in-view)
                     (lambda (&optional checkouts)
                       (setq classified checkouts)
                       (mapc #'file-truename checkouts)
                       checkouts)))
            (expect (jf/workspace--worktree-repo-candidates)
                    :to-equal '("/repos/local/"))))
        (expect classified :to-equal '("/repos/local/"))
        (expect (workspaces-test-fake-remote-operations)
                :to-equal '(file-remote-p)))))

  (it "still reaches the prompt and its floor when classification passes it through"
    ;; The classifier is unbound (portable-bookmarks not loaded), so nothing
    ;; but the local-only filter where magit's list enters stands between
    ;; magit's remote entry and the real git-repo filter.
    (spy-on 'completing-read :and-return-value "/does/not/exist/")
    (spy-on 'read-directory-name :and-return-value "/repos/floor/")
    (let ((raised nil) (result nil))
      (workspaces-test-with-fake-remote "/ssh:"
        (git-worktrees-spec--with-sources '(magit)
            '("/ssh:nohost.invalid:/a/")
          (cl-letf (((symbol-function
                      'workspace-bookmark-canonical-checkouts-in-view)
                     nil))
            (condition-case err
                (setq result (jf/workspace--worktree-read-repo))
              (error (setq raised err)))))
        (expect raised :to-be nil)
        (expect (workspaces-test-fake-remote-operations)
                :to-equal '(file-remote-p)))
      (expect result :to-equal (expand-file-name "/repos/floor/"))
      (expect (nth 1 (spy-calls-args-for 'completing-read 0)) :to-be nil)
      (expect 'read-directory-name :to-have-been-called))))

(describe "git-derived repo candidates"
  ;; Real git: REPO is a canonical checkout; workspace A's home holds a
  ;; linked worktree of it, which is what puts REPO in view.  OTHER is a
  ;; second canonical repository in view of no workspace, the kind of
  ;; repository only magit's list would report.
  :var (repo other home-a linked)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote "git@example.com:fixtures/gw-candidates.git"
                :prefix "gw-cand-repo-")
          other (workspace-bookmark-fixtures-make-repo
                 :prefix "gw-cand-other-")
          home-a (workspace-bookmark-fixtures-make-workspace "gw-cand-a")
          linked (file-name-as-directory
                  (workspace-bookmark-fixtures-add-worktree
                   repo (expand-file-name "gw-candidates" home-a)
                   :branch "gw-cand-a"))))

  (it "includes the repositories of checkouts in view with no other source"
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources nil nil
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo)))))

  (it "ignores projectile and project.el even when loaded and populated"
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources '(projectile project) nil
        (let ((projectile-known-projects (list other)))
          (cl-letf (((symbol-function 'project-known-project-roots)
                     (lambda () (list other))))
            (expect (jf/workspace--worktree-repo-candidates)
                    :to-equal (list repo)))))))

  (it "places git-derived candidates before magit's"
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources '(magit) (list other)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo other)))))

  (it "lists a repository known to git and to magit once"
    ;; Magit spells the root without its trailing slash.
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources '(magit)
          (list (directory-file-name repo) other)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo other)))))

  (it "lists a repository once when magit spells it via a symlinked prefix"
    ;; Git reports the resolved path (/private/var/... on macOS); magit's
    ;; list names the same repository through the /var symlink.
    (let ((alias (and (string-prefix-p "/private/var/" repo)
                      (substring repo (length "/private")))))
      (assume (and alias
                   (file-directory-p alias)
                   (equal (file-truename alias) repo))
              "temp dir is not reached through the /var -> /private/var symlink")
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources '(magit) (list alias other)
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo other))))))

  (it "offers a repository once, at its first position, when a later source names it through a symbolic link"
    ;; Scenario: workspace-git-worktrees § Two spellings of one repository are one candidate
    ;; seam/worktree-candidate-identity.  The merge point itself is driven:
    ;; with the classifier unbound, magit's spellings reach it as magit
    ;; gives them (a bound classifier would already have respelled the link
    ;; as git's main-worktree path).  Unlike the /var spec above, the link
    ;; is made here, so the spec runs wherever symbolic links can be made.
    (let ((jf/emacs-dir nil)
          (alias (expand-file-name
                  "alias" (workspace-bookmark-fixtures-temp-dir "gw-cand-link-"))))
      (make-symbolic-link (directory-file-name repo) alias)
      (spy-on 'jf/workspace--worktree-git-derived-candidates
              :and-return-value (list repo))
      (git-worktrees-spec--with-sources '(magit) (list other alias)
        (cl-letf (((symbol-function
                    'workspace-bookmark-canonical-checkouts-in-view)
                   nil))
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo other))))))

  (it "still excludes directories that are not git repositories"
    (let ((jf/emacs-dir nil)
          (plain (workspace-bookmark-fixtures-temp-dir "gw-cand-plain-")))
      (git-worktrees-spec--with-sources '(magit) (list plain)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo)))))

  (describe "workspace-owned checkouts"
    ;; Workspace B's home holds a full clone of REPO: a checkout with a
    ;; `.git' directory that a workspace owns.  A's linked worktree is
    ;; owned too.  Neither may be offered, whichever source names it.
    :var (clone)

    (before-each
      (let ((home-b (workspace-bookmark-fixtures-make-workspace "gw-cand-b")))
        (workspace-bookmark-fixtures-git home-b "clone" "--quiet" repo "gw-clone")
        (setq clone (file-name-as-directory
                     (expand-file-name "gw-clone" home-b)))))

    (it "are not offered from the git-derived source"
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources nil nil
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo)))))

    (it "are not offered when magit reports them"
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources '(magit) (list linked clone other)
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo other))))))

  (it "passes magit candidates through when the classifier is unbound"
    ;; Without the classifier the git-derived source contributes nothing,
    ;; and magit's list is offered unfiltered, owned checkout included.
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources '(magit) (list linked other)
        (cl-letf (((symbol-function
                    'workspace-bookmark-canonical-checkouts-in-view)
                   nil))
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list linked other))))))

  (describe "when a portable-bookmarks helper signals"

    (before-each (spy-on 'display-warning))

    (it "warns once and still takes candidates from magit"
      (spy-on 'workspace-bookmark--checkouts-in-view :and-call-fake
              (lambda () (error "Simulated discovery failure")))
      (let ((jf/emacs-dir nil)
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (condition-case err
              (setq result (jf/workspace--worktree-repo-candidates))
            (error (setq raised err))))
        (expect raised :to-be nil)
        (expect result :to-equal (list other))
        (expect 'display-warning :to-have-been-called-times 1)
        (expect (nth 1 (spy-calls-args-for 'display-warning 0))
                :to-match "Simulated discovery failure")))

    (it "still reaches the read-directory-name floor"
      (spy-on 'workspace-bookmark--checkouts-in-view :and-call-fake
              (lambda () (error "Simulated discovery failure")))
      (spy-on 'completing-read :and-return-value "/does/not/exist/")
      (spy-on 'read-directory-name :and-return-value other)
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (expect (jf/workspace--worktree-read-repo) :to-equal other)))
      (expect (nth 1 (spy-calls-args-for 'completing-read 0))
              :to-equal (list other))
      (expect 'read-directory-name :to-have-been-called))

    (it "offers nothing unchecked and warns once when the classifier signals"
      (spy-on 'workspace-bookmark--classify :and-call-fake
              (lambda (_dir) (error "Simulated classify failure")))
      (let ((jf/emacs-dir nil)
            (raised nil)
            (result 'unset))
        (git-worktrees-spec--with-sources '(magit) (list linked other)
          (condition-case err
              (setq result (jf/workspace--worktree-repo-candidates))
            (error (setq raised err))))
        (expect raised :to-be nil)
        (expect result :to-be nil)
        (expect 'display-warning :to-have-been-called-times 1)
        (expect (nth 1 (spy-calls-args-for 'display-warning 0))
                :to-match "Simulated classify failure")))

    ;; The discovery scope is a portable-bookmarks call like the two
    ;; above (architect on-touch finding 47): a scope that fails before
    ;; the build starts costs the sharing, never the candidates or the
    ;; prompt; the build's own outcome is never the scope's to change.
    ;; Scenario: workspace-git-worktrees § A discovery scope that cannot be entered does not shrink discovery
    ;; (the three specs below).

    (it "warns once and still builds both sources, unscoped, when the discovery scope signals"
      (spy-on 'workspace-bookmark-call-with-discovery-memo :and-call-fake
              (lambda (_function) (error "Simulated scope failure")))
      (spy-on 'jf/workspace--worktree-git-derived-candidates :and-call-through)
      (let ((jf/emacs-dir nil)
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (condition-case err
              (setq result (jf/workspace--worktree-repo-candidates))
            (error (setq raised err))))
        (expect raised :to-be nil)
        (expect result :to-equal (list repo other))
        (expect 'jf/workspace--worktree-git-derived-candidates
                :to-have-been-called-times 1)
        (expect 'display-warning :to-have-been-called-times 1)
        (expect (nth 1 (spy-calls-args-for 'display-warning 0))
                :to-match "Simulated scope failure")))

    (it "falls back the same way when the scope's calling convention changed"
      (spy-on 'workspace-bookmark-call-with-discovery-memo :and-call-fake
              (lambda (_function _extra) nil))
      (let ((jf/emacs-dir nil)
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (condition-case err
              (setq result (jf/workspace--worktree-repo-candidates))
            (error (setq raised err))))
        (expect raised :to-be nil)
        (expect result :to-equal (list repo other))
        (expect 'display-warning :to-have-been-called-times 1)))

    (it "still reaches the source prompt and its read-directory-name floor when the scope signals"
      (spy-on 'workspace-bookmark-call-with-discovery-memo :and-call-fake
              (lambda (_function) (error "Simulated scope failure")))
      (spy-on 'completing-read :and-return-value "/does/not/exist/")
      (spy-on 'read-directory-name :and-return-value other)
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (expect (jf/workspace--worktree-read-repo) :to-equal other)))
      (expect (nth 1 (spy-calls-args-for 'completing-read 0))
              :to-equal (list repo other))
      (expect 'read-directory-name :to-have-been-called)
      (expect 'display-warning :to-have-been-called-times 1))

    ;; Scenario: workspace-git-worktrees § The discovery's own error is not masked
    (it "lets the build's own error, signalled inside the scope, propagate unchanged and unwarned"
      (let ((runs 0)
            (raised nil)
            (jf/emacs-dir nil))
        (spy-on 'workspace-bookmark-call-with-discovery-memo :and-call-through)
        (spy-on 'jf/workspace--worktree-git-derived-candidates :and-call-fake
                (lambda () (setq runs (1+ runs)) (error "Simulated build failure")))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (condition-case err
              (jf/workspace--worktree-repo-candidates)
            (error (setq raised err))))
        (expect 'workspace-bookmark-call-with-discovery-memo
                :to-have-been-called-times 1)
        (expect raised :to-equal '(error "Simulated build failure"))
        (expect runs :to-equal 1)
        (expect 'display-warning :not :to-have-been-called))))

  (it "offers the main clone of a linked worktree only magit lists"
    ;; Architect on-touch finding 3: magit's entry is REPLACED by its
    ;; repository's main worktree, which magit does not list and which is
    ;; not in view.
    (let* ((main3 (workspace-bookmark-fixtures-make-repo
                   :remote "git@example.com:fixtures/gw-magit-only.git"
                   :prefix "gw-cand-main3-"))
           (linked3 (file-name-as-directory
                     (workspace-bookmark-fixtures-add-worktree
                      main3 (expand-file-name
                             "feature" (workspace-bookmark-fixtures-temp-dir
                                        "gw-cand-linked3-"))
                      :branch "feature")))
           (jf/emacs-dir nil))
      (git-worktrees-spec--with-sources nil nil
        (expect (jf/workspace--worktree-repo-candidates)
                :not :to-contain main3))
      (git-worktrees-spec--with-sources '(magit) (list linked3)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo main3)))))

  (describe "a husk left by a `.git'-named git directory"
    ;; canonical-predicate-consolidation (architect F2 on
    ;; discovery-refinements).  SEP's git directory was moved to
    ;; HUSK/.git, so git names HUSK, a directory holding only `.git', as
    ;; the main worktree.  `--classify' alone called it canonical, and
    ;; this picker offered it.
    :var (husk)

    (before-each
      (let ((sep (workspace-bookmark-fixtures-make-repo
                  :remote "git@example.com:fixtures/gw-husk.git"
                  :prefix "gw-cand-sep-"))
            (home-h (workspace-bookmark-fixtures-make-workspace "gw-cand-h")))
        (setq husk (file-name-as-directory
                    (expand-file-name
                     "husk" (workspace-bookmark-fixtures-temp-dir
                             "gw-cand-store-"))))
        (make-directory husk)
        (workspace-bookmark-fixtures-git
         sep "init" "--quiet"
         (concat "--separate-git-dir=" (expand-file-name ".git" husk)))
        (workspace-bookmark-fixtures-add-worktree
         sep (expand-file-name "sep" home-h) :branch "h")))

    (it "is not offered from the git-derived source"
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources nil nil
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo)))))

    (it "is not offered when magit reports it"
      (let ((jf/emacs-dir nil))
        (git-worktrees-spec--with-sources '(magit) (list husk other)
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo other))))))

  (describe "with portable-bookmarks not loaded"

    (it "takes candidates from magit and signals nothing"
      (let ((jf/emacs-dir nil)
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(magit) (list other)
          (cl-letf (((symbol-function
                      'workspace-bookmark-canonical-checkouts-in-view)
                     nil)
                    ((symbol-function
                      'workspace-bookmark-call-with-discovery-memo)
                     nil))
            (expect (fboundp 'workspace-bookmark-canonical-checkouts-in-view)
                    :to-be nil)
            (expect (fboundp 'workspace-bookmark-call-with-discovery-memo)
                    :to-be nil)
            (condition-case err
                (setq result (jf/workspace--worktree-repo-candidates))
              (error (setq raised err)))
            (expect raised :to-be nil)
            (expect result :to-equal (list other))
            (expect (jf/workspace--worktree-git-derived-candidates)
                    :to-be nil)))))))

(describe "repo picker subprocess budget"
  ;; The cost contract of register/boundary/git-derived-checkout-discovery,
  ;; picker side (validator 6).  Real git, one fixture: three canonical
  ;; main clones and two linked worktrees of the first, all reported by
  ;; magit and none in view of a workspace, so every subprocess counted is
  ;; portable-bookmarks classifying magit's list.  Every git subprocess of
  ;; that module goes through `workspace-bookmark--git', so a call-through
  ;; spy on it is the count (portable-bookmarks-cost-spec.el has the
  ;; resolver side and the arithmetic).
  ;;
  ;; KNOWINGLY UNCOUNTED: the git-repository post-filter
  ;; (`jf/workspace--worktree-git-repo-p') calls `magit-toplevel' once per
  ;; surviving candidate when magit is loaded — `featurep' says it is, in
  ;; `git-worktrees-spec--with-sources'.  Those spawns go through magit's
  ;; own process calls, not the one runner `workspace-bookmark--git', so
  ;; they are outside this count by design (the entry's one-runner
  ;; boundary), not by accident.
  ;;
  ;; Both sources — the checkouts in view and magit's list — are built in
  ;; ONE discovery call (`workspace-bookmark-call-with-discovery-memo'),
  ;; so a repository reached from both pays once; the nested describe
  ;; below pins that, with a companion that splits the scope.
  ;;
  ;; A canonical main clone costs THREE (its position, its repository's
  ;; worktree listing, the tracked-file check); a further checkout of a
  ;; repository already seen costs ONE (its position).  Three is the
  ;; floor: the three are different git commands.  The plan's "<= 2 per
  ;; magit entry" is not reachable without dropping one of the canonical
  ;; test's signals.  17 at 20944160 on this fixture.
  :var (repo-a repo-b repo-c w1 w2)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo-a (workspace-bookmark-fixtures-make-repo
                  :remote "git@example.com:fixtures/gw-cost-a.git" :prefix "gw-cost-a-")
          repo-b (workspace-bookmark-fixtures-make-repo
                  :remote "git@example.com:fixtures/gw-cost-b.git" :prefix "gw-cost-b-")
          repo-c (workspace-bookmark-fixtures-make-repo
                  :remote "git@example.com:fixtures/gw-cost-c.git" :prefix "gw-cost-c-"))
    (let ((plain (workspace-bookmark-fixtures-temp-dir "gw-cost-plain-")))
      (setq w1 (workspace-bookmark-fixtures-add-worktree
                repo-a (expand-file-name "w1" plain) :branch "gw-cost-w1")
            w2 (workspace-bookmark-fixtures-add-worktree
                repo-a (expand-file-name "w2" plain) :branch "gw-cost-w2")))
    (workspace-bookmark-clear-identity-cache))

  (after-each
    (workspace-bookmark-clear-identity-cache))

  (defun git-worktrees-spec--picker-spawns (magit-repos)
    "Run the picker over MAGIT-REPOS; return (CANDIDATES . SPAWNS)."
    (spy-on 'workspace-bookmark--git :and-call-through)
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources '(magit) magit-repos
        (cons (jf/workspace--worktree-repo-candidates)
              (spy-calls-count 'workspace-bookmark--git)))))

  (it "costs three subprocesses per canonical main clone magit lists"
    (let ((run (git-worktrees-spec--picker-spawns
                (mapcar #'directory-file-name (list repo-a repo-b repo-c)))))
      (expect (car run) :to-equal (list repo-a repo-b repo-c))
      (expect (cdr run) :to-equal 9)))

  (it "costs one more per further checkout of a repository already listed"
    (let ((run (git-worktrees-spec--picker-spawns
                (mapcar #'directory-file-name (list w1 repo-a w2 repo-b repo-c)))))
      (expect (car run) :to-equal (list repo-a repo-b repo-c))
      (expect (cdr run) :to-equal 11)))

  (it "would cost nineteen for the same five entries with the per-call memo bypassed"
    (cl-letf (((symbol-function 'workspace-bookmark--memoised-in-call)
               (lambda (_key function &rest args) (apply function args))))
      (let ((run (git-worktrees-spec--picker-spawns
                  (mapcar #'directory-file-name (list w1 repo-a w2 repo-b repo-c)))))
        (expect (car run) :to-equal (list repo-a repo-b repo-c))
        (expect (cdr run) :to-equal 19))))

  (it "does not depend on the identity memo: the picker computes no identity"
    (spy-on 'workspace-bookmark--memoised-identity :and-call-through)
    (spy-on 'workspace-bookmark--remote-url :and-call-through)
    (let ((run (git-worktrees-spec--picker-spawns
                (mapcar #'directory-file-name (list w1 repo-a w2 repo-b repo-c)))))
      (expect (cdr run) :to-equal 11)
      (expect 'workspace-bookmark--memoised-identity :not :to-have-been-called)
      (expect 'workspace-bookmark--remote-url :not :to-have-been-called)))

  (describe "with a repository both in view and in magit's list"
    ;; The everyday shape under DD-15: REPO-A's main clone is one magit
    ;; lists (~/src), and a linked worktree of it sits in a workspace home,
    ;; so it is in view too.  The git-derived source reaches REPO-A through
    ;; that worktree — its position and REPO-A's listing, then REPO-A's
    ;; canonical test (position, tracked-file check; the listing is
    ;; shared): 4.  Magit's pass then finds REPO-A's position, listing and
    ;; tracked-file check already asked: 0, and 3 each for B and C.

    (before-each
      (let ((home (workspace-bookmark-fixtures-make-workspace "gw-cost-view")))
        (workspace-bookmark-fixtures-add-worktree
         repo-a (expand-file-name "wa" home) :branch "gw-cost-wa")))

    (it "pays for that repository once: ten for the view's worktree and three clones"
      (let ((run (git-worktrees-spec--picker-spawns
                  (mapcar #'directory-file-name (list repo-a repo-b repo-c)))))
        (expect (car run) :to-equal (list repo-a repo-b repo-c))
        (expect (cdr run) :to-equal 10)))

    (it "would pay thirteen with the two sources in separate discovery calls"
      (cl-letf (((symbol-function 'workspace-bookmark-call-with-discovery-memo)
                 (lambda (function) (funcall function))))
        (let ((run (git-worktrees-spec--picker-spawns
                    (mapcar #'directory-file-name (list repo-a repo-b repo-c)))))
          (expect (car run) :to-equal (list repo-a repo-b repo-c))
          (expect (cdr run) :to-equal 13))))))

(describe "git-worktrees.el's portable-bookmarks surface"

  (it "names no private portable-bookmarks function in its code"
    ;; Docstrings and comments may mention them; code may not.  The
    ;; permitted surface is `workspace-bookmark-canonical-checkouts-in-view'
    ;; and the scope `workspace-bookmark-call-with-discovery-memo'.
    (let ((file (expand-file-name "../git-worktrees.el"
                                  git-worktrees-spec--this-dir))
          (private nil)
          (public nil))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((walk nil))
                (setq walk
                      (lambda (form)
                        (cond
                         ((symbolp form)
                          (let ((name (symbol-name form)))
                            (cond
                             ((string-prefix-p "workspace-bookmark--" name)
                              (push form private))
                             ((string-prefix-p "workspace-bookmark-" name)
                              (push form public)))))
                         ((consp form)
                          (while (consp form)
                            (funcall walk (car form))
                            (setq form (cdr form)))
                          (when form (funcall walk form))))))
                (funcall walk (read (current-buffer)))))
          (end-of-file nil)))
      (expect private :to-be nil)
      (expect (delete-dups public)
              :to-have-same-items-as
              '(workspace-bookmark-canonical-checkouts-in-view
                workspace-bookmark-call-with-discovery-memo)))))

(describe "jf/workspace--worktree-sanitize-branch"

  (it "replaces whitespace with hyphens"
    (expect (jf/workspace--worktree-sanitize-branch "my feature")
            :to-equal "my-feature"))

  (it "replaces git-illegal ref characters and collapses runs"
    (expect (jf/workspace--worktree-sanitize-branch "feat: cool^thing")
            :to-equal "feat-cool-thing"))

  (it "trims leading and trailing hyphens"
    (expect (jf/workspace--worktree-sanitize-branch "  spaced  ")
            :to-equal "spaced"))

  (it "leaves an already-valid name untouched"
    (expect (jf/workspace--worktree-sanitize-branch "valid-name")
            :to-equal "valid-name"))

  (it "treats nil as the empty string"
    (expect (jf/workspace--worktree-sanitize-branch nil)
            :to-equal "")))

(describe "jf/workspace--add-worktree"

  ;; Behavioral specs for the :menu command. Magit is never required;
  ;; `magit-main-branch' and `magit-worktree-branch' are SPIED, as are
  ;; the prompts (`jf/workspace--worktree-read-repo', `read-string').
  ;; `default-directory'-binding correctness is asserted by having the
  ;; magit spies capture `default-directory' at call time.

  (let (dd-at-main dd-at-branch)
    (before-each
      (setq dd-at-main nil dd-at-branch nil)
      ;; Default: a repo with no pre-existing target dir, main = "main".
      (spy-on 'jf/workspace--worktree-read-repo
              :and-return-value "/repos/source-repo/")
      (spy-on 'magit-main-branch :and-call-fake
              (lambda () (setq dd-at-main default-directory) "main"))
      (spy-on 'read-string :and-call-fake
              (lambda (_prompt &optional initial &rest _) initial))
      (spy-on 'file-exists-p :and-return-value nil)
      (spy-on 'magit-worktree-branch :and-call-fake
              (lambda (&rest _) (setq dd-at-branch default-directory) nil)))

    (it "creates a worktree at <home>/<repo-basename> on a new branch off main"
      (let ((result (jf/workspace--add-worktree
                     '(:name "my feature" :home "/ws/home/"))))
        (expect result :to-equal 'ok)
        (expect 'magit-worktree-branch :to-have-been-called-with
                (expand-file-name "source-repo" "/ws/home/")
                "my-feature" "main")))

    (it "defaults the branch to the sanitized workspace name"
      (jf/workspace--add-worktree '(:name "Cool Feature!" :home "/ws/home/"))
      ;; read-string returned its INITIAL (sanitized default) verbatim.
      (let ((args (spy-calls-args-for 'magit-worktree-branch 0)))
        (expect (nth 1 args) :to-equal "Cool-Feature!")))

    (it "binds default-directory to the chosen repo for both magit calls"
      (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
      (expect dd-at-main :to-equal "/repos/source-repo/")
      (expect dd-at-branch :to-equal "/repos/source-repo/"))

    (it "yields two distinct child worktrees for two different repos"
      (spy-on 'jf/workspace--worktree-read-repo
              :and-return-value "/repos/alpha/")
      (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
      (spy-on 'jf/workspace--worktree-read-repo
              :and-return-value "/repos/beta/")
      (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
      (expect (nth 0 (spy-calls-args-for 'magit-worktree-branch 0))
              :to-equal (expand-file-name "alpha" "/ws/home/"))
      (expect (nth 0 (spy-calls-args-for 'magit-worktree-branch 1))
              :to-equal (expand-file-name "beta" "/ws/home/")))

    (it "returns (failed . _) and makes no magit call when the dir exists"
      (spy-on 'file-exists-p :and-return-value t)
      (let ((result (jf/workspace--add-worktree
                     '(:name "feat" :home "/ws/home/"))))
        (expect (car result) :to-equal 'failed)
        (expect (cdr result) :to-match "already exists")
        (expect 'magit-worktree-branch :not :to-have-been-called)))

    (it "returns (failed . _) when the magit worktree call signals"
      (spy-on 'magit-worktree-branch :and-call-fake
              (lambda (&rest _) (error "git: fatal worktree failure")))
      (let ((result (jf/workspace--add-worktree
                     '(:name "feat" :home "/ws/home/"))))
        (expect (car result) :to-equal 'failed)
        (expect (cdr result) :to-match "worktree failure")))

    (it "prompts for the source as before when the payload has no :source-repo"
      (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
      (expect 'jf/workspace--worktree-read-repo :to-have-been-called-times 1)
      (expect dd-at-main :to-equal "/repos/source-repo/"))

    (describe "with a :source-repo in the payload"
      ;; The portable-bookmarks missing-repo chooser ("add worktree here")
      ;; hands the canonical checkout its resolver admitted.  The command
      ;; must make the worktree from it and ask for no source: no prompt,
      ;; no candidate discovery.
      :var (source)

      (before-each
        (setq source (file-name-as-directory (make-temp-file "gw-source-repo-" t)))
        (spy-on 'jf/workspace--worktree-repo-candidates))

      (after-each
        (delete-directory source t))

      (it "targets it without the source prompt or candidate discovery"
        (let ((result (jf/workspace--add-worktree
                       (list :name "feat" :home "/ws/home/" :source-repo source))))
          (expect result :to-equal 'ok)
          (expect 'jf/workspace--worktree-read-repo :not :to-have-been-called)
          (expect 'jf/workspace--worktree-repo-candidates :not :to-have-been-called)
          (expect dd-at-main :to-equal source)
          (expect dd-at-branch :to-equal source)
          (expect 'magit-worktree-branch :to-have-been-called-with
                  (expand-file-name
                   (file-name-nondirectory (directory-file-name source))
                   "/ws/home/")
                  "feat" "main")))

      (it "binds default-directory to the source itself when it is spelled without a trailing slash"
        (expect (jf/workspace--add-worktree
                 (list :name "feat" :home "/ws/home/"
                       :source-repo (directory-file-name source)))
                :to-equal 'ok)
        (expect dd-at-main :to-equal source)
        (expect dd-at-branch :to-equal source))

      (it "fails without prompting or calling magit when it names no local directory"
        (dolist (bad (list (expand-file-name "missing/" source)
                           nil
                           42
                           "/ssh:nohost.invalid:/repo/"))
          (let ((result (jf/workspace--add-worktree
                         (list :name "feat" :home "/ws/home/" :source-repo bad))))
            (expect (car result) :to-equal 'failed)
            (expect (cdr result) :to-match "source repository is not a directory")))
        (expect 'jf/workspace--worktree-read-repo :not :to-have-been-called)
        (expect 'magit-main-branch :not :to-have-been-called)
        (expect 'magit-worktree-branch :not :to-have-been-called)))

    (it "derives worktree state — never stores :repo/:branch on the payload"
      ;; Spec `workspace-git-worktrees': "Worktree state is derived, not
      ;; stored".  The :menu command operates against the anchor payload
      ;; (a plain :name/:home plist) and MUST NOT add persistent slots
      ;; such as :repo or :branch.  After a successful add, the payload
      ;; carries exactly the keys it was handed — the repo and branch
      ;; live in git/the filesystem, the single source of truth.
      (let ((payload (list :name "feat" :home "/ws/home/")))
        (expect (jf/workspace--add-worktree payload) :to-equal 'ok)
        (expect (plist-member payload :repo) :to-be nil)
        (expect (plist-member payload :branch) :to-be nil)
        ;; The plist is unchanged from what was passed in.
        (expect payload :to-equal '(:name "feat" :home "/ws/home/"))))))

(describe "jf/workspace--add-worktree with the source answered at the prompt"
  ;; worktree-read-repo-trailing-slash: the prompt's answer is bound as
  ;; `default-directory' for both magit calls.  Spelled without a trailing
  ;; slash, Emacs reads it as a FILE name and git runs in its parent, so
  ;; each arm of the prompt must hand back a directory name.  The real
  ;; `jf/workspace--worktree-read-repo' runs; only its own prompts, the
  ;; branch prompt and magit are stubbed.
  :var (source dd-at-main dd-at-branch)

  (before-each
    (setq source (file-name-as-directory (make-temp-file "gw-prompted-repo-" t))
          dd-at-main nil
          dd-at-branch nil)
    (spy-on 'jf/workspace--worktree-repo-candidates :and-return-value nil)
    (spy-on 'magit-main-branch :and-call-fake
            (lambda () (setq dd-at-main default-directory) "main"))
    (spy-on 'read-string :and-call-fake
            (lambda (_prompt &optional initial &rest _) initial))
    (spy-on 'magit-worktree-branch :and-call-fake
            (lambda (&rest _) (setq dd-at-branch default-directory) nil)))

  (after-each
    (delete-directory source t))

  (it "binds default-directory to a typed path spelled without a trailing slash"
    (spy-on 'completing-read :and-return-value (directory-file-name source))
    (spy-on 'read-directory-name)
    (expect (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
            :to-equal 'ok)
    (expect 'read-directory-name :not :to-have-been-called)
    (expect dd-at-main :to-equal source)
    (expect dd-at-branch :to-equal source))

  (it "binds default-directory to the floor's answer spelled without a trailing slash"
    (spy-on 'completing-read :and-return-value "/does/not/exist")
    (spy-on 'read-directory-name :and-return-value (directory-file-name source))
    (expect (jf/workspace--add-worktree '(:name "feat" :home "/ws/home/"))
            :to-equal 'ok)
    (expect 'read-directory-name :to-have-been-called)
    (expect dd-at-main :to-equal source)
    (expect dd-at-branch :to-equal source)))

(defvar git-worktrees-spec--saved-integrations nil
  "The live `workspace--integrations', set aside while a registration spec runs.")

(describe "workspace-git-worktrees registration gating (droppable / magit-soft)"

  ;; Spec `workspace-git-worktrees': the module attaches SOLELY through
  ;; `workspace-register-integration', double-gated behind
  ;; `with-eval-after-load 'workspaces' AND `with-eval-after-load 'magit'.
  ;; Removing the module leaves workspaces working; absent magit means no
  ;; git integration is registered (not an error).  We exercise the gate
  ;; directly with a fresh, isolated registry — magit is never required.

  (before-each
    (setq git-worktrees-spec--saved-integrations workspace--integrations
          workspace--integrations nil))
  ;; Put the live registry back: the registration spec below leaves
  ;; `git-worktree' registered in the isolated one.
  (after-each
    (setq workspace--integrations git-worktrees-spec--saved-integrations
          git-worktrees-spec--saved-integrations nil))

  (it "is gated behind with-eval-after-load 'workspaces AND 'magit in source"
    ;; Structural guarantee that absent magit ⇒ no registration (not an
    ;; error), and removing the module leaves workspaces working: the
    ;; SOLE registration site is doubly deferred.  Read the tangled source
    ;; and confirm the registration is nested inside both load guards.
    (let* ((el (expand-file-name "../git-worktrees.el"
                                 git-worktrees-spec--this-dir))
           (src (with-temp-buffer
                  (insert-file-contents el)
                  (buffer-string))))
      ;; The register call exists exactly once (the SOLE attach point).
      (expect (with-temp-buffer
                (insert src)
                (goto-char (point-min))
                (how-many "(workspace-register-integration"))
              :to-equal 1)
      ;; It is reached only via both deferred-load guards.
      (expect src :to-match
              (concat "with-eval-after-load[ \t\n]+'workspaces"
                      "[\0-\377[:nonascii:]]*?"
                      "with-eval-after-load[ \t\n]+'magit"
                      "[\0-\377[:nonascii:]]*?"
                      "workspace-register-integration"))))

  (it "declares exactly the :menu + :on-purge surfaces (no :on-create)"
    ;; When the gate does fire, the consumer attaches a menu command and a
    ;; purge-teardown handler and nothing else.  Drive the registration
    ;; call directly against an isolated registry — no global `provide'.
    (workspace-register-integration 'git-worktree
      :label "git worktree"
      :menu (cons "w" #'jf/workspace--add-worktree)
      :on-purge #'jf/workspace--worktree-on-purge)
    (let ((entry (cdr (assq 'git-worktree workspace--integrations))))
      (expect entry :not :to-be nil)
      (expect (plist-get entry :on-purge)
              :to-equal #'jf/workspace--worktree-on-purge)
      (expect (car (plist-get entry :menu)) :to-equal "w")
      (expect (plist-get entry :on-create) :to-be nil))))

(describe "jf/workspace--worktree-on-purge"

  ;; Behavioral specs for the :on-purge teardown handler. Magit is never
  ;; required; the worktree enumeration (`jf/workspace--worktree-children')
  ;; and the magit operations (`magit-worktree-delete', `magit-main-branch',
  ;; `magit-branch-merged-p', `magit-branch-delete') are SPIED so the tests
  ;; are deterministic. `default-directory'-binding correctness is asserted
  ;; by having `magit-worktree-delete' capture `default-directory' at call
  ;; time, which must be the worktree's SOURCE repo.

  (let (dd-at-delete)
    (before-each
      (setq dd-at-delete nil)
      ;; Default: two clean linked worktrees under :home, each with a
      ;; distinct source repo and branch.
      (spy-on 'jf/workspace--worktree-children :and-return-value
              (list (list :dir "/ws/home/alpha" :branch "feat-a"
                          :source "/repos/alpha/")
                    (list :dir "/ws/home/beta" :branch "feat-b"
                          :source "/repos/beta/")))
      (spy-on 'magit-worktree-delete :and-call-fake
              (lambda (&rest _) (push default-directory dd-at-delete) nil))
      (spy-on 'magit-main-branch :and-return-value "main")
      ;; By default branches are merged, so deletion is offered.
      (spy-on 'magit-branch-merged-p :and-return-value t)
      (spy-on 'magit-branch-delete :and-return-value nil))

    (it "returns 'skipped when there are no worktrees under :home"
      (spy-on 'jf/workspace--worktree-children :and-return-value nil)
      (expect (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
              :to-equal 'skipped)
      (expect 'magit-worktree-delete :not :to-have-been-called))

    (it "removes every worktree under :home and returns 'ok"
      (expect (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
              :to-equal 'ok)
      (expect 'magit-worktree-delete :to-have-been-called-times 2)
      (expect (nth 0 (spy-calls-args-for 'magit-worktree-delete 0))
              :to-equal "/ws/home/alpha")
      (expect (nth 0 (spy-calls-args-for 'magit-worktree-delete 1))
              :to-equal "/ws/home/beta"))

    (it "binds default-directory to each worktree's source repo for the delete"
      (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
      ;; dd-at-delete accumulates newest-first.
      (expect (nreverse dd-at-delete)
              :to-equal '("/repos/alpha/" "/repos/beta/")))

    (it "deletes a MERGED branch without force after removal"
      (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
      (expect 'magit-branch-merged-p :to-have-been-called-with "feat-a" "main")
      (expect 'magit-branch-delete :to-have-been-called-times 2)
      (expect (spy-calls-args-for 'magit-branch-delete 0)
              :to-equal (list (list "feat-a")))
      ;; No force argument: exactly one arg, the branch list.
      (expect (length (spy-calls-args-for 'magit-branch-delete 0))
              :to-equal 1))

    (it "KEEPS an unmerged branch — magit-branch-delete is never called"
      (spy-on 'magit-branch-merged-p :and-return-value nil)
      (expect (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
              :to-equal 'ok)
      ;; Worktrees still removed.
      (expect 'magit-worktree-delete :to-have-been-called-times 2)
      ;; But no branch was deleted, with or without force.
      (expect 'magit-branch-delete :not :to-have-been-called))

    (it "reports a dirty worktree in (failed . _) while removing the others"
      ;; alpha's removal signals (dirty tree magit refuses); beta succeeds.
      (spy-on 'magit-worktree-delete :and-call-fake
              (lambda (dir &rest _)
                (push default-directory dd-at-delete)
                (when (string= dir "/ws/home/alpha")
                  (error "git: contains modified or untracked files"))
                nil))
      (let ((result (jf/workspace--worktree-on-purge '(:home "/ws/home/"))))
        (expect (car result) :to-equal 'failed)
        (expect (cdr result) :to-match "/ws/home/alpha")
        ;; beta was still attempted and removed.
        (expect (nth 0 (spy-calls-args-for 'magit-worktree-delete 1))
                :to-equal "/ws/home/beta")
        ;; alpha's branch is NOT deleted (its removal failed); beta's is.
        (expect 'magit-branch-delete :to-have-been-called-times 1)
        (expect (spy-calls-args-for 'magit-branch-delete 0)
                :to-equal (list (list "feat-b")))))

    (it "does not delete a branch when the worktree has none (detached)"
      (spy-on 'jf/workspace--worktree-children :and-return-value
              (list (list :dir "/ws/home/detached" :branch nil
                          :source "/repos/gamma/")))
      (expect (jf/workspace--worktree-on-purge '(:home "/ws/home/"))
              :to-equal 'ok)
      (expect 'magit-worktree-delete :to-have-been-called-times 1)
      (expect 'magit-branch-delete :not :to-have-been-called))))

(provide 'git-worktrees-spec)
