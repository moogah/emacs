;;; git-worktrees-spec.el --- Tests for workspace git-worktrees integration -*- lexical-binding: t; -*-

;; Covers the repo-candidate seam of the git-worktrees consumer module
;; (`jf/workspace--worktree-repo-candidates'): it merges distinct repos
;; from multiple loaded project sources, de-dupes cross-source
;; duplicates on expanded paths, excludes non-git directories, and the
;; read-repo helper's `read-directory-name' floor reaches a repo present
;; in no source.
;;
;; The project sources (projectile / magit / project.el) are SPIED via
;; `cl-letf' — never really required — so the tests are deterministic and
;; do not depend on those packages being installed.
;;
;; The git-derived source (`jf/workspace--worktree-git-derived-candidates')
;; is spied to nil in the unit block and exercised for real in the
;; "git-derived repo candidates" block, which builds throwaway repositories
;; with `portable-bookmarks-fixtures.el' and drives the real
;; portable-bookmarks discovery helpers against a fresh workspace registry.
;; Those specs assert only what the candidate list contains, so they do not
;; pin how the helpers decide what is in view or who owns a checkout.

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

;; The git-derived source reuses portable-bookmarks' discovery helpers when
;; they are bound.  git-worktrees.el never loads that module, so the spec
;; does, together with the real-git fixtures its own suite uses.
(load (expand-file-name "portable-bookmarks-fixtures.el"
                        git-worktrees-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        git-worktrees-spec--this-dir))

(defvar jf/emacs-dir)
(defvar projectile-known-projects)

;; `workspace--registered-names' is owned by tabs.el, which the test harness
;; loads through init.el.  When this file runs outside the harness, supply
;; the same registry walk (as portable-bookmarks-discovery-spec.el does).
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))

(describe "workspace git-worktrees repo-candidate seam"

  ;; By default, treat every probed directory as a real git repo so the
  ;; source-merging tests are not filtered by the git check. Individual
  ;; tests override this to exercise the non-git exclusion.
  (before-each
    (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
            (lambda (dir) dir))
    ;; Keep these unit specs to the project sources: the git-derived
    ;; source would otherwise see whatever checkouts the running Emacs
    ;; has in view.  It is exercised for real in its own block below.
    (spy-on 'jf/workspace--worktree-git-derived-candidates
            :and-return-value nil))

  (describe "jf/workspace--worktree-repo-candidates"

    (it "merges distinct repos from multiple loaded sources"
      ;; projectile + project.el both present, disjoint repo sets.
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (memq f '(projectile))))
                ((symbol-function 'project-known-project-roots)
                 (lambda () '("/repos/proj-c/")))
                (projectile-known-projects '("/repos/proj-a/" "/repos/proj-b/")))
        (let ((result (jf/workspace--worktree-repo-candidates)))
          (expect (sort (copy-sequence result) #'string<)
                  :to-equal '("/repos/proj-a/" "/repos/proj-b/" "/repos/proj-c/")))))

    (it "de-dupes a repo that appears in more than one source"
      ;; projectile and project.el name the SAME repo.
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (memq f '(projectile))))
                ((symbol-function 'project-known-project-roots)
                 (lambda () '("/repos/shared/")))
                (projectile-known-projects '("/repos/shared/" "/repos/only-a/")))
        (let ((result (jf/workspace--worktree-repo-candidates)))
          (expect (sort (copy-sequence result) #'string<)
                  :to-equal '("/repos/only-a/" "/repos/shared/"))
          (expect (length result) :to-equal 2))))

    (it "excludes directories that are not git repositories"
      ;; Re-spy the git predicate: only /repos/real/ qualifies.
      (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
              (lambda (dir) (string-prefix-p "/repos/real/" dir)))
      (cl-letf (((symbol-function 'featurep)
                 (lambda (f) (memq f '(projectile))))
                ((symbol-function 'project-known-project-roots) (lambda () nil))
                (projectile-known-projects '("/repos/real/" "/tmp/not-a-repo/")))
        (let ((result (jf/workspace--worktree-repo-candidates)))
          (expect result :to-equal '("/repos/real/")))))

    (it "returns nil when no source is present"
      (cl-letf (((symbol-function 'featurep) (lambda (_f) nil))
                ((symbol-function 'fboundp)
                 (lambda (s) (not (eq s 'project-known-project-roots)))))
        (expect (jf/workspace--worktree-repo-candidates) :to-equal nil))))

  (describe "jf/workspace--worktree-read-repo"

    (it "reaches a free-form repo via the read-directory-name floor"
      ;; No project sources; the completing-read returns a path that is
      ;; not an existing directory, so the read-directory-name floor
      ;; supplies the final answer.
      (spy-on 'jf/workspace--worktree-repo-candidates :and-return-value nil)
      (spy-on 'completing-read :and-return-value "/does/not/exist/")
      (spy-on 'read-directory-name :and-return-value "/repos/floor/")
      (expect (jf/workspace--worktree-read-repo)
              :to-equal (expand-file-name "/repos/floor/"))
      (expect 'read-directory-name :to-have-been-called))))

(defvar git-worktrees-spec--saved-registry nil
  "The real `workspace--registry', set aside for the duration of a spec.")

(defun git-worktrees-spec--only-features (features)
  "Return a `featurep' replacement hiding projectile and magit.
FEATURES lists which of those two to report as loaded; every other
feature answers as the real `featurep' does."
  (let ((real (symbol-function 'featurep)))
    (lambda (feature &optional subfeature)
      (if (memq feature '(projectile magit))
          (and (memq feature features) t)
        (funcall real feature subfeature)))))

(defmacro git-worktrees-spec--with-sources (features &rest body)
  "Run BODY with only FEATURES of projectile/magit loaded and no project.el.
`project-known-project-roots' is unbound for BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'featurep)
              (git-worktrees-spec--only-features ,features))
             ((symbol-function 'project-known-project-roots) nil))
     ,@body))

(describe "git-derived repo candidates"
  ;; Real git: REPO is a canonical checkout; workspace A's home holds a
  ;; linked worktree of it, which is what puts REPO in view.  OTHER is a
  ;; second canonical repository in view of no workspace — the kind of
  ;; repository only projectile would report.
  :var (repo other home-a linked)

  (before-each
    (setq git-worktrees-spec--saved-registry workspace--registry
          workspace--registry (make-hash-table :test 'equal))
    (workspace-bookmark-fixtures-setup)
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

  (after-each
    (workspace-bookmark-fixtures-teardown)
    (setq workspace--registry git-worktrees-spec--saved-registry
          git-worktrees-spec--saved-registry nil))

  (it "includes the repositories of checkouts in view with no project source"
    (let ((jf/emacs-dir nil))
      (git-worktrees-spec--with-sources nil
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo)))))

  (it "is not empty with projectile unloaded"
    ;; project.el present and knowing nothing; magit and projectile absent.
    (let ((jf/emacs-dir nil))
      (cl-letf (((symbol-function 'featurep)
                 (git-worktrees-spec--only-features nil))
                ((symbol-function 'project-known-project-roots)
                 (lambda () nil)))
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo)))))

  (it "places git-derived candidates before projectile's"
    (let ((jf/emacs-dir nil)
          (projectile-known-projects (list other)))
      (git-worktrees-spec--with-sources '(projectile)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo other)))))

  (it "lists a repository known to git and to projectile once"
    ;; Projectile spells the root without its trailing slash.
    (let ((jf/emacs-dir nil)
          (projectile-known-projects
           (list (directory-file-name repo) other)))
      (git-worktrees-spec--with-sources '(projectile)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo other)))))

  (it "still excludes directories that are not git repositories"
    (let* ((jf/emacs-dir nil)
           (plain (workspace-bookmark-fixtures-temp-dir "gw-cand-plain-"))
           (projectile-known-projects (list plain)))
      (git-worktrees-spec--with-sources '(projectile)
        (expect (jf/workspace--worktree-repo-candidates)
                :to-equal (list repo)))))

  (it "does not offer a workspace-owned checkout as a source"
    ;; Workspace B's home holds a full clone of REPO: a canonical-looking
    ;; checkout that a workspace owns.  Neither it nor A's linked worktree
    ;; may be offered — only REPO itself.
    (let* ((jf/emacs-dir nil)
           (home-b (workspace-bookmark-fixtures-make-workspace "gw-cand-b"))
           (clone (file-name-as-directory
                   (expand-file-name "gw-clone" home-b))))
      (workspace-bookmark-fixtures-git home-b "clone" "--quiet" repo "gw-clone")
      (git-worktrees-spec--with-sources nil
        (let ((result (jf/workspace--worktree-repo-candidates)))
          (expect result :to-equal (list repo))
          (expect (member linked result) :to-be nil)
          (expect (member clone result) :to-be nil)))))

  (it "lists a repository once when projectile spells it via a symlinked prefix"
    ;; Git reports the resolved path (/private/var/... on macOS); projectile
    ;; recorded the same repository through the /var symlink.
    (let ((alias (and (string-prefix-p "/private/var/" repo)
                      (substring repo (length "/private")))))
      (assume (and alias
                   (file-directory-p alias)
                   (equal (file-truename alias) repo))
              "temp dir is not reached through the /var -> /private/var symlink")
      (let ((jf/emacs-dir nil)
            (projectile-known-projects (list alias other)))
        (git-worktrees-spec--with-sources '(projectile)
          (expect (jf/workspace--worktree-repo-candidates)
                  :to-equal (list repo other))))))

  (describe "when a portable-bookmarks helper signals"

    (before-each
      (spy-on 'workspace-bookmark--checkouts-in-view :and-call-fake
              (lambda () (error "Simulated discovery failure")))
      (spy-on 'display-warning))

    (it "warns once and still takes candidates from the other sources"
      (let ((jf/emacs-dir nil)
            (projectile-known-projects (list other))
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(projectile)
          (condition-case err
              (setq result (jf/workspace--worktree-repo-candidates))
            (error (setq raised err))))
        (expect raised :to-be nil)
        (expect result :to-equal (list other))
        (expect 'display-warning :to-have-been-called-times 1)
        (expect (nth 1 (spy-calls-args-for 'display-warning 0))
                :to-match "Simulated discovery failure")))

    (it "still reaches the read-directory-name floor"
      (spy-on 'completing-read :and-return-value "/does/not/exist/")
      (spy-on 'read-directory-name :and-return-value other)
      (let ((jf/emacs-dir nil)
            (projectile-known-projects (list other)))
        (git-worktrees-spec--with-sources '(projectile)
          (expect (jf/workspace--worktree-read-repo) :to-equal other)))
      (expect (nth 1 (spy-calls-args-for 'completing-read 0))
              :to-equal (list other))
      (expect 'read-directory-name :to-have-been-called)))

  (describe "with portable-bookmarks not loaded"

    (it "takes candidates from the other sources and signals nothing"
      (let ((jf/emacs-dir nil)
            (projectile-known-projects (list other))
            (raised nil)
            (result nil))
        (git-worktrees-spec--with-sources '(projectile)
          (cl-letf (((symbol-function 'workspace-bookmark--checkouts-in-view)
                     nil)
                    ((symbol-function 'workspace-bookmark--checkouts-of) nil)
                    ((symbol-function 'workspace-bookmark--classify) nil))
            (expect (fboundp 'workspace-bookmark--checkouts-in-view)
                    :to-be nil)
            (condition-case err
                (setq result (jf/workspace--worktree-repo-candidates))
              (error (setq raised err)))
            (expect raised :to-be nil)
            (expect result :to-equal (list other))
            (expect (jf/workspace--worktree-git-derived-candidates)
                    :to-be nil)))))))

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

(describe "workspace-git-worktrees registration gating (droppable / magit-soft)"

  ;; Spec `workspace-git-worktrees': the module attaches SOLELY through
  ;; `workspace-register-integration', double-gated behind
  ;; `with-eval-after-load 'workspaces' AND `with-eval-after-load 'magit'.
  ;; Removing the module leaves workspaces working; absent magit means no
  ;; git integration is registered (not an error).  We exercise the gate
  ;; directly with a fresh, isolated registry — magit is never required.

  (before-each (setq workspace--integrations nil))

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
