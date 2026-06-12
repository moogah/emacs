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

(require 'buttercup)
(require 'cl-lib)

(defconst git-worktrees-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "../git-worktrees.el" git-worktrees-spec--this-dir))

(describe "workspace git-worktrees repo-candidate seam"

  ;; By default, treat every probed directory as a real git repo so the
  ;; source-merging tests are not filtered by the git check. Individual
  ;; tests override this to exercise the non-git exclusion.
  (before-each
    (spy-on 'jf/workspace--worktree-git-repo-p :and-call-fake
            (lambda (dir) dir)))

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
        (expect (cdr result) :to-match "worktree failure")))))

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
