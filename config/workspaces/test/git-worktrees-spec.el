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

(provide 'git-worktrees-spec)
