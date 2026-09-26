;;; portable-bookmarks-resolver-spec.el --- Resolution outcome specs -*- lexical-binding: t; -*-

;; Specs for `workspace-bookmark--resolve', the decision function that maps
;; a bookmark record and a workspace name onto exactly one outcome from the
;; closed set `workspace-bookmark--valid-outcomes'.
;;
;; BEHAVIORAL specs run real `git' against throwaway repositories built by
;; `portable-bookmarks-fixtures.el': a canonical clone, linked worktrees in
;; registered fake workspace homes, and records stamped by the real stamp
;; function.
;;
;; UNIT specs mock the two plural discovery functions with `cl-letf',
;; scoped to the call under test, for the outcomes and failures that are
;; fiddly to force against real repositories.
;;
;; Every resolver call in this file goes through
;; `portable-bookmarks-resolver-spec--resolve', which (a) fails the spec if
;; the resolver visits a file, displays a buffer, reads input or writes to
;; the echo area, and (b) records the result, so the last `describe' can
;; assert that every outcome returned anywhere in this file is a member of
;; the closed set and that every member was produced at least once.
;;
;; Every spec runs against a FRESH `workspace--registry' and with
;; `jf/emacs-dir' bound to nil, so the developer's real workspaces and the
;; configuration checkout running the suite are never in view.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

(defconst portable-bookmarks-resolver-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-resolver-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-resolver-spec--this-dir))

(defvar jf/emacs-dir)

;; Owned by tabs.el; see portable-bookmarks-discovery-spec.el for why this
;; stand-in exists.  Under the harness the real definition is bound.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))

(defconst portable-bookmarks-resolver-spec--remote
  "git@example.com:fixtures/resolver.git"
  "Remote URL given to the fixture repositories.")

(defconst portable-bookmarks-resolver-spec--identity
  "example.com/fixtures/resolver"
  "The identity `portable-bookmarks-resolver-spec--remote' normalizes to.")

(defconst portable-bookmarks-resolver-spec--stored
  "/nonexistent/stored/elsewhere/README.md"
  "A stored `filename' that no outcome but `non-portable' may return.")

(defvar portable-bookmarks-resolver-spec--saved-registry nil
  "The real `workspace--registry', set aside for the duration of a spec.")

(defvar portable-bookmarks-resolver-spec--results nil
  "Every plist `portable-bookmarks-resolver-spec--resolve' returned.")


;;; Helpers

(defun portable-bookmarks-resolver-spec--setup ()
  "Open a fixture scope with an empty registry."
  (setq portable-bookmarks-resolver-spec--saved-registry workspace--registry
        workspace--registry (make-hash-table :test 'equal))
  (workspace-bookmark-fixtures-setup))

(defun portable-bookmarks-resolver-spec--teardown ()
  "Close the fixture scope and restore the real registry."
  (workspace-bookmark-fixtures-teardown)
  (setq workspace--registry portable-bookmarks-resolver-spec--saved-registry
        portable-bookmarks-resolver-spec--saved-registry nil))

(defconst portable-bookmarks-resolver-spec--interaction-functions
  '(find-file find-file-noselect find-file-other-window find-file-read-only
    view-file switch-to-buffer pop-to-buffer display-buffer
    completing-read read-string read-from-minibuffer y-or-n-p yes-or-no-p
    message)
  "Functions the resolver must never call.")

(defun portable-bookmarks-resolver-spec--resolve (record workspace-name)
  "Resolve RECORD in WORKSPACE-NAME hermetically, and check discipline.

No current workspace and no configuration checkout are in view.  Fails
the running spec when the resolver calls any of
`portable-bookmarks-resolver-spec--interaction-functions' or leaves a
new buffer behind.  Records and returns the resolver's plist."
  (let* ((calls nil)
         (before (buffer-list))
         (result
          (let ((jf/emacs-dir nil))
            (cl-letf* (((symbol-function
                         'workspace-bookmark--current-workspace)
                        (lambda () nil))
                       ((symbol-function 'find-file)
                        (lambda (&rest _) (push 'find-file calls)))
                       ((symbol-function 'find-file-noselect)
                        (lambda (&rest _) (push 'find-file-noselect calls)))
                       ((symbol-function 'find-file-other-window)
                        (lambda (&rest _) (push 'find-file-other-window calls)))
                       ((symbol-function 'find-file-read-only)
                        (lambda (&rest _) (push 'find-file-read-only calls)))
                       ((symbol-function 'view-file)
                        (lambda (&rest _) (push 'view-file calls)))
                       ((symbol-function 'switch-to-buffer)
                        (lambda (&rest _) (push 'switch-to-buffer calls)))
                       ((symbol-function 'pop-to-buffer)
                        (lambda (&rest _) (push 'pop-to-buffer calls)))
                       ((symbol-function 'display-buffer)
                        (lambda (&rest _) (push 'display-buffer calls)))
                       ((symbol-function 'completing-read)
                        (lambda (&rest _) (push 'completing-read calls) ""))
                       ((symbol-function 'read-string)
                        (lambda (&rest _) (push 'read-string calls) ""))
                       ((symbol-function 'read-from-minibuffer)
                        (lambda (&rest _) (push 'read-from-minibuffer calls) ""))
                       ((symbol-function 'y-or-n-p)
                        (lambda (&rest _) (push 'y-or-n-p calls) nil))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) (push 'yes-or-no-p calls) nil))
                       ((symbol-function 'message)
                        (lambda (&rest _) (push 'message calls) nil)))
              (workspace-bookmark--resolve record workspace-name)))))
    (expect calls :to-equal nil)
    (expect (seq-remove (lambda (b) (memq b before)) (buffer-list))
            :to-equal nil)
    (push result portable-bookmarks-resolver-spec--results)
    result))

(cl-defun portable-bookmarks-resolver-spec--record
    (&key (filename portable-bookmarks-resolver-spec--stored)
          (identity portable-bookmarks-resolver-spec--identity)
          (relpath "README.md")
          (version workspace-bookmark-format-version))
  "Return a stamped bookmark record with the given FILENAME and stamp."
  (list "resolver-spec-bookmark"
        (cons 'filename filename)
        (cons workspace-bookmark-repo-property identity)
        (cons workspace-bookmark-relpath-property relpath)
        (cons workspace-bookmark-format-property version)))

(defun portable-bookmarks-resolver-spec--stamp (file)
  "Return the record the REAL stamp seam produces for FILE.
Fails the spec when FILE does not get stamped."
  (let ((record (workspace-bookmark--stamp-record
                 (list "resolver-spec-bookmark" (cons 'filename file)))))
    (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
            :not :to-be nil)
    record))

(defun portable-bookmarks-resolver-spec--commit-file (repo relpath &optional contents)
  "Create RELPATH in REPO with CONTENTS, commit it, and return its path."
  (let ((path (expand-file-name relpath repo)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path (insert (or contents "x\n")))
    (workspace-bookmark-fixtures-git repo "config" "user.email"
                                     "fixtures@example.invalid")
    (workspace-bookmark-fixtures-git repo "config" "user.name" "Fixtures")
    (workspace-bookmark-fixtures-git repo "config" "commit.gpgsign" "false")
    (workspace-bookmark-fixtures-git repo "add" "--" relpath)
    (workspace-bookmark-fixtures-git repo "commit" "--quiet" "-m" relpath)
    path))

(defun portable-bookmarks-resolver-spec--no-discovery (thunk)
  "Call THUNK, failing the spec if it runs either discovery function.
The decision is expected before any git runs.  Returns THUNK's value."
  (let ((ran nil))
    (cl-letf (((symbol-function 'workspace-bookmark--workspace-checkouts)
               (lambda (&rest _) (setq ran t) nil))
              ((symbol-function
                'workspace-bookmark--canonical-checkouts-for-identity)
               (lambda (&rest _) (setq ran t) nil)))
      (prog1 (funcall thunk)
        (expect ran :to-be nil)))))

(defmacro portable-bookmarks-resolver-spec--with-discovery (workspace canonical &rest body)
  "Run BODY with the plural discovery functions mocked.
WORKSPACE and CANONICAL are the lists they return."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'workspace-bookmark--workspace-checkouts)
              (lambda (&rest _) ,workspace))
             ((symbol-function
               'workspace-bookmark--canonical-checkouts-for-identity)
              (lambda (&rest _) ,canonical)))
     ,@body))


;;; Behavioral — one canonical clone, a workspace worktree, an empty workspace

(describe "Resolution against real checkouts"
  :var (repo home-a home-b wt)

  (before-each
    (portable-bookmarks-resolver-spec--setup)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-resolver-spec--remote))
    (portable-bookmarks-resolver-spec--commit-file repo "sub/deep.el")
    (portable-bookmarks-resolver-spec--commit-file repo " lead.txt")
    (portable-bookmarks-resolver-spec--commit-file repo "trail.txt ")
    (setq home-a (workspace-bookmark-fixtures-make-workspace "wsbm-res-a")
          home-b (workspace-bookmark-fixtures-make-workspace "wsbm-res-b")
          wt (workspace-bookmark-fixtures-add-worktree
              repo (expand-file-name "proj" home-a) :branch "a")))

  (after-each (portable-bookmarks-resolver-spec--teardown))

  (describe "resolved"
    (it "answers with the workspace's own copy, never the stored filename"
      (let* ((record (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name "README.md" repo)))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path)
                :to-equal (expand-file-name "README.md" wt))
        (expect (plist-get result :path)
                :not :to-equal (bookmark-prop-get record 'filename))
        (expect (plist-get result :identity)
                :to-equal portable-bookmarks-resolver-spec--identity)
        (expect (plist-get result :relpath) :to-equal "README.md")
        (expect (plist-get result :candidates) :to-be nil)))

    (it "wins over a canonical checkout in view (first match wins)"
      ;; Both predicates hold: the workspace holds a checkout AND the
      ;; canonical clone is in view.
      (let ((jf/emacs-dir nil))
        (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                   (lambda () nil)))
          (expect (workspace-bookmark--canonical-checkouts-for-identity
                   portable-bookmarks-resolver-spec--identity)
                  :to-equal (list repo))))
      (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                          (portable-bookmarks-resolver-spec--record)
                          "wsbm-res-a")
                         :outcome)
              :to-be 'resolved))

    (it "opens nothing: no buffer visits the resolved path"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record) "wsbm-res-a")))
        (expect (get-file-buffer (plist-get result :path)) :to-be nil))))

  (describe "not-on-branch"
    (it "reports a relpath the workspace's checkout lacks, with a nil :path"
      (delete-file (expand-file-name "README.md" wt))
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record) "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'not-on-branch)
        (expect (plist-get result :path) :to-be nil)
        (expect (plist-get result :relpath) :to-equal "README.md")
        (expect (get-file-buffer (expand-file-name "README.md" wt)) :to-be nil)))

    (it "reports a file committed on another branch only"
      (portable-bookmarks-resolver-spec--commit-file repo "main-only.el")
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name "main-only.el" repo))
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'not-on-branch)
        (expect (plist-get result :path) :to-be nil)))

    (it "reports a file relpath that names a directory on this branch"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record :relpath "sub")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'not-on-branch)
        (expect (plist-get result :path) :to-be nil)))

    (it "reports a directory relpath that names a file on this branch"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record
                      :relpath "README.md/")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'not-on-branch)
        (expect (plist-get result :path) :to-be nil))))

  (describe "missing-repo"
    (it "offers the canonical checkout's copy when the workspace holds none"
      (let* ((record (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name "README.md" wt)))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-b")))
        (expect (plist-get result :outcome) :to-be 'missing-repo)
        (expect (plist-get result :path)
                :to-equal (expand-file-name "README.md" repo))
        (expect (plist-get result :path)
                :not :to-equal (bookmark-prop-get record 'filename))
        (expect (get-file-buffer (plist-get result :path)) :to-be nil)))

    (it "answers the same with no workspace at all"
      (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                          (portable-bookmarks-resolver-spec--record) nil)
                         :outcome)
              :to-be 'missing-repo))

    (it "has a nil :path when the canonical checkout lacks the relpath"
      (let* ((file (portable-bookmarks-resolver-spec--commit-file
                    wt "branch-a-only.el"))
             (result (portable-bookmarks-resolver-spec--resolve
                      (portable-bookmarks-resolver-spec--stamp file)
                      "wsbm-res-b")))
        (expect (plist-get result :outcome) :to-be 'missing-repo)
        (expect (plist-get result :path) :to-be nil)
        (expect (plist-get result :relpath) :to-equal "branch-a-only.el"))))

  (describe "ambiguous"
    (it "lists two checkouts in the workspace and chooses neither"
      (let ((wt2 (workspace-bookmark-fixtures-add-worktree
                  repo (expand-file-name "proj2" home-a) :branch "a2")))
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record)
                       "wsbm-res-a")))
          (expect (plist-get result :outcome) :to-be 'ambiguous)
          (expect (plist-get result :path) :to-be nil)
          (expect (sort (copy-sequence (plist-get result :candidates))
                        #'string<)
                  :to-equal (sort (list wt wt2) #'string<)))))

    (it "lists two separate canonical clones and chooses neither"
      (let* ((repo2 (workspace-bookmark-fixtures-make-repo
                     :remote portable-bookmarks-resolver-spec--remote))
             (home-c (workspace-bookmark-fixtures-make-workspace "wsbm-res-c")))
        (workspace-bookmark-fixtures-add-worktree
         repo2 (expand-file-name "proj" home-c) :branch "c")
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record)
                       "wsbm-res-b")))
          (expect (plist-get result :outcome) :to-be 'ambiguous)
          (expect (plist-get result :path) :to-be nil)
          (expect (sort (copy-sequence (plist-get result :candidates))
                        #'string<)
                  :to-equal (sort (list repo repo2) #'string<))))))

  (describe "no-clone"
    (it "answers when no checkout of the identity is in view"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record
                      :identity "example.com/nobody/nothing")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'no-clone)
        (expect (plist-get result :path) :to-be nil)
        (expect (plist-get result :identity)
                :to-equal "example.com/nobody/nothing"))))

  (describe "relpath forms"
    (it "re-anchors the root \"./\" to the checkout root"
      (let* ((record (portable-bookmarks-resolver-spec--stamp repo))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-a")))
        (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
                :to-equal "./")
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path) :to-equal wt)))

    (it "re-anchors a directory relpath with its trailing slash"
      (let* ((record (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name "sub/" repo)))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-a")))
        (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
                :to-equal "sub/")
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path)
                :to-equal (expand-file-name "sub/" wt))))

    (it "keeps leading whitespace in a relpath verbatim"
      (let* ((record (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name " lead.txt" repo)))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-a")))
        (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
                :to-equal " lead.txt")
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path)
                :to-equal (concat wt " lead.txt"))))

    (it "keeps trailing whitespace in a relpath verbatim"
      (let* ((record (portable-bookmarks-resolver-spec--stamp
                      (expand-file-name "trail.txt " repo)))
             (result (portable-bookmarks-resolver-spec--resolve
                      record "wsbm-res-a")))
        (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
                :to-equal "trail.txt ")
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path)
                :to-equal (concat wt "trail.txt "))))

    (it "treats a trimmed spelling of a whitespace name as a different file"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record
                      :relpath "lead.txt")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'not-on-branch))))

  (describe "confinement of a relpath to its checkout (lexical only, DD-16)"
    (it "degrades `..' to non-portable, even when the target exists"
      (with-temp-file (expand-file-name "outside.txt" home-a) (insert "x"))
      (let ((result (portable-bookmarks-resolver-spec--no-discovery
                     (lambda ()
                       (portable-bookmarks-resolver-spec--resolve
                        (portable-bookmarks-resolver-spec--record
                         :relpath "../outside.txt")
                        "wsbm-res-a")))))
        (expect (plist-get result :outcome) :to-be 'non-portable)
        (expect (plist-get result :path)
                :to-equal portable-bookmarks-resolver-spec--stored)))

    (it "degrades a `..' that stays inside to non-portable too"
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record
                      :relpath "sub/../README.md")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'non-portable)))

    (it "degrades an absolute relpath to non-portable without running git"
      (dolist (relpath (list (expand-file-name "README.md" wt)
                             "~/README.md" "/etc/hosts"))
        (let ((result (portable-bookmarks-resolver-spec--no-discovery
                       (lambda ()
                         (portable-bookmarks-resolver-spec--resolve
                          (portable-bookmarks-resolver-spec--record
                           :relpath relpath)
                          "wsbm-res-a")))))
          (expect (plist-get result :outcome) :to-be 'non-portable)
          (expect (plist-get result :path)
                  :to-equal portable-bookmarks-resolver-spec--stored))))

    (it "resolves a symbolic link out of the workspace's checkout by its spelling"
      ;; The spelling is inside the checkout; where the link points is the
      ;; branch's business, exactly as visiting the path by hand.
      (let ((outside (workspace-bookmark-fixtures-temp-dir "wsbm-res-out-")))
        (with-temp-file (expand-file-name "secret.txt" outside) (insert "x"))
        (make-symbolic-link (directory-file-name outside)
                            (expand-file-name "link" wt))
        (dolist (relpath '("link/secret.txt" "link/"))
          (let ((result (portable-bookmarks-resolver-spec--resolve
                         (portable-bookmarks-resolver-spec--record
                          :relpath relpath)
                         "wsbm-res-a")))
            (expect (plist-get result :outcome) :to-be 'resolved)
            (expect (plist-get result :path)
                    :to-equal (expand-file-name relpath wt))
            (expect (plist-get result :path)
                    :not :to-equal portable-bookmarks-resolver-spec--stored)
            (expect (plist-get result :checkout) :to-equal wt)))))

    (it "offers a symbolic link out of the canonical checkout by its spelling"
      (let ((outside (workspace-bookmark-fixtures-temp-dir "wsbm-res-out-")))
        (with-temp-file (expand-file-name "secret.txt" outside) (insert "x"))
        (make-symbolic-link (directory-file-name outside)
                            (expand-file-name "link" repo))
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record
                        :relpath "link/secret.txt")
                       "wsbm-res-b")))
          (expect (plist-get result :outcome) :to-be 'missing-repo)
          (expect (plist-get result :path)
                  :to-equal (expand-file-name "link/secret.txt" repo))
          (expect (plist-get result :path)
                  :not :to-equal portable-bookmarks-resolver-spec--stored)
          (expect (plist-get result :checkout) :to-equal repo))))

    (it "follows a symbolic link that stays inside the checkout"
      (make-symbolic-link "sub" (expand-file-name "inlink" wt))
      (let ((result (portable-bookmarks-resolver-spec--resolve
                     (portable-bookmarks-resolver-spec--record
                      :relpath "inlink/deep.el")
                     "wsbm-res-a")))
        (expect (plist-get result :outcome) :to-be 'resolved)
        (expect (plist-get result :path)
                :to-equal (expand-file-name "inlink/deep.el" wt))))

    (it "reports a dangling link as not-on-branch"
      (make-symbolic-link "/nonexistent/wsbm-res-target"
                          (expand-file-name "dangling" wt))
      (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                          (portable-bookmarks-resolver-spec--record
                           :relpath "dangling")
                          "wsbm-res-a")
                         :outcome)
              :to-be 'not-on-branch))))


;;; Behavioral — a valid stamp resolved from a SECOND workspace, through links

;; The two reproductions from the on-touch audit of this task.  Each
;; record is stamped by the real stamp seam in workspace A and resolved in
;; workspace B, whose checkout reaches the relpath through a link that
;; leaves the checkout.  The only acceptable answer is B's own spelling of
;; the relpath (DD-16); the stored filename is A's copy, and returning it
;; would be the defect this capability exists to remove.

(describe "Resolution from a second workspace through links (DD-16)"
  :var (repo home-a home-b wt-a wt-b)

  (before-each
    (portable-bookmarks-resolver-spec--setup)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-resolver-spec--remote))
    (portable-bookmarks-resolver-spec--commit-file repo "sub/f.txt")
    ;; A tracked link that points out of whichever checkout holds it.
    (make-symbolic-link "../shared/x.txt" (expand-file-name "link-rel" repo))
    (workspace-bookmark-fixtures-git repo "add" "--" "link-rel")
    (workspace-bookmark-fixtures-git repo "commit" "--quiet" "-m" "link-rel")
    (setq home-a (workspace-bookmark-fixtures-make-workspace "wsbm-res2-a")
          home-b (workspace-bookmark-fixtures-make-workspace "wsbm-res2-b"))
    (dolist (home (list home-a home-b))
      (make-directory (expand-file-name "shared" home))
      (with-temp-file (expand-file-name "shared/x.txt" home) (insert home)))
    (setq wt-a (workspace-bookmark-fixtures-add-worktree
                repo (expand-file-name "proj" home-a) :branch "a")
          wt-b (workspace-bookmark-fixtures-add-worktree
                repo (expand-file-name "proj" home-b) :branch "b")))

  (after-each (portable-bookmarks-resolver-spec--teardown))

  (it "resolves a tracked link that leaves the checkout to the second workspace's spelling"
    (let* ((record (portable-bookmarks-resolver-spec--stamp
                    (expand-file-name "link-rel" wt-a)))
           (result (portable-bookmarks-resolver-spec--resolve
                    record "wsbm-res2-b")))
      (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
              :to-equal "link-rel")
      (expect (plist-get result :outcome) :to-be 'resolved)
      (expect (plist-get result :path)
              :to-equal (expand-file-name "link-rel" wt-b))
      (expect (plist-get result :path)
              :not :to-equal (bookmark-prop-get record 'filename))
      (expect (plist-get result :checkout) :to-equal wt-b)
      ;; B's spelling is B's own link, which reaches B's shared file, not A's.
      (expect (file-truename (plist-get result :path))
              :to-equal (file-truename (expand-file-name "shared/x.txt" home-b)))))

  (it "resolves a file whose parent is a link on the second workspace's branch to that spelling"
    ;; Nothing in A is a link.  On B's branch `sub' is a link to a directory
    ;; outside the checkout.
    (workspace-bookmark-fixtures-git wt-b "rm" "--quiet" "-r" "--" "sub")
    (make-directory (expand-file-name "vendor-sub" home-b))
    (with-temp-file (expand-file-name "vendor-sub/f.txt" home-b) (insert "b"))
    (make-symbolic-link "../vendor-sub" (expand-file-name "sub" wt-b))
    (workspace-bookmark-fixtures-git wt-b "add" "--" "sub")
    (workspace-bookmark-fixtures-git wt-b "commit" "--quiet" "-m" "sub is a link")
    (let* ((record (portable-bookmarks-resolver-spec--stamp
                    (expand-file-name "sub/f.txt" wt-a)))
           (result (portable-bookmarks-resolver-spec--resolve
                    record "wsbm-res2-b")))
      (expect (bookmark-prop-get record workspace-bookmark-relpath-property)
              :to-equal "sub/f.txt")
      (expect (plist-get result :outcome) :to-be 'resolved)
      (expect (plist-get result :path)
              :to-equal (expand-file-name "sub/f.txt" wt-b))
      (expect (plist-get result :path)
              :not :to-equal (bookmark-prop-get record 'filename))
      (expect (plist-get result :checkout) :to-equal wt-b)))

  (it "spells a valid stamp under whichever checkout the asking workspace holds"
    ;; Never non-portable, never the stored filename by way of a fallback:
    ;; the answer is always RELPATH under the ASKING side's checkout.
    (dolist (relpath (list "link-rel" "sub/f.txt"))
      (let ((record (portable-bookmarks-resolver-spec--stamp
                     (expand-file-name relpath wt-a))))
        (dolist (side (list (cons "wsbm-res2-a" wt-a)
                            (cons "wsbm-res2-b" wt-b)))
          (let ((result (portable-bookmarks-resolver-spec--resolve
                         record (car side))))
            (expect (plist-get result :outcome) :to-be 'resolved)
            (expect (plist-get result :path)
                    :to-equal (expand-file-name relpath (cdr side)))
            (expect (plist-get result :checkout) :to-equal (cdr side))))))))


;;; Unit — discovery mocked

(describe "Resolution with discovery mocked"
  :var (dir)

  (before-each
    (portable-bookmarks-resolver-spec--setup)
    (setq dir (workspace-bookmark-fixtures-temp-dir "wsbm-res-unit-"))
    (with-temp-file (expand-file-name "README.md" dir) (insert "x")))

  (after-each (portable-bookmarks-resolver-spec--teardown))

  (describe "no-clone"
    (it "answers when neither discovery function finds anything"
      (portable-bookmarks-resolver-spec--with-discovery nil nil
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record) "ws")))
          (expect result
                  :to-equal
                  (list :outcome 'no-clone :path nil
                        :identity portable-bookmarks-resolver-spec--identity
                        :relpath "README.md" :candidates nil
                        :checkout nil)))))

    (it "answers when workspace discovery signals, instead of propagating"
      (cl-letf (((symbol-function 'workspace-bookmark--workspace-checkouts)
                 (lambda (&rest _) (error "Boom"))))
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record) "ws")))
          (expect (plist-get result :outcome) :to-be 'no-clone)
          (expect (plist-get result :path) :to-be nil)
          (expect (plist-get result :identity)
                  :to-equal portable-bookmarks-resolver-spec--identity))))

    (it "answers when canonical discovery signals, instead of propagating"
      (cl-letf (((symbol-function 'workspace-bookmark--workspace-checkouts)
                 (lambda (&rest _) nil))
                ((symbol-function
                  'workspace-bookmark--canonical-checkouts-for-identity)
                 (lambda (&rest _) (signal 'file-error '("Boom")))))
        (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                            (portable-bookmarks-resolver-spec--record) "ws")
                           :outcome)
                :to-be 'no-clone)))

    (it "answers when re-anchoring signals, instead of propagating"
      (portable-bookmarks-resolver-spec--with-discovery (list dir) nil
        (cl-letf (((symbol-function 'workspace-bookmark--anchor)
                   (lambda (&rest _) (error "Boom"))))
          (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                              (portable-bookmarks-resolver-spec--record) "ws")
                             :outcome)
                  :to-be 'no-clone))))

    (it "answers for an anchoring answer nobody wrote code for, never a location"
      ;; The dispatch on the anchoring answer has no catch-all that returns
      ;; a location: an unknown answer signals, and the barrier maps it.
      (dolist (answer (list (list 'escapes)
                            (cons 'present-elsewhere "/x/README.md")
                            nil))
        (dolist (setup (list (list (list dir) nil)
                             (list nil (list dir))))
          (portable-bookmarks-resolver-spec--with-discovery
              (nth 0 setup) (nth 1 setup)
            (cl-letf (((symbol-function 'workspace-bookmark--anchor)
                       (lambda (&rest _) answer)))
              (let ((result (portable-bookmarks-resolver-spec--resolve
                             (portable-bookmarks-resolver-spec--record) "ws")))
                (expect (plist-get result :outcome) :to-be 'no-clone)
                (expect (plist-get result :path) :to-be nil)
                (expect (plist-get result :checkout) :to-be nil))))))))

  (describe "non-portable"
    (it "answers for a record with no stamp, with the stored filename"
      (let ((result (portable-bookmarks-resolver-spec--no-discovery
                     (lambda ()
                       (portable-bookmarks-resolver-spec--resolve
                        (list "plain" (cons 'filename "/x/y.el")) "ws")))))
        (expect result
                :to-equal
                (list :outcome 'non-portable :path "/x/y.el"
                      :identity nil :relpath nil :candidates nil
                      :checkout nil))))

    (it "answers for every record carrying only two of the three properties"
      (dolist (drop (list workspace-bookmark-repo-property
                          workspace-bookmark-relpath-property
                          workspace-bookmark-format-property))
        (let* ((full (portable-bookmarks-resolver-spec--record))
               (partial (cons (car full)
                              (seq-remove (lambda (p) (eq (car p) drop))
                                          (cdr full))))
               (result (portable-bookmarks-resolver-spec--no-discovery
                        (lambda ()
                          (portable-bookmarks-resolver-spec--resolve
                           partial "ws")))))
          (expect (plist-get result :outcome) :to-be 'non-portable)
          (expect (plist-get result :path)
                  :to-equal portable-bookmarks-resolver-spec--stored))))

    (it "answers for a format version this module does not know"
      (dolist (version (list (1+ workspace-bookmark-format-version) 0 "1" nil))
        (expect (plist-get (portable-bookmarks-resolver-spec--no-discovery
                            (lambda ()
                              (portable-bookmarks-resolver-spec--resolve
                               (portable-bookmarks-resolver-spec--record
                                :version version)
                               "ws")))
                           :outcome)
                :to-be 'non-portable)))

    (it "answers for a malformed identity or relpath"
      (dolist (record (list (portable-bookmarks-resolver-spec--record :identity "")
                            (portable-bookmarks-resolver-spec--record :identity 'sym)
                            (portable-bookmarks-resolver-spec--record :relpath "")
                            (portable-bookmarks-resolver-spec--record :relpath 42)
                            (portable-bookmarks-resolver-spec--record
                             :relpath "a\0b")))
        (expect (plist-get (portable-bookmarks-resolver-spec--no-discovery
                            (lambda ()
                              (portable-bookmarks-resolver-spec--resolve
                               record "ws")))
                           :outcome)
                :to-be 'non-portable)))

    (it "answers for a value that is not a record, without looking it up"
      (let ((bookmark-alist (list (portable-bookmarks-resolver-spec--record))))
        (dolist (value (list "resolver-spec-bookmark" nil 42
                             '("improper" . 5)))
          (let ((result (portable-bookmarks-resolver-spec--resolve value "ws")))
            (expect (plist-get result :outcome) :to-be 'non-portable)
            (expect (plist-get result :path) :to-be nil)))))

    (it "answers when an error strikes before the identity is known"
      (cl-letf (((symbol-function 'workspace-bookmark--stamped-coordinates)
                 (lambda (&rest _) (error "Boom"))))
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record) "ws")))
          (expect (plist-get result :outcome) :to-be 'non-portable)
          (expect (plist-get result :path)
                  :to-equal portable-bookmarks-resolver-spec--stored)))))

  (describe "the stored filename"
    (it "is used by no outcome but non-portable"
      (let ((stored (expand-file-name "README.md" dir)))
        (dolist (setup (list (list (list dir) nil)
                             (list nil (list dir))
                             (list (list dir dir) nil)
                             (list nil (list dir dir))
                             (list nil nil)))
          (portable-bookmarks-resolver-spec--with-discovery
              (nth 0 setup) (nth 1 setup)
            (let ((result (portable-bookmarks-resolver-spec--resolve
                           (portable-bookmarks-resolver-spec--record
                            :filename (concat stored ".stored"))
                           "ws")))
              (expect (plist-get result :outcome) :not :to-be 'non-portable)
              (expect (plist-get result :path)
                      :not :to-equal (concat stored ".stored"))))))))

  (describe "the :checkout key"
    (it "names the workspace checkout for resolved and not-on-branch"
      (portable-bookmarks-resolver-spec--with-discovery (list dir) nil
        (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                            (portable-bookmarks-resolver-spec--record) "ws")
                           :checkout)
                :to-equal dir)
        (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                            (portable-bookmarks-resolver-spec--record
                             :relpath "absent.el")
                            "ws")
                           :checkout)
                :to-equal dir)))

    (it "names the canonical checkout for missing-repo, with or without a :path"
      (portable-bookmarks-resolver-spec--with-discovery nil (list dir)
        (let ((present (portable-bookmarks-resolver-spec--resolve
                        (portable-bookmarks-resolver-spec--record) "ws"))
              (absent (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record
                        :relpath "absent.el")
                       "ws")))
          (expect (plist-get present :outcome) :to-be 'missing-repo)
          (expect (plist-get present :checkout) :to-equal dir)
          (expect (plist-get absent :outcome) :to-be 'missing-repo)
          (expect (plist-get absent :path) :to-be nil)
          (expect (plist-get absent :checkout) :to-equal dir))))

    (it "is nil for ambiguous and no-clone, and does not stand in for :candidates"
      (dolist (setup (list (list (list dir dir) nil)
                           (list nil (list dir dir))
                           (list nil nil)))
        (portable-bookmarks-resolver-spec--with-discovery
            (nth 0 setup) (nth 1 setup)
          (expect (plist-get (portable-bookmarks-resolver-spec--resolve
                              (portable-bookmarks-resolver-spec--record) "ws")
                             :checkout)
                  :to-be nil)))))

  (describe "ambiguous"
    (it "answers for two workspace checkouts without asking canonical discovery"
      (cl-letf (((symbol-function 'workspace-bookmark--workspace-checkouts)
                 (lambda (&rest _) (list "/a/" "/b/")))
                ((symbol-function
                  'workspace-bookmark--canonical-checkouts-for-identity)
                 (lambda (&rest _) (error "Must not be asked"))))
        (expect (portable-bookmarks-resolver-spec--resolve
                 (portable-bookmarks-resolver-spec--record) "ws")
                :to-equal
                (list :outcome 'ambiguous :path nil
                      :identity portable-bookmarks-resolver-spec--identity
                      :relpath "README.md" :candidates (list "/a/" "/b/")
                      :checkout nil))))

    (it "answers for two canonical checkouts"
      (portable-bookmarks-resolver-spec--with-discovery nil (list "/c1/" "/c2/")
        (let ((result (portable-bookmarks-resolver-spec--resolve
                       (portable-bookmarks-resolver-spec--record) "ws")))
          (expect (plist-get result :outcome) :to-be 'ambiguous)
          (expect (plist-get result :candidates)
                  :to-equal (list "/c1/" "/c2/"))))))

  (describe "the anchoring helper"
    (it "signals for a relpath that anchors outside the checkout, rather than answering"
      ;; Unreachable through the resolver (confinement refuses `..' first);
      ;; pinned so the helper's contract cannot quietly become an answer.
      (expect (workspace-bookmark--anchor "../README.md" dir) :to-throw 'error)
      (expect (workspace-bookmark--anchor "/nonexistent/wsbm-res/x" dir)
              :to-throw 'error))

    (it "anchors a hand-edited \".\" to the checkout itself, whose kind is a directory"
      (expect (workspace-bookmark--anchor "." dir) :to-equal (list 'absent))
      (expect (workspace-bookmark--anchor "./" dir)
              :to-equal (cons 'present (file-name-as-directory dir)))))

  (describe "the outcome constructor"
    (it "refuses a symbol outside the closed set"
      (expect (workspace-bookmark--outcome 'open-anyway) :to-throw 'error)
      (expect (workspace-bookmark--valid-outcome-p 'open-anyway) :to-be nil))

    (it "declares exactly six outcomes"
      (expect (sort (mapcar #'symbol-name workspace-bookmark--valid-outcomes)
                    #'string<)
              :to-equal '("ambiguous" "missing-repo" "no-clone"
                          "non-portable" "not-on-branch" "resolved")))))


;;; The closed set, across this whole file

(describe "Every outcome the resolver returned in this file"
  (it "is a member of the closed set, in a six-key plist"
    (expect portable-bookmarks-resolver-spec--results :not :to-be nil)
    (dolist (result portable-bookmarks-resolver-spec--results)
      (expect (workspace-bookmark--valid-outcome-p (plist-get result :outcome))
              :to-be t)
      (expect (seq-filter #'keywordp result)
              :to-equal '(:outcome :path :identity :relpath :candidates
                          :checkout))))

  (it "names a checkout exactly for resolved, not-on-branch and missing-repo"
    (dolist (result portable-bookmarks-resolver-spec--results)
      (let ((names (memq (plist-get result :outcome)
                         '(resolved not-on-branch missing-repo))))
        (expect (and (plist-get result :checkout) t)
                :to-be (and names t)))))

  (it "covers every member of the closed set"
    (dolist (outcome workspace-bookmark--valid-outcomes)
      (expect (seq-some (lambda (result)
                          (eq outcome (plist-get result :outcome)))
                        portable-bookmarks-resolver-spec--results)
              :to-be-truthy))))

(provide 'portable-bookmarks-resolver-spec)
;;; portable-bookmarks-resolver-spec.el ends here
