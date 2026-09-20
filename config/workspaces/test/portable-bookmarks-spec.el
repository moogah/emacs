;;; portable-bookmarks-spec.el --- Smoke specs for the portable-bookmarks module -*- lexical-binding: t; -*-

;; Proves the harness before any behaviour exists: the module loads and
;; provides its feature, both advice seams are installed on the functions
;; the boundary names (and only those), and the shared fixtures can build
;; a real repository with a linked worktree, a registry entry and an
;; isolated bookmark file.
;;
;; Everything asserted here is scaffolding.  The stamp and jump seams are
;; deliberately pass-through at this point, so the seam specs assert only
;; that the advice EXISTS and that a pass-through record survives it
;; unchanged — behaviour arrives with the later tasks.

(require 'buttercup)
(require 'cl-lib)
(require 'bookmark)

(defconst portable-bookmarks-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-spec--this-dir))

(describe "portable-bookmarks module scaffold"

  (it "provides the workspace-prefixed feature symbol"
    ;; The file is portable-bookmarks.el but the feature is
    ;; workspace-portable-bookmarks — the package's convention for every
    ;; sibling (data-model.el -> workspace-data-model, and so on).
    (expect (featurep 'workspace-portable-bookmarks) :to-be-truthy))

  (it "names the two stamped properties in exactly one place"
    (expect workspace-bookmark-repo-property :to-equal 'workspace-bookmark-repo)
    (expect workspace-bookmark-relpath-property
            :to-equal 'workspace-bookmark-relpath))

  (it "installs advice on both bookmark seams"
    (expect (advice-member-p #'workspace-bookmark--stamp-record
                             'bookmark-make-record)
            :to-be-truthy)
    (expect (advice-member-p #'workspace-bookmark--handle-bookmark-advice
                             'bookmark-handle-bookmark)
            :to-be-truthy))

  (it "advises neither bookmark-jump nor find-file"
    ;; Advising `bookmark-jump' would miss bookmark-jump-other-window and
    ;; every completion front-end; advising `find-file' would be a far
    ;; broader surface than a bookmark concern warrants.
    (expect (advice-member-p #'workspace-bookmark--handle-bookmark-advice
                             'bookmark-jump)
            :to-be nil)
    (expect (advice-member-p #'workspace-bookmark--handle-bookmark-advice
                             'find-file)
            :to-be nil))

  (it "installs advice idempotently"
    ;; The whole suite shares one Emacs process, so the re-install has to
    ;; happen on the failure path too: a `buttercup' `expect' throws, which
    ;; would otherwise skip it and leave both seams un-advised for every
    ;; later spec — including the wave-2/3 seam specs.
    (unwind-protect
        (progn
          (workspace-bookmark-install-advice)
          (workspace-bookmark-install-advice)
          (workspace-bookmark-remove-advice)
          (expect (advice-member-p #'workspace-bookmark--stamp-record
                                   'bookmark-make-record)
                  :to-be nil))
      (workspace-bookmark-install-advice))
    (expect (advice-member-p #'workspace-bookmark--stamp-record
                             'bookmark-make-record)
            :to-be-truthy))

  (it "leaves records unchanged while the stamp seam is a stub"
    (let ((record '(("filename" . "/tmp/x.el"))))
      (expect (workspace-bookmark--stamp-record record) :to-be record))))

(describe "portable-bookmarks fixtures"

  (before-each (workspace-bookmark-fixtures-setup))
  (after-each  (workspace-bookmark-fixtures-teardown))

  (it "builds a git repository with one commit"
    (let ((repo (workspace-bookmark-fixtures-make-repo)))
      (expect (file-directory-p (expand-file-name ".git" repo)) :to-be-truthy)
      (expect (file-exists-p (expand-file-name "README.md" repo)) :to-be-truthy)
      (expect (workspace-bookmark-fixtures-git repo "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal "main")))

  (it "records a caller-supplied remote URL verbatim"
    (let ((repo (workspace-bookmark-fixtures-make-repo
                 :remote "git@github.com:acme/widgets.git")))
      (expect (workspace-bookmark-fixtures-git repo "remote" "get-url" "origin")
              :to-equal "git@github.com:acme/widgets.git")))

  (it "adds a linked worktree that git itself reports"
    (let* ((repo (workspace-bookmark-fixtures-make-repo))
           (parent (workspace-bookmark-fixtures-temp-dir "wsbm-wt-"))
           (wt (workspace-bookmark-fixtures-add-worktree
                repo (expand-file-name "checkout" parent) :branch "topic")))
      (expect (file-exists-p (expand-file-name "README.md" wt)) :to-be-truthy)
      (expect (workspace-bookmark-fixtures-git repo "worktree" "list" "--porcelain")
              :to-match (regexp-quote (directory-file-name wt)))
      (expect (workspace-bookmark-fixtures-git wt "rev-parse" "--abbrev-ref" "HEAD")
              :to-equal "topic")))

  (it "registers a workspace through the data-model constructor"
    (let* ((home (workspace-bookmark-fixtures-make-workspace "fixture-ws"))
           (ws (gethash "fixture-ws" workspace--registry)))
      (expect ws :not :to-be nil)
      (expect (workspace--name ws) :to-equal "fixture-ws")
      (expect (workspace--home ws) :to-equal (file-name-as-directory home))))

  (it "isolates the bookmark list from the real bookmark file"
    (expect bookmark-alist :to-be nil)
    (expect bookmark-bookmarks-timestamp :to-be nil)
    (expect (file-exists-p bookmark-default-file) :to-be nil)
    (expect (file-name-nondirectory bookmark-default-file)
            :to-equal "bookmarks")))

(describe "portable-bookmarks fixture teardown"

  ;; This describe drives setup/teardown by hand because teardown IS the
  ;; subject.  The safety net is still required: `workspace-bookmark-fixtures-teardown'
  ;; is idempotent and documented safe when nothing was created, so running it
  ;; again here costs nothing and stops a failed assertion from leaving the
  ;; bookmark globals, registry entries and temp dirs visible to later specs.
  (after-each (workspace-bookmark-fixtures-teardown))

  (it "deletes every temp directory it created and unregisters workspaces"
    (workspace-bookmark-fixtures-setup)
    (let* ((repo (workspace-bookmark-fixtures-make-repo))
           (home (workspace-bookmark-fixtures-make-workspace "teardown-ws"))
           (saved-bookmark-file bookmark-default-file))
      (expect (file-directory-p repo) :to-be-truthy)
      (workspace-bookmark-fixtures-teardown)
      (expect (file-directory-p repo) :to-be nil)
      (expect (file-directory-p home) :to-be nil)
      (expect (gethash "teardown-ws" workspace--registry) :to-be nil)
      (expect (file-directory-p (file-name-directory saved-bookmark-file))
              :to-be nil))))

;;; portable-bookmarks-spec.el ends here
