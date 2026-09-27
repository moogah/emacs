;;; portable-bookmarks-spec.el --- Smoke specs for the portable-bookmarks module -*- lexical-binding: t; -*-

;; Proves the harness before any behaviour exists: the module loads and
;; provides its feature, both advice seams are installed on the functions
;; the boundary names (and only those), and the shared fixtures can build
;; a real repository with a linked worktree, a registry entry and an
;; isolated bookmark file.
;;
;; Everything asserted here is scaffolding: that the advice EXISTS, that
;; the stamped property names are pinned, and that a record the stamp seam
;; cannot locate survives it unchanged.  Stamping behaviour itself is
;; specified in portable-bookmarks-stamp-spec.el; the jump seam is still
;; pass-through.

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

  (it "names the three stamped properties in exactly one place"
    ;; These symbols are serialised into the user's bookmark file, so the
    ;; literal values are part of the on-disk format.
    (expect workspace-bookmark-repo-property :to-equal 'workspace-bookmark-repo)
    (expect workspace-bookmark-relpath-property
            :to-equal 'workspace-bookmark-relpath)
    (expect workspace-bookmark-format-property
            :to-equal 'workspace-bookmark-format)
    (expect workspace-bookmark-format-version :to-equal 1))

  (it "makes the on-disk names constants, not user options"
    ;; DD-9 / OD-2: a customised name would orphan every existing stamp.
    (dolist (var '(workspace-bookmark-repo-property
                   workspace-bookmark-relpath-property
                   workspace-bookmark-format-property
                   workspace-bookmark-format-version))
      (expect (cons var (custom-variable-p var)) :to-equal (list var))))

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

  (it "returns a record it cannot locate unchanged and eq"
    ;; The seam's pass-through arm: no single existing location, no stamp,
    ;; and the very same object back.
    (dolist (record (list '(("filename" . "/tmp/x.el"))
                          '("gone" (filename . "/nonexistent/wsbm/x.el"))
                          '("tabs" (filename . "3 buffers"))))
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

(describe "portable-bookmarks fixtures: registry isolation"

  ;; `workspace-bookmark-fixtures-with-empty-registry' is the ONE construct
  ;; a spec may swap `workspace--registry' through.  These specs pin that
  ;; it hands each spec a fresh table and puts the live one back on every
  ;; exit, so a spec that runs after one of the isolated suites starts from
  ;; the same registry it would have without them.

  (describe "around the specs of a describe"
    :var (live)

    ;; Outer hooks bracket the macro's own: buttercup runs a parent's
    ;; `before-each' first and its `after-each' last, so LIVE is captured
    ;; before the swap and inspected after the restore.
    (before-each (setq live (workspace-bookmark-fixtures-live-registry)))
    (after-each
      (expect workspace--registry :to-be live)
      (expect (workspace-bookmark-fixtures-live-registry) :to-be live)
      (expect (gethash "wsbm-fx-inner" live) :to-be nil)
      (expect (gethash "wsbm-fx-stray" live) :to-be nil)
      (expect workspace-bookmark-fixtures--live-registry :to-be nil))

    (describe "with the macro installed"
      :var (home)

      (workspace-bookmark-fixtures-with-empty-registry
        (setq home (workspace-bookmark-fixtures-make-workspace "wsbm-fx-inner")))

      (it "gives the spec a fresh `equal' table holding only its own fixtures"
        (expect workspace--registry :not :to-be live)
        (expect (workspace-bookmark-fixtures-live-registry) :to-be live)
        (expect (hash-table-test workspace--registry) :to-be 'equal)
        (expect (hash-table-count workspace--registry) :to-equal 1)
        (expect (workspace--home (gethash "wsbm-fx-inner" workspace--registry))
                :to-equal (file-name-as-directory home))
        (expect (gethash "wsbm-fx-inner" live) :to-be nil))

      (it "keeps an entry the spec writes itself out of the live table"
        (puthash "wsbm-fx-stray" 'stray workspace--registry)
        (expect (gethash "wsbm-fx-stray" workspace--registry) :to-be 'stray))))

  (describe "on a non-local exit"
    ;; Driven by hand: a `before-each' that signals would fail the spec
    ;; that owns it, so the setup primitive the macro expands into is
    ;; called directly.  The after-each is the same safety net the teardown
    ;; describe uses; it is a no-op when the primitive did its job.
    (after-each (workspace-bookmark-fixtures-teardown))

    (it "keeps the scope open when setup signals, so the body stays isolated until after-each"
      (let ((live (workspace-bookmark-fixtures-live-registry))
            (home nil))
        (expect (workspace-bookmark-fixtures-setup-with-empty-registry
                 (lambda ()
                   (setq home (workspace-bookmark-fixtures-make-workspace
                               "wsbm-fx-boom"))
                   (error "fixture broke")))
                :to-throw 'error)
        ;; Still isolated: buttercup runs the spec body after a failed
        ;; before-each, and that body must not see the live table.
        (expect workspace--registry :not :to-be live)
        (expect (gethash "wsbm-fx-boom" workspace--registry) :not :to-be nil)
        (expect (gethash "wsbm-fx-boom" live) :to-be nil)
        ;; The after-each path restores everything.
        (workspace-bookmark-fixtures-teardown)
        (expect workspace--registry :to-be live)
        (expect (gethash "wsbm-fx-boom" live) :to-be nil)
        (expect workspace-bookmark-fixtures--live-registry :to-be nil)
        (expect workspace-bookmark-fixtures--registered-names :to-be nil)
        (expect (file-directory-p home) :to-be nil)))

    (it "restores the live table even when a temp directory cannot be deleted"
      (let ((live (workspace-bookmark-fixtures-live-registry)))
        (workspace-bookmark-fixtures-setup-with-empty-registry
         (lambda () (workspace-bookmark-fixtures-temp-dir "wsbm-fx-stuck-")))
        (expect workspace--registry :not :to-be live)
        (cl-letf (((symbol-function 'delete-directory)
                   (lambda (&rest _) (error "stuck"))))
          (expect (workspace-bookmark-fixtures-teardown) :to-throw 'error))
        (expect workspace--registry :to-be live)
        (expect workspace-bookmark-fixtures--live-registry :to-be nil))))

  (it "expands to the before-each / after-each pair the specs rely on"
    (expect (macroexpand-1
             '(workspace-bookmark-fixtures-with-empty-registry (setq repo 1)))
            :to-equal
            '(progn
               (before-each
                 (workspace-bookmark-fixtures-setup-with-empty-registry
                  (lambda () (setq repo 1))))
               (after-each
                 (workspace-bookmark-fixtures-teardown))))))

(describe "portable-bookmarks fixtures: tracked and untracked files"
  :var (repo)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo (workspace-bookmark-fixtures-make-repo)))

  (it "commit-file creates, adds and commits RELPATH and returns its path"
    (let ((path (workspace-bookmark-fixtures-commit-file
                 repo "sub/deep.el" "hello\n")))
      (expect path :to-equal (expand-file-name "sub/deep.el" repo))
      (expect (with-temp-buffer (insert-file-contents path) (buffer-string))
              :to-equal "hello\n")
      (expect (workspace-bookmark-fixtures-git repo "ls-files" "--" "sub/deep.el")
              :to-equal "sub/deep.el")
      (expect (workspace-bookmark-fixtures-git repo "status" "--porcelain")
              :to-equal "")
      (expect (workspace-bookmark-fixtures-git repo "rev-list" "--count" "HEAD")
              :to-equal "2")))

  (it "commit-file forces a name an ignore rule would skip into the index"
    (workspace-bookmark-fixtures-commit-file repo ".gitignore" "*.o\n")
    (workspace-bookmark-fixtures-commit-file repo "build/out.o")
    (expect (workspace-bookmark-fixtures-git repo "ls-files" "--" "build/out.o")
            :to-equal "build/out.o"))

  (it "commit-file commits on the branch of the checkout it is given"
    (let ((wt (workspace-bookmark-fixtures-add-worktree
               repo (expand-file-name
                     "wt" (workspace-bookmark-fixtures-temp-dir "wsbm-fx-wt-"))
               :branch "topic")))
      (workspace-bookmark-fixtures-commit-file wt "topic-only.el")
      (expect (workspace-bookmark-fixtures-git wt "ls-files" "--" "topic-only.el")
              :to-equal "topic-only.el")
      (expect (workspace-bookmark-fixtures-git repo "ls-files" "--" "topic-only.el")
              :to-equal "")))

  (it "write-file creates parent directories and leaves the file untracked"
    (let ((path (workspace-bookmark-fixtures-write-file repo "scratch/note.txt")))
      (expect path :to-equal (expand-file-name "scratch/note.txt" repo))
      (expect (with-temp-buffer (insert-file-contents path) (buffer-string))
              :to-equal "x\n")
      (expect (workspace-bookmark-fixtures-git repo "ls-files" "--" "scratch")
              :to-equal "")
      (expect (workspace-bookmark-fixtures-git repo "ls-files" "--others")
              :to-equal "scratch/note.txt"))))

;;; portable-bookmarks-spec.el ends here
