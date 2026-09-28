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
(require 'seq)
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
        (workspace-bookmark-fixtures-signalling 'delete-directory "stuck"
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

;;; The counting validator
;;
;; register/invariant/spec-registry-isolation-via-fixtures, validator 6.
;; Every stand-in the workspaces specs share has ONE home, and neither
;; home is a spec file: portable-bookmarks-fixtures.el (the registered-names
;; stand-in, the bookmark-jump harness, the signalling collaborator) and
;; the core helper workspaces-test-helpers.el (the fake remote file-name
;; handler), which core specs load without the consumer's fixtures.  This
;; spec reads every spec file of this directory and counts the shapes of an
;; inline copy, so a regrown copy fails the suite, not only a review.

(defconst portable-bookmarks-spec--stand-in-shapes
  '(("registered-names stand-in" . "[(]defun workspace--registered-names\\_>")
    ("fake remote file-name handler" . "remote-handler [(]operation")
    ("fake remote file-name handler" . "[(]defun [^ \t\n()]+ [(]operation &rest")
    ("file-remote-p stand-in" . "cl-letf.*file-remote[-]p")
    ("file-remote-p stand-in" . "[(]symbol-function[ \t\n]+#?'file-remote[-]p)")
    ("remote handler bound in the handler alist" . "file[-]name[-]handler[-]alist")
    ("file-remote-p stand-in" . "spy-on 'file-remote[-]p")
    ("jump harness --visits-p" . "--visits-p [(]path)")
    ("jump harness --visits-p" . "[(]defun [^ \t\n()]*--visits-p\\_>")
    ("jump harness --hermetic" . "[(]defmacro [^ \t\n()]*--hermetic\\_>"))
  "(SHAPE . REGEXP): inline stand-ins no spec file may define.
Each REGEXP is written so that it does not match its own source text
\(\"[(]\" for a parenthesis, \"[-]\" for a hyphen), because this file is
one of the files it reads.")

(defconst portable-bookmarks-spec--fixtures-stand-in-shapes
  '(("signalling collaborator"
     . "[(]symbol-function[ \t\n]+'[^ \t\n()]+)[ \t\n]*[(]lambda [(]&rest _) [(]error "))
  "(SHAPE . REGEXP) counted in the spec files that load the fixtures.
Those files have `workspace-bookmark-fixtures-signalling' to hand; a
core spec that does not load the fixtures is outside this count.
Written as `portable-bookmarks-spec--stand-in-shapes' is.")

(defconst portable-bookmarks-spec--planted-stand-ins
  (let ((template "{unless {fboundp 'workspace~~registered~names}
  {defun workspace~~registered~names {} nil}}
{defun planted~spec~~remote~handler {operation &rest args} nil}
{cl~letf* {{real {symbol~function
                    'file~remote~p}}} nil}
{let {{file~name~handler~alist {cons {cons \"planted\" {lambda {op &rest args} nil}}
                                     file~name~handler~alist}}} nil}
{defun planted~spec~~visits~p {path} nil}
{defmacro planted~spec~~hermetic {&rest body} body}
{cl~letf {{{symbol~function
            'planted~collaborator}
           {lambda {&rest _} {error \"Boom\"}}}}
  nil}
"))
    (dolist (sub '((?{ . ?\() (?} . ?\)) (?~ . ?-)) template)
      (setq template (subst-char-in-string (car sub) (cdr sub) template))))
  "One inline copy of every counted shape, to show the count can see them.
Spelled with stand-in characters in the source and translated at load
time, so this file itself holds no copy for the count to find.")

(defun portable-bookmarks-spec--stand-in-hits (text shapes)
  "Return (SHAPE LINE) for each match in TEXT of the (SHAPE . REGEXP) SHAPES."
  (with-temp-buffer
    (insert text)
    (let ((case-fold-search nil)
          (hits nil))
      (dolist (shape shapes)
        (goto-char (point-min))
        (while (re-search-forward (cdr shape) nil t)
          (push (list (car shape) (line-number-at-pos (match-beginning 0)))
                hits)))
      (nreverse hits))))

(defun portable-bookmarks-spec--spec-file-stand-ins (file)
  "Return (FILE SHAPE LINE) for every inline stand-in in spec FILE."
  (let* ((text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (shapes (append portable-bookmarks-spec--stand-in-shapes
                         (and (string-match-p
                               (regexp-quote "\"portable-bookmarks-fixtures.el\"")
                               text)
                              portable-bookmarks-spec--fixtures-stand-in-shapes))))
    (mapcar (lambda (hit) (cons (file-name-nondirectory file) hit))
            (portable-bookmarks-spec--stand-in-hits text shapes))))

(describe "portable-bookmarks fixtures: one home per stand-in"

  (it "finds no inline stand-in in any spec file"
    ;; Not vacuous: the count sees one planted copy of every shape, the
    ;; homes are loaded, and it reads the whole directory.
    (let ((planted (mapcar #'car (portable-bookmarks-spec--stand-in-hits
                                  portable-bookmarks-spec--planted-stand-ins
                                  (append portable-bookmarks-spec--stand-in-shapes
                                          portable-bookmarks-spec--fixtures-stand-in-shapes)))))
      (dolist (shape (append portable-bookmarks-spec--stand-in-shapes
                             portable-bookmarks-spec--fixtures-stand-in-shapes))
        (expect (cons (car shape) (and (member (car shape) planted) t))
                :to-equal (cons (car shape) t))))
    (dolist (home '(workspace--registered-names
                    workspace-bookmark-fixtures-hermetic
                    workspace-bookmark-fixtures-visits-p
                    workspace-bookmark-fixtures-signalling
                    workspaces-test-with-fake-remote))
      (expect (cons home (fboundp home)) :to-equal (cons home t)))
    (let ((files (directory-files portable-bookmarks-spec--this-dir t
                                  "-spec\\.el\\'")))
      (expect (length files) :to-be-greater-than 30)
      (expect (seq-mapcat #'portable-bookmarks-spec--spec-file-stand-ins files)
              :to-equal nil))))

;;; portable-bookmarks-spec.el ends here
