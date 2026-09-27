;;; portable-bookmarks-stamp-spec.el --- Stamp seam specs -*- lexical-binding: t; -*-

;; Specs for seam 1: the `:filter-return' advice on `bookmark-make-record'
;; that stamps portable coordinates onto new bookmark records.
;;
;; * BEHAVIORAL — real `git' against throwaway repositories and the real
;;   bookmark API (`bookmark-set', `bookmark-write-file', `bookmark-load')
;;   with the bookmark file isolated by the fixtures.  The thing under test
;;   is the translation of git's answers into a persisted record, so git is
;;   never imitated here.
;;
;; * UNIT — the Tier 1 gate on synthetic records, the error barrier, and
;;   the subprocess budget (DD-7), with spies scoped to one spec.

(require 'buttercup)
(require 'cl-lib)
(require 'bookmark)
(require 'dired)

(defconst portable-bookmarks-stamp-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-stamp-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-stamp-spec--this-dir))


;;; Helpers

(defconst portable-bookmarks-stamp-spec--remote "git@github.com:acme/widgets.git"
  "The remote every stamped fixture repository gets.")

(defconst portable-bookmarks-stamp-spec--identity "github.com/acme/widgets"
  "The identity `portable-bookmarks-stamp-spec--remote' normalizes to.")

(defvar portable-bookmarks-stamp-spec--buffers nil
  "Buffers a spec visited; killed after each spec.")

(defun portable-bookmarks-stamp-spec--track (root &rest relpaths)
  "Create and `git add' each of RELPATHS under ROOT.  Return ROOT.
Staged, not committed: the stamp seam treats an indexed file as tracked."
  (dolist (relpath relpaths)
    (workspace-bookmark-fixtures-write-file root relpath))
  (apply #'workspace-bookmark-fixtures-git root "add" "--" relpaths)
  root)

(defun portable-bookmarks-stamp-spec--visit (path)
  "Visit PATH (a file, or a directory in Dired) and return the buffer."
  (let ((buffer (if (file-directory-p path)
                    (dired-noselect path)
                  (find-file-noselect path))))
    (push buffer portable-bookmarks-stamp-spec--buffers)
    buffer))

(defun portable-bookmarks-stamp-spec--bookmark (path name)
  "Set bookmark NAME on PATH through `bookmark-set'; return the stored record."
  (with-current-buffer (portable-bookmarks-stamp-spec--visit path)
    (bookmark-set name))
  (bookmark-get-bookmark name))

(defun portable-bookmarks-stamp-spec--stamps (record)
  "Return the three stamped values of RECORD as a list (REPO RELPATH FORMAT)."
  (list (bookmark-prop-get record workspace-bookmark-repo-property)
        (bookmark-prop-get record workspace-bookmark-relpath-property)
        (bookmark-prop-get record workspace-bookmark-format-property)))

(defun portable-bookmarks-stamp-spec--unstamped-p (record)
  "Non-nil when RECORD carries none of the three stamped properties."
  (let ((alist (bookmark-get-bookmark-record record)))
    (not (or (assq workspace-bookmark-repo-property alist)
             (assq workspace-bookmark-relpath-property alist)
             (assq workspace-bookmark-format-property alist)))))

(defun portable-bookmarks-stamp-spec--expected (relpath)
  "The stamped triple expected for RELPATH in a fixture repository."
  (list portable-bookmarks-stamp-spec--identity relpath
        workspace-bookmark-format-version))

(defun portable-bookmarks-stamp-spec--record-for (path)
  "A minimal file record for PATH, as `bookmark-make-record' would build."
  (list (file-name-nondirectory path) (cons 'filename path)))

(defun portable-bookmarks-stamp-spec--repo (&optional remote)
  "Make a fixture repository with REMOTE (default: the standard remote)."
  (workspace-bookmark-fixtures-make-repo
   :remote (if (eq remote 'none) nil
             (or remote portable-bookmarks-stamp-spec--remote))))


;;; Behavioral — files

(describe "stamp seam: file bookmarks (real git, real bookmark API)"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache)
    (setq portable-bookmarks-stamp-spec--buffers nil))

  (after-each
    (dolist (buffer portable-bookmarks-stamp-spec--buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "stamps identity, relpath and format version, leaving filename alone"
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "lisp/deep/foo.el"))
           (path (expand-file-name "lisp/deep/foo.el" repo))
           (record (portable-bookmarks-stamp-spec--bookmark path "foo")))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected "lisp/deep/foo.el"))
      (expect (expand-file-name (bookmark-get-filename record)) :to-equal path)
      (expect (car record) :to-equal "foo")))

  (it "keeps every property stock Emacs wrote, in order"
    ;; Additive only: the stamped record is the stock record plus three
    ;; properties appended, nothing removed, reordered or rewritten.
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (path (expand-file-name "README.md" repo)))
      (with-current-buffer (portable-bookmarks-stamp-spec--visit path)
        (let* ((stock (unwind-protect
                          (progn (workspace-bookmark-remove-advice)
                                 (bookmark-make-record))
                        (workspace-bookmark-install-advice)))
               (stamped (bookmark-make-record))
               ;; `last-modified' is the time the record was made, not a
               ;; property of the file, so it differs between two calls.
               (timeless (lambda (alist) (assq-delete-all 'last-modified
                                                          (copy-alist alist)))))
          (expect (funcall timeless (cdr stamped)) :to-equal
                  (append (funcall timeless (cdr stock))
                          (list (cons workspace-bookmark-repo-property
                                      portable-bookmarks-stamp-spec--identity)
                                (cons workspace-bookmark-relpath-property "README.md")
                                (cons workspace-bookmark-format-property
                                      workspace-bookmark-format-version))))))))

  (it "stamps a file that is staged but never committed (tracked means indexed)"
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "staged.el"))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name "staged.el" repo) "staged")))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected "staged.el"))))

  (it "leaves a file in no repository unstamped"
    (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-plain-"))
           (path (workspace-bookmark-fixtures-write-file dir "notes.txt"))
           (record (portable-bookmarks-stamp-spec--bookmark path "plain")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p record) :to-be-truthy)
      (expect (expand-file-name (bookmark-get-filename record)) :to-equal path)))

  (it "leaves a tracked file in a repository with no remote unstamped"
    (let* ((repo (portable-bookmarks-stamp-spec--repo 'none))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name "README.md" repo) "no-remote")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p record) :to-be-truthy)))

  (it "leaves an untracked file in a repository with a remote unstamped"
    ;; DD-9 / OD-1: git does not report it, so there is no relpath, and no
    ;; fallback path is computed.
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (path (workspace-bookmark-fixtures-write-file
                  repo "scratch/untracked.el"))
           (record (portable-bookmarks-stamp-spec--bookmark path "untracked")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p record) :to-be-truthy)
      (expect (expand-file-name (bookmark-get-filename record)) :to-equal path)))

  (it "stamps the same identity and relpath from a linked worktree"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (parent (workspace-bookmark-fixtures-temp-dir "wsbm-wt-"))
           (wt (workspace-bookmark-fixtures-add-worktree
                repo (expand-file-name "checkout" parent)))
           (main (portable-bookmarks-stamp-spec--bookmark
                  (expand-file-name "README.md" repo) "main"))
           (linked (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name "README.md" wt) "linked")))
      (expect (portable-bookmarks-stamp-spec--stamps linked)
              :to-equal (portable-bookmarks-stamp-spec--stamps main))
      (expect (portable-bookmarks-stamp-spec--stamps linked)
              :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))))

  (it "stamps a file with unmerged index entries during a merge conflict"
    ;; `ls-files' prints a conflicted path once per stage (1, 2 and 3).
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (path (expand-file-name "README.md" repo))
           (git (lambda (&rest args)
                  (apply #'workspace-bookmark-fixtures-git repo args))))
      (funcall git "checkout" "--quiet" "-b" "theirs")
      (workspace-bookmark-fixtures-write-file repo "README.md" "theirs\n")
      (funcall git "commit" "--quiet" "-am" "theirs")
      (funcall git "checkout" "--quiet" "main")
      (workspace-bookmark-fixtures-write-file repo "README.md" "ours\n")
      (funcall git "commit" "--quiet" "-am" "ours")
      ;; The merge must stop with a conflict, so its non-zero exit is the
      ;; expected outcome and bypasses the fixture runner's error.
      (let ((default-directory repo))
        (expect (process-file "git" nil nil nil "merge" "--quiet" "theirs")
                :not :to-equal 0))
      (expect (length (split-string (funcall git "ls-files" "--" "README.md") "\n"))
              :to-equal 3)
      (expect (portable-bookmarks-stamp-spec--stamps
               (portable-bookmarks-stamp-spec--bookmark path "conflicted"))
              :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))))

  (it "survives bookmark-write-file and bookmark-load unchanged"
    ;; The property that makes the design additive: stock Emacs writes the
    ;; extra properties out and reads them back with no handler of ours.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "src/a.el"))
           (path (expand-file-name "src/a.el" repo))
           (record (portable-bookmarks-stamp-spec--bookmark path "round-trip"))
           (before (copy-tree record))
           (file (expand-file-name
                  "roundtrip-bookmarks"
                  (workspace-bookmark-fixtures-temp-dir "wsbm-rt-"))))
      (let ((inhibit-message t))
        (bookmark-write-file file)
        (setq bookmark-alist nil)
        (bookmark-load file t t))
      (let ((after (bookmark-get-bookmark "round-trip")))
        (expect (portable-bookmarks-stamp-spec--stamps after)
                :to-equal (portable-bookmarks-stamp-spec--expected "src/a.el"))
        (expect (bookmark-get-filename after)
                :to-equal (bookmark-get-filename before))
        (expect after :to-equal before)))))


;;; Behavioral — directories

(describe "stamp seam: directory (Dired) bookmarks"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache)
    (setq portable-bookmarks-stamp-spec--buffers nil))

  (after-each
    (dolist (buffer portable-bookmarks-stamp-spec--buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "stamps a tracked subdirectory with git's prefix, not a file list"
    ;; `ls-files -- DIR' would list lisp/deep/foo.el; the directory's own
    ;; relpath is what `rev-parse --show-prefix' reports inside it.
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (dir (file-name-as-directory (expand-file-name "lisp" repo))))
      (workspace-bookmark-fixtures-commit-file repo "lisp/deep/foo.el")
      (let ((record (portable-bookmarks-stamp-spec--bookmark dir "lisp-dir")))
        (expect (portable-bookmarks-stamp-spec--stamps record)
                :to-equal (portable-bookmarks-stamp-spec--expected "lisp/")))))

  (it "stamps the repository root as \"./\", which re-anchors to the root"
    ;; git spells the root's prefix as the empty string, and the runner
    ;; collapses empty output to nil; the root must still be stamped.
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (portable-bookmarks-stamp-spec--bookmark repo "root-dir"))
           (relpath (bookmark-prop-get record workspace-bookmark-relpath-property)))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected "./"))
      (expect (expand-file-name relpath repo) :to-equal repo)))

  (it "leaves a staged-but-never-committed directory unstamped, not its files"
    ;; Directories are asked of HEAD (a bounded lookup), files of the index.
    ;; A brand-new directory therefore degrades until its first commit.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "fresh/new.el"))
           (dir-record (portable-bookmarks-stamp-spec--bookmark
                        (file-name-as-directory (expand-file-name "fresh" repo))
                        "fresh-dir"))
           (file-record (portable-bookmarks-stamp-spec--bookmark
                         (expand-file-name "fresh/new.el" repo) "fresh-file")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p dir-record) :to-be-truthy)
      (expect (portable-bookmarks-stamp-spec--stamps file-record)
              :to-equal (portable-bookmarks-stamp-spec--expected "fresh/new.el"))))

  (it "leaves the root of a repository with no commit yet unstamped"
    ;; No HEAD, so no tree to find the root in.
    (let* ((repo (workspace-bookmark-fixtures-temp-dir "wsbm-unborn-")))
      (workspace-bookmark-fixtures-git repo "init" "--quiet")
      (workspace-bookmark-fixtures-git
       repo "remote" "add" "origin" portable-bookmarks-stamp-spec--remote)
      (expect (portable-bookmarks-stamp-spec--unstamped-p
               (portable-bookmarks-stamp-spec--bookmark repo "unborn"))
              :to-be-truthy)))

  (it "leaves a directory holding only untracked files unstamped"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (dir (file-name-directory
                 (workspace-bookmark-fixtures-write-file repo "build/out.o")))
           (record (portable-bookmarks-stamp-spec--bookmark dir "build-dir")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p record) :to-be-truthy)))

  (it "leaves the .git directory unstamped"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (file-name-as-directory (expand-file-name ".git" repo))
                    "git-dir")))
      (expect (portable-bookmarks-stamp-spec--unstamped-p record) :to-be-truthy))))


;;; Behavioral — path spellings

(describe "stamp seam: unusual path spellings"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache)
    (setq portable-bookmarks-stamp-spec--buffers nil))

  (after-each
    (dolist (buffer portable-bookmarks-stamp-spec--buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "stores non-ASCII, quote, backslash and space names verbatim"
    ;; Without -z git C-quotes these: "dir with spaces/caf\303\251 \"q\" \\b.txt".
    (let* ((relpath "dir with spaces/café \"q\" \\b.txt")
           (repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) relpath))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name relpath repo) "odd-name")))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected relpath))))

  (it "decodes git's output as UTF-8 whatever the process coding system"
    (let* ((relpath "naïve/résumé.txt")
           (repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) relpath))
           (record (let ((default-process-coding-system '(latin-1 . latin-1)))
                     (portable-bookmarks-stamp-spec--bookmark
                      (expand-file-name relpath repo) "latin"))))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected relpath))))

  (it "treats a bracketed file name literally, not as a glob"
    ;; Measured: `git ls-files -- glob[1].txt' also returns glob1.txt.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "glob[1].txt" "glob1.txt"))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name "glob[1].txt" repo) "glob")))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected "glob[1].txt"))))

  (it "stamps a file named like an option"
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "-v"))
           (record (portable-bookmarks-stamp-spec--bookmark
                    (expand-file-name "-v" repo) "dash")))
      (expect (portable-bookmarks-stamp-spec--stamps record)
              :to-equal (portable-bookmarks-stamp-spec--expected "-v"))))

  (it "stamps a file reached through a symlinked, unresolved repository path"
    ;; macOS temp paths: /var/... is a symlink to /private/var/..., and git
    ;; reports the resolved one.  Visiting through a symlink must still work.
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (links (workspace-bookmark-fixtures-temp-dir "wsbm-link-"))
           (link (expand-file-name "repo-link" links))
           (find-file-visit-truename nil))
      (make-symbolic-link (directory-file-name repo) link)
      (let ((record (portable-bookmarks-stamp-spec--bookmark
                     (expand-file-name "README.md" link) "via-link")))
        (expect (portable-bookmarks-stamp-spec--stamps record)
                :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))
        (expect (expand-file-name (bookmark-get-filename record))
                :to-equal (expand-file-name "README.md" link))))))


;;; Unit — the Tier 1 gate

(describe "workspace-bookmark--record-location"

  (it "rejects a tab-bookmark label such as \"3 buffers\" without running git"
    (spy-on 'workspace-bookmark--git :and-call-through)
    (let ((record '("tabs" (filename . "3 buffers") (position . 1))))
      (expect (workspace-bookmark--record-location record) :to-be nil)
      (expect (workspace-bookmark--stamp-record record) :to-be record)
      (expect 'workspace-bookmark--git :not :to-have-been-called)))

  (it "rejects a record whose buffer property nests a record"
    (let ((record `("nested" (filename . ,(expand-file-name "README.md" jf/emacs-dir))
                    (buffer . (("inner" (filename . "/tmp/x")))))))
      (expect (workspace-bookmark--record-location record) :to-be nil)
      (expect (workspace-bookmark--stamp-record record) :to-be record)))

  (it "rejects a record carrying a window tree"
    (let ((record `("win" (filename . ,(expand-file-name "README.md" jf/emacs-dir))
                    (window . (leaf (buffer "x"))))))
      (expect (workspace-bookmark--record-location record) :to-be nil)))

  (it "accepts a record whose buffer property is a plain name"
    (let* ((path (expand-file-name "README.md" jf/emacs-dir))
           (record `("named" (filename . ,path) (buffer . "README.md"))))
      (expect (workspace-bookmark--record-location record) :to-equal path)))

  (it "rejects a missing filename, a relative one, and one that does not exist"
    (dolist (record '(("none" (position . 1))
                      ("relative" (filename . "lisp/foo.el"))
                      ("missing" (filename . "/nonexistent/wsbm/foo.el"))
                      ("not-a-string" (filename . 42))))
      (expect (cons (car record) (workspace-bookmark--record-location record))
              :to-equal (list (car record)))))

  (it "rejects a TRAMP filename without touching the filesystem or git"
    (spy-on 'workspace-bookmark--git :and-call-through)
    (spy-on 'file-exists-p :and-call-through)
    (let ((record '("remote" (filename . "/ssh:nonexistent.invalid:/srv/repo/foo.el"))))
      (expect (workspace-bookmark--record-location record) :to-be nil)
      (expect (workspace-bookmark--stamp-record record) :to-be record)
      (expect 'workspace-bookmark--git :not :to-have-been-called)
      (expect 'file-exists-p :not :to-have-been-called)))

  (it "expands an abbreviated ~ filename"
    (let ((record `("home" (filename . ,(abbreviate-file-name
                                         (expand-file-name "README.md" jf/emacs-dir))))))
      (expect (workspace-bookmark--record-location record)
              :to-equal (expand-file-name "README.md" jf/emacs-dir)))))


;;; Unit — barrier, silence, atomicity, verification

(describe "workspace-bookmark--stamp-record"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache)
    (setq portable-bookmarks-stamp-spec--buffers nil))

  (after-each
    (dolist (buffer portable-bookmarks-stamp-spec--buffers)
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "still lets bookmark-set succeed, unstamped, when stamping signals"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (path (expand-file-name "README.md" repo)))
      (cl-letf (((symbol-function 'workspace-bookmark--coordinates)
                 (lambda (&rest _) (error "Boom"))))
        (let ((record (portable-bookmarks-stamp-spec--bookmark path "boom")))
          (expect record :not :to-be nil)
          (expect (expand-file-name (bookmark-get-filename record)) :to-equal path)
          (expect (portable-bookmarks-stamp-spec--unstamped-p record)
                  :to-be-truthy)))))

  (it "returns the original record, unmutated, when building the stamp signals"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (list "orig" (cons 'filename (expand-file-name "README.md" repo))))
           (snapshot (copy-tree record)))
      (cl-letf (((symbol-function 'workspace-bookmark--with-coordinates)
                 (lambda (&rest _) (error "Boom"))))
        (expect (workspace-bookmark--stamp-record record) :to-be record))
      (expect record :to-equal snapshot)))

  (it "does not modify the record it was given when it stamps"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (list "orig" (cons 'filename (expand-file-name "README.md" repo))))
           (snapshot (copy-tree record))
           (stamped (workspace-bookmark--stamp-record record)))
      (expect stamped :not :to-be record)
      (expect record :to-equal snapshot)
      (expect (portable-bookmarks-stamp-spec--stamps stamped)
              :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))))

  (it "replaces, rather than duplicates, stale stamped properties"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (list "stale"
                         (cons 'filename (expand-file-name "README.md" repo))
                         (cons workspace-bookmark-repo-property "old/identity")
                         (cons workspace-bookmark-relpath-property "old/path")))
           (stamped (workspace-bookmark--stamp-record record)))
      (expect (seq-count (lambda (prop)
                           (eq (car-safe prop) workspace-bookmark-repo-property))
                         (cdr stamped))
              :to-equal 1)
      (expect (portable-bookmarks-stamp-spec--stamps stamped)
              :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))))

  (it "neither messages nor prompts, and keeps the bookmark name"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (list "My Name" (cons 'filename (expand-file-name "README.md" repo))))
           (noise nil)
           (stamped
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest args) (push (cons 'message args) noise)))
                      ((symbol-function 'read-from-minibuffer)
                       (lambda (&rest args) (push (cons 'prompt args) noise)))
                      ((symbol-function 'yes-or-no-p)
                       (lambda (&rest args) (push (cons 'prompt args) noise)))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest args) (push (cons 'prompt args) noise))))
              (workspace-bookmark--stamp-record record))))
      (expect noise :to-be nil)
      (expect (car stamped) :to-equal "My Name")
      (expect (portable-bookmarks-stamp-spec--stamps stamped)
              :to-equal (portable-bookmarks-stamp-spec--expected "README.md"))))

  (it "does not stamp a location git does not report"
    ;; Amendment A: no `..' guard — a location outside the repository is
    ;; simply not reported by git.  Here the file is in no repository at all.
    (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-outside-"))
           (path (workspace-bookmark-fixtures-write-file dir "x.el")))
      (expect (workspace-bookmark--coordinates path) :to-be nil)))

  (it "refuses a relpath git reports that does not re-anchor to the location"
    ;; Defence against a misparsed or whitespace-trimmed git answer: the
    ;; reported path must name the SAME file under the root.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "other.el"))
           (path (expand-file-name "README.md" repo))
           (real (symbol-function 'workspace-bookmark--git)))
      (cl-letf (((symbol-function 'workspace-bookmark--git)
                 (lambda (dir &rest args)
                   (if (member "ls-files" args)
                       "other.el\0"
                     (apply real dir args)))))
        (expect (workspace-bookmark--coordinates path) :to-be nil)))))


;;; Unit — subprocess budget and identity memo (DD-7)

(describe "stamp seam subprocess budget"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache))

  (after-each
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "looks up a repository's identity once, however many records it stamps"
    (let ((repo (portable-bookmarks-stamp-spec--track
                 (portable-bookmarks-stamp-spec--repo) "a.el" "b/c.el")))
      (spy-on 'workspace-bookmark--remote-url :and-call-through)
      (dolist (relpath '("README.md" "a.el" "b/c.el" "a.el"))
        (expect (portable-bookmarks-stamp-spec--stamps
                 (workspace-bookmark--stamp-record
                  (portable-bookmarks-stamp-spec--record-for
                   (expand-file-name relpath repo))))
                :to-equal (portable-bookmarks-stamp-spec--expected relpath)))
      (expect 'workspace-bookmark--remote-url :to-have-been-called-times 1)))

  (it "memoises the absence of a remote too"
    (let ((repo (portable-bookmarks-stamp-spec--track
                 (portable-bookmarks-stamp-spec--repo 'none) "a.el")))
      (spy-on 'workspace-bookmark--remote-url :and-call-through)
      (dolist (relpath '("README.md" "a.el"))
        (expect (portable-bookmarks-stamp-spec--unstamped-p
                 (workspace-bookmark--stamp-record
                  (portable-bookmarks-stamp-spec--record-for
                   (expand-file-name relpath repo))))
                :to-be-truthy))
      (expect 'workspace-bookmark--remote-url :to-have-been-called-times 1)))

  (it "costs two git calls per file record once identity is memoised"
    (let ((repo (portable-bookmarks-stamp-spec--track
                 (portable-bookmarks-stamp-spec--repo) "a.el")))
      (workspace-bookmark--stamp-record
       (portable-bookmarks-stamp-spec--record-for (expand-file-name "README.md" repo)))
      (spy-on 'workspace-bookmark--git :and-call-through)
      (workspace-bookmark--stamp-record
       (portable-bookmarks-stamp-spec--record-for (expand-file-name "a.el" repo)))
      (expect 'workspace-bookmark--git :to-have-been-called-times 2)))

  (it "costs one bounded git call per directory record, root or not, whatever its size"
    ;; The tracked-check must not list the subtree: 300 committed files
    ;; would be about 3 KB of `ls-files' output, and the root lists all.
    (let ((repo (portable-bookmarks-stamp-spec--repo)))
      (apply #'portable-bookmarks-stamp-spec--track repo
             (mapcar (lambda (n) (format "sub/deep/file-%03d.el" n))
                     (number-sequence 1 300)))
      (workspace-bookmark-fixtures-git repo "commit" "--quiet" "-m" "spec")
      (workspace-bookmark--stamp-record
       (portable-bookmarks-stamp-spec--record-for (expand-file-name "README.md" repo)))
      (dolist (case (list (cons repo "./")
                          (cons (file-name-as-directory (expand-file-name "sub" repo))
                                "sub/")))
        (spy-on 'workspace-bookmark--git :and-call-through)
        (expect (portable-bookmarks-stamp-spec--stamps
                 (workspace-bookmark--stamp-record
                  (portable-bookmarks-stamp-spec--record-for (car case))))
                :to-equal (portable-bookmarks-stamp-spec--expected (cdr case)))
        (expect 'workspace-bookmark--git :to-have-been-called-times 1)
        (let ((out (spy-context-return-value
                    (car (spy-calls-all 'workspace-bookmark--git)))))
          ;; root, common dir, prefix, tree id: four short lines
          (expect (length (split-string out "\n")) :to-equal 4)
          (expect (< (length out) (+ 200 (* 2 (length repo)))) :to-be-truthy))
        (spy-calls-reset 'workspace-bookmark--git))))

  (it "costs one git call for a file in no repository"
    (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-plain-"))
           (path (workspace-bookmark-fixtures-write-file dir "x.el")))
      (spy-on 'workspace-bookmark--git :and-call-through)
      (workspace-bookmark--stamp-record (portable-bookmarks-stamp-spec--record-for path))
      (expect 'workspace-bookmark--git :to-have-been-called-times 1)))

  (it "never asks for identity for an untracked file"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (path (workspace-bookmark-fixtures-write-file repo "untracked.el")))
      (spy-on 'workspace-bookmark--remote-url :and-call-through)
      (workspace-bookmark--stamp-record (portable-bookmarks-stamp-spec--record-for path))
      (expect 'workspace-bookmark--remote-url :not :to-have-been-called)))

  (it "empties the memo when the module is loaded again"
    ;; A reload may have changed the normalizer whose output the memo holds.
    (puthash "/wsbm/stale/root/" (cons nil "stale.example/identity")
             workspace-bookmark--identity-memo)
    (load (expand-file-name "../portable-bookmarks.el"
                            portable-bookmarks-stamp-spec--this-dir)
          nil t)
    (expect (hash-table-count workspace-bookmark--identity-memo) :to-equal 0)
    (expect (advice-member-p #'workspace-bookmark--stamp-record
                             'bookmark-make-record)
            :to-be-truthy))

  (it "re-reads identity after origin is re-pointed mid-session"
    (let* ((repo (portable-bookmarks-stamp-spec--repo))
           (record (portable-bookmarks-stamp-spec--record-for
                    (expand-file-name "README.md" repo)))
           (config (expand-file-name ".git/config" repo)))
      (expect (car (portable-bookmarks-stamp-spec--stamps
                    (workspace-bookmark--stamp-record record)))
              :to-equal portable-bookmarks-stamp-spec--identity)
      (workspace-bookmark-fixtures-git
       repo "remote" "set-url" "origin" "https://github.com/acme/gadgets.git")
      ;; Pin a distinct mtime so the spec does not depend on filesystem
      ;; timestamp granularity.
      (set-file-times config (time-add (current-time) 10))
      (expect (car (portable-bookmarks-stamp-spec--stamps
                    (workspace-bookmark--stamp-record record)))
              :to-equal "github.com/acme/gadgets")))

  (it "re-reads identity after origin is re-pointed, reached through a symlink to a subdirectory"
    ;; Review finding A1 (stamp seam).  A relative --git-common-dir
    ;; (\"../../.git\", relative to git's RESOLVED directory) expanded
    ;; against the symlinked spelling named a config file that does not
    ;; exist; the memo's nil stamp then matched forever.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) "sub/deep/f.el"))
           (links (workspace-bookmark-fixtures-temp-dir "wsbm-sublink-"))
           (link (expand-file-name "linksub" links))
           (via (expand-file-name "deep/f.el" link))
           (record (portable-bookmarks-stamp-spec--record-for via))
           (config (expand-file-name ".git/config" repo)))
      (make-symbolic-link (expand-file-name "sub" repo) link)
      (expect (nth 1 (workspace-bookmark--checkout-position
                      (file-name-directory via)))
              :to-equal (expand-file-name ".git" repo))
      (expect (portable-bookmarks-stamp-spec--stamps
               (workspace-bookmark--stamp-record record))
              :to-equal (portable-bookmarks-stamp-spec--expected "sub/deep/f.el"))
      (workspace-bookmark-fixtures-git
       repo "remote" "set-url" "origin" "https://github.com/acme/gadgets.git")
      (set-file-times config (time-add (current-time) 10))
      (expect (car (portable-bookmarks-stamp-spec--stamps
                    (workspace-bookmark--stamp-record record)))
              :to-equal "github.com/acme/gadgets")))

  (it "never reuses a memoised identity whose config stamp is unreadable"
    (spy-on 'workspace-bookmark--root-identity :and-return-value "example.com/a/b")
    (dotimes (_ 2)
      (expect (workspace-bookmark--memoised-identity
               "/wsbm/no/root/" "/wsbm/no/such/common/.git")
              :to-equal "example.com/a/b"))
    (expect 'workspace-bookmark--root-identity :to-have-been-called-times 2)))


;;; Behavioral — the stamp names the file's own repository (wave-3 review)

(describe "stamp seam: the runner answers about the file's own repository"

  (before-each
    (workspace-bookmark-fixtures-setup)
    (workspace-bookmark-clear-identity-cache))

  (after-each
    (workspace-bookmark-clear-identity-cache)
    (workspace-bookmark-fixtures-teardown))

  (it "stamps the right identity with GIT_DIR, GIT_WORK_TREE or GIT_INDEX_FILE naming another repository"
    ;; Review finding A2 (stamp seam): with GIT_DIR inherited, a file in
    ;; one repository was stamped with another repository's identity.
    (let* ((other (portable-bookmarks-stamp-spec--repo
                   "https://gitlab.com/other/two.git"))
           (repo (portable-bookmarks-stamp-spec--repo))
           (record (portable-bookmarks-stamp-spec--record-for
                    (expand-file-name "README.md" repo))))
      (dolist (env (list (list (concat "GIT_DIR=" (expand-file-name ".git" other)))
                         (list (concat "GIT_DIR=" (expand-file-name ".git" other))
                               (concat "GIT_WORK_TREE=" (directory-file-name other))
                               (concat "GIT_INDEX_FILE="
                                       (expand-file-name ".git/index" other)))))
        (workspace-bookmark-clear-identity-cache)
        (let ((process-environment (append env process-environment)))
          (expect (cons env (portable-bookmarks-stamp-spec--stamps
                             (workspace-bookmark--stamp-record record)))
                  :to-equal (cons env (portable-bookmarks-stamp-spec--expected
                                       "README.md")))))))

  (it "stamps a file at the repository root whose name begins with a space"
    ;; The runner used to trim the whole of git's output, which removed
    ;; the leading blank from a root-level `ls-files -z' answer.
    (let* ((repo (portable-bookmarks-stamp-spec--track
                  (portable-bookmarks-stamp-spec--repo) " lead.txt"))
           (record (portable-bookmarks-stamp-spec--record-for
                    (expand-file-name " lead.txt" repo))))
      (expect (portable-bookmarks-stamp-spec--stamps
               (workspace-bookmark--stamp-record record))
              :to-equal (portable-bookmarks-stamp-spec--expected " lead.txt")))))

;;; portable-bookmarks-stamp-spec.el ends here
