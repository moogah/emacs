;;; portable-bookmarks-discovery-spec.el --- Checkout discovery specs -*- lexical-binding: t; -*-

;; Specs for checkout discovery and classification:
;; `workspace-bookmark--checkouts-of', `workspace-bookmark--canonical-checkout',
;; `workspace-bookmark--classify', `workspace-bookmark--workspace-checkout'
;; and `workspace-bookmark--canonical-for-identity' (the canonical-safety
;; rule).
;;
;; BEHAVIORAL specs run real `git' against throwaway repositories built by
;; `portable-bookmarks-fixtures.el': what is under test is the reading of
;; git's own worktree output, including the layouts where git's answers are
;; surprising (bare main repositories, submodules, prunable entries).
;;
;; UNIT specs mock the git runner or the registry helpers with `cl-letf',
;; scoped to the call under test, for the failure paths and for path
;; spellings no fixture produces directly.
;;
;; Every spec runs against a FRESH `workspace--registry' and with
;; `jf/emacs-dir' bound to nil, so the developer's real workspaces and the
;; configuration checkout running the suite are never in view.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

(defconst portable-bookmarks-discovery-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-discovery-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-discovery-spec--this-dir))

(defvar jf/emacs-dir)
(defvar workspace-home-builder)

;; `workspace--registered-names' is owned by tabs.el, which the test harness
;; loads through init.el.  tabs.el cannot be loaded on its own (it requires
;; the rest of the package by feature name), so when this file is run
;; outside the harness, supply the same one-line registry walk.  Under the
;; harness the real definition is already bound and this is skipped.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))

(defconst portable-bookmarks-discovery-spec--remote
  "git@example.com:fixtures/discovery.git"
  "Remote URL given to the fixture repositories.")

(defconst portable-bookmarks-discovery-spec--identity
  "example.com/fixtures/discovery"
  "The identity `portable-bookmarks-discovery-spec--remote' normalizes to.")

(defvar portable-bookmarks-discovery-spec--saved-registry nil
  "The real `workspace--registry', set aside for the duration of a spec.")

(defun portable-bookmarks-discovery-spec--setup ()
  "Open a fixture scope with an empty registry and no config checkout."
  (setq portable-bookmarks-discovery-spec--saved-registry workspace--registry
        workspace--registry (make-hash-table :test 'equal))
  (workspace-bookmark-fixtures-setup))

(defun portable-bookmarks-discovery-spec--teardown ()
  "Close the fixture scope and restore the real registry."
  (workspace-bookmark-fixtures-teardown)
  (setq workspace--registry portable-bookmarks-discovery-spec--saved-registry
        portable-bookmarks-discovery-spec--saved-registry nil))

(defmacro portable-bookmarks-discovery-spec--hermetic (&rest body)
  "Run BODY with no current workspace and `jf/emacs-dir' unbound from view."
  (declare (indent 0))
  `(let ((jf/emacs-dir nil))
     (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                (lambda () nil)))
       ,@body)))

(defun portable-bookmarks-discovery-spec--sorted (paths)
  "Return PATHS sorted, for order-insensitive comparison."
  (sort (copy-sequence paths) #'string<))


;;; Behavioral — one repository, two linked worktrees

(describe "Checkout discovery against a real repository"
  :var (repo owned stale home)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-discovery-spec--remote)
          home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-a")
          owned (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "discovery" home) :branch "owned")
          stale (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name
                       "stale sibling"
                       (workspace-bookmark-fixtures-temp-dir "wsbm-stale-"))
                 :branch "stale")))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (describe "workspace-bookmark--checkouts-of"

    (it "finds all three checkouts from any one of them"
      (let ((all (portable-bookmarks-discovery-spec--sorted
                  (list repo owned stale))))
        (dolist (from (list repo owned stale))
          (expect (cons from (portable-bookmarks-discovery-spec--sorted
                              (workspace-bookmark--checkouts-of from)))
                  :to-equal (cons from all)))))

    (it "lists the main worktree first"
      (expect (car (workspace-bookmark--checkouts-of stale)) :to-equal repo))

    (it "accepts a file or a subdirectory inside a checkout"
      (make-directory (expand-file-name "sub/dir" owned) t)
      (expect (length (workspace-bookmark--checkouts-of
                       (expand-file-name "README.md" stale)))
              :to-equal 3)
      (expect (length (workspace-bookmark--checkouts-of
                       (expand-file-name "sub/dir" owned)))
              :to-equal 3))

    (it "keeps a path containing spaces intact"
      (expect (workspace-bookmark--checkouts-of repo) :to-contain stale)
      (expect stale :to-match "stale sibling/\\'"))

    (it "drops a prunable worktree whose directory is gone"
      (let ((gone (workspace-bookmark-fixtures-add-worktree
                   repo (expand-file-name
                         "gone" (workspace-bookmark-fixtures-temp-dir))
                   :branch "gone")))
        (delete-directory gone t)
        (expect (workspace-bookmark-fixtures-git repo "worktree" "list"
                                                 "--porcelain")
                :to-match "^prunable")
        (expect (workspace-bookmark--checkouts-of repo) :not :to-contain gone)
        (expect (length (workspace-bookmark--checkouts-of repo)) :to-equal 3)))

    (it "keeps a locked worktree that exists (the ~/.emacs.d shape)"
      (workspace-bookmark-fixtures-git repo "worktree" "lock"
                                       "--reason" "Production" stale)
      (expect (workspace-bookmark--checkouts-of repo) :to-contain stale))

    (it "drops a locked worktree whose directory is missing"
      (workspace-bookmark-fixtures-git repo "worktree" "lock" stale)
      (delete-directory stale t)
      ;; git never marks a locked entry prunable, so only the existence
      ;; test stands between the caller and a directory that is not there.
      (expect (workspace-bookmark-fixtures-git repo "worktree" "list"
                                               "--porcelain")
              :not :to-match "^prunable")
      (expect (workspace-bookmark--checkouts-of repo) :not :to-contain stale))

    (it "keeps a detached worktree"
      (let ((detached (workspace-bookmark-fixtures-track-dir
                       (expand-file-name
                        "detached" (workspace-bookmark-fixtures-temp-dir)))))
        (workspace-bookmark-fixtures-git repo "worktree" "add" "--quiet"
                                         "--detach"
                                         (directory-file-name detached))
        (expect (workspace-bookmark--checkouts-of repo) :to-contain detached))))

  (describe "workspace-bookmark--canonical-checkout"

    (it "is the main worktree from every checkout"
      (dolist (from (list repo owned stale))
        (expect (cons from (workspace-bookmark--canonical-checkout from))
                :to-equal (cons from repo))))

    (it "is absolute from inside the main worktree, where git's plain answer is relative"
      ;; Plain `--git-common-dir' prints ".git" at the root and "../.git"
      ;; one level down; the helper must never hand either back.
      (make-directory (expand-file-name "sub" repo) t)
      (expect (workspace-bookmark-fixtures-git repo "rev-parse"
                                               "--git-common-dir")
              :to-equal ".git")
      (expect (workspace-bookmark--canonical-checkout repo) :to-equal repo)
      (expect (workspace-bookmark--canonical-checkout
               (expand-file-name "sub" repo))
              :to-equal repo)
      (expect (workspace-bookmark--canonical-checkout
               (expand-file-name "README.md" repo))
              :to-equal repo))

    (it "is nil outside any repository"
      (expect (workspace-bookmark--canonical-checkout
               (workspace-bookmark-fixtures-temp-dir))
              :to-be nil)))

  (describe "workspace-bookmark--classify"

    (it "classifies the main worktree canonical"
      (expect (workspace-bookmark--classify repo) :to-be 'canonical))

    (it "classifies a linked worktree, whose .git is a FILE, as not canonical"
      (expect (file-regular-p (expand-file-name ".git" stale)) :to-be-truthy)
      (expect (workspace-bookmark--classify stale) :to-be 'other))

    (it "classifies a worktree inside a registered workspace home workspace-owned"
      (expect (workspace-bookmark--classify owned) :to-be 'workspace-owned))

    (it "classifies the same worktree `other' once its workspace is gone"
      (remhash "wsbm-disc-a" workspace--registry)
      (expect (workspace-bookmark--classify owned) :to-be 'other)))

  (describe "workspace-bookmark--workspace-checkout"

    (it "finds the checkout inside the workspace's home"
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity "wsbm-disc-a")
              :to-equal owned))

    (it "lists exactly that one checkout, so it is unique"
      (expect (workspace-bookmark--workspace-checkouts
               portable-bookmarks-discovery-spec--identity "wsbm-disc-a")
              :to-equal (list owned)))

    (it "is nil for an identity the workspace does not hold"
      (expect (workspace-bookmark--workspace-checkout
               "example.com/fixtures/absent" "wsbm-disc-a")
              :to-be nil))

    (it "is nil for a workspace that holds no checkout"
      (workspace-bookmark-fixtures-make-workspace "wsbm-disc-empty")
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity "wsbm-disc-empty")
              :to-be nil))

    (it "is nil for an unregistered workspace"
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity "wsbm-no-such")
              :to-be nil))

    (it "ignores a grandchild checkout"
      (let ((nested-home (workspace-bookmark-fixtures-make-workspace
                          "wsbm-disc-nested")))
        (make-directory (expand-file-name "group" nested-home))
        (workspace-bookmark-fixtures-add-worktree
         repo (expand-file-name "group/deep" nested-home) :branch "deep")
        (expect (workspace-bookmark--workspace-checkout
                 portable-bookmarks-discovery-spec--identity "wsbm-disc-nested")
                :to-be nil))))

  (describe "workspace-bookmark--canonical-for-identity"

    (it "returns the main worktree, reached through a workspace's worktree"
      ;; The main worktree is NOT in view; git expands the in-view
      ;; workspace worktree to it.
      (portable-bookmarks-discovery-spec--hermetic
        (expect (workspace-bookmark--checkouts-in-view) :not :to-contain repo)
        (expect (workspace-bookmark--canonical-for-identity
                 portable-bookmarks-discovery-spec--identity)
                :to-equal repo)))

    (it "never returns the workspace-owned or the stale worktree"
      (portable-bookmarks-discovery-spec--hermetic
        (let ((answer (workspace-bookmark--canonical-for-identity
                       portable-bookmarks-discovery-spec--identity)))
          (expect answer :not :to-equal owned)
          (expect answer :not :to-equal stale))))

    (it "is nil when no checkout of the identity is in view"
      (portable-bookmarks-discovery-spec--hermetic
        (remhash "wsbm-disc-a" workspace--registry)
        (expect (workspace-bookmark--canonical-for-identity
                 portable-bookmarks-discovery-spec--identity)
                :to-be nil)))

    (it "is nil for an identity nothing in view has"
      (portable-bookmarks-discovery-spec--hermetic
        (expect (workspace-bookmark--canonical-for-identity
                 "example.com/fixtures/absent")
                :to-be nil)))

    (it "reaches the repository through the configuration checkout"
      (portable-bookmarks-discovery-spec--hermetic
        (remhash "wsbm-disc-a" workspace--registry)
        (let ((jf/emacs-dir stale))
          (expect (workspace-bookmark--canonical-for-identity
                   portable-bookmarks-discovery-spec--identity)
                  :to-equal repo))))))


;;; Behavioral — layouts where the two canonical signals disagree

(describe "Canonical checkout when git's two signals disagree"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "is nil for a worktree of a bare main repository"
    (let* ((seed (workspace-bookmark-fixtures-make-repo
                  :remote portable-bookmarks-discovery-spec--remote))
           (parent (workspace-bookmark-fixtures-temp-dir "wsbm-bare-"))
           (bare (expand-file-name "repo.git" parent))
           (wt (expand-file-name "wt" parent)))
      (workspace-bookmark-fixtures-git parent "clone" "--quiet" "--bare"
                                       seed bare)
      (workspace-bookmark-fixtures-add-worktree bare wt :branch "wt")
      ;; The parent of the common dir would be PARENT — not a checkout.
      (expect (workspace-bookmark--canonical-checkout wt) :to-be nil)
      (expect (workspace-bookmark--checkouts-of wt)
              :to-equal (list (file-name-as-directory wt)))))

  (it "is nil for a bare repository stored as D/.git, which passes the .git-is-a-directory test"
    (let* ((seed (workspace-bookmark-fixtures-make-repo
                  :remote portable-bookmarks-discovery-spec--remote))
           (d (workspace-bookmark-fixtures-temp-dir "wsbm-dotgit-"))
           (wt (expand-file-name "wt" d)))
      (workspace-bookmark-fixtures-git d "clone" "--quiet" "--bare"
                                       seed (expand-file-name ".git" d))
      (workspace-bookmark-fixtures-add-worktree
       (expand-file-name ".git" d) wt :branch "wt")
      ;; The common-dir signal alone says D; the classifier's cheap test
      ;; agrees.  Only the bare first porcelain entry refuses it.
      (expect (workspace-bookmark--classify d) :to-be 'canonical)
      (expect (workspace-bookmark--canonical-checkout wt) :to-be nil)))

  (it "is nil inside a submodule, whose .git is a FILE"
    (let* ((sub-origin (workspace-bookmark-fixtures-make-repo
                        :remote portable-bookmarks-discovery-spec--remote))
           (super (workspace-bookmark-fixtures-make-repo))
           (sm (file-name-as-directory (expand-file-name "sm" super))))
      (workspace-bookmark-fixtures-git
       super "-c" "protocol.file.allow=always"
       "submodule" "add" "--quiet" sub-origin "sm")
      (expect (file-regular-p (expand-file-name ".git" sm)) :to-be-truthy)
      (expect (workspace-bookmark--classify sm) :to-be 'other)
      (expect (workspace-bookmark--canonical-checkout sm) :to-be nil)
      ;; git reports the submodule's git dir as its "main worktree"; it is
      ;; not a checkout and must not be listed as one.
      (expect (workspace-bookmark--checkouts-of sm) :to-be nil)))

  (it "is nil when core.worktree puts the main work tree somewhere else"
    ;; The one layout where both signals AGREE and are both wrong: git's
    ;; main-worktree slot is the common dir's parent D, but D's files live
    ;; in the core.worktree directory.
    (let* ((d (workspace-bookmark-fixtures-make-repo
               :remote portable-bookmarks-discovery-spec--remote))
           (real (workspace-bookmark-fixtures-temp-dir "wsbm-coreworktree-"))
           (wt (workspace-bookmark-fixtures-add-worktree
                d (expand-file-name
                   "wt" (workspace-bookmark-fixtures-temp-dir "wsbm-cw-wt-"))
                :branch "wt")))
      (copy-file (expand-file-name "README.md" d)
                 (expand-file-name "README.md" real))
      (workspace-bookmark-fixtures-git d "config" "core.worktree"
                                       (directory-file-name real))
      ;; Preconditions: the two signals agree on D, and D is a main
      ;; worktree by the cheap structural test ...
      (expect (plist-get (car (workspace-bookmark--worktree-entries wt)) :path)
              :to-equal (directory-file-name d))
      (expect (file-directory-p (expand-file-name ".git" d)) :to-be-truthy)
      ;; ... yet git's own top level for D is the core.worktree directory.
      (expect (file-name-as-directory
               (workspace-bookmark-fixtures-git d "rev-parse" "--show-toplevel"))
              :to-equal real)
      (expect (workspace-bookmark--canonical-checkout wt) :to-be nil)
      (expect (workspace-bookmark--canonical-checkout d) :to-be nil))))


;;; Behavioral — a `.git'-named git directory outside its work tree
;;
;; Review finding 1 (wave3-review-fixes).  When the real git directory is
;; named `.git' but lives elsewhere, every git signal agrees on the
;; directory that CONTAINS it — a husk holding nothing but `.git'.

(defun portable-bookmarks-discovery-spec--husk-only-p (husk)
  "Return non-nil when HUSK holds nothing but `.git'."
  (equal (directory-files husk nil directory-files-no-dot-files-regexp)
         '(".git")))

(describe "Canonical checkout when a `.git'-named git directory lives outside the work tree"
  :var (main store husk)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    (setq main (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-discovery-spec--remote)
          store (workspace-bookmark-fixtures-temp-dir "wsbm-store-")
          husk (file-name-as-directory (expand-file-name "husk" store)))
    (make-directory husk))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "is nil for `git init --separate-git-dir=<dir>/.git', not the husk"
    ;; Re-running init with --separate-git-dir MOVES the git directory and
    ;; leaves a gitfile in MAIN: the same layout as a fresh
    ;; `git init --separate-git-dir=HUSK/.git MAIN'.
    (workspace-bookmark-fixtures-git
     main "init" "--quiet"
     (concat "--separate-git-dir=" (expand-file-name ".git" husk)))
    (let* ((home-b (workspace-bookmark-fixtures-make-workspace "wsbm-disc-sep-b"))
           (wt (workspace-bookmark-fixtures-add-worktree
                main (expand-file-name "sep" home-b) :branch "b")))
      ;; Preconditions: MAIN's .git is a gitfile, git's main-worktree slot
      ;; names the husk, and the husk holds only the git directory.
      (expect (file-regular-p (expand-file-name ".git" main)) :to-be-truthy)
      (expect (file-name-as-directory
               (plist-get (car (workspace-bookmark--worktree-entries wt)) :path))
              :to-equal husk)
      (expect (portable-bookmarks-discovery-spec--husk-only-p husk)
              :to-be-truthy)
      (expect (workspace-bookmark--canonical-checkout wt) :to-be nil)
      (expect (workspace-bookmark--canonical-checkout main) :to-be nil)
      (portable-bookmarks-discovery-spec--hermetic
        (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                   (lambda () "wsbm-disc-sep-b")))
          (expect (workspace-bookmark--canonical-for-identity
                   portable-bookmarks-discovery-spec--identity)
                  :to-be nil)))))

  (it "is nil for a main worktree whose .git is a symlink, not the husk"
    ;; The git directory kept on another disk or outside a sync folder.
    ;; The configuration checkout is the only thing in view.
    (rename-file (expand-file-name ".git" main) (expand-file-name ".git" husk))
    (make-symbolic-link (expand-file-name ".git" husk)
                        (expand-file-name ".git" main))
    (expect (file-symlink-p (expand-file-name ".git" main)) :to-be-truthy)
    (expect (file-name-as-directory
             (plist-get (car (workspace-bookmark--worktree-entries main)) :path))
            :to-equal husk)
    (expect (portable-bookmarks-discovery-spec--husk-only-p husk) :to-be-truthy)
    (expect (workspace-bookmark--canonical-checkout main) :to-be nil)
    (portable-bookmarks-discovery-spec--hermetic
      (let ((jf/emacs-dir main))
        (expect (workspace-bookmark--canonical-for-identity
                 portable-bookmarks-discovery-spec--identity)
                :to-be nil))))

  (it "is nil for a husk that holds an untracked file besides .git"
    ;; Finder, Spotlight and sync clients drop files like .DS_Store into
    ;; any directory they visit; "something besides .git" is not enough.
    (rename-file (expand-file-name ".git" main) (expand-file-name ".git" husk))
    (make-symbolic-link (expand-file-name ".git" husk)
                        (expand-file-name ".git" main))
    (with-temp-file (expand-file-name ".DS_Store" husk) (insert "x"))
    (expect (workspace-bookmark--canonical-checkout main) :to-be nil))

  (it "still returns an ordinary main worktree that holds its tracked files"
    (expect (workspace-bookmark--holds-tracked-file-p main) :to-be-truthy)
    (expect (workspace-bookmark--canonical-checkout main) :to-equal main)))


;;; Behavioral — the safety rule against a full clone inside a workspace

(describe "Canonical safety with a full clone inside a workspace home"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "never offers another workspace's clone as canonical"
    (let* ((origin (workspace-bookmark-fixtures-make-repo))
           (home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-clone"))
           (clone (file-name-as-directory (expand-file-name "repo" home))))
      (workspace-bookmark-fixtures-git home "clone" "--quiet" origin clone)
      (workspace-bookmark-fixtures-git clone "remote" "set-url" "origin"
                                       portable-bookmarks-discovery-spec--remote)
      ;; Git itself calls the clone a main worktree ...
      (expect (workspace-bookmark--canonical-checkout clone) :to-equal clone)
      (expect (file-directory-p (expand-file-name ".git" clone)) :to-be-truthy)
      ;; ... but it belongs to a workspace, so it is not canonical ...
      (expect (workspace-bookmark--classify clone) :to-be 'workspace-owned)
      ;; ... and the only in-view checkout of the identity yields nothing.
      (portable-bookmarks-discovery-spec--hermetic
        (expect (workspace-bookmark--canonical-for-identity
                 portable-bookmarks-discovery-spec--identity)
                :to-be nil))))

  (it "never offers it when the home is registered with its letter case flipped"
    ;; macOS disks are case-insensitive by default, and
    ;; `read-directory-name' accepts a typed spelling whose case differs
    ;; from the disk's; `workspace--make' keeps it.  The in-view child then
    ;; carries the home's case while git reports the on-disk case, and a
    ;; string-based containment test misses the clone.
    (let* ((origin (workspace-bookmark-fixtures-make-repo))
           (real-home (workspace-bookmark-fixtures-temp-dir "wsbm-case-"))
           (flipped (upcase real-home))
           (clone (file-name-as-directory (expand-file-name "repo" real-home))))
      (assume (and (not (equal flipped real-home))
                   (file-directory-p flipped)
                   (file-equal-p flipped real-home))
              "temp filesystem is case-sensitive; the case-flip layout cannot exist here")
      (workspace-bookmark-fixtures-register-workspace "wsbm-disc-case" flipped)
      (workspace-bookmark-fixtures-git real-home "clone" "--quiet" origin clone)
      (workspace-bookmark-fixtures-git clone "remote" "set-url" "origin"
                                       portable-bookmarks-discovery-spec--remote)
      ;; The in-view spelling carries the home's flipped case; git's
      ;; spelling of the same clone carries the on-disk case.
      (expect (workspace-bookmark--home-checkouts flipped)
              :to-equal (list (expand-file-name "repo/" flipped)))
      (expect (workspace-bookmark--canonical-checkout clone) :to-equal clone)
      (expect (string-prefix-p flipped clone) :to-be nil)
      ;; Both spellings classify workspace-owned ...
      (expect (workspace-bookmark--classify clone) :to-be 'workspace-owned)
      (expect (workspace-bookmark--classify (expand-file-name "repo/" flipped))
              :to-be 'workspace-owned)
      ;; ... so the safety rule refuses the clone.
      (portable-bookmarks-discovery-spec--hermetic
        (expect (workspace-bookmark--canonical-for-identity
                 portable-bookmarks-discovery-spec--identity)
                :to-be nil)))))


;;; Behavioral — a workspace anchored at an existing repository
;;
;; User decision 2026-09-24 (ask 7a).  An anchored workspace has the
;; repository AS its :home, so the home itself is one of its checkouts.

(describe "A workspace anchored at an existing repository"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "resolves its identity to the home, and never offers the home as canonical"
    (assume (fboundp 'workspace-new) "tabs.el is not loaded")
    (let* ((repo (workspace-bookmark-fixtures-make-repo
                  :remote portable-bookmarks-discovery-spec--remote))
           (ws nil))
      ;; Case 1 of `workspace--new-anchor-existing': a repository that
      ;; already has a home.org is registered as it stands.
      (with-temp-file (expand-file-name "home.org" repo) (insert "* home\n"))
      (spy-on 'tab-bar-new-tab)
      (spy-on 'tab-bar-rename-tab)
      (spy-on 'workspace--dispatch-create-integrations)
      (let ((workspace-home-builder nil))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) repo)))
          (setq ws (workspace-new "ignored" t))))
      (push (workspace--name ws) workspace-bookmark-fixtures--registered-names)
      (let ((name (workspace--name ws)))
        (expect (workspace--home ws) :to-equal repo)
        (expect (workspace-bookmark--home-checkouts repo) :to-equal (list repo))
        (expect (workspace-bookmark--workspace-checkouts
                 portable-bookmarks-discovery-spec--identity name)
                :to-equal (list repo))
        (expect (workspace-bookmark--workspace-checkout
                 portable-bookmarks-discovery-spec--identity name)
                :to-equal repo)
        ;; Safety: the home is the repository's main worktree, and it is
        ;; the anchored workspace's — so it is offered to no one else.
        (expect (workspace-bookmark--canonical-checkout repo) :to-equal repo)
        (expect (workspace-bookmark--classify repo) :to-be 'workspace-owned)
        (let ((home-b (workspace-bookmark-fixtures-make-workspace
                       "wsbm-disc-anchor-b")))
          (workspace-bookmark-fixtures-add-worktree
           repo (expand-file-name "wt" home-b) :branch "b")
          (portable-bookmarks-discovery-spec--hermetic
            (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                       (lambda () "wsbm-disc-anchor-b")))
              (expect (workspace-bookmark--canonical-for-identity
                       portable-bookmarks-discovery-spec--identity)
                      :to-be nil))))))))


;;; Behavioral — several checkouts of one repository in one home
;;
;; User decision 2026-09-24 (ask 7b).  The alphabetically-first sibling
;; used to win silently.

(describe "Several checkouts of one identity in one workspace home"
  :var (repo home)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-discovery-spec--remote)
          home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-sib")))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "lists every sibling worktree, and names none as THE checkout"
    (let ((zeta (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "project-zeta" home) :branch "zeta"))
          (alpha (workspace-bookmark-fixtures-add-worktree
                  repo (expand-file-name "project-alpha" home) :branch "alpha")))
      (expect (workspace-bookmark--workspace-checkouts
               portable-bookmarks-discovery-spec--identity "wsbm-disc-sib")
              :to-equal (list alpha zeta))
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity "wsbm-disc-sib")
              :to-be nil)))

  (it "treats a home that is a checkout and also holds a worktree of it as ambiguous"
    (let* ((anchored (workspace-bookmark-fixtures-make-workspace
                      "wsbm-disc-sib-anchored" repo))
           (child (workspace-bookmark-fixtures-add-worktree
                   repo (expand-file-name "child" anchored) :branch "child")))
      (expect (workspace-bookmark--workspace-checkouts
               portable-bookmarks-discovery-spec--identity
               "wsbm-disc-sib-anchored")
              :to-equal (list repo child))
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity
               "wsbm-disc-sib-anchored")
              :to-be nil)))

  (it "is nil for both when the home holds no checkout of the identity"
    (expect (workspace-bookmark--workspace-checkouts
             portable-bookmarks-discovery-spec--identity "wsbm-disc-sib")
            :to-be nil)
    (expect (workspace-bookmark--workspace-checkout
             portable-bookmarks-discovery-spec--identity "wsbm-disc-sib")
            :to-be nil)))


;;; Behavioral — ownership while the home is being written to
;;
;; Review finding B1 (wave 3).  `file-equal-p' compares whole attribute
;; lists, timestamps included, so a home modified between the two reads of
;; one comparison was "not the home", and ownership-first classification
;; then called the workspace's full clone `canonical'.

(defun portable-bookmarks-discovery-spec--clone-into (dir)
  "Clone a fresh fixture repository into DIR and give it the fixture remote.
Returns DIR as a directory name."
  (let ((origin (workspace-bookmark-fixtures-make-repo))
        (dir (file-name-as-directory dir)))
    (workspace-bookmark-fixtures-git (file-name-directory (directory-file-name dir))
                                     "clone" "--quiet" origin
                                     (directory-file-name dir))
    (workspace-bookmark-fixtures-git dir "remote" "set-url" "origin"
                                     portable-bookmarks-discovery-spec--remote)
    dir))

(describe "Canonical safety while the workspace home is being written to"
  :var (home clone)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    (setq home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-race")
          clone (portable-bookmarks-discovery-spec--clone-into
                 (expand-file-name "clone" home))))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "still owns a full clone when every read of a file sees new timestamps"
    ;; Deterministic model of a concurrent writer (or reader: on APFS the
    ;; first read after a change updates atime): every call to
    ;; `file-attributes' reports access, modification and status-change
    ;; times one second later than the call before.  Identity — device and
    ;; inode — never changes, because the files do not.
    (let ((real-file-attributes (symbol-function 'file-attributes))
          (reads 0))
      (cl-letf (((symbol-function 'file-attributes)
                 (lambda (file &optional id-format)
                   (let ((attrs (funcall real-file-attributes file id-format)))
                     (when attrs
                       (setq reads (1+ reads)
                             attrs (copy-sequence attrs))
                       (dolist (slot '(4 5 6))
                         (setf (nth slot attrs)
                               (time-add (nth slot attrs) reads))))
                     attrs))))
        (expect (workspace-bookmark--classify clone) :to-be 'workspace-owned)
        (portable-bookmarks-discovery-spec--hermetic
          (expect (workspace-bookmark--canonical-for-identity
                   portable-bookmarks-discovery-spec--identity)
                  :to-be nil)))
      (expect reads :to-be-greater-than 1)))

  (it "never classifies the clone canonical under a real concurrent writer (bounded)"
    ;; A stress companion to the spec above.  With the fix it cannot fail:
    ;; ownership no longer reads timestamps at all.  Without the fix the
    ;; reviewer measured about one wrong answer in four.
    (let ((writer nil)
          (wrong 0)
          (calls 0)
          (deadline (time-add nil 3)))
      (unwind-protect
          (progn
            (setq writer
                  (make-process
                   :name "wsbm-disc-writer" :buffer nil :noquery t
                   :command
                   (list "sh" "-c"
                         "while :; do : > \"$1/.#wsbm-lock\"; rm -f \"$1/.#wsbm-lock\"; done"
                         "sh" (directory-file-name home))))
            (while (and (< calls 1000) (time-less-p nil deadline))
              (setq calls (1+ calls))
              (unless (eq 'workspace-owned (workspace-bookmark--classify clone))
                (setq wrong (1+ wrong)))))
        (when (process-live-p writer)
          (delete-process writer))
        (with-timeout (5)
          (while (process-live-p writer)
            (accept-process-output nil 0.05))))
      (expect calls :to-be-greater-than 100)
      (expect wrong :to-equal 0))))


;;; Behavioral — symbolic links in a workspace home are ignored
;;
;; User decision 2026-09-24 (discovery-refinements, part D).  Only real
;; directories count: a symlinked child of a home is not one of the
;; workspace's checkouts, and no link makes its target workspace-owned.
;; This replaces the symlink ownership added for review finding B2
;; (wave 3).  The accepted consequence — a main clone linked into a home
;; can be offered to another workspace — is documented in the org as a
;; known limitation, and pinned here so a change to it is deliberate.

(defun portable-bookmarks-discovery-spec--expect-offered-main-only
    (main home-a home-b)
  "Expect MAIN, and only MAIN, to be offered to workspace B.
With workspace B current, the answer is MAIN, the repository's main
worktree: no workspace owns it, and it is none of the checkouts of
HOME-A or HOME-B (B's own worktree included)."
  (portable-bookmarks-discovery-spec--hermetic
    (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
               (lambda () "wsbm-disc-link-b")))
      (let ((answer (workspace-bookmark--canonical-for-identity
                     portable-bookmarks-discovery-spec--identity)))
        (expect answer :to-equal main)
        (expect (workspace-bookmark--owning-workspace answer) :to-be nil)
        (dolist (home (list home-a home-b))
          (dolist (held (workspace-bookmark--home-checkouts home))
            (expect (workspace-bookmark--same-file-p held answer)
                    :to-be nil)))))))

(describe "Symbolic links in a workspace home are ignored"
  :var (outer main home-a home-b)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    ;; MAIN is a full clone outside every home, inside a directory of its
    ;; own; workspace B holds a linked worktree of it, so the identity is
    ;; in view whatever workspace A links to.
    (setq outer (workspace-bookmark-fixtures-temp-dir "wsbm-outer-")
          main (portable-bookmarks-discovery-spec--clone-into
                (expand-file-name "main" outer))
          home-a (workspace-bookmark-fixtures-make-workspace "wsbm-disc-link-a")
          home-b (workspace-bookmark-fixtures-make-workspace "wsbm-disc-link-b"))
    (make-directory (expand-file-name "sub" main))
    (workspace-bookmark-fixtures-add-worktree
     main (expand-file-name "discovery" home-b) :branch "b")
    ;; Control: before any link, MAIN is the canonical checkout.
    (portable-bookmarks-discovery-spec--hermetic
      (expect (workspace-bookmark--canonical-for-identity
               portable-bookmarks-discovery-spec--identity)
              :to-equal main)))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "does not list a symlinked child among the workspace's checkouts"
    (make-symbolic-link (directory-file-name main)
                        (expand-file-name "x" home-a))
    (expect (workspace-bookmark--home-checkouts home-a) :to-be nil)
    (expect (workspace-bookmark--workspace-checkouts
             portable-bookmarks-discovery-spec--identity "wsbm-disc-link-a")
            :to-be nil)
    (expect (workspace-bookmark--workspace-checkout
             portable-bookmarks-discovery-spec--identity "wsbm-disc-link-a")
            :to-be nil))

  (it "does not own the clone a home child links to (known limitation)"
    ;; The accepted consequence: A edits MAIN through the link, yet MAIN
    ;; is offered to B.  Guidance: add a worktree, do not link a clone in.
    (let ((link (expand-file-name "x" home-a)))
      (make-symbolic-link (directory-file-name main) link)
      (expect (workspace-bookmark--classify main) :to-be 'canonical)
      (expect (workspace-bookmark--classify (file-name-as-directory link))
              :to-be 'canonical)
      (portable-bookmarks-discovery-spec--expect-offered-main-only
       main home-a home-b)))

  (it "does not own the clone when a home child links to a directory inside it"
    (make-symbolic-link (expand-file-name "sub" main)
                        (expand-file-name "sub-link" home-a))
    (expect (workspace-bookmark--classify main) :to-be 'canonical)
    (portable-bookmarks-discovery-spec--expect-offered-main-only
     main home-a home-b))

  (it "does not own the clone when a home child links to one of its files"
    (let ((link (expand-file-name "README-link" home-a)))
      (make-symbolic-link (expand-file-name "README.md" main) link)
      (expect (file-regular-p link) :to-be-truthy)
      (expect (workspace-bookmark--classify main) :to-be 'canonical)
      (portable-bookmarks-discovery-spec--expect-offered-main-only
       main home-a home-b)))

  (it "does not own a checkout below a directory a home child links to"
    (make-symbolic-link (directory-file-name outer)
                        (expand-file-name "src" home-a))
    (expect (workspace-bookmark--classify main) :to-be 'canonical)
    (portable-bookmarks-discovery-spec--expect-offered-main-only
     main home-a home-b))

  (it "does not own the clone because of a dangling link or a link elsewhere"
    (make-symbolic-link (expand-file-name "gone" outer)
                        (expand-file-name "dangling" home-a))
    (make-symbolic-link (directory-file-name
                         (workspace-bookmark-fixtures-temp-dir "wsbm-elsewhere-"))
                        (expand-file-name "elsewhere" home-a))
    (expect (workspace-bookmark--classify main) :to-be 'canonical))

  (it "still owns a real clone child of the home, next to any links"
    ;; Links are ignored in both directions: they neither add nor remove
    ;; ownership.  A real child clone of the same identity stays A's.
    (let ((clone (portable-bookmarks-discovery-spec--clone-into
                  (expand-file-name "real" home-a))))
      (make-symbolic-link (directory-file-name main)
                          (expand-file-name "x" home-a))
      (expect (workspace-bookmark--home-checkouts home-a)
              :to-equal (list clone))
      (expect (workspace-bookmark--classify clone) :to-be 'workspace-owned)
      (portable-bookmarks-discovery-spec--hermetic
        (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                   (lambda () "wsbm-disc-link-b")))
          (expect (workspace-bookmark--canonical-for-identity
                   portable-bookmarks-discovery-spec--identity)
                  :not :to-equal clone))))))


;;; Behavioral — the runner answers about the checkout it is given
;;
;; On-touch audit findings F2 and F3 (wave 3).

(describe "Checkout discovery with repository-locating variables in the environment"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "finds the workspace's checkout of the identity, not of GIT_DIR's repository"
    (let* ((other (workspace-bookmark-fixtures-make-repo
                   :remote "git@example.com:fixtures/other.git"))
           (repo (workspace-bookmark-fixtures-make-repo
                  :remote portable-bookmarks-discovery-spec--remote))
           (home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-env"))
           (child (workspace-bookmark-fixtures-add-worktree
                   repo (expand-file-name "zz" home) :branch "zz")))
      (let ((process-environment
             (append (list (concat "GIT_DIR=" (expand-file-name ".git" other))
                           (concat "GIT_WORK_TREE=" (directory-file-name other)))
                     process-environment)))
        (expect (workspace-bookmark--identity child)
                :to-equal portable-bookmarks-discovery-spec--identity)
        (expect (workspace-bookmark--workspace-checkout
                 "example.com/fixtures/other" "wsbm-disc-env")
                :to-be nil)
        (expect (workspace-bookmark--workspace-checkout
                 portable-bookmarks-discovery-spec--identity "wsbm-disc-env")
                :to-equal child)
        (expect (workspace-bookmark--checkouts-of child) :to-contain child)
        (expect (workspace-bookmark--canonical-checkout child) :to-equal repo)))))

(describe "A checkout whose directory name ends in a space"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "has its own identity, not that of the repository enclosing it"
    ;; The home is itself a repository (scaffolded homes are), with a
    ;; different remote.  Trimming "proj " to "proj", then running git in
    ;; the parent of that missing path, answered with the home's identity.
    (let* ((home (workspace-bookmark-fixtures-make-repo
                  :remote "git@example.com:fixtures/home-repo.git"
                  :prefix "wsbm-home-repo-"))
           (proj (portable-bookmarks-discovery-spec--clone-into
                  (expand-file-name "proj " home))))
      (workspace-bookmark-fixtures-register-workspace "wsbm-disc-space" home)
      (expect (workspace-bookmark--repo-root proj) :to-equal proj)
      (expect (workspace-bookmark--identity proj)
              :to-equal portable-bookmarks-discovery-spec--identity)
      ;; The home is a checkout of its own identity (anchored homes), and
      ;; only of that one: PROJ does not answer for it.
      (expect (workspace-bookmark--workspace-checkouts
               "example.com/fixtures/home-repo" "wsbm-disc-space")
              :to-equal (list home))
      (expect (workspace-bookmark--workspace-checkout
               portable-bookmarks-discovery-spec--identity "wsbm-disc-space")
              :to-equal proj))))


;;; Behavioral — containment across real path spellings

(describe "Workspace containment across path spellings"
  :var (raw real child)

  (before-each
    (portable-bookmarks-discovery-spec--setup)
    ;; `make-temp-file' hands back /var/... on macOS; its truename is
    ;; /private/var/....  Register the home under the RAW spelling, as a
    ;; user-typed path would be, and name the child by the truename, as git
    ;; reports it.
    (setq raw (file-name-as-directory (make-temp-file "wsbm-spell-" t))
          real (file-name-as-directory (file-truename raw))
          child (expand-file-name "child/" real))
    (workspace-bookmark-fixtures-track-dir real)
    (make-directory (expand-file-name ".git" child) t)
    (workspace-bookmark-fixtures-register-workspace "wsbm-spell" raw))

  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "matches a truenamed checkout against a raw-spelled home"
    (expect (workspace-bookmark--classify child) :to-be 'workspace-owned))

  (it "matches with and without a trailing slash"
    (expect (workspace-bookmark--classify (directory-file-name child))
            :to-be 'workspace-owned))

  (it "matches a ~ spelling of the checkout"
    (let* ((process-environment
            (cons (concat "HOME=" (directory-file-name
                                   (file-name-directory
                                    (directory-file-name real))))
                  process-environment))
           (tilde (concat "~/" (file-name-nondirectory
                                (directory-file-name real))
                          "/child")))
      (expect (file-equal-p tilde child) :to-be-truthy)
      (expect (workspace-bookmark--classify tilde) :to-be 'workspace-owned)))

  (it "does not treat a sibling sharing a name prefix as contained"
    (let ((sibling (file-name-as-directory
                    (concat (directory-file-name real) "-old"))))
      (workspace-bookmark-fixtures-track-dir sibling)
      (make-directory (expand-file-name ".git" sibling) t)
      (expect (workspace-bookmark--classify sibling) :to-be 'canonical))))


;;; Unit — failure paths and ordering

(describe "Checkout discovery failure paths"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "workspace-bookmark--checkouts-of is nil when git fails"
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (&rest _) nil)))
      (expect (workspace-bookmark--checkouts-of "/tmp/") :to-be nil)))

  (it "workspace-bookmark--canonical-checkout is nil when git fails"
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (&rest _) nil)))
      (expect (workspace-bookmark--canonical-checkout "/tmp/") :to-be nil)))

  (it "workspace-bookmark--canonical-checkout refuses a relative common dir"
    ;; An old git ignoring --path-format would answer ".git"; re-expanding
    ;; that against the wrong directory is the trap.
    (cl-letf (((symbol-function 'workspace-bookmark--git)
               (lambda (_dir &rest args)
                 (if (member "--git-common-dir" args)
                     ".git"
                   "worktree /tmp\nHEAD 0\nbranch refs/heads/main"))))
      (expect (workspace-bookmark--canonical-checkout "/tmp/") :to-be nil)))

  (it "workspace-bookmark--canonical-for-identity is nil for a nil identity"
    (expect (workspace-bookmark--canonical-for-identity nil) :to-be nil))

  (it "workspace-bookmark--canonical-for-identity never returns a checkout that is not canonical"
    (let ((home (workspace-bookmark-fixtures-make-workspace "wsbm-disc-unit")))
      (make-directory (expand-file-name "a/.git" home) t)
      (portable-bookmarks-discovery-spec--hermetic
        (dolist (kind '(workspace-owned other))
          (cl-letf (((symbol-function 'workspace-bookmark--identity)
                     (lambda (_) "example.com/x/y"))
                    ((symbol-function 'workspace-bookmark--canonical-checkout)
                     (lambda (_) "/somewhere/main/"))
                    ((symbol-function 'workspace-bookmark--classify)
                     (lambda (_) kind)))
            (expect (cons kind (workspace-bookmark--canonical-for-identity
                                "example.com/x/y"))
                    :to-equal (cons kind nil)))))))

  (it "workspace-bookmark--classify reports workspace-owned across ~ and trailing-slash spellings of the home"
    ;; The registry canonicalises :home through `workspace--make', so a ~
    ;; or slash-less home can only arrive from a hand-built or stale
    ;; plist.  Stand in for that with the home accessor; the directories
    ;; are real, because containment compares file identity on disk.
    (let* ((fake-home (workspace-bookmark-fixtures-temp-dir "wsbm-home-"))
           (process-environment
            (cons (concat "HOME=" (directory-file-name fake-home))
                  process-environment))
           (ws (expand-file-name "ws/" fake-home)))
      (make-directory (expand-file-name "repo/.git" ws) t)
      (dolist (home (list "~/ws" "~/ws/" (directory-file-name ws) ws))
        (cl-letf (((symbol-function 'workspace-bookmark--registered-names)
                   (lambda () '("ws")))
                  ((symbol-function 'workspace-bookmark--workspace-home)
                   (lambda (_) home)))
          (dolist (checkout (list (expand-file-name "repo" ws)
                                  (expand-file-name "repo/" ws)
                                  "~/ws/repo"))
            (expect (list home checkout
                          (workspace-bookmark--classify checkout))
                    :to-equal (list home checkout 'workspace-owned)))))))

  (it "workspace-bookmark--home-checkouts is nil for a missing or remote home"
    (expect (workspace-bookmark--home-checkouts nil) :to-be nil)
    (expect (workspace-bookmark--home-checkouts "/nonexistent/wsbm/home/")
            :to-be nil)
    (expect (workspace-bookmark--home-checkouts "/ssh:nonexistent.invalid:/h/")
            :to-be nil)))

(describe "workspace-bookmark--classify on a remote path"

  ;; Deliberately outside any fixture scope: the spies below stay active
  ;; through `after-each', and a spied `file-directory-p' would make the
  ;; fixture teardown skip deleting its temp directories.

  (it "workspace-bookmark--classify is `other' for a remote path, without touching it"
    ;; The first `file-remote-p' on a TRAMP name autoloads TRAMP, whose
    ;; own loading probes directories; do that before the spies go in.
    (file-remote-p "/ssh:nonexistent.invalid:/srv/repo/")
    (spy-on 'file-in-directory-p)
    (spy-on 'file-directory-p)
    (expect (workspace-bookmark--classify "/ssh:nonexistent.invalid:/srv/repo/")
            :to-be 'other)
    (expect (workspace-bookmark--classify nil) :to-be 'other)
    (expect 'file-in-directory-p :not :to-have-been-called)
    (expect 'file-directory-p :not :to-have-been-called)))

(describe "workspace-bookmark--parse-worktree-porcelain"

  (it "reads paths and flags in git's order"
    (expect (workspace-bookmark--parse-worktree-porcelain
             (concat "worktree /r/main\nHEAD 1\nbranch refs/heads/main\n\n"
                     "worktree /r/with space\nHEAD 1\ndetached\n\n"
                     "worktree /r/locked\nHEAD 1\nbranch b\nlocked Production\n\n"
                     "worktree /r/gone\nHEAD 1\nbranch c\nprunable gitdir file points to non-existent location\n\n"
                     "worktree /r/bare.git\nbare"))
            :to-equal
            '((:path "/r/main" :bare nil :detached nil :locked nil :prunable nil)
              (:path "/r/with space" :bare nil :detached t :locked nil :prunable nil)
              (:path "/r/locked" :bare nil :detached nil :locked t :prunable nil)
              (:path "/r/gone" :bare nil :detached nil :locked nil :prunable t)
              (:path "/r/bare.git" :bare t :detached nil :locked nil :prunable nil))))

  (it "reads bare `locked' and `prunable' lines with no reason"
    (expect (workspace-bookmark--parse-worktree-porcelain
             "worktree /r/x\nHEAD 1\nlocked\nprunable")
            :to-equal
            '((:path "/r/x" :bare nil :detached nil :locked t :prunable t))))

  (it "is nil for nil input"
    (expect (workspace-bookmark--parse-worktree-porcelain nil) :to-be nil)))

(describe "workspace-bookmark--checkouts-in-view"

  (before-each (portable-bookmarks-discovery-spec--setup))
  (after-each (portable-bookmarks-discovery-spec--teardown))

  (it "orders the current workspace, then the others, then jf/emacs-dir"
    (let* ((other-home (workspace-bookmark-fixtures-make-workspace "wsbm-view-b"))
           (current-home (workspace-bookmark-fixtures-make-workspace "wsbm-view-a"))
           (config (workspace-bookmark-fixtures-temp-dir "wsbm-config-")))
      (dolist (dir (list (expand-file-name "b1/.git" other-home)
                         (expand-file-name "a1/.git" current-home)
                         (expand-file-name "a2/.git" current-home)))
        (make-directory dir t))
      ;; A child without .git is not a checkout.
      (make-directory (expand-file-name "notes" current-home))
      (let ((jf/emacs-dir config))
        (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                   (lambda () "wsbm-view-a")))
          (expect (workspace-bookmark--checkouts-in-view)
                  :to-equal (list (expand-file-name "a1/" current-home)
                                  (expand-file-name "a2/" current-home)
                                  (expand-file-name "b1/" other-home)
                                  config))))))

  (it "is empty with no registry and no configuration checkout"
    (portable-bookmarks-discovery-spec--hermetic
      (expect (workspace-bookmark--checkouts-in-view) :to-be nil))))

;;; portable-bookmarks-discovery-spec.el ends here
