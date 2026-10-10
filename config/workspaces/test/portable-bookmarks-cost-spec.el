;;; portable-bookmarks-cost-spec.el --- Discovery cost contract specs -*- lexical-binding: t; -*-

;; The subprocess budget of checkout discovery on a warm identity memo,
;; and the invalidation rule of the memo now that discovery shares it
;; (`register/boundary/git-derived-checkout-discovery', cost_contract and
;; validator 6; end-of-cycle finding 6 of cycle-1789935943).
;;
;; BEHAVIORAL: real `git' against throwaway repositories from
;; `portable-bookmarks-fixtures.el', ONE fixture for the whole budget
;; block so the counts below are comparable with each other.  Every git
;; subprocess this module runs goes through `workspace-bookmark--git', so a
;; call-through spy on it IS the spawn count — the pattern of
;; portable-bookmarks-stamp-spec.el § "stamp seam subprocess budget".
;;
;; Each budget is pinned EXACTLY, and each has a companion that bypasses
;; the memo it depends on and shows the count rise: a budget that holds
;; with the memo bypassed would not be measuring the memo.  The identity
;; memo is bypassed by making `workspace-bookmark--memoised-identity'
;; compute every time; the per-call discovery memo by making
;; `workspace-bookmark--memoised-in-call' apply every time.
;;
;; Every spec runs against a FRESH `workspace--registry'.  `jf/emacs-dir'
;; is bound per spec — to the fixture's canonical clone where a canonical
;; checkout must be in view, to nil otherwise — so the configuration
;; checkout running the suite is never in view.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'bookmark)

(defconst portable-bookmarks-cost-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-cost-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-cost-spec--this-dir))
;; The layout-capture block drives core's capture walk.  Under the harness
;; init.el has loaded the workspaces package; load the modules that walk
;; needs by relative path only when it has not.
(unless (fboundp 'workspace--capture-frameset)
  (let ((dir portable-bookmarks-cost-spec--this-dir))
    (load (expand-file-name "../tabs.el" dir))
    (unless (featurep 'bufferlo)
      (defalias 'bufferlo-mode (lambda (&optional _) nil))
      (provide 'bufferlo))
    (load (expand-file-name "../buffer-membership.el" dir))
    (load (expand-file-name "../layouts.el" dir))))

(defvar jf/emacs-dir)

(defconst portable-bookmarks-cost-spec--remote
  "git@example.com:fixtures/cost.git"
  "Remote URL of the repository whose bookmarks are resolved.")

(defconst portable-bookmarks-cost-spec--identity
  "example.com/fixtures/cost"
  "The identity `portable-bookmarks-cost-spec--remote' normalizes to.")

(defconst portable-bookmarks-cost-spec--other-remote
  "git@example.com:fixtures/cost-other.git"
  "Remote URL of the unrelated repository that shares the workspace homes.")


;;; Helpers

(defun portable-bookmarks-cost-spec--record (&optional relpath)
  "Return a record stamped with the fixture identity and RELPATH."
  (list "cost-spec-bookmark"
        (cons 'filename "/nonexistent/stored/elsewhere/README.md")
        (cons workspace-bookmark-repo-property portable-bookmarks-cost-spec--identity)
        (cons workspace-bookmark-relpath-property (or relpath "README.md"))
        (cons workspace-bookmark-format-property workspace-bookmark-format-version)))

(defun portable-bookmarks-cost-spec--resolve (workspace canonical)
  "Resolve the fixture record in WORKSPACE, which is also current.
CANONICAL, when non-nil, is bound as `jf/emacs-dir' so that it is in view."
  (let ((jf/emacs-dir canonical))
    (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
               (lambda () workspace)))
      (workspace-bookmark--resolve (portable-bookmarks-cost-spec--record)
                                   workspace))))

(defmacro portable-bookmarks-cost-spec--counting (&rest body)
  "Run BODY and return (RESULT . SPAWNS), SPAWNS the git subprocesses it ran."
  (declare (indent 0))
  `(progn
     (spy-on 'workspace-bookmark--git :and-call-through)
     (let ((result (progn ,@body)))
       (cons result (spy-calls-count 'workspace-bookmark--git)))))

(defmacro portable-bookmarks-cost-spec--identity-memo-bypassed (&rest body)
  "Run BODY with every identity computed afresh, the memo neither read nor written."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'workspace-bookmark--memoised-identity)
              (lambda (root _git-common-dir)
                (workspace-bookmark--root-identity root))))
     ,@body))

(defmacro portable-bookmarks-cost-spec--call-memo-bypassed (&rest body)
  "Run BODY with every in-call question asked afresh."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'workspace-bookmark--memoised-in-call)
              (lambda (_key function &rest args) (apply function args))))
     ,@body))

(defun portable-bookmarks-cost-spec--warm (workspace canonical)
  "Resolve once in WORKSPACE so every identity the resolution needs is memoised."
  (workspace-bookmark-clear-identity-cache)
  (portable-bookmarks-cost-spec--resolve workspace canonical))


;;; Behavioral — the budget

(describe "Discovery subprocess budget on a warm identity memo"
  ;; REPO is the canonical clone, in view as `jf/emacs-dir'.  Workspace A's
  ;; home holds a linked worktree of REPO and one of OTHER; workspace B's
  ;; home holds one of OTHER only.  So resolving in A is `resolved' with
  ;; two candidates to examine, and resolving in B is `missing-repo' with
  ;; four checkouts in view (B's one, A's two, REPO) of which REPO's
  ;; repository is the one that matches.
  :var (repo other home-a home-b)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-cost-spec--remote :prefix "wsbm-cost-repo-")
          other (workspace-bookmark-fixtures-make-repo
                 :remote portable-bookmarks-cost-spec--other-remote
                 :prefix "wsbm-cost-other-")
          home-a (workspace-bookmark-fixtures-make-workspace "wsbm-cost-a")
          home-b (workspace-bookmark-fixtures-make-workspace "wsbm-cost-b"))
    (workspace-bookmark-fixtures-add-worktree
     repo (expand-file-name "proj" home-a) :branch "a")
    (workspace-bookmark-fixtures-add-worktree
     other (expand-file-name "other" home-a) :branch "a-other")
    (workspace-bookmark-fixtures-add-worktree
     other (expand-file-name "other" home-b) :branch "b-other"))

  (after-each
    (workspace-bookmark-clear-identity-cache))

  (describe "resolved"

    (it "costs one subprocess per checkout of the workspace's home: two"
      (portable-bookmarks-cost-spec--warm "wsbm-cost-a" repo)
      (let ((run (portable-bookmarks-cost-spec--counting
                   (portable-bookmarks-cost-spec--resolve "wsbm-cost-a" repo))))
        (expect (plist-get (car run) :outcome) :to-be 'resolved)
        (expect (plist-get (car run) :path)
                :to-equal (expand-file-name "proj/README.md" home-a))
        (expect (cdr run) :to-equal 2)))

    (it "would cost four with the identity memo bypassed"
      (portable-bookmarks-cost-spec--warm "wsbm-cost-a" repo)
      (let ((run (portable-bookmarks-cost-spec--counting
                   (portable-bookmarks-cost-spec--identity-memo-bypassed
                     (portable-bookmarks-cost-spec--resolve "wsbm-cost-a" repo)))))
        (expect (plist-get (car run) :outcome) :to-be 'resolved)
        (expect (cdr run) :to-equal 4))))

  (describe "missing-repo"
    ;; 4 for the checkouts in view (B's own checkout among them, positioned
    ;; once: the whole resolve decision is one discovery call), then for
    ;; the one matching repository its worktree listing and the
    ;; tracked-file check of its main worktree: the position that answers
    ;; the top-level check is already held.  16 at 20944160 on this
    ;; fixture; 7 while the decision ran its two plural calls in separate
    ;; memo scopes.

    (it "costs the checkouts in view plus two: six"
      (portable-bookmarks-cost-spec--warm "wsbm-cost-b" repo)
      (let ((run (portable-bookmarks-cost-spec--counting
                   (portable-bookmarks-cost-spec--resolve "wsbm-cost-b" repo))))
        (expect (plist-get (car run) :outcome) :to-be 'missing-repo)
        (expect (plist-get (car run) :path)
                :to-equal (expand-file-name "README.md" repo))
        (expect (cdr run) :to-equal 6)))

    (it "would cost eleven with the identity memo bypassed"
      (portable-bookmarks-cost-spec--warm "wsbm-cost-b" repo)
      (let ((run (portable-bookmarks-cost-spec--counting
                   (portable-bookmarks-cost-spec--identity-memo-bypassed
                     (portable-bookmarks-cost-spec--resolve "wsbm-cost-b" repo)))))
        (expect (plist-get (car run) :outcome) :to-be 'missing-repo)
        (expect (cdr run) :to-equal 11)))

    (it "would cost twelve with the per-call memo bypassed"
      (portable-bookmarks-cost-spec--warm "wsbm-cost-b" repo)
      (let ((run (portable-bookmarks-cost-spec--counting
                   (portable-bookmarks-cost-spec--call-memo-bypassed
                     (portable-bookmarks-cost-spec--resolve "wsbm-cost-b" repo)))))
        (expect (plist-get (car run) :outcome) :to-be 'missing-repo)
        (expect (cdr run) :to-equal 12))))

  (describe "the canonical test of one main clone"

    (it "costs three subprocesses: position, worktree listing, tracked-file check"
      (let ((run (portable-bookmarks-cost-spec--counting
                   (let ((jf/emacs-dir nil))
                     (workspace-bookmark-canonical-checkouts-in-view (list repo))))))
        (expect (car run) :to-equal (list repo))
        (expect (cdr run) :to-equal 3)
        (expect (mapcar (lambda (call) (cadr (spy-context-args call)))
                        (spy-calls-all 'workspace-bookmark--git))
                :to-equal '("rev-parse" "worktree" "ls-files"))))

    (it "costs one more for each further checkout of the same repository"
      (let* ((plain (workspace-bookmark-fixtures-temp-dir "wsbm-cost-plain-"))
             (w1 (workspace-bookmark-fixtures-add-worktree
                  repo (expand-file-name "w1" plain) :branch "w1"))
             (w2 (workspace-bookmark-fixtures-add-worktree
                  repo (expand-file-name "w2" plain) :branch "w2"))
             (run (portable-bookmarks-cost-spec--counting
                    (let ((jf/emacs-dir nil))
                      (workspace-bookmark-canonical-checkouts-in-view
                       (list w1 repo w2))))))
        (expect (car run) :to-equal (list repo))
        (expect (cdr run) :to-equal 5)))))


;;; Behavioral — the invalidation rule, now that discovery shares the memo

(describe "Identity memo invalidation seen through discovery"
  :var (repo home linked)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-cost-spec--remote :prefix "wsbm-cost-inv-")
          home (workspace-bookmark-fixtures-make-workspace "wsbm-cost-inv")
          linked (file-name-as-directory
                  (workspace-bookmark-fixtures-add-worktree
                   repo (expand-file-name "proj" home) :branch "inv")))
    (workspace-bookmark-clear-identity-cache))

  (after-each
    (workspace-bookmark-clear-identity-cache))

  (describe "a linked worktree's own config.worktree"
    ;; Under extensions.worktreeConfig, `git config --worktree' writes to
    ;; .git/worktrees/<name>/config.worktree, which `git remote get-url'
    ;; honours (an insteadOf rule rewrites the URL in that worktree only)
    ;; and which is NOT the shared config the memo used to stamp with.

    (before-each
      (workspace-bookmark-fixtures-git repo "config" "extensions.worktreeConfig" "true"))

    (it "is read by git remote get-url for that worktree only"
      (workspace-bookmark-fixtures-git
       linked "config" "--worktree"
       "url.https://mirror.example/.insteadOf" "git@example.com:")
      (expect (workspace-bookmark-fixtures-git linked "remote" "get-url" "origin")
              :to-equal "https://mirror.example/fixtures/cost.git")
      (expect (workspace-bookmark-fixtures-git repo "remote" "get-url" "origin")
              :to-equal portable-bookmarks-cost-spec--remote))

    (it "changes the identity discovery reports, without the shared config changing"
      (expect (workspace-bookmark--workspace-checkouts
               portable-bookmarks-cost-spec--identity "wsbm-cost-inv")
              :to-equal (list linked))
      (let* ((shared (expand-file-name ".git/config" repo))
             (before (file-attribute-modification-time (file-attributes shared))))
        (workspace-bookmark-fixtures-git
         linked "config" "--worktree"
         "url.https://mirror.example/.insteadOf" "git@example.com:")
        (set-file-times (expand-file-name "config.worktree"
                                          (workspace-bookmark--checkout-git-dir linked))
                        (time-add (current-time) 10))
        (expect (file-attribute-modification-time (file-attributes shared))
                :to-equal before)
        (expect (workspace-bookmark--workspace-checkouts
                 portable-bookmarks-cost-spec--identity "wsbm-cost-inv")
                :to-be nil)
        (expect (workspace-bookmark--workspace-checkouts
                 "mirror.example/fixtures/cost" "wsbm-cost-inv")
                :to-equal (list linked))
        ;; The main worktree is not affected by the linked worktree's
        ;; override, and its identity says so.
        (expect (workspace-bookmark--memoised-identity
                 repo (expand-file-name ".git" repo))
                :to-equal portable-bookmarks-cost-spec--identity)))

    (it "is found without running git"
      (expect (workspace-bookmark--checkout-git-dir linked)
              :to-equal (workspace-bookmark-fixtures-git
                         linked "rev-parse" "--path-format=absolute" "--git-dir"))
      (expect (workspace-bookmark--checkout-git-dir repo)
              :to-equal (expand-file-name ".git" repo))
      (expect (workspace-bookmark--checkout-git-dir
               (workspace-bookmark-fixtures-temp-dir "wsbm-cost-nogit-"))
              :to-be nil)))

  (describe "the main worktree's config.worktree"

    (it "changes the identity too, without the shared config changing"
      ;; REPO (jf/emacs-dir) and LINKED (in the registered home) are in
      ;; view.  The override applies to the main worktree only, so the
      ;; fixture identity still reaches REPO through LINKED; what the memo
      ;; must notice is that REPO itself now answers to the rewritten one.
      (workspace-bookmark-fixtures-git repo "config" "extensions.worktreeConfig" "true")
      (let ((jf/emacs-dir repo))
        (cl-letf (((symbol-function 'workspace-bookmark--current-workspace)
                   (lambda () nil)))
          (expect (workspace-bookmark--canonical-checkouts-for-identity
                   "mirror.example/fixtures/cost")
                  :to-be nil)
          (expect (workspace-bookmark--canonical-checkouts-for-identity
                   portable-bookmarks-cost-spec--identity)
                  :to-equal (list repo))
          (let* ((shared (expand-file-name ".git/config" repo))
                 (before (file-attribute-modification-time (file-attributes shared))))
            (workspace-bookmark-fixtures-git
             repo "config" "--worktree"
             "url.https://mirror.example/.insteadOf" "git@example.com:")
            (set-file-times (expand-file-name ".git/config.worktree" repo)
                            (time-add (current-time) 10))
            (expect (file-attribute-modification-time (file-attributes shared))
                    :to-equal before)
            (expect (workspace-bookmark--canonical-checkouts-for-identity
                     "mirror.example/fixtures/cost")
                    :to-equal (list repo))
            (expect (workspace-bookmark--canonical-checkouts-for-identity
                     portable-bookmarks-cost-spec--identity)
                    :to-equal (list repo)))))))

  (describe "a memoised nil identity"

    (it "is replaced once the checkout gains a remote"
      (let* ((bare-home (workspace-bookmark-fixtures-make-workspace "wsbm-cost-gain"))
             (clone (file-name-as-directory (expand-file-name "clone" bare-home))))
        (workspace-bookmark-fixtures-git bare-home "clone" "--quiet" "--no-local"
                                         repo "clone")
        (workspace-bookmark-fixtures-git clone "remote" "remove" "origin")
        (spy-on 'workspace-bookmark--root-identity :and-call-through)
        ;; Two lookups, one identity computation: nil is memoised.
        (dotimes (_ 2)
          (expect (workspace-bookmark--workspace-checkouts
                   portable-bookmarks-cost-spec--identity "wsbm-cost-gain")
                  :to-be nil))
        (expect 'workspace-bookmark--root-identity :to-have-been-called-times 1)
        ;; `git remote add' writes the shared config; pin a distinct mtime
        ;; so the spec does not depend on timestamp granularity.
        (workspace-bookmark-fixtures-git
         clone "remote" "add" "origin" portable-bookmarks-cost-spec--remote)
        (set-file-times (expand-file-name ".git/config" clone)
                        (time-add (current-time) 10))
        (expect (workspace-bookmark--workspace-checkouts
                 portable-bookmarks-cost-spec--identity "wsbm-cost-gain")
                :to-equal (list clone))
        (expect 'workspace-bookmark--root-identity :to-have-been-called-times 2)))))

;;; Behavioral — a remote (TRAMP) name never reaches a file-name handler
;;
;; The register's load-bearing rule for this boundary: `file-remote-p' is
;; tested before ANY file-name-handler dispatch, so TRAMP can neither
;; signal nor prompt.  The per-call memo keys a position by truename, and
;; `file-truename' on a TRAMP name opens the connection — so the memo must
;; ask `file-remote-p' first, exactly as the runner does.  Real TRAMP is
;; never reached: "/fake:" names are served by the core fake remote handler
;; (`workspaces-test-with-fake-remote', workspaces-test-helpers.el, loaded
;; by the fixtures) with its strict default, which answers `file-remote-p'
;; from the name and signals `file-error' for every other operation,
;; recording each operation it is asked.

(describe "A remote checkout name reaches no file-name handler but file-remote-p"
  :var (repo home)

  (workspace-bookmark-fixtures-with-empty-registry
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-cost-spec--remote :prefix "wsbm-cost-rem-")
          home (workspace-bookmark-fixtures-make-workspace "wsbm-cost-rem"))
    (workspace-bookmark-fixtures-add-worktree
     repo (expand-file-name "proj" home) :branch "rem")
    (workspace-bookmark-clear-identity-cache))

  (after-each
    (workspace-bookmark-clear-identity-cache))

  (defconst portable-bookmarks-cost-spec--remote-name "/fake:host:/srv/repo/"
    "A remote checkout name; the fake handler serves it.")

  (it "is skipped by the public entry, which still returns the local candidates"
    (let ((result (workspaces-test-with-fake-remote "/fake:"
                    (let ((jf/emacs-dir nil))
                      (workspace-bookmark-canonical-checkouts-in-view
                       (list portable-bookmarks-cost-spec--remote-name repo))))))
      (expect result :to-equal (list repo))
      (expect (workspaces-test-fake-remote-operations)
              :to-equal '(file-remote-p))))

  (it "is nil for the public entry when it is the only candidate"
    (expect (workspaces-test-with-fake-remote "/fake:"
              (let ((jf/emacs-dir nil))
                (workspace-bookmark-canonical-checkouts-in-view
                 (list portable-bookmarks-cost-spec--remote-name))))
            :to-be nil)
    (expect (workspaces-test-fake-remote-operations)
            :to-equal '(file-remote-p)))

  (it "has no canonical checkout, and no position"
    (expect (workspaces-test-with-fake-remote "/fake:"
              (workspace-bookmark--canonical-checkout
               portable-bookmarks-cost-spec--remote-name))
            :to-be nil)
    (expect (workspaces-test-fake-remote-operations)
            :to-equal '(file-remote-p))
    (expect (workspaces-test-with-fake-remote "/fake:"
              (workspace-bookmark--with-discovery-memo
                (workspace-bookmark--shared-position
                 portable-bookmarks-cost-spec--remote-name)))
            :to-be nil)
    (expect (workspaces-test-fake-remote-operations)
            :to-equal '(file-remote-p)))

  (it "is skipped by --canonical-checkouts-for-identity when it is in view"
    (spy-on 'workspace-bookmark--checkouts-in-view :and-return-value
            (list portable-bookmarks-cost-spec--remote-name repo))
    (expect (workspaces-test-with-fake-remote "/fake:"
              (workspace-bookmark--canonical-checkouts-for-identity
               portable-bookmarks-cost-spec--identity))
            :to-equal (list repo))
    (expect (workspaces-test-fake-remote-operations)
            :to-equal '(file-remote-p)))

  (it "is skipped by --workspace-checkouts when it is a candidate of the home"
    (spy-on 'workspace-bookmark--home-checkouts :and-return-value
            (list portable-bookmarks-cost-spec--remote-name
                  (file-name-as-directory (expand-file-name "proj" home))))
    (expect (workspaces-test-with-fake-remote "/fake:"
              (workspace-bookmark--workspace-checkouts
               portable-bookmarks-cost-spec--identity "wsbm-cost-rem"))
            :to-equal (list (file-name-as-directory (expand-file-name "proj" home))))
    (expect (workspaces-test-fake-remote-operations)
            :to-equal '(file-remote-p)))

  (it "is refused as a non-string too, without signalling"
    (expect (workspace-bookmark--with-discovery-memo
              (workspace-bookmark--shared-position nil))
            :to-be nil)
    (expect (workspace-bookmark-canonical-checkouts-in-view (list nil 42 repo))
            :to-equal (list repo))))

;;; Behavioral — the stamp seam during a layout capture
;;
;; seam/capture-stamp-cost (design.md of review-followups-2026-10-10, D4).
;; Every layout capture — a tab switch, an idle autosave, `kill-emacs' —
;; stamps one record per window leaf through `bookmark-make-record'.  Real
;; git, real windows and the real capture walk (`workspace--capture-frameset');
;; the spawn count is the call-through spy on `workspace-bookmark--git',
;; counted per git subcommand.  A spy installed between two captures counts
;; only the second.  The three-leaf, one-directory budget of the same
;; scenario is pending in review-findings-2026-10-10-spec.el until promoted
;; here.

(defun portable-bookmarks-cost-spec--show (files)
  "Show each of FILES in a window of its own, the last one selected.
Windows alternate between a split to the right and one below, so three
leaves fit a batch frame."
  (delete-other-windows)
  (find-file (car files))
  (let ((right t))
    (dolist (file (cdr files))
      (select-window (if right (split-window-right) (split-window-below)))
      (setq right (not right))
      (find-file file))))

(defun portable-bookmarks-cost-spec--leaf-stamps (state)
  "Return the (IDENTITY . RELPATH) stamped on each leaf of window-state STATE.
In leaf order; nil for a leaf whose record carries no stamp."
  (let ((stamps nil))
    (workspace--window-state-walk-leaves
     state
     (lambda (leaf)
       (let* ((ws-buffer (map-elt (map-elt (cdr leaf) 'parameters)
                                  'workspace-buffer))
              (record (and ws-buffer (workspace-buffer-bookmark ws-buffer)))
              (identity (and record (bookmark-prop-get
                                     record workspace-bookmark-repo-property))))
         (push (and identity
                    (cons identity (bookmark-prop-get
                                    record workspace-bookmark-relpath-property)))
               stamps))
       leaf))
    (nreverse stamps)))

(defun portable-bookmarks-cost-spec--git-calls (&optional subcommand)
  "Return how many spied `workspace-bookmark--git' calls ran SUBCOMMAND.
Every call when SUBCOMMAND is nil.  A call's subcommand is its first
argument after the directory that is not an option."
  (seq-count (lambda (args)
               (or (null subcommand)
                   (equal subcommand
                          (seq-find (lambda (arg)
                                      (and (stringp arg)
                                           (not (string-prefix-p "-" arg))))
                                    (cdr args)))))
             (spy-calls-all-args 'workspace-bookmark--git)))

(defun portable-bookmarks-cost-spec--index-mtime (git-dir)
  "Return the modification time of GIT-DIR's `index'."
  (file-attribute-modification-time
   (file-attributes (expand-file-name "index" git-dir))))

(defun portable-bookmarks-cost-spec--bump-index (git-dir)
  "Move the modification time of GIT-DIR's `index' ten seconds ahead.
The index's content is unchanged.  Remembered coordinates keyed on that
time are no longer reused; the identity memo, keyed on the config files,
stays warm."
  (set-file-times (expand-file-name "index" git-dir)
                  (time-add (portable-bookmarks-cost-spec--index-mtime git-dir)
                            10)))

(defconst portable-bookmarks-cost-spec--capture-relpaths
  '("one.el" "lisp/two.el" "lisp/deep/three.el")
  "Three tracked files of one checkout, each in a directory of its own.")

(describe "Stamping cost of a layout capture"
  :var (repo files)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-cost-spec--remote
                :prefix "wsbm-cost-cap-")
          files (mapcar (lambda (relpath)
                          (workspace-bookmark-fixtures-commit-file repo relpath))
                        portable-bookmarks-cost-spec--capture-relpaths)))

  (after-each
    (delete-other-windows)
    (workspace-bookmark-clear-identity-cache))

  (xit "positions the checkout once for three leaves in three of its directories"
    ;; Scenario: portable-bookmarks § Capturing a layout positions each checkout once
    ;; seam/capture-stamp-cost.  The leaves sit in three directories, so a
    ;; position shared per directory is not enough: the checkout is
    ;; positioned once.  The first capture learns the identity; the index
    ;; is then bumped so the measured capture positions afresh instead of
    ;; reusing remembered coordinates (the next spec's case).
    (workspace-bookmark-fixtures-hermetic
      (portable-bookmarks-cost-spec--show files)
      (workspace--capture-frameset)
      (portable-bookmarks-cost-spec--bump-index (expand-file-name ".git" repo))
      (spy-on 'workspace-bookmark--git :and-call-through)
      (expect (portable-bookmarks-cost-spec--leaf-stamps (workspace--capture-frameset))
              :to-equal (mapcar (lambda (relpath)
                                  (cons portable-bookmarks-cost-spec--identity relpath))
                                portable-bookmarks-cost-spec--capture-relpaths))
      (expect (portable-bookmarks-cost-spec--git-calls "rev-parse") :to-equal 1)))

  (xit "re-captures an unchanged layout without running git, stamping the same coordinates"
    ;; Scenario: portable-bookmarks § Re-capturing an unchanged leaf runs no git
    ;; seam/capture-stamp-cost.  Nothing changes between the two captures:
    ;; not the files, not the checkout's index, not the configuration.
    (workspace-bookmark-fixtures-hermetic
      (portable-bookmarks-cost-spec--show files)
      (let ((expected (mapcar (lambda (relpath)
                                (cons portable-bookmarks-cost-spec--identity relpath))
                              portable-bookmarks-cost-spec--capture-relpaths)))
        (expect (portable-bookmarks-cost-spec--leaf-stamps (workspace--capture-frameset))
                :to-equal expected)
        (spy-on 'workspace-bookmark--git :and-call-through)
        (expect (portable-bookmarks-cost-spec--leaf-stamps (workspace--capture-frameset))
                :to-equal expected)
        (expect (portable-bookmarks-cost-spec--git-calls) :to-equal 0))))

  (it "stamps a linked worktree's leaf afresh once that worktree's own index changed"
    ;; seam/capture-stamp-cost: remembered coordinates are keyed on the
    ;; index in the worktree's OWN git directory, not in the common
    ;; directory its position names, which holds the main worktree's
    ;; index.  Only the linked worktree's index changes here (the file is
    ;; untracked, and stays on disk), so a memo keyed on the common
    ;; directory's index would hand back the stale stamp.  Holds at HEAD,
    ;; which remembers nothing; it pins the key once something is.
    (workspace-bookmark-fixtures-hermetic
      (let* ((linked (workspace-bookmark-fixtures-add-worktree
                      repo (expand-file-name
                            "linked" (workspace-bookmark-fixtures-temp-dir
                                      "wsbm-cost-cap-wt-"))
                      :branch "cap"))
             (file (expand-file-name "one.el" linked))
             (common (expand-file-name ".git" repo))
             (main-index (portable-bookmarks-cost-spec--index-mtime common)))
        (portable-bookmarks-cost-spec--show (list file))
        (expect (portable-bookmarks-cost-spec--leaf-stamps (workspace--capture-frameset))
                :to-equal (list (cons portable-bookmarks-cost-spec--identity "one.el")))
        (workspace-bookmark-fixtures-git linked "rm" "--cached" "--quiet" "--" "one.el")
        (expect (file-exists-p file) :to-be t)
        (expect (portable-bookmarks-cost-spec--index-mtime common) :to-equal main-index)
        (expect (portable-bookmarks-cost-spec--leaf-stamps (workspace--capture-frameset))
                :to-equal '(nil))))))

;;; portable-bookmarks-cost-spec.el ends here
