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

(defconst portable-bookmarks-cost-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-cost-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-cost-spec--this-dir))

(defvar jf/emacs-dir)

;; Owned by tabs.el; see portable-bookmarks-discovery-spec.el for why this
;; stand-in exists.  Under the harness the real definition is bound.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))

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
;; never loaded: `file-name-handler-alist' is bound to one fake handler for
;; "/fake:" names that answers `file-remote-p' from the name and signals
;; `file-error' for every other operation (the pattern of
;; git-worktrees-spec.el § "remote (TRAMP) magit entries"), recording each
;; operation it is asked.

(defvar portable-bookmarks-cost-spec--remote-ops nil
  "Operations the fake remote file-name handler was asked, newest first.")

(defun portable-bookmarks-cost-spec--fake-remote-handler (operation &rest args)
  "Fake handler for \"/fake:\" names: records OPERATION, answers only `file-remote-p'."
  (push operation portable-bookmarks-cost-spec--remote-ops)
  (if (eq operation 'file-remote-p)
      (let ((name (car args)))
        (and (stringp name)
             (string-match "\\`/fake:[^:]*:" name)
             (match-string 0 name)))
    (signal 'file-error (list "remote probed" (symbol-name operation)))))

(defmacro portable-bookmarks-cost-spec--with-fake-remote (&rest body)
  "Run BODY with \"/fake:\" names served by the fake remote handler only.
Returns BODY's value; the operations asked are left in
`portable-bookmarks-cost-spec--remote-ops'."
  (declare (indent 0))
  `(let ((file-name-handler-alist
          (list (cons "\\`/fake:" #'portable-bookmarks-cost-spec--fake-remote-handler))))
     (setq portable-bookmarks-cost-spec--remote-ops nil)
     ,@body))

(defun portable-bookmarks-cost-spec--remote-ops-asked ()
  "Return the distinct operations the fake remote handler was asked."
  (seq-uniq (reverse portable-bookmarks-cost-spec--remote-ops)))

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
    (let ((result (portable-bookmarks-cost-spec--with-fake-remote
                    (let ((jf/emacs-dir nil))
                      (workspace-bookmark-canonical-checkouts-in-view
                       (list portable-bookmarks-cost-spec--remote-name repo))))))
      (expect result :to-equal (list repo))
      (expect (portable-bookmarks-cost-spec--remote-ops-asked)
              :to-equal '(file-remote-p))))

  (it "is nil for the public entry when it is the only candidate"
    (expect (portable-bookmarks-cost-spec--with-fake-remote
              (let ((jf/emacs-dir nil))
                (workspace-bookmark-canonical-checkouts-in-view
                 (list portable-bookmarks-cost-spec--remote-name))))
            :to-be nil)
    (expect (portable-bookmarks-cost-spec--remote-ops-asked)
            :to-equal '(file-remote-p)))

  (it "has no canonical checkout, and no position"
    (expect (portable-bookmarks-cost-spec--with-fake-remote
              (workspace-bookmark--canonical-checkout
               portable-bookmarks-cost-spec--remote-name))
            :to-be nil)
    (expect (portable-bookmarks-cost-spec--remote-ops-asked)
            :to-equal '(file-remote-p))
    (expect (portable-bookmarks-cost-spec--with-fake-remote
              (workspace-bookmark--with-discovery-memo
                (workspace-bookmark--shared-position
                 portable-bookmarks-cost-spec--remote-name)))
            :to-be nil)
    (expect (portable-bookmarks-cost-spec--remote-ops-asked)
            :to-equal '(file-remote-p)))

  (it "is skipped by --canonical-checkouts-for-identity when it is in view"
    (spy-on 'workspace-bookmark--checkouts-in-view :and-return-value
            (list portable-bookmarks-cost-spec--remote-name repo))
    (expect (portable-bookmarks-cost-spec--with-fake-remote
              (workspace-bookmark--canonical-checkouts-for-identity
               portable-bookmarks-cost-spec--identity))
            :to-equal (list repo))
    (expect (portable-bookmarks-cost-spec--remote-ops-asked)
            :to-equal '(file-remote-p)))

  (it "is skipped by --workspace-checkouts when it is a candidate of the home"
    (spy-on 'workspace-bookmark--home-checkouts :and-return-value
            (list portable-bookmarks-cost-spec--remote-name
                  (file-name-as-directory (expand-file-name "proj" home))))
    (expect (portable-bookmarks-cost-spec--with-fake-remote
              (workspace-bookmark--workspace-checkouts
               portable-bookmarks-cost-spec--identity "wsbm-cost-rem"))
            :to-equal (list (file-name-as-directory (expand-file-name "proj" home))))
    (expect (portable-bookmarks-cost-spec--remote-ops-asked)
            :to-equal '(file-remote-p)))

  (it "is refused as a non-string too, without signalling"
    (expect (workspace-bookmark--with-discovery-memo
              (workspace-bookmark--shared-position nil))
            :to-be nil)
    (expect (workspace-bookmark-canonical-checkouts-in-view (list nil 42 repo))
            :to-equal (list repo))))

;;; portable-bookmarks-cost-spec.el ends here
