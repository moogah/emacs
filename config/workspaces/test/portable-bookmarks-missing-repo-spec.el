;;; portable-bookmarks-missing-repo-spec.el --- Missing-repo choice specs -*- lexical-binding: t; -*-

;; Specs for the `missing-repo' prompt: the bounded choice the jump seam
;; offers when the current workspace holds no checkout of a bookmark's
;; repository and exactly one canonical checkout is in view.
;;
;; * BEHAVIORAL — real `git' (one repository; a linked worktree in
;;   workspace A's home; workspace C holding nothing), records stamped by
;;   the real stamp seam through `bookmark-set', jumps driven through
;;   `bookmark-jump'.  The ONE boundary stubbed inside the jump is the
;;   prompt itself: the fixtures' jump harness
;;   (`workspace-bookmark-fixtures-jump' with `:choose') binds
;;   `read-multiple-choice' to a stand-in that records what it was offered
;;   and answers with one key.
;;   `jf/workspace--add-worktree' (git-worktrees, magit-backed) is bound
;;   per spec to a stub that either really adds a worktree with the
;;   fixtures or returns a vocabulary outcome; `nil' unbinds it.  The
;;   integration registry is bound per spec too (the `:integrations'
;;   option of `workspace-bookmark-fixtures-hermetic'): the command is
;;   registered as git-worktrees registers it once magit is loaded, so the
;;   offer never depends on whether the harness happened to load magit.  ONE
;;   spec runs the REAL command (git-worktrees.el is loaded here for it)
;;   and stands in only for magit and the branch prompt, so the contract
;;   the chooser relies on — the command honours `:source-repo' — is
;;   pinned across the two modules, not against a stub.
;;
;; * UNIT — the offered option set and the prompt text, the workspace
;;   health gate behind the add-worktree offer, the payload handed to the
;;   worktree command, the chooser's own barrier, and a structural read of
;;   the tangled section.
;;
;; The "current workspace" is bound per jump (`workspace--current-name'),
;; as in portable-bookmarks-jump-spec.el.  Registry isolation goes
;; through the fixtures' describe-level macro
;; `workspace-bookmark-fixtures-with-empty-registry'
;; (register/invariant/spec-registry-isolation-via-fixtures); this file
;; never touches `workspace--registry' itself.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'bookmark)
(require 'dired)
(require 'view)
(require 'rmc)

(defconst portable-bookmarks-missing-repo-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-missing-repo-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-missing-repo-spec--this-dir))
;; The anchor payload's one constructor, and the real worktree command.
;; Neither is required by portable-bookmarks; production loads both.
(load (expand-file-name "../integrations.el"
                        portable-bookmarks-missing-repo-spec--this-dir))
(load (expand-file-name "../git-worktrees.el"
                        portable-bookmarks-missing-repo-spec--this-dir))

(defvar jf/emacs-dir)
(defvar workspace--integrations)


;;; Helpers

(defconst portable-bookmarks-missing-repo-spec--remote
  "git@example.com:fixtures/missing.git"
  "Remote URL given to the fixture repository.")

(defconst portable-bookmarks-missing-repo-spec--identity
  "example.com/fixtures/missing"
  "The identity `portable-bookmarks-missing-repo-spec--remote' normalizes to.")

(defun portable-bookmarks-missing-repo-spec--git-worktree-registry ()
  "Return an integration registry holding only the git-worktree integration.
Registered through core's real `workspace-register-integration' exactly
as git-worktrees.el registers it once magit is loaded."
  (let ((workspace--integrations nil))
    (workspace-register-integration 'git-worktree
      :label "git worktree"
      :menu (cons "w" #'jf/workspace--add-worktree)
      :on-purge #'jf/workspace--worktree-on-purge)
    workspace--integrations))

(defun portable-bookmarks-missing-repo-spec--quits-p (thunk)
  "Return `quit' when THUNK signals `quit', `error' when it signals an error, else nil."
  (condition-case nil
      (progn (funcall thunk) nil)
    (quit 'quit)
    (error 'error)))

(defun portable-bookmarks-missing-repo-spec--marker (buffer)
  "Return BUFFER's `mode-line-buffer-identification' as a string, or nil when not local."
  (and (local-variable-p 'mode-line-buffer-identification buffer)
       (format "%S" (buffer-local-value 'mode-line-buffer-identification buffer))))

(defmacro portable-bookmarks-missing-repo-spec--collecting-messages (var &rest body)
  "Run BODY with every `message' call collected into VAR (a list, newest first)."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf* ((real (symbol-function 'message))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format-message fmt args) ,var))
                   (apply real fmt args))))
       ,@body)))


;;; Behavioral — the three answers, through the real jump path

(describe "missing-repo choice: the offer"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-mr" portable-bookmarks-missing-repo-spec--remote
                 :holding '("a") :empty '("c")
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "offers add worktree, view read-only and cancel, naming the bookmark, the workspace, the identity and the canonical checkout"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?c))))
        (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1)
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?a ?v ?c))
        (let ((prompt (workspace-bookmark-fixtures-prompt-text)))
          (expect prompt :to-match (regexp-quote "\"readme\""))
          (expect prompt :to-match (regexp-quote "\"wsbm-mr-c\""))
          (expect prompt :to-match (regexp-quote portable-bookmarks-missing-repo-spec--identity))
          (expect prompt :to-match (regexp-quote (abbreviate-file-name (plist-get world :repo))))
          (expect prompt :not :to-match "not offered")))))

  (it "omits add worktree when jf/workspace--add-worktree is not loaded, and says so"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree) nil))
          (expect (fboundp 'jf/workspace--add-worktree) :to-be nil)
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?c))))
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?v ?c))
        (expect (workspace-bookmark-fixtures-prompt-text)
                :to-match "add worktree here not offered: the git worktree integration is not loaded"))))

  (it "omits add worktree when the command is defined but not registered (magit not loaded), and says so"
    ;; git-worktrees.el defines `jf/workspace--add-worktree' whether or not
    ;; magit is loaded; only its registration waits for magit.  Here the
    ;; REAL command is bound and nothing is registered.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
            (workspace--integrations nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (expect (fboundp 'jf/workspace--add-worktree) :to-be-truthy)
        (portable-bookmarks-missing-repo-spec--quits-p
         (lambda () (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?c)))
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?v ?c))
        (expect (workspace-bookmark-fixtures-prompt-text)
                :to-match "add worktree here not offered: the git worktree integration is not loaded"))))

  (it "omits add worktree when the current workspace is tagged broken, says so, and never calls the command"
    ;; Audit eoc-1790453714-01, probe P1, as a spec: the tab still carries
    ;; the workspace's name, persistence tagged the entry broken at load,
    ;; and the home IS present on disk (what a recreated home looks like),
    ;; so the tag alone must withhold the offer.  The prompt stand-in takes
    ;; add-worktree whenever it is offered: a gate that offers it is caught
    ;; by the command being called.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (workspace-bookmark-fixtures-mark-broken "wsbm-mr-c")
        (expect (file-directory-p (plist-get world :home-c)) :to-be-truthy)
        (spy-on 'jf/workspace--add-worktree :and-return-value 'skipped)
        (portable-bookmarks-missing-repo-spec--quits-p
         (lambda ()
           (workspace-bookmark-fixtures-jump
            "readme" :workspace "wsbm-mr-c"
            :choose '(?a ?c))))
        (expect 'jf/workspace--add-worktree :not :to-have-been-called)
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?v ?c))
        (expect (workspace-bookmark-fixtures-prompt-text)
                :to-match (regexp-quote
                           (concat "add worktree here not offered: workspace \"wsbm-mr-c\" is broken"
                                   " (tagged at load: its home was missing or remote);"
                                   " re-anchor or purge it first"))))))

  (it "omits add worktree when the current workspace's home was deleted mid-session, names the home, and never calls the command"
    ;; Probe P2: no broken tag (the tag is load-time only), the tab is
    ;; still current, the home is gone.  Offered, `git worktree add' would
    ;; recreate the home as a bare directory holding one worktree.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (home-c (plist-get world :home-c)))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (delete-directory home-c t)
        (expect (workspace--broken-p (gethash "wsbm-mr-c" workspace--registry)) :to-be nil)
        (spy-on 'jf/workspace--add-worktree :and-return-value 'skipped)
        (portable-bookmarks-missing-repo-spec--quits-p
         (lambda ()
           (workspace-bookmark-fixtures-jump
            "readme" :workspace "wsbm-mr-c"
            :choose '(?a ?c))))
        (expect 'jf/workspace--add-worktree :not :to-have-been-called)
        (expect (file-directory-p home-c) :to-be nil)
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?v ?c))
        (let ((prompt (workspace-bookmark-fixtures-prompt-text)))
          (expect prompt :to-match "add worktree here not offered: the home of workspace \"wsbm-mr-c\"")
          (expect prompt :to-match (regexp-quote (file-name-nondirectory (directory-file-name home-c))))
          (expect prompt :to-match "no longer exists")))))

  (it "asks the gate again at the call: a home deleted while the prompt is up declines naming it, and never calls the command"
    ;; The offer is computed before the prompt; the answer comes after an
    ;; unbounded wait.  Here the home is present at the offer (add worktree
    ;; IS offered) and deleted while the prompt is up, then add worktree is
    ;; chosen: `git worktree add' would recreate the home.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (home-c (plist-get world :home-c)))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (spy-on 'jf/workspace--add-worktree :and-return-value 'skipped)
        (let ((message (workspace-bookmark-fixtures-decline
                        "readme" :workspace "wsbm-mr-c"
                        :choose (workspace-bookmark-fixtures-answering
                                 ?a (lambda () (delete-directory home-c t))))))
          (expect 'jf/workspace--add-worktree :not :to-have-been-called)
          (expect message :to-match (regexp-quote "\"readme\""))
          (expect message :to-match (regexp-quote "the home of workspace \"wsbm-mr-c\""))
          (expect message :to-match (regexp-quote (file-name-nondirectory (directory-file-name home-c))))
          (expect message :to-match "no longer exists")
          (expect message :to-match "nothing opened"))
        (expect (file-directory-p home-c) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil))))

  (it "omits view read-only when the relpath is absent under the canonical checkout (:path nil), and says so"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (workspace-bookmark-fixtures-commit-file
                     (plist-get world :wt-a) "only-a.el")))
        (expect (file-exists-p (expand-file-name "only-a.el" (plist-get world :repo)))
                :to-be nil)
        (workspace-bookmark-fixtures-set-bookmark a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (workspace-bookmark-fixtures-jump "only-a" :workspace "wsbm-mr-c" :choose ?c))))
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?a ?c))
        (expect (workspace-bookmark-fixtures-prompt-text)
                :to-match "view read-only not offered: only-a.el is absent there"))))

  (it "is cancel-only when both are withheld, stating both reasons"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let ((a-path (workspace-bookmark-fixtures-commit-file
                     (plist-get world :wt-a) "only-a.el")))
        (workspace-bookmark-fixtures-set-bookmark a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree) nil))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (workspace-bookmark-fixtures-jump "only-a" :workspace "wsbm-mr-c" :choose ?c))))
        (expect (workspace-bookmark-fixtures-offered-keys) :to-equal '(?c))
        (let ((prompt (workspace-bookmark-fixtures-prompt-text)))
          (expect prompt :to-match "add worktree here not offered")
          (expect prompt :to-match "view read-only not offered")))))

  (it "declines with the seam's missing-repo report instead of prompting when no one can answer (noninteractive)"
    ;; The harness IS non-interactive, so the real predicate is used here
    ;; and the prompt stand-in must never be reached.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (asked nil))
        (expect noninteractive :to-be-truthy)
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((message
               (condition-case err
                   (progn
                     (cl-letf (((symbol-function 'workspace--current-name)
                                (lambda () "wsbm-mr-c"))
                               ((symbol-function 'read-multiple-choice)
                                (lambda (&rest _) (setq asked t) nil)))
                       (bookmark-jump "readme"))
                     nil)
                 (user-error (cadr err)))))
          (expect asked :to-be nil)
          (expect message :to-match "holds no checkout")
          (expect message :to-match "nothing opened"))
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        ;; The predicate is the only thing between that decline and the
        ;; prompt: forced to t, the same jump asks.
        (portable-bookmarks-missing-repo-spec--quits-p
         (lambda () (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?c)))
        (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1))))

  (it "lets quit at the prompt propagate, opening nothing"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (expect (portable-bookmarks-missing-repo-spec--quits-p
                 (lambda ()
                   (workspace-bookmark-fixtures-jump
                    "readme" :workspace "wsbm-mr-c" :choose (lambda (&rest _) (signal 'quit nil)))))
                :to-be 'quit)
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)))))


(describe "missing-repo choice: the workspace health gate"
  ;; UNIT — `workspace-bookmark--workspace-unusable', the module's ONE
  ;; health notion (register/invariant/add-worktree-offer-shares-workspace-
  ;; health-gate): the add-worktree offer and the off-workspace picker both
  ;; read it.  Nil means usable; otherwise the reason, which the offer
  ;; turns into its prompt phrase.
  (workspace-bookmark-fixtures-with-empty-registry)

  (it "answers nil for a registered workspace whose home is a local directory"
    (workspace-bookmark-fixtures-make-workspace "wsbm-mr-ok")
    (expect (workspace-bookmark--workspace-unusable "wsbm-mr-ok") :to-be nil))

  (it "names an unregistered name, and nil, `unregistered'"
    (expect (workspace-bookmark--workspace-unusable "wsbm-mr-nope") :to-be 'unregistered)
    (expect (workspace-bookmark--workspace-unusable nil) :to-be 'unregistered))

  (it "names a tagged workspace `broken', whether or not its home is on disk"
    (let ((home (workspace-bookmark-fixtures-make-workspace "wsbm-mr-tagged")))
      (workspace-bookmark-fixtures-mark-broken "wsbm-mr-tagged")
      (expect (workspace-bookmark--workspace-unusable "wsbm-mr-tagged") :to-be 'broken)
      (delete-directory home t)
      (expect (workspace-bookmark--workspace-unusable "wsbm-mr-tagged") :to-be 'broken)))

  (it "names a plist without a :home `no-home'"
    ;; A stale or hand-constructed plist: `workspace--home' answers nil.
    (workspace-bookmark-fixtures-make-workspace "wsbm-mr-homeless")
    (puthash "wsbm-mr-homeless"
             (plist-put (copy-sequence (gethash "wsbm-mr-homeless" workspace--registry))
                        :home nil)
             workspace--registry)
    (expect (workspace-bookmark--workspace-unusable "wsbm-mr-homeless") :to-be 'no-home))

  (it "names a remote home `home-remote' without ever asking the disk about it"
    ;; The home is served by the core fake remote handler with its strict
    ;; default: `file-remote-p' answers from the name, and any other
    ;; operation on it (a `file-directory-p', say) is recorded and signals.
    (let ((prefix "/wsbm-mr-remote:"))
      (workspace-bookmark-fixtures-register-workspace
       "wsbm-mr-remote" (concat prefix "/srv/mr/"))
      (workspaces-test-with-fake-remote prefix
        (expect (workspace-bookmark--workspace-unusable "wsbm-mr-remote") :to-be 'home-remote))
      (expect workspaces-test-fake-remote-touches :to-be nil)
      (expect (workspaces-test-fake-remote-operations) :to-equal '(file-remote-p))))

  (it "names a home gone from disk `home-missing', untagged"
    (let ((home (workspace-bookmark-fixtures-make-workspace "wsbm-mr-gone")))
      (delete-directory home t)
      (expect (workspace--broken-p (gethash "wsbm-mr-gone" workspace--registry)) :to-be nil)
      (expect (workspace-bookmark--workspace-unusable "wsbm-mr-gone") :to-be 'home-missing)))

  (it "withholds add-worktree with the reason's phrase, after the no-workspace and not-runnable arms"
    (let ((home (workspace-bookmark-fixtures-make-workspace "wsbm-mr-phrase"))
          (resolution '(:path "/nonexistent/README.md" :relpath "README.md"))
          (workspace--integrations
           (portable-bookmarks-missing-repo-spec--git-worktree-registry)))
      (expect (assq 'add-worktree (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-phrase"))
              :to-be nil)
      (delete-directory home t)
      (expect (cdr (assq 'add-worktree (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-phrase")))
              :to-match "no longer exists")
      (expect (cdr (assq 'add-worktree (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-absent")))
              :to-equal "workspace \"wsbm-mr-absent\" is not registered")
      ;; No workspace, then not runnable, come first.
      (expect (cdr (assq 'add-worktree (workspace-bookmark--missing-repo-withheld resolution nil)))
              :to-equal "no workspace is current to add a worktree to")
      (let ((workspace--integrations nil))
        (expect (cdr (assq 'add-worktree (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-phrase")))
                :to-equal "the git worktree integration is not loaded")))))

(describe "missing-repo choice: the add-worktree offer follows git-worktrees' real registration"
  ;; Cross-module pin: no hand-copied registration.  git-worktrees.el's
  ;; OWN registration form (double-gated on `workspaces' and `magit') runs
  ;; into an isolated integration registry, and the chooser's predicate is
  ;; asked through core's accessor before and after it.  If git-worktrees
  ;; registered a different command, or core's accessor stopped seeing it,
  ;; the option would silently disappear; this spec would fail.
  (workspace-bookmark-fixtures-with-empty-registry)

  (it "withholds add-worktree before git-worktrees registers, and offers it after its own registration runs"
    ;; Both load guards (`workspaces' and `magit') must really be satisfied:
    ;; `features' is not a special variable, so a `let' of it in this
    ;; lexical file would be invisible to `featurep' and the spec would
    ;; pass only when the harness had already loaded both.  The global
    ;; list is changed for real, adding ONLY the features that are absent,
    ;; and exactly those are removed again in `unwind-protect' (anything
    ;; the load itself provides stays).  Pushing a symbol onto `features'
    ;; without `provide' runs no `after-load-alist' entry.
    (let* ((workspace--integrations nil)
           (added (seq-remove #'featurep '(workspaces magit)))
           (home (workspace-bookmark-fixtures-make-workspace "wsbm-mr-reg"))
           (resolution '(:path "/nonexistent/README.md" :relpath "README.md")))
      (unwind-protect
          (progn
            (setq features (append added features))
            (expect (and (featurep 'workspaces) (featurep 'magit)) :to-be-truthy)
            (expect home :to-be-truthy)
            (expect (fboundp 'jf/workspace--add-worktree) :to-be-truthy)
            (expect (workspace-bookmark--add-worktree-runnable-p) :to-be nil)
            (expect (assq 'add-worktree
                          (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-reg"))
                    :to-equal '(add-worktree . "the git worktree integration is not loaded"))
            (load (expand-file-name "../git-worktrees.el"
                                    portable-bookmarks-missing-repo-spec--this-dir)
                  nil t)
            (expect (assq 'git-worktree workspace--integrations) :to-be-truthy)
            (expect (workspace-bookmark--add-worktree-runnable-p) :to-be t)
            (expect (assq 'add-worktree
                          (workspace-bookmark--missing-repo-withheld resolution "wsbm-mr-reg"))
                    :to-be nil))
        (dolist (feature added)
          (setq features (delq feature features)))))))

(describe "missing-repo choice: view read-only"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-mr" portable-bookmarks-missing-repo-spec--remote
                 :holding '("a") :empty '("c")
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "opens the canonical copy in view-mode, read-only, marked in the mode line, with one message naming path and branch; the stored record is untouched"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored))
             (buffer nil))
        (portable-bookmarks-missing-repo-spec--collecting-messages messages
          (setq buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v))
          (let ((ours (seq-filter (lambda (m) (string-match-p "Bookmark \"readme\"" m))
                                  messages)))
            (expect (length ours) :to-equal 1)
            (expect (car ours) :to-match "read-only")
            (expect (car ours) :to-match (regexp-quote (abbreviate-file-name canonical-path)))
            (expect (car ours) :to-match "branch main")))
        (expect (workspace--same-file-p (buffer-file-name buffer) canonical-path)
                :to-be-truthy)
        (expect (workspace--same-file-p (buffer-file-name buffer) a-path)
                :to-be nil)
        (with-current-buffer buffer
          (expect (point) :to-equal workspace-bookmark-fixtures-jump-position)
          (expect buffer-read-only :to-be-truthy)
          (expect view-mode :to-be-truthy)
          (expect workspace-bookmark--canonical-view :to-be-truthy)
          (expect (workspace--same-file-p workspace-bookmark--canonical-view
                                          (plist-get world :repo))
                  :to-be-truthy))
        (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")
        (expect (portable-bookmarks-missing-repo-spec--marker buffer)
                :to-match (regexp-quote (abbreviate-file-name
                                         (directory-file-name (plist-get world :repo)))))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "leaves a stock C-x C-q as the deliberate way out: read-only-mode off makes it writable, and the marker stays"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v)))
          (with-current-buffer buffer
            (expect view-mode :to-be-truthy)
            (read-only-mode -1)
            (expect buffer-read-only :to-be nil)
            (expect view-mode :to-be nil)
            (expect workspace-bookmark--canonical-view :to-be-truthy))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")))))

  (it "keeps the view through a revert-buffer that re-runs the major mode: marker, view-mode and read-only all come back"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v)))
          (with-current-buffer buffer
            (revert-buffer t t nil)
            (expect buffer-read-only :to-be-truthy)
            (expect view-mode :to-be-truthy)
            (expect workspace-bookmark--canonical-view :to-be-truthy))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")
          (expect (length (split-string (portable-bookmarks-missing-repo-spec--marker buffer)
                                        "\\[canonical" t))
                  :to-equal 2)))))

  (it "keeps the marker through a revert after C-x C-q, without putting the buffer back in view-mode"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v)))
          (with-current-buffer buffer
            (read-only-mode -1)
            (revert-buffer t t nil)
            (expect buffer-read-only :to-be nil)
            (expect view-mode :to-be nil)
            (expect workspace-bookmark--canonical-view :to-be-truthy))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")))))

  (it "does not stack the marker when the same canonical copy is viewed twice"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v)
        (let* ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v))
               (marker (portable-bookmarks-missing-repo-spec--marker buffer)))
          (expect (length (split-string marker "\\[canonical" t)) :to-equal 2)))))

  (it "opens a directory record's canonical directory in Dired, marked, without view-mode"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-dir (expand-file-name "sub/" (plist-get world :wt-a)))
             (canonical-dir (expand-file-name "sub/" (plist-get world :repo))))
        (workspace-bookmark-fixtures-set-bookmark a-dir "sub")
        (let ((buffer (workspace-bookmark-fixtures-jump "sub" :workspace "wsbm-mr-c" :choose ?v)))
          (with-current-buffer buffer
            (expect (derived-mode-p 'dired-mode) :to-be-truthy)
            (expect (workspace--same-file-p default-directory canonical-dir)
                    :to-be-truthy)
            (expect buffer-read-only :to-be-truthy)
            (expect view-mode :to-be nil))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical"))
        (expect (workspace-bookmark-fixtures-visits-p a-dir) :to-be nil)
        (expect (bookmark-get-filename "sub") :to-equal a-dir))))

  (it "does not mark the originating buffer when the host does not land"
    ;; The host, given a RECORD, swallows bookmark-error-no-filename and
    ;; returns nil without changing the current buffer.  Forced with a
    ;; handler that does exactly that: the view marking must not touch
    ;; the buffer the user was in.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (origin (generate-new-buffer "wsbm-mr-origin")))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (with-current-buffer origin
          (insert "origin\n")
          (cl-letf (((symbol-function 'bookmark-default-handler)
                     (lambda (_record) nil)))
            (portable-bookmarks-missing-repo-spec--collecting-messages messages
              (expect (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?v)
                      :to-be origin)
              (expect (seq-some (lambda (m) (string-match-p "read-only" m)) messages)
                      :to-be nil)))
          (expect view-mode :to-be nil)
          (expect buffer-read-only :to-be nil)
          (expect workspace-bookmark--canonical-view :to-be nil)
          (expect (portable-bookmarks-missing-repo-spec--marker origin) :to-be nil)))))

  (it "declines when the canonical copy vanished between the resolver and the choice"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((message
               (workspace-bookmark-fixtures-decline
                "readme" :workspace "wsbm-mr-c"
                :choose (lambda (_prompt choices &rest _)
                  (delete-file canonical-path)
                  (assq ?v choices)))))
          (expect message :to-match "vanished")
          (expect message :to-match "nothing opened"))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)))))


(describe "missing-repo choice: cancel"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-mr" portable-bookmarks-missing-repo-spec--remote
                 :holding '("a") :empty '("c")
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "opens nothing, signals quit (never an error), leaves the originating buffer unmarked and says nothing"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored))
             (origin (generate-new-buffer "wsbm-mr-origin")))
        (with-current-buffer origin
          (insert "origin\n")
          (let ((overlays (length (overlays-in (point-min) (point-max)))))
            (portable-bookmarks-missing-repo-spec--collecting-messages messages
              (expect (portable-bookmarks-missing-repo-spec--quits-p
                       (lambda ()
                         (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?c)))
                      :to-be 'quit)
              (expect messages :to-equal nil))
            (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1)
            (expect (current-buffer) :to-be origin)
            (expect (length (overlays-in (point-min) (point-max))) :to-equal overlays)
            (expect (local-variable-p 'bookmark-current-bookmark origin) :to-be nil)
            (expect buffer-read-only :to-be nil)))
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


(describe "missing-repo choice: add worktree here"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-mr" portable-bookmarks-missing-repo-spec--remote
                 :holding '("a") :empty '("c")
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "hands the worktree command the current workspace, its home and :checkout as the source; on ok resolves once more and opens the new worktree's copy"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (home-c (plist-get world :home-c))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored))
             (received nil))
        (spy-on 'workspace-bookmark--resolve :and-call-through)
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (payload)
                     (setq received payload)
                     (workspace-bookmark-fixtures-add-worktree
                      (plist-get world :repo)
                      (expand-file-name "proj" (plist-get payload :home))
                      :branch "c")
                     'ok)))
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?a)))
            (expect (workspace--same-file-p
                     (buffer-file-name buffer)
                     (expand-file-name "proj/README.md" home-c))
                    :to-be-truthy)
            (expect (with-current-buffer buffer view-mode) :to-be nil)
            (expect (with-current-buffer buffer buffer-read-only) :to-be nil)))
        (expect (plist-get received :name) :to-equal "wsbm-mr-c")
        (expect (workspace--same-file-p (plist-get received :home) home-c)
                :to-be-truthy)
        (expect (workspace--same-file-p (plist-get received :source-repo)
                                        (plist-get world :repo))
                :to-be-truthy)
        (expect (plist-get received :context) :to-equal 'bookmark-jump)
        (expect (spy-calls-count 'workspace-bookmark--resolve) :to-equal 2)
        (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1)
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "targets :checkout even when the relpath is absent there, and reports not-on-branch after the add without a second prompt"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (home-c (plist-get world :home-c))
             (received nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (payload)
                     (setq received payload)
                     (workspace-bookmark-fixtures-add-worktree
                      (plist-get world :repo)
                      (expand-file-name "proj" (plist-get payload :home))
                      :branch "c")
                     'ok)))
          (let ((message (workspace-bookmark-fixtures-decline "only-a" :workspace "wsbm-mr-c" :choose ?a)))
            (expect message :to-match "not on this branch")
            (expect message :to-match "after adding a worktree")))
        (expect (workspace--same-file-p (plist-get received :source-repo)
                                        (plist-get world :repo))
                :to-be-truthy)
        (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1)
        (expect (file-directory-p (expand-file-name "proj" home-c)) :to-be-truthy)
        (expect (workspace-bookmark-fixtures-visits-p
                 (expand-file-name "proj/only-a.el" home-c))
                :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil))))

  (it "through the REAL git-worktrees command: no source prompt, no discovery re-run, the worktree made from :checkout"
    ;; Architect on-touch finding 31.  Only magit is stood in for — its
    ;; main-branch lookup, and its worktree creation, which really adds
    ;; the worktree with the fixtures — and the branch prompt, answered
    ;; with its default.  Every source prompt fails the spec.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((repo (plist-get world :repo))
             (home-c (plist-get world :home-c))
             (a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (made nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (spy-on 'jf/workspace--worktree-read-repo :and-call-through)
        (spy-on 'workspace-bookmark-canonical-checkouts-in-view :and-call-through)
        (cl-letf (((symbol-function 'magit-main-branch) (lambda () "main"))
                  ((symbol-function 'magit-worktree-branch)
                   (lambda (dir branch _start)
                     (push (list default-directory dir branch) made)
                     (workspace-bookmark-fixtures-add-worktree
                      default-directory dir :branch branch)))
                  ((symbol-function 'read-string)
                   (lambda (_prompt &optional initial &rest _) initial))
                  ((symbol-function 'completing-read)
                   (lambda (&rest args) (error "Prompted for a source: %S" args)))
                  ((symbol-function 'read-directory-name)
                   (lambda (&rest args) (error "Prompted for a source: %S" args))))
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-mr-c" :choose ?a)))
            (expect (workspace--same-file-p
                     (buffer-file-name buffer)
                     (expand-file-name
                      "README.md"
                      (expand-file-name (file-name-nondirectory (directory-file-name repo))
                                        home-c)))
                    :to-be-truthy)))
        (expect 'jf/workspace--worktree-read-repo :not :to-have-been-called)
        (expect 'workspace-bookmark-canonical-checkouts-in-view :not :to-have-been-called)
        (expect (length made) :to-equal 1)
        (expect (workspace--same-file-p (nth 0 (car made)) repo) :to-be-truthy)
        (expect (nth 2 (car made)) :to-equal "wsbm-mr-c")
        (expect (length workspace-bookmark-fixtures-chooser-prompts) :to-equal 1))))

  (it "re-resolves after the add in a fresh discovery memo, so an enclosing scope's listing cannot hide the new worktree"
    ;; Architect on-touch finding 48.  The enclosing scope lists the
    ;; repository (main clone + A's worktree) before the add; the re-resolve
    ;; after it must see the worktree the add made.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((repo (plist-get world :repo))
             (home-c (plist-get world :home-c))
             (a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (resolve (symbol-function 'workspace-bookmark--resolve))
             (listings nil)
             (plan nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (record workspace)
                     (push (workspace-bookmark--checkouts-of repo) listings)
                     (funcall resolve record workspace)))
                  ((symbol-function 'jf/workspace--add-worktree)
                   (lambda (payload)
                     (workspace-bookmark-fixtures-add-worktree
                      repo (expand-file-name "proj" (plist-get payload :home))
                      :branch "c")
                     'ok)))
          (workspace-bookmark-call-with-discovery-memo
           (lambda ()
             (let* ((record (bookmark-get-bookmark "readme"))
                    (resolution (workspace-bookmark--resolve record "wsbm-mr-c")))
               (expect (plist-get resolution :outcome) :to-be 'missing-repo)
               (setq plan (workspace-bookmark--add-worktree-and-replan
                           record resolution "wsbm-mr-c"))))))
        (expect (length listings) :to-equal 2)
        (expect (length (cadr listings)) :to-equal 2)
        (expect (length (car listings)) :to-equal 3)
        (expect (seq-some (lambda (checkout)
                            (workspace--same-file-p
                             checkout (expand-file-name "proj" home-c)))
                          (car listings))
                :to-be-truthy)
        (expect (car plan) :to-be 'call-through)
        (expect (workspace--same-file-p
                 (bookmark-get-filename (cdr plan))
                 (expand-file-name "proj/README.md" home-c))
                :to-be-truthy))))

  (it "reports (failed . REASON) and opens nothing"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (spy-on 'workspace-bookmark--resolve :and-call-through)
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) (cons 'failed "worktree dir already exists: proj"))))
          (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-mr-c" :choose ?a)))
            (expect message :to-match (regexp-quote "\"readme\""))
            (expect message :to-match "worktree dir already exists: proj")
            (expect message :to-match "nothing opened")))
        (expect (spy-calls-count 'workspace-bookmark--resolve) :to-equal 1)
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil))))

  (it "reports skipped and opens nothing"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (expect (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-mr-c" :choose ?a)
                  :to-match "skipped the add"))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)))))


;;; Unit — the payload handed to the worktree command

(describe "missing-repo choice: the add-worktree payload"

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-fixtures-make-workspace "wsbm-mr-payload"))

  (it "is core's anchor payload for the workspace, with context bookmark-jump, plus :source-repo"
    (let ((home (workspace-bookmark--workspace-home "wsbm-mr-payload")))
      (expect home :to-be-truthy)
      (expect (workspace-bookmark--add-worktree-payload "wsbm-mr-payload" "/src/repo/")
              :to-equal (append (workspace--integration-payload
                                 "wsbm-mr-payload" home 'bookmark-jump)
                                (list :source-repo "/src/repo/")))
      (expect (plist-get (workspace-bookmark--add-worktree-payload "wsbm-mr-payload" "/src/repo/")
                         :sessions-dir)
              :to-equal (workspace--sessions-dir home)))))


;;; Unit — the chooser's own barrier

(describe "missing-repo choice: barrier"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-mr" portable-bookmarks-missing-repo-spec--remote
                 :holding '("a") :empty '("c")
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "declines on an error inside the choice instead of falling through to the stored path"
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (workspace-bookmark-fixtures-signalling 'workspace-bookmark--missing-repo-prompt "Prompt boom"
          (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-mr-c" :choose ?v)))
            (expect message :to-match "Prompt boom")
            (expect message :to-match "nothing opened")))
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines a view whose resolution names no canonical checkout instead of opening the copy unmarked"
    ;; Unreachable from today's resolver (a missing-repo with a :path
    ;; always carries :checkout); forced by dropping :checkout from the
    ;; real resolution, so a producer change cannot open main writable.
    (workspace-bookmark-fixtures-hermetic
      :integrations (portable-bookmarks-missing-repo-spec--git-worktree-registry)
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (resolve (symbol-function 'workspace-bookmark--resolve)))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest args)
                     (let ((resolution (copy-sequence (apply resolve args))))
                       (expect (plist-get resolution :outcome) :to-be 'missing-repo)
                       (expect (plist-get resolution :path) :to-be-truthy)
                       (plist-put resolution :checkout nil)))))
          (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-mr-c" :choose ?v)))
            (expect message :to-match "No canonical checkout")
            (expect message :to-match "nothing opened")))
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)))))


;;; Structural — the section, read off the tangled module

(describe "missing-repo choice: structure"

  (it "opens nothing itself: the section names no file-visiting function, no stored filename, and builds no record copy"
    (let* ((source (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "../portable-bookmarks.el"
                                        portable-bookmarks-missing-repo-spec--this-dir))
                     (buffer-string)))
           (start (string-match-p "(defconst workspace-bookmark--missing-repo-actions" source))
           ;; The section ends where the next one, "Off-workspace", begins:
           ;; its first form declares `workspace--broken-p'.
           (end (string-match-p "(declare-function workspace--broken-p " source))
           (section (and start end (< start end) (substring source start end))))
      (expect section :not :to-be nil)
      (expect section :to-match "read-multiple-choice")
      (expect section :to-match "(view-mode 1)")
      (expect section :to-match "(fboundp 'jf/workspace--add-worktree)")
      (expect section :to-match "workspace-bookmark--plan-to-open")
      (expect section :not :to-match "find-file")
      (expect section :not :to-match "switch-to-buffer")
      (expect section :not :to-match "pop-to-buffer")
      (expect section :not :to-match "bookmark-get-filename")
      (expect section :not :to-match "stored-filename")
      (expect section :not :to-match "(copy-tree ")
      (expect section :not :to-match "bookmark-prop-set")
      (expect source :not :to-match "(require 'magit)")
      (expect source :not :to-match "magit-"))))

(provide 'portable-bookmarks-missing-repo-spec)
;;; portable-bookmarks-missing-repo-spec.el ends here
