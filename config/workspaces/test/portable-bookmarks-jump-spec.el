;;; portable-bookmarks-jump-spec.el --- Jump seam specs -*- lexical-binding: t; -*-

;; Specs for seam 2: the `:around' advice on `bookmark-handle-bookmark'
;; that resolves a stamped record against the current workspace and hands
;; the native handler a substituted COPY.
;;
;; * BEHAVIORAL — real `git' (one repository, a linked worktree in each of
;;   two registered workspace homes), records stamped by the real stamp
;;   seam through `bookmark-set', and jumps driven through the REAL entry
;;   points (`bookmark-jump', `bookmark-jump-other-window'), never the
;;   advice in isolation.  The bookmark file is the fixtures' scratch file.
;;
;; * UNIT — the seam's own error barrier (declining for a stamped record,
;;   stock for an unstamped one), `quit' propagation and the host's
;;   relocate arm, forced with the fixtures' stand-ins scoped to one spec;
;;   the one outcome-to-action mapping, cell by cell, with the openers
;;   replaced by markers; plus the structural half of the copy rule and of
;;   the mapping, read off the tangled module.
;;
;; The "current workspace" is the one boundary mocked: `workspace--current-name'
;; (tabs.el) is bound per jump, because a tab-bar tab is not a thing a batch
;; spec can select.  Everything below it — the registry, discovery, the
;; resolver, the seam, bookmark.el — is real.
;;
;; Registry isolation goes through the fixtures module's describe-level
;; macro `workspace-bookmark-fixtures-with-empty-registry', whose body
;; builds each spec's world inside `before-each' against a fresh table
;; (register/invariant/spec-registry-isolation-via-fixtures); this file
;; never touches `workspace--registry' itself.

(require 'buttercup)
(require 'cl-lib)
(require 'seq)
(require 'bookmark)
(require 'dired)

(defconst portable-bookmarks-jump-spec--this-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this spec file, captured at load time.")

(load (expand-file-name "portable-bookmarks-fixtures.el"
                        portable-bookmarks-jump-spec--this-dir))
(load (expand-file-name "../portable-bookmarks.el"
                        portable-bookmarks-jump-spec--this-dir))

(defvar jf/emacs-dir)


;;; Helpers

(defconst portable-bookmarks-jump-spec--remote "git@example.com:fixtures/jump.git"
  "Remote URL given to the fixture repository.")

(defconst portable-bookmarks-jump-spec--identity "example.com/fixtures/jump"
  "The identity `portable-bookmarks-jump-spec--remote' normalizes to.")

(defun portable-bookmarks-jump-spec--reload-bookmarks ()
  "Round-trip `bookmark-alist' through the scratch bookmark file.
Saves with `bookmark-save', empties the alist, loads the file back."
  (let ((inhibit-message t))
    (bookmark-save)
    (setq bookmark-alist nil)
    (bookmark-load bookmark-default-file t t)))

;;; A native handler of our own making, for the handler-preservation spec.
;;; A symbol, not a closure, so the record survives bookmark-save / -load.

(defvar portable-bookmarks-jump-spec--handled nil
  "Records `portable-bookmarks-jump-spec--recording-handler' received, newest first.")

(defun portable-bookmarks-jump-spec--recording-handler (record)
  "Record RECORD, then do what the default handler does with it."
  (push record portable-bookmarks-jump-spec--handled)
  (bookmark-default-handler record))


;;; Behavioral — resolved jumps through the real jump path

(describe "jump seam: resolved jumps (real git, real jump path)"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-jump" portable-bookmarks-jump-spec--remote
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "opens the current workspace's copy at the recorded position, and the stored record survives a save round trip unchanged"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored)))
        (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                :to-equal "README.md")
        (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b")))
          (expect (workspace--same-file-p (buffer-file-name buffer) b-path) :to-be-truthy)
          ;; Not vacuous: A's copy exists, so nil means "a different file".
          (expect (file-exists-p a-path) :to-be-truthy)
          (expect (workspace--same-file-p (buffer-file-name buffer) a-path) :to-be nil)
          (expect (with-current-buffer buffer (point))
                  :to-equal workspace-bookmark-fixtures-jump-position))
        ;; The regression guard: in memory, then through the file.  A
        ;; shared cons would read back the same in memory; only the round
        ;; trip catches a write-through.
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        (portable-bookmarks-jump-spec--reload-bookmarks)
        (expect (bookmark-get-filename "readme") :to-equal a-path)
        (expect (bookmark-prop-get "readme" workspace-bookmark-relpath-property)
                :to-equal "README.md"))))

  (it "opens whichever workspace is current, from the same stored record"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (dolist (side (list (cons "wsbm-jump-b" (plist-get world :wt-b))
                            (cons "wsbm-jump-a" (plist-get world :wt-a))))
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace (car side))))
            (expect (workspace--same-file-p (buffer-file-name buffer)
                                            (expand-file-name "README.md" (cdr side)))
                    :to-be-truthy)))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "hands the native handler a copy that shares no cons with the stored record"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (real (symbol-function 'bookmark-default-handler))
             (received nil))
        (cl-letf (((symbol-function 'bookmark-default-handler)
                   (lambda (record) (push record received) (funcall real record))))
          (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b"))
        (expect (length received) :to-equal 1)
        (let ((copy (car received)))
          (expect copy :not :to-be stored)
          (expect (bookmark-name-from-full-record copy) :to-equal "readme")
          (expect (bookmark-get-filename copy) :to-equal b-path)
          (expect (bookmark-get-handler copy) :to-equal (bookmark-get-handler stored))
          ;; No cons of the copy's alist is a cons of the stored alist.
          (expect (cl-intersection (cdr copy) (cdr stored) :test #'eq) :to-be nil)
          (expect (assq 'filename (cdr copy))
                  :not :to-be (assq 'filename (cdr stored))))
        (expect (bookmark-get-filename stored) :to-equal a-path)
        (expect (bookmark-get-bookmark "readme") :to-be stored))))

  (it "opens a directory record in the current workspace's checkout (default handler visiting a directory: Dired)"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-dir (expand-file-name "sub/" (plist-get world :wt-a)))
             (b-dir (expand-file-name "sub/" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-dir "sub")))
        (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                :to-equal "sub/")
        (let ((buffer (workspace-bookmark-fixtures-jump "sub" :workspace "wsbm-jump-b")))
          (with-current-buffer buffer
            (expect (derived-mode-p 'dired-mode) :to-be-truthy)
            (expect (workspace--same-file-p default-directory b-dir) :to-be-truthy)
            ;; Not vacuous: A's directory exists.
            (expect (file-directory-p a-dir) :to-be-truthy)
            (expect (workspace--same-file-p default-directory a-dir) :to-be nil)))
        (expect (bookmark-get-filename "sub") :to-equal a-dir)
        (portable-bookmarks-jump-spec--reload-bookmarks)
        (expect (bookmark-get-filename "sub") :to-equal a-dir))))

  (it "hands a record's own handler the substituted copy, handler intact, stored record untouched"
    ;; Emacs 30 dired records carry no handler; this is the case org,
    ;; pdf-view and bufferlo present: a record whose `handler' is set.
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme")))
        (bookmark-prop-set stored 'handler
                           #'portable-bookmarks-jump-spec--recording-handler)
        (let ((snapshot (copy-tree stored)))
          (setq portable-bookmarks-jump-spec--handled nil)
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b")))
            (expect (workspace--same-file-p (buffer-file-name buffer) b-path) :to-be-truthy))
          (expect (length portable-bookmarks-jump-spec--handled) :to-equal 1)
          (let ((copy (car portable-bookmarks-jump-spec--handled)))
            (expect copy :not :to-be stored)
            (expect (bookmark-get-filename copy) :to-equal b-path)
            (expect (bookmark-get-handler copy)
                    :to-be #'portable-bookmarks-jump-spec--recording-handler))
          (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
          (portable-bookmarks-jump-spec--reload-bookmarks)
          (expect (bookmark-get-filename "readme") :to-equal a-path)
          (expect (bookmark-get-handler "readme")
                  :to-be #'portable-bookmarks-jump-spec--recording-handler)))))

  (it "resolves through bookmark-jump-other-window too (the choke point is bookmark-handle-bookmark)"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b))))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((buffer (workspace-bookmark-fixtures-jump
                       "readme" :workspace "wsbm-jump-b" :jumper #'bookmark-jump-other-window)))
          (expect (workspace--same-file-p (buffer-file-name buffer) b-path) :to-be-truthy))
        (expect (workspace-bookmark-fixtures-visits-p b-path) :to-be-truthy)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "sets bookmark-current-bookmark after a substituted jump by name, as the host does for a plain one"
    ;; Driven at `bookmark-handle-bookmark' with a STRING on purpose: in
    ;; this configuration bufferlo's advice on `bookmark--jump-via' hands
    ;; the host a record for every jump, so `bookmark-jump' can never show
    ;; the string path.  `bookmark-current-bookmark' is buffer-local; each
    ;; visited buffer is fresh (the buffer `bookmark-set' ran in was
    ;; killed), so a value can only have come from the jump.
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (dir (workspace-bookmark-fixtures-temp-dir "wsbm-jump-plain-"))
             (plain-path (expand-file-name "notes.txt" dir)))
        (with-temp-file plain-path (insert workspace-bookmark-fixtures-jump-contents))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (workspace-bookmark-fixtures-set-bookmark plain-path "plain")
        (dolist (name '("plain" "readme"))
          (let ((buffer (workspace-bookmark-fixtures-jump
                         name :workspace "wsbm-jump-b" :jumper #'bookmark-handle-bookmark)))
            (expect (cons name (buffer-local-value 'bookmark-current-bookmark buffer))
                    :to-equal (cons name name))
            ;; The stamped one must ALSO have been substituted: parity
            ;; alone is something the stock host satisfies by itself.
            (when (equal name "readme")
              (expect (workspace--same-file-p (buffer-file-name buffer)
                                              (expand-file-name "README.md" (plist-get world :wt-b)))
                      :to-be-truthy))))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "leaves an unstamped record alone: it opens its stored absolute path"
    (workspace-bookmark-fixtures-hermetic
      (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-jump-plain-"))
             (path (expand-file-name "notes.txt" dir)))
        (with-temp-file path (insert workspace-bookmark-fixtures-jump-contents))
        (let* ((stored (workspace-bookmark-fixtures-set-bookmark path "plain"))
               (snapshot (copy-tree stored)))
          (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                  :to-be nil)
          (let ((buffer (workspace-bookmark-fixtures-jump "plain" :workspace "wsbm-jump-b")))
            (expect (workspace--same-file-p (buffer-file-name buffer) path) :to-be-truthy)
            (expect (with-current-buffer buffer (point))
                    :to-equal workspace-bookmark-fixtures-jump-position))
          (expect (bookmark-get-bookmark "plain") :to-equal snapshot)))))

  (it "leaves an unstamped record alone with no current workspace too: it opens its stored path, stock behaviour"
    ;; arch-on-touch-1790604160-11.  With no workspace a STAMPED record is
    ;; handed off-workspace; an UNSTAMPED one is not, and is mapped in
    ;; context `in-workspace', whose non-portable cell is the only one
    ;; that calls through.  Mapped in `none' instead it would decline, so
    ;; every unstamped bookmark jumped from a tab that is not a workspace
    ;; would open nothing.
    (workspace-bookmark-fixtures-hermetic
      (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-jump-plain-"))
             (path (expand-file-name "notes.txt" dir)))
        (with-temp-file path (insert workspace-bookmark-fixtures-jump-contents))
        (let* ((stored (workspace-bookmark-fixtures-set-bookmark path "plain"))
               (snapshot (copy-tree stored)))
          (expect (workspace-bookmark--stamped-coordinates stored) :to-be nil)
          (let ((buffer (workspace-bookmark-fixtures-jump "plain" :workspace nil)))
            (expect (workspace--same-file-p (buffer-file-name buffer) path) :to-be-truthy)
            (expect (with-current-buffer buffer (point))
                    :to-equal workspace-bookmark-fixtures-jump-position))
          (expect (bookmark-get-bookmark "plain") :to-equal snapshot)))))

  (it "hands a stamped record off-workspace to the workspace prompt, restores the choice, then resolves against it"
    ;; The off-workspace flow's own specs (real tabs, real restore, the
    ;; deferred display of the jump's buffer) are
    ;; portable-bookmarks-off-workspace-spec.el.  Here the two core calls
    ;; are stubbed so the seam's shape is what is under test: the prompt
    ;; is asked BEFORE the resolver, the chosen workspace is restored,
    ;; and resolution runs against it — the record is NOT passed through
    ;; to its stored path.  The prompt guard is lifted (batch has no one
    ;; to ask); the finish step this leaves queued is cancelled by the
    ;; fixtures' teardown, which the next spec checks.
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored))
             (restored nil)
             (events nil)
             (resolve (symbol-function 'workspace-bookmark--resolve)))
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest args) (push 'resolve events) (apply resolve args)))
                  ((symbol-function 'workspace-bookmark--can-prompt-p)
                   (lambda () t))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) (push 'prompt events) "wsbm-jump-b"))
                  ((symbol-function 'workspace-restore)
                   (lambda (name) (push 'restore events) (setq restored name) name)))
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace nil)))
            (expect (workspace--same-file-p (buffer-file-name buffer) b-path) :to-be-truthy)
            ;; Not vacuous: A's copy exists.
            (expect (file-exists-p a-path) :to-be-truthy)
            (expect (workspace--same-file-p (buffer-file-name buffer) a-path) :to-be nil)))
        (expect restored :to-equal "wsbm-jump-b")
        (expect (reverse events) :to-equal '(prompt restore resolve))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        ;; The jump left its finish step queued; nothing here drains it.
        (expect (seq-some (lambda (timer)
                            (eq (timer--function timer)
                                #'workspace-bookmark--finish-restore))
                          timer-list)
                :to-be-truthy))))

  (it "leaves nothing of the off-workspace jump pending for the next spec: the fixtures' teardown cancelled the finish step and cleared the jump state"
    ;; Runs after the spec above, whose teardown has run by now.
    (expect (seq-some (lambda (timer)
                        (eq (timer--function timer)
                            #'workspace-bookmark--finish-restore))
                      timer-list)
            :to-be nil)
    (expect workspace-bookmark--awaiting-jump-target :to-be nil)
    (expect workspace-bookmark--jump-target :to-be nil)))


;;; Behavioral — outcomes that open nothing

(describe "jump seam: outcomes that open nothing"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-jump" portable-bookmarks-jump-spec--remote
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "declines not-on-branch: no buffer visits the missing path, nor the stored one"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (b-path (expand-file-name "only-a.el" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "only-a"))
             (snapshot (copy-tree stored)))
        (expect (file-exists-p b-path) :to-be nil)
        (let ((message (workspace-bookmark-fixtures-decline "only-a" :workspace "wsbm-jump-b")))
          (expect message :to-match (regexp-quote "\"only-a\""))
          (expect message :to-match (regexp-quote "only-a.el"))
          (expect message :to-match (regexp-quote portable-bookmarks-jump-spec--identity)))
        (expect (workspace-bookmark-fixtures-visits-p b-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (file-exists-p b-path) :to-be nil)
        (expect (bookmark-get-bookmark "only-a") :to-equal snapshot))))

  (it "declines ambiguous: two checkouts in the workspace are reported, neither chosen"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (wt-b (plist-get world :wt-b))
             (wt-b2 (workspace-bookmark-fixtures-add-worktree
                     (plist-get world :repo)
                     (expand-file-name "proj2" (plist-get world :home-b))
                     :branch "b2")))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-jump-b")))
          (expect message :to-match "2 checkouts")
          (expect message :to-match (regexp-quote (abbreviate-file-name wt-b)))
          (expect message :to-match (regexp-quote (abbreviate-file-name wt-b2))))
        (dolist (path (list a-path
                            (expand-file-name "README.md" wt-b)
                            (expand-file-name "README.md" wt-b2)))
          (expect (workspace-bookmark-fixtures-visits-p path) :to-be nil))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines no-clone: no checkout of the identity in view"
    (workspace-bookmark-fixtures-hermetic
      (let* ((elsewhere (workspace-bookmark-fixtures-make-repo
                         :remote "git@example.com:fixtures/elsewhere.git"))
             (path (expand-file-name "README.md" elsewhere))
             (stored (workspace-bookmark-fixtures-set-bookmark path "elsewhere")))
        (expect (bookmark-prop-get stored workspace-bookmark-repo-property)
                :to-equal "example.com/fixtures/elsewhere")
        (let ((message (workspace-bookmark-fixtures-decline "elsewhere" :workspace "wsbm-jump-b")))
          (expect message :to-match "no checkout of example.com/fixtures/elsewhere in view"))
        (expect (workspace-bookmark-fixtures-visits-p path) :to-be nil)
        (expect (bookmark-get-filename "elsewhere") :to-equal path))))

  (it "hands missing-repo to the bounded choice, opening neither the canonical copy nor the stored one until the user chooses"
    ;; The choice itself is portable-bookmarks-missing-repo-spec.el's.
    ;; Here: the seam prompts instead of declining or opening, the prompt
    ;; is the missing-repo one, and cancel ends the jump with `quit'.
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (prompts nil)
             (quit-p nil))
        (workspace-bookmark-fixtures-make-workspace "wsbm-jump-c")
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p)
                   (lambda () t))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (prompt choices &rest _)
                     (push prompt prompts)
                     (assq ?c choices))))
          (condition-case nil
              (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-c")
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect (length prompts) :to-equal 1)
        (expect (car prompts) :to-match "holds no checkout")
        (expect (car prompts) :to-match
                (regexp-quote (abbreviate-file-name (plist-get world :repo))))
        (expect (workspace-bookmark-fixtures-visits-p canonical-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines with user-error, never bookmark-error-no-filename, and never offers to relocate"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (signalled nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "only-a")
        (condition-case err
            (workspace-bookmark-fixtures-jump "only-a" :workspace "wsbm-jump-b")
          (bookmark-errors (setq signalled (car err)))
          (user-error (setq signalled (car err))))
        (expect signalled :to-be 'user-error)))))


;;; Unit — the barrier, quit, and the host's contract

(describe "jump seam: barrier and host contract"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (workspace-bookmark-fixtures-jump-world
                 "wsbm-jump" portable-bookmarks-jump-spec--remote
                 :files '(("sub/deep.el" . "(deep)\n")))))

  (it "an error inside the seam's decision for a STAMPED record opens nothing: the resolver signals, the jump declines"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored)))
        (expect (workspace-bookmark--stamped-coordinates stored) :to-be-truthy)
        (workspace-bookmark-fixtures-signalling 'workspace-bookmark--resolve "Boom"
          (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-jump-b")))
            (expect message :to-match "jump decision failed")
            (expect message :to-match "Boom")
            (expect message :to-match "nothing opened")))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p b-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "an error inside the seam's decision for a STAMPED record opens nothing: the copy step signals, the jump declines"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored)))
        (expect (workspace-bookmark--stamped-coordinates stored) :to-be-truthy)
        (workspace-bookmark-fixtures-signalling 'workspace-bookmark--substituted-record "Boom"
          (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-jump-b")))
            (expect message :to-match "jump decision failed")
            (expect message :to-match "Boom")
            (expect message :to-match "nothing opened")))
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p b-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "keeps stock behaviour for an UNSTAMPED record when the resolver signals: the stored location opens, the record is untouched"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (workspace-bookmark-fixtures-strip-stamp
                      (workspace-bookmark-fixtures-set-bookmark a-path "readme")))
             (snapshot (copy-tree stored)))
        (expect (workspace-bookmark--stamped-coordinates stored) :to-be nil)
        (workspace-bookmark-fixtures-signalling 'workspace-bookmark--resolve "Boom"
          (let ((buffer (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b")))
            (expect (workspace--same-file-p (buffer-file-name buffer) a-path) :to-be-truthy)
            (expect (with-current-buffer buffer (point))
                    :to-equal workspace-bookmark-fixtures-jump-position)))
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "lets quit propagate through the barrier"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (quit-p nil))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest _) (signal 'quit nil))))
          (condition-case nil
              (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b")
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p
                 (expand-file-name "README.md" (plist-get world :wt-b)))
                :to-be nil))))

  (it "does not catch the handler's own errors, and calls the host exactly once"
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (calls 0))
        (workspace-bookmark-fixtures-set-bookmark a-path "readme")
        (cl-letf (((symbol-function 'bookmark-default-handler)
                   (lambda (&rest _) (cl-incf calls) (error "Handler boom"))))
          (expect (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-b")
                  :to-throw 'error '("Handler boom")))
        (expect calls :to-equal 1))))

  (it "declines when a resolved path vanished before the visit, touching neither the originating buffer nor the stored record"
    ;; The seam pre-flights the resolved :path.  Without it the host, given a
    ;; RECORD, swallows bookmark-error-no-filename and returns nil, and
    ;; bookmark--jump-via then fringe-marks and hooks the buffer the user
    ;; was already in.  Driven from a scratch ORIGIN buffer so that tail
    ;; would be visible.
    (workspace-bookmark-fixtures-hermetic
      (let* ((wt-b (plist-get world :wt-b))
             (a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (vanished (expand-file-name "vanished.el" wt-b))
             (stored (workspace-bookmark-fixtures-set-bookmark a-path "readme"))
             (snapshot (copy-tree stored))
             (origin (generate-new-buffer "wsbm-jump-origin")))
        (with-current-buffer origin
          (insert "origin\n")
          (let ((overlays (length (overlays-in (point-min) (point-max)))))
            (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                       (lambda (&rest _)
                         (workspace-bookmark--outcome
                          'resolved :path vanished
                          :identity portable-bookmarks-jump-spec--identity
                          :relpath "vanished.el" :checkout wt-b))))
              (let ((message (workspace-bookmark-fixtures-decline "readme" :workspace "wsbm-jump-b")))
                (expect message :to-match "vanished.el")
                (expect message :to-match "nothing opened")))
            (expect (current-buffer) :to-be origin)
            (expect (length (overlays-in (point-min) (point-max))) :to-equal overlays)
            (expect (local-variable-p 'bookmark-current-bookmark origin) :to-be nil)))
        (expect (file-exists-p vanished) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p vanished) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "declines naming the path, rather than returning silently or signalling a type error"
    ;; Scenario: portable-bookmarks § A resolved file that cannot be read is reported, not skipped
    ;; Real git, real jump: the current workspace's copy exists at mode 000,
    ;; so the resolver answers `resolved' and only the seam's readability
    ;; pre-flight stands between the host and a silent nil.
    (workspace-bookmark-fixtures-hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (modes (file-modes a-path))
             (origin (generate-new-buffer "wsbm-unr-origin"))
             (outcome nil)
             (still-origin nil))
        (workspace-bookmark-fixtures-set-bookmark b-path "readme")
        (unwind-protect
            (progn
              (set-file-modes a-path #o000)
              (expect (file-exists-p a-path) :to-be-truthy)
              (expect (file-readable-p a-path) :to-be nil)
              (with-current-buffer origin
                (setq outcome
                      (condition-case err
                          (progn
                            (workspace-bookmark-fixtures-jump "readme" :workspace "wsbm-jump-a")
                            (list 'returned
                                  :current (buffer-name (current-buffer))
                                  :visits-a (workspace-bookmark-fixtures-visits-p a-path)))
                        (user-error (list 'declined (cadr err)))
                        (error (list 'error (error-message-string err)))))
                (setq still-origin (eq (current-buffer) origin))))
          (set-file-modes a-path modes))
        (expect (car outcome) :to-be 'declined)
        (expect (cadr outcome) :to-match (regexp-quote a-path))
        (expect (cadr outcome) :to-match "nothing opened")
        (expect (cadr outcome) :not :to-match "vanished")
        (expect still-origin :to-be t)
        (expect (workspace-bookmark-fixtures-visits-p a-path) :to-be nil)
        (expect (workspace-bookmark-fixtures-visits-p b-path) :to-be nil)
        (kill-buffer origin)))))


;;; Unit — the one outcome-to-action mapping, cell by cell

(defconst portable-bookmarks-jump-spec--contexts '(in-workspace none after-add)
  "The three contexts `workspace-bookmark--plan-for-outcome' has a column for.")

(defconst portable-bookmarks-jump-spec--matrix
  '((non-portable  original decline decline)
    (resolved      copy     decline copy)
    (missing-repo  offer    offer   decline)
    (not-on-branch decline  decline decline)
    (ambiguous     decline  decline decline)
    (no-clone      decline  decline decline))
  "The expected plan per (OUTCOME IN-WORKSPACE NONE AFTER-ADD) row.
`original' is (call-through . ORIGINAL); `copy' a call-through of what
`workspace-bookmark--plan-to-open' returned; `offer' what
`workspace-bookmark--offer-missing-repo' returned; `decline' a
\(decline . MESSAGE) naming the bookmark.")

(defun portable-bookmarks-jump-spec--cell-resolution (outcome)
  "A resolution plist for OUTCOME with every key a consumer might read."
  (workspace-bookmark--outcome
   outcome
   :path (and (memq outcome '(resolved missing-repo non-portable)) "/nowhere/checkout/cell.el")
   :identity (and (not (eq outcome 'non-portable)) "example.com/fixtures/cell")
   :relpath (and (not (eq outcome 'non-portable)) "cell.el")
   :candidates (and (eq outcome 'ambiguous) '("/nowhere/one/" "/nowhere/two/"))
   :checkout (and (memq outcome '(resolved not-on-branch missing-repo)) "/nowhere/checkout/")))

(describe "jump seam: one outcome-to-action mapping"

  (it "returns a known plan variant for every outcome x context, and hands the host the ORIGINAL argument in exactly one cell"
    (let ((record (list "cell" (cons 'filename "/nowhere/stored/cell.el")))
          (original 'the-original-argument)
          (cells nil))
      (expect (mapcar #'car portable-bookmarks-jump-spec--matrix)
              :to-have-same-items-as workspace-bookmark--valid-outcomes)
      (workspace-bookmark-fixtures-with-marker-plans
        (dolist (row portable-bookmarks-jump-spec--matrix)
          (cl-loop for context in portable-bookmarks-jump-spec--contexts
                   for want in (cdr row)
                   do (let* ((outcome (car row))
                             (plan (workspace-bookmark--plan-for-outcome
                                    record (portable-bookmarks-jump-spec--cell-resolution outcome)
                                    context original)))
                        (push (list outcome context plan) cells)
                        (expect (memq (car plan) '(call-through decline view cancel))
                                :to-be-truthy)
                        (pcase want
                          ('original
                           (expect plan :to-equal (cons 'call-through original)))
                          ('copy
                           (expect plan :to-equal
                                   (cons 'call-through workspace-bookmark-fixtures-copy-marker)))
                          ('offer
                           (expect (car plan) :to-be 'view)
                           (expect (cadr plan) :to-be workspace-bookmark-fixtures-offer-marker))
                          ('decline
                           (expect (car plan) :to-be 'decline)
                           (expect (cdr plan) :to-match "Bookmark \"cell\"")
                           (expect (cdr plan) :to-match "nothing opened\\|none chosen")
                           (when (eq context 'after-add)
                             (expect (cdr plan) :to-match "after adding a worktree"))
                           (when (eq context 'none)
                             (expect (cdr plan) :not :to-match "after adding")))
                          (_ (error "Unknown expectation %S" want)))))))
      (expect (length cells) :to-equal 18)
      (let ((originals (seq-filter (lambda (cell)
                                     (equal (nth 2 cell) (cons 'call-through original)))
                                   cells)))
        (expect (length originals) :to-equal 1)
        (expect (car originals) :to-equal (list 'non-portable 'in-workspace
                                                (cons 'call-through original))))))

  (it "signals for an outcome outside the closed set, in every context"
    (let ((record (list "cell" (cons 'filename "/nowhere/stored/cell.el"))))
      (workspace-bookmark-fixtures-with-marker-plans
        (dolist (context portable-bookmarks-jump-spec--contexts)
          (expect (workspace-bookmark--plan-for-outcome
                   record (list :outcome 'seventh :path "/nowhere/stored/cell.el")
                   context 'the-original-argument)
                  :to-throw 'error)))))

  (it "signals for a context it has no column for, whatever the outcome"
    (let ((record (list "cell" (cons 'filename "/nowhere/stored/cell.el"))))
      (workspace-bookmark-fixtures-with-marker-plans
        (dolist (outcome workspace-bookmark--valid-outcomes)
          (expect (workspace-bookmark--plan-for-outcome
                   record (portable-bookmarks-jump-spec--cell-resolution outcome)
                   'elsewhere 'the-original-argument)
                  :to-throw 'error)))))

  (it "is the only outcome dispatch in the seam, missing-repo and off-workspace sections, and every planner calls it"
    (let* ((source (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "../portable-bookmarks.el"
                                        portable-bookmarks-jump-spec--this-dir))
                     (buffer-string)))
           (start (string-match-p "(defun workspace-bookmark--substituted-record " source))
           ;; The three sections end where "Checkout discovery" begins:
           ;; its first form declares `workspace--registry'.
           (end (string-match-p "(defvar workspace--registry)" source))
           (section (and start end (< start end) (substring source start end)))
           (defun-of (lambda (name)
                       (let* ((from (string-match-p (concat "(defun " (regexp-quote name) " ") section))
                              (to (and from (string-match-p "\n(\\(defun\\|defconst\\|defvar\\|declare-function\\) "
                                                            section (1+ from)))))
                         (expect from :not :to-be nil)
                         (substring section from to)))))
      (expect section :not :to-be nil)
      ;; One dispatch over the outcome in these sections: the mapping.
      (expect (length (seq-filter (lambda (line) (string-match-p "(pcase (plist-get" line))
                                  (split-string section "\n")))
              :to-equal 1)
      (expect (funcall defun-of "workspace-bookmark--plan-for-outcome")
              :to-match "(pcase (plist-get resolution :outcome)")
      ;; Every planner calls it and reads no outcome of its own.
      (dolist (planner '("workspace-bookmark--jump-plan"
                         "workspace-bookmark--no-workspace-plan"
                         "workspace-bookmark--add-worktree-and-replan"))
        (let ((body (funcall defun-of planner)))
          (expect body :to-match "(workspace-bookmark--plan-for-outcome")
          (expect body :not :to-match ":outcome)")
          (expect body :not :to-match "(pcase (plist-get")))
      ;; The two message functions read the constant, not a list of their own.
      (dolist (reader '("workspace-bookmark--decline-message"
                        "workspace-bookmark--after-add-message"))
        (let ((body (funcall defun-of reader)))
          (expect body :to-match "(memq outcome workspace-bookmark--declining-outcomes)")
          (expect body :not :to-match "'(not-on-branch"))))))


;;; Structural — the copy rule, read off the tangled module

(describe "jump seam: structure"

  (it "builds the substituted record with copy-tree at exactly one site, and sets no handler"
    (let ((source (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "../portable-bookmarks.el"
                                       portable-bookmarks-jump-spec--this-dir))
                    (buffer-string))))
      (expect (length (seq-filter (lambda (line) (string-match-p "(copy-tree " line))
                                  (split-string source "\n")))
              :to-equal 1)
      (expect source :not :to-match "(copy-alist ")
      (expect source :not :to-match "(copy-sequence ")
      (expect source :not :to-match "bookmark-prop-set[^\n]*'handler")
      (expect source :not :to-match "advice-add 'bookmark-jump"))))

(provide 'portable-bookmarks-jump-spec)
;;; portable-bookmarks-jump-spec.el ends here
