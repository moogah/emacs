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
;; * UNIT — the seam's own error barrier, `quit' propagation and the host's
;;   relocate arm, forced with `cl-letf' scoped to one spec; plus the
;;   structural half of the copy rule, read off the tangled module.
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

;; Owned by tabs.el; see portable-bookmarks-discovery-spec.el for why this
;; stand-in exists.  Under the harness the real definition is bound.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))


;;; Helpers

(defconst portable-bookmarks-jump-spec--remote "git@example.com:fixtures/jump.git"
  "Remote URL given to the fixture repository.")

(defconst portable-bookmarks-jump-spec--identity "example.com/fixtures/jump"
  "The identity `portable-bookmarks-jump-spec--remote' normalizes to.")

(defconst portable-bookmarks-jump-spec--contents "line one\nline two\nline three\n"
  "Contents of the seeded README, identical in every checkout.")

(defconst portable-bookmarks-jump-spec--position 10
  "A position inside `portable-bookmarks-jump-spec--contents' (on line 2).")

(defmacro portable-bookmarks-jump-spec--hermetic (&rest body)
  "Run one spec's BODY with nothing but the fixtures' world in view.

`jf/emacs-dir' is nil so the configuration checkout running the suite is
not a canonical checkout in view; the identity memo is cleared on exit;
every buffer BODY leaves behind is killed before the `after-each'
teardown deletes the directories those buffers visit.  Registry
isolation is NOT this macro's business: the describe-level
`workspace-bookmark-fixtures-with-empty-registry' owns it."
  (declare (indent 0))
  `(let ((jf/emacs-dir nil)
         (before (buffer-list)))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (workspace-bookmark-clear-identity-cache))))

(defun portable-bookmarks-jump-spec--world ()
  "Build the standard world and return it as a plist.

Called from the fixtures macro's `before-each' body, so every spec gets
a fresh world against an empty registry.  One repository with a remote,
seeded with README.md and sub/deep.el on `main'; workspace A
\(\"wsbm-jump-a\") and workspace B (\"wsbm-jump-b\"), each holding a
linked worktree of it under its home.  Keys: :repo :home-a :home-b
:wt-a :wt-b."
  (let* ((repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-jump-spec--remote
                :contents portable-bookmarks-jump-spec--contents))
         (home-a (workspace-bookmark-fixtures-make-workspace "wsbm-jump-a"))
         (home-b (workspace-bookmark-fixtures-make-workspace "wsbm-jump-b")))
    (workspace-bookmark-fixtures-commit-file repo "sub/deep.el" "(deep)\n")
    (list :repo repo :home-a home-a :home-b home-b
          :wt-a (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "proj" home-a) :branch "a")
          :wt-b (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "proj" home-b) :branch "b"))))

(defun portable-bookmarks-jump-spec--set (path name)
  "Set bookmark NAME on PATH through `bookmark-set'; return the stored record.

PATH is visited (a directory in Dired), point is put at
`portable-bookmarks-jump-spec--position' in a file, the bookmark is set
by the real API — so the real stamp seam runs — and the buffer is killed
so a later jump has to visit PATH afresh."
  (let ((buffer (if (file-directory-p path)
                    (dired-noselect path)
                  (find-file-noselect path))))
    (with-current-buffer buffer
      (unless (file-directory-p path)
        (goto-char portable-bookmarks-jump-spec--position))
      (let ((inhibit-message t))
        (bookmark-set name)))
    (kill-buffer buffer)
    (bookmark-get-bookmark name)))

(defun portable-bookmarks-jump-spec--jump (name workspace &optional jumper)
  "Jump to bookmark NAME with WORKSPACE current; return the buffer left current.

JUMPER defaults to `bookmark-jump'.  WORKSPACE (a name or nil) is what
`workspace--current-name' answers for the duration.  Any prompt during
the jump fails the spec: the seam must neither ask nor let the host's
relocate offer through."
  (cl-letf (((symbol-function 'workspace--current-name)
             (lambda () workspace))
            ((symbol-function 'y-or-n-p)
             (lambda (&rest args) (error "Prompted: %S" args)))
            ((symbol-function 'yes-or-no-p)
             (lambda (&rest args) (error "Prompted: %S" args))))
    (funcall (or jumper #'bookmark-jump) name)
    (current-buffer)))

(defun portable-bookmarks-jump-spec--decline (name workspace)
  "Jump to NAME with WORKSPACE current, expecting a `user-error'.
Returns the error's message.  Fails the spec if nothing was signalled."
  (let ((message
         (condition-case err
             (progn (portable-bookmarks-jump-spec--jump name workspace) nil)
           (user-error (cadr err)))))
    (expect message :not :to-be nil)
    message))

(defun portable-bookmarks-jump-spec--reload-bookmarks ()
  "Round-trip `bookmark-alist' through the scratch bookmark file.
Saves with `bookmark-save', empties the alist, loads the file back."
  (let ((inhibit-message t))
    (bookmark-save)
    (setq bookmark-alist nil)
    (bookmark-load bookmark-default-file t t)))

(defun portable-bookmarks-jump-spec--visits-p (path)
  "Non-nil when some live buffer visits PATH (a file or a directory)."
  (and (or (find-buffer-visiting path)
           (and (file-directory-p path)
                (seq-some (lambda (buffer)
                            (with-current-buffer buffer
                              (and (derived-mode-p 'dired-mode)
                                   (file-equal-p default-directory path))))
                          (buffer-list))))
       t))


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
    (setq world (portable-bookmarks-jump-spec--world)))

  (it "opens the current workspace's copy at the recorded position, and the stored record survives a save round trip unchanged"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                :to-equal "README.md")
        (let ((buffer (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")))
          (expect (file-equal-p (buffer-file-name buffer) b-path) :to-be-truthy)
          (expect (file-equal-p (buffer-file-name buffer) a-path) :to-be nil)
          (expect (with-current-buffer buffer (point))
                  :to-equal portable-bookmarks-jump-spec--position))
        ;; The regression guard: in memory, then through the file.  A
        ;; shared cons would read back the same in memory; only the round
        ;; trip catches a write-through.
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        (portable-bookmarks-jump-spec--reload-bookmarks)
        (expect (bookmark-get-filename "readme") :to-equal a-path)
        (expect (bookmark-prop-get "readme" workspace-bookmark-relpath-property)
                :to-equal "README.md"))))

  (it "opens whichever workspace is current, from the same stored record"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (dolist (side (list (cons "wsbm-jump-b" (plist-get world :wt-b))
                            (cons "wsbm-jump-a" (plist-get world :wt-a))))
          (let ((buffer (portable-bookmarks-jump-spec--jump "readme" (car side))))
            (expect (file-equal-p (buffer-file-name buffer)
                                  (expand-file-name "README.md" (cdr side)))
                    :to-be-truthy)))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "hands the native handler a copy that shares no cons with the stored record"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
             (real (symbol-function 'bookmark-default-handler))
             (received nil))
        (cl-letf (((symbol-function 'bookmark-default-handler)
                   (lambda (record) (push record received) (funcall real record))))
          (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b"))
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
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-dir (expand-file-name "sub/" (plist-get world :wt-a)))
             (b-dir (expand-file-name "sub/" (plist-get world :wt-b)))
             (stored (portable-bookmarks-jump-spec--set a-dir "sub")))
        (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                :to-equal "sub/")
        (let ((buffer (portable-bookmarks-jump-spec--jump "sub" "wsbm-jump-b")))
          (with-current-buffer buffer
            (expect (derived-mode-p 'dired-mode) :to-be-truthy)
            (expect (file-equal-p default-directory b-dir) :to-be-truthy)
            (expect (file-equal-p default-directory a-dir) :to-be nil)))
        (expect (bookmark-get-filename "sub") :to-equal a-dir)
        (portable-bookmarks-jump-spec--reload-bookmarks)
        (expect (bookmark-get-filename "sub") :to-equal a-dir))))

  (it "hands a record's own handler the substituted copy, handler intact, stored record untouched"
    ;; Emacs 30 dired records carry no handler; this is the case org,
    ;; pdf-view and bufferlo present: a record whose `handler' is set.
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme")))
        (bookmark-prop-set stored 'handler
                           #'portable-bookmarks-jump-spec--recording-handler)
        (let ((snapshot (copy-tree stored)))
          (setq portable-bookmarks-jump-spec--handled nil)
          (let ((buffer (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")))
            (expect (file-equal-p (buffer-file-name buffer) b-path) :to-be-truthy))
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
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (b-path (expand-file-name "README.md" (plist-get world :wt-b))))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (let ((buffer (portable-bookmarks-jump-spec--jump
                       "readme" "wsbm-jump-b" #'bookmark-jump-other-window)))
          (expect (file-equal-p (buffer-file-name buffer) b-path) :to-be-truthy))
        (expect (portable-bookmarks-jump-spec--visits-p b-path) :to-be-truthy)
        (expect (portable-bookmarks-jump-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "sets bookmark-current-bookmark after a substituted jump by name, as the host does for a plain one"
    ;; Driven at `bookmark-handle-bookmark' with a STRING on purpose: in
    ;; this configuration bufferlo's advice on `bookmark--jump-via' hands
    ;; the host a record for every jump, so `bookmark-jump' can never show
    ;; the string path.  `bookmark-current-bookmark' is buffer-local; each
    ;; visited buffer is fresh (the buffer `bookmark-set' ran in was
    ;; killed), so a value can only have come from the jump.
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (dir (workspace-bookmark-fixtures-temp-dir "wsbm-jump-plain-"))
             (plain-path (expand-file-name "notes.txt" dir)))
        (with-temp-file plain-path (insert portable-bookmarks-jump-spec--contents))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (portable-bookmarks-jump-spec--set plain-path "plain")
        (dolist (name '("plain" "readme"))
          (let ((buffer (portable-bookmarks-jump-spec--jump
                         name "wsbm-jump-b" #'bookmark-handle-bookmark)))
            (expect (cons name (buffer-local-value 'bookmark-current-bookmark buffer))
                    :to-equal (cons name name))
            ;; The stamped one must ALSO have been substituted: parity
            ;; alone is something the stock host satisfies by itself.
            (when (equal name "readme")
              (expect (file-equal-p (buffer-file-name buffer)
                                    (expand-file-name "README.md" (plist-get world :wt-b)))
                      :to-be-truthy))))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "leaves an unstamped record alone: it opens its stored absolute path"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((dir (workspace-bookmark-fixtures-temp-dir "wsbm-jump-plain-"))
             (path (expand-file-name "notes.txt" dir)))
        (with-temp-file path (insert portable-bookmarks-jump-spec--contents))
        (let* ((stored (portable-bookmarks-jump-spec--set path "plain"))
               (snapshot (copy-tree stored)))
          (expect (bookmark-prop-get stored workspace-bookmark-relpath-property)
                  :to-be nil)
          (let ((buffer (portable-bookmarks-jump-spec--jump "plain" "wsbm-jump-b")))
            (expect (file-equal-p (buffer-file-name buffer) path) :to-be-truthy)
            (expect (with-current-buffer buffer (point))
                    :to-equal portable-bookmarks-jump-spec--position))
          (expect (bookmark-get-bookmark "plain") :to-equal snapshot)))))

  (it "passes a stamped record through unchanged when there is no current workspace (until off-workspace-restore lands)"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (spy-on 'workspace-bookmark--resolve :and-call-through)
        (let ((buffer (portable-bookmarks-jump-spec--jump "readme" nil)))
          (expect (file-equal-p (buffer-file-name buffer) a-path) :to-be-truthy))
        (expect 'workspace-bookmark--resolve :not :to-have-been-called)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


;;; Behavioral — outcomes that open nothing

(describe "jump seam: outcomes that open nothing"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-jump-spec--world)))

  (it "declines not-on-branch: no buffer visits the missing path, nor the stored one"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (b-path (expand-file-name "only-a.el" (plist-get world :wt-b)))
             (stored (portable-bookmarks-jump-spec--set a-path "only-a"))
             (snapshot (copy-tree stored)))
        (expect (file-exists-p b-path) :to-be nil)
        (let ((message (portable-bookmarks-jump-spec--decline "only-a" "wsbm-jump-b")))
          (expect message :to-match (regexp-quote "\"only-a\""))
          (expect message :to-match (regexp-quote "only-a.el"))
          (expect message :to-match (regexp-quote portable-bookmarks-jump-spec--identity)))
        (expect (portable-bookmarks-jump-spec--visits-p b-path) :to-be nil)
        (expect (portable-bookmarks-jump-spec--visits-p a-path) :to-be nil)
        (expect (file-exists-p b-path) :to-be nil)
        (expect (bookmark-get-bookmark "only-a") :to-equal snapshot))))

  (it "declines ambiguous: two checkouts in the workspace are reported, neither chosen"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (wt-b (plist-get world :wt-b))
             (wt-b2 (workspace-bookmark-fixtures-add-worktree
                     (plist-get world :repo)
                     (expand-file-name "proj2" (plist-get world :home-b))
                     :branch "b2")))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (let ((message (portable-bookmarks-jump-spec--decline "readme" "wsbm-jump-b")))
          (expect message :to-match "2 checkouts")
          (expect message :to-match (regexp-quote (abbreviate-file-name wt-b)))
          (expect message :to-match (regexp-quote (abbreviate-file-name wt-b2))))
        (dolist (path (list a-path
                            (expand-file-name "README.md" wt-b)
                            (expand-file-name "README.md" wt-b2)))
          (expect (portable-bookmarks-jump-spec--visits-p path) :to-be nil))
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines no-clone: no checkout of the identity in view"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((elsewhere (workspace-bookmark-fixtures-make-repo
                         :remote "git@example.com:fixtures/elsewhere.git"))
             (path (expand-file-name "README.md" elsewhere))
             (stored (portable-bookmarks-jump-spec--set path "elsewhere")))
        (expect (bookmark-prop-get stored workspace-bookmark-repo-property)
                :to-equal "example.com/fixtures/elsewhere")
        (let ((message (portable-bookmarks-jump-spec--decline "elsewhere" "wsbm-jump-b")))
          (expect message :to-match "no checkout of example.com/fixtures/elsewhere in view"))
        (expect (portable-bookmarks-jump-spec--visits-p path) :to-be nil)
        (expect (bookmark-get-filename "elsewhere") :to-equal path))))

  (it "hands missing-repo to the bounded choice, opening neither the canonical copy nor the stored one until the user chooses"
    ;; The choice itself is portable-bookmarks-missing-repo-spec.el's.
    ;; Here: the seam prompts instead of declining or opening, the prompt
    ;; is the missing-repo one, and cancel ends the jump with `quit'.
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (prompts nil)
             (quit-p nil))
        (workspace-bookmark-fixtures-make-workspace "wsbm-jump-c")
        (portable-bookmarks-jump-spec--set a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--can-prompt-p)
                   (lambda () t))
                  ((symbol-function 'read-multiple-choice)
                   (lambda (prompt choices &rest _)
                     (push prompt prompts)
                     (assq ?c choices))))
          (condition-case nil
              (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-c")
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect (length prompts) :to-equal 1)
        (expect (car prompts) :to-match "holds no checkout")
        (expect (car prompts) :to-match
                (regexp-quote (abbreviate-file-name (plist-get world :repo))))
        (expect (portable-bookmarks-jump-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-jump-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines with user-error, never bookmark-error-no-filename, and never offers to relocate"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (signalled nil))
        (portable-bookmarks-jump-spec--set a-path "only-a")
        (condition-case err
            (portable-bookmarks-jump-spec--jump "only-a" "wsbm-jump-b")
          (bookmark-errors (setq signalled (car err)))
          (user-error (setq signalled (car err))))
        (expect signalled :to-be 'user-error)))))


;;; Unit — the barrier, quit, and the host's contract

(describe "jump seam: barrier and host contract"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-jump-spec--world)))

  (it "still opens the stored location when the resolver signals"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest _) (error "Boom"))))
          (let ((buffer (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")))
            (expect (file-equal-p (buffer-file-name buffer) a-path) :to-be-truthy)))
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "still opens the stored location when the seam's own copy step signals"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
             (snapshot (copy-tree stored)))
        (cl-letf (((symbol-function 'workspace-bookmark--substituted-record)
                   (lambda (&rest _) (error "Boom"))))
          (let ((buffer (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")))
            (expect (file-equal-p (buffer-file-name buffer) a-path) :to-be-truthy)))
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "lets quit propagate through the barrier"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (quit-p nil))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest _) (signal 'quit nil))))
          (condition-case nil
              (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")
            (quit (setq quit-p t))))
        (expect quit-p :to-be t)
        (expect (portable-bookmarks-jump-spec--visits-p a-path) :to-be nil)
        (expect (portable-bookmarks-jump-spec--visits-p
                 (expand-file-name "README.md" (plist-get world :wt-b)))
                :to-be nil))))

  (it "does not catch the handler's own errors, and calls the host exactly once"
    (portable-bookmarks-jump-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (calls 0))
        (portable-bookmarks-jump-spec--set a-path "readme")
        (cl-letf (((symbol-function 'bookmark-default-handler)
                   (lambda (&rest _) (cl-incf calls) (error "Handler boom"))))
          (expect (portable-bookmarks-jump-spec--jump "readme" "wsbm-jump-b")
                  :to-throw 'error '("Handler boom")))
        (expect calls :to-equal 1))))

  (it "declines when a resolved path vanished before the visit, touching neither the originating buffer nor the stored record"
    ;; The seam pre-flights the resolved :path.  Without it the host, given a
    ;; RECORD, swallows bookmark-error-no-filename and returns nil, and
    ;; bookmark--jump-via then fringe-marks and hooks the buffer the user
    ;; was already in.  Driven from a scratch ORIGIN buffer so that tail
    ;; would be visible.
    (portable-bookmarks-jump-spec--hermetic
      (let* ((wt-b (plist-get world :wt-b))
             (a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (vanished (expand-file-name "vanished.el" wt-b))
             (stored (portable-bookmarks-jump-spec--set a-path "readme"))
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
              (let ((message (portable-bookmarks-jump-spec--decline "readme" "wsbm-jump-b")))
                (expect message :to-match "vanished.el")
                (expect message :to-match "nothing opened")))
            (expect (current-buffer) :to-be origin)
            (expect (length (overlays-in (point-min) (point-max))) :to-equal overlays)
            (expect (local-variable-p 'bookmark-current-bookmark origin) :to-be nil)))
        (expect (file-exists-p vanished) :to-be nil)
        (expect (portable-bookmarks-jump-spec--visits-p vanished) :to-be nil)
        (expect (portable-bookmarks-jump-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


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
