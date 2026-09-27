;;; portable-bookmarks-missing-repo-spec.el --- Missing-repo choice specs -*- lexical-binding: t; -*-

;; Specs for the `missing-repo' prompt: the bounded choice the jump seam
;; offers when the current workspace holds no checkout of a bookmark's
;; repository and exactly one canonical checkout is in view.
;;
;; * BEHAVIORAL — real `git' (one repository; a linked worktree in
;;   workspace A's home; workspace C holding nothing), records stamped by
;;   the real stamp seam through `bookmark-set', jumps driven through
;;   `bookmark-jump'.  The ONE boundary stubbed inside the jump is the
;;   prompt itself: `read-multiple-choice' is bound with `cl-letf' to a
;;   stand-in that records what it was offered and answers with one key.
;;   `jf/workspace--add-worktree' (git-worktrees, magit-backed) is bound
;;   per spec to a stub that either really adds a worktree with the
;;   fixtures or returns a vocabulary outcome; `nil' unbinds it.  ONE
;;   spec runs the REAL command (git-worktrees.el is loaded here for it)
;;   and stands in only for magit and the branch prompt, so the contract
;;   the chooser relies on — the command honours `:source-repo' — is
;;   pinned across the two modules, not against a stub.
;;
;; * UNIT — the offered option set and the prompt text, the payload handed
;;   to the worktree command, the chooser's own barrier, and a structural
;;   read of the tangled section.
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

;; Owned by tabs.el; see portable-bookmarks-discovery-spec.el for why this
;; stand-in exists.  Under the harness the real definition is bound.
(unless (fboundp 'workspace--registered-names)
  (defun workspace--registered-names ()
    "Return the list of workspace names currently in the registry."
    (let (names)
      (maphash (lambda (k _v) (push k names)) workspace--registry)
      (nreverse names))))


;;; Helpers

(defconst portable-bookmarks-missing-repo-spec--remote
  "git@example.com:fixtures/missing.git"
  "Remote URL given to the fixture repository.")

(defconst portable-bookmarks-missing-repo-spec--identity
  "example.com/fixtures/missing"
  "The identity `portable-bookmarks-missing-repo-spec--remote' normalizes to.")

(defconst portable-bookmarks-missing-repo-spec--contents
  "line one\nline two\nline three\n"
  "Contents of the seeded README, identical in every checkout.")

(defconst portable-bookmarks-missing-repo-spec--position 10
  "A position inside `portable-bookmarks-missing-repo-spec--contents' (line 2).")

(defvar portable-bookmarks-missing-repo-spec--prompts nil
  "(PROMPT CHOICES) pairs the stubbed `read-multiple-choice' received, newest first.")

(defmacro portable-bookmarks-missing-repo-spec--hermetic (&rest body)
  "Run one spec's BODY with nothing but the fixtures' world in view.
`jf/emacs-dir' is nil so the configuration checkout running the suite
is not a canonical checkout in view; the identity memo is cleared on
exit; every buffer BODY leaves behind is killed before teardown deletes
the directories those buffers visit."
  (declare (indent 0))
  `(let ((jf/emacs-dir nil)
         (before (buffer-list)))
     (unwind-protect (progn ,@body)
       (dolist (buffer (buffer-list))
         (unless (memq buffer before)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (workspace-bookmark-clear-identity-cache))))

(defun portable-bookmarks-missing-repo-spec--world ()
  "Build the standard world and return it as a plist.
One repository with a remote, seeded with README.md and sub/deep.el on
`main'; workspace A (\"wsbm-mr-a\") holding a linked worktree of it, and
workspace C (\"wsbm-mr-c\") holding nothing.  Keys: :repo :home-a
:home-c :wt-a."
  (let* ((repo (workspace-bookmark-fixtures-make-repo
                :remote portable-bookmarks-missing-repo-spec--remote
                :contents portable-bookmarks-missing-repo-spec--contents))
         (home-a (workspace-bookmark-fixtures-make-workspace "wsbm-mr-a"))
         (home-c (workspace-bookmark-fixtures-make-workspace "wsbm-mr-c")))
    (workspace-bookmark-fixtures-commit-file repo "sub/deep.el" "(deep)\n")
    (list :repo repo :home-a home-a :home-c home-c
          :wt-a (workspace-bookmark-fixtures-add-worktree
                 repo (expand-file-name "proj" home-a) :branch "a"))))

(defun portable-bookmarks-missing-repo-spec--set (path name)
  "Set bookmark NAME on PATH through `bookmark-set'; return the stored record.
The real stamp seam runs; the visiting buffer is killed afterwards."
  (let ((buffer (if (file-directory-p path)
                    (dired-noselect path)
                  (find-file-noselect path))))
    (with-current-buffer buffer
      (unless (file-directory-p path)
        (goto-char portable-bookmarks-missing-repo-spec--position))
      (let ((inhibit-message t))
        (bookmark-set name)))
    (kill-buffer buffer)
    (bookmark-get-bookmark name)))

(defun portable-bookmarks-missing-repo-spec--answering (key)
  "Return a `read-multiple-choice' stand-in that answers with KEY.
Records (PROMPT CHOICES) in `portable-bookmarks-missing-repo-spec--prompts'
and returns the offered entry for KEY; signals when KEY is not offered."
  (lambda (prompt choices &rest _)
    (push (list prompt choices) portable-bookmarks-missing-repo-spec--prompts)
    (or (assq key choices)
        (error "Option %c is not offered; offered: %S"
               key (mapcar #'cadr choices)))))

(defun portable-bookmarks-missing-repo-spec--jump (name workspace answer &optional jumper)
  "Jump to bookmark NAME with WORKSPACE current; return the buffer left current.
ANSWER is the key the stubbed prompt answers with, or a function to use
as `read-multiple-choice'.  `workspace-bookmark--can-prompt-p' answers t
\(the harness is non-interactive; the prompt under test is stubbed).
JUMPER defaults to `bookmark-jump'.  Any yes/no prompt fails the spec."
  (setq portable-bookmarks-missing-repo-spec--prompts nil)
  (cl-letf (((symbol-function 'workspace--current-name)
             (lambda () workspace))
            ((symbol-function 'workspace-bookmark--can-prompt-p)
             (lambda () t))
            ((symbol-function 'read-multiple-choice)
             (if (functionp answer)
                 answer
               (portable-bookmarks-missing-repo-spec--answering answer)))
            ((symbol-function 'y-or-n-p)
             (lambda (&rest args) (error "Prompted: %S" args)))
            ((symbol-function 'yes-or-no-p)
             (lambda (&rest args) (error "Prompted: %S" args))))
    (let ((inhibit-message t))
      (funcall (or jumper #'bookmark-jump) name))
    (current-buffer)))

(defun portable-bookmarks-missing-repo-spec--decline (name workspace answer)
  "Jump to NAME with WORKSPACE current answering ANSWER, expecting a `user-error'.
Returns the error's message; fails the spec when nothing was signalled."
  (let ((message
         (condition-case err
             (progn (portable-bookmarks-missing-repo-spec--jump name workspace answer)
                    nil)
           (user-error (cadr err)))))
    (expect message :not :to-be nil)
    message))

(defun portable-bookmarks-missing-repo-spec--quits-p (thunk)
  "Return `quit' when THUNK signals `quit', `error' when it signals an error, else nil."
  (condition-case nil
      (progn (funcall thunk) nil)
    (quit 'quit)
    (error 'error)))

(defun portable-bookmarks-missing-repo-spec--visits-p (path)
  "Non-nil when some live buffer visits PATH (a file or a directory)."
  (and (or (find-buffer-visiting path)
           (and (file-directory-p path)
                (seq-some (lambda (buffer)
                            (with-current-buffer buffer
                              (and (derived-mode-p 'dired-mode)
                                   (workspace--same-file-p
                                    default-directory path))))
                          (buffer-list))))
       t))

(defun portable-bookmarks-missing-repo-spec--offered-keys ()
  "Return the keys the most recent stubbed prompt offered, in order."
  (mapcar #'car (cadr (car portable-bookmarks-missing-repo-spec--prompts))))

(defun portable-bookmarks-missing-repo-spec--prompt-text ()
  "Return the most recent stubbed prompt's text."
  (car (car portable-bookmarks-missing-repo-spec--prompts)))

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
    (setq world (portable-bookmarks-missing-repo-spec--world)))

  (it "offers add worktree, view read-only and cancel, naming the bookmark, the workspace, the identity and the canonical checkout"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?c))))
        (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1)
        (expect (portable-bookmarks-missing-repo-spec--offered-keys) :to-equal '(?a ?v ?c))
        (let ((prompt (portable-bookmarks-missing-repo-spec--prompt-text)))
          (expect prompt :to-match (regexp-quote "\"readme\""))
          (expect prompt :to-match (regexp-quote "\"wsbm-mr-c\""))
          (expect prompt :to-match (regexp-quote portable-bookmarks-missing-repo-spec--identity))
          (expect prompt :to-match (regexp-quote (abbreviate-file-name (plist-get world :repo))))
          (expect prompt :not :to-match "not offered")))))

  (it "omits add worktree when jf/workspace--add-worktree is not loaded, and says so"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree) nil))
          (expect (fboundp 'jf/workspace--add-worktree) :to-be nil)
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?c))))
        (expect (portable-bookmarks-missing-repo-spec--offered-keys) :to-equal '(?v ?c))
        (expect (portable-bookmarks-missing-repo-spec--prompt-text)
                :to-match "add worktree here not offered: the git worktree integration is not loaded"))))

  (it "omits view read-only when the relpath is absent under the canonical checkout (:path nil), and says so"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let ((a-path (workspace-bookmark-fixtures-commit-file
                     (plist-get world :wt-a) "only-a.el")))
        (expect (file-exists-p (expand-file-name "only-a.el" (plist-get world :repo)))
                :to-be nil)
        (portable-bookmarks-missing-repo-spec--set a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (portable-bookmarks-missing-repo-spec--jump "only-a" "wsbm-mr-c" ?c))))
        (expect (portable-bookmarks-missing-repo-spec--offered-keys) :to-equal '(?a ?c))
        (expect (portable-bookmarks-missing-repo-spec--prompt-text)
                :to-match "view read-only not offered: only-a.el is absent there"))))

  (it "is cancel-only when both are withheld, stating both reasons"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let ((a-path (workspace-bookmark-fixtures-commit-file
                     (plist-get world :wt-a) "only-a.el")))
        (portable-bookmarks-missing-repo-spec--set a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree) nil))
          (portable-bookmarks-missing-repo-spec--quits-p
           (lambda () (portable-bookmarks-missing-repo-spec--jump "only-a" "wsbm-mr-c" ?c))))
        (expect (portable-bookmarks-missing-repo-spec--offered-keys) :to-equal '(?c))
        (let ((prompt (portable-bookmarks-missing-repo-spec--prompt-text)))
          (expect prompt :to-match "add worktree here not offered")
          (expect prompt :to-match "view read-only not offered")))))

  (it "declines with the seam's missing-repo report instead of prompting when no one can answer (noninteractive)"
    ;; The harness IS non-interactive, so the real predicate is used here
    ;; and the prompt stand-in must never be reached.
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (asked nil))
        (expect noninteractive :to-be-truthy)
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
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
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)
        ;; The predicate is the only thing between that decline and the
        ;; prompt: forced to t, the same jump asks.
        (portable-bookmarks-missing-repo-spec--quits-p
         (lambda () (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?c)))
        (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1))))

  (it "lets quit at the prompt propagate, opening nothing"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (expect (portable-bookmarks-missing-repo-spec--quits-p
                 (lambda ()
                   (portable-bookmarks-missing-repo-spec--jump
                    "readme" "wsbm-mr-c" (lambda (&rest _) (signal 'quit nil)))))
                :to-be 'quit)
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)))))


(describe "missing-repo choice: view read-only"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-missing-repo-spec--world)))

  (it "opens the canonical copy in view-mode, read-only, marked in the mode line, with one message naming path and branch; the stored record is untouched"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (stored (portable-bookmarks-missing-repo-spec--set a-path "readme"))
             (snapshot (copy-tree stored))
             (buffer nil))
        (portable-bookmarks-missing-repo-spec--collecting-messages messages
          (setq buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v))
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
          (expect (point) :to-equal portable-bookmarks-missing-repo-spec--position)
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
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "leaves a stock C-x C-q as the deliberate way out: read-only-mode off makes it writable, and the marker stays"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (let ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v)))
          (with-current-buffer buffer
            (expect view-mode :to-be-truthy)
            (read-only-mode -1)
            (expect buffer-read-only :to-be nil)
            (expect view-mode :to-be nil)
            (expect workspace-bookmark--canonical-view :to-be-truthy))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")))))

  (it "keeps the view through a revert-buffer that re-runs the major mode: marker, view-mode and read-only all come back"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (let ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v)))
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
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (let ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v)))
          (with-current-buffer buffer
            (read-only-mode -1)
            (revert-buffer t t nil)
            (expect buffer-read-only :to-be nil)
            (expect view-mode :to-be nil)
            (expect workspace-bookmark--canonical-view :to-be-truthy))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical")))))

  (it "does not stack the marker when the same canonical copy is viewed twice"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v)
        (let* ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v))
               (marker (portable-bookmarks-missing-repo-spec--marker buffer)))
          (expect (length (split-string marker "\\[canonical" t)) :to-equal 2)))))

  (it "opens a directory record's canonical directory in Dired, marked, without view-mode"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-dir (expand-file-name "sub/" (plist-get world :wt-a)))
             (canonical-dir (expand-file-name "sub/" (plist-get world :repo))))
        (portable-bookmarks-missing-repo-spec--set a-dir "sub")
        (let ((buffer (portable-bookmarks-missing-repo-spec--jump "sub" "wsbm-mr-c" ?v)))
          (with-current-buffer buffer
            (expect (derived-mode-p 'dired-mode) :to-be-truthy)
            (expect (workspace--same-file-p default-directory canonical-dir)
                    :to-be-truthy)
            (expect buffer-read-only :to-be-truthy)
            (expect view-mode :to-be nil))
          (expect (portable-bookmarks-missing-repo-spec--marker buffer) :to-match "canonical"))
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-dir) :to-be nil)
        (expect (bookmark-get-filename "sub") :to-equal a-dir))))

  (it "does not mark the originating buffer when the host does not land"
    ;; The host, given a RECORD, swallows bookmark-error-no-filename and
    ;; returns nil without changing the current buffer.  Forced with a
    ;; handler that does exactly that: the view marking must not touch
    ;; the buffer the user was in.
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (origin (generate-new-buffer "wsbm-mr-origin")))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (with-current-buffer origin
          (insert "origin\n")
          (cl-letf (((symbol-function 'bookmark-default-handler)
                     (lambda (_record) nil)))
            (portable-bookmarks-missing-repo-spec--collecting-messages messages
              (expect (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?v)
                      :to-be origin)
              (expect (seq-some (lambda (m) (string-match-p "read-only" m)) messages)
                      :to-be nil)))
          (expect view-mode :to-be nil)
          (expect buffer-read-only :to-be nil)
          (expect workspace-bookmark--canonical-view :to-be nil)
          (expect (portable-bookmarks-missing-repo-spec--marker origin) :to-be nil)))))

  (it "declines when the canonical copy vanished between the resolver and the choice"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (let ((message
               (portable-bookmarks-missing-repo-spec--decline
                "readme" "wsbm-mr-c"
                (lambda (_prompt choices &rest _)
                  (delete-file canonical-path)
                  (assq ?v choices)))))
          (expect message :to-match "vanished")
          (expect message :to-match "nothing opened"))
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)))))


(describe "missing-repo choice: cancel"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-missing-repo-spec--world)))

  (it "opens nothing, signals quit (never an error), leaves the originating buffer unmarked and says nothing"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (stored (portable-bookmarks-missing-repo-spec--set a-path "readme"))
             (snapshot (copy-tree stored))
             (origin (generate-new-buffer "wsbm-mr-origin")))
        (with-current-buffer origin
          (insert "origin\n")
          (let ((overlays (length (overlays-in (point-min) (point-max)))))
            (portable-bookmarks-missing-repo-spec--collecting-messages messages
              (expect (portable-bookmarks-missing-repo-spec--quits-p
                       (lambda ()
                         (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?c)))
                      :to-be 'quit)
              (expect messages :to-equal nil))
            (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1)
            (expect (current-buffer) :to-be origin)
            (expect (length (overlays-in (point-min) (point-max))) :to-equal overlays)
            (expect (local-variable-p 'bookmark-current-bookmark origin) :to-be nil)
            (expect buffer-read-only :to-be nil)))
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot)))))


(describe "missing-repo choice: add worktree here"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-missing-repo-spec--world)))

  (it "hands the worktree command the current workspace, its home and :checkout as the source; on ok resolves once more and opens the new worktree's copy"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (home-c (plist-get world :home-c))
             (stored (portable-bookmarks-missing-repo-spec--set a-path "readme"))
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
          (let ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?a)))
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
        (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1)
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-bookmark "readme") :to-equal snapshot))))

  (it "targets :checkout even when the relpath is absent there, and reports not-on-branch after the add without a second prompt"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (workspace-bookmark-fixtures-commit-file
                      (plist-get world :wt-a) "only-a.el"))
             (home-c (plist-get world :home-c))
             (received nil))
        (portable-bookmarks-missing-repo-spec--set a-path "only-a")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (payload)
                     (setq received payload)
                     (workspace-bookmark-fixtures-add-worktree
                      (plist-get world :repo)
                      (expand-file-name "proj" (plist-get payload :home))
                      :branch "c")
                     'ok)))
          (let ((message (portable-bookmarks-missing-repo-spec--decline "only-a" "wsbm-mr-c" ?a)))
            (expect message :to-match "not on this branch")
            (expect message :to-match "after adding a worktree")))
        (expect (workspace--same-file-p (plist-get received :source-repo)
                                        (plist-get world :repo))
                :to-be-truthy)
        (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1)
        (expect (file-directory-p (expand-file-name "proj" home-c)) :to-be-truthy)
        (expect (portable-bookmarks-missing-repo-spec--visits-p
                 (expand-file-name "proj/only-a.el" home-c))
                :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil))))

  (it "through the REAL git-worktrees command: no source prompt, no discovery re-run, the worktree made from :checkout"
    ;; Architect on-touch finding 31.  Only magit is stood in for — its
    ;; main-branch lookup, and its worktree creation, which really adds
    ;; the worktree with the fixtures — and the branch prompt, answered
    ;; with its default.  Every source prompt fails the spec.
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((repo (plist-get world :repo))
             (home-c (plist-get world :home-c))
             (a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (made nil))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
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
          (let ((buffer (portable-bookmarks-missing-repo-spec--jump "readme" "wsbm-mr-c" ?a)))
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
        (expect (length portable-bookmarks-missing-repo-spec--prompts) :to-equal 1))))

  (it "reports (failed . REASON) and opens nothing"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (spy-on 'workspace-bookmark--resolve :and-call-through)
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) (cons 'failed "worktree dir already exists: proj"))))
          (let ((message (portable-bookmarks-missing-repo-spec--decline "readme" "wsbm-mr-c" ?a)))
            (expect message :to-match (regexp-quote "\"readme\""))
            (expect message :to-match "worktree dir already exists: proj")
            (expect message :to-match "nothing opened")))
        (expect (spy-calls-count 'workspace-bookmark--resolve) :to-equal 1)
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil))))

  (it "reports skipped and opens nothing"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (cl-letf (((symbol-function 'jf/workspace--add-worktree)
                   (lambda (_payload) 'skipped)))
          (expect (portable-bookmarks-missing-repo-spec--decline "readme" "wsbm-mr-c" ?a)
                  :to-match "skipped the add"))
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)))))


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
              :to-equal (workspace--sessions-dir home))))

  (it "lists the same keys but :sessions-dir when core's constructor is not loaded"
    (let ((home (workspace-bookmark--workspace-home "wsbm-mr-payload")))
      (cl-letf (((symbol-function 'workspace--integration-payload) nil))
        (expect (workspace-bookmark--add-worktree-payload "wsbm-mr-payload" "/src/repo/")
                :to-equal (list :name "wsbm-mr-payload" :home home
                                :context 'bookmark-jump :source-repo "/src/repo/"))))))


;;; Unit — the chooser's own barrier

(describe "missing-repo choice: barrier"
  :var (world)

  (workspace-bookmark-fixtures-with-empty-registry
    (workspace-bookmark-clear-identity-cache)
    (setq world (portable-bookmarks-missing-repo-spec--world)))

  (it "declines on an error inside the choice instead of falling through to the stored path"
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo))))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--missing-repo-prompt)
                   (lambda (&rest _) (error "Prompt boom"))))
          (let ((message (portable-bookmarks-missing-repo-spec--decline "readme" "wsbm-mr-c" ?v)))
            (expect message :to-match "Prompt boom")
            (expect message :to-match "nothing opened")))
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)
        (expect (bookmark-get-filename "readme") :to-equal a-path))))

  (it "declines a view whose resolution names no canonical checkout instead of opening the copy unmarked"
    ;; Unreachable from today's resolver (a missing-repo with a :path
    ;; always carries :checkout); forced by dropping :checkout from the
    ;; real resolution, so a producer change cannot open main writable.
    (portable-bookmarks-missing-repo-spec--hermetic
      (let* ((a-path (expand-file-name "README.md" (plist-get world :wt-a)))
             (canonical-path (expand-file-name "README.md" (plist-get world :repo)))
             (resolve (symbol-function 'workspace-bookmark--resolve)))
        (portable-bookmarks-missing-repo-spec--set a-path "readme")
        (cl-letf (((symbol-function 'workspace-bookmark--resolve)
                   (lambda (&rest args)
                     (let ((resolution (copy-sequence (apply resolve args))))
                       (expect (plist-get resolution :outcome) :to-be 'missing-repo)
                       (expect (plist-get resolution :path) :to-be-truthy)
                       (plist-put resolution :checkout nil)))))
          (let ((message (portable-bookmarks-missing-repo-spec--decline "readme" "wsbm-mr-c" ?v)))
            (expect message :to-match "No canonical checkout")
            (expect message :to-match "nothing opened")))
        (expect (portable-bookmarks-missing-repo-spec--visits-p canonical-path) :to-be nil)
        (expect (portable-bookmarks-missing-repo-spec--visits-p a-path) :to-be nil)))))


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
