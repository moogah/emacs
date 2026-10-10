;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'bookmark)
(require 'view)

(defgroup workspace-bookmark nil
  "Workspace-relative resolution of Emacs bookmarks."
  :group 'workspaces
  :prefix "workspace-bookmark-")

(defconst workspace-bookmark-repo-property 'workspace-bookmark-repo
  "Bookmark-record property holding the repository identity.
The value stored under this property is the normalized remote URL of the
repository the bookmarked location belongs to, for example
\"github.com/moogah/emacs\".

This symbol is serialised into the user's bookmark file.  Changing it
orphans coordinates stamped under the previous name; records so orphaned
degrade to the `non-portable' outcome and behave exactly as stock
bookmarks do.")

(defconst workspace-bookmark-relpath-property 'workspace-bookmark-relpath
  "Bookmark-record property holding the repository-relative path.
The value is the location's path relative to the root of its checkout,
exactly as git spells it: \"lisp/foo.el\" for a file, \"lisp/\" (with a
trailing slash) for a directory, and \"./\" for the repository root
itself.  It can be re-anchored against any checkout of the same
repository with `expand-file-name'.

This symbol is serialised into the user's bookmark file; see
`workspace-bookmark-repo-property' for the consequence of renaming it.")

(defconst workspace-bookmark-format-property 'workspace-bookmark-format
  "Bookmark-record property holding the stamp-format version.
The value is `workspace-bookmark-format-version' at the time the record
was stamped.  This symbol is serialised into the user's bookmark file;
see `workspace-bookmark-repo-property'.")

(defconst workspace-bookmark-format-version 1
  "The stamp-format version this module writes.
Bump it only when the MEANING of the stamped identity or relpath
changes, so records stamped under an older meaning can be recognised
rather than misread.")

(defconst workspace-bookmark--git-unset-environment
  '("GIT_DIR" "GIT_WORK_TREE" "GIT_COMMON_DIR" "GIT_INDEX_FILE"
    "GIT_OBJECT_DIRECTORY" "GIT_ALTERNATE_OBJECT_DIRECTORIES"
    "GIT_NAMESPACE" "GIT_CEILING_DIRECTORIES"
    "GIT_DISCOVERY_ACROSS_FILESYSTEM" "GIT_IMPLICIT_WORK_TREE"
    "GIT_PREFIX" "GIT_CONFIG" "GIT_CONFIG_PARAMETERS" "GIT_CONFIG_COUNT"
    "GIT_GRAFT_FILE" "GIT_NO_REPLACE_OBJECTS" "GIT_REPLACE_REF_BASE"
    "GIT_SHALLOW_FILE")
  "Environment variables removed for every git subprocess this module runs.
Each one can make git answer about a repository other than the one
`default-directory' names.")

(defun workspace-bookmark--git (dir &rest args)
  "Run git with ARGS in DIR and return its stdout, or nil.

Only the one line terminator git appends is removed from the output;
other leading and trailing whitespace is kept, because it can be part of
a path.  Returns nil — never signals — when DIR is nil, remote or
missing, when git exits non-zero, or when git produces no output.  If
DIR names an existing file, git is run in the file's directory; a DIR
that does not exist is never resolved to its parent.  stderr is
discarded.  The variables in `workspace-bookmark--git-unset-environment'
are removed from git's environment, so `default-directory' alone decides
which repository git answers for.

DIR must be LOCAL.  A remote (TRAMP) name returns nil without touching
the filesystem: probing one can signal, can block on a password prompt,
and — on a reachable host — would run git remotely and stamp the wrong
machine's identity."
  (condition-case nil
      (let ((path (and (stringp dir)
                       (not (file-remote-p dir))
                       (expand-file-name dir))))
        (when (and path (not (file-remote-p path)))
          (let ((cwd (cond ((file-directory-p path)
                            (file-name-as-directory path))
                           ((file-exists-p path)
                            (file-name-directory (directory-file-name path))))))
            (when (and cwd (file-directory-p cwd))
              (let ((default-directory cwd)
                    (process-environment
                     (append workspace-bookmark--git-unset-environment
                             process-environment)))
                (with-temp-buffer
                  (when (eq 0 (apply #'process-file "git" nil '(t nil) nil args))
                    (let ((out (string-remove-suffix "\n" (buffer-string))))
                      (unless (string-empty-p out) out)))))))))
    (error nil)))

(declare-function workspace--same-file-p "data-model" (a b))
(declare-function workspace--path-within-p "data-model" (path dir))

(defun workspace-bookmark--remote-url (dir)
  "Return the effective remote URL of the repository containing DIR, or nil.

`origin' is preferred.  When the repository has no `origin' but has
exactly ONE remote, that remote is used.  Returns nil when the repository
has no remote, when it has several and none is `origin', or when DIR is
in no repository.

Uses `git remote get-url', which expands `url.*.insteadOf' rewrites and
yields the URL git actually uses.  `git config --get remote.origin.url'
would return the raw, machine-local spelling."
  (or (workspace-bookmark--git dir "remote" "get-url" "origin")
      (let ((remotes (split-string
                      (or (workspace-bookmark--git dir "remote") "")
                      "\n" t "[ \t\r]+")))
        (when (= 1 (length remotes))
          (workspace-bookmark--git dir "remote" "get-url" (car remotes))))))

(defconst workspace-bookmark--scheme-prefix-re
  "\\`[a-zA-Z][a-zA-Z0-9+.-]*://"
  "Match the leading SCHEME:// of a remote URL.
Which branch of `workspace-bookmark--normalize-identity' applies is
decided by this alone.  A URL that has a scheme but does not parse is
nil, never retried as scp-like syntax: \"https://github.com\" read as
scp-like would yield the host \"https\".")

(defconst workspace-bookmark--scheme-url-re
  "\\`[a-zA-Z][a-zA-Z0-9+.-]*://\\(?:[^/@]*@\\)?\\([^/:]*\\)\\(?::[0-9]*\\)?/+\\(.*\\)\\'"
  "Match a remote URL carrying an explicit scheme.
Group 1 is the host, group 2 the path.  Any userinfo and any :PORT are
matched and discarded.  A `file://' URL has an empty host and is
therefore rejected downstream.")

(defconst workspace-bookmark--scp-url-re
  "\\`\\(?:[^/@]*@\\)?\\([^/:]+\\):\\(.*\\)\\'"
  "Match git's scp-like remote syntax, [USERINFO@]HOST:PATH.
Group 1 is the host, group 2 the path.  There is no port in this form:
git reads everything after the colon as the path.")

(defun workspace-bookmark--normalize-identity (url)
  "Return the normalized repository identity of URL, or nil.

The identity is HOST/PATH, with the scheme, any userinfo, any port, a
trailing \".git\" (in any case) and any trailing slash removed, runs of
slashes collapsed, and the host — but not the path — downcased.  The SSH, HTTPS and ssh:// spellings of one
repository therefore compare `equal'.

Returns nil for anything that does not name a host: local paths,
`file://' URLs, and malformed input."
  (when (stringp url)
    (let ((url (string-trim url))
          host path)
      (cond
       ((string-match-p workspace-bookmark--scheme-prefix-re url)
        (when (string-match workspace-bookmark--scheme-url-re url)
          (setq host (match-string 1 url)
                path (match-string 2 url))))
       ((string-match workspace-bookmark--scp-url-re url)
        (setq host (match-string 1 url)
              path (match-string 2 url))))
      (when (and host path)
        (setq host (downcase host))
        (setq path (replace-regexp-in-string "/+" "/" path))
        (setq path (replace-regexp-in-string "\\`/+" "" path))
        (setq path (replace-regexp-in-string "/+\\'" "" path))
        (when (string-suffix-p ".git" path t)
          (setq path (substring path 0 -4)))
        (setq path (replace-regexp-in-string "/+\\'" "" path))
        (unless (or (string-empty-p host) (string-empty-p path))
          (concat host "/" path))))))

(defun workspace-bookmark--root-identity (root)
  "Return the normalized identity of the checkout whose root is ROOT, or nil.
ROOT must already be a checkout root, as the first element of
`workspace-bookmark--checkout-position' names it.  Callers ask through
`workspace-bookmark--memoised-identity'."
  (workspace-bookmark--normalize-identity
   (workspace-bookmark--remote-url root)))

(defvar workspace-bookmark--identity-memo (make-hash-table :test #'equal)
  "Memoised repository identities, keyed by checkout root.
Each value is (STAMP . IDENTITY), where STAMP is the value
`workspace-bookmark--config-stamp' returned when IDENTITY was computed
and IDENTITY may be nil.")

(defun workspace-bookmark-clear-identity-cache ()
  "Forget every memoised repository identity.

Each memoised identity is revalidated against the modification times of
its repository's own git config and of its checkout's `config.worktree',
so `git remote add', `set-url' and `remove', and `git config --worktree'
changes, are picked up without this command.  It is needed only after a
change the memo cannot see: a `url.*.insteadOf' rule (or anything else
`git remote get-url' honours) added to the global, XDG or system
gitconfig, or to an included config file.  Loading this module clears
the memo too."
  (interactive)
  (clrhash workspace-bookmark--identity-memo))

(defun workspace-bookmark--checkout-git-dir (root)
  "Return the git directory of the checkout at ROOT, or nil.  Runs no git.

ROOT's `.git' entry is the git directory itself when it is a directory
\(a main worktree), and names it when it is a file — the one-line
`gitdir: PATH' that git leaves at the root of a linked worktree, a
`--separate-git-dir' main worktree or a submodule, PATH being absolute
or relative to ROOT.  Nil when ROOT has no readable `.git' entry of
either kind.  Never signals."
  (condition-case nil
      (let ((dot-git (expand-file-name ".git" root)))
        (cond
         ((file-directory-p dot-git) dot-git)
         ((file-regular-p dot-git)
          (with-temp-buffer
            (insert-file-contents dot-git nil 0 4096)
            (goto-char (point-min))
            (when (looking-at "gitdir: \\(.+\\)$")
              (expand-file-name (match-string 1) root))))))
    (error nil)))

(defun workspace-bookmark--config-stamp (git-common-dir &optional root)
  "Return the stamp that validates a memoised identity, or nil.

The stamp is the list of two modification times: of GIT-COMMON-DIR's
`config', which `git remote' writes, and of the `config.worktree' in
the git directory of the checkout at ROOT (`workspace-bookmark--checkout-git-dir'),
which `git config --worktree' writes and `git remote get-url' honours
under `extensions.worktreeConfig'.  The second is nil while that file,
or ROOT, does not exist.  The whole stamp is nil when the shared
config's modification time cannot be read."
  (let ((shared (and git-common-dir
                     (file-attribute-modification-time
                      (file-attributes
                       (expand-file-name "config" git-common-dir))))))
    (and shared
         (list shared
               (let ((git-dir (and root
                                   (workspace-bookmark--checkout-git-dir root))))
                 (and git-dir
                      (file-attribute-modification-time
                       (file-attributes
                        (expand-file-name "config.worktree" git-dir)))))))))

(defun workspace-bookmark--memoised-identity (root git-common-dir)
  "Return the identity of the checkout at ROOT, memoised per ROOT.
GIT-COMMON-DIR is the repository's shared git directory.  The memoised
value is reused only while `workspace-bookmark--config-stamp' — the
shared config's modification time and that of ROOT's own
`config.worktree' — is unchanged, and never when the shared config's
modification time cannot be read.  A nil identity is memoised like any
other."
  (let ((stamp (workspace-bookmark--config-stamp git-common-dir root))
        (hit (gethash root workspace-bookmark--identity-memo)))
    (if (and hit stamp (equal (car hit) stamp))
        (cdr hit)
      (let ((identity (workspace-bookmark--root-identity root)))
        (puthash root (cons stamp identity) workspace-bookmark--identity-memo)
        identity))))

;; Every load starts with an empty memo: a reload may have changed the
;; normalizer whose output the memo holds.
(workspace-bookmark-clear-identity-cache)

(defun workspace-bookmark--record-location (record)
  "Return the single local file or directory RECORD refers to, or nil.

RECORD is a bookmark record (NAME . ALIST).  Returns the expanded
`filename' only when it is an absolute, local (non-TRAMP) path naming an
existing file or directory, and RECORD carries no nested sub-record or
window tree (a `buffer' or `window' property whose value is not a
string).  Runs no subprocess."
  (let ((filename (bookmark-prop-get record 'filename)))
    (and (stringp filename)
         (file-name-absolute-p filename)
         (not (file-remote-p filename))
         (not (seq-some (lambda (prop)
                          (let ((value (bookmark-prop-get record prop)))
                            (and value (not (stringp value)))))
                        '(buffer window)))
         (let ((location (expand-file-name filename)))
           (and (not (file-remote-p location))
                (file-exists-p location)
                location)))))

(defun workspace-bookmark--checkout-position (dir &optional committed)
  "Return (ROOT GIT-COMMON-DIR PREFIX [TREE]) for directory DIR, or nil.

ROOT is the checkout root as a directory name, GIT-COMMON-DIR the
absolute shared git directory, and PREFIX DIR's path relative to ROOT as
git spells it (\"sub/dir/\", or \"\" at the root).  One git subprocess.
Nil when DIR is in no work tree.

With COMMITTED non-nil, also return TREE, the object id of DIR's tree in
HEAD, and return nil when DIR is not in HEAD or there is no HEAD.  The
cost is bounded by DIR's depth, not by the size of its subtree."
  (let* ((out (apply #'workspace-bookmark--git
                     dir "rev-parse"
                     "--path-format=absolute" "--show-toplevel" "--git-common-dir"
                     "--show-prefix"
                     (and committed '("HEAD:./"))))
         (lines (and out (split-string out "\n"))))
    (when (and (= (length lines) (if committed 4 3))
               (file-name-absolute-p (nth 0 lines))
               (file-name-absolute-p (nth 1 lines)))
      (append (list (file-name-as-directory (nth 0 lines))
                    (nth 1 lines)
                    (nth 2 lines))
              (and committed (list (nth 3 lines)))))))

(defconst workspace-bookmark--root-relpath "./"
  "The relpath stamped for the repository root itself.
git spells the root's prefix as the empty string; see
`workspace-bookmark-relpath-property' for why it is stored as \"./\".")

(defun workspace-bookmark--relpath (location position)
  "Return LOCATION's repository-relative path, or nil when git does not track it.

POSITION is (ROOT GIT-COMMON-DIR PREFIX) from
`workspace-bookmark--checkout-position' for the directory holding
LOCATION (LOCATION itself, when it is a directory).  A file's relpath is
what `git ls-files --full-name' reports for it.  A directory's is its
PREFIX (`workspace-bookmark--root-relpath' at the root), and only when
POSITION carries the directory's HEAD tree (see
`workspace-bookmark--checkout-position').  Merge-conflict stages are
collapsed, so a conflicted file still has one relpath.  The result must
re-anchor on ROOT to the same file as LOCATION, or nil is returned."
  (let* ((root (nth 0 position))
         (relpath
          (if (file-directory-p location)
              (and (nth 3 position)
                   (let ((prefix (nth 2 position)))
                     (if (string-empty-p prefix)
                         workspace-bookmark--root-relpath
                       prefix)))
            (let ((reported (split-string
                             (or (workspace-bookmark--git
                                  location "--literal-pathspecs" "ls-files"
                                  "-z" "--full-name" "--"
                                  (file-name-nondirectory location))
                                 "")
                             "\0" t)))
              (setq reported (delete-dups reported))
              (and (= 1 (length reported)) (car reported))))))
    (and relpath
         (workspace--same-file-p (expand-file-name relpath root) location)
         relpath)))

(defun workspace-bookmark--coordinates (location)
  "Return (IDENTITY . RELPATH) for the local file or directory LOCATION, or nil.

Nil when LOCATION is in no git work tree, when git does not track it,
or when its repository has no usable remote.  LOCATION must already
have passed `workspace-bookmark--record-location'."
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (directory-p (file-directory-p location))
         (dir (if directory-p
                  (file-name-as-directory location)
                (file-name-directory location)))
         (position (workspace-bookmark--checkout-position dir directory-p))
         (relpath (and position (workspace-bookmark--relpath location position)))
         (identity (and relpath
                        (workspace-bookmark--memoised-identity
                         (nth 0 position) (nth 1 position)))))
    (and identity (cons identity relpath))))

(defun workspace-bookmark--with-coordinates (record coordinates)
  "Return a new record: RECORD's name and properties plus COORDINATES.

COORDINATES is (IDENTITY . RELPATH).  The three stamped properties are
appended together after RECORD's own properties, replacing any earlier
copies of them.  RECORD itself is not modified."
  (let ((stamped (list (cons workspace-bookmark-repo-property (car coordinates))
                       (cons workspace-bookmark-relpath-property (cdr coordinates))
                       (cons workspace-bookmark-format-property
                             workspace-bookmark-format-version))))
    (cons (car record)
          (append (seq-remove (lambda (prop)
                                (and (consp prop) (assq (car prop) stamped)))
                              (bookmark-get-bookmark-record record))
                  stamped))))

(defun workspace-bookmark--stamp-record (record)
  "Return RECORD with portable coordinates added, or RECORD unchanged.

Intended as `:filter-return' advice on `bookmark-make-record'.  A record
for a single tracked file or directory inside a repository with a remote
comes back as a NEW record carrying all three stamped properties
\(`workspace-bookmark-repo-property', `workspace-bookmark-relpath-property',
`workspace-bookmark-format-property'); any other record comes back
unchanged and `eq' to RECORD.

Never prompts, never messages, never touches the bookmark name.  Any
internal error is swallowed and the original RECORD returned: failing to
stamp must not prevent a bookmark from being created."
  (condition-case nil
      (let* ((location (workspace-bookmark--record-location record))
             (coordinates (and location
                               (workspace-bookmark--coordinates location))))
        (if coordinates
            (workspace-bookmark--with-coordinates record coordinates)
          record))
    (error record)))

(defun workspace-bookmark--substituted-record (record path)
  "Return a copy of bookmark RECORD whose `filename' is PATH.

The copy shares no cons cell with RECORD: `copy-tree' copies the
\(NAME . ALIST) cons, the alist and every property cons, so setting
`filename' on the copy cannot reach `bookmark-alist'.  Every other
property — the handler above all — is kept.  This is the ONLY place a
substituted record is built.  Signals when PATH is not a string."
  (unless (stringp path)
    (error "Not a path to substitute into %S: %S"
           (bookmark-name-from-full-record record) path))
  (let ((copy (copy-tree record)))
    (bookmark-prop-set copy 'filename path)
    copy))

(defun workspace-bookmark--plan-to-open (record resolution kind)
  "Return the plan that opens RESOLUTION's :path for RECORD, or a decline.

KIND is `call-through' — the seam's own `resolved' jump — or `view',
the canonical read-only view chosen at the missing-repo prompt.  The
:path is pre-flighted with `file-readable-p' immediately before the
copy is built (a readable directory counts), so the host is never
handed a copy it cannot read: one that has vanished since the resolver
looked declines with `workspace-bookmark--vanished-message', one that
is there but cannot be read with
`workspace-bookmark--unreadable-message'.  The copy comes from
`workspace-bookmark--substituted-record', the only place one is built.  Returns (call-through . COPY), (view COPY . CHECKOUT) or
\(decline . MESSAGE); signals for any other KIND, and for a `view'
whose RESOLUTION names no :checkout — a view that cannot say which
canonical checkout it shows is never opened unmarked."
  (let ((path (plist-get resolution :path))
        (checkout (plist-get resolution :checkout)))
    (cond
     ((not (and (stringp path) (file-exists-p path)))
      (cons 'decline (workspace-bookmark--vanished-message record resolution)))
     ((not (file-readable-p path))
      (cons 'decline (workspace-bookmark--unreadable-message record resolution)))
     ((eq kind 'call-through)
      (cons 'call-through (workspace-bookmark--substituted-record record path)))
     ((eq kind 'view)
      (unless (stringp checkout)
        (error "No canonical checkout to view %S from: %S"
               (bookmark-name-from-full-record record) checkout))
      (cons 'view (cons (workspace-bookmark--substituted-record record path)
                        checkout)))
     (t (error "Not a jump plan kind: %S" kind)))))

(defun workspace-bookmark--landed-p (origin record)
  "Return non-nil when the jump of the substituted RECORD landed somewhere.
ORIGIN is the buffer that was current before the host was called.  The
jump landed when the current buffer is no longer ORIGIN, or now visits
RECORD's `filename' (compared by file identity)."
  (or (not (eq (current-buffer) origin))
      (let ((file (buffer-file-name)))
        (and file
             (workspace--same-file-p
              file (bookmark-get-filename record))))))

(defconst workspace-bookmark--declining-outcomes
  '(not-on-branch ambiguous missing-repo no-clone)
  "The outcomes `workspace-bookmark--decline-message' can report.
The four that open nothing by themselves: `not-on-branch', `ambiguous',
`missing-repo' (where it is not offered) and `no-clone'.  Read by
`workspace-bookmark--decline-message' and
`workspace-bookmark--after-add-message'; which outcome gets which plan
is decided only by `workspace-bookmark--plan-for-outcome'.")

(defun workspace-bookmark--decline-message (record resolution)
  "Return the report for declining to jump to RECORD.

RESOLUTION is the plist `workspace-bookmark--resolve' returned; its
outcome must be a member of `workspace-bookmark--declining-outcomes'.
Signals for any other outcome."
  (let ((name (bookmark-name-from-full-record record))
        (outcome (plist-get resolution :outcome))
        (identity (plist-get resolution :identity))
        (relpath (plist-get resolution :relpath))
        (checkout (plist-get resolution :checkout))
        (candidates (plist-get resolution :candidates))
        (abbreviate (lambda (dir)
                      (if (stringp dir) (abbreviate-file-name dir) "?"))))
    (unless (memq outcome workspace-bookmark--declining-outcomes)
      (error "Not an outcome that declines: %S" outcome))
    (pcase outcome
      ('not-on-branch
       (format "Bookmark %S: %s is not on this branch of %s (%s); nothing opened"
               name relpath identity (funcall abbreviate checkout)))
      ('ambiguous
       (format "Bookmark %S: %d checkouts of %s in view, none chosen: %s"
               name (length candidates) identity
               (mapconcat abbreviate candidates ", ")))
      ('missing-repo
       (format "Bookmark %S: this workspace holds no checkout of %s (canonical checkout %s); nothing opened"
               name identity (funcall abbreviate checkout)))
      ('no-clone
       (format "Bookmark %S: no checkout of %s in view; nothing opened"
               name identity))
      (_ (error "No report written for the declining outcome %S" outcome)))))

(defun workspace-bookmark--vanished-message (record resolution)
  "Return the report for a `resolved' RECORD whose location has vanished.

RESOLUTION is the resolver's plist; its :path passed the resolver's
existence check but fails the seam's pre-flight.  A seam-local decline
reason, not a resolution outcome."
  (format "Bookmark %S: %s vanished from %s before it could be opened; nothing opened"
          (bookmark-name-from-full-record record)
          (plist-get resolution :relpath)
          (let ((checkout (plist-get resolution :checkout)))
            (if (stringp checkout) (abbreviate-file-name checkout) "?"))))

(defun workspace-bookmark--unreadable-message (record resolution)
  "Return the report for a `resolved' RECORD whose location cannot be read.

RESOLUTION is the resolver's plist; its :path exists but fails the
seam's `file-readable-p' pre-flight — its permissions deny reading it.
The report names that :path itself, unabbreviated.  A seam-local
decline reason, not a resolution outcome."
  (format "Bookmark %S: %s exists but cannot be read; nothing opened"
          (bookmark-name-from-full-record record)
          (plist-get resolution :path)))

(defun workspace-bookmark--plan-for-outcome (record resolution context original)
  "Return the jump plan for RECORD, whose RESOLUTION was decided in CONTEXT.

The ONE site that maps a resolution outcome to a plan.  RESOLUTION is
the plist `workspace-bookmark--resolve' returned for RECORD, a bookmark
record.  CONTEXT says where it was decided: `in-workspace', the seam's
own decision (`workspace-bookmark--jump-plan') against a current or
chosen workspace, and also for an UNSTAMPED record jumped with no
current workspace, which keeps stock behaviour; `none', the off-workspace fallback
resolved with no workspace (`workspace-bookmark--no-workspace-plan');
`after-add', the re-resolve after a worktree was added
\(`workspace-bookmark--add-worktree-and-replan').  ORIGINAL is the
argument the seam received; exactly one cell reads it.

Returns (call-through . ARG), (decline . MESSAGE),
\(view COPY . CHECKOUT) or (cancel).  The cells, by outcome:
  non-portable    in-workspace: (call-through . ORIGINAL) — the ONLY
                  cell that hands the host the stored path, the answer
                  stock Emacs gives for a record with no usable stamp;
                  none / after-add: decline.
  resolved        in-workspace / after-add:
                  `workspace-bookmark--plan-to-open' (pre-flight, then
                  a copy); none: decline.
  missing-repo    in-workspace / none:
                  `workspace-bookmark--offer-missing-repo'; after-add:
                  decline.
  not-on-branch, ambiguous, no-clone: decline in every context.
A decline carries `workspace-bookmark--decline-message' in
`in-workspace' and `none' — except `non-portable' and `resolved' with
no workspace, which name the missing workspace — and
`workspace-bookmark--after-add-message' in `after-add'.  Any other
outcome, and any other context, signals; the caller's barrier catches.
Four cells cannot be reached from `workspace-bookmark--resolve' and are
kept as a defensive decline: `none' with `non-portable', `resolved' or
`not-on-branch', and `after-add' with `non-portable'."
  (pcase (plist-get resolution :outcome)
    ((and 'non-portable (guard (eq context 'in-workspace)))
     (cons 'call-through original))
    ((and 'resolved (guard (memq context '(in-workspace after-add))))
     (workspace-bookmark--plan-to-open record resolution 'call-through))
    ((and 'missing-repo (guard (memq context '(in-workspace none))))
     (workspace-bookmark--offer-missing-repo record resolution))
    ((and (or 'not-on-branch 'ambiguous 'no-clone)
          (guard (memq context '(in-workspace none))))
     (cons 'decline (workspace-bookmark--decline-message record resolution)))
    ((and (or 'non-portable 'resolved) (guard (eq context 'none)))
     (cons 'decline
           (format "Bookmark %S: no workspace chosen, and it cannot be resolved without one (%s); nothing opened"
                   (bookmark-name-from-full-record record)
                   (plist-get resolution :outcome))))
    ((and (or 'non-portable 'missing-repo 'not-on-branch 'ambiguous 'no-clone)
          (guard (eq context 'after-add)))
     (cons 'decline (workspace-bookmark--after-add-message record resolution)))
    (outcome
     (error "Not a bookmark-resolution outcome in a jump context: %S in %S"
            outcome context))))

(defun workspace-bookmark--jump-plan (bookmark-name-or-record &optional workspace)
  "Decide what the jump seam does with BOOKMARK-NAME-OR-RECORD.

Returns (call-through . ARG), meaning hand ARG to the host — ARG is
the original argument, or a substituted copy for `resolved' —
\(decline . MESSAGE), meaning open nothing and report MESSAGE, or,
from the missing-repo prompt, (view COPY . CHECKOUT) — hand COPY to
the host, then mark the buffer as a read-only view of CHECKOUT — or
\(cancel), meaning open nothing and say nothing.

WORKSPACE is the name of the workspace to resolve against; it
defaults to the current one.  Decided before the resolver is asked: a
value that is not a bookmark record calls through (the host signals
for it); no workspace on a STAMPED record is the off-workspace
hand-off, `workspace-bookmark--off-workspace-plan', which establishes
a workspace and re-enters here with its name.  Otherwise the record is
resolved against WORKSPACE and the outcome mapped in context
`in-workspace' by `workspace-bookmark--plan-for-outcome', the one
outcome-to-plan mapping.  May signal; the seam's barrier catches."
  (let ((record (bookmark-get-bookmark bookmark-name-or-record 'noerror))
        (workspace (or workspace (workspace-bookmark--current-workspace))))
    (cond
     ((not (consp record))
      (cons 'call-through bookmark-name-or-record))
     ((and (null workspace)
           (workspace-bookmark--stamped-coordinates record))
      (workspace-bookmark--off-workspace-plan record bookmark-name-or-record))
     (t
      (workspace-bookmark--plan-for-outcome
       record (workspace-bookmark--resolve record workspace)
       'in-workspace bookmark-name-or-record)))))

(defun workspace-bookmark--plan-after-error (bookmark-name-or-record err)
  "Return the seam's plan when ERR was signalled deciding BOOKMARK-NAME-OR-RECORD's.

The barrier's answer.  A STAMPED record — `workspace-bookmark--stamped-coordinates'
non-nil for the normalised argument — declines, naming ERR: its stored
path may be another workspace's copy, and a bug in the decision is no
licence to open it.  An unstamped record, or an argument that is no
record, calls through with the ORIGINAL argument: stock behaviour, the
right answer for such a record.  Never signals: the normalisation is
guarded and the stamp test cannot fail."
  (let ((record (condition-case nil
                    (bookmark-get-bookmark bookmark-name-or-record 'noerror)
                  (error nil))))
    (if (workspace-bookmark--stamped-coordinates record)
        (cons 'decline
              (format "Bookmark %S: the jump decision failed (%s); nothing opened"
                      (bookmark-name-from-full-record record)
                      (error-message-string err)))
      (cons 'call-through bookmark-name-or-record))))

(defun workspace-bookmark--handle-bookmark-advice (orig-fn bookmark-name-or-record &rest args)
  "Resolve BOOKMARK-NAME-OR-RECORD against the current workspace, then jump.

Intended as `:around' advice on `bookmark-handle-bookmark'; ORIG-FN is
the advised function and ARGS its remaining arguments.  Acts on the
plan `workspace-bookmark--jump-plan' returns: calls ORIG-FN with the
original argument (`non-portable' only: a record with no usable
stamp), with a substituted copy (`resolved', or the canonical
read-only view chosen at the missing-repo prompt), or not at all — a
decline signals a `user-error' naming the bookmark and opens nothing,
`workspace-restore-declined' (its child, core's decline condition)
while `workspace--restoring-layout-leaf' is non-nil; a cancel at that
prompt signals `quit'.  A stamped record with no
current workspace goes through the off-workspace plan
\(`workspace-bookmark--off-workspace-plan'), which establishes a
workspace and resolves against it, and never calls through with the
original argument.  An error in this seam's own decision is answered
by `workspace-bookmark--plan-after-error': a STAMPED record declines,
naming the error, and opens nothing; an unstamped record, or an
argument that is no record, falls through to ORIG-FN with the ORIGINAL
argument, so a failure affecting one bookmark cannot affect another or
the bookmark file; `quit' propagates.  A decline reaches every caller
of the host alike — `bookmark-insert' calls it directly, so a declined
stamped record aborts the insert with the `user-error'.  After a
substituted jump that landed in a buffer (the current buffer changed,
or visits the substituted location): a view is marked read-only there
by `workspace-bookmark--mark-canonical-view', and a jump by name sets
`bookmark-current-bookmark' there as the host sets it for a plain one.
Before either, the host call is offered to
`workspace-bookmark--note-jump-target', which records where the jump
landed when it is the outer jump of a restore-then-jump.  The
originating buffer is never touched."
  (let ((plan (condition-case err
                  (workspace-bookmark--jump-plan bookmark-name-or-record)
                (error (workspace-bookmark--plan-after-error
                        bookmark-name-or-record err))))
        (origin (current-buffer)))
    (pcase plan
      (`(decline . ,message)
       (if (and (boundp 'workspace--restoring-layout-leaf)
                workspace--restoring-layout-leaf)
           (signal 'workspace-restore-declined (list message))
         (user-error "%s" message)))
      (`(cancel)
       (signal 'quit nil))
      ((or `(call-through . ,arg) `(view ,arg . ,_))
       (let ((checkout (and (eq (car plan) 'view) (cddr plan))))
         (prog1 (apply orig-fn arg args)
           (workspace-bookmark--note-jump-target origin arg)
           (when (and (not (eq arg bookmark-name-or-record))
                      (workspace-bookmark--landed-p origin arg))
             (when (eq (car plan) 'view)
               (workspace-bookmark--mark-canonical-view arg checkout))
             (when (stringp bookmark-name-or-record)
               (setq bookmark-current-bookmark bookmark-name-or-record))))))
      (_ (error "Not a jump plan: %S" plan)))))

(defconst workspace-bookmark--missing-repo-actions
  '((add-worktree ?a "add worktree here"
     "Add a worktree of the repository to the current workspace, resolve the bookmark again and open it there")
    (view-canonical-read-only ?v "view read-only"
     "Open the canonical checkout's copy in view-mode, marked as the canonical checkout")
    (cancel ?c "cancel"
     "Open nothing"))
  "The closed set of answers to the `missing-repo' prompt, in prompt order.
Each entry is (ACTION KEY NAME DESCRIPTION): ACTION names the answer;
KEY, NAME and DESCRIPTION are its `read-multiple-choice' entry.  Which
entries are offered is decided by
`workspace-bookmark--missing-repo-withheld'; what each does, by
`workspace-bookmark--offer-missing-repo'.")

(declare-function workspace--integration-menu-command-p "integrations" (command))

(defun workspace-bookmark--add-worktree-runnable-p ()
  "Return non-nil when git-worktrees' add-worktree command can run.
That is when `jf/workspace--add-worktree' is defined AND registered as
the menu command of an integration, asked of core through
`workspace--integration-menu-command-p' (called only when bound).
git-worktrees defines the command whenever it is loaded, but it can run
only once the integration that provides it has registered it, which
waits for the command's own dependencies."
  (and (fboundp 'jf/workspace--add-worktree)
       (fboundp 'workspace--integration-menu-command-p)
       (workspace--integration-menu-command-p 'jf/workspace--add-worktree)
       t))

(defun workspace-bookmark--unusable-phrase (workspace reason)
  "Return the prompt phrase for WORKSPACE (a name) being unusable for REASON.
REASON is what `workspace-bookmark--workspace-unusable' returned for
WORKSPACE; the phrase names it and, for a home that is remote or gone,
the home."
  (let ((home (workspace-bookmark--workspace-home workspace)))
    (pcase reason
      ('unregistered
       (format "workspace %S is not registered" workspace))
      ('broken
       (format "workspace %S is broken (tagged at load: its home was missing or remote); re-anchor or purge it first"
               workspace))
      ('no-home
       (format "workspace %S has no home to add a worktree to" workspace))
      ('home-remote
       (format "the home of workspace %S, %s, is remote" workspace home))
      ('home-missing
       (format "the home of workspace %S, %s, no longer exists; re-anchor or purge it first"
               workspace (abbreviate-file-name home)))
      (_ (error "Not an unusable-workspace reason: %S" reason)))))

(defun workspace-bookmark--missing-repo-withheld (resolution workspace)
  "Return the `missing-repo' actions not offered for RESOLUTION, with reasons.

An alist of (ACTION . REASON), REASON a phrase for the prompt.
`add-worktree' needs a current WORKSPACE (a name), the git-worktrees
command `jf/workspace--add-worktree' runnable
\(`workspace-bookmark--add-worktree-runnable-p'), and WORKSPACE usable
\(`workspace-bookmark--workspace-unusable' nil: registered, not broken,
its home an existing local directory) — asked in that order, the first
failing condition giving the reason; `view-canonical-read-only' needs
a non-nil :path, the relpath present under the canonical checkout.
`cancel' is always offered."
  (let ((withheld nil)
        (unusable (and workspace
                       (workspace-bookmark--workspace-unusable workspace))))
    (cond
     ((null workspace)
      (push (cons 'add-worktree "no workspace is current to add a worktree to")
            withheld))
     ((not (workspace-bookmark--add-worktree-runnable-p))
      (push (cons 'add-worktree "the git worktree integration is not loaded")
            withheld))
     (unusable
      (push (cons 'add-worktree
                  (workspace-bookmark--unusable-phrase workspace unusable))
            withheld)))
    (unless (plist-get resolution :path)
      (push (cons 'view-canonical-read-only
                  (format "%s is absent there" (plist-get resolution :relpath)))
            withheld))
    (nreverse withheld)))

(defun workspace-bookmark--missing-repo-choices (withheld)
  "Return the `read-multiple-choice' entries for the actions not in WITHHELD."
  (seq-keep (lambda (entry)
              (unless (assq (car entry) withheld) (cdr entry)))
            workspace-bookmark--missing-repo-actions))

(defun workspace-bookmark--missing-repo-action (answer)
  "Return the action symbol for ANSWER, the entry `read-multiple-choice' returned."
  (or (car (seq-find (lambda (entry) (eq (nth 1 entry) (car answer)))
                     workspace-bookmark--missing-repo-actions))
      (error "Not a missing-repo answer: %S" answer)))

(defun workspace-bookmark--missing-repo-prompt (record resolution workspace withheld)
  "Return the prompt for RECORD, whose RESOLUTION is `missing-repo', in WORKSPACE.
Names the bookmark, WORKSPACE (a name, or nil for none), the repository
identity and the canonical checkout, then states each entry of WITHHELD
\(see `workspace-bookmark--missing-repo-withheld') so the user knows why
an option is absent."
  (let ((checkout (plist-get resolution :checkout)))
    (concat
     (format "Bookmark %S: workspace %s holds no checkout of %s; canonical checkout %s."
             (bookmark-name-from-full-record record)
             (if workspace (format "%S" workspace) "(none)")
             (plist-get resolution :identity)
             (if (stringp checkout) (abbreviate-file-name checkout) "?"))
     (mapconcat (lambda (entry)
                  (format " (%s not offered: %s)"
                          (nth 2 (assq (car entry) workspace-bookmark--missing-repo-actions))
                          (cdr entry)))
                withheld ""))))

(defvar workspace--restoring-layout-leaf)

(defun workspace-bookmark--can-prompt-p ()
  "Return non-nil when a prompt may be put and someone can answer it.
Nil in a non-interactive Emacs; nil while `inhibit-interaction' is
bound, because `read-multiple-choice' does not honour that variable
itself (it swallows the `inhibited-interaction' signal and loops); and
nil while core restores a layout leaf (`workspace--restoring-layout-leaf',
read only when bound), where a prompt would be one question per leaf."
  (and (not noninteractive)
       (not inhibit-interaction)
       (not (and (boundp 'workspace--restoring-layout-leaf)
                 workspace--restoring-layout-leaf))))

(defun workspace-bookmark--offer-missing-repo (record resolution)
  "Offer the bounded choice for RECORD, whose RESOLUTION is `missing-repo'.

Prompts with `read-multiple-choice' over the actions
`workspace-bookmark--missing-repo-withheld' leaves offered — add a
worktree of the canonical checkout to the current workspace, view the
canonical copy read-only, or cancel — and returns the seam's plan for
the answer: (view COPY . CHECKOUT), (call-through . COPY) once a
worktree was added and RECORD re-resolved, (decline . MESSAGE), or
\(cancel).  Opens nothing itself.  Signals `quit' when the prompt is
quit; any other error inside the choice declines, so the seam's
barrier can never answer it with the stored path.  When no one can
answer (`workspace-bookmark--can-prompt-p' is nil) declines with the
seam's `missing-repo' report instead of prompting."
  (condition-case err
      (if (not (workspace-bookmark--can-prompt-p))
          (cons 'decline (workspace-bookmark--decline-message record resolution))
        (let* ((workspace (workspace-bookmark--current-workspace))
               (withheld (workspace-bookmark--missing-repo-withheld
                          resolution workspace))
               (answer (read-multiple-choice
                        (workspace-bookmark--missing-repo-prompt
                         record resolution workspace withheld)
                        (workspace-bookmark--missing-repo-choices withheld))))
          (pcase (workspace-bookmark--missing-repo-action answer)
            ('cancel (list 'cancel))
            ('view-canonical-read-only
             (workspace-bookmark--plan-to-open record resolution 'view))
            ('add-worktree
             (workspace-bookmark--add-worktree-and-replan
              record resolution workspace))
            (action (error "Not a missing-repo action: %S" action)))))
    (error
     (cons 'decline
           (format "Bookmark %S: the missing-repo choice failed (%s); nothing opened"
                   (bookmark-name-from-full-record record)
                   (error-message-string err))))))

(defvar-local workspace-bookmark--canonical-view nil
  "The canonical checkout this buffer views from the missing-repo prompt, or nil.
Set once by `workspace-bookmark--mark-canonical-view'; survives `C-x C-q'
and, being permanent-local, a `revert-buffer' that re-runs the major mode.")
(put 'workspace-bookmark--canonical-view 'permanent-local t)

(defun workspace-bookmark--canonical-branch (checkout)
  "Return the branch CHECKOUT has checked out, or a phrase for a detached HEAD."
  (or (workspace-bookmark--git checkout "symbolic-ref" "--short" "--quiet" "HEAD")
      "detached HEAD"))

(defun workspace-bookmark--canonical-marker-p (element)
  "Return non-nil when mode-line ELEMENT is the canonical-view marker."
  (and (stringp element)
       (> (length element) 0)
       (get-text-property 0 'workspace-bookmark-canonical-view element)))

(defun workspace-bookmark--show-canonical-marker (checkout)
  "Append a marker naming CHECKOUT to the buffer's mode-line identification.
Buffer-local, and never twice: a marker already present is left alone."
  (unless (and (proper-list-p mode-line-buffer-identification)
               (seq-some #'workspace-bookmark--canonical-marker-p
                         mode-line-buffer-identification))
    (setq mode-line-buffer-identification
          (list "" mode-line-buffer-identification
                (propertize
                 (format " [canonical %s]"
                         (abbreviate-file-name (directory-file-name checkout)))
                 'face 'warning
                 'workspace-bookmark-canonical-view t
                 'help-echo (format "Canonical checkout %s, not a workspace checkout: opened read-only from a bookmark"
                                    checkout))))))

(defun workspace-bookmark--restore-canonical-view ()
  "Return a function that puts the canonical view back after a revert.
Buffer-local on `revert-buffer-restore-functions', and permanent like
`workspace-bookmark--canonical-view', so it outlives the revert's
`kill-all-local-variables'.  Called before the revert; the function it
returns runs after it, once the stock read-only state is restored, and
re-adds the marker and — only if it was on before the revert —
`view-mode', so a revert never undoes a deliberate `C-x C-q'."
  (let ((buffer (current-buffer))
        (viewing view-mode))
    (lambda ()
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when workspace-bookmark--canonical-view
            (when (and viewing (not view-mode))
              (view-mode 1))
            (workspace-bookmark--show-canonical-marker
             workspace-bookmark--canonical-view)))))))
(put 'workspace-bookmark--restore-canonical-view 'permanent-local-hook t)

(defun workspace-bookmark--mark-canonical-view (record checkout)
  "Make the current buffer an unmistakable read-only view of CHECKOUT's copy.

The current buffer is where the host landed RECORD's substituted copy.
Turns on `view-mode' (not in Dired, which is read-only already), appends
a marker naming CHECKOUT to the buffer's `mode-line-buffer-identification'
— once, buffer-locally, so it outlives the echo-area message, a later
`C-x C-q' and a mode-resetting `revert-buffer' — and reports the path
and CHECKOUT's branch in one message."
  (unless (derived-mode-p 'dired-mode)
    (view-mode 1))
  (unless workspace-bookmark--canonical-view
    (setq workspace-bookmark--canonical-view checkout))
  (workspace-bookmark--show-canonical-marker workspace-bookmark--canonical-view)
  (add-hook 'revert-buffer-restore-functions
            #'workspace-bookmark--restore-canonical-view nil t)
  (message "Bookmark %S: viewing %s read-only in the canonical checkout %s (branch %s)"
           (bookmark-name-from-full-record record)
           (abbreviate-file-name (or (buffer-file-name) default-directory))
           (abbreviate-file-name (directory-file-name checkout))
           (workspace-bookmark--canonical-branch checkout)))

(declare-function workspace--integration-payload "integrations" (name home context))
(defvar workspace-bookmark--discovery-memo)

(defun workspace-bookmark--add-worktree-payload (workspace checkout)
  "Return the anchor payload for adding a worktree of CHECKOUT to WORKSPACE.
What `workspace--integration-payload' builds for WORKSPACE, its home
and the context `bookmark-jump', plus :source-repo, the canonical
checkout the resolver chose, so the git-worktrees command need not ask
for one.  Builds nothing of its own: the add-worktree action is
offered only when `workspace-bookmark--add-worktree-runnable-p' holds,
which asks core's `workspace--integration-menu-command-p', defined
beside the constructor, so the constructor is loaded by the time a
payload is built."
  (append (workspace--integration-payload
           workspace (workspace-bookmark--workspace-home workspace) 'bookmark-jump)
          (list :source-repo checkout)))

(defun workspace-bookmark--after-add-message (record resolution)
  "Return the report for RECORD when RESOLUTION opens nothing after a worktree add.
A member of `workspace-bookmark--declining-outcomes' gets its usual
report, marked as coming after the add; any other outcome is named."
  (let ((outcome (plist-get resolution :outcome)))
    (if (memq outcome workspace-bookmark--declining-outcomes)
        (format "%s (after adding a worktree)"
                (workspace-bookmark--decline-message record resolution))
      (format "Bookmark %S: resolves %s after adding a worktree; nothing opened"
              (bookmark-name-from-full-record record) outcome))))

(defun workspace-bookmark--add-worktree-and-replan (record resolution workspace)
  "Add a worktree of RESOLUTION's :checkout to WORKSPACE, then replan RECORD.

Calls git-worktrees' `jf/workspace--add-worktree' with
`workspace-bookmark--add-worktree-payload'; the action is offered only
when `workspace-bookmark--add-worktree-runnable-p' holds, so the
command is loaded by the time this runs.  On `ok' the resolver runs
once more against WORKSPACE, outside any enclosing discovery memo (the
add changed the repository), and the answer is mapped in context
`after-add' by `workspace-bookmark--plan-for-outcome': `resolved'
continues through `workspace-bookmark--plan-to-open', pre-flight
included; anything else declines with
`workspace-bookmark--after-add-message', never a second prompt.
\(failed . REASON) and `skipped' decline naming the reason.  Signals
on an unknown result; the caller's barrier declines.

Asks `workspace-bookmark--workspace-unusable' of WORKSPACE again
immediately before the call: the offer's answer may be stale after the
prompt, and a reason declines as (failed . PHRASE), PHRASE the one
`workspace-bookmark--unusable-phrase' gives the prompt, without calling
the command."
  (let* ((name (bookmark-name-from-full-record record))
         (unusable (workspace-bookmark--workspace-unusable workspace))
         (result
          (if unusable
              (cons 'failed
                    (workspace-bookmark--unusable-phrase workspace unusable))
            (funcall 'jf/workspace--add-worktree
                     (workspace-bookmark--add-worktree-payload
                      workspace (plist-get resolution :checkout))))))
    (pcase result
      ('ok
       (let ((workspace-bookmark--discovery-memo nil))
         (workspace-bookmark--plan-for-outcome
          record (workspace-bookmark--resolve record workspace) 'after-add nil)))
      (`(failed . ,reason)
       (cons 'decline
             (format "Bookmark %S: no worktree added (%s); nothing opened"
                     name reason)))
      ;; `skipped' is a legitimate handler result
      ;; (register/vocabulary/workspace-integration-outcome).  The
      ;; vocabulary is read STRICTLY here — `ok', (failed . REASON) and
      ;; `skipped' are the whole of it, and anything else signals into
      ;; the chooser's barrier, i.e. declines — where core's
      ;; `workspace--run-one-integration' reads an unknown handler value
      ;; leniently as `ok'.
      ('skipped
       (cons 'decline
             (format "Bookmark %S: the git worktree integration skipped the add; nothing opened"
                     name)))
      (_ (error "Not a workspace-integration outcome: %S" result)))))

(declare-function workspace--broken-p "data-model" (ws))
(declare-function workspace-restore "persistence" (name))

(defvar workspace-bookmark--awaiting-jump-target nil
  "Non-nil from the return of a jump-triggered restore until the jump is noted.
Raised by `workspace-bookmark--restore-for-jump' once `workspace-restore'
has RETURNED — after the restore's own leaf jumps, so none of those can
be taken for the jump — and lowered by the first
`workspace-bookmark--note-jump-target' after it (the outer jump's own
host call) or, when that jump never reached the host, by
`workspace-bookmark--finish-restore'.  Never `let'-bound: the finish
step runs from a timer.  Read by `workspace-bookmark--note-jump-target'
only; it has nothing to do with autosave.")

(defvar workspace-bookmark--jump-target nil
  "The buffer the outer jump of a restore-then-jump landed in, or nil.
Recorded by `workspace-bookmark--note-jump-target'; shown in the
selected window of the painted layout, then cleared, by
`workspace-bookmark--finish-restore'.")

(defconst workspace-bookmark--finish-delay 0.001
  "Seconds from queuing until the restore-then-jump finish step fires.
Non-zero on purpose: the restore's deferred paint was queued earlier
with `run-at-time' nil, and timer.el fires equal-time timers newest
first, so only a strictly later time orders the finish step after the
paint whatever the clock read.")

(defvar workspace-bookmark--workspace-history nil
  "Minibuffer history for the off-workspace workspace prompt.")

(defun workspace-bookmark--workspace-unusable (name)
  "Return why the workspace NAME cannot be used, or nil when it can.

The one health notion of this module, read by
`workspace-bookmark--missing-repo-withheld' (the add-worktree offer),
`workspace-bookmark--add-worktree-and-replan' (the same gate again at
the call) and `workspace-bookmark--offerable-workspaces' (the
off-workspace picker).  NAME is usable when it is registered, not tagged broken
\(`workspace--broken-p') and its `:home' is an existing local
directory.  Otherwise the reason, a symbol, the first that applies:
`unregistered' (NAME is nil, not in the registry, or the registry is
not loaded), `broken' (the tag persistence sets at load when the home
was missing or remote), `no-home' (no `:home' on the plist), `home-remote'
\(remote homes are unsupported), `home-missing' (the home is not a
directory on disk, however it went — the broken tag is load-time only
and cannot record a deletion since).  `file-remote-p' is asked before
`file-directory-p', so a remote home is never stat'ed."
  (let ((ws (and name
                 (boundp 'workspace--registry)
                 (hash-table-p workspace--registry)
                 (gethash name workspace--registry))))
    (cond
     ((null ws) 'unregistered)
     ((and (fboundp 'workspace--broken-p) (workspace--broken-p ws)) 'broken)
     (t
      (let ((home (workspace-bookmark--workspace-home name)))
        (cond
         ((not (stringp home)) 'no-home)
         ((file-remote-p home) 'home-remote)
         ((not (file-directory-p home)) 'home-missing)))))))

(defun workspace-bookmark--offerable-workspaces (identity)
  "Return the workspaces that may serve a jump into IDENTITY, holders first.
An ordered list of (NAME . HOLDS-P): every registered workspace that is
usable (`workspace-bookmark--workspace-unusable' nil), those holding
one or more checkouts of IDENTITY (HOLDS-P non-nil) before those
holding none, registry order within each group.  Asks
`workspace-bookmark--workspace-checkouts' once per workspace."
  (let (holders others)
    (dolist (name (workspace-bookmark--registered-names))
      (unless (workspace-bookmark--workspace-unusable name)
        (if (workspace-bookmark--workspace-checkouts identity name)
            (push (cons name t) holders)
          (push (cons name nil) others))))
    (nconc (nreverse holders) (nreverse others))))

(defun workspace-bookmark--workspace-table (candidates identity)
  "Return a completion table over CANDIDATES that keeps their order and annotates them.
CANDIDATES is what `workspace-bookmark--offerable-workspaces' returned
for IDENTITY.  The metadata pins `display-sort-function' and
`cycle-sort-function' to `identity', so a front-end cannot sort the
holders-first order away, and annotates each name with whether it
holds a checkout of IDENTITY."
  (let ((names (mapcar #'car candidates)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (category . workspace)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (annotation-function
             . ,(lambda (name)
                  (if (cdr (assoc name candidates))
                      (format "  holds a checkout of %s" identity)
                    "  no checkout"))))
        (complete-with-action action names string predicate)))))

(defun workspace-bookmark--choose-workspace (candidates identity)
  "Ask which of CANDIDATES should serve a jump into IDENTITY; return its name, or nil.
CANDIDATES is the non-empty list `workspace-bookmark--offerable-workspaces'
returned for IDENTITY; it is offered through `completing-read' with a
required match, the first holder as the default when there is one.
With a default, an empty answer returns that default (`completing-read'
substitutes it), so \"no workspace\" cannot be chosen.  Without one (no
candidate holds a checkout), an empty answer returns nil — \"no
workspace\", which the fallback acts on.  Does not catch `quit': C-g
ends the jump, nothing else is asked.  The caller has checked
`workspace-bookmark--can-prompt-p'."
  (let* ((default (and (cdr (car candidates)) (car (car candidates))))
         (choice (completing-read
                  (format-prompt "Restore a workspace for %s" default identity)
                  (workspace-bookmark--workspace-table candidates identity)
                  nil t nil 'workspace-bookmark--workspace-history default)))
    (and (stringp choice) (not (string-empty-p choice)) choice)))

(defun workspace-bookmark--finish-restore ()
  "Deferred tail of a restore-then-jump: show the jump's buffer in the painted layout.
Queued by `workspace-bookmark--restore-for-jump' to fire strictly after
the layout's own deferred `window-state-put' (see
`workspace-bookmark--finish-delay').  Shows
`workspace-bookmark--jump-target', when the outer jump landed in a live
buffer, in the selected window of the painted layout — same window, no
split — then clears the target and
`workspace-bookmark--awaiting-jump-target'.  The clearing is an
`unwind-protect' cleanup, so a display that signals still clears both."
  (unwind-protect
      (let ((target workspace-bookmark--jump-target))
        (when (buffer-live-p target)
          (pop-to-buffer-same-window target)))
    (setq workspace-bookmark--jump-target nil
          workspace-bookmark--awaiting-jump-target nil)))

(defun workspace-bookmark--restore-for-jump (name)
  "Restore workspace NAME for a bookmark jump; return NAME.
Calls `workspace-restore', and only once it has RETURNED clears any
target and raises `workspace-bookmark--awaiting-jump-target', so the
restore's own leaf jumps are never taken for the jump.  Queues
`workspace-bookmark--finish-restore' from an `unwind-protect' cleanup —
whether the restore returned, signalled or was quit — at
`workspace-bookmark--finish-delay' seconds from now: strictly later
than the restore's own `run-at-time' nil paint, so the paint fires
first."
  (setq workspace-bookmark--jump-target nil
        workspace-bookmark--awaiting-jump-target nil)
  (unwind-protect
      (progn
        (workspace-restore name)
        (setq workspace-bookmark--jump-target nil
              workspace-bookmark--awaiting-jump-target t))
    (run-at-time workspace-bookmark--finish-delay nil
                 #'workspace-bookmark--finish-restore))
  name)

(defun workspace-bookmark--note-jump-target (origin arg)
  "Record where the outer jump of a restore-then-jump landed.
Called by the seam right after every host call, with ORIGIN the buffer
current before the host ran and ARG the argument it was handed.  A
no-op unless `workspace-bookmark--awaiting-jump-target' is raised — only
the first host call after a jump-triggered restore returned, the outer
jump's own, gets past it — and it lowers the flag.  The current buffer
is recorded when the jump landed (`workspace-bookmark--landed-p'): it is
no longer ORIGIN, or it visits ARG's location, as it does when the file
is the buffer the new tab inherited."
  (when workspace-bookmark--awaiting-jump-target
    (setq workspace-bookmark--awaiting-jump-target nil)
    (when (workspace-bookmark--landed-p origin arg)
      (setq workspace-bookmark--jump-target (current-buffer)))))

(defun workspace-bookmark--no-workspace-plan (record)
  "Plan the jump to stamped RECORD when no workspace will serve it.
Resolves RECORD with no workspace (rows 4–6 of the resolver's table)
and maps the outcome in context `none' with
`workspace-bookmark--plan-for-outcome', whose `none' column never
calls through: `missing-repo' is handed to
`workspace-bookmark--offer-missing-repo', whose chooser withholds
`add-worktree' when no workspace is current and offers the read-only
canonical view only when :path is non-nil; every other outcome
declines with its report."
  (workspace-bookmark--plan-for-outcome
   record (workspace-bookmark--resolve record nil) 'none nil))

(defun workspace-bookmark--off-workspace-plan (record bookmark-name-or-record)
  "Plan the jump to stamped RECORD when no workspace is current.
Establishes a workspace first — `workspace-bookmark--choose-workspace',
then `workspace-bookmark--restore-for-jump' — and re-enters
`workspace-bookmark--jump-plan' with the chosen name, so the outcome is
decided by the seam's normal path against that workspace.  With nothing
offerable (empty or all-broken registry) or an empty answer, the plan
is `workspace-bookmark--no-workspace-plan''s.  When workspaces are
offerable but no one can be asked (`workspace-bookmark--can-prompt-p'
is nil) the jump declines without prompting.  BOOKMARK-NAME-OR-RECORD
is the seam's original argument, passed on for the plan.  Any error in
this flow declines naming the error, and a plan that would call
through with the ORIGINAL argument — the stored path — declines too;
`quit', at the prompt or anywhere else, passes."
  (let ((name (bookmark-name-from-full-record record)))
    (condition-case err
        (let* ((identity (car (workspace-bookmark--stamped-coordinates record)))
               (candidates (workspace-bookmark--offerable-workspaces identity)))
          (cond
           ((null candidates)
            (workspace-bookmark--no-workspace-plan record))
           ((not (workspace-bookmark--can-prompt-p))
            (cons 'decline
                  (format "Bookmark %S: no workspace is current, and no one can be asked which to restore; nothing opened"
                          name)))
           (t
            (let ((workspace (workspace-bookmark--choose-workspace
                              candidates identity)))
              (if (null workspace)
                  (workspace-bookmark--no-workspace-plan record)
                (let ((plan (workspace-bookmark--jump-plan
                             bookmark-name-or-record
                             (workspace-bookmark--restore-for-jump workspace))))
                  (if (and (eq (car plan) 'call-through)
                           (eq (cdr plan) bookmark-name-or-record))
                      (cons 'decline
                            (format "Bookmark %S: cannot be resolved in workspace %s; nothing opened"
                                    name workspace))
                    plan)))))))
      (error
       (cons 'decline
             (format "Bookmark %S: could not establish a workspace for it (%s); nothing opened"
                     name (error-message-string err)))))))

(defvar workspace--registry)
(defvar jf/emacs-dir)
(declare-function workspace--registered-names "tabs" ())
(declare-function workspace--current-name "tabs" ())
(declare-function workspace--home "data-model" (ws))

(defun workspace-bookmark--registered-names ()
  "Return the names of the registered workspaces, or nil."
  (and (fboundp 'workspace--registered-names)
       (workspace--registered-names)))

(defun workspace-bookmark--current-workspace ()
  "Return the name of the current workspace, or nil."
  (and (fboundp 'workspace--current-name)
       (workspace--current-name)))

(defun workspace-bookmark--workspace-home (name)
  "Return the `:home' of the registered workspace NAME, or nil."
  (and name
       (boundp 'workspace--registry)
       (hash-table-p workspace--registry)
       (fboundp 'workspace--home)
       (let ((ws (gethash name workspace--registry)))
         (and ws (workspace--home ws)))))

(defun workspace-bookmark--checkout-dir-p (dir)
  "Return non-nil when DIR is an existing local checkout root.
That is: DIR is a local directory containing a `.git' file or directory.
Never signals; a remote DIR is rejected without touching the network."
  (and (stringp dir)
       (not (file-remote-p dir))
       (condition-case nil
           (let ((dir (expand-file-name dir)))
             (and (file-directory-p dir)
                  (file-exists-p (expand-file-name ".git" dir))))
         (error nil))))

(defun workspace-bookmark--parse-worktree-porcelain (text)
  "Parse TEXT, output of `git worktree list --porcelain', into entries.

Returns a list of plists in git's order, the main worktree first:

  (:path PATH :bare BOOL :detached BOOL :locked BOOL :prunable BOOL)

Lines before the first `worktree' line, and attribute lines this parser
does not know, are ignored.  TEXT nil yields nil."
  (let (entries current)
    (dolist (line (split-string (or text "") "\n"))
      (cond
       ((string-match "\\`worktree \\(.+\\)\\'" line)
        (when current (push current entries))
        (setq current (list :path (match-string 1 line)
                            :bare nil :detached nil
                            :locked nil :prunable nil)))
       ((null current))
       ((string-match-p "\\`bare\\'" line)
        (setq current (plist-put current :bare t)))
       ((string-match-p "\\`detached\\'" line)
        (setq current (plist-put current :detached t)))
       ((string-match-p "\\`locked\\(?: \\|\\'\\)" line)
        (setq current (plist-put current :locked t)))
       ((string-match-p "\\`prunable\\(?: \\|\\'\\)" line)
        (setq current (plist-put current :prunable t)))))
    (when current (push current entries))
    (nreverse entries)))

(defun workspace-bookmark--worktree-entries (dir)
  "Return the parsed worktree listing of the repository containing DIR.
See `workspace-bookmark--parse-worktree-porcelain' for the entry shape.
Returns nil when git fails."
  (workspace-bookmark--parse-worktree-porcelain
   (workspace-bookmark--git dir "worktree" "list" "--porcelain")))

(defun workspace-bookmark--usable-checkout (entry)
  "Return the checkout a worktree-list ENTRY names, or nil when it is not usable.
ENTRY is a plist from `workspace-bookmark--parse-worktree-porcelain'.
Usable means not bare, not prunable, and an existing local directory
containing `.git'.  The value is an absolute directory name with a
trailing slash."
  (let ((path (plist-get entry :path)))
    (and (not (plist-get entry :bare))
         (not (plist-get entry :prunable))
         (workspace-bookmark--checkout-dir-p path)
         (file-name-as-directory (expand-file-name path)))))

(defun workspace-bookmark--checkouts-of (dir)
  "Return every usable checkout of the repository containing DIR.

The value is a list of absolute directory names (trailing slash), in the
order git lists them, which puts the main worktree first when it is
usable.  Bare, prunable and missing entries are dropped, as is anything
that is not an existing local directory containing `.git' (see
`workspace-bookmark--usable-checkout').  Returns nil when git fails or
DIR is in no repository, and never signals.

Within a discovery call (`workspace-bookmark--with-discovery-memo')
DIR's position and its repository's worktree listing are shared with
the rest of the call; see `workspace-bookmark--shared-position' and
`workspace-bookmark--shared-worktree-entries'."
  (delq nil (mapcar #'workspace-bookmark--usable-checkout
                    (workspace-bookmark--shared-worktree-entries
                     dir (nth 1 (workspace-bookmark--shared-position dir))))))

(defvar workspace-bookmark--discovery-memo nil
  "Answers git gave earlier in the current discovery call, or nil outside one.
A hash table (test `equal') keyed by (QUESTION . KEY) — see
`workspace-bookmark--shared-position',
`workspace-bookmark--shared-worktree-entries' and
`workspace-bookmark--shared-tracked-file-p' — and bound by
`workspace-bookmark--with-discovery-memo'.  It never outlives the call
that bound it.")

(defmacro workspace-bookmark--with-discovery-memo (&rest body)
  "Run BODY as one discovery call, asking git each question at most once.
A call nested inside another shares the outer call's memo rather than
starting its own."
  (declare (indent 0) (debug t))
  `(let ((workspace-bookmark--discovery-memo
          (or workspace-bookmark--discovery-memo
              (make-hash-table :test #'equal))))
     ,@body))

(defun workspace-bookmark--memoised-in-call (key function &rest args)
  "Return (apply FUNCTION ARGS), asked once per KEY within a discovery call.
Outside a discovery call (`workspace-bookmark--discovery-memo' nil)
FUNCTION is simply applied.  A nil answer is memoised like any other."
  (let ((memo workspace-bookmark--discovery-memo))
    (if (null memo)
        (apply function args)
      (let ((hit (gethash key memo 'workspace-bookmark--unasked)))
        (if (eq hit 'workspace-bookmark--unasked)
            (puthash key (apply function args) memo)
          hit)))))

(defun workspace-bookmark--discovery-memo-key (dir)
  "Return the key under which DIR's position is memoised: its truename.
Falls back to DIR's expanded spelling when the truename cannot be read,
and to DIR itself when not even that can be.  DIR must already be a
LOCAL name: the caller tests `file-remote-p' first, because both
`expand-file-name' and `file-truename' dispatch to a file-name handler,
and for a TRAMP name `file-truename' would open the connection."
  (condition-case nil
      (let ((dir (directory-file-name (expand-file-name dir))))
        (condition-case nil (file-truename dir) (error dir)))
    (error dir)))

(defun workspace-bookmark--shared-position (dir)
  "Return `workspace-bookmark--checkout-position' of DIR, once per discovery call.
Two spellings of one directory share the answer (see
`workspace-bookmark--discovery-memo-key').  A DIR that is not a string,
or is remote, is nil at once — `file-remote-p' is the ONLY file
operation asked of it, the same answer and the same discipline as
`workspace-bookmark--git', which is where a remote DIR was refused
before the memo existed."
  (and (stringp dir)
       (not (file-remote-p dir))
       (workspace-bookmark--memoised-in-call
        (cons 'position (workspace-bookmark--discovery-memo-key dir))
        #'workspace-bookmark--checkout-position dir)))

(defun workspace-bookmark--shared-tracked-file-p (dir)
  "Return `workspace-bookmark--holds-tracked-file-p' of DIR, once per discovery call.
Keyed like a position (`workspace-bookmark--discovery-memo-key').  A
DIR that is not a string, or is remote, is nil at once."
  (and (stringp dir)
       (not (file-remote-p dir))
       (workspace-bookmark--memoised-in-call
        (cons 'tracked (workspace-bookmark--discovery-memo-key dir))
        #'workspace-bookmark--holds-tracked-file-p dir)))

(defun workspace-bookmark--shared-worktree-entries (dir git-common-dir)
  "Return `workspace-bookmark--worktree-entries' of DIR, once per repository.
GIT-COMMON-DIR is DIR's shared git directory; every checkout of the
repository it names shares the listing within a discovery call.  With
GIT-COMMON-DIR nil the listing is asked without memoisation."
  (if git-common-dir
      (workspace-bookmark--memoised-in-call
       (cons 'worktrees git-common-dir)
       #'workspace-bookmark--worktree-entries dir)
    (workspace-bookmark--worktree-entries dir)))

(defconst workspace-bookmark--os-artifact-names
  '(".DS_Store" ".localized" "Icon\r" "Thumbs.db" "ehthumbs.db"
    "desktop.ini" ".directory")
  "Basenames of files an OS or sync client writes into any directory.
A tracked path with one of these basenames is no evidence that a
directory holds the work tree, because the file may have been written
there by something other than git.  Compared ignoring letter case.
AppleDouble files (a basename starting \"._\") are artifacts too; see
`workspace-bookmark--os-artifact-p'.")

(defun workspace-bookmark--os-artifact-p (path)
  "Return non-nil when PATH's basename names an OS or sync-client artifact.
See `workspace-bookmark--os-artifact-names'; any basename starting
\"._\" (a macOS AppleDouble file) also counts."
  (let ((base (file-name-nondirectory path)))
    (or (string-prefix-p "._" base)
        (and (member-ignore-case base workspace-bookmark--os-artifact-names)
             t))))

(defun workspace-bookmark--holds-tracked-file-p (dir)
  "Return non-nil when DIR holds at least one path git tracks there.

Runs `git ls-files -z' in DIR and returns t at the first listed path
that exists under DIR.  A listed path whose basename is an OS or
sync-client artifact (`workspace-bookmark--os-artifact-p') is skipped:
Finder and friends write those into any directory, the husk of a
`.git'-named git directory included.  Nil when git fails, lists
nothing, or no other listed path exists — the shape of a directory that
holds only a git directory.  Never signals."
  (let ((out (workspace-bookmark--git dir "ls-files" "-z")))
    (when out
      (condition-case nil
          (let ((root (file-name-as-directory (expand-file-name dir)))
                (file-name-handler-alist nil)
                (start 0)
                (found nil))
            (while (and (not found) (< start (length out)))
              (let* ((end (or (string-search "\0" out start) (length out)))
                     (path (substring out start end)))
                (when (and (> end start)
                           (not (workspace-bookmark--os-artifact-p path))
                           (file-attributes (concat root path)))
                  (setq found t))
                (setq start (1+ end))))
            found)
        (error nil)))))

(defun workspace-bookmark--canonical-checkout (dir)
  "Return the main worktree of the repository containing DIR, or nil.

The value is an absolute directory name with a trailing slash.  Two
signals must agree: the parent of the shared git directory
\(`git rev-parse --path-format=absolute --git-common-dir'), which must
be a directory named `.git', and the first, non-bare entry of `git
worktree list --porcelain'.  The candidate must then be its own top
level: `git rev-parse --show-toplevel' run in it names it, and it must
hold the work tree: at least one path git tracks exists under it (see
`workspace-bookmark--holds-tracked-file-p').  When the signals disagree
— a bare main repository, a `--separate-git-dir' layout, a submodule —
or the candidate's work tree is elsewhere (`core.worktree'), or the
candidate is a husk holding only a `.git'-named git directory whose
work tree lives elsewhere (`--separate-git-dir=<dir>/.git', or a `.git'
symlink), or any answer is unavailable, the answer is nil: no canonical
checkout is better than a guessed one.  Never signals.

Within a discovery call (`workspace-bookmark--with-discovery-memo') the
position, the worktree listing and the tracked-file check are shared
with the rest of the call, and the top-level check is answered from
DIR's own position when DIR's top level is the candidate; the answer is
the same either way."
  (let* ((position (workspace-bookmark--shared-position dir))
         (common (if position
                     (nth 1 position)
                   (workspace-bookmark--git
                    dir "rev-parse" "--path-format=absolute" "--git-common-dir"))))
    (when (and common
               (file-name-absolute-p common)
               (equal ".git" (file-name-nondirectory
                              (directory-file-name common)))
               (condition-case nil (file-directory-p common) (error nil)))
      (let ((candidate (file-name-directory
                        (directory-file-name (expand-file-name common))))
            (first (car (workspace-bookmark--shared-worktree-entries
                         dir common))))
        (when (and first
                   (not (plist-get first :bare))
                   (workspace--same-file-p (plist-get first :path)
                                           candidate))
          (let ((top (if (and position
                              (workspace--same-file-p (nth 0 position)
                                                      candidate))
                         (nth 0 position)
                       (workspace-bookmark--git
                        candidate "rev-parse" "--show-toplevel"))))
            (when (and top
                       (workspace--same-file-p top candidate)
                       (workspace-bookmark--shared-tracked-file-p candidate))
              (file-name-as-directory candidate))))))))

(defun workspace-bookmark--same-repository-worktree-p (home checkout)
  "Return non-nil when HOME is a checkout root of CHECKOUT's own repository.
HOME must be its own top level (`git rev-parse --show-toplevel' run in it
names it), and its shared git directory must be CHECKOUT's, by
`workspace--same-file-p'.  CHECKOUT's shared git directory is
its `.git' directory when it has one; otherwise git is asked.  One
subprocess, two when CHECKOUT's `.git' is a file.  Never signals."
  (let* ((out (workspace-bookmark--git
               home "rev-parse" "--path-format=absolute"
               "--show-toplevel" "--git-common-dir"))
         (lines (and out (split-string out "\n"))))
    (and (= 2 (length lines))
         (workspace--same-file-p (nth 0 lines) home)
         (workspace--same-file-p
          (nth 1 lines)
          (let ((dotgit (expand-file-name ".git" checkout)))
            (if (file-directory-p dotgit)
                dotgit
              (workspace-bookmark--git
               checkout "rev-parse" "--path-format=absolute"
               "--git-common-dir")))))))

(defun workspace-bookmark--owning-workspace (checkout)
  "Return the name of a registered workspace that owns CHECKOUT, or nil.
A workspace owns CHECKOUT when CHECKOUT is its `:home', really lies
inside it, or really contains it, by file identity (see
`workspace--path-within-p').  A contained home owns CHECKOUT
only when it is not itself a worktree of CHECKOUT's repository (see
`workspace-bookmark--same-repository-worktree-p'), and only a home
that exists owns anything.  Symbolic links confer nothing: a clone
linked into a home from outside it is not owned, and neither is a clone
holding a link to a home.  Never signals."
  (seq-find (lambda (name)
              (let ((home (workspace-bookmark--workspace-home name)))
                (or (workspace--path-within-p checkout home)
                    (and (workspace--path-within-p home checkout)
                         (not (workspace-bookmark--same-repository-worktree-p
                               home checkout))))))
            (workspace-bookmark--registered-names)))

(defun workspace-bookmark--classify (checkout)
  "Classify CHECKOUT as `canonical', `workspace-owned' or `other'.

`workspace-owned' when a registered workspace owns CHECKOUT (see
`workspace-bookmark--owning-workspace'); otherwise `canonical' when
its `.git' is a directory (a main worktree); otherwise `other'.  Ownership is tested FIRST: a full clone
inside a workspace home, or containing one, is workspace-owned, never
canonical.  A remote or non-string CHECKOUT is `other'.

`canonical' here is structural only, and NOT sufficient to offer
CHECKOUT as a repository's canonical checkout: see
`workspace-bookmark--offerable-canonical-p'.  Never signals."
  (condition-case nil
      (cond
       ((not (and (stringp checkout) (not (file-remote-p checkout))))
        'other)
       ((workspace-bookmark--owning-workspace checkout)
        'workspace-owned)
       ((file-directory-p (expand-file-name ".git" checkout))
        'canonical)
       (t 'other))
    (error 'other)))

(defun workspace-bookmark--home-checkouts (home)
  "Return HOME and its immediate children that are checkouts.

HOME comes first when it is itself a checkout (a workspace anchored at
an existing repository), then the checkout children, sorted.  A child
that is a symbolic link is skipped, whatever it points to.  Each is an
absolute directory name with a trailing slash.  Returns nil when HOME
is nil, remote, or not an existing directory.  Never signals."
  (when (and (stringp home) (not (file-remote-p home)))
    (condition-case nil
        (let ((home (file-name-as-directory (expand-file-name home))))
          (when (file-directory-p home)
            (mapcar #'file-name-as-directory
                    (seq-filter #'workspace-bookmark--checkout-dir-p
                                (cons home
                                      (seq-remove
                                       #'file-symlink-p
                                       (directory-files
                                        home t
                                        directory-files-no-dot-files-regexp)))))))
      (error nil))))

(defun workspace-bookmark--workspace-checkouts (identity workspace-name)
  "Return every checkout of IDENTITY in WORKSPACE-NAME's home.

The candidates are those of `workspace-bookmark--home-checkouts' — the
home itself when it is a checkout, then its immediate children — and
the value keeps their order.  A candidate that is not its own checkout
root (a child whose `.git' git does not accept, which git resolves to
the repository enclosing it) is dropped.  Each is an absolute directory
name with a trailing slash.  Returns nil when IDENTITY or
WORKSPACE-NAME is nil, when the workspace is not registered, or when
nothing matches.  Never signals.

One subprocess per candidate (its position); the identity comes from
`workspace-bookmark--memoised-identity'."
  (when (and identity workspace-name)
    (workspace-bookmark--with-discovery-memo
      (seq-filter (lambda (checkout)
                    (let ((position (workspace-bookmark--shared-position checkout)))
                      (and position
                           (workspace--same-file-p (nth 0 position) checkout)
                           (equal identity
                                  (workspace-bookmark--memoised-identity
                                   (nth 0 position) (nth 1 position))))))
                  (workspace-bookmark--home-checkouts
                   (workspace-bookmark--workspace-home workspace-name))))))

(defun workspace-bookmark--checkouts-in-view ()
  "Return the checkouts in view, most relevant first, without duplicates.
The current workspace's checkouts, then every other registered
workspace's, then `jf/emacs-dir'.  A workspace's checkouts are its
home, when that is a checkout, and its home's checkout children (see
`workspace-bookmark--home-checkouts')."
  (let* ((current (workspace-bookmark--current-workspace))
         (others (remove current (workspace-bookmark--registered-names)))
         (config (bound-and-true-p jf/emacs-dir)))
    (delete-dups
     (append
      (and current
           (workspace-bookmark--home-checkouts
            (workspace-bookmark--workspace-home current)))
      (seq-mapcat (lambda (name)
                    (workspace-bookmark--home-checkouts
                     (workspace-bookmark--workspace-home name)))
                  others)
      (and (stringp config)
           (not (file-remote-p config))
           (list (file-name-as-directory (expand-file-name config))))))))

(defun workspace-bookmark--offerable-canonical-p (dir)
  "Return non-nil when DIR may be offered as its repository's canonical checkout.

DIR must classify `canonical' (`workspace-bookmark--classify': no
registered workspace owns it and its `.git' is a directory) AND be, by
file identity, the main worktree `workspace-bookmark--canonical-checkout'
finds for DIR's own repository.  Neither condition is sufficient alone.
This is the ONLY test that may admit a checkout as canonical.  Never
signals."
  (and (stringp dir)
       (eq 'canonical (workspace-bookmark--classify dir))
       (workspace--same-file-p
        dir (workspace-bookmark--canonical-checkout dir))))

(defun workspace-bookmark--main-worktree-of (checkout)
  "Return the first usable checkout git lists for CHECKOUT's repository, or nil.
The first of `workspace-bookmark--checkouts-of': within a discovery
call CHECKOUT's position and its repository's worktree listing are
shared with the rest of the call."
  (car (workspace-bookmark--checkouts-of checkout)))

(defun workspace-bookmark--canonical-checkouts-among (checkouts)
  "Return the canonical checkouts of the repositories of CHECKOUTS.

For each of CHECKOUTS, in order, the main-worktree entry git lists for
its repository is a candidate; candidates are de-duplicated by file
identity (first occurrence kept) and only those that pass
`workspace-bookmark--offerable-canonical-p' are returned.  Each is an
absolute directory name with a trailing slash.  Never signals.

Runs as one discovery call: on a canonical main clone the whole test
costs three subprocesses, and further checkouts of a repository already
seen cost one each."
  (workspace-bookmark--with-discovery-memo
    (seq-filter
     #'workspace-bookmark--offerable-canonical-p
     (seq-uniq (delq nil (mapcar #'workspace-bookmark--main-worktree-of
                                 checkouts))
               #'workspace--same-file-p))))

(defun workspace-bookmark--canonical-checkouts-for-identity (identity)
  "Return every distinct canonical checkout of IDENTITY, or nil.

Searches the checkouts in view (`workspace-bookmark--checkouts-in-view')
for those whose identity is IDENTITY, and returns the canonical
checkouts of their repositories that pass
`workspace-bookmark--offerable-canonical-p', de-duplicated by file
identity, in order of first appearance.  A workspace-owned or `other'
checkout is never returned.  Nil when IDENTITY is nil, when no checkout
of it is in view, or when none passes.  Never signals.

One subprocess per checkout in view (its position); identities come
from `workspace-bookmark--memoised-identity', and the positions are
shared with the canonical test that follows."
  (when identity
    (workspace-bookmark--with-discovery-memo
      (workspace-bookmark--canonical-checkouts-among
       (seq-filter (lambda (checkout)
                     (let ((position (workspace-bookmark--shared-position checkout)))
                       (and position
                            (equal identity
                                   (workspace-bookmark--memoised-identity
                                    (nth 0 position) (nth 1 position))))))
                   (workspace-bookmark--checkouts-in-view))))))

(cl-defun workspace-bookmark-canonical-checkouts-in-view
    (&optional (checkouts nil checkouts-given))
  "Return the canonical checkouts of the repositories in view.

With no argument, the repositories in view are those of the checkouts
in view (`workspace-bookmark--checkouts-in-view'): the current
workspace's, then every other registered workspace's, then
`jf/emacs-dir'.  With CHECKOUTS, a list of directories, they are the
repositories of CHECKOUTS instead; CHECKOUTS nil yields nil.

Each element is its repository's main worktree and passed
`workspace-bookmark--offerable-canonical-p': never a workspace-owned
checkout, a linked worktree, or a directory that is not a main
worktree.  The value is in order of first appearance, de-duplicated by
file identity, each an absolute directory name with a trailing slash.

This is a CROSS-MODULE INTERFACE, called from git-worktrees.el behind a
call-time `fboundp' guard; its name, calling convention and meaning are
stable.  Never signals in normal operation."
  (workspace-bookmark--canonical-checkouts-among
   (if checkouts-given
       checkouts
     (workspace-bookmark--checkouts-in-view))))

(defun workspace-bookmark-call-with-discovery-memo (function)
  "Call FUNCTION with no arguments as ONE discovery call; return its value.

Every call of `workspace-bookmark-canonical-checkouts-in-view' made
while FUNCTION runs shares one discovery memo, so a repository those
calls reach more than once is asked each git question once.  So does
every other discovery made while FUNCTION runs, not only that entry
point.  The memo is dropped when FUNCTION returns or exits non-locally;
nothing survives the call.

Precondition: answers are exactly those the calls give on their own
only while nothing changes the repositories inside the scope.
FUNCTION must not create, move or remove a checkout or change a
repository's configuration, and must not wait for user input (while
it waits, anything may change); a question asked again after such a
change is answered from the memo, as before it.

This is a CROSS-MODULE INTERFACE, called from git-worktrees.el behind a
call-time `fboundp' guard; its name, calling convention and meaning are
stable."
  (workspace-bookmark--with-discovery-memo
    (funcall function)))

(defconst workspace-bookmark--valid-outcomes
  '(resolved not-on-branch missing-repo no-clone non-portable ambiguous)
  "Closed set of bookmark-resolution outcomes.
`workspace-bookmark--resolve' returns exactly one of these.  Adding a
member is a deliberate change here, in the resolver, and in
`workspace-bookmark--plan-for-outcome' — the ONE site that maps an
outcome to a jump plan (plus `workspace-bookmark--declining-outcomes'
and `workspace-bookmark--decline-message' when the member declines).")

(defun workspace-bookmark--valid-outcome-p (outcome)
  "Return non-nil when OUTCOME is a member of `workspace-bookmark--valid-outcomes'."
  (and (symbolp outcome)
       (memq outcome workspace-bookmark--valid-outcomes)
       t))

(cl-defun workspace-bookmark--outcome
    (outcome &key path identity relpath candidates checkout)
  "Return the resolution plist for OUTCOME.

The plist always carries the six keys :outcome, :path, :identity,
:relpath, :candidates and :checkout, in that order.  Signals an error
when OUTCOME is not in `workspace-bookmark--valid-outcomes'."
  (unless (workspace-bookmark--valid-outcome-p outcome)
    (error "Not a bookmark-resolution outcome: %S" outcome))
  (list :outcome outcome :path path :identity identity
        :relpath relpath :candidates candidates :checkout checkout))

(defconst workspace-bookmark--known-format-versions
  (list workspace-bookmark-format-version)
  "The stamp-format versions `workspace-bookmark--resolve' can read.
A record stamped under any other version resolves `non-portable'.")

(defun workspace-bookmark--confined-relpath-p (relpath)
  "Return non-nil when RELPATH is a relpath that stays inside its checkout.

RELPATH must be a non-empty string that is not absolute (in the sense
of `file-name-absolute-p'), has no component that is exactly \"..\",
and holds no NUL byte.  This test of spelling is the WHOLE confinement
test (DD-16): `workspace-bookmark--anchor' decides the LOCATION by that
spelling, and asks the filesystem — which follows links — only whether
something of RELPATH's kind is present there."
  (and (stringp relpath)
       (not (string-empty-p relpath))
       (not (string-search "\0" relpath))
       (not (file-name-absolute-p relpath))
       (not (member ".." (split-string relpath "/")))))

(defun workspace-bookmark--stamped-coordinates (record)
  "Return the (IDENTITY . RELPATH) stamped on RECORD, or nil.

Non-nil only when RECORD is a bookmark record (a cons) carrying all
three stamped properties: a non-empty string identity, a format version
in `workspace-bookmark--known-format-versions', and a relpath that
satisfies `workspace-bookmark--confined-relpath-p'.  Runs no
subprocess.  Never signals."
  (and (consp record)
       (condition-case nil
           (let ((identity (bookmark-prop-get
                            record workspace-bookmark-repo-property))
                 (relpath (bookmark-prop-get
                           record workspace-bookmark-relpath-property))
                 (version (bookmark-prop-get
                           record workspace-bookmark-format-property)))
             (and (stringp identity)
                  (not (string-empty-p identity))
                  (memql version workspace-bookmark--known-format-versions)
                  (workspace-bookmark--confined-relpath-p relpath)
                  (cons identity relpath)))
         (error nil))))

(defun workspace-bookmark--stored-filename (record)
  "Return RECORD's stored `filename' when it is a string, else nil.
Never signals."
  (and (consp record)
       (condition-case nil
           (let ((filename (bookmark-prop-get record 'filename)))
             (and (stringp filename) filename))
         (error nil))))

(defun workspace-bookmark--anchor (relpath checkout)
  "Anchor RELPATH under CHECKOUT and check it against the disk.

RELPATH must already satisfy `workspace-bookmark--confined-relpath-p'.
Returns one of:

  (present . PATH)  PATH exists and is a directory exactly when RELPATH
                    ends in \"/\";
  (absent)          nothing of RELPATH's kind exists at PATH.

PATH is RELPATH expanded against CHECKOUT's directory name, and is the
answer by its spelling; whether something of RELPATH's kind exists
there is asked of the filesystem, which follows links, so a link that
points out of CHECKOUT still answers PATH and a dangling link is absent
\(DD-16).  Signals when PATH does not start with CHECKOUT, which a
confined RELPATH cannot produce."
  (let* ((root (file-name-as-directory checkout))
         (path (expand-file-name relpath root)))
    (unless (or (string-prefix-p root path)
                (string-equal path (directory-file-name root)))
      (error "Relpath %S anchors outside %s: %s" relpath root path))
    (cond
     ((not (file-exists-p path)) (list 'absent))
     ((not (eq (and (string-suffix-p "/" relpath) t)
               (and (file-directory-p path) t)))
      (list 'absent))
     (t (cons 'present path)))))

(defun workspace-bookmark--resolve-anchored (outcome relpath checkout identity)
  "Anchor RELPATH under CHECKOUT and return the outcome for it.

OUTCOME is `resolved' (CHECKOUT is the workspace's) or `missing-repo'
\(CHECKOUT is the canonical checkout).  A present location gives OUTCOME
with its :path.  An absent one gives `not-on-branch' for `resolved',
and `missing-repo' with a nil :path for `missing-repo'.  Either way
:checkout is CHECKOUT.  Any other anchoring answer is a programming
error and signals: nothing here returns a location it did not anchor."
  (let ((anchored (workspace-bookmark--anchor relpath checkout)))
    (pcase (car anchored)
      ('present
       (workspace-bookmark--outcome outcome :path (cdr anchored)
                                    :identity identity :relpath relpath
                                    :checkout checkout))
      ('absent
       (workspace-bookmark--outcome (if (eq outcome 'resolved)
                                        'not-on-branch
                                      outcome)
                                    :identity identity :relpath relpath
                                    :checkout checkout))
      (_ (error "Unknown anchoring answer: %S" anchored)))))

(defun workspace-bookmark--non-portable (record)
  "Return the `non-portable' outcome for RECORD, with its stored filename."
  (workspace-bookmark--outcome
   'non-portable :path (workspace-bookmark--stored-filename record)))

(defun workspace-bookmark--canonical-copy-p (stored relpath checkout)
  "Return non-nil when STORED is CHECKOUT's own copy of RELPATH.

STORED is a record's stored `filename' (or nil); RELPATH a confined
relpath; CHECKOUT the one canonical checkout in view.  True when STORED
names the same existing local file as RELPATH anchored under CHECKOUT,
by device and inode (`workspace--same-file-p'), never by spelling.  Nil
for a nil or remote STORED, and whenever either side does not exist.
Two stats, no subprocess.  Never signals."
  (and (stringp stored)
       (not (file-remote-p stored))
       (condition-case nil
           (workspace--same-file-p
            stored (expand-file-name relpath (file-name-as-directory checkout)))
         (error nil))))

(defun workspace-bookmark--resolve-coordinates (identity relpath workspace-name
                                                         &optional stored)
  "Decide the outcome for the stamped IDENTITY and RELPATH in WORKSPACE-NAME.
See `workspace-bookmark--resolve'.  May signal; the caller catches.

STORED is the record's stored `filename', or nil.  It is consulted in
one arm only: with a workspace current (WORKSPACE-NAME non-nil) that
holds no checkout, and exactly one canonical checkout in view, a STORED
that is that checkout's own copy of RELPATH
\(`workspace-bookmark--canonical-copy-p') answers `resolved' instead of
`missing-repo'; the :path is still the anchored one.  With no workspace
at all the arm answers `missing-repo' as before, so the off-workspace
fallback's bounded choice stands.

The whole decision is ONE discovery call
\(`workspace-bookmark--with-discovery-memo'): the current workspace's
checkouts, which both plural discovery functions examine, are
positioned once.  Nothing between the two calls can change the layout,
so the outcome is the same as with two."
  (workspace-bookmark--with-discovery-memo
    (let ((checkouts (workspace-bookmark--workspace-checkouts
                      identity workspace-name)))
      (cond
       ((and checkouts (null (cdr checkouts)))
        (workspace-bookmark--resolve-anchored
         'resolved relpath (car checkouts) identity))
       (checkouts
        (workspace-bookmark--outcome 'ambiguous :identity identity
                                     :relpath relpath :candidates checkouts))
       (t
        (let ((canonical (workspace-bookmark--canonical-checkouts-for-identity
                          identity)))
          (cond
           ((and canonical (null (cdr canonical)))
            (workspace-bookmark--resolve-anchored
             (if (and workspace-name
                      (workspace-bookmark--canonical-copy-p
                       stored relpath (car canonical)))
                 'resolved
               'missing-repo)
             relpath (car canonical) identity))
           (canonical
            (workspace-bookmark--outcome 'ambiguous :identity identity
                                         :relpath relpath :candidates canonical))
           (t
            (workspace-bookmark--outcome 'no-clone :identity identity
                                         :relpath relpath)))))))))

(defun workspace-bookmark--resolve (record workspace-name)
  "Decide where bookmark RECORD points in workspace WORKSPACE-NAME.

RECORD is a bookmark record (NAME . ALIST); WORKSPACE-NAME a registered
workspace's name, or nil for none.  Returns the plist

  (:outcome OUTCOME :path PATH :identity ID :relpath REL
   :candidates CHECKOUTS :checkout CHECKOUT)

where OUTCOME is one of `workspace-bookmark--valid-outcomes', decided
first-match-wins:

  `non-portable'   RECORD carries no usable stamp; :path is its
                   stored `filename' (the ONLY outcome that uses it).
  `resolved'       WORKSPACE-NAME holds exactly one checkout of the
                   identity and the relpath exists there; :path is it,
                   :checkout that checkout.  Also: WORKSPACE-NAME is a
                   workspace holding none, exactly one canonical
                   checkout is in view, and the stored `filename' IS
                   that checkout's copy of the relpath by file
                   identity; :path is the anchored path, :checkout the
                   canonical checkout.  Never with WORKSPACE-NAME nil.
  `not-on-branch'  that one checkout lacks the relpath; :path nil,
                   :checkout that checkout.
  `ambiguous'      WORKSPACE-NAME holds several checkouts, or holds
                   none and several canonical checkouts are in view;
                   :candidates lists them, :path nil.
  `missing-repo'   WORKSPACE-NAME holds none and exactly one canonical
                   checkout is in view; :checkout is it, and :path is
                   the relpath under it when it exists there, else
                   nil.  A candidate to offer, never a location to act
                   on.
  `no-clone'       nothing in view; :path nil.

:checkout is nil for every outcome not listed with one.  A relpath
that would leave its checkout by its spelling (`..', an absolute name)
resolves `non-portable'; one that leaves it only through a symbolic
link resolves by its spelling, as visiting it by hand would (DD-16).
The stamp is not checked against the stored `filename' (see the Org
prose on stale stamps); the same-file tie-break above compares the two
and returns the anchored path, never the stored string.

This is a decision function: it runs git through the discovery
functions but never visits a file, displays a buffer, reads input or
writes to the echo area.  It never signals: an error once the identity
is known gives `no-clone', before that `non-portable'."
  (condition-case nil
      (let ((coordinates (workspace-bookmark--stamped-coordinates record)))
        (if (not coordinates)
            (workspace-bookmark--non-portable record)
          (condition-case nil
              (workspace-bookmark--resolve-coordinates
               (car coordinates) (cdr coordinates) workspace-name
               (workspace-bookmark--stored-filename record))
            (error (workspace-bookmark--outcome
                    'no-clone :identity (car coordinates)
                    :relpath (cdr coordinates))))))
    (error (workspace-bookmark--non-portable record))))

(defun workspace-bookmark-install-advice ()
  "Install the two portable-bookmark advice seams.  Idempotent."
  (interactive)
  (advice-add 'bookmark-make-record :filter-return
              #'workspace-bookmark--stamp-record)
  (advice-add 'bookmark-handle-bookmark :around
              #'workspace-bookmark--handle-bookmark-advice))

(defun workspace-bookmark-remove-advice ()
  "Remove the two portable-bookmark advice seams, restoring stock behaviour."
  (interactive)
  (advice-remove 'bookmark-make-record
                 #'workspace-bookmark--stamp-record)
  (advice-remove 'bookmark-handle-bookmark
                 #'workspace-bookmark--handle-bookmark-advice))

(workspace-bookmark-install-advice)

(provide 'workspace-portable-bookmarks)
;;; portable-bookmarks.el ends here
