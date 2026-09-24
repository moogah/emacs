;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'bookmark)

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

(defun workspace-bookmark--git (dir &rest args)
  "Run git with ARGS in DIR and return its trimmed stdout, or nil.

Returns nil — never signals — when DIR is nil, remote, not (and not
inside) an existing directory, when git exits non-zero, or when git
produces no output.  If DIR names a file, git is run in the file's
directory.  stderr is discarded.

DIR must be LOCAL.  A remote (TRAMP) name returns nil without touching
the filesystem: probing one can signal, can block on a password prompt,
and — on a reachable host — would run git remotely and stamp the wrong
machine's identity."
  (condition-case nil
      (let ((path (and (stringp dir)
                       (not (file-remote-p dir))
                       (expand-file-name dir))))
        (when (and path (not (file-remote-p path)))
          (let ((cwd (if (file-directory-p path)
                         (file-name-as-directory path)
                       (file-name-directory (directory-file-name path)))))
            (when (and cwd (file-directory-p cwd))
              (let ((default-directory cwd))
                (with-temp-buffer
                  (when (eq 0 (apply #'process-file "git" nil '(t nil) nil args))
                    (let ((out (string-trim (buffer-string))))
                      (unless (string-empty-p out) out)))))))))
    (error nil)))

(defun workspace-bookmark--repo-root (dir)
  "Return the root of the git checkout containing DIR, or nil.

The value is an absolute directory name with a trailing slash.  Returns
nil when DIR is in no git repository, and never signals."
  (let ((root (workspace-bookmark--git dir "rev-parse" "--show-toplevel")))
    (and root (file-name-as-directory (expand-file-name root)))))

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
ROOT must already be a checkout root; see `workspace-bookmark--identity'
for the version that finds it."
  (workspace-bookmark--normalize-identity
   (workspace-bookmark--remote-url root)))

(defun workspace-bookmark--identity (dir)
  "Return the normalized identity of the repository containing DIR, or nil.

Returns nil — never signals — when DIR is in no git repository, when that
repository has no remote, or when the remote URL names no host."
  (let ((root (workspace-bookmark--repo-root dir)))
    (and root (workspace-bookmark--root-identity root))))

(defvar workspace-bookmark--identity-memo (make-hash-table :test #'equal)
  "Memoised repository identities, keyed by checkout root.
Each value is (STAMP . IDENTITY), where STAMP is the value
`workspace-bookmark--config-stamp' returned when IDENTITY was computed
and IDENTITY may be nil.")

(defun workspace-bookmark-clear-identity-cache ()
  "Forget every memoised repository identity.

Each memoised identity is revalidated against the modification time of
its repository's own git config, so `git remote add', `set-url' and
`remove' are picked up without this command.  It is needed only after a
change the memo cannot see: a `url.*.insteadOf' rule (or anything else
`git remote get-url' honours) added to the global, XDG or system
gitconfig, to an included config file, or to `config.worktree'.  Loading
this module clears the memo too."
  (interactive)
  (clrhash workspace-bookmark--identity-memo))

(defun workspace-bookmark--config-stamp (git-common-dir)
  "Return the modification time of GIT-COMMON-DIR's config file, or nil."
  (and git-common-dir
       (file-attribute-modification-time
        (file-attributes (expand-file-name "config" git-common-dir)))))

(defun workspace-bookmark--memoised-identity (root git-common-dir)
  "Return the identity of the checkout at ROOT, memoised per ROOT.
GIT-COMMON-DIR is the repository's shared git directory; the memoised
value is reused only while its config file is unchanged.  A nil identity
is memoised like any other."
  (let ((stamp (workspace-bookmark--config-stamp git-common-dir))
        (hit (gethash root workspace-bookmark--identity-memo)))
    (if (and hit (equal (car hit) stamp))
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
                     dir "rev-parse" "--show-toplevel" "--git-common-dir" "--show-prefix"
                     (and committed '("HEAD:./"))))
         (lines (and out (split-string out "\n"))))
    (when (memq (length lines) (if committed '(4) '(2 3)))
      (append (list (file-name-as-directory (nth 0 lines))
                    (expand-file-name (nth 1 lines) dir)
                    (or (nth 2 lines) ""))
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
         (file-equal-p (expand-file-name relpath root) location)
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

(defun workspace-bookmark--handle-bookmark-advice (orig-fn bookmark-name-or-record &rest args)
  "Resolve BOOKMARK-NAME-OR-RECORD against the current workspace, then jump.

Intended as `:around' advice on `bookmark-handle-bookmark'; ORIG-FN is the
advised function and ARGS its remaining arguments.  Any internal error
falls through to ORIG-FN with the ORIGINAL argument, so a failure
affecting one bookmark cannot affect another or the bookmark file."
  (apply orig-fn bookmark-name-or-record args))

(require 'seq)

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

(defun workspace-bookmark--checkouts-of (dir)
  "Return every usable checkout of the repository containing DIR.

The value is a list of absolute directory names (trailing slash), in the
order git lists them, which puts the main worktree first when it is
usable.  Bare, prunable and missing entries are dropped, as is anything
that is not an existing local directory containing `.git'.  Returns nil
when git fails or DIR is in no repository, and never signals."
  (delq nil
        (mapcar (lambda (entry)
                  (let ((path (plist-get entry :path)))
                    (and (not (plist-get entry :bare))
                         (not (plist-get entry :prunable))
                         (workspace-bookmark--checkout-dir-p path)
                         (file-name-as-directory (expand-file-name path)))))
                (workspace-bookmark--worktree-entries dir))))

(defun workspace-bookmark--canonical-checkout (dir)
  "Return the main worktree of the repository containing DIR, or nil.

The value is an absolute directory name with a trailing slash.  Two
signals must agree: the parent of `git rev-parse --git-common-dir', which
must be a directory named `.git', and the first, non-bare entry of `git
worktree list --porcelain'.  The candidate must then be its own top
level: `git rev-parse --show-toplevel' run in it names it.  When the
signals disagree — a bare main repository, a `--separate-git-dir'
layout, a submodule — or the candidate's work tree is elsewhere
\(`core.worktree'), or any answer is unavailable, the answer is nil: no
canonical checkout is better than a guessed one.  Never signals."
  (let ((common (workspace-bookmark--git
                 dir "rev-parse" "--path-format=absolute" "--git-common-dir")))
    (when (and common
               (file-name-absolute-p common)
               (equal ".git" (file-name-nondirectory
                              (directory-file-name common)))
               (condition-case nil (file-directory-p common) (error nil)))
      (let ((candidate (file-name-directory
                        (directory-file-name (expand-file-name common))))
            (first (car (workspace-bookmark--worktree-entries dir))))
        (when (and first
                   (not (plist-get first :bare))
                   (condition-case nil
                       (file-equal-p (plist-get first :path) candidate)
                     (error nil)))
          (let ((top (workspace-bookmark--git
                      candidate "rev-parse" "--show-toplevel")))
            (when (and top
                       (condition-case nil
                           (file-equal-p top candidate)
                         (error nil)))
              (file-name-as-directory candidate))))))))

(defun workspace-bookmark--path-within-p (path dir)
  "Return non-nil when PATH is DIR or lies inside it.

Both must be local and exist.  Containment is by file identity: PATH is
truenamed, then it and each of its ancestors is compared to DIR with
`file-equal-p', so differences of spelling — trailing slash, `~',
symlinked prefixes, and letter case on a case-insensitive filesystem —
do not matter.  Never signals."
  (and (stringp path) (stringp dir)
       (not (file-remote-p path))
       (not (file-remote-p dir))
       (condition-case nil
           (let ((dir (expand-file-name dir))
                 (current (directory-file-name
                           (file-truename (expand-file-name path))))
                 (previous nil)
                 (found nil))
             (when (file-directory-p dir)
               (while (and (not found) (not (equal current previous)))
                 (if (file-equal-p current dir)
                     (setq found t)
                   (setq previous current
                         current (directory-file-name
                                  (file-name-directory current))))))
             found)
         (error nil))))

(defun workspace-bookmark--owning-workspace (checkout)
  "Return the name of a registered workspace whose home holds CHECKOUT.
Returns nil when CHECKOUT lies inside no registered workspace's `:home'."
  (seq-find (lambda (name)
              (workspace-bookmark--path-within-p
               checkout (workspace-bookmark--workspace-home name)))
            (workspace-bookmark--registered-names)))

(defun workspace-bookmark--classify (checkout)
  "Classify CHECKOUT as `canonical', `workspace-owned' or `other'.

`workspace-owned' when CHECKOUT lies inside a registered workspace's
home; otherwise `canonical' when its `.git' is a directory (a main
worktree); otherwise `other'.  Ownership is tested FIRST: a full clone
inside a workspace home is workspace-owned, never canonical.  A remote
or non-string CHECKOUT is `other'.  Never signals."
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
  "Return the checkouts that are immediate children of HOME, sorted.
Each is an absolute directory name with a trailing slash.  Returns nil
when HOME is nil, remote, or not an existing directory.  Never signals."
  (when (and (stringp home) (not (file-remote-p home)))
    (condition-case nil
        (let ((home (file-name-as-directory (expand-file-name home))))
          (when (file-directory-p home)
            (mapcar #'file-name-as-directory
                    (seq-filter #'workspace-bookmark--checkout-dir-p
                                (directory-files
                                 home t directory-files-no-dot-files-regexp)))))
      (error nil))))

(defun workspace-bookmark--workspace-checkout (identity workspace-name)
  "Return the checkout of IDENTITY inside WORKSPACE-NAME's home, or nil.

Walks the immediate children of the workspace's `:home' that are
checkouts and returns the first whose repository identity is IDENTITY,
as an absolute directory name with a trailing slash.  Returns nil when
IDENTITY or WORKSPACE-NAME is nil, when the workspace is not registered,
or when no child matches.  Never signals."
  (when (and identity workspace-name)
    (seq-find (lambda (checkout)
                (equal identity (workspace-bookmark--identity checkout)))
              (workspace-bookmark--home-checkouts
               (workspace-bookmark--workspace-home workspace-name)))))

(defun workspace-bookmark--checkouts-in-view ()
  "Return the checkouts in view, most relevant first, without duplicates.
The current workspace's home children, then every other registered
workspace's home children, then `jf/emacs-dir'."
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

(defun workspace-bookmark--canonical-for-identity (identity)
  "Return the canonical checkout of IDENTITY, or nil.

Searches the checkouts in view (see `workspace-bookmark--checkouts-in-view')
for one whose identity is IDENTITY, asks git for that repository's main
worktree, and returns it ONLY when it classifies `canonical'.  A
workspace-owned or `other' checkout is never returned.  Returns nil when
IDENTITY is nil, when no checkout of it is in view, or when no in-view
repository of it has a main worktree that passes.  Never signals."
  (when identity
    (seq-some (lambda (checkout)
                (when (equal identity (workspace-bookmark--identity checkout))
                  (let ((canonical
                         (workspace-bookmark--canonical-checkout checkout)))
                    (and canonical
                         (eq 'canonical
                             (workspace-bookmark--classify canonical))
                         canonical))))
              (workspace-bookmark--checkouts-in-view))))

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
