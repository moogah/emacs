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
Needed only after a change the memo cannot see, such as a new
`url.*.insteadOf' rule in the global gitconfig."
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

(defun workspace-bookmark--checkout-position (dir)
  "Return (ROOT GIT-COMMON-DIR PREFIX) for directory DIR, or nil.

ROOT is the checkout root as a directory name, GIT-COMMON-DIR the
absolute shared git directory, and PREFIX DIR's path relative to ROOT as
git spells it (\"sub/dir/\", or \"\" at the root).  One git subprocess.
Nil when DIR is in no work tree."
  (let* ((out (workspace-bookmark--git
               dir "rev-parse" "--show-toplevel" "--git-common-dir" "--show-prefix"))
         (lines (and out (split-string out "\n"))))
    (when (memq (length lines) '(2 3))
      (list (file-name-as-directory (nth 0 lines))
            (expand-file-name (nth 1 lines) dir)
            (or (nth 2 lines) "")))))

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
git tracks at least one file beneath it.  The result must re-anchor on
ROOT to the same file as LOCATION, or nil is returned."
  (let* ((root (nth 0 position))
         (relpath
          (if (file-directory-p location)
              (and (workspace-bookmark--git location "ls-files" "-z" "--" ".")
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
         (dir (if (file-directory-p location)
                  (file-name-as-directory location)
                (file-name-directory location)))
         (position (workspace-bookmark--checkout-position dir))
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
