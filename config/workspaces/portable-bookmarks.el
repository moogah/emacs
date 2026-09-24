;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'bookmark)

(defgroup workspace-bookmark nil
  "Workspace-relative resolution of Emacs bookmarks."
  :group 'workspaces
  :prefix "workspace-bookmark-")

(defcustom workspace-bookmark-repo-property 'workspace-bookmark-repo
  "Bookmark-record property holding the repository identity.
The value stored under this property is the normalized remote URL of the
repository the bookmarked file belongs to.

This symbol is serialised into the user's bookmark file.  Changing it
orphans coordinates stamped under the previous name; records so orphaned
degrade to the `non-portable' outcome and behave exactly as stock
bookmarks do."
  :type 'symbol
  :group 'workspace-bookmark)

(defcustom workspace-bookmark-relpath-property 'workspace-bookmark-relpath
  "Bookmark-record property holding the repository-relative file path.
The value stored under this property is the bookmarked file's path
relative to the root of its checkout, so it can be re-anchored against a
different checkout of the same repository.

This symbol is serialised into the user's bookmark file; see
`workspace-bookmark-repo-property' for the consequence of renaming it."
  :type 'symbol
  :group 'workspace-bookmark)

(defun workspace-bookmark--git (dir &rest args)
  "Run git with ARGS in DIR and return its trimmed stdout, or nil.

Returns nil — never signals — when DIR is not (and is not inside) an
existing directory, when git exits non-zero, or when git produces no
output.  If DIR names a file, git is run in the file's directory.
stderr is discarded."
  (let* ((path (and (stringp dir) (expand-file-name dir)))
         (cwd (cond ((null path) nil)
                    ((file-directory-p path) (file-name-as-directory path))
                    (t (file-name-directory (directory-file-name path))))))
    (when (and cwd (file-directory-p cwd))
      (condition-case nil
          (let ((default-directory cwd))
            (with-temp-buffer
              (when (eq 0 (apply #'process-file "git" nil '(t nil) nil args))
                (let ((out (string-trim (buffer-string))))
                  (unless (string-empty-p out) out)))))
        (error nil)))))

(defun workspace-bookmark--repo-root (dir)
  "Return the root of the git checkout containing DIR, or nil.

The value is an absolute directory name with a trailing slash.  Returns
nil when DIR is in no git repository, and never signals."
  (let ((root (workspace-bookmark--git dir "rev-parse" "--show-toplevel")))
    (and root (file-name-as-directory (expand-file-name root)))))

(defun workspace-bookmark--remote-url (dir)
  "Return the remote URL of the repository containing DIR, or nil.

`origin' is preferred.  When the repository has no `origin', the first
remote `git remote' lists is used.  Returns nil when the repository has
no remote at all, or when DIR is in no repository."
  (or (workspace-bookmark--git dir "config" "--get" "remote.origin.url")
      (let ((first (car (split-string
                         (or (workspace-bookmark--git dir "remote") "")
                         "\n" t "[ \t\r]+"))))
        (and first
             (workspace-bookmark--git dir "config" "--get"
                                      (concat "remote." first ".url"))))))

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
trailing \".git\" and any trailing slash removed, and the host — but not
the path — downcased.  The SSH, HTTPS and ssh:// spellings of one
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
        (setq path (replace-regexp-in-string "\\`/+" "" path))
        (setq path (replace-regexp-in-string "/+\\'" "" path))
        (when (string-suffix-p ".git" path)
          (setq path (substring path 0 -4)))
        (setq path (replace-regexp-in-string "/+\\'" "" path))
        (unless (or (string-empty-p host) (string-empty-p path))
          (concat host "/" path))))))

(defun workspace-bookmark--identity (dir)
  "Return the normalized identity of the repository containing DIR, or nil.

Returns nil — never signals — when DIR is in no git repository, when that
repository has no remote, or when the remote URL names no host."
  (let ((root (workspace-bookmark--repo-root dir)))
    (and root
         (workspace-bookmark--normalize-identity
          (workspace-bookmark--remote-url root)))))

(defun workspace-bookmark--stamp-record (record)
  "Return RECORD with portable coordinates added, or RECORD unchanged.

Intended as `:filter-return' advice on `bookmark-make-record'.  Never
prompts, never messages, never touches the bookmark name.  Any internal
error must be swallowed and the original RECORD returned: failing to
stamp must not prevent a bookmark from being created."
  record)

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
