;;; portable-bookmarks.el --- Workspace-relative bookmark resolution -*- lexical-binding: t; -*-

(require 'cl-lib)
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
