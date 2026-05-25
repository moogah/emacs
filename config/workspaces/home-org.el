;;; home-org.el --- Workspaces home.org reader -*- lexical-binding: t; -*-

(defun workspace-home-org-path (home)
  "Return the absolute path to `home.org' inside HOME directory.
HOME is the workspace's home directory (an absolute path)."
  (expand-file-name "home.org" home))

(defun workspace-home-org-exists-p (home)
  "Return non-nil if HOME contains a readable `home.org' file."
  (file-readable-p (workspace-home-org-path home)))

(defun workspace-home-org-title (home)
  "Return the trimmed `#+TITLE:' keyword value from HOME/home.org, or nil.
Returns nil if the file is missing, unreadable, has no `#+TITLE:'
keyword, or has only whitespace after the keyword.

Live read on every call — no caching.  Implementation uses a regex
on a temp buffer; `org-mode' is never activated."
  (let ((path (workspace-home-org-path home)))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:[ \t]+\\(.*\\)$" nil t)
          (let ((title (string-trim (match-string 1))))
            (and (not (string-empty-p title)) title)))))))

(provide 'workspace-home-org)
;;; home-org.el ends here
