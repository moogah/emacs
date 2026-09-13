;;; discovery.el --- org-graph vault discovery for vulpea -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-id)   ; built-in; the org-id-locations seed depends on it explicitly

(defvar vulpea-db-sync-directories)
(defvar vulpea-default-notes-directory)
(defvar vulpea-create-default-template)
(declare-function vulpea-db-autosync-mode "vulpea-db-sync" (&optional arg))
(declare-function vulpea-db-sync-full-scan "vulpea-db-sync" (&optional arg))
(declare-function vulpea-db-query "vulpea-db-query" (&optional predicate))
(declare-function vulpea-note-id "vulpea-db" (note))
(declare-function vulpea-note-path "vulpea-db" (note))

(setq vulpea-default-notes-directory
      (file-name-as-directory
       (expand-file-name (if (boundp 'org-graph-vault-root)
                             org-graph-vault-root
                           "~/org"))))

(setq vulpea-create-default-template
      '(:file-name "${timestamp}-${slug}.org"))

(defun org-graph/index-roots ()
  "Return the explicit list of directories vulpea SHALL index.
Exactly one entry: `org-graph-vault-root' (canonicalised: expanded,
trailing slash), falling back to ~/org/ when the loader defcustom is
unbound.  No other package is consulted and no wider tree is ever
walked to discover notes; hidden subdirectories of the vault are
excluded by vulpea itself
\(`register/invariant/bounded-discovery-roots')."
  (list (file-name-as-directory
         (expand-file-name
          (if (boundp 'org-graph-vault-root)
              org-graph-vault-root
            "~/org/")))))

(defun org-graph/configure-sync ()
  "Point vulpea at the vault root and enable autosync.
Sets `vulpea-db-sync-directories' to `org-graph/index-roots', enables
`vulpea-db-autosync-mode', and triggers an initial (async) full scan.

Idempotent: re-running overwrites the directory list, leaves autosync
enabled, and re-scans with smart change detection.  Interactive so it
can be invoked on demand as a manual re-index."
  (interactive)
  (setq vulpea-db-sync-directories (org-graph/index-roots))
  (vulpea-db-autosync-mode 1)
  (vulpea-db-sync-full-scan))

(defun org-graph/seed-org-id-locations ()
  "Seed Emacs's global `org-id-locations' from the vulpea DB.
Iterate every note returned by `vulpea-db-query' and register its
id -> path with `org-id-add-location'.  Notes missing an id or path are
skipped.  Cheap; intended to run once per session — the org-graph loader
defers it to `emacs-startup-hook' — so `id:' links resolve on a fresh
session without waiting for vulpea's lazy per-file-touch registration."
  (dolist (note (vulpea-db-query))
    (let ((id (vulpea-note-id note))
          (path (vulpea-note-path note)))
      (when (and id path)
        (org-id-add-location id path)))))

(provide 'org-graph-discovery)
;;; discovery.el ends here
