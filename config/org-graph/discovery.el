;;; discovery.el --- org-graph registry-driven vulpea discovery -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar workspace--registry)
(declare-function workspace--registered-names "workspace-tabs" ())
(declare-function workspace--home "workspace-data-model" (ws))
(declare-function workspace--sessions-dir "workspace-data-model" (home))

(defvar vulpea-db-sync-directories)
(declare-function vulpea-db-autosync-mode "vulpea-db-sync" (&optional arg))
(declare-function vulpea-db-sync-full-scan "vulpea-db-sync" (&optional arg))
(declare-function vulpea-db-query "vulpea-db-query" (&optional predicate))
(declare-function vulpea-note-id "vulpea-db" (note))
(declare-function vulpea-note-path "vulpea-db" (note))

(defun org-graph--active-workspace-homes ()
  "Return active workspace `:home' dirs and their `sessions/' subdirs.
Reads the `workspaces' registry (`workspace--registry').  Each home is
returned canonicalised (absolute, trailing slash) and followed by its
`sessions/' subdirectory.  Returns nil when the `workspaces' feature is
not loaded — workspaces is a soft dependency."
  (when (featurep 'workspaces)
    (let (dirs)
      (dolist (name (workspace--registered-names))
        (let* ((ws (gethash name workspace--registry))
               (home (and ws (workspace--home ws))))
          (when home
            (push (file-name-as-directory (expand-file-name home)) dirs)
            (push (file-name-as-directory (workspace--sessions-dir home))
                  dirs))))
      (nreverse dirs))))

(defun org-graph/index-roots ()
  "Return the explicit list of directories vulpea SHALL index.
This is `org-graph-roam-root' (canonicalised) plus, when
`org-graph-watch-workspace-homes' is non-nil and the `workspaces'
feature is loaded, each active workspace `:home' directory and its
`sessions/' subdirectory from the workspaces registry.

This is a bounded, explicit set.  org-graph deliberately NEVER walks a
wider tree (e.g. ~/work) to discover notes: the registry already
enumerates the handful of homes the user created
\(`register/invariant/bounded-discovery-roots')."
  (let ((roots (list (file-name-as-directory
                      (expand-file-name
                       (if (boundp 'org-graph-roam-root)
                           org-graph-roam-root
                         "~/org/roam/"))))))
    (when (and (boundp 'org-graph-watch-workspace-homes)
               org-graph-watch-workspace-homes)
      (setq roots (append roots (org-graph--active-workspace-homes))))
    (delete-dups roots)))

(defun org-graph/configure-sync ()
  "Point vulpea at the bounded discovery roots and enable autosync.
Sets `vulpea-db-sync-directories' to `org-graph/index-roots', enables
`vulpea-db-autosync-mode', and triggers an initial (async) full scan.

Idempotent: re-running overwrites the directory list, leaves autosync
enabled, and re-scans with smart change detection.  Interactive so it
can be invoked after the active workspace set changes."
  (interactive)
  (setq vulpea-db-sync-directories (org-graph/index-roots))
  (vulpea-db-autosync-mode 1)
  (vulpea-db-sync-full-scan))

(defun org-graph/seed-org-id-locations ()
  "Seed Emacs's global `org-id-locations' from the vulpea DB.
Iterate every note returned by `vulpea-db-query' and register its
id -> path with `org-id-add-location'.  Notes missing an id or path are
skipped.  Cheap; intended to run once at module load so `id:' links
resolve on a fresh session without waiting for vulpea's lazy
per-file-touch registration."
  (dolist (note (vulpea-db-query))
    (let ((id (vulpea-note-id note))
          (path (vulpea-note-path note)))
      (when (and id path)
        (org-id-add-location id path)))))

(ignore-errors (org-graph/seed-org-id-locations))

(provide 'org-graph-discovery)
;;; discovery.el ends here
