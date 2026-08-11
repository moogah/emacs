;;; org-graph.el --- Layered knowledge graph over vulpea -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup org-graph nil
  "Layered knowledge graph over vulpea, plugged into workspaces."
  :group 'convenience
  :prefix "org-graph-")

(defcustom org-graph-roam-root "~/org/roam/"
  "Durable concept vault and typed-edge extraction scope.
This directory is always indexed by vulpea, and it is the only root on
which the typed-edge extractor runs.  Workspace homes are indexed for
discovery/navigation but excluded from typed-edge extraction."
  :type 'directory
  :group 'org-graph)

(defcustom org-graph-relation-types
  '(implements contradicts supersedes relates-to)
  "Closed set of typed-edge relation symbols recognised by the extractor.
Each symbol maps to an uppercased PROPERTIES-drawer key (e.g. `implements'
-> :IMPLEMENTS:)."
  :type '(repeat symbol)
  :group 'org-graph)

(defcustom org-graph-watch-workspace-homes t
  "When non-nil, add workspace `:home' directories to vulpea's sync roots.
Consumed by the registry-driven discovery layer and the workspace
`:on-create' integration handler."
  :type 'boolean
  :group 'org-graph)

(defcustom org-graph-note-types
  '(log debug topic reference project)
  "Fixed taxonomy of note-type symbols backed by vulpea-schema definitions.
Each symbol has a schema with a predicate (typically a filetag) selecting
its notes and field expectations validated by `vulpea-schema-validate'."
  :type '(repeat symbol)
  :group 'org-graph)

(use-package vulpea
  :straight (vulpea :type git :host github :repo "d12frosted/vulpea"
                    :branch "v2.4.0")
  :config
  (setq vulpea-db-location
        (expand-file-name "state/vulpea/notes.db" user-emacs-directory))
  (make-directory (file-name-directory vulpea-db-location) t))

(jf/load-module (expand-file-name "config/org-graph/schemas.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/extractor.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/coordinator.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/query.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/finders.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/tools.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/discovery.el" jf/emacs-dir))
(jf/load-module (expand-file-name "config/org-graph/workspace-integration.el" jf/emacs-dir))

(defun org-graph--run-deferred-op (fn what)
  "Run FN, a deferred DB-touching org-graph op, resiliently.
Shared body for the org-graph `emacs-startup-hook' deferrals: FN opens
the vulpea DB and so runs post-init rather than at module-load time
(module load stays DB-free).  WHAT is a short description used in the
warning when FN fails.  A failure (e.g. a missing/unbuilt DB) is logged
via `display-warning' rather than aborting startup."
  (condition-case err
      (funcall fn)
    (error
     (display-warning 'org-graph
                      (format "%s skipped: %S" what err)
                      :warning))))

(defun org-graph--seed-org-id-locations-deferred ()
  "Run the `org-id-locations' seed once, resiliently, for `emacs-startup-hook'.
`org-graph/seed-org-id-locations' calls `vulpea-db-query', which opens
the vulpea DB, so it is deferred out of module-load time (load stays
DB-free).  A missing/unbuilt DB is logged via `display-warning' rather
than aborting startup."
  (org-graph--run-deferred-op #'org-graph/seed-org-id-locations
                              "org-id-locations seed"))

(add-hook 'emacs-startup-hook #'org-graph--seed-org-id-locations-deferred)

(defun org-graph--register-extractor ()
  "Register the org-graph typed-edge extractor with vulpea, resiliently.
Wraps `org-graph-extractor-register' (which applies the `typed_edges'
schema and so opens the vulpea DB) for `emacs-startup-hook'.  A
missing/unbuilt DB is logged via `display-warning' rather than aborting
startup."
  (org-graph--run-deferred-op #'org-graph-extractor-register
                              "typed-edge extractor registration"))

(add-hook 'emacs-startup-hook #'org-graph--register-extractor)

(provide 'org-graph)
;;; org-graph.el ends here
