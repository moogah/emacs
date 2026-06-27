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

;; implemented in registry-discovery

;; implemented in auto-id-scaffold

;; implemented in note-type-schemas

;; implemented in finders-and-filters

;; implemented in parse-typed-edges

;; implemented in vulpea-extractor-plugin

;; implemented in typed-edge-query

;; implemented in coordinator-lock

;; implemented in gptel-tools

;; implemented in workspace-integration

;; Submodule loads land here as tasks complete, e.g.:
;; (jf/load-module (expand-file-name "config/org-graph/discovery.el" jf/emacs-dir))

;; Schema-aware note-type finders.  Requires `org-graph-schemas' (the
;; finders' :filter-fn delegates to the note-type schema predicates), so
;; the schemas module must be loaded before this; wire-into-init owns the
;; full ordered load sequence.
(jf/load-module (expand-file-name "config/org-graph/finders.el" jf/emacs-dir))

(provide 'org-graph)
;;; org-graph.el ends here
