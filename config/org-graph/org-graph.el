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
  "Completion seed of typed-edge relation symbols.
The relation vocabulary is OPEN: the extractor accepts any relation an
author coins as an edge-drawer item tag (see `org-graph-edge-drawer'),
and this list gates NOTHING.  It only seeds completion on authoring
surfaces; author-coined relations are first-class the moment they
appear in a note."
  :type '(repeat symbol)
  :group 'org-graph)

(defcustom org-graph-edge-drawer "EDGES"
  "Name of the drawer holding typed-edge description-list items.
Each item `- <type> :: [[id:...]]' inside this drawer declares one
typed edge per `id:' link; the item tag (normalized) is the relation.
The drawer name is matched case-insensitively and is the ONLY edge
discriminator: ordinary PROPERTIES entries and body links are never
edges.

Export: unlike properties, custom drawers export by default.  The
loader therefore adds this drawer to `org-export-with-drawers''
exclusion list -- but only while that variable still has its default
value ((not \"LOGBOOK\")).  If you have customized
`org-export-with-drawers' yourself, the loader leaves it alone and you
own excluding this drawer from export.

Org can only parse drawer names made of word characters, `-' and `_';
setting a value outside that alphabet triggers a warning, and no drawer
will ever match it.

Renaming through the customize setter keeps the export exclusion in
step: when `org-export-with-drawers' still holds the loader-managed
shape ((not \"LOGBOOK\" <old-name>)), the setter swaps in the new name.
A `setq' rename bypasses this — re-evaluate the exclusion or restart."
  :type 'string
  :group 'org-graph
  :set (lambda (symbol value)
         (when (and (stringp value)
                    (not (string-match-p "\\`[[:word:]_-]+\\'" value)))
           (display-warning
            'org-graph
            (format "`org-graph-edge-drawer' value %S is not a valid org drawer name (word characters, `-' and `_' only); no drawer will match it"
                    value)
            :warning))
         (let ((old (and (boundp symbol) (symbol-value symbol))))
           (set-default symbol value)
           ;; LD-6: if ox already loaded and the exclusion list still has
           ;; the loader-managed shape for the OLD name, follow the rename;
           ;; a user-customized `org-export-with-drawers' is left alone.
           (when (and (stringp old)
                      (boundp 'org-export-with-drawers)
                      (equal org-export-with-drawers (list 'not "LOGBOOK" old)))
             (setq org-export-with-drawers (list 'not "LOGBOOK" value))))))

(with-eval-after-load 'ox
  (when (equal org-export-with-drawers '(not "LOGBOOK"))
    (setq org-export-with-drawers
          (list 'not "LOGBOOK" org-graph-edge-drawer))))

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
(jf/load-module (expand-file-name "config/org-graph/edge-type.el" jf/emacs-dir))
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
