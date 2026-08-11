;;; extractor.el --- org-graph typed-edge extractor -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)
(require 'org-element)

(defvar org-graph-edge-drawer)          ; defined by the loader's defcustom

(defun org-graph-extractor--edge-drawer-name ()
  "Return the configured edge-drawer name, or nil when unavailable.
`org-graph-edge-drawer' is owned by the loader's defcustom.  When this
file is loaded standalone and the variable is unbound, nil, or empty,
return nil so the scanner fails closed (no drawer ever matches) rather
than guessing a name."
  (and (boundp 'org-graph-edge-drawer)
       (stringp org-graph-edge-drawer)
       (not (string-empty-p org-graph-edge-drawer))
       org-graph-edge-drawer))

(defun org-graph-extractor--normalize-rel (raw)
  "Return the canonical relation symbol for RAW relation text, or nil.
Normalization contract (register/vocabulary/relation-types): trim,
downcase, map each run of whitespace and underscores to a single
hyphen, intern.  E.g. \"follows up\" -> `follows-up', \"FOLLOWS_UP\" ->
`follows-up', \"Implements\" -> `implements'.  Returns nil for a nil
RAW or text that is empty after trimming."
  (let ((trimmed (string-trim (or raw ""))))
    (unless (string-empty-p trimmed)
      (intern (replace-regexp-in-string
               "[[:space:]_]+" "-" (downcase trimmed))))))

(defun org-graph-extractor--enclosing-note-id (element)
  "Return the note id ELEMENT's edges attribute to, or nil.
Walks ELEMENT's ancestors via `org-element-lineage' for the nearest
`headline' carrying its own `:ID:' property, falling back to the
file-level node (the root `org-data', which carries the file's
top-level PROPERTIES drawer).  Returns nil when no ancestor carries an
id; the caller MUST then drop the edge — an edge with no ID-bearing
ancestor is never attributed to a neighbouring node."
  (let ((node (org-element-lineage element '(headline org-data)))
        id)
    (while (and node (not id))
      (setq id (org-element-property :ID node))
      (unless id
        (setq node (org-element-lineage node '(headline org-data)))))
    id))

(defun org-graph-extractor--item-relation (item)
  "Return ITEM's relation symbol, or nil for an untagged item.
ITEM is a plain-list `item' element.  A description-list item's tag
carries the relation (LD-2); the tag is a parsed secondary string, so
it is interpreted back to plain text and normalized via
`org-graph-extractor--normalize-rel'.  Items with no tag (plain list
items, or `- :: ...' items org parses as untagged) yield nil."
  (let ((tag (org-element-property :tag item)))
    (when tag
      (org-graph-extractor--normalize-rel
       (substring-no-properties (org-element-interpret-data tag))))))

(defun org-graph-extractor/parse-drawer-edges (element-tree)
  "Parse typed edges from the edge drawer(s) in ELEMENT-TREE.

ELEMENT-TREE is an `org-element' AST (as from `org-element-parse-buffer').
Scans every `drawer' whose name equals `org-graph-edge-drawer'
\(case-insensitively); within each, every description-list item's tag is
normalized to the relation symbol and each `id:' link in the item yields
one (FROM-ID REL-TYPE TO-ID) tuple.  FROM-ID is resolved per drawer by
`org-graph-extractor--enclosing-note-id' — the nearest ID-bearing
ancestor; a drawer with none contributes nothing.

The relation vocabulary is OPEN: any author-coined tag is a valid
relation; nothing gates membership.  Ordinary PROPERTIES entries and
body links are never edges (the drawer name is the only discriminator).

This is a pure function — no file I/O, no vulpea, no DB.  Non-item
drawer content, untagged items, and non-`id:' links are skipped; the
function never signals on malformed input."
  (let ((drawer-name (org-graph-extractor--edge-drawer-name))
        edges)
    (when drawer-name
      (org-element-map element-tree 'drawer
        (lambda (drawer)
          (when (string-equal
                 (downcase (or (org-element-property :drawer-name drawer) ""))
                 (downcase drawer-name))
            (let ((from-id (org-graph-extractor--enclosing-note-id drawer)))
              (when from-id
                (org-element-map drawer 'item
                  (lambda (item)
                    (let ((rel (org-graph-extractor--item-relation item)))
                      (when rel
                        (org-element-map (org-element-contents item) 'link
                          (lambda (link)
                            (when (equal (org-element-property :type link) "id")
                              (push (list from-id rel
                                          (org-element-property :path link))
                                    edges)))
                          nil nil 'item))))
                  nil nil 'item)))))))
    (nreverse edges)))

(defvar org-graph-roam-root)            ; defined by the loader's defcustom

(defun org-graph-extractor--roam-note-p (path)
  "Return non-nil when typed-edge extraction is in scope for PATH.
PATH is in scope only when it lives under `org-graph-roam-root'.  A
non-string PATH or an unbound/nil/empty `org-graph-roam-root' yields nil,
so the extractor fails closed (no edges) rather than indexing
out-of-scope notes.  The empty-string guard matters: an empty root would
otherwise `expand-file-name' to `default-directory' and silently scope
extraction to the current working directory."
  (and (stringp path)
       (boundp 'org-graph-roam-root)
       (stringp org-graph-roam-root)
       (not (string-empty-p (string-trim org-graph-roam-root)))
       (string-prefix-p
        (file-name-as-directory (expand-file-name org-graph-roam-root))
        (expand-file-name path))))

(defun org-graph-extractor/extract (ctx note-data)
  "Vulpea extractor: write the note's typed edges into `typed_edges'.

CTX is a `vulpea-parse-ctx' (provides the file AST and path); NOTE-DATA
is the note plist vulpea is building.  Runs the pure drawer scanner over
the whole-file AST and keeps only the tuples the shared enclosing-node
walk attributed to THIS note (from-id = the note's id), so a multi-note
file never duplicates rows across its notes.  Emits only for a note
under `org-graph-roam-root' (scope gate).  Inserts
\(from-id rel-type to-id) rows, rel-type stored as a symbol.  Returns
NOTE-DATA unchanged, per the extractor contract."
  (let ((note-id (plist-get note-data :id))
        (path (vulpea-parse-ctx-path ctx)))
    (when (and note-id
               (org-graph-extractor--roam-note-p path))
      (let ((edges (seq-filter
                    (lambda (edge) (equal (nth 0 edge) note-id))
                    (org-graph-extractor/parse-drawer-edges
                     (vulpea-parse-ctx-ast ctx)))))
        (when edges
          (emacsql (vulpea-db)
                   [:insert :into typed_edges :values $v1]
                   (mapcar (lambda (edge)
                             (vector (nth 0 edge)    ; from-id  (string)
                                     (nth 1 edge)    ; rel-type (symbol)
                                     (nth 2 edge)))  ; to-id    (string)
                           edges))))))
  note-data)

(defun org-graph-extractor-register ()
  "Register the org-graph typed-edge extractor with vulpea.
Applies the `typed_edges' schema and installs `org-graph-extractor/extract'
at priority 50.  Idempotent; intended to be called by the loader."
  (vulpea-db-register-extractor
   (make-vulpea-extractor
    :name 'org-graph-typed-edges
    :version 2
    :priority 50
    :schema '((typed_edges
               [(from-id :not-null) (rel-type :not-null) (to-id :not-null)]
               (:foreign-key [from-id] :references notes [id] :on-delete :cascade)))
    :extract-fn #'org-graph-extractor/extract)))

(provide 'org-graph-extractor)
;;; extractor.el ends here
