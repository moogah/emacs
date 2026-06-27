;;; extractor.el --- org-graph typed-edge extractor -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org-element)

(defconst org-graph-extractor--default-relation-types
  '(implements contradicts supersedes relates-to)
  "Fallback relation-type set when `org-graph-relation-types' is unbound.
Kept in sync with the loader's defcustom default so the parser behaves
identically whether loaded standalone or as part of the full module.")

(defun org-graph-extractor--relation-types ()
  "Return the active list of relation-type symbols."
  (if (boundp 'org-graph-relation-types)
      org-graph-relation-types
    org-graph-extractor--default-relation-types))

(defun org-graph-extractor--rel-key (symbol)
  "Return the PROPERTIES-drawer key string for relation SYMBOL.
E.g. `relates-to' -> \"RELATES_TO\"."
  (upcase (replace-regexp-in-string "-" "_" (symbol-name symbol))))

(defun org-graph-extractor--key->rel (key)
  "Return the relation symbol for PROPERTIES KEY, or nil if not configured.
KEY is matched case-insensitively against the configured relation types."
  (let ((upper (upcase key)))
    (cl-find-if (lambda (sym)
                  (equal upper (org-graph-extractor--rel-key sym)))
                (org-graph-extractor--relation-types))))

(defun org-graph-extractor--ids-in-value (value)
  "Return the list of `id:' link targets in VALUE, in order.
VALUE is the raw string of a PROPERTIES entry.  Returns nil for an
empty or malformed value rather than signalling."
  (when (stringp value)
    (let ((start 0) ids)
      (while (string-match "\\[\\[id:\\([^]]+\\)\\]" value start)
        (push (match-string 1 value) ids)
        (setq start (match-end 0)))
      (nreverse ids))))

(defun org-graph-extractor/parse-typed-edges (element-tree note-id)
  "Parse typed-edge relations from ELEMENT-TREE for note NOTE-ID.

ELEMENT-TREE is an `org-element' AST (as from `org-element-parse-buffer').
Reads PROPERTIES-drawer entries whose key matches a configured relation
type (`org-graph-relation-types'), extracts each value's `id:' link
targets, and returns a list of (FROM-ID REL-TYPE TO-ID) tuples where
FROM-ID is NOTE-ID and REL-TYPE is the relation symbol (e.g. `implements').

A relation property MAY appear multiple times and MAY carry multiple
links; each (property-occurrence, link) pair yields its own row.
Directional, explicitly-authored edges: no inverse is derived.

This is a pure function — no file I/O, no vulpea, no DB.  Malformed or
empty values are skipped; the function never signals on bad input."
  (let (edges)
    (org-element-map element-tree 'node-property
      (lambda (np)
        (let ((rel (org-graph-extractor--key->rel
                    (org-element-property :key np))))
          (when rel
            (dolist (to-id (org-graph-extractor--ids-in-value
                            (org-element-property :value np)))
              (push (list note-id rel to-id) edges))))))
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

(defun org-graph-extractor--note-property-drawer (element-tree note-id)
  "Return the `property-drawer' in ELEMENT-TREE owned by NOTE-ID, or nil.
A note owns the drawer whose `:ID:' node-property equals NOTE-ID: this
selects the file-level drawer for a file node and a heading's own drawer
for a heading node, never a descendant heading's drawer.  Returns nil
when no drawer carries NOTE-ID (e.g. an ID'd note that authored no
PROPERTIES of its own)."
  (org-element-map element-tree 'property-drawer
    (lambda (drawer)
      (when (org-element-map drawer 'node-property
              (lambda (np)
                (and (equal (org-element-property :key np) "ID")
                     (equal (org-element-property :value np) note-id)))
              nil t)
        drawer))
    nil t))

(defun org-graph-extractor--edges-from-note (element-tree note-id)
  "Return the typed-edge tuples authored by NOTE-ID's OWN drawer.
Scopes extraction to the single note vulpea is processing: finds the
property-drawer whose `:ID:' equals NOTE-ID (see
`org-graph-extractor--note-property-drawer') and parses ONLY that drawer
with the pure parser, so in a multi-note file each note is credited with
just its own edges — no whole-file duplication, `from-id' = NOTE-ID.
Parsing the drawer sub-tree (not `note-data''s `:properties' alist)
preserves repeated relation keys.  Returns nil when the note owns no
drawer."
  (let ((drawer (org-graph-extractor--note-property-drawer
                 element-tree note-id)))
    (when drawer
      (org-graph-extractor/parse-typed-edges drawer note-id))))

(defun org-graph-extractor/extract (ctx note-data)
  "Vulpea extractor: write the note's typed edges into `typed_edges'.

CTX is a `vulpea-parse-ctx' (provides the file AST and path); NOTE-DATA
is the note plist vulpea is building.  Edges are scoped to the note's OWN
PROPERTIES drawer (see `org-graph-extractor--edges-from-note') and emitted
only for a note under `org-graph-roam-root' (scope gate).  Inserts
\(from-id rel-type to-id) rows, rel-type stored as a symbol.  Returns
NOTE-DATA unchanged, per the extractor contract."
  (let ((note-id (plist-get note-data :id))
        (path (vulpea-parse-ctx-path ctx)))
    (when (and note-id
               (org-graph-extractor--roam-note-p path))
      (let ((edges (org-graph-extractor--edges-from-note
                    (vulpea-parse-ctx-ast ctx) note-id)))
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
    :version 1
    :priority 50
    :schema '((typed_edges
               [(from-id :not-null) (rel-type :not-null) (to-id :not-null)]
               (:foreign-key [from-id] :references notes [id] :on-delete :cascade)))
    :extract-fn #'org-graph-extractor/extract)))

(provide 'org-graph-extractor)
;;; extractor.el ends here
