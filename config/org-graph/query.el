;;; query.el --- Typed-edge query API -*- lexical-binding: t; -*-

(defun org-graph-query--select (column id &optional rel-type)
  "Return raw `typed_edges' rows where COLUMN equals ID.
COLUMN is the identifier symbol `from-id' or `to-id'.  When REL-TYPE is
non-nil it further restricts rows to that relation SYMBOL.  Each returned
row is a positional list (FROM-ID REL-TYPE TO-ID), matching
`register/shape/typed-edge-tuple'."
  (if rel-type
      (emacsql (vulpea-db)
               `[:select [from-id rel-type to-id] :from typed_edges
                 :where (and (= ,column $s1) (= rel-type $s2))]
               id rel-type)
    (emacsql (vulpea-db)
             `[:select [from-id rel-type to-id] :from typed_edges
               :where (= ,column $s1)]
             id)))

(defun org-graph-query--row->edge (row other-id)
  "Build an edge plist from ROW, resolving OTHER-ID as the `:note'.
ROW is a (FROM-ID REL-TYPE TO-ID) triple; OTHER-ID is the org id of the
far end of the edge relative to the query subject."
  (list :from (nth 0 row)
        :rel  (nth 1 row)
        :to   (nth 2 row)
        :note (and other-id (vulpea-db-get-by-id other-id))))

(defun org-graph-query/outgoing (from-id &optional rel-type)
  "Return outgoing typed edges authored on note FROM-ID.
Each result is an edge plist (see `org-graph-query--row->edge') whose
`:note' is the resolved destination note.  With REL-TYPE (a relation
SYMBOL, e.g. `implements') only edges of that relation are returned.
Directional: this is NOT symmetrized with `org-graph-query/incoming'."
  (mapcar (lambda (row)
            (org-graph-query--row->edge row (nth 2 row)))
          (org-graph-query--select 'from-id from-id rel-type)))

(defun org-graph-query/incoming (to-id &optional rel-type)
  "Return incoming typed edges that point at note TO-ID.
Each result is an edge plist (see `org-graph-query--row->edge') whose
`:note' is the resolved source note (the note authoring the edge).  With
REL-TYPE (a relation SYMBOL) only edges of that relation are returned.
Edges are stored directionally and are NOT auto-symmetrized, so this is a
real query against the `to-id' column, not a mirror of
`org-graph-query/outgoing'."
  (mapcar (lambda (row)
            (org-graph-query--row->edge row (nth 0 row)))
          (org-graph-query--select 'to-id to-id rel-type)))

(defun org-graph-query/connected (note-id)
  "Return every typed edge touching NOTE-ID, the union of outgoing and incoming.
Outgoing edges come first.  Because edges are directional and not
symmetrized, a self-edge (FROM = TO = NOTE-ID) legitimately appears in
both halves.  Each `:note' is the resolved far end relative to NOTE-ID."
  (append (org-graph-query/outgoing note-id)
          (org-graph-query/incoming note-id)))

(provide 'org-graph-query)
;;; query.el ends here
