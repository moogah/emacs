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

(provide 'org-graph-extractor)
;;; extractor.el ends here
