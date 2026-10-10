;;; finders.el --- Schema-aware note-type finders -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'vulpea)
(require 'org-graph-schemas)

(defun org-graph/note-of-type-p (note type)
  "Return non-nil when NOTE is of note-type TYPE.
TYPE is a symbol from `org-graph-note-types'.  Delegates to the
note-type schema's predicate (the single source of truth for \"what a
note of type X is\") via `vulpea-schema-applies-p', rather than
re-implementing filetag matching.  Returns nil when no schema is
registered for TYPE."
  (let ((name (org-graph-schemas--schema-name type)))
    (and (vulpea-schema-get name)
         (vulpea-schema-applies-p note name))))

(defun org-graph-finders--type-filter (type)
  "Return a `vulpea-find' :filter-fn admitting notes of note-type TYPE.
The returned predicate takes a `vulpea-note' and delegates to
`org-graph/note-of-type-p'."
  (lambda (note) (org-graph/note-of-type-p note type)))

(defun org-graph/find-topic ()
  "Find and visit a topic note."
  (interactive)
  (vulpea-find :filter-fn (org-graph-finders--type-filter 'topic)
               :require-match t))

(defun org-graph/find-debug ()
  "Find and visit a debug note."
  (interactive)
  (vulpea-find :filter-fn (org-graph-finders--type-filter 'debug)
               :require-match t))

(defun org-graph/find-log ()
  "Find and visit a log note."
  (interactive)
  (vulpea-find :filter-fn (org-graph-finders--type-filter 'log)
               :require-match t))

(defun org-graph/find-reference ()
  "Find and visit a reference note."
  (interactive)
  (vulpea-find :filter-fn (org-graph-finders--type-filter 'reference)
               :require-match t))

(defun org-graph/find-project ()
  "Find and visit a project note."
  (interactive)
  (vulpea-find :filter-fn (org-graph-finders--type-filter 'project)
               :require-match t))

(defun org-graph/find-any ()
  "Find and visit any indexed note, regardless of note type."
  (interactive)
  (vulpea-find :require-match t))

(defun org-graph-finders--agent-draft-p (note)
  "Return non-nil when NOTE carries the cross-cutting agent-draft filetag."
  (and (member "agent-draft" (vulpea-note-tags note)) t))

(defun org-graph/find-agent-drafts ()
  "Find and visit a note stamped with the agent-draft filetag.
agent-draft is NOT a note type; it is a cross-cutting filetag the write
tool stamps on agent-authored drafts.  This finder filters the filetag
directly and deliberately does not route through the note-type schemas."
  (interactive)
  (vulpea-find :filter-fn #'org-graph-finders--agent-draft-p
               :require-match t))

(provide 'org-graph-finders)
;;; finders.el ends here
