;;; schemas.el --- org-graph note-type schemas -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'vulpea)

(defconst org-graph-schemas--default-note-types
  '(log debug topic reference project)
  "Fallback note-type set when `org-graph-note-types' is unbound.
Kept in sync with the loader's defcustom default so the schemas behave
identically whether loaded standalone or as part of the full module.")

(defun org-graph-schemas--note-types ()
  "Return the active list of note-type symbols."
  (if (boundp 'org-graph-note-types)
      org-graph-note-types
    org-graph-schemas--default-note-types))

(defun org-graph-schemas--schema-name (type)
  "Return the vulpea-schema name symbol for note-type TYPE.
E.g. `topic' -> `org-graph-topic'."
  (intern (format "org-graph-%s" type)))

(defun org-graph-schemas--tag-predicate (tag)
  "Return a predicate selecting notes carrying filetag TAG.
The returned function takes a `vulpea-note' and is non-nil when TAG is
among the note's tags."
  (lambda (note) (and (member tag (vulpea-note-tags note)) t)))

(defun org-graph-schemas-register ()
  "Define and register a vulpea-schema for each org-graph note type.
Idempotent: re-running replaces the existing registrations.  Returns the
list of registered schema names."
  (vulpea-schema-define 'org-graph-log
    :predicate (org-graph-schemas--tag-predicate "log")
    :fields '((:key "date" :type string)))

  (vulpea-schema-define 'org-graph-debug
    :predicate (org-graph-schemas--tag-predicate "debug")
    :fields '((:key "status" :type symbol :one-of (open resolved))))

  (vulpea-schema-define 'org-graph-topic
    :predicate (org-graph-schemas--tag-predicate "topic")
    :fields '((:key "category" :type string)))

  (vulpea-schema-define 'org-graph-reference
    :predicate (org-graph-schemas--tag-predicate "reference")
    :fields '((:key "source" :type string :required t)))

  (vulpea-schema-define 'org-graph-project
    :predicate (org-graph-schemas--tag-predicate "project")
    :fields '((:key "status" :type symbol :required t
               :one-of (active paused done))))

  (mapcar #'org-graph-schemas--schema-name (org-graph-schemas--note-types)))

(org-graph-schemas-register)

(defun org-graph/validate-note-type (note)
  "Validate NOTE against whichever org-graph note-type schema(s) apply.
For each note-type in `org-graph-note-types' whose schema predicate
matches NOTE (by filetag), run `vulpea-schema-validate' and return the
appended list of `vulpea-violation's.  Returns nil when NOTE conforms or
when no note-type schema applies.  Pure with respect to the DB."
  (cl-loop for type in (org-graph-schemas--note-types)
           for name = (org-graph-schemas--schema-name type)
           when (and (vulpea-schema-get name)
                     (vulpea-schema-applies-p note name))
           append (vulpea-schema-validate note name)))

(defun org-graph/validate-all-of-type (type)
  "Return all violations for stored notes of note-type TYPE.
TYPE is a symbol from `org-graph-note-types'.  Thin wrapper over
`vulpea-schema-validate-all', which selects matching notes via the schema
predicate and validates each; this hits the vulpea DB."
  (vulpea-schema-validate-all (org-graph-schemas--schema-name type)))

(provide 'org-graph-schemas)
;;; schemas.el ends here
