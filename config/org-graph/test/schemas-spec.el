;;; schemas-spec.el --- Note-type schema tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for the org-graph note-type schemas (note-type-schemas
;; task / RE-3).  The vulpea-schema *validation engine* is pure: it reads a
;; note's already-extracted metadata in memory and reports violations
;; without touching the DB (none of the org-graph field specs use the `note'
;; reference type, which is the only field path that would query the DB).
;; So these specs exercise the REAL `vulpea-schema-define' /
;; `vulpea-schema-validate' against real fixtures rather than stubbing them
;; -- stubbing the engine would make the field-spec assertions vacuous.
;;
;; Only the DB boundary is mocked: `org-graph/validate-all-of-type' goes
;; through `vulpea-schema-validate-all', which calls `vulpea-db-query'.  That
;; one call is shadowed with a local `cl-letf' that applies the schema's
;; predicate to an in-memory note list, so the real predicate + validate run
;; end-to-end with no SQLite.  (`org-graph-test/with-stubbed-vulpea' stubs
;; `vulpea-schema-validate' but NOT `vulpea-schema-validate-all', so a local
;; stub is the right tool here -- see the task's helper-gap addendum.)

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-schemas (expand-file-name "schemas.el" module-dir)))

;; `org-graph-note-types' is owned by the loader (a defcustom) which is not
;; loaded in this standalone process; declare it special so the schemas fall
;; back to their internal default.
(defvar org-graph-note-types)

(describe "org-graph note-type schemas"

  (before-each
    ;; Re-register before every spec so the registry is in a known state
    ;; even if another spec unregistered or replaced an entry.
    (org-graph-schemas-register))

  (describe "registration"
    (it "registers a schema for each of the five note types"
      (dolist (type '(log debug topic reference project))
        (expect (vulpea-schema-get (org-graph-schemas--schema-name type))
                :not :to-be nil)))

    (it "does NOT register a schema for the agent-draft filetag"
      ;; agent-draft is a cross-cutting filetag, not a taxonomy member.
      (expect (vulpea-schema-get 'org-graph-agent-draft) :to-be nil)))

  (describe "predicate selection"
    (it "selects only notes carrying the matching filetag"
      (let ((ref (org-graph-test/note-fixture
                  :id "r1" :title "A reference" :tags '("reference")))
            (log (org-graph-test/note-fixture
                  :id "l1" :title "A log" :tags '("log"))))
        (expect (vulpea-schema-applies-p ref 'org-graph-reference) :to-be-truthy)
        (expect (vulpea-schema-applies-p log 'org-graph-reference) :to-be nil)
        (expect (vulpea-schema-applies-p log 'org-graph-log) :to-be-truthy)))

    (it "ignores notes with no filetags at all"
      (let ((bare (org-graph-test/note-fixture :id "b1" :title "Bare")))
        (dolist (type '(log debug topic reference project))
          (expect (vulpea-schema-applies-p
                   bare (org-graph-schemas--schema-name type))
                  :to-be nil)))))

  (describe "vulpea-schema-validate via org-graph/validate-note-type"
    (it "reports zero violations for a conformant note"
      ;; A reference whose required `source' meta key is present.
      (let ((note (org-graph-test/note-fixture
                   :id "r2" :title "Conformant ref" :tags '("reference")
                   :meta '(("source" "https://example.com")))))
        (expect (org-graph/validate-note-type note) :to-be nil)))

    (it "reports a missing-required violation when a required field is absent"
      ;; A reference with no `source' meta key.
      (let* ((note (org-graph-test/note-fixture
                    :id "r3" :title "Bad ref" :tags '("reference")))
             (violations (org-graph/validate-note-type note)))
        (expect (length violations) :to-equal 1)
        (expect (vulpea-violation-type (car violations)) :to-equal 'missing-required)
        (expect (vulpea-violation-field (car violations)) :to-equal "source")))

    (it "reports a missing-required violation for a project lacking status"
      (let* ((note (org-graph-test/note-fixture
                    :id "p1" :title "Bad project" :tags '("project")))
             (violations (org-graph/validate-note-type note)))
        (expect (length violations) :to-equal 1)
        (expect (vulpea-violation-type (car violations)) :to-equal 'missing-required)
        (expect (vulpea-violation-field (car violations)) :to-equal "status")))

    (it "treats lighter types (log) with no required fields as conformant"
      (let ((note (org-graph-test/note-fixture
                   :id "l2" :title "A log" :tags '("log"))))
        (expect (org-graph/validate-note-type note) :to-be nil)))

    (it "returns nil for a note matching no note-type schema"
      (let ((note (org-graph-test/note-fixture
                   :id "x1" :title "Untyped" :tags '("misc"))))
        (expect (org-graph/validate-note-type note) :to-be nil)))

    (it "flags a disallowed project status value"
      (let* ((note (org-graph-test/note-fixture
                    :id "p2" :title "Weird project" :tags '("project")
                    :meta '(("status" "frozen"))))
             (violations (org-graph/validate-note-type note)))
        (expect (length violations) :to-equal 1)
        (expect (vulpea-violation-type (car violations))
                :to-equal 'disallowed-value))))

  (describe "org-graph/validate-all-of-type"
    ;; `vulpea-schema-validate-all' calls `vulpea-db-query' with the schema
    ;; predicate; shadow ONLY that one DB call with a local stub that applies
    ;; the predicate to an in-memory note list, so the real predicate and the
    ;; real validate run end-to-end without a database.
    (it "validates only the predicate-matched notes and aggregates violations"
      (let* ((good-ref (org-graph-test/note-fixture
                        :id "g1" :title "Good ref" :tags '("reference")
                        :meta '(("source" "https://ok"))))
             (bad-ref  (org-graph-test/note-fixture
                        :id "b1" :title "Bad ref" :tags '("reference")))
             (a-log    (org-graph-test/note-fixture
                        :id "l3" :title "A log" :tags '("log")))
             (all (list good-ref bad-ref a-log)))
        (cl-letf (((symbol-function 'vulpea-db-query)
                   (lambda (filter) (seq-filter filter all))))
          (let ((violations (org-graph/validate-all-of-type 'reference)))
            ;; Only the two references are considered (the log is filtered
            ;; out by the predicate); only the bad one violates.
            (expect (length violations) :to-equal 1)
            (expect (vulpea-violation-note-id (car violations)) :to-equal "b1")
            (expect (vulpea-violation-type (car violations))
                    :to-equal 'missing-required)))))

    (it "returns nil when every matched note conforms"
      (let* ((good (org-graph-test/note-fixture
                    :id "g2" :title "Good ref" :tags '("reference")
                    :meta '(("source" "https://ok")))))
        (cl-letf (((symbol-function 'vulpea-db-query)
                   (lambda (filter) (seq-filter filter (list good)))))
          (expect (org-graph/validate-all-of-type 'reference) :to-be nil))))))

;;; schemas-spec.el ends here
