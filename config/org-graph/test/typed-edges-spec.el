;;; typed-edges-spec.el --- Typed-edge query API tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the typed-edge read API in query.el:
;; `org-graph-query/outgoing', `org-graph-query/incoming', and
;; `org-graph-query/connected' (register/boundary/typed-edge-query-api).
;;
;; Two layers are mocked at the genuine vulpea boundary, never globally:
;;
;; 1. `org-graph-query--select' issues `(emacsql (vulpea-db) ...)' against
;;    the `typed_edges' side table.  `typed_edges' is NOT a vulpea-managed
;;    table, so there is no `vulpea-db-query'-style accessor to stub — the
;;    boundary is `emacsql' + `vulpea-db' (exactly as the extractor's write
;;    path is mocked in extractor-spec.el).  Those specs stub both and
;;    assert the emacsql query is built correctly (right column, rel-type
;;    bound as a SYMBOL per register/shape/typed-edge-tuple).
;;
;; 2. The public outgoing/incoming/connected specs stub `org-graph-query--select'
;;    (the DB seam) and `vulpea-db-get-by-id' (note resolution), so they
;;    isolate the plist shape, far-end note resolution, rel-type passthrough,
;;    and the union semantics of `connected'.  No live SQLite.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar org-graph-test/module-dir)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (setq org-graph-test/module-dir module-dir)
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-query (expand-file-name "query.el" module-dir)))

;;; Scaffolding ------------------------------------------------------------

(defvar org-graph-test/captured-query nil
  "The emacsql query vector captured by the stubbed `emacsql'.")
(defvar org-graph-test/captured-args nil
  "The emacsql substitution args captured by the stubbed `emacsql'.")

(defmacro org-graph-test/with-stubbed-emacsql (rows &rest body)
  "Run BODY with `vulpea-db' + `emacsql' stubbed; the select returns ROWS.
Captures the query vector and args into `org-graph-test/captured-query'
and `org-graph-test/captured-args' so specs can assert query construction.
No real DB is opened."
  (declare (indent 1) (debug (form body)))
  `(progn
     (setq org-graph-test/captured-query nil
           org-graph-test/captured-args nil)
     (cl-letf (((symbol-function 'vulpea-db)
                (lambda (&rest _) 'stub-db))
               ((symbol-function 'emacsql)
                (lambda (_db sql &rest args)
                  (setq org-graph-test/captured-query sql
                        org-graph-test/captured-args args)
                  ,rows)))
       ,@body)))

(defun org-graph-test/note-map (alist)
  "Return a `vulpea-db-get-by-id' stub resolving ids per ALIST.
ALIST maps an id string to the value the stub returns (a `vulpea-note' or
nil).  Ids absent from ALIST resolve to nil."
  (lambda (id) (cdr (assoc id alist))))

;;; org-graph-query--select (DB boundary) ----------------------------------

(describe "org-graph-query--select"

  (it "selects on the given column and returns the raw rows"
    (org-graph-test/with-stubbed-emacsql '(("n1" implements "abc"))
      (expect (org-graph-query--select 'from-id "n1")
              :to-equal '(("n1" implements "abc")))
      (expect org-graph-test/captured-args :to-equal '("n1"))
      ;; the column identifier is spliced into the WHERE clause
      (expect (member '(= from-id $s1)
                      (append org-graph-test/captured-query nil))
              :to-be-truthy)))

  (it "queries the to-id column when asked"
    (org-graph-test/with-stubbed-emacsql nil
      (org-graph-query--select 'to-id "abc")
      (expect (member '(= to-id $s1)
                      (append org-graph-test/captured-query nil))
              :to-be-truthy)))

  (it "binds rel-type as a SYMBOL, not a string, when filtering"
    (org-graph-test/with-stubbed-emacsql nil
      (org-graph-query--select 'from-id "n1" 'implements)
      ;; second substitution arg is the relation SYMBOL (round-trips via prin1)
      (expect org-graph-test/captured-args :to-equal '("n1" implements))
      (expect (symbolp (nth 1 org-graph-test/captured-args)) :to-be t)
      (expect (member '(and (= from-id $s1) (= rel-type $s2))
                      (append org-graph-test/captured-query nil))
              :to-be-truthy)))

  (it "omits the rel-type predicate when rel-type is nil"
    (org-graph-test/with-stubbed-emacsql nil
      (org-graph-query--select 'from-id "n1")
      (expect org-graph-test/captured-args :to-equal '("n1")))))

;;; org-graph-query/outgoing -----------------------------------------------

(describe "org-graph-query/outgoing"

  (it "returns an edge plist per outgoing row, resolving the destination note"
    (let ((note-abc (org-graph-test/note-fixture :id "abc" :title "ABC")))
      (cl-letf (((symbol-function 'org-graph-query--select)
                 (lambda (&rest _) '(("n1" implements "abc"))))
                ((symbol-function 'vulpea-db-get-by-id)
                 (org-graph-test/note-map (list (cons "abc" note-abc)))))
        (expect (org-graph-query/outgoing "n1")
                :to-equal (list (list :from "n1" :rel 'implements :to "abc"
                                      :note note-abc))))))

  (it "selects on from-id and passes rel-type through to the DB seam"
    (let (captured)
      (cl-letf (((symbol-function 'org-graph-query--select)
                 (lambda (column id rel-type)
                   (setq captured (list column id rel-type))
                   nil))
                ((symbol-function 'vulpea-db-get-by-id) #'ignore))
        (org-graph-query/outgoing "n1" 'implements)
        (expect captured :to-equal '(from-id "n1" implements)))))

  (it "leaves :note nil when the destination id is not in the index"
    (cl-letf (((symbol-function 'org-graph-query--select)
               (lambda (&rest _) '(("n1" relates-to "ghost"))))
              ((symbol-function 'vulpea-db-get-by-id) #'ignore))
      (expect (org-graph-query/outgoing "n1")
              :to-equal (list (list :from "n1" :rel 'relates-to :to "ghost"
                                    :note nil)))))

  (it "returns nil when the note has no outgoing edges"
    (cl-letf (((symbol-function 'org-graph-query--select) (lambda (&rest _) nil))
              ((symbol-function 'vulpea-db-get-by-id) #'ignore))
      (expect (org-graph-query/outgoing "lonely") :to-equal nil))))

;;; org-graph-query/incoming -----------------------------------------------

(describe "org-graph-query/incoming"

  (it "returns an edge plist per incoming row, resolving the SOURCE note"
    (let ((note-src (org-graph-test/note-fixture :id "src" :title "Source")))
      (cl-letf (((symbol-function 'org-graph-query--select)
                 (lambda (&rest _) '(("src" supersedes "n1"))))
                ((symbol-function 'vulpea-db-get-by-id)
                 (org-graph-test/note-map (list (cons "src" note-src)))))
        ;; far end relative to the queried to-id is the from-id (the author)
        (expect (org-graph-query/incoming "n1")
                :to-equal (list (list :from "src" :rel 'supersedes :to "n1"
                                      :note note-src))))))

  (it "selects on to-id and passes rel-type through to the DB seam"
    (let (captured)
      (cl-letf (((symbol-function 'org-graph-query--select)
                 (lambda (column id rel-type)
                   (setq captured (list column id rel-type))
                   nil))
                ((symbol-function 'vulpea-db-get-by-id) #'ignore))
        (org-graph-query/incoming "n1" 'contradicts)
        (expect captured :to-equal '(to-id "n1" contradicts))))))

;;; org-graph-query/connected ----------------------------------------------

(describe "org-graph-query/connected"

  (it "returns the union of outgoing then incoming edges"
    (let ((note-to  (org-graph-test/note-fixture :id "to"   :title "Dest"))
          (note-from (org-graph-test/note-fixture :id "from" :title "Author")))
      (cl-letf (((symbol-function 'org-graph-query--select)
                 ;; dispatch on the queried column to mimic two real selects
                 (lambda (column _id &optional _rel)
                   (pcase column
                     ('from-id '(("n1" implements "to")))
                     ('to-id   '(("from" relates-to "n1"))))))
                ((symbol-function 'vulpea-db-get-by-id)
                 (org-graph-test/note-map
                  (list (cons "to" note-to) (cons "from" note-from)))))
        (expect (org-graph-query/connected "n1")
                :to-equal
                (list (list :from "n1" :rel 'implements :to "to" :note note-to)
                      (list :from "from" :rel 'relates-to :to "n1" :note note-from))))))

  (it "returns nil when the note is connected to nothing"
    (cl-letf (((symbol-function 'org-graph-query--select) (lambda (&rest _) nil))
              ((symbol-function 'vulpea-db-get-by-id) #'ignore))
      (expect (org-graph-query/connected "island") :to-equal nil))))

;;; typed-edges-spec.el ends here
