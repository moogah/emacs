;;; extractor-spec.el --- Vulpea typed-edge extractor wrapper tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Tests for the vulpea extractor wrapper around the pure edge-drawer
;; scanner (`vulpea-extractor-plugin' task; drawer surface per the
;; `edges-drawer' task).  Three concerns:
;;
;;   1. Scope gate -- typed edges are emitted ONLY for notes under
;;      `org-graph-roam-root' (register/invariant/typed-edge-extraction-scope).
;;      Roam-path note -> tuples inserted; non-roam-path note -> nothing.
;;   2. Note-granular attribution -- the scanner attributes tuples via
;;      the shared enclosing-node walk over the WHOLE file AST; the
;;      wrapper keeps only the tuples attributed to the note vulpea is
;;      processing, so a multi-note file never duplicates rows
;;      (register/boundary/parser-extractor-db stage 2).
;;   3. Storage shape -- the `rel-type' column receives the relation
;;      SYMBOL (register/shape/typed-edge-tuple), `from-id'/`to-id'
;;      remain strings.
;;
;; Fixtures build their edge drawers via
;; `org-graph-test/edge-drawer-text', which reads the drawer name from
;; the `org-graph-edge-drawer' defcustom (no hardcoded literal).
;;
;; vulpea is mocked at the API boundary via `cl-letf' (the parse context,
;; the DB handle, and the `emacsql' insert are stubbed); no real SQLite
;; database is ever opened.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar org-graph-test/module-dir
  ;; Captured at load time (load-file-name is nil inside `it' bodies).
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the org-graph module directory.")

(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-extractor
           (expand-file-name "extractor.el" org-graph-test/module-dir)))

;; `org-graph-roam-root' is owned by the loader defcustom; the extractor
;; marks it special with a bare `defvar'.  Declare it here too so the
;; specs can dynamically `let'-bind it.  (`org-graph-edge-drawer' is
;; declared special by helpers-spec.)
(defvar org-graph-roam-root)

;;; Test scaffolding -------------------------------------------------------

(defvar org-graph-test/captured-inserts nil
  "Accumulator for rows passed to the stubbed `emacsql' insert.")

(defmacro org-graph-test/with-captured-db (&rest body)
  "Run BODY with `vulpea-db' and `emacsql' stubbed to capture inserts.
Resets `org-graph-test/captured-inserts' to nil, then appends each
`emacsql' call's final argument (the rows list) to it.  The accumulator
is a global (not let-bound) so it survives for assertions after BODY; each
call resets it, so tests do not contaminate each other.  No real DB is
opened."
  (declare (indent 0) (debug (body)))
  `(progn
     (setq org-graph-test/captured-inserts nil)
     (cl-letf (((symbol-function 'vulpea-db)
                (lambda (&rest _) 'stub-db))
               ((symbol-function 'emacsql)
                (lambda (_db _sql &rest args)
                  (setq org-graph-test/captured-inserts
                        (append org-graph-test/captured-inserts
                                (car (last args))))
                  nil)))
       ,@body)))

(defun org-graph-test/parse-ctx (path tree)
  "Return a real `vulpea-parse-ctx' carrying PATH and AST TREE."
  (make-vulpea-parse-ctx :path path :ast tree))

;;; Scope gate -------------------------------------------------------------

(describe "org-graph-extractor/extract scope gate"

  (it "inserts typed-edge tuples for a note under the roam root"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "n1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]")
                              ("relates-to" . "[[id:def]] [[id:ghi]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n1")))
      (expect org-graph-test/captured-inserts
              :to-equal (list (vector "n1" 'implements "abc")
                              (vector "n1" 'relates-to "def")
                              (vector "n1" 'relates-to "ghi")))))

  (it "inserts nothing for a note OUTSIDE the roam root"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "w1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/work/proj/home.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "w1")))
      (expect org-graph-test/captured-inserts :to-equal nil)))

  (it "treats a roam SUBDIRECTORY note as in scope"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "n2"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("supersedes" . "[[id:old]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/sub/dir/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n2")))
      (expect org-graph-test/captured-inserts
              :to-equal (list (vector "n2" 'supersedes "old")))))

  (it "does not insert when an in-scope note has no edge drawer"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  '(:id "n3" :properties ((CATEGORY . "work")))))
           (ctx (org-graph-test/parse-ctx "/roam/plain.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n3")))
      (expect org-graph-test/captured-inserts :to-equal nil)))

  (it "fails closed (no insert) when org-graph-roam-root is unset"
    (let* ((org-graph-roam-root nil)
           (tree (org-graph-test/build-tree
                  `(:id "n4"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n4")))
      (expect org-graph-test/captured-inserts :to-equal nil)))

  (it "fails closed (no insert) when org-graph-roam-root is the empty string"
    ;; Regression: an empty root must NOT expand to default-directory and
    ;; silently scope extraction to cwd (reviewer finding -2).
    (let* ((org-graph-roam-root "")
           (tree (org-graph-test/build-tree
                  `(:id "n5"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n5")))
      (expect org-graph-test/captured-inserts :to-equal nil)))

  (it "inserts nothing for an in-scope note with no edges attributed to it"
    ;; vulpea runs the extractor once per ID-bearing note.  A note to
    ;; which the enclosing-node walk attributed no tuples (e.g. an
    ;; indexed heading that authored no drawer of its own) contributes
    ;; nothing -- the file-level edges are NOT mis-attributed to it.
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "n6"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "h1" :level 1)))
      (expect org-graph-test/captured-inserts :to-equal nil)))

  (it "returns NOTE-DATA unchanged per the extractor contract"
    (let* ((org-graph-roam-root "/roam/")
           (note-data '(:id "n1" :title "T"))
           (tree (org-graph-test/build-tree
                  `(:id "n1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (expect (org-graph-extractor/extract ctx note-data)
                :to-equal '(:id "n1" :title "T"))))))

;;; Note-granular attribution (multi-note files) ---------------------------
;;
;; Regression for reviewer finding -1 / register/boundary/parser-extractor-db:
;; vulpea runs :extract-fn ONCE PER ID-bearing note (file node + every ID'd
;; heading).  The scanner attributes every tuple internally via the shared
;; enclosing-node walk; the wrapper keeps only the tuples whose from-id is
;; the note being processed, so each note is credited with only its own
;; edges -- no N-fold duplication, correct from-id.

(describe "org-graph-extractor/extract note-granular attribution"

  (it "credits a file note with only its file-level drawer edges"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "file1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:fa]]")))
                    :headings ((:id "head1"
                                :body ,(org-graph-test/edge-drawer-text
                                        '(("relates-to" . "[[id:hb]]"))))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "file1")))
      ;; ONLY the file's own edge; the heading's relates-to is NOT credited.
      (expect org-graph-test/captured-inserts
              :to-equal (list (vector "file1" 'implements "fa")))))

  (it "credits an ID'd heading note with only its own drawer edges"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "file1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:fa]]")))
                    :headings ((:id "head1"
                                :body ,(org-graph-test/edge-drawer-text
                                        '(("relates-to" . "[[id:hb]]"))))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "head1" :level 1)))
      ;; from-id is the HEADING id, and ONLY the heading's edge appears.
      (expect org-graph-test/captured-inserts
              :to-equal (list (vector "head1" 'relates-to "hb")))))

  (it "does not duplicate edges across the notes of one file"
    ;; Run the extractor for BOTH notes (as vulpea would) and assemble the
    ;; full set of rows: each edge appears exactly once, attributed to its
    ;; authoring note.
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "file1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:fa]]")))
                    :headings ((:id "head1"
                                :body ,(org-graph-test/edge-drawer-text
                                        '(("relates-to" . "[[id:hb]]"))))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree))
           all-rows)
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "file1")))
      (setq all-rows (append all-rows org-graph-test/captured-inserts))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "head1" :level 1)))
      (setq all-rows (append all-rows org-graph-test/captured-inserts))
      (expect all-rows
              :to-equal (list (vector "file1" 'implements "fa")
                              (vector "head1" 'relates-to "hb")))))

  (it "preserves a repeated relation type across a note's drawer items"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "file1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:a]]")
                              ("implements" . "[[id:b]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "file1")))
      (expect org-graph-test/captured-inserts
              :to-equal (list (vector "file1" 'implements "a")
                              (vector "file1" 'implements "b"))))))

;;; Storage shape (string-vs-symbol) ---------------------------------------

(describe "org-graph-extractor/extract storage shape"

  (it "stores rel-type as a SYMBOL and the ids as strings"
    (let* ((org-graph-roam-root "/roam/")
           (tree (org-graph-test/build-tree
                  `(:id "n1"
                    :body ,(org-graph-test/edge-drawer-text
                            '(("implements" . "[[id:abc]]"))))))
           (ctx (org-graph-test/parse-ctx "/roam/note.org" tree)))
      (org-graph-test/with-captured-db
        (org-graph-extractor/extract ctx '(:id "n1")))
      (let ((row (car org-graph-test/captured-inserts)))
        (expect (stringp (aref row 0)) :to-be t)   ; from-id
        (expect (symbolp (aref row 1)) :to-be t)   ; rel-type
        (expect (aref row 1) :to-be 'implements)
        (expect (stringp (aref row 2)) :to-be t))))) ; to-id

;;; Registration -----------------------------------------------------------

(describe "org-graph-extractor-register"

  (it "registers an extractor named org-graph-typed-edges at priority 50"
    (let (captured)
      (cl-letf (((symbol-function 'vulpea-db-register-extractor)
                 (lambda (ex) (setq captured ex) ex)))
        (org-graph-extractor-register))
      (expect (vulpea-extractor-p captured) :to-be t)
      (expect (vulpea-extractor-name captured) :to-be 'org-graph-typed-edges)
      (expect (vulpea-extractor-priority captured) :to-equal 50)
      (expect (vulpea-extractor-extract-fn captured)
              :to-be #'org-graph-extractor/extract)))

  (it "declares the typed_edges schema with a cascading FK onto notes(id)"
    (let (captured)
      (cl-letf (((symbol-function 'vulpea-db-register-extractor)
                 (lambda (ex) (setq captured ex) ex)))
        (org-graph-extractor-register))
      (let* ((schema (vulpea-extractor-schema captured))
             (table (car schema)))
        (expect (car table) :to-be 'typed_edges)
        ;; FK clause: (:foreign-key [from-id] :references notes [id] :on-delete :cascade)
        (let ((fk (assq :foreign-key (cdr table))))
          (expect fk :not :to-be nil)
          (expect (memq :cascade fk) :not :to-be nil)
          (expect (memq 'notes fk) :not :to-be nil)))))

  (it "carries version 2, marking the properties-to-drawer surface swap"
    ;; Parser-epoch discipline: the drawer surface changes the scanner's
    ;; output for the same input, so the extractor version was bumped to
    ;; invalidate vulpea's per-file cache and force re-extraction.
    (let (captured)
      (cl-letf (((symbol-function 'vulpea-db-register-extractor)
                 (lambda (ex) (setq captured ex) ex)))
        (org-graph-extractor-register))
      (expect (vulpea-extractor-version captured) :to-equal 2))))

;;; extractor-spec.el ends here
