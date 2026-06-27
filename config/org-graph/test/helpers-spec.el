;;; helpers-spec.el --- Shared test helpers for org-graph specs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Shared Buttercup test infrastructure for the org-graph spike.  These
;; helpers construct synthetic data and install function-scoped mocks
;; (via `cl-letf', never global state) so the parser, extractor, query,
;; schema, finder, and coordinator specs stay deterministic and never
;; spin up a real vulpea SQLite database.
;;
;; The helpers only build data and install scoped stubs; they never
;; assert.  The self-test `describe' blocks at the bottom prove each
;; helper produces the shapes downstream specs expect.
;;
;; Provides:
;; 1. `org-graph-test/build-tree'         - synthetic org-element AST
;; 2. `org-graph-test/with-stubbed-vulpea'- scoped vulpea API stubs
;; 3. `org-graph-test/note-fixture'       - a `vulpea-note'-shaped value
;; 4. `org-graph-test/link-plist'         - a vulpea link plist

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'org)
(require 'org-element)

;; vulpea is not in `jf/enabled-modules' yet (that is the `wire-into-init'
;; task), so it is not on `load-path' in the test process.  Add its
;; straight build dir before requiring it, so `make-vulpea-note' and the
;; vulpea API symbols the stubs shadow are defined.
(let* ((root (or (and (boundp 'jf/emacs-dir) jf/emacs-dir)
                 (locate-dominating-file
                  (or load-file-name buffer-file-name default-directory)
                  "early-init.el")))
       (vbuild (and root (expand-file-name "runtime/straight/build/vulpea" root))))
  (when (and vbuild (file-directory-p vbuild))
    (add-to-list 'load-path vbuild)))
(require 'vulpea)

;;; 1. Synthetic org-element AST

(defun org-graph-test/--drawer (id properties)
  "Return PROPERTIES-drawer text for ID and PROPERTIES.
ID is an optional string `:ID:' value; PROPERTIES is an alist of
\(KEY . VALUE) emitted IN ORDER, KEY a symbol or string.  KEY MAY repeat
to model multi-occurrence typed-edge properties (the drawer keeps each as
a distinct entry, so the whole-AST parser sees them all)."
  (concat
   ":PROPERTIES:\n"
   (when id (format ":ID:       %s\n" id))
   (mapconcat
    (lambda (cell)
      (let ((key (car cell)))
        (format ":%s: %s\n"
                (upcase (if (symbolp key) (symbol-name key) key))
                (cdr cell))))
    properties
    "")
   ":END:\n"))

(defun org-graph-test/build-tree (spec)
  "Build an `org-element' parse tree from SPEC, a plist describing a note.

Recognised SPEC keys:
  :id         - string, becomes the file-level note's :ID: property
  :title      - string, becomes the #+title: keyword
  :properties - alist of (KEY . VALUE) where KEY is a symbol or string
                and VALUE is a string.  Emitted as the FILE-LEVEL
                PROPERTIES-drawer entries IN ORDER.  KEY MAY repeat (e.g.
                two `IMPLEMENTS' entries) to model multi-occurrence
                typed-edge properties.
  :filetags   - list of strings, emitted as a #+filetags: keyword
  :headings   - list of heading plists, each recognising:
                  :id         - string, the heading's :ID: property
                  :title      - string, the heading text (default \"Heading\")
                  :level      - integer star count (default 1)
                  :properties - alist as above, the HEADING's own drawer.
                Models the multi-note file vulpea indexes (file node +
                every ID'd heading), so specs can assert per-note
                attribution.

The result is the file-level org-element AST returned by
`org-element-parse-buffer'.  No file I/O and no persistent org state:
the buffer is transient and `org-mode-hook' is suppressed."
  (let* ((id (plist-get spec :id))
         (title (plist-get spec :title))
         (properties (plist-get spec :properties))
         (filetags (plist-get spec :filetags))
         (headings (plist-get spec :headings))
         (text
          (concat
           (org-graph-test/--drawer id properties)
           (when title (format "#+title: %s\n" title))
           (when filetags
             (format "#+filetags: :%s:\n" (mapconcat #'identity filetags ":")))
           (mapconcat
            (lambda (h)
              (concat
               (make-string (or (plist-get h :level) 1) ?*)
               " " (or (plist-get h :title) "Heading") "\n"
               (org-graph-test/--drawer (plist-get h :id)
                                        (plist-get h :properties))))
            headings
            ""))))
    (with-temp-buffer
      (insert text)
      (let ((org-mode-hook nil)
            (org-element-use-cache nil))
        (delay-mode-hooks (org-mode)))
      (org-element-parse-buffer))))

;;; 2. Scoped vulpea API stubs

(defmacro org-graph-test/with-stubbed-vulpea (bindings &rest body)
  "Evaluate BODY with vulpea API functions stubbed via `cl-letf'.

BINDINGS is a plist mapping a stub keyword to the value the stubbed
function should return (the stub ignores its arguments and returns the
fixture).  Recognised keywords and the function each shadows:

  :query        -> `vulpea-db-query'
  :links        -> `vulpea-db-query-links'
  :links-from   -> `vulpea-db-query-links-from'
  :links-to     -> `vulpea-db-query-links-to'
  :register     -> `vulpea-db-register-extractor'
  :schema       -> `vulpea-schema-define'
  :validate     -> `vulpea-schema-validate'

Unlisted functions are left untouched.  All stubs are function-scoped
and unwound on exit; no global state is mutated."
  (declare (indent 1) (debug (form body)))
  (let* ((map '((:query      . vulpea-db-query)
                (:links      . vulpea-db-query-links)
                (:links-from . vulpea-db-query-links-from)
                (:links-to   . vulpea-db-query-links-to)
                (:register   . vulpea-db-register-extractor)
                (:schema     . vulpea-schema-define)
                (:validate   . vulpea-schema-validate)))
         (letf-forms
          (cl-loop for (kw . fn) in map
                   when (plist-member bindings kw)
                   collect `((symbol-function ',fn)
                             (let ((ret ,(plist-get bindings kw)))
                               (lambda (&rest _) ret))))))
    `(cl-letf (,@letf-forms)
       ,@body)))

;;; 3. vulpea-note fixture

(cl-defun org-graph-test/note-fixture
    (&key id title (tags nil) (properties nil) (path "/tmp/note.org")
          (links nil) (level 0) (meta nil))
  "Construct a `vulpea-note' value for query and finder specs.
Only the slots the specs care about are populated; the rest default to
their struct defaults.  Returns a real `vulpea-note' struct so callers
exercise the genuine accessors (`vulpea-note-tags', etc.)."
  (make-vulpea-note
   :id id
   :title title
   :path path
   :level level
   :tags tags
   :properties properties
   :links links
   :meta meta))

;;; 4. vulpea link plist

(cl-defun org-graph-test/link-plist
    (&key source dest (type "id") (pos 1) (description nil))
  "Build a vulpea link plist of the shape `vulpea-db-query-links*' return.
Keys: :source :dest :type :pos :description.  TYPE defaults to the
link-kind \"id\" (NOT a semantic relation type — semantic relations
live only in the org-graph typed_edges index)."
  (list :source source :dest dest :type type :pos pos :description description))

;;; Self-tests: prove each helper produces the expected shape.

(describe "org-graph-test/build-tree"
  (it "produces a parseable org-element tree with the note properties"
    (let* ((tree (org-graph-test/build-tree
                  '(:id "n1"
                    :title "Note One"
                    :properties ((IMPLEMENTS . "[[id:abc]]")
                                 (RELATES_TO . "[[id:def]] [[id:ghi]]"))
                    :filetags ("topic" "agent-draft"))))
           (props (org-element-map tree 'node-property
                    (lambda (np)
                      (cons (org-element-property :key np)
                            (org-element-property :value np))))))
      (expect (eq (org-element-type tree) 'org-data) :to-be t)
      (expect (cdr (assoc "ID" props)) :to-equal "n1")
      (expect (cdr (assoc "IMPLEMENTS" props)) :to-equal "[[id:abc]]")
      (expect (cdr (assoc "RELATES_TO" props)) :to-equal "[[id:def]] [[id:ghi]]")))

  (it "preserves a repeated property key as distinct AST entries"
    (let* ((tree (org-graph-test/build-tree
                  '(:id "n2"
                    :properties ((IMPLEMENTS . "[[id:a]]")
                                 (IMPLEMENTS . "[[id:b]]")))))
           (values (org-element-map tree 'node-property
                     (lambda (np)
                       (when (equal (org-element-property :key np) "IMPLEMENTS")
                         (org-element-property :value np))))))
      (expect values :to-equal '("[[id:a]]" "[[id:b]]"))))

  (it "builds an ID'd heading node with its own PROPERTIES drawer"
    (let* ((tree (org-graph-test/build-tree
                  '(:id "file1"
                    :properties ((IMPLEMENTS . "[[id:f]]"))
                    :headings ((:id "head1" :title "A Heading"
                                :properties ((RELATES_TO . "[[id:h]]")))))))
           (headlines (org-element-map tree 'headline #'identity))
           (heading (car headlines))
           ;; Collect each property-drawer's owning :ID: value.
           (drawer-ids (org-element-map tree 'property-drawer
                         (lambda (d)
                           (org-element-map d 'node-property
                             (lambda (np)
                               (when (equal (org-element-property :key np) "ID")
                                 (org-element-property :value np)))
                             nil t)))))
      (expect (length headlines) :to-equal 1)
      (expect (org-element-property :raw-value heading) :to-equal "A Heading")
      ;; Both the file-level and the heading drawer carry their own id.
      (expect drawer-ids :to-equal '("file1" "head1")))))

(describe "org-graph-test/with-stubbed-vulpea"
  (it "stubs only the listed vulpea functions, scoped to the body"
    (org-graph-test/with-stubbed-vulpea
        (:query '(:row 1) :links-to '(:link 2))
      (expect (vulpea-db-query 'ignored) :to-equal '(:row 1))
      (expect (vulpea-db-query-links-to "id") :to-equal '(:link 2))))

  (it "unwinds the stubs after the body"
    (let ((before (symbol-function 'vulpea-db-query)))
      (org-graph-test/with-stubbed-vulpea (:query nil)
        (ignore))
      (expect (eq (symbol-function 'vulpea-db-query) before) :to-be t))))

(describe "org-graph-test/note-fixture"
  (it "builds a real vulpea-note with working accessors"
    (let ((note (org-graph-test/note-fixture
                 :id "abc" :title "T" :tags '("topic")
                 :properties '(("ID" . "abc")))))
      (expect (vulpea-note-p note) :to-be t)
      (expect (vulpea-note-id note) :to-equal "abc")
      (expect (vulpea-note-tags note) :to-equal '("topic")))))

(describe "org-graph-test/link-plist"
  (it "builds a link plist with the vulpea-db-query-links shape"
    (let ((link (org-graph-test/link-plist :source "a" :dest "b")))
      (expect (plist-get link :source) :to-equal "a")
      (expect (plist-get link :dest) :to-equal "b")
      (expect (plist-get link :type) :to-equal "id"))))

;;; Provide

(provide 'org-graph-test-helpers)

;;; helpers-spec.el ends here
