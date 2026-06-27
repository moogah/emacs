;;; parse-typed-edges-spec.el --- Pure typed-edge parser tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for `org-graph-extractor/parse-typed-edges', the pure
;; function over an org-element AST that turns PROPERTIES-drawer typed
;; relations into (FROM-ID REL-TYPE TO-ID) tuples.
;;
;; The parser is exercised with synthetic trees built by
;; `org-graph-test/build-tree' — no file I/O, no vulpea DB.  Covers:
;; single-valued, multi-valued, multi-property, malformed, empty, and
;; non-relation-key cases (parse-typed-edges step 2).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-extractor (expand-file-name "extractor.el" module-dir)))

;; `org-graph-relation-types' is owned by the loader (a defcustom), which is
;; not loaded in this standalone parser process.  Declare it special so the
;; customization test below can dynamically `let'-bind it.
(defvar org-graph-relation-types)

(describe "org-graph-extractor/parse-typed-edges"

  (it "creates exactly one edge row for a single typed property"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[id:abc]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" implements "abc")))))

  (it "creates one row per link for a multi-valued property"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((RELATES_TO . "[[id:abc]] [[id:def]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" relates-to "abc")
                          ("n1" relates-to "def")))))

  (it "handles multiple distinct relation properties on one note"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[id:a]]")
                                         (CONTRADICTS . "[[id:b]]")
                                         (SUPERSEDES . "[[id:c]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" implements "a")
                          ("n1" contradicts "b")
                          ("n1" supersedes "c")))))

  (it "treats a repeated relation key as multiple rows"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[id:a]]")
                                         (IMPLEMENTS . "[[id:b]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" implements "a")
                          ("n1" implements "b")))))

  (it "ignores property keys that are not configured relation types"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((CATEGORY . "work")
                                         (IMPLEMENTS . "[[id:a]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" implements "a")))))

  (it "skips an empty relation value without signalling"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . ""))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal nil)))

  (it "skips a malformed (non-id-link) relation value without signalling"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[file:foo.org]] not-a-link"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal nil)))

  (it "extracts the id from a link that carries a description"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[id:abc][My Concept]]"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" implements "abc")))))

  (it "returns nil for a note with no relation properties"
    (let ((tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((CATEGORY . "work"))))))
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal nil)))

  (it "respects a customized org-graph-relation-types set"
    (let ((org-graph-relation-types '(relates-to))
          (tree (org-graph-test/build-tree
                 '(:id "n1" :properties ((IMPLEMENTS . "[[id:a]]")
                                         (RELATES_TO . "[[id:b]]"))))))
      ;; IMPLEMENTS is no longer configured, so only the relates-to edge appears.
      (expect (org-graph-extractor/parse-typed-edges tree "n1")
              :to-equal '(("n1" relates-to "b"))))))

;;; parse-typed-edges-spec.el ends here
