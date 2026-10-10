;;; parse-drawer-edges-spec.el --- Pure edge-drawer scanner tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for `org-graph-extractor/parse-drawer-edges', the pure
;; scanner over an org-element AST that turns edge-drawer
;; description-list items into (FROM-ID REL-TYPE TO-ID) tuples
;; (design LD-1..LD-5).
;;
;; The scanner is exercised with synthetic trees built by
;; `org-graph-test/build-tree' -- no file I/O, no vulpea DB.  Fixtures
;; build their drawers via `org-graph-test/edge-drawer-text', which
;; reads the drawer name from the `org-graph-edge-drawer' defcustom
;; rather than hardcoding a literal, so the cases follow the configured
;; name (register/invariant/edge-drawer-discriminator).  Covers:
;;
;;   - open vocabulary: registered AND novel relation tags each yield
;;     rows (register/vocabulary/relation-types)
;;   - tag normalization: trim / downcase / spaces+underscores->hyphens
;;   - the drawer-name discriminator: properties holding id: links,
;;     bare body links, and identically-shaped items in a
;;     differently-named drawer are never edges; a rebound
;;     `org-graph-edge-drawer' moves the discriminator
;;   - enclosing-node attribution: file-top drawer -> file node id;
;;     ID-bearing subheading -> its own id; ID-less heading -> walks up
;;     to the nearest ID-bearing ancestor; no ID-bearing ancestor at
;;     all -> dropped (register/invariant/enclosing-node-attribution)
;;   - malformed/empty input never signals and adds no spurious rows.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-extractor (expand-file-name "extractor.el" module-dir)))

;; `org-graph-edge-drawer' is owned by the loader's defcustom (loaded in
;; the test process via init.el); helpers-spec declares it special so the
;; rebinding cases below can `let'-bind it.

(defun org-graph-drawer-test/parse (spec)
  "Build the tree for SPEC and run the pure drawer scanner over it."
  (org-graph-extractor/parse-drawer-edges (org-graph-test/build-tree spec)))

(describe "org-graph-extractor/parse-drawer-edges"

  (describe "edge extraction and open vocabulary"

    (it "creates one row for a registered relation item"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc]]")))))
              :to-equal '(("n1" implements "abc"))))

    (it "creates one row for a novel, unregistered relation (open vocabulary)"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("falsifies" . "[[id:abc]]")))))
              :to-equal '(("n1" falsifies "abc"))))

    (it "creates one row per id: link in a multi-link item"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("relates-to" . "[[id:a]] [[id:b]]")))))
              :to-equal '(("n1" relates-to "a")
                          ("n1" relates-to "b"))))

    (it "keeps a repeated relation type across items as distinct rows"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:a]]")
                           ("implements" . "[[id:b]]")))))
              :to-equal '(("n1" implements "a")
                          ("n1" implements "b"))))

    (it "extracts the id from a link that carries a description"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc][My Concept]]")))))
              :to-equal '(("n1" implements "abc")))))

  (describe "relation-tag normalization"

    (it "normalizes a multi-word tag: spaces map to hyphens"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("follows up" . "[[id:abc]]")))))
              :to-equal '(("n1" follows-up "abc"))))

    (it "normalizes case and underscores: FOLLOWS_UP -> follows-up"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("FOLLOWS_UP" . "[[id:abc]]")))))
              :to-equal '(("n1" follows-up "abc")))))

  (describe "drawer-name discriminator"

    (it "never treats a PROPERTIES entry holding an id: link as an edge"
      (expect (org-graph-drawer-test/parse
               '(:id "n1" :properties ((SOURCE . "[[id:abc]]"))))
              :to-equal nil))

    (it "never treats a bare body id: link as an edge"
      (expect (org-graph-drawer-test/parse
               '(:id "n1" :body "See [[id:x]] for details.\n"))
              :to-equal nil))

    (it "ignores identical items in a differently-named drawer"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc]]"))
                         "NOTEDGES")))
              :to-equal nil))

    (it "matches the configured drawer name case-insensitively"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc]]"))
                         (downcase org-graph-edge-drawer))))
              :to-equal '(("n1" implements "abc"))))

    (it "follows a rebound org-graph-edge-drawer to the new name"
      (let* ((default-name org-graph-edge-drawer)
             (org-graph-edge-drawer "RELATIONS"))
        ;; items in the NEW configured drawer are edges ...
        (expect (org-graph-drawer-test/parse
                 `(:id "n1"
                   :body ,(org-graph-test/edge-drawer-text
                           '(("implements" . "[[id:abc]]")))))
                :to-equal '(("n1" implements "abc")))
        ;; ... and items in the formerly-configured drawer are not.
        (expect (org-graph-drawer-test/parse
                 `(:id "n1"
                   :body ,(org-graph-test/edge-drawer-text
                           '(("implements" . "[[id:abc]]"))
                           default-name)))
                :to-equal nil))))

  (describe "enclosing-node attribution"

    (it "attributes a file-top drawer to the file-level node"
      (expect (org-graph-drawer-test/parse
               `(:id "file1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc]]")))))
              :to-equal '(("file1" implements "abc"))))

    (it "attributes a drawer under an ID-bearing subheading to that heading"
      (expect (org-graph-drawer-test/parse
               `(:id "file1"
                 :headings ((:id "head1"
                             :body ,(org-graph-test/edge-drawer-text
                                     '(("implements" . "[[id:abc]]")))))))
              :to-equal '(("head1" implements "abc"))))

    (it "walks up from an ID-less heading to the nearest ID-bearing ancestor"
      (expect (org-graph-drawer-test/parse
               `(:id "file1"
                 :headings ((:id "parent1" :level 1)
                            (:level 2
                             :body ,(org-graph-test/edge-drawer-text
                                     '(("implements" . "[[id:abc]]")))))))
              :to-equal '(("parent1" implements "abc"))))

    (it "drops (never mis-attributes) a drawer with no ID-bearing ancestor"
      (expect (org-graph-drawer-test/parse
               `(:body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:abc]]")))))
              :to-equal nil))

    (it "attributes each drawer of a multi-note file to its own note"
      (expect (org-graph-drawer-test/parse
               `(:id "file1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[id:fa]]")))
                 :headings ((:id "head1"
                             :body ,(org-graph-test/edge-drawer-text
                                     '(("relates-to" . "[[id:hb]]")))))))
              :to-equal '(("file1" implements "fa")
                          ("head1" relates-to "hb")))))

  (describe "malformed and empty input (never signals)"

    (it "returns nil for an empty edge drawer"
      (expect (org-graph-drawer-test/parse
               `(:id "n1" :body ,(org-graph-test/edge-drawer-text nil)))
              :to-equal nil))

    (it "skips an item with an empty tag"
      ;; `- :: [[id:x]]' parses as an untagged item; no relation, no row.
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '("-  :: [[id:empty-tag]]"))))
              :to-equal nil))

    (it "skips non-item drawer content and untagged plain items"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '("not an item line"
                           "- a plain item without a tag"
                           ("implements" . "[[id:abc]]")))))
              :to-equal '(("n1" implements "abc"))))

    (it "skips non-id links in a tagged item"
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(org-graph-test/edge-drawer-text
                         '(("implements" . "[[file:foo.org]] not-a-link")))))
              :to-equal nil))

    (it "contributes nothing from an unclosed (unparseable) drawer"
      ;; without :END: org never parses a drawer element at all.
      (expect (org-graph-drawer-test/parse
               `(:id "n1"
                 :body ,(format ":%s:\n- implements :: [[id:abc]]\n"
                                org-graph-edge-drawer)))
              :to-equal nil))))

;;; parse-drawer-edges-spec.el ends here
