;;; parse-rel-links-spec.el --- Pure inline rel-link scanner tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for `org-graph-extractor/parse-rel-links', the pure
;; scanner over an org-element AST that turns inline rel-links
;; (path `<type>:<target-id>') into (FROM-ID REL-TYPE TO-ID) tuples
;; (design OV-3/OV-4/OV-5, LD-4/LD-5).
;;
;; The scanner is exercised with synthetic trees built by
;; `org-graph-test/build-tree' -- no file I/O, no vulpea DB.  org only
;; parses `[[rel:...]]' as a typed link when the type is registered in
;; `org-link-parameters' at parse time, so fixtures build their trees
;; inside `org-graph-rel-test/with-link-type', which registers the type
;; scoped to the body and restores the global link regexps on exit.
;; Covers:
;;
;;   - path split on the FIRST colon: type segment normalized+interned,
;;     target id kept VERBATIM even when it contains colons
;;     (register/boundary/rel-link-path-syntax)
;;   - open vocabulary + normalization shared with the drawer surface
;;     (register/vocabulary/relation-types)
;;   - enclosing-node attribution: file-top link -> file node id;
;;     ID-bearing subheading -> its own id; ID-less heading -> walks up
;;     to the nearest ID-bearing ancestor; no ID-bearing ancestor at
;;     all -> dropped (register/invariant/enclosing-node-attribution)
;;   - the link-type discriminator follows `org-graph-edge-link-type'
;;     and fails closed when it is unset; ordinary id: links and
;;     unregistered link types are never edges
;;   - malformed paths never signal and add no rows.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'ol)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-extractor (expand-file-name "extractor.el" module-dir)))

;; `org-graph-edge-link-type' is owned by the rel-link runtime's
;; defcustom (rel-link-type task); declared special here so the cases
;; below can `let'-bind it whether or not that module is loaded.
(defvar org-graph-edge-link-type)

(defmacro org-graph-rel-test/with-link-type (type &rest body)
  "Evaluate BODY with TYPE registered as an org link type, scoped.
org-element only assigns `:type' TYPE to a link when TYPE is present in
`org-link-parameters' at parse time.  This rebinds the parameter list,
recomputes the derived link regexps for BODY, and recomputes them again
from the untouched global value on exit -- no persistent state."
  (declare (indent 1) (debug (form body)))
  `(unwind-protect
       (let ((org-link-parameters (cons (list ,type) org-link-parameters)))
         (org-link-make-regexps)
         ,@body)
     (org-link-make-regexps)))

(defun org-graph-rel-test/parse (spec &optional link-type)
  "Build the tree for SPEC and run the pure rel-link scanner over it.
LINK-TYPE (default \"rel\") is registered for the parse and bound as
`org-graph-edge-link-type' for the scan."
  (let ((type (or link-type "rel")))
    (org-graph-rel-test/with-link-type type
      (let ((org-graph-edge-link-type type))
        (org-graph-extractor/parse-rel-links
         (org-graph-test/build-tree spec))))))

(describe "org-graph-extractor/parse-rel-links"

  (describe "edge extraction and path split"

    (it "creates one row for a rel link in file-top prose"
      (expect (org-graph-rel-test/parse
               '(:id "n1"
                 :body "This claim [[rel:implements:abc][the target]] holds.\n"))
              :to-equal '(("n1" implements "abc"))))

    (it "creates a row for a novel, unregistered relation (open vocabulary)"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "See [[rel:falsifies:abc]].\n"))
              :to-equal '(("n1" falsifies "abc"))))

    (it "splits on the FIRST colon: the target id keeps its own colons verbatim"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel:implements:urn:x:y]]\n"))
              :to-equal '(("n1" implements "urn:x:y"))))

    (it "creates one row per rel link, in document order"
      (expect (org-graph-rel-test/parse
               '(:id "n1"
                 :body "A [[rel:implements:a]] then [[rel:contradicts:b]].\n"))
              :to-equal '(("n1" implements "a")
                          ("n1" contradicts "b"))))

    (it "never treats a bare id: link in the same prose as an edge"
      (expect (org-graph-rel-test/parse
               '(:id "n1"
                 :body "See [[id:plain]] but [[rel:implements:abc]].\n"))
              :to-equal '(("n1" implements "abc")))))

  (describe "relation-type normalization (shared with the drawer surface)"

    (it "normalizes case and underscores: FOLLOWS_UP -> follows-up"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel:FOLLOWS_UP:abc]]\n"))
              :to-equal '(("n1" follows-up "abc"))))

    (it "normalizes spaces in the type segment to hyphens"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel:follows up:abc]]\n"))
              :to-equal '(("n1" follows-up "abc")))))

  (describe "enclosing-node attribution"

    (it "attributes a file-top link to the file-level node"
      (expect (org-graph-rel-test/parse
               '(:id "file1" :body "[[rel:implements:abc]]\n"))
              :to-equal '(("file1" implements "abc"))))

    (it "attributes a link under an ID-bearing subheading to that heading"
      (expect (org-graph-rel-test/parse
               '(:id "file1"
                 :headings ((:id "head1"
                             :body "[[rel:implements:abc]]\n"))))
              :to-equal '(("head1" implements "abc"))))

    (it "walks up from an ID-less heading to the nearest ID-bearing ancestor"
      (expect (org-graph-rel-test/parse
               '(:id "file1"
                 :headings ((:id "parent1" :level 1)
                            (:level 2
                             :body "[[rel:implements:abc]]\n"))))
              :to-equal '(("parent1" implements "abc"))))

    (it "drops (never mis-attributes) a link with no ID-bearing ancestor"
      (expect (org-graph-rel-test/parse
               '(:body "[[rel:implements:abc]]\n"))
              :to-equal nil))

    (it "attributes each link of a multi-note file to its own note"
      (expect (org-graph-rel-test/parse
               '(:id "file1"
                 :body "[[rel:implements:fa]]\n"
                 :headings ((:id "head1"
                             :body "[[rel:relates-to:hb]]\n"))))
              :to-equal '(("file1" implements "fa")
                          ("head1" relates-to "hb")))))

  (describe "link-type discriminator and configuration"

    (it "follows a rebound org-graph-edge-link-type to the new name"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[edge:implements:abc]]\n")
               "edge")
              :to-equal '(("n1" implements "abc"))))

    (it "only matches links of the configured type"
      (org-graph-rel-test/with-link-type "rel"
        (let ((org-graph-edge-link-type "edge")
              (tree (org-graph-test/build-tree
                     '(:id "n1" :body "[[rel:implements:abc]]\n"))))
          (expect (org-graph-extractor/parse-rel-links tree)
                  :to-equal nil))))

    (it "fails closed when org-graph-edge-link-type is not configured"
      (org-graph-rel-test/with-link-type "rel"
        (let ((org-graph-edge-link-type nil)
              (tree (org-graph-test/build-tree
                     '(:id "n1" :body "[[rel:implements:abc]]\n"))))
          (expect (org-graph-extractor/parse-rel-links tree)
                  :to-equal nil))))

    (it "yields nothing when the link type was not registered at parse time"
      ;; an unregistered [[xrel:...]] parses as a fuzzy link, not a typed
      ;; one; the scanner reads only the structural :type org produced.
      ;; ("xrel" so this stays valid once the rel-link runtime registers
      ;; "rel" globally in the test process.)
      (let ((org-graph-edge-link-type "xrel")
            (tree (org-graph-test/build-tree
                   '(:id "n1" :body "[[xrel:implements:abc]]\n"))))
        (expect (org-graph-extractor/parse-rel-links tree)
                :to-equal nil))))

  (describe "malformed paths (never signals)"

    (it "skips a path with no colon separator"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel:implements]]\n"))
              :to-equal nil))

    (it "skips a path with an empty type segment"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel::abc]]\n"))
              :to-equal nil))

    (it "skips a path with an empty target"
      (expect (org-graph-rel-test/parse
               '(:id "n1" :body "[[rel:implements:]]\n"))
              :to-equal nil))

    (it "keeps well-formed rows while skipping malformed neighbours"
      (expect (org-graph-rel-test/parse
               '(:id "n1"
                 :body "Bad [[rel:notarget]] and [[rel::x]] but [[rel:implements:abc]].\n"))
              :to-equal '(("n1" implements "abc"))))))

;;; parse-rel-links-spec.el ends here
