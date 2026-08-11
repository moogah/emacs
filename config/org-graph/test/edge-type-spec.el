;;; edge-type-spec.el --- Edge-type registry tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for the org-graph edge-type registry (edge-type-registry
;; task / OV-6).  The registry is enrich-only
;; (register/boundary/edge-type-registry-lookup): a missing entry is a normal
;; nil result, no extraction path consults it, and with zero registry notes
;; the lookup is empty rather than an error.
;;
;; Only the DB boundary is mocked: `org-graph/edge-types' reads notes via
;; `vulpea-db-query' with the `:edge-type:' filetag predicate; that one call
;; is shadowed with a local `cl-letf' stub that applies the REAL predicate to
;; an in-memory note list, so predicate + metadata parsing run end-to-end
;; with no SQLite.  The finder spec captures the plist `vulpea-find' receives
;; instead of driving real completion.  Seed installation runs against a
;; temporary directory — never the real vault.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-edge-type (expand-file-name "edge-type.el" module-dir)))

;; Owned by the loader (a defcustom), not loaded in this standalone process.
(defvar org-graph-roam-root)

(defmacro org-graph-edge-type-test--with-notes (notes &rest body)
  "Evaluate BODY with `vulpea-db-query' stubbed over the NOTES list.
The stub applies the received predicate to NOTES, so the real
`org-graph-edge-type-note-p' filtering is exercised."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'vulpea-db-query)
              (lambda (&optional pred)
                (if pred (seq-filter pred ,notes) ,notes))))
     ,@body))

(describe "org-graph edge-type registry"

  (before-each
    (org-graph-edge-type-invalidate-cache))

  (describe ":edge-type: selector predicate"
    (it "admits a note carrying the edge-type filetag"
      (let ((note (org-graph-test/note-fixture
                   :id "e1" :title "implements" :tags '("edge-type"))))
        (expect (org-graph-edge-type-note-p note) :to-be t)))

    (it "rejects notes with other tags or no tags"
      (let ((topic (org-graph-test/note-fixture
                    :id "t1" :title "A topic" :tags '("topic")))
            (bare (org-graph-test/note-fixture :id "b1" :title "Bare")))
        (expect (org-graph-edge-type-note-p topic) :to-be nil)
        (expect (org-graph-edge-type-note-p bare) :to-be nil))))

  (describe "org-graph/edge-types metadata load"
    (it "keys the lookup by the normalized title and parses all four fields"
      (let* ((note (org-graph-test/note-fixture
                    :id "e2" :title "Follows Up" :tags '("edge-type")
                    :properties '(("LABEL" . "follows up on")
                                  ("INVERSE" . "followed_up_by")
                                  ("SYMMETRIC" . "nil")
                                  ("DESCRIPTION" . "Continues an earlier thread."))))
             (lookup (org-graph-edge-type-test--with-notes (list note)
                       (org-graph/edge-types)))
             (meta (gethash 'follows-up lookup)))
        (expect (hash-table-count lookup) :to-equal 1)
        (expect (plist-get meta :label) :to-equal "follows up on")
        (expect (plist-get meta :inverse) :to-equal 'followed-up-by)
        (expect (plist-get meta :symmetric) :to-be nil)
        (expect (plist-get meta :description)
                :to-equal "Continues an earlier thread.")))

    (it "defaults the label to the title and absent inverse/symmetric to nil"
      (let* ((note (org-graph-test/note-fixture
                    :id "e3" :title "implements" :tags '("edge-type")))
             (meta (org-graph-edge-type-test--with-notes (list note)
                     (org-graph/edge-type 'implements))))
        (expect (plist-get meta :label) :to-equal "implements")
        (expect (plist-get meta :inverse) :to-be nil)
        (expect (plist-get meta :symmetric) :to-be nil)
        (expect (plist-get meta :description) :to-be nil)))

    (it "parses :SYMMETRIC: t as boolean t"
      (let* ((note (org-graph-test/note-fixture
                    :id "e4" :title "contradicts" :tags '("edge-type")
                    :properties '(("SYMMETRIC" . "t")))))
        (expect (plist-get (org-graph-edge-type-test--with-notes (list note)
                             (org-graph/edge-type 'contradicts))
                           :symmetric)
                :to-be t)))

    (it "falls back to the description vulpea meta entry"
      (let* ((note (org-graph-test/note-fixture
                    :id "e5" :title "supersedes" :tags '("edge-type")
                    :meta '(("description" "Replaces the target.")))))
        (expect (plist-get (org-graph-edge-type-test--with-notes (list note)
                             (org-graph/edge-type 'supersedes))
                           :description)
                :to-equal "Replaces the target.")))

    (it "loads only :edge-type:-tagged notes into the lookup"
      (let* ((reg (org-graph-test/note-fixture
                   :id "e6" :title "implements" :tags '("edge-type")))
             (topic (org-graph-test/note-fixture
                     :id "t2" :title "benchmarks" :tags '("topic")))
             (lookup (org-graph-edge-type-test--with-notes (list reg topic)
                       (org-graph/edge-types))))
        (expect (hash-table-count lookup) :to-equal 1)
        (expect (gethash 'implements lookup) :not :to-be nil)
        (expect (gethash 'benchmarks lookup) :to-be nil))))

  (describe "graceful absence"
    (it "returns an EMPTY lookup when no registry notes exist"
      (let ((lookup (org-graph-edge-type-test--with-notes nil
                      (org-graph/edge-types))))
        (expect (hash-table-p lookup) :to-be t)
        (expect (hash-table-count lookup) :to-equal 0)))

    (it "returns nil (a normal result) for an unregistered type"
      (let ((reg (org-graph-test/note-fixture
                  :id "e7" :title "implements" :tags '("edge-type"))))
        (org-graph-edge-type-test--with-notes (list reg)
          (expect (org-graph/edge-type 'falsifies) :to-be nil)))))

  (describe "session cache"
    (it "queries the DB once across repeated reads"
      (let ((calls 0)
            (note (org-graph-test/note-fixture
                   :id "e8" :title "implements" :tags '("edge-type"))))
        (cl-letf (((symbol-function 'vulpea-db-query)
                   (lambda (&optional pred)
                     (cl-incf calls)
                     (seq-filter pred (list note)))))
          (org-graph/edge-types)
          (org-graph/edge-types)
          (expect calls :to-equal 1))))

    (it "re-reads after an explicit invalidation or with REFRESH"
      (let ((calls 0))
        (cl-letf (((symbol-function 'vulpea-db-query)
                   (lambda (&optional _pred) (cl-incf calls) nil)))
          (org-graph/edge-types)
          (org-graph-edge-type-invalidate-cache)
          (org-graph/edge-types)
          (org-graph/edge-types 'refresh)
          (expect calls :to-equal 3)))))

  (describe "seed registry notes"
    (it "defines the four starter types with their inverse/symmetric metadata"
      (let ((seeds org-graph-edge-type-seed-definitions))
        (expect (mapcar #'car seeds)
                :to-equal '(implements contradicts supersedes relates-to))
        (expect (plist-get (alist-get 'implements seeds) :inverse)
                :to-equal 'implemented-by)
        (expect (plist-get (alist-get 'contradicts seeds) :symmetric) :to-be t)
        (expect (plist-get (alist-get 'supersedes seeds) :inverse)
                :to-equal 'superseded-by)
        (expect (plist-get (alist-get 'relates-to seeds) :symmetric) :to-be t)))

    (it "installs four registry-note files with ID, filetag, and metadata"
      (let* ((dir (make-temp-file "edge-type-seeds" t))
             (written (org-graph-edge-type-install-seeds dir)))
        (unwind-protect
            (progn
              (expect (length written) :to-equal 4)
              (dolist (path written)
                (expect (file-exists-p path) :to-be t))
              (let ((text (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "edge-type-implements.org" dir))
                            (buffer-string))))
                (expect text :to-match ":ID:")
                (expect text :to-match "#\\+filetags: :edge-type:")
                (expect text :to-match ":INVERSE:  implemented-by")
                (expect text :to-match "#\\+title: implements"))
              (let ((text (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "edge-type-contradicts.org" dir))
                            (buffer-string))))
                (expect text :to-match ":SYMMETRIC: t")))
          (delete-directory dir t))))

    (it "is idempotent: a second install writes nothing and keeps user edits"
      (let ((dir (make-temp-file "edge-type-seeds" t)))
        (unwind-protect
            (let* ((first (org-graph-edge-type-install-seeds dir))
                   (path (car first)))
              (with-temp-file path (insert "user edit"))
              (expect (org-graph-edge-type-install-seeds dir) :to-be nil)
              (expect (with-temp-buffer
                        (insert-file-contents path)
                        (buffer-string))
                      :to-equal "user edit"))
          (delete-directory dir t))))

    (it "loads seed-shaped notes into the expected lookup entries"
      ;; Model the four installed seeds as indexed notes and prove the
      ;; loader graduates each with its declared metadata.
      (let* ((notes
              (mapcar
               (lambda (seed)
                 (let ((rel (car seed))
                       (plist (cdr seed)))
                   (org-graph-test/note-fixture
                    :id (format "seed-%s" rel)
                    :title (symbol-name rel)
                    :tags '("edge-type")
                    :properties
                    (append
                     (when (plist-get plist :inverse)
                       (list (cons "INVERSE"
                                   (symbol-name (plist-get plist :inverse)))))
                     (when (plist-get plist :symmetric)
                       (list (cons "SYMMETRIC" "t")))))))
               org-graph-edge-type-seed-definitions))
             (lookup (org-graph-edge-type-test--with-notes notes
                       (org-graph/edge-types))))
        (expect (hash-table-count lookup) :to-equal 4)
        (expect (plist-get (gethash 'implements lookup) :inverse)
                :to-equal 'implemented-by)
        (expect (plist-get (gethash 'contradicts lookup) :symmetric) :to-be t)
        (expect (plist-get (gethash 'supersedes lookup) :inverse)
                :to-equal 'superseded-by)
        (expect (plist-get (gethash 'relates-to lookup) :symmetric) :to-be t))))

  (describe "org-graph/find-edge-type"
    (it "restricts vulpea-find to registry notes with require-match"
      (let (captured)
        (cl-letf (((symbol-function 'vulpea-find)
                   (lambda (&rest args) (setq captured args))))
          (org-graph/find-edge-type))
        (let ((filter (plist-get captured :filter-fn))
              (reg (org-graph-test/note-fixture
                    :id "e9" :title "implements" :tags '("edge-type")))
              (topic (org-graph-test/note-fixture
                      :id "t3" :title "A topic" :tags '("topic"))))
          (expect (plist-get captured :require-match) :to-be t)
          (expect (funcall filter reg) :to-be-truthy)
          (expect (funcall filter topic) :to-be nil))))))

;;; edge-type-spec.el ends here
