;;; finders-spec.el --- Schema-aware finder tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for the org-graph schema-aware finders
;; (finders-and-filters task / RE-3).  Finders are thin: they hand
;; `vulpea-find' a `:filter-fn' and let it source candidates and visit.
;; So these specs test the FILTER, not the UI: each spec stubs
;; `vulpea-find' with a local `cl-letf' to CAPTURE the args plist, then
;; feeds `org-graph-test/note-fixture' notes through the captured
;; `:filter-fn' and asserts it admits the right type and rejects others
;; (including an untagged note).
;;
;; `org-graph-test/with-stubbed-vulpea' does not stub `vulpea-find', so a
;; local `cl-letf' is the right tool here (mirroring the helper-gap note
;; in note-type-schemas).  The real note-type schemas are registered and
;; the real `vulpea-schema-applies-p' runs end-to-end, so the filter is
;; exercised against the genuine taxonomy predicates rather than a mock.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  ;; schemas.el must load first: finders.el `(require 'org-graph-schemas)'
  ;; and its filters delegate to the registered schema predicates.
  (require 'org-graph-schemas (expand-file-name "schemas.el" module-dir))
  (require 'org-graph-finders (expand-file-name "finders.el" module-dir)))

;; Owned by the loader defcustom, which is not loaded standalone; declare
;; special so the schemas fall back to their internal default.
(defvar org-graph-note-types)

(defun org-graph-test--capture-find-args (thunk)
  "Invoke THUNK with `vulpea-find' stubbed; return the captured args plist.
THUNK is a finder command.  `vulpea-find' is shadowed for the dynamic
extent so no completion UI or DB access occurs."
  (let (captured)
    (cl-letf (((symbol-function 'vulpea-find)
               (lambda (&rest args) (setq captured args) nil)))
      (funcall thunk))
    captured))

(defun org-graph-test--capture-filter (thunk)
  "Return the `:filter-fn' a finder THUNK passes to `vulpea-find'."
  (plist-get (org-graph-test--capture-find-args thunk) :filter-fn))

(describe "org-graph schema-aware finders"

  (before-each
    ;; Register before every spec so the predicates the filters delegate
    ;; to are present regardless of cross-spec ordering.
    (org-graph-schemas-register))

  (describe "org-graph/note-of-type-p"
    (it "admits a note carrying the type's filetag"
      (let ((topic (org-graph-test/note-fixture :id "t1" :tags '("topic"))))
        (expect (org-graph/note-of-type-p topic 'topic) :to-be-truthy)))

    (it "rejects a note of a different type"
      (let ((log (org-graph-test/note-fixture :id "l1" :tags '("log"))))
        (expect (org-graph/note-of-type-p log 'topic) :to-be nil)))

    (it "rejects a note with no filetags"
      (let ((bare (org-graph-test/note-fixture :id "b1")))
        (expect (org-graph/note-of-type-p bare 'topic) :to-be nil)))

    (it "returns nil for an unregistered note type"
      (let ((topic (org-graph-test/note-fixture :id "t2" :tags '("topic"))))
        (expect (org-graph/note-of-type-p topic 'nonexistent) :to-be nil))))

  (describe "per-type finder filters"
    ;; Table-drive the five per-type finders: each filter must admit its
    ;; own type and reject the other types and an untagged note.
    (let ((finders '((org-graph/find-topic     . "topic")
                     (org-graph/find-debug     . "debug")
                     (org-graph/find-log       . "log")
                     (org-graph/find-reference . "reference")
                     (org-graph/find-project   . "project"))))
      (dolist (entry finders)
        (let ((fn (car entry))
              (tag (cdr entry)))
          (it (format "%s passes a filter admitting only %s notes" fn tag)
            (let* ((filter (org-graph-test--capture-filter fn))
                   (own  (org-graph-test/note-fixture :id "own" :tags (list tag)))
                   (other-tag (if (equal tag "topic") "log" "topic"))
                   (other (org-graph-test/note-fixture
                           :id "other" :tags (list other-tag)))
                   (bare (org-graph-test/note-fixture :id "bare")))
              (expect (functionp filter) :to-be-truthy)
              (expect (funcall filter own) :to-be-truthy)
              (expect (funcall filter other) :to-be nil)
              (expect (funcall filter bare) :to-be nil)))))))

  (describe "org-graph/find-any"
    (it "passes no :filter-fn, so vulpea-find offers every note"
      (let ((args (org-graph-test--capture-find-args #'org-graph/find-any)))
        (expect (plist-get args :filter-fn) :to-be nil))))

  (describe "org-graph/find-agent-drafts"
    (it "filters the agent-draft filetag directly, not a type schema"
      (let* ((filter (org-graph-test--capture-filter #'org-graph/find-agent-drafts))
             (draft (org-graph-test/note-fixture
                     :id "d1" :tags '("topic" "agent-draft")))
             (plain (org-graph-test/note-fixture :id "p1" :tags '("topic")))
             (bare  (org-graph-test/note-fixture :id "b2")))
        (expect (funcall filter draft) :to-be-truthy)
        (expect (funcall filter plain) :to-be nil)
        (expect (funcall filter bare) :to-be nil)))

    (it "admits an agent-draft note regardless of its note type"
      ;; agent-draft is cross-cutting: a draft tagged with no taxonomy
      ;; type at all is still surfaced by the review finder.
      (let* ((filter (org-graph-test--capture-filter #'org-graph/find-agent-drafts))
             (typeless-draft (org-graph-test/note-fixture
                              :id "d2" :tags '("agent-draft"))))
        (expect (funcall filter typeless-draft) :to-be-truthy))))

  (describe "agent-draft is orthogonal to the type finders"
    (it "find-topic still admits a topic note that is also an agent-draft"
      (let* ((filter (org-graph-test--capture-filter #'org-graph/find-topic))
             (both (org-graph-test/note-fixture
                    :id "x1" :tags '("topic" "agent-draft"))))
        (expect (funcall filter both) :to-be-truthy)))))

;;; finders-spec.el ends here
