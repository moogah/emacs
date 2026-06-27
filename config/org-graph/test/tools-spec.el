;;; tools-spec.el --- org-graph gptel agent tool tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the agent-facing tool surface in tools.el:
;;
;; 1. The three gptel tools are constructed and surfaced as a reusable
;;    list via `org-graph/agent-tools' (the accessor the
;;    workspace-integration task hands to the workspace-assistant preset).
;;    register/boundary/org-graph-agent-tools (crystallised by this task).
;;
;; 2. `org-graph-tools/write-node' routes its write through the per-file
;;    coordinator lock (register/invariant/coordinator-lock-contract) and
;;    stamps the agent-draft filetag.  The lock is exercised for real (the
;;    coordinator macro cannot be cl-letf-stubbed): the stubbed
;;    `vulpea-create' observes the live lock table while it "writes",
;;    proving the body ran inside the lock and the lock released after.
;;
;; 3. The read tools (`org-graph-tools/query', `.../typed-edges') project
;;    their results onto flat agent plists, layered on the genuine
;;    boundaries (`vulpea-db-query' for notes; the query.el API for
;;    edges), stubbed function-scoped via `cl-letf'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir))
       (root (locate-dominating-file test-dir "early-init.el")))
  ;; gptel (and its compat dep) for `gptel-make-tool' / `gptel-tool-p'.
  ;; Like helpers-spec does for vulpea, add the straight build dirs since
  ;; gptel is not activated in the test process.
  (when root
    (dolist (pkg '("compat" "gptel"))
      (let ((d (expand-file-name (format "runtime/straight/build/%s" pkg) root)))
        (when (file-directory-p d) (add-to-list 'load-path d)))))
  (require 'gptel)
  (add-to-list 'load-path test-dir)
  ;; helpers-spec adds vulpea to load-path and requires it.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-query (expand-file-name "query.el" module-dir))
  (require 'org-graph-coordinator (expand-file-name "coordinator.el" module-dir))
  (require 'org-graph-tools (expand-file-name "tools.el" module-dir)))

;;; Tool construction & accessor -------------------------------------------

(describe "org-graph gptel tool surface"

  (it "registers three tools and exposes them via org-graph/agent-tools"
    (org-graph-tools-register)
    (let ((tools (org-graph/agent-tools)))
      (expect (length tools) :to-equal 3)
      (expect (seq-every-p #'gptel-tool-p tools) :to-be t)
      (expect (mapcar #'gptel-tool-name tools)
              :to-equal '("org_graph_query"
                          "org_graph_typed_edges"
                          "org_graph_write_node"))))

  (it "returns gptel-tool objects, not bare names (workspace preset :tools)"
    (org-graph-tools-register)
    ;; The speculated register entry must expose objects so the preset's
    ;; :tools slot receives them directly.
    (expect (cl-every #'gptel-tool-p (org-graph/agent-tools)) :to-be t))

  (it "states the roam-only typed-edge boundary in the write tool description"
    (org-graph-tools-register)
    (let ((wt (seq-find (lambda (tl)
                          (equal (gptel-tool-name tl) "org_graph_write_node"))
                        (org-graph/agent-tools))))
      (expect (gptel-tool-description wt) :to-match "roam")
      (expect (gptel-tool-description wt) :to-match "agent-draft"))))

;;; Write node -------------------------------------------------------------

(describe "org-graph-tools/write-node"

  (before-each
    (clrhash org-graph-coordinator--locks))

  (it "routes the write through the coordinator file lock"
    (let (held-during one-lock-held seen-path)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest _rest)
                   ;; Observe the live lock table from inside the "write".
                   (setq seen-path path
                         held-during (gethash
                                      (org-graph-coordinator--canonical path)
                                      org-graph-coordinator--locks)
                         one-lock-held
                         (= (hash-table-count org-graph-coordinator--locks) 1))
                   (org-graph-test/note-fixture :id "new1" :title title :path path))))
        (org-graph-tools/write-node "My Note" "/tmp/og-write/")
        ;; BODY ran while exactly this path's lock was held ...
        (expect held-during :to-be t)
        (expect one-lock-held :to-be t)
        ;; ... and the lock was released afterward (unwind-protect).
        (expect (gethash (org-graph-coordinator--canonical seen-path)
                         org-graph-coordinator--locks)
                :to-be nil))))

  (it "stamps the agent-draft filetag"
    (let (seen-tags)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest rest)
                   (setq seen-tags (plist-get rest :tags))
                   (org-graph-test/note-fixture :id "n" :title title :path path))))
        (org-graph-tools/write-node "T" "/tmp/og/")
        (expect (member "agent-draft" seen-tags) :to-be-truthy))))

  (it "preserves caller tags and does not duplicate agent-draft"
    (let (seen-tags)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest rest)
                   (setq seen-tags (plist-get rest :tags))
                   (org-graph-test/note-fixture :id "n" :title title :path path))))
        (org-graph-tools/write-node "T" "/tmp/og/" '("topic" "agent-draft"))
        (expect (member "topic" seen-tags) :to-be-truthy)
        (expect (seq-count (lambda (tg) (equal tg "agent-draft")) seen-tags)
                :to-equal 1))))

  (it "coerces a tags vector (JSON array) to a list"
    (let (seen-tags)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest rest)
                   (setq seen-tags (plist-get rest :tags))
                   (org-graph-test/note-fixture :id "n" :title title :path path))))
        (org-graph-tools/write-node "T" "/tmp/og/" ["topic"])
        (expect (listp seen-tags) :to-be t)
        (expect (member "topic" seen-tags) :to-be-truthy)
        (expect (member "agent-draft" seen-tags) :to-be-truthy))))

  (it "returns the new note id and path"
    (cl-letf (((symbol-function 'vulpea-create)
               (lambda (title path &rest _rest)
                 (org-graph-test/note-fixture :id "id-42" :title title :path path))))
      (let ((result (org-graph-tools/write-node "Topic A" "/tmp/og/")))
        (expect (plist-get result :id) :to-equal "id-42")
        (expect (string-prefix-p "/tmp/og/" (plist-get result :path)) :to-be t)
        (expect (string-suffix-p ".org" (plist-get result :path)) :to-be t))))

  (it "writes a slugged filename under the given directory"
    (let (seen-path)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest _rest)
                   (setq seen-path path)
                   (org-graph-test/note-fixture :id "n" :title title :path path))))
        (org-graph-tools/write-node "Hello World!" "/tmp/og/")
        (expect (string-prefix-p "/tmp/og/" seen-path) :to-be t)
        ;; slug is lower-cased, non-alnum collapsed to single hyphens
        (expect (string-match-p "hello-world\\.org\\'" seen-path) :to-be-truthy))))

  (it "defaults to the roam root when no directory is given"
    (let (seen-path)
      (cl-letf (((symbol-function 'vulpea-create)
                 (lambda (title path &rest _rest)
                   (setq seen-path path)
                   (org-graph-test/note-fixture :id "n" :title title :path path))))
        (org-graph-tools/write-node "Note")
        (expect (string-prefix-p (expand-file-name "~/org/roam/") seen-path)
                :to-be t)))))

;;; Note query -------------------------------------------------------------

(describe "org-graph-tools/query"

  (defun org-graph-tools-spec--notes ()
    "Two synthetic notes for query specs."
    (list (org-graph-test/note-fixture
           :id "a" :title "Alpha" :tags '("topic") :path "/x/a.org")
          (org-graph-test/note-fixture
           :id "b" :title "Beta" :tags '("log") :path "/x/b.org")))

  (defmacro org-graph-tools-spec--with-db (&rest body)
    "Run BODY with `vulpea-db-query' stubbed over the two synthetic notes."
    `(cl-letf (((symbol-function 'vulpea-db-query)
                (lambda (&optional pred)
                  (let ((notes (org-graph-tools-spec--notes)))
                    (if pred (seq-filter pred notes) notes)))))
       ,@body))

  (it "projects matching notes onto flat plists"
    (org-graph-tools-spec--with-db
     (expect (org-graph-tools/query "alph")
             :to-equal
             (list (list :id "a" :title "Alpha"
                         :tags '("topic") :path "/x/a.org")))))

  (it "matches titles case-insensitively as a substring"
    (org-graph-tools-spec--with-db
     (expect (mapcar (lambda (p) (plist-get p :id))
                     (org-graph-tools/query "ET"))
             :to-equal '("b"))))

  (it "filters by exact filetag"
    (org-graph-tools-spec--with-db
     (expect (mapcar (lambda (p) (plist-get p :id))
                     (org-graph-tools/query nil "log"))
             :to-equal '("b"))))

  (it "returns all notes when neither filter is given"
    (org-graph-tools-spec--with-db
     (expect (mapcar (lambda (p) (plist-get p :id))
                     (org-graph-tools/query))
             :to-equal '("a" "b")))))

;;; Typed-edge query -------------------------------------------------------

(describe "org-graph-tools/typed-edges"

  (it "wraps connected by default and resolves far-end titles"
    (let ((note (org-graph-test/note-fixture :id "to" :title "Target")))
      (cl-letf (((symbol-function 'org-graph-query/connected)
                 (lambda (id)
                   (list (list :from id :rel 'implements :to "to" :note note)))))
        (expect (org-graph-tools/typed-edges "n1")
                :to-equal
                (list (list :from "n1" :rel 'implements :to "to" :title "Target"))))))

  (it "routes outgoing and interns a string rel-type to a SYMBOL"
    (let (captured)
      (cl-letf (((symbol-function 'org-graph-query/outgoing)
                 (lambda (id rel)
                   (setq captured (list id rel))
                   nil)))
        (org-graph-tools/typed-edges "n1" "outgoing" "implements")
        (expect captured :to-equal '("n1" implements)))))

  (it "routes incoming"
    (let (captured)
      (cl-letf (((symbol-function 'org-graph-query/incoming)
                 (lambda (id rel) (setq captured (list id rel)) nil)))
        (org-graph-tools/typed-edges "n1" "incoming")
        (expect captured :to-equal '("n1" nil)))))

  (it "a rel-filtered connected query is the union of both directions"
    (let (out-rel in-rel)
      (cl-letf (((symbol-function 'org-graph-query/outgoing)
                 (lambda (_id rel) (setq out-rel rel) nil))
                ((symbol-function 'org-graph-query/incoming)
                 (lambda (_id rel) (setq in-rel rel) nil)))
        (org-graph-tools/typed-edges "n1" "connected" "relates-to")
        (expect out-rel :to-equal 'relates-to)
        (expect in-rel :to-equal 'relates-to))))

  (it "leaves :title nil when the far-end note is unresolved"
    (cl-letf (((symbol-function 'org-graph-query/connected)
               (lambda (id)
                 (list (list :from id :rel 'relates-to :to "ghost" :note nil)))))
      (expect (org-graph-tools/typed-edges "n1")
              :to-equal
              (list (list :from "n1" :rel 'relates-to :to "ghost" :title nil))))))

;;; tools-spec.el ends here
