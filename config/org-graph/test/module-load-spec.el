;;; module-load-spec.el --- org-graph module-load smoke / integration spec -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Cold-standalone-load gate for the org-graph spike (design D7 / D8,
;; RE-5).  This is the integration smoke spec that proves, in ONE place,
;; that after the module's load sequence runs EVERY registration landed
;; and org-roam is untouched:
;;
;;   - the loader defcustoms exist with their documented defaults,
;;   - the five note-type schemas are registered with vulpea,
;;   - the typed-edge extractor is registered (its `typed_edges' schema
;;     present in vulpea's extractor set),
;;   - the three snake_case gptel tools are in gptel's registry and
;;     `org-graph/agent-tools' returns the constructed tool objects,
;;   - the workspace integration registers an :on-create + :menu seam,
;;   - org-roam coexists unchanged and the vulpea DB path is isolated
;;     from org-roam's (D8, the spike's clean-rollback property).
;;
;; This spec PROBES `register/invariant/org-graph-loader-ordered-sequence'
;; (the canonical submodule order is
;;   schemas -> extractor -> coordinator -> query -> finders -> tools
;;   -> discovery, with workspace-integration AFTER tools).
;;
;; DIVERGENCE recorded by this task (see the task's `## Discoveries'):
;; the current `org-graph.el' loader is still SCATTERED -- it does NOT
;; load `extractor.el' or `discovery.el', and loads query/tools before
;; schemas -- so a bare `(require 'org-graph)' does NOT fire every
;; registration.  Consolidating the loader is the downstream
;; `wire-into-init' task, NOT this one.  This spec therefore drives the
;; END STATE the consolidated loader must reach: it loads every submodule
;; by path in canonical order and calls the function-exposed registration
;; entry points (`org-graph-schemas-register',
;; `org-graph-extractor-register', `org-graph-tools-register' -- exposed
;; as functions precisely to avoid a require-time DB open), then asserts
;; the effects.  Registration is mocked at the vulpea/gptel/workspaces
;; boundary (`cl-letf', function-scoped); no live SQLite DB or fswatch is
;; required.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

;; --- Load-path + module setup ------------------------------------------
;;
;; Mirror the standalone-load pattern used by the per-module specs
;; (helpers-spec adds vulpea; tools-spec adds compat + gptel).  vulpea,
;; gptel and org-roam are not in `jf/enabled-modules' yet (that is the
;; wire-into-init task), so they are not on `load-path' in the test
;; process; add their straight build dirs before requiring them.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir))
       (root (locate-dominating-file test-dir "early-init.el")))
  (add-to-list 'load-path test-dir)
  (when root
    (dolist (pkg '("compat" "gptel" "org-roam"))
      (let ((d (expand-file-name (format "runtime/straight/build/%s" pkg) root)))
        (when (file-directory-p d) (add-to-list 'load-path d)))))
  ;; gptel: so `(fboundp 'gptel-make-tool)' holds and tool registration
  ;; can fire (step 5).  org-roam: so the coexistence assertions (step 7)
  ;; exercise the real org-roam symbols.
  (require 'gptel)
  (require 'org-roam)
  ;; helpers-spec adds vulpea to `load-path' and requires it.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  ;; The loader: fires `use-package vulpea' (pins `vulpea-db-location')
  ;; and its current (scattered) submodule loads.
  (require 'org-graph (expand-file-name "org-graph.el" module-dir))
  ;; The canonical submodule set the CONSOLIDATED loader must load, in
  ;; dependency order.  Most are already loaded transitively by the
  ;; loader / each other; `require' is idempotent.  `extractor.el' and
  ;; `discovery.el' are the two the current loader OMITS -- load them
  ;; here so every registration entry point is defined (the END STATE).
  (require 'org-graph-schemas (expand-file-name "schemas.el" module-dir))
  (require 'org-graph-coordinator (expand-file-name "coordinator.el" module-dir))
  (require 'org-graph-query (expand-file-name "query.el" module-dir))
  (require 'org-graph-finders (expand-file-name "finders.el" module-dir))
  (require 'org-graph-tools (expand-file-name "tools.el" module-dir))
  (require 'org-graph-workspace-integration
           (expand-file-name "workspace-integration.el" module-dir))
  ;; discovery.el runs `org-graph/seed-org-id-locations' at load (guarded
  ;; by a condition-case); stub the DB query so the seed is a true no-op
  ;; and no SQLite DB is opened.
  (cl-letf (((symbol-function 'vulpea-db-query) (lambda (&rest _) nil)))
    (require 'org-graph-extractor (expand-file-name "extractor.el" module-dir))
    (require 'org-graph-discovery (expand-file-name "discovery.el" module-dir))))

(describe "org-graph module load smoke (D7/D8, RE-5; probes register/invariant/org-graph-loader-ordered-sequence)"

  ;; --- Step 2: loader defcustoms ---------------------------------------

  (describe "loader defcustoms exist with documented defaults"

    (it "org-graph-roam-root defaults to the roam concept vault"
      (expect (boundp 'org-graph-roam-root) :to-be-truthy)
      (expect org-graph-roam-root :to-equal "~/org/roam/"))

    (it "org-graph-relation-types is the closed relation set"
      (expect (boundp 'org-graph-relation-types) :to-be-truthy)
      (expect org-graph-relation-types
              :to-equal '(implements contradicts supersedes relates-to)))

    (it "org-graph-note-types is the fixed note-type taxonomy"
      (expect (boundp 'org-graph-note-types) :to-be-truthy)
      (expect org-graph-note-types
              :to-equal '(log debug topic reference project)))

    (it "org-graph-watch-workspace-homes defaults on"
      (expect (boundp 'org-graph-watch-workspace-homes) :to-be-truthy)
      (expect org-graph-watch-workspace-homes :to-be t)))

  ;; --- Step 3: note-type schemas registered ----------------------------

  (describe "note-type schemas registered with vulpea"

    (it "registers the five org-graph note-type schemas"
      ;; Idempotent; schemas.el also calls this at load.
      (let ((names (org-graph-schemas-register)))
        (expect names :to-equal
                '(org-graph-log org-graph-debug org-graph-topic
                  org-graph-reference org-graph-project)))
      (dolist (name '(org-graph-log org-graph-debug org-graph-topic
                      org-graph-reference org-graph-project))
        (expect (vulpea-schema-get name) :to-be-truthy)
        (expect (memq name (vulpea-schema-list)) :to-be-truthy))))

  ;; --- Step 4: typed-edge extractor registered -------------------------

  (describe "typed-edge extractor registered with vulpea"

    (it "registers org-graph-typed-edges carrying the typed_edges schema"
      ;; `vulpea-db-register-extractor' applies the plugin schema, which
      ;; opens the DB; stub the DB boundary so registration is inert and
      ;; no SQLite file is touched.  The push onto `vulpea-db--extractors'
      ;; (the registry we inspect) still happens for real.
      (cl-letf (((symbol-function 'vulpea-db) (lambda (&rest _) 'stub-db))
                ((symbol-function 'emacsql) (lambda (&rest _) nil)))
        (org-graph-extractor-register))
      (let ((ex (vulpea-db-get-extractor 'org-graph-typed-edges)))
        (expect ex :to-be-truthy)
        (expect (vulpea-extractor-name ex) :to-be 'org-graph-typed-edges)
        (expect (vulpea-extractor-extract-fn ex)
                :to-be #'org-graph-extractor/extract)
        ;; the typed_edges schema is present in the registered set
        (expect (assq 'typed_edges (vulpea-extractor-schema ex))
                :to-be-truthy))))

  ;; --- Step 5: gptel agent tools registered ----------------------------

  (describe "gptel agent tools registered"

    ;; gptel is loaded in this process, so `gptel-make-tool' is fbound
    ;; and registration fires.  In a gptel-less process the loader skips
    ;; it and `org-graph/agent-tools' is nil (asserted nil-tolerant in
    ;; the per-module tools-spec); here we drive the real path.
    (before-all (org-graph-tools-register))

    (it "exposes exactly the three constructed gptel-tool objects"
      (let ((tools (org-graph/agent-tools)))
        (expect (length tools) :to-equal 3)
        (expect (seq-every-p #'gptel-tool-p tools) :to-be t)
        (expect (mapcar #'gptel-tool-name tools)
                :to-equal '("org_graph_query"
                            "org_graph_typed_edges"
                            "org_graph_write_node"))))

    (it "the three snake_case tools are present in the gptel registry"
      (dolist (name '("org_graph_query" "org_graph_typed_edges"
                      "org_graph_write_node"))
        (let ((tool (ignore-errors (gptel-get-tool (list "org-graph" name)))))
          (expect (gptel-tool-p tool) :to-be t)
          (expect (gptel-tool-name tool) :to-equal name)))))

  ;; --- Step 6: workspace integration registered ------------------------

  (describe "workspace integration registered"

    ;; Registration is `with-eval-after-load 'workspaces'-gated, so in a
    ;; bare test process the form never fires; drive the register body
    ;; directly with the registry entry point stubbed (a spy).
    (it "registers org-graph with an :on-create and a :menu seam"
      (let (captured-id captured-plist)
        (cl-letf (((symbol-function 'workspace-register-integration)
                   (lambda (id &rest plist)
                     (setq captured-id id captured-plist plist)
                     id)))
          (org-graph-workspace-integration-register))
        (expect captured-id :to-be 'org-graph)
        (expect (plist-get captured-plist :label) :to-equal "org-graph")
        ;; :on-create handler present and fbound
        (expect (plist-get captured-plist :on-create)
                :to-be #'org-graph-workspace-integration--on-create)
        (expect (fboundp 'org-graph-workspace-integration--on-create)
                :to-be-truthy)
        ;; :menu is a (KEY . COMMAND) cons; COMMAND fbound, accepts the
        ;; anchor payload (arity 1)
        (let ((menu (plist-get captured-plist :menu)))
          (expect (consp menu) :to-be-truthy)
          (expect (car menu) :to-equal "G")
          (expect (cdr menu) :to-be #'org-graph-workspace-integration--menu)
          (expect (fboundp 'org-graph-workspace-integration--menu)
                  :to-be-truthy)))))

  ;; --- Step 7: org-roam coexistence (D8) -------------------------------

  (describe "org-roam coexistence (D8 / register/invariant/vulpea-db-isolation)"

    (it "leaves org-roam's variables and functions bound and intact"
      (expect (boundp 'org-roam-directory) :to-be-truthy)
      (expect (stringp org-roam-directory) :to-be-truthy)
      (expect (boundp 'org-roam-db-location) :to-be-truthy)
      (expect (fboundp 'org-roam-db-sync) :to-be-truthy))

    (it "keeps the vulpea DB path distinct from org-roam's DB and dir"
      (expect (boundp 'vulpea-db-location) :to-be-truthy)
      (expect (stringp vulpea-db-location) :to-be-truthy)
      ;; the org-graph vulpea index is NOT org-roam's DB ...
      (expect vulpea-db-location :not :to-equal org-roam-db-location)
      ;; ... and does not live inside org-roam's directory.
      (expect (file-in-directory-p
               vulpea-db-location
               (expand-file-name org-roam-directory))
              :not :to-be-truthy)))

  ;; --- DB isolation at the loader level (D8) ---------------------------

  (describe "vulpea DB isolation pinned by the loader (D8)"

    (it "resolves the vulpea DB under runtime state/, not vulpea's default"
      (let ((state-dir (expand-file-name "state" user-emacs-directory)))
        (expect (file-in-directory-p vulpea-db-location state-dir)
                :to-be-truthy)
        (expect (file-name-nondirectory vulpea-db-location)
                :to-equal "notes.db")
        (expect vulpea-db-location
                :not :to-equal
                (expand-file-name "vulpea.db" user-emacs-directory))))))

(provide 'module-load-spec)
;;; module-load-spec.el ends here
