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
;; This spec is the REAL cold-load guard for
;; `register/invariant/org-graph-loader-ordered-sequence' (canonical
;; ten-submodule order schemas -> extractor -> coordinator -> query ->
;; finders -> authoring -> edge-type -> tools -> discovery, with
;; workspace-integration AFTER tools).  It loads org-graph via
;; `(require 'org-graph)' ALONE -- it does
;; NOT path-load the submodules itself -- and then asserts that every
;; registration fired FROM THE LOADER PATH.  If the loader were still
;; scattered (omitting extractor/coordinator/discovery, or mis-ordered),
;; the require would not fire all registrations and these assertions would
;; fail.  This is what makes the guard meaningful rather than a tautology.
;;
;; DB-free load contract: `(require 'org-graph)' must not open the vulpea
;; DB.  The two DB-touching registrations (the typed-edge extractor, which
;; applies its schema, and the discovery `org-id-locations' seed) are
;; deferred by the loader to `emacs-startup-hook' (both wired by the
;; loader through the shared `org-graph--run-deferred-op' idiom), which
;; does NOT run under `-batch'.  So the guard asserts the DEFERRALS
;; differently from the others: it checks the loader WIRED
;; `org-graph--register-extractor' and
;; `org-graph--seed-org-id-locations-deferred' onto `emacs-startup-hook',
;; then drives the extractor function directly with the vulpea DB
;; boundary stubbed (`cl-letf', function-scoped) and asserts the
;; extractor landed.  No live SQLite DB or fswatch is required.
;;
;; Test process note: `make' runs specs via `EMACS_TEST_BATCH', which
;; loads `init.el'.  So `jf/load-module'/`jf/emacs-dir' are defined (the
;; loader's by-path loads work) and workspaces/gptel/org-roam are already
;; loaded -- which is why `workspace-integration's `with-eval-after-load
;; 'workspaces' fires from the loader path into the REAL registry, and the
;; gptel tools register at load.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

;; --- Load-path + module setup ------------------------------------------
;;
;; vulpea/gptel/org-roam build dirs are added defensively (helpers-spec
;; adds vulpea; the per-module specs add compat + gptel).  Under the
;; `make' test runner these are already loaded by `init.el', but adding
;; them keeps the spec robust if run in a leaner process.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir))
       (root (locate-dominating-file test-dir "early-init.el")))
  (add-to-list 'load-path test-dir)
  (when root
    (dolist (pkg '("compat" "gptel" "org-roam"))
      (let ((d (expand-file-name (format "runtime/straight/build/%s" pkg) root)))
        (when (file-directory-p d) (add-to-list 'load-path d)))))
  ;; gptel: so `(fboundp 'gptel-make-tool)' holds and the loader registers
  ;; the tools at load (step 5).  org-roam: so the coexistence assertions
  ;; (step 7) exercise the real org-roam symbols.
  (require 'gptel)
  (require 'org-roam)
  ;; helpers-spec adds vulpea to `load-path' and requires it.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  ;; THE COLD LOAD: require the consolidated loader ALONE.  It fires
  ;; `use-package vulpea' (pins `vulpea-db-location') and load-wires EVERY
  ;; submodule by path in canonical order.  Module load is DB-free (the
  ;; DB-touching extractor registration + discovery seed are deferred to
  ;; `emacs-startup-hook', which does not run under `-batch'), so no DB
  ;; stub is needed at require time.  `require' is idempotent, so this is
  ;; a no-op if `init.el' already loaded org-graph via `jf/enabled-modules'
  ;; -- either way the assertions check the LOADER-produced state.
  (require 'org-graph (expand-file-name "org-graph.el" module-dir)))

(describe "org-graph cold-load guard (D7/D8, RE-5; verifies register/invariant/org-graph-loader-ordered-sequence: (require 'org-graph) ALONE fires every registration from the loader path)"

  ;; --- Step 2: loader defcustoms ---------------------------------------

  (describe "loader defcustoms exist with documented defaults"

    (it "org-graph-vault-root defaults to the whole durable vault"
      (expect (boundp 'org-graph-vault-root) :to-be-truthy)
      (expect org-graph-vault-root :to-equal "~/org/"))

    (it "org-graph-roam-root defaults to the roam concept vault"
      (expect (boundp 'org-graph-roam-root) :to-be-truthy)
      (expect org-graph-roam-root :to-equal "~/org/roam/"))

    (it "org-graph-relation-types is the open-vocabulary completion seed"
      (expect (boundp 'org-graph-relation-types) :to-be-truthy)
      (expect org-graph-relation-types
              :to-equal '(implements contradicts supersedes relates-to)))

    (it "org-graph-edge-drawer defaults to the EDGES drawer"
      (expect (boundp 'org-graph-edge-drawer) :to-be-truthy)
      (expect org-graph-edge-drawer :to-equal "EDGES"))

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

  ;; --- Step 4: typed-edge extractor registration DEFERRED + wired ------

  (describe "typed-edge extractor registration deferred to post-init, wired by the loader"

    ;; The extractor registration opens the DB (it applies the typed_edges
    ;; schema), so the loader does NOT run it at module-load time -- it
    ;; defers it to `emacs-startup-hook' to keep `(require 'org-graph)'
    ;; DB-free.  The guard therefore asserts the loader WIRED the deferral,
    ;; then drives the wired function with the DB boundary stubbed and
    ;; asserts the extractor landed.  (This is the point-C resolution of
    ;; the registration-touches-DB tension.)

    (it "wires both DB-touching deferrals onto emacs-startup-hook (deferred, not eager at load)"
      ;; The loader added the DB-touching registrations to the post-init
      ;; seam instead of calling them at load -- this is the "not run at
      ;; module-load" evidence (the seam does not fire under -batch).
      ;; The loader owns BOTH deferrals (extractor registration + the
      ;; discovery org-id-locations seed) through the shared resilient
      ;; idiom `org-graph--run-deferred-op'; each stays a NAMED function
      ;; on the hook so this membership check (and idempotent re-load)
      ;; keeps working.
      (expect (fboundp 'org-graph--register-extractor) :to-be-truthy)
      (expect (memq #'org-graph--register-extractor emacs-startup-hook)
              :to-be-truthy)
      (expect (fboundp 'org-graph--seed-org-id-locations-deferred)
              :to-be-truthy)
      (expect (memq #'org-graph--seed-org-id-locations-deferred
                    emacs-startup-hook)
              :to-be-truthy))

    (it "org-graph--run-deferred-op logs a failing op as a warning and never signals"
      ;; The resilience contract every deferral inherits: a failing op
      ;; (e.g. missing/unbuilt DB at startup) is reported via
      ;; `display-warning' with the "<what> skipped: <err>" message and
      ;; must NOT propagate an error out of the hook function.
      (let (warnings)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &optional level &rest _)
                     (push (list type message level) warnings))))
          (expect
           (org-graph--run-deferred-op
            (lambda () (error "DB not built"))
            "resilience-spec op")
           :not :to-throw))
        (expect (length warnings) :to-equal 1)
        (pcase-let ((`(,type ,message ,level) (car warnings)))
          (expect type :to-be 'org-graph)
          (expect message :to-match "\\`resilience-spec op skipped: ")
          (expect message :to-match "DB not built")
          (expect level :to-be :warning))))

    (it "the wired deferral registers org-graph-typed-edges carrying the typed_edges schema"
      ;; Drive the deferred entry point directly.  `vulpea-db-register-extractor'
      ;; applies the plugin schema, which opens the DB; stub the DB boundary
      ;; so registration is inert and no SQLite file is touched.  The push
      ;; onto `vulpea-db--extractors' (the registry we inspect) still
      ;; happens for real.
      (cl-letf (((symbol-function 'vulpea-db) (lambda (&rest _) 'stub-db))
                ((symbol-function 'emacsql) (lambda (&rest _) nil)))
        (org-graph--register-extractor))
      (let ((ex (vulpea-db-get-extractor 'org-graph-typed-edges)))
        (expect ex :to-be-truthy)
        (expect (vulpea-extractor-name ex) :to-be 'org-graph-typed-edges)
        (expect (vulpea-extractor-extract-fn ex)
                :to-be #'org-graph-extractor/extract)
        ;; the typed_edges schema is present in the registered set
        (expect (assq 'typed_edges (vulpea-extractor-schema ex))
                :to-be-truthy))))

  ;; --- Step 5: gptel agent tools registered ----------------------------

  (describe "gptel agent tools registered from the loader path"

    ;; gptel is loaded in this process, so `gptel-make-tool' is fbound and
    ;; the loader's load of tools.el registers the tools AT LOAD.  We do
    ;; NOT re-register here: these assertions verify the LOADER fired it.
    ;; (In a gptel-less process the loader skips it and
    ;; `org-graph/agent-tools' is nil -- asserted nil-tolerant in the
    ;; per-module tools-spec.)

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

  ;; --- Step 5a: authoring module loaded (finders -> authoring -> edge-type) --

  (describe "authoring module loaded from the loader path"

    ;; authoring.el (vulpea-human-commands) sits between finders and
    ;; edge-type in the canonical sequence.  DB-free at load: it only
    ;; defines the two interactive thin wrappers over vulpea built-ins
    ;; (the wrapped `vulpea-find'/`vulpea-insert' touch the DB at
    ;; COMMAND time, never at load), so these are definition assertions
    ;; only -- the same pattern as edge-type below.
    (it "defines the two interactive authoring commands"
      (expect (featurep 'org-graph-authoring) :to-be-truthy)
      (dolist (cmd '(org-graph/find-or-create org-graph/insert-link))
        (expect (fboundp cmd) :to-be-truthy)
        (expect (commandp cmd) :to-be-truthy))))

  ;; --- Step 5b: edge-type registry loaded (authoring -> edge-type -> tools) --

  (describe "edge-type registry loaded from the loader path"

    ;; edge-type.el (cycle-1786458912) sits between authoring and tools in
    ;; the canonical sequence; it requires org-graph-extractor for the
    ;; shared rel-normalization helper.  DB-free at load: these are
    ;; definition assertions only (the lookup touches the DB lazily).
    (it "defines the registry selector, lookup, accessor, finder, and seed installer"
      (expect (featurep 'org-graph-edge-type) :to-be-truthy)
      (dolist (fn '(org-graph-edge-type-note-p
                    org-graph/edge-types
                    org-graph/edge-type
                    org-graph/find-edge-type
                    org-graph-edge-type-install-seeds
                    org-graph-edge-type-invalidate-cache))
        (expect (fboundp fn) :to-be-truthy))))

  ;; --- Step 6: workspace integration registered ------------------------

  (describe "workspace integration registered from the loader path"

    ;; `workspace-integration.el' registers via `with-eval-after-load
    ;; 'workspaces'.  The `make' test runner loads `init.el', which loads
    ;; the workspaces module, so that form FIRES when the loader loads
    ;; workspace-integration.el -- registering org-graph in the REAL
    ;; `workspace--integrations' registry.  Asserting that registry proves
    ;; the loader path fired (not a stubbed call).
    (it "registers org-graph in workspace--integrations with :on-create and :menu"
      (expect (featurep 'workspaces) :to-be-truthy)
      (expect (boundp 'workspace--integrations) :to-be-truthy)
      (let* ((cell (assq 'org-graph workspace--integrations))
             (plist (cdr cell)))
        (expect cell :to-be-truthy)
        (expect (plist-get plist :label) :to-equal "org-graph")
        ;; :on-create handler present and fbound
        (expect (plist-get plist :on-create)
                :to-be #'org-graph-workspace-integration--on-create)
        (expect (fboundp 'org-graph-workspace-integration--on-create)
                :to-be-truthy)
        ;; :menu is a (KEY . COMMAND) cons; COMMAND fbound, accepts the
        ;; anchor payload (arity 1)
        (let ((menu (plist-get plist :menu)))
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

  ;; --- Edge-drawer export exclusion (LD-6) -----------------------------

  (describe "edge-drawer export exclusion follows customize renames (LD-6)"

    (it "swaps the drawer name in the loader-managed exclusion shape"
      ;; The setter uses `set-default', so save/restore the default binding
      ;; explicitly instead of let-binding (which set-default would bypass).
      (let ((setter (get 'org-graph-edge-drawer 'custom-set))
            (saved (default-value 'org-graph-edge-drawer))
            (org-export-with-drawers (list 'not "LOGBOOK"
                                           (default-value 'org-graph-edge-drawer))))
        (expect setter :to-be-truthy)
        (unwind-protect
            (progn
              (funcall setter 'org-graph-edge-drawer "RELATIONS")
              (expect org-export-with-drawers
                      :to-equal '(not "LOGBOOK" "RELATIONS"))
              (expect (default-value 'org-graph-edge-drawer)
                      :to-equal "RELATIONS"))
          (set-default 'org-graph-edge-drawer saved))))

    (it "leaves a user-customized exclusion list alone"
      (let ((setter (get 'org-graph-edge-drawer 'custom-set))
            (saved (default-value 'org-graph-edge-drawer))
            (org-export-with-drawers '(not "LOGBOOK" "MYSTUFF")))
        (unwind-protect
            (progn
              (funcall setter 'org-graph-edge-drawer "RELATIONS")
              (expect org-export-with-drawers
                      :to-equal '(not "LOGBOOK" "MYSTUFF")))
          (set-default 'org-graph-edge-drawer saved))))

    (it "warns on a value org cannot parse as a drawer name (LD-1)"
      (let ((setter (get 'org-graph-edge-drawer 'custom-set))
            (saved (default-value 'org-graph-edge-drawer))
            warnings)
        (unwind-protect
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (type message &optional level &rest _)
                         (push (list type message level) warnings))))
              (funcall setter 'org-graph-edge-drawer "BAD NAME!")
              (expect (length warnings) :to-equal 1)
              (expect (nth 0 (car warnings)) :to-be 'org-graph)
              (expect (nth 1 (car warnings))
                      :to-match "not a valid org drawer name")
              ;; A valid name warns nothing.
              (funcall setter 'org-graph-edge-drawer "EDGES")
              (expect (length warnings) :to-equal 1))
          (set-default 'org-graph-edge-drawer saved))))))

  ;; NOTE: the loader-level vulpea DB isolation invariant (resolves under
  ;; runtime state/ as notes.db, != vulpea's default vulpea.db) is owned
  ;; by `config/org-graph/test/db-location-spec.el'; not re-asserted here.

(provide 'module-load-spec)
;;; module-load-spec.el ends here
