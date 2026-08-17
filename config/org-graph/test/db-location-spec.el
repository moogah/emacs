;;; db-location-spec.el --- vulpea DB isolation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Enforcement spec for `register/invariant/vulpea-db-isolation' (design
;; D8): the org-graph vulpea index DB MUST live under the worktree
;; runtime `state/' directory (`runtime/state/vulpea/notes.db'), isolated
;; from org-roam and from vulpea's default (`runtime/vulpea.db').  This is
;; the spike's clean-rollback property -- wiping/rebuilding the org-graph
;; DB never touches org-roam's DB or the `org-id-locations' cache.
;;
;; The loader (`org-graph.org') pins `vulpea-db-location' in the vulpea
;; `use-package' `:config'.  Requiring the loader runs that `:config'
;; (vulpea is already on `load-path' / loaded via the shared helpers, and
;; `init.el' is loaded in the batch test process so `use-package' and
;; `user-emacs-directory' are live).  `user-emacs-directory' resolves to
;; this worktree's `runtime/' dir (see `early-init.el'), so the asserted
;; path is worktree-isolated.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Pull in the shared helpers (adds vulpea to `load-path' and requires
;; it), then load the loader under test, which sets `vulpea-db-location'.
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph (expand-file-name "org-graph.el" module-dir)))

(describe "vulpea DB isolation (D8 / register/invariant/vulpea-db-isolation)"
  (it "sets vulpea-db-location to a concrete path"
    (expect (boundp 'vulpea-db-location) :to-be-truthy)
    (expect (stringp vulpea-db-location) :to-be-truthy))

  (it "resolves the DB under the runtime state/ dir, not vulpea's default"
    (let ((state-dir (expand-file-name "state" user-emacs-directory)))
      ;; Under runtime/state/ ...
      (expect (file-in-directory-p vulpea-db-location state-dir)
              :to-be-truthy)
      ;; ... specifically the isolated vulpea subdir + db name ...
      (expect (file-name-nondirectory vulpea-db-location)
              :to-equal "notes.db")
      (expect (file-name-nondirectory
               (directory-file-name (file-name-directory vulpea-db-location)))
              :to-equal "vulpea")
      ;; ... and NOT vulpea's default (directly under runtime/).
      (expect vulpea-db-location
              :not :to-equal (expand-file-name "vulpea.db" user-emacs-directory))))

  (it "creates the DB's parent directory so vulpea need not"
    (expect (file-directory-p (file-name-directory vulpea-db-location))
            :to-be-truthy)))

;;; db-location-spec.el ends here
