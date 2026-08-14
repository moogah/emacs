;;; authoring-spec.el --- Authoring command delegation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Delegation tests for the org-graph human-side authoring commands
;; (authoring-module task, design D3 / D9,
;; `register/boundary/authoring-commands-delegation').  The commands are
;; THIN wrappers over vulpea 2.4 built-ins -- no custom completion,
;; creation, or link-insertion logic -- so these specs assert exactly
;; that: each command hands off to its vulpea entry point with the right
;; arguments, and nothing more.  The wrapped behavior (create-on-miss,
;; synchronous birth-index, region-as-description) is vulpea's own,
;; covered by vulpea itself and the runbook's live checks.
;;
;; Pattern: spy on `vulpea-find' / `vulpea-insert' with a local
;; `cl-letf' capturing the args (mirroring finders-spec.el), so no
;; completion UI or DB access occurs.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (module-dir (expand-file-name ".." test-dir)))
  (add-to-list 'load-path test-dir)
  ;; helpers-spec adds vulpea to `load-path' and requires it, which
  ;; authoring.el's hard `(require 'vulpea)' needs in a lean process.
  (require 'org-graph-test-helpers (expand-file-name "helpers-spec.el" test-dir))
  (require 'org-graph-authoring (expand-file-name "authoring.el" module-dir)))

(describe "org-graph authoring commands (thin vulpea wrappers)"

  (describe "org-graph/find-or-create"

    (it "is an interactive command"
      (expect (commandp #'org-graph/find-or-create) :to-be-truthy))

    (it "delegates to vulpea-find with :require-match nil"
      (let* ((capture (org-graph-test/capture-call
                       'vulpea-find #'org-graph/find-or-create))
             (args (cdr capture)))
        (expect (car capture) :to-be-truthy)
        (expect (plist-member args :require-match) :to-be-truthy)
        (expect (plist-get args :require-match) :to-be nil)))

    (it "passes no :filter-fn -- completion covers every indexed note"
      (let ((args (cdr (org-graph-test/capture-call
                        'vulpea-find #'org-graph/find-or-create))))
        (expect (plist-member args :filter-fn) :to-be nil))))

  (describe "org-graph/insert-link"

    (it "is an interactive command"
      (expect (commandp #'org-graph/insert-link) :to-be-truthy))

    (it "delegates to vulpea-insert with no arguments"
      (let ((capture (org-graph-test/capture-call
                      'vulpea-insert #'org-graph/insert-link)))
        (expect (car capture) :to-be-truthy)
        (expect (cdr capture) :to-be nil)))))

;;; authoring-spec.el ends here
