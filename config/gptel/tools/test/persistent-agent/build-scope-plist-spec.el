;;; build-scope-plist-spec.el --- read/write scope-plist split tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs pinning the read/write split of
;; `jf/gptel-persistent-agent--build-scope-plist'.  After the
;; `agent-build-scope-plist-split' task the helper takes READ-PATHS and
;; WRITE-PATHS separately: `:read' is the supplied read-paths verbatim,
;; `:write' is the supplied write-paths with `/tmp/**' appended as a
;; guaranteed scratch grant (NOT the write default), and `:deny' is the
;; standard deny set.
;;
;; The returned plist preserves `register/shape/scope-config-plist'
;; shape: (:paths (:read ... :write ... :deny ...)).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tools-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path tools-dir)
  (require 'gptel-persistent-agent
           (expand-file-name "persistent-agent.el" tools-dir)))

(describe "jf/gptel-persistent-agent--build-scope-plist"

  ;; Scenario: read+write supplied -> :read verbatim, :write + /tmp scratch,
  ;; :deny standard set.
  (it "splits read and write paths and appends /tmp scratch to write"
    (let* ((plist (jf/gptel-persistent-agent--build-scope-plist
                   '("/p/**") '("/p/**")))
           (paths (plist-get plist :paths)))
      (expect (plist-get paths :read) :to-equal '("/p/**"))
      (expect (plist-get paths :write) :to-equal '("/p/**" "/tmp/**"))
      (expect (plist-get paths :deny)
              :to-equal jf/gptel-persistent-agent--standard-deny-paths)))

  ;; Scenario: write-paths nil -> :write is scratch-only.
  (it "yields scratch-only write when write-paths is nil"
    (let* ((plist (jf/gptel-persistent-agent--build-scope-plist
                   '("/p/**") nil))
           (paths (plist-get plist :paths)))
      (expect (plist-get paths :write) :to-equal '("/tmp/**"))))

  ;; Scenario: read-paths nil -> :read is nil (no read access).
  (it "yields nil read when read-paths is nil"
    (let* ((plist (jf/gptel-persistent-agent--build-scope-plist
                   nil '("/p/**")))
           (paths (plist-get plist :paths)))
      (expect (plist-get paths :read) :to-equal nil)))

  ;; Shape invariant: register/shape/scope-config-plist is preserved —
  ;; top-level :paths key wrapping a :read/:write/:deny inner plist.
  (it "preserves the scope-config-plist shape"
    (let ((plist (jf/gptel-persistent-agent--build-scope-plist
                  '("/p/**") '("/p/**"))))
      (expect (plist-member plist :paths) :not :to-be nil)
      (let ((paths (plist-get plist :paths)))
        (expect (plist-member paths :read) :not :to-be nil)
        (expect (plist-member paths :write) :not :to-be nil)
        (expect (plist-member paths :deny) :not :to-be nil))))

  ;; Non-mutation guard: `:write' is built with `(append write-paths
  ;; '("/tmp/**"))', which copies and must NOT destructively extend the
  ;; caller's list.  Pins the contract so a future refactor to `nconc'
  ;; (which would silently append /tmp/** onto a caller-reused list)
  ;; fails loudly here.
  (it "does not mutate the caller's write-paths list"
    (let ((wp (list "/p/**")))
      (jf/gptel-persistent-agent--build-scope-plist nil wp)
      (expect wp :to-equal '("/p/**")))))

;;; build-scope-plist-spec.el ends here
