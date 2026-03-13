;;; operation-mapping-spec.el --- Unit tests for operation → scope-section mapping -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Unit tests for jf/gptel-scope--map-operation-to-scope-section.
;;
;; The mapping function is the canonical source of truth for translating
;; granular bash-parser operation types (11 total) into the three high-level
;; scope plan sections used by scope.yml (paths.read, paths.write,
;; paths.execute).
;;
;; Rule: Every place in gptel that needs to route by operation type must use
;; this function — not ad-hoc tool-category lookups.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (sessions-dir (expand-file-name "sessions" gptel-dir)))
  (require 'gptel-session-constants (expand-file-name "constants.el" sessions-dir))
  (require 'gptel-session-logging (expand-file-name "logging.el" sessions-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir)))

;;; Tests

(describe "jf/gptel-scope--map-operation-to-scope-section"

  (describe "read-like operations → :read"
    (it "maps :read to :read"
      (expect (jf/gptel-scope--map-operation-to-scope-section :read)
              :to-equal :read))

    (it "maps :read-directory to :read"
      (expect (jf/gptel-scope--map-operation-to-scope-section :read-directory)
              :to-equal :read))

    (it "maps :read-metadata to :read"
      (expect (jf/gptel-scope--map-operation-to-scope-section :read-metadata)
              :to-equal :read))

    (it "maps :match-pattern to :read"
      (expect (jf/gptel-scope--map-operation-to-scope-section :match-pattern)
              :to-equal :read)))

  (describe "write-like operations → :write"
    (it "maps :write to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :write)
              :to-equal :write))

    (it "maps :create to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :create)
              :to-equal :write))

    (it "maps :create-or-modify to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :create-or-modify)
              :to-equal :write))

    (it "maps :append to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :append)
              :to-equal :write))

    (it "maps :delete to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :delete)
              :to-equal :write))

    (it "maps :modify to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :modify)
              :to-equal :write)))

  (describe "execute-like operations → :execute"
    (it "maps :execute to :execute"
      (expect (jf/gptel-scope--map-operation-to-scope-section :execute)
              :to-equal :execute)))

  (describe "unknown operations → :write (fail-safe)"
    (it "maps an unknown keyword to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section :unknown-op)
              :to-equal :write))

    (it "maps nil to :write"
      (expect (jf/gptel-scope--map-operation-to-scope-section nil)
              :to-equal :write))))

(provide 'operation-mapping-spec)

;;; operation-mapping-spec.el ends here
