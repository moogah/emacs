;;; add-bash-to-scope-routing-spec.el --- add-bash-to-scope routes by error type, not string content -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; UNIT TESTS for add-bash-to-scope routing logic against the drawer
;; writer.
;;
;; Background: an earlier version routed by checking
;; (string-match-p "/" resource), which mishandled composite keys
;; like "which brew:/" and bare command names that contain neither
;; a slash nor a glob.  The current implementation routes by shape:
;;
;;   - Path-shaped (absolute, tilde, glob, directory) → delegates to
;;     `jf/gptel-scope--add-path-to-scope', which writes to the drawer
;;     key matching the denied operation.
;;   - Bare command names (no slash, no glob) → returns nil and emits a
;;     user message ("not expandable in the operation-first model").
;;
;; Migration note: this spec was rewritten as part of
;; migrate-expansion-tests (cycle-3) — fixtures moved from scope.yml
;; files to drawer-text fixtures via `jf/gptel-test--with-scope-drawer'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Tests

(describe "add-bash-to-scope routing"

  (describe "with file path resource (path_out_of_scope scenario)"

    (it "routes a clean file path to the drawer key matching the operation"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-bash-to-scope
           "/etc/passwd" "run_bash_command" :read)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/etc/passwd"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))

    (it "respects :read-metadata routing when the bash op was a stat"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-bash-to-scope
           "/brew" "run_bash_command" :read-metadata)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ_METADATA")
                  :to-equal '("/brew"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal nil)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))

    (it "writes a glob pattern as a path"
      ;; Globs (containing * or ?) are path-shaped per the routing
      ;; predicate, so they delegate to add-path-to-scope.
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-bash-to-scope
           "*.txt" "run_bash_command" :read)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("*.txt"))))))

  (describe "with bare command name (not path-shaped)"

    (it "returns nil for a bare command (no slash, no glob)"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (let ((result (jf/gptel-scope--add-bash-to-scope
                         "brew" "run_bash_command" :execute)))
            ;; Bare command names are not expandable; the writer
            ;; signals "no-op" by returning nil.
            (expect result :to-be nil)
            ;; And it must NOT have written anything to any drawer key.
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_READ")
                    :to-equal nil)
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_WRITE")
                    :to-equal nil)
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_EXECUTE")
                    :to-equal nil)))))))

(provide 'add-bash-to-scope-routing-spec)

;;; add-bash-to-scope-routing-spec.el ends here
