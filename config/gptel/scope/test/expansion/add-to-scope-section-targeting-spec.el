;;; add-to-scope-section-targeting-spec.el --- add-path-to-scope drawer-key targeting by denied operation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Specifies how `jf/gptel-scope--add-path-to-scope' picks which
;; `:GPTEL_SCOPE_*' drawer key to write to, based on the
;; DENIED-OPERATION keyword passed from the validation pipeline.
;;
;; Historical context: an earlier version of add-path-to-scope chose
;; the target section from a tool-name registry
;; (jf/gptel-scope--tool-categories).  That made run_bash_command
;; always target paths.write, even when the denial was a read-metadata
;; failure on a sub-command.  The fix: drive the section choice from
;; the actual denied operation in the validation error, not from the
;; tool name.  These tests lock that behaviour in.
;;
;; Migration note: this spec was rewritten as part of
;; migrate-expansion-tests (cycle-3) — fixtures moved from scope.yml
;; files to drawer-text fixtures via `jf/gptel-test--with-scope-drawer'.
;; Cycle-2 ask 10A added a dedicated `GPTEL_SCOPE_READ_METADATA' bucket;
;; ask 10C kept `:delete' under `GPTEL_SCOPE_WRITE'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Tests

(describe "add-path-to-scope drawer-key targeting"

  (describe "with :read-family operations"

    (it ":read targets GPTEL_SCOPE_READ"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope "/brew" "run_bash_command" :read)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/brew"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))

    (it ":read-metadata targets GPTEL_SCOPE_READ_METADATA (cycle-2 ask 10A)"
      ;; Cycle-2 disposition (ask 10A): a metadata stat does not silently
      ;; grant persistent content read access.  The bucket is separate.
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope "/brew" "run_bash_command" :read-metadata)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ_METADATA")
                  :to-equal '("/brew"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal nil)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))

    (it ":read-directory targets GPTEL_SCOPE_READ"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/usr/local" "run_bash_command" :read-directory)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/usr/local"))))))

  (describe "with :write-family operations"

    (it ":write targets GPTEL_SCOPE_WRITE"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/tmp/output.txt" "run_bash_command" :write)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/tmp/output.txt"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal nil))))

    (it ":create targets GPTEL_SCOPE_WRITE"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/tmp/new.txt" "write_file_in_scope" :create)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/tmp/new.txt")))))

    (it ":delete targets GPTEL_SCOPE_WRITE (cycle-2 ask 10C disposition)"
      ;; Cycle-2 disposition: :delete intentionally collapses to
      ;; GPTEL_SCOPE_WRITE rather than its own bucket.  Granting WRITE
      ;; already covers delete.
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/tmp/old.txt" "run_bash_command" :delete)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/tmp/old.txt"))
          ;; No separate :delete bucket — must NOT exist.
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_DELETE")
                  :to-equal nil))))

    (it ":modify targets GPTEL_SCOPE_MODIFY"
      ;; :modify gets its own drawer key (it is NOT folded into WRITE
      ;; in the drawer-key vocabulary, even though the legacy YAML
      ;; layout folded it into paths.write).
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/tmp/config.el" "edit_file_in_scope" :modify)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_MODIFY")
                  :to-equal '("/tmp/config.el"))))))

  (describe "with :execute"

    (it ":execute targets GPTEL_SCOPE_EXECUTE"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/workspace/scripts/run.sh" "run_bash_command" :execute)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_EXECUTE")
                  :to-equal '("/workspace/scripts/run.sh"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal nil)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil)))))

  (describe "without denied-operation (defaults to :read)"

    (it "defaults to GPTEL_SCOPE_READ (the safest default)"
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (jf/gptel-scope--add-path-to-scope
           "/outside/file.txt" "run_bash_command")
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/outside/file.txt"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))))

(provide 'add-to-scope-section-targeting-spec)

;;; add-to-scope-section-targeting-spec.el ends here
