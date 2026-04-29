;;; expansion-ui-handlers-spec.el --- Tests for expansion UI handler implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; REAL UI HANDLER TESTS: Expansion UI Implementation
;;
;; These tests verify the ACTUAL expansion UI handler implementation
;; (jf/gptel-scope--deny-expansion, jf/gptel-scope--add-to-scope,
;; jf/gptel-scope--allow-once-action) against drawer-buffer fixtures.
;;
;; What this catches that mocked UI tests don't:
;; - Transient scope storage/retrieval bugs
;; - JSON serialization format changes
;; - Error handling in callback invocation
;; - Handler logic regressions
;; - All user choice paths (deny, approve, allow-once)
;;
;; Migration note: rewritten as part of migrate-expansion-tests
;; (cycle-3) — fixtures moved from scope.yml mocks to drawer-text
;; fixtures via `jf/gptel-test--with-scope-drawer'.  The transient
;; scope is stubbed via `cl-letf' on `transient-scope', and the chat
;; buffer locator is stubbed to return the fixture buffer so the
;; drawer writer mutates the fixture rather than the test runner's
;; current buffer.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope-tool-wrapper.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test Helpers

(defvar handlers--scope-data nil
  "Storage for the stubbed transient scope plist.")

(defun handlers--make-scope (violation-info callback patterns tool-name &optional buffer)
  "Construct a five-key transient-scope plist for testing.
BUFFER defaults to (current-buffer)."
  (list :violation violation-info
        :callback callback
        :patterns patterns
        :tool-name tool-name
        :chat-buffer (or buffer (current-buffer))))

(defmacro handlers--with-stub-scope (scope-form &rest body)
  "Stub transient-scope to return SCOPE-FORM and run BODY.
Also stubs transient-quit-one and the queue pump to no-ops, and
save-buffer to a no-op so the temp fixture buffer (with no file
backing) does not signal."
  (declare (indent 1))
  `(let ((handlers--scope-data ,scope-form))
     (cl-letf (((symbol-function 'transient-scope)
                (lambda (&rest _) handlers--scope-data))
               ((symbol-function 'transient-quit-one)
                (lambda (&rest _) nil))
               ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                (lambda (&rest _) nil))
               ((symbol-function 'save-buffer)
                (lambda (&rest _) nil)))
       ,@body)))

;;; Test Suite

(describe "Expansion UI Handlers (Real Implementation)"

  (describe "Deny action"

    (it "retrieves callback from transient scope and invokes with denial JSON"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (test-callback (lambda (json)
                              (setq callback-invoked t)
                              (setq callback-result json)))
             (violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "not-in-scope"
                              :path "/tmp/outside.txt"
                              :operation :read)))

        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info test-callback
                                    '("/tmp/outside.txt") "read_file_in_scope")
            (jf/gptel-scope--deny-expansion))

          ;; Verify callback was invoked
          (expect callback-invoked :to-be t)

          ;; Verify REAL JSON format from REAL handler
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be nil)
            (expect (plist-get parsed :user_denied) :to-be t)
            (expect (plist-get parsed :message) :to-equal "User denied scope expansion request.")))))

    (it "handles missing callback gracefully"
      (let ((violation-info (list :tool "test" :resource "test")))
        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info nil '("test") "test")
            ;; Should not crash, just warn
            (expect (jf/gptel-scope--deny-expansion) :not :to-throw)))))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Boom!")))
             (violation-info (list :tool "test" :resource "test")))
        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info evil-callback '("test") "test")
            ;; Should catch error and not propagate
            (expect (jf/gptel-scope--deny-expansion) :not :to-throw))))))

  (describe "Add to scope action"

    (it "writes path to drawer and invokes callback with success JSON"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (helpers-spec--make-violation-info
                              "write_file_in_scope" "not-in-scope"
                              :path "/workspace/newfile.txt"
                              :operation :write)))

        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info test-callback
                                    '("/workspace/newfile.txt")
                                    "write_file_in_scope"
                                    (current-buffer))
            (jf/gptel-scope--add-to-scope))

          ;; Drawer was mutated
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/workspace/newfile.txt"))

          ;; Verify REAL JSON from REAL handler
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (vectorp (plist-get parsed :patterns_added)) :to-be t)
            (expect (length (plist-get parsed :patterns_added)) :to-equal 1)
            (expect (plist-get parsed :message) :to-match "Scope expanded")))))

    (it "handles bash validation type with a path-shaped resource"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (helpers-spec--make-violation-info
                              "run_bash_command" "not-in-scope"
                              :path "/etc/hosts"
                              :operation :read)))
        ;; helpers-spec--make-violation-info infers :validation-type
        ;; from the tool name; for run_bash_command that is 'bash.
        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info test-callback
                                    '("/etc/hosts") "run_bash_command"
                                    (current-buffer))
            (jf/gptel-scope--add-to-scope))

          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/etc/hosts"))

          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)))))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Callback explosion!")))
             (violation-info (helpers-spec--make-violation-info
                              "write_file_in_scope" "not-in-scope"
                              :path "/workspace/file.txt"
                              :operation :write)))
        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info evil-callback
                                    '("/workspace/file.txt")
                                    "write_file_in_scope"
                                    (current-buffer))
            ;; Should not propagate error (handler catches it via safe-callback)
            (expect (jf/gptel-scope--add-to-scope) :not :to-throw))))))

  (describe "Allow once action"

    (it "invokes callback with success + allowed_once flag"
      (let* ((callback-result nil)
             (test-callback (lambda (json) (setq callback-result json)))
             (violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "not-in-scope"
                              :path "/tmp/temp.txt"
                              :operation :read)))

        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info test-callback
                                    '("/tmp/temp.txt") "read_file_in_scope")
            (jf/gptel-scope--allow-once-action))

          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (plist-get parsed :allowed_once) :to-be t)
            (expect (plist-get parsed :message)
                    :to-equal "Permission granted for this invocation only.")))))

    (it "handles callback invocation errors gracefully"
      (let* ((evil-callback (lambda (_) (error "Boom!")))
             (violation-info (list :tool "read_file_in_scope"
                                   :resource "/tmp/test.txt"
                                   :validation-type 'path)))
        (jf/gptel-test--with-scope-drawer '()
          (handlers--with-stub-scope
              (handlers--make-scope violation-info evil-callback
                                    '("/tmp/test.txt") "read_file_in_scope")
            (expect (jf/gptel-scope--allow-once-action) :not :to-throw))))))

  (describe "Scope setup function"

    (it "creates transient scope with five-key well-formed plist"
      ;; Asserts register/shape/expansion-transient-scope: the plist
      ;; passed via :scope to transient-setup contains exactly the
      ;; five required keys.
      (let* ((test-callback (lambda (json) json))
             (violation-info (helpers-spec--make-violation-info
                              "read_file_in_scope" "not-in-scope"
                              :path "/tmp/test.txt"
                              :operation :read))
             (patterns '("/tmp/test.txt"))
             (tool-name "read_file_in_scope")
             (captured-scope nil))

        ;; Reset the queue/active state so prompt-expansion takes the
        ;; "show transient" branch rather than the "queue" branch.
        (setq jf/gptel-scope--expansion-queue nil)
        (setq jf/gptel-scope--expansion-active nil)

        ;; Mock transient-setup to capture the scope keyword arg
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (_prefix _ignored1 _ignored2 &rest args)
                     (setq captured-scope (plist-get args :scope)))))
          (jf/gptel-scope-prompt-expansion violation-info test-callback patterns tool-name))

        ;; Verify the five-key shape
        (expect (plist-get captured-scope :violation) :to-equal violation-info)
        (expect (plist-get captured-scope :callback) :to-equal test-callback)
        (expect (plist-get captured-scope :patterns) :to-equal patterns)
        (expect (plist-get captured-scope :tool-name) :to-equal tool-name)
        (expect (bufferp (plist-get captured-scope :chat-buffer)) :to-be t)
        (expect (buffer-live-p (plist-get captured-scope :chat-buffer)) :to-be t)

        ;; Cleanup
        (setq jf/gptel-scope--expansion-active nil)))))

(provide 'expansion-ui-handlers-spec)
;;; expansion-ui-handlers-spec.el ends here
