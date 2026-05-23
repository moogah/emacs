;;; expansion-ui-spec.el --- Scope expansion UI behavioral tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; SCOPE EXPANSION UI BEHAVIORAL TESTS
;;
;; Consolidated tests for expansion UI mechanics:
;; - Transient menu triggering and action handlers
;; - Drawer writer routing for read/write/execute/modify operations
;; - Inline expansion trigger and approval flow
;; - Expansion prompt display and user choices
;; - Allow-once permission lifecycle through expansion UI
;; - Drawer writer preserves-structure invariants (idempotency,
;;   key preservation, bare-vs-`+:' form)
;; - Cycle-2 ask 10A coverage: :read-metadata routes to its own bucket
;; - Cycle-2 ask 10C coverage: :delete routes to GPTEL_SCOPE_WRITE
;; - register/shape/expansion-transient-scope (five-key plist shape)
;; - register/invariant/expansion-queue-always-progresses (queue drains)
;;
;; Migration note: rewritten as part of migrate-expansion-tests
;; (cycle-3) — fixtures moved from scope.yml mocks to drawer-text
;; fixtures via `jf/gptel-test--with-scope-drawer'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Helper Functions

(defun expansion-ui-spec--make-scope (violation callback patterns tool-name &optional buffer)
  "Construct a five-key transient-scope plist for testing."
  (list :violation violation
        :callback callback
        :patterns patterns
        :tool-name tool-name
        :chat-buffer (or buffer (current-buffer))))

(defmacro expansion-ui-spec--with-stub-scope (scope-form &rest body)
  "Run BODY with `transient-scope' stubbed to return SCOPE-FORM."
  (declare (indent 1))
  `(let ((scope ,scope-form))
     (cl-letf (((symbol-function 'transient-scope)
                (lambda (&rest _) scope))
               ((symbol-function 'transient-quit-one)
                (lambda (&rest _) nil))
               ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                (lambda (&rest _) nil))
               ((symbol-function 'save-buffer)
                (lambda (&rest _) nil)))
       ,@body)))

;;; ============================================================
;;; Inline expansion (trigger / approval / denial)
;;; ============================================================

(describe "Inline scope expansion: bash command workflows"

  (before-each
    (helpers-spec-setup-session)
    (helpers-spec-setup-bash-mocks))

  (after-each
    (helpers-spec-teardown-bash-mocks)
    (helpers-spec-teardown-session))

  (describe "Semantic validation: in-scope commands"

    (it "validates read operation when path is in read scope"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /workspace/README.md")
             (directory "/workspace"))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/workspace/README.md" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))
          (expect result :to-be nil)))))

  (describe "Semantic validation: out-of-scope commands"

    (it "denies read operation when path is out of scope"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /tmp/secret.txt")
             (directory "/workspace"))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/secret.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((result (jf/gptel-scope--validate-command-semantics
                       command directory scope-config)))
          (expect result :not :to-be nil)
          (expect (plist-get result :error) :to-equal "not-in-scope")
          (expect (plist-get result :resource) :to-equal "/tmp/secret.txt")
          (expect (plist-get result :operation) :to-equal :read)))))

  (describe "Inline expansion: trigger and approval flow"

    (it "user approves with add-to-scope: wrapper callback receives approval"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (expect validation-error :not :to-be nil)

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :patterns_added (vector "/tmp/data.txt")
                                    :message "Added to scope permanently")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be t))))

    (it "user approves with allow-once: wrapper callback receives approval"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "cat /tmp/data.txt")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("cat") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :read "/tmp/data.txt" :command-name "cat"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (expect validation-error :not :to-be nil)

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success t
                                    :allowed_once t
                                    :message "Allowed for this invocation")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be t))))

    (it "user denies: wrapper callback receives denial"
      (let* ((scope-config (helpers-spec-make-minimal-scope-config))
             (command "rm /etc/hosts")
             (directory "/workspace")
             (expansion-ui-called nil)
             (wrapper-callback-result nil))

        (helpers-spec-mock-bash-parse command '("rm") t)
        (helpers-spec-mock-bash-semantics
         (list (helpers-spec--make-file-op :write "/etc/hosts" :command-name "rm"))
         nil
         '(:ratio 1.0))

        (let ((validation-error
               (append (jf/gptel-scope--validate-command-semantics
                        command directory scope-config)
                       (list :validation-type 'bash))))

          (spy-on 'jf/gptel-scope-prompt-expansion
                  :and-call-fake
                  (lambda (_violation-info callback _patterns _tool-name)
                    (setq expansion-ui-called t)
                    (funcall callback
                             (json-serialize
                              (list :success nil
                                    :user_denied t
                                    :message "User denied request")))))

          (jf/gptel-scope--trigger-inline-expansion
           validation-error "run_bash_command"
           (lambda (expansion-result)
             (setq wrapper-callback-result expansion-result)))

          (expect expansion-ui-called :to-be t)
          (expect (plist-get wrapper-callback-result :approved) :to-be nil)
          (expect (plist-get wrapper-callback-result :reason) :to-equal "user_denied")))))

  (describe "Transient action handlers (bash)"

    (it "add-to-scope action mutates drawer and invokes callback with success"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result)))
             (violation (list :tool "run_bash_command"
                              :resource "/tmp/data.txt"
                              :operation :read
                              :validation-type 'bash
                              :reason "test")))

        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/tmp/data.txt") "run_bash_command"
               (current-buffer))
            (jf/gptel-scope--add-to-scope))

          (expect callback-invoked :to-be t)
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/tmp/data.txt"))

          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (plist-get parsed :patterns_added) :not :to-be nil)))))

    (it "deny action invokes callback with user_denied result"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result)))
             (violation (list :tool "run_bash_command"
                              :resource "/tmp/file.txt"
                              :operation :read
                              :validation-type 'bash
                              :reason "test")))

        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/tmp/**") "run_bash_command")
            (jf/gptel-scope--deny-expansion))

          (expect callback-invoked :to-be t)
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (or (eq (plist-get parsed :success) :json-false)
                        (eq (plist-get parsed :success) nil)) :to-be t)
            (expect (plist-get parsed :user_denied) :to-be t)))))

    (it "allow-once action invokes callback with success + allowed_once flag"
      (let* ((callback-invoked nil)
             (callback-result nil)
             (mock-callback (lambda (result)
                              (setq callback-invoked t)
                              (setq callback-result result)))
             (violation (list :tool "run_bash_command"
                              :resource "/tmp/file.txt"
                              :operation :read
                              :validation-type 'bash
                              :reason "test")))

        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/tmp/file.txt") "run_bash_command")
            (jf/gptel-scope--allow-once-action))

          (expect callback-invoked :to-be t)
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (plist-get parsed :allowed_once) :to-be t)))))))

;;; ============================================================
;;; Filesystem-tool action handler operation routing
;;; ============================================================

(describe "Transient action handlers (filesystem tools)"

  (describe "Add-to-scope action — operation routing"

    (it "routes :read operation to GPTEL_SCOPE_READ"
      (let* ((mock-callback (lambda (_) nil))
             (violation (list :tool "read_file_in_scope"
                              :resource "/home/user/data.txt"
                              :operation :read
                              :validation-type 'path
                              :reason "test")))
        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/home/user/data.txt")
               "read_file_in_scope")
            (jf/gptel-scope--add-to-scope))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal '("/home/user/data.txt"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal nil))))

    (it "routes :write operation to GPTEL_SCOPE_WRITE"
      (let* ((mock-callback (lambda (_) nil))
             (violation (list :tool "write_file_in_scope"
                              :resource "/home/user/output.txt"
                              :operation :write
                              :validation-type 'path
                              :reason "test")))
        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/home/user/output.txt")
               "write_file_in_scope")
            (jf/gptel-scope--add-to-scope))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/home/user/output.txt"))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_READ")
                  :to-equal nil))))

    (it "routes edit-file (:write) to GPTEL_SCOPE_WRITE"
      (let* ((mock-callback (lambda (_) nil))
             (violation (list :tool "edit_file_in_scope"
                              :resource "/home/user/code.el"
                              :operation :write
                              :validation-type 'path
                              :reason "test")))
        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/home/user/code.el")
               "edit_file_in_scope")
            (jf/gptel-scope--add-to-scope))
          (expect (org-entry-get-multivalued-property
                   (point-min) "GPTEL_SCOPE_WRITE")
                  :to-equal '("/home/user/code.el"))))))

  (describe "Deny action"

    (it "returns user_denied when user denies request"
      (let* ((callback-result nil)
             (mock-callback (lambda (result) (setq callback-result result)))
             (violation (list :tool "read_file_in_scope"
                              :resource "/etc/passwd"
                              :operation :read
                              :validation-type 'path
                              :reason "test")))
        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/etc/passwd") "read_file_in_scope")
            (jf/gptel-scope--deny-expansion))
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (or (eq (plist-get parsed :success) :json-false)
                        (eq (plist-get parsed :success) nil)) :to-be t)
            (expect (plist-get parsed :user_denied) :to-be t))))))

  (describe "Allow-once action"

    (it "callback receives success + allowed_once when user allows once"
      (let* ((callback-result nil)
             (mock-callback (lambda (result) (setq callback-result result)))
             (violation (list :tool "read_file_in_scope"
                              :resource "/tmp/temp.txt"
                              :operation :read
                              :validation-type 'path
                              :reason "test")))
        (jf/gptel-test--with-scope-drawer '()
          (expansion-ui-spec--with-stub-scope
              (expansion-ui-spec--make-scope
               violation mock-callback '("/tmp/temp.txt") "read_file_in_scope")
            (jf/gptel-scope--allow-once-action))
          (let ((parsed (json-parse-string callback-result :object-type 'plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (plist-get parsed :allowed_once) :to-be t)))))))

;;; ============================================================
;;; Operation-specific routing through the action handler
;;; ============================================================
;;;
;;; These tests exercise the live --add-to-scope action handler with
;;; different :operation keywords and assert that the drawer key the
;;; pattern lands in matches the operation, per
;;; register/vocabulary/operation-to-drawer-key.

(describe "--add-to-scope routing by :operation (action-handler level)"

  (it "routes :read-metadata to GPTEL_SCOPE_READ_METADATA (cycle-2 ask 10A)"
    ;; Asserts ask 10A at the action-handler level: a metadata stat
    ;; (e.g., `which python3') does not silently grant persistent
    ;; content read access.  The pattern lands in
    ;; GPTEL_SCOPE_READ_METADATA, leaving GPTEL_SCOPE_READ untouched.
    (let* ((mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource "/etc/passwd"
                            :operation :read-metadata
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback '("/etc/passwd") "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-to-scope))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ_METADATA")
                :to-equal '("/etc/passwd"))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal nil))))

  (it "routes :delete to GPTEL_SCOPE_WRITE (cycle-2 ask 10C)"
    ;; Asserts ask 10C at the action-handler level: :delete
    ;; intentionally collapses to GPTEL_SCOPE_WRITE rather than its
    ;; own bucket — granting WRITE already covers delete.
    (let* ((mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource "/tmp/x"
                            :operation :delete
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback '("/tmp/x") "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-to-scope))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_WRITE")
                :to-equal '("/tmp/x"))
        ;; No separate :delete bucket may have been written.
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_DELETE")
                :to-equal nil))))

  (it "routes :read-directory to GPTEL_SCOPE_READ"
    (let* ((mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource "/usr/bin/python3"
                            :operation :read-directory
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback '("/usr/bin/python3") "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-to-scope))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/usr/bin/python3"))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_WRITE")
                :to-equal nil))))

  (it "routes :write to GPTEL_SCOPE_WRITE"
    (let* ((mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource "/tmp/output.txt"
                            :operation :write
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback '("/tmp/output.txt") "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-to-scope))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_WRITE")
                :to-equal '("/tmp/output.txt"))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal nil)))))

;;; ============================================================
;;; Wildcard helper and add-wildcard-to-scope
;;; ============================================================

(describe "jf/gptel-scope--parent-wildcard-for"

  (it "returns parent dir with /** for a file path"
    (expect (jf/gptel-scope--parent-wildcard-for "~/foo/bar.txt")
            :to-equal "~/foo/**"))

  (it "handles deeply nested file paths"
    (expect (jf/gptel-scope--parent-wildcard-for "/a/b/src/init.el")
            :to-equal "/a/b/src/**"))

  (it "normalizes trailing slash in parent dir"
    (expect (jf/gptel-scope--parent-wildcard-for "/home/user/projects/foo.txt")
            :to-equal "/home/user/projects/**")))

(describe "Transient action handlers — wildcard add-to-scope"

  (it "writes parent-directory wildcard to the matching drawer key"
    (let* ((resource "/home/user/data.txt")
           (mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource resource
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-wildcard-to-scope))
        ;; The wildcard /home/user/** lands in GPTEL_SCOPE_READ; the
        ;; raw resource /home/user/data.txt does NOT.
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/home/user/**")))))

  (it "callback receives derived wildcard pattern in patterns_added"
    (let* ((resource "/home/user/data.txt")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result)))
           (violation (list :tool "run_bash_command"
                            :resource resource
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "run_bash_command"
             (current-buffer))
          (jf/gptel-scope--add-wildcard-to-scope))
        (let* ((parsed (json-parse-string callback-result :object-type 'plist))
               (patterns-added (plist-get parsed :patterns_added)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (aref patterns-added 0) :to-equal "/home/user/**")))))

  (it "validation-type 'path also routes the wildcard via the writer"
    (let* ((resource "/home/user/data.txt")
           (mock-callback (lambda (_) nil))
           (violation (list :tool "read_file_in_scope"
                            :resource resource
                            :operation :read
                            :validation-type 'path
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "read_file_in_scope"
             (current-buffer))
          (jf/gptel-scope--add-wildcard-to-scope))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/home/user/**"))))))

;;; ============================================================
;;; Custom-pattern add-to-scope
;;; ============================================================

(describe "Transient action handlers — custom pattern add-to-scope"

  (it "pre-populates read-string prompt with denied resource"
    (let* ((resource "/home/user/data.txt")
           (read-string-initial nil)
           (mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource resource
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "run_bash_command"
             (current-buffer))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt &optional initial &rest _args)
                       (setq read-string-initial initial)
                       initial)))
            (jf/gptel-scope--add-custom-to-scope)))
        (expect read-string-initial :to-equal resource))))

  (it "writes edited custom pattern to the drawer when confirmed"
    (let* ((resource "/home/user/data.txt")
           (custom-pattern "/home/user/**")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result)))
           (violation (list :tool "run_bash_command"
                            :resource resource
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "run_bash_command"
             (current-buffer))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt &optional _initial &rest _args)
                       custom-pattern)))
            (jf/gptel-scope--add-custom-to-scope)))

        ;; Drawer was mutated with the edited pattern
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal (list custom-pattern))

        ;; Callback received success with the edited pattern
        (let* ((parsed (json-parse-string callback-result :object-type 'plist))
               (patterns-added (plist-get parsed :patterns_added)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (aref patterns-added 0) :to-equal custom-pattern)))))

  (it "invokes callback with user_denied when prompt cancelled"
    (let* ((resource "/home/user/data.txt")
           (callback-result nil)
           (mock-callback (lambda (result) (setq callback-result result)))
           (violation (list :tool "run_bash_command"
                            :resource resource
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (expansion-ui-spec--with-stub-scope
            (expansion-ui-spec--make-scope
             violation mock-callback (list resource) "run_bash_command"
             (current-buffer))
          (cl-letf (((symbol-function 'read-string)
                     (lambda (_prompt &optional _initial &rest _args)
                       (signal 'quit nil))))
            (jf/gptel-scope--add-custom-to-scope)))

        (let ((parsed (json-parse-string callback-result :object-type 'plist)))
          (expect (or (eq (plist-get parsed :success) :json-false)
                      (eq (plist-get parsed :success) nil)) :to-be t)
          (expect (plist-get parsed :user_denied) :to-be t))))))

;;; ============================================================
;;; Edit Manually action
;;; ============================================================

(describe "Edit Manually action"

  (it "switches to the chat buffer associated with the expansion"
    ;; The action no longer opens scope.yml — it switches to the chat
    ;; buffer (the one whose drawer holds the scope) and focuses the
    ;; :PROPERTIES: drawer at point-min.
    (let* ((switched-buffer nil)
           (mock-callback (lambda (_) nil))
           (violation (list :tool "run_bash_command"
                            :resource "/tmp/file.txt"
                            :operation :read
                            :validation-type 'bash
                            :reason "test")))
      (jf/gptel-test--with-scope-drawer '()
        (let ((chat-buffer (current-buffer)))
          (cl-letf (((symbol-function 'switch-to-buffer)
                     (lambda (buf &rest _) (setq switched-buffer buf)))
                    ((symbol-function 'org-cycle)
                     (lambda (&rest _) nil)))
            (expansion-ui-spec--with-stub-scope
                (expansion-ui-spec--make-scope
                 violation mock-callback '("/tmp/file.txt") "run_bash_command"
                 chat-buffer)
              (jf/gptel-scope--edit-scope)))
          (expect switched-buffer :to-equal chat-buffer))))))

;;; ============================================================
;;; Drawer writer preserves structure (ADDED requirement)
;;; ============================================================
;;;
;;; New per the spec delta — covers idempotency, key preservation
;;; when adding to one key, and bare-vs-`+:' form for first vs
;;; subsequent adds.  These exercise --write-pattern-to-drawer
;;; directly.

(describe "Drawer writer preserves structure"

  (it "preserves existing drawer keys when adding to one key"
    ;; Fixture has GPTEL_SCOPE_READ + GPTEL_SCOPE_WRITE keys; adding
    ;; a new READ value must NOT clobber the WRITE values.
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_READ . ("/old/read"))
            (:GPTEL_SCOPE_WRITE . ("/old/write/a" "/old/write/b")))
        (jf/gptel-scope--write-pattern-to-drawer
         (current-buffer) :read "/new/read")
        ;; READ now contains the old + new value
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/old/read" "/new/read"))
        ;; WRITE values are untouched
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_WRITE")
                :to-equal '("/old/write/a" "/old/write/b")))))

  (it "is idempotent — duplicate adds are skipped"
    ;; register/invariant/scope-add-pattern-idempotent: writing a
    ;; pattern that already exists is a no-op (writer returns nil
    ;; and does not mutate the buffer).
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_READ . ("/already/here")))
        (let ((result (jf/gptel-scope--write-pattern-to-drawer
                       (current-buffer) :read "/already/here")))
          (expect result :to-be nil))
        ;; Still exactly one entry — no duplicate
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/already/here")))))

  (it "first add to an empty key writes the bare-key form"
    ;; The first write to a previously-absent key uses the bare
    ;; `:GPTEL_SCOPE_READ:' line.  org's `org-entry-put-multivalued-
    ;; property' emits all values for a key on a single line
    ;; separated by spaces (it does NOT emit the `+:' continuation
    ;; form that the user can hand-write — those are only read, not
    ;; written, by the multi-value API).
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (jf/gptel-scope--write-pattern-to-drawer
         (current-buffer) :read "/first")
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (expect text :to-match ":GPTEL_SCOPE_READ: /first")))))

  (it "subsequent adds preserve all values readable via the multi-value API"
    ;; After the first bare-key entry, additional values for the
    ;; same key are appended.  org's writer emits them on a single
    ;; line; they round-trip via `org-entry-get-multivalued-property'
    ;; as a list.  Hand-written `+:' continuations are also accepted
    ;; on read (the second sub-test below confirms this).
    (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
      (jf/gptel-test--with-scope-drawer '()
        (jf/gptel-scope--write-pattern-to-drawer
         (current-buffer) :read "/first")
        (jf/gptel-scope--write-pattern-to-drawer
         (current-buffer) :read "/second")
        ;; Both values round-trip via the multi-value reader
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/first" "/second"))
        ;; The on-disk text uses the bare-key emission form
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (expect text :to-match ":GPTEL_SCOPE_READ: /first /second")))
      ;; Hand-written `+:' continuations (as used by the test fixture
      ;; helper) are accepted on read — the writer does not need to
      ;; emit them, but the round-trip is still well-defined.
      (jf/gptel-test--with-scope-drawer
          '((:GPTEL_SCOPE_READ . ("/first" "/second")))
        (expect (org-entry-get-multivalued-property
                 (point-min) "GPTEL_SCOPE_READ")
                :to-equal '("/first" "/second"))))))

;;; ============================================================
;;; Transient-scope shape (register/shape/expansion-transient-scope)
;;; ============================================================

(describe "register/shape/expansion-transient-scope"

  (it "the transient scope plist passed to transient-setup has the five required keys"
    (let* ((violation (list :tool "read_file_in_scope"
                            :resource "/tmp/x"
                            :operation :read
                            :validation-type 'path
                            :reason "test"))
           (callback (lambda (_) nil))
           (patterns '("/tmp/x"))
           (tool-name "read_file_in_scope")
           (captured-scope nil))

      (setq jf/gptel-scope--expansion-queue nil)
      (setq jf/gptel-scope--expansion-active nil)

      (cl-letf (((symbol-function 'transient-setup)
                 (lambda (_prefix _ignored1 _ignored2 &rest args)
                   (setq captured-scope (plist-get args :scope)))))
        (jf/gptel-scope-prompt-expansion violation callback patterns tool-name))

      ;; All five keys present and well-formed
      (expect (plist-get captured-scope :violation) :to-equal violation)
      (expect (plist-get captured-scope :callback) :to-equal callback)
      (expect (plist-get captured-scope :patterns) :to-equal patterns)
      (expect (plist-get captured-scope :tool-name) :to-equal tool-name)
      (expect (bufferp (plist-get captured-scope :chat-buffer)) :to-be t)
      (expect (buffer-live-p (plist-get captured-scope :chat-buffer)) :to-be t)

      (setq jf/gptel-scope--expansion-active nil))))

;;; ============================================================
;;; Queue progression (register/invariant/expansion-queue-always-progresses)
;;; ============================================================

(describe "register/invariant/expansion-queue-always-progresses"

  (it "a synthetic three-element queue drains monotonically as actions fire"
    ;; Fire three prompts back-to-back: first shows transient, two
    ;; queue.  Then drive three terminal actions and assert the queue
    ;; length monotonically decreases until empty, with the active
    ;; flag clearing on the final pump.
    (with-temp-buffer
      (org-mode)
      (setq jf/gptel-scope--expansion-queue nil)
      (setq jf/gptel-scope--expansion-active nil)

      (let ((transient-calls 0)
            (queue-lengths nil))
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _) (setq transient-calls (1+ transient-calls)))))
          (dolist (i '(1 2 3))
            (jf/gptel-scope-prompt-expansion
             (list :tool "test" :resource (format "r%d" i) :validation-type 'bash)
             (lambda (_) nil)
             (list (format "r%d" i))
             "test")))

        ;; First call shows transient; subsequent two queue.
        (expect transient-calls :to-equal 1)
        (expect (length jf/gptel-scope--expansion-queue) :to-equal 2)
        (expect jf/gptel-scope--expansion-active :to-be t)

        ;; Drive the queue through three pumps (simulating the three
        ;; terminal actions firing).  Capture queue length after each.
        (cl-letf (((symbol-function 'transient-setup)
                   (lambda (&rest _) (setq transient-calls (1+ transient-calls)))))
          ;; Pump 1: pops next, shows transient (queue 2 → 1)
          (jf/gptel-scope--process-expansion-queue)
          (push (length jf/gptel-scope--expansion-queue) queue-lengths)
          ;; Pump 2: pops next, shows transient (queue 1 → 0)
          (jf/gptel-scope--process-expansion-queue)
          (push (length jf/gptel-scope--expansion-queue) queue-lengths)
          ;; Pump 3: queue empty, clears active flag
          (jf/gptel-scope--process-expansion-queue)
          (push (length jf/gptel-scope--expansion-queue) queue-lengths))

        ;; Reverse so first->last
        (setq queue-lengths (nreverse queue-lengths))

        ;; Monotonic non-increasing from 1 to 0
        (expect queue-lengths :to-equal '(1 0 0))
        (expect jf/gptel-scope--expansion-active :to-be nil)
        ;; Three transient-setup calls total: one initial + two pumps
        (expect transient-calls :to-equal 3))

      (setq jf/gptel-scope--expansion-active nil))))

(provide 'expansion-ui-spec)
;;; expansion-ui-spec.el ends here
