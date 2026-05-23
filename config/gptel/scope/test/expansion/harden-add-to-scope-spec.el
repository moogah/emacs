;;; harden-add-to-scope-spec.el --- Action-handler hardening for nil-operation, :match-pattern, and no-op signal threading -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Pins the four-stage upstream guard at jf/gptel-scope--add-to-scope
;; (and the Stage 4 no-op-signal threading at the wildcard / custom
;; outer action handlers).
;;
;; Stages under test:
;;
;; - Stage 1: nil :operation refuse — cloud_auth_denied routes the
;;   provider to GPTEL_SCOPE_CLOUD_PROVIDERS via the dedicated cloud
;;   writer; parse_incomplete signals user-error; defensive fallback
;;   signals user-error.  In all branches the writer is NOT called and
;;   the queue is pumped before signaling.
;; - Stage 2: :match-pattern refuse — action handler signals user-error
;;   directing the user to scope the search root; writer is NOT called.
;; - Stage 4: writer no-op signal threading — bare-command branch and
;;   dedup short-circuit both surface :patterns_added [] (not a phantom-
;;   add).  Verified at all three outer handlers (--add-to-scope,
;;   --add-wildcard-to-scope, --add-custom-to-scope).
;;
;; Cited register entries:
;;   - register/boundary/scope-expansion-action-handler
;;   - register/vocabulary/operation-to-drawer-key
;;   - register/shape/violation-info
;;   - register/shape/expansion-transient-scope
;;   - register/invariant/expansion-queue-always-progresses

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope-validation.el" scope-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope-expansion.el" scope-dir)))

;;; Test helpers

(defvar harden--captured-callback-json nil
  "Last JSON string handed to the test callback.")

(defun harden--callback (json)
  "Test callback that captures JSON for inspection."
  (setq harden--captured-callback-json json))

(defun harden--last-callback-plist ()
  "Parse the last captured callback JSON into a plist."
  (when harden--captured-callback-json
    (json-parse-string harden--captured-callback-json :object-type 'plist)))

(defun harden--make-scope (violation &optional patterns tool-name buffer)
  "Construct a transient-scope plist from VIOLATION (helpers test factory).
PATTERNS / TOOL-NAME default to a single-pattern, named tool.
BUFFER is the chat-buffer; defaults to the current buffer."
  (list :violation violation
        :callback #'harden--callback
        :patterns (or patterns (list (plist-get violation :resource)))
        :tool-name (or tool-name "test_tool")
        :chat-buffer (or buffer (current-buffer))))

(defmacro harden--with-stub-scope (scope-form &rest body)
  "Run BODY with `transient-scope' stubbed to return SCOPE-FORM.
Also stubs `transient-quit-one' to a no-op."
  (declare (indent 1))
  `(let ((scope ,scope-form))
     (cl-letf (((symbol-function 'transient-scope)
                (lambda (&rest _) scope))
               ((symbol-function 'transient-quit-one)
                (lambda (&rest _) nil))
               ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                (lambda (&rest _) nil)))
       (setq harden--captured-callback-json nil)
       ,@body)))

;;; Tests

(describe "harden-add-to-scope: Stage 1 — nil :operation refuse"

  (describe "cloud_auth_denied violation (operation = nil)"

    (it "routes the provider to GPTEL_SCOPE_CLOUD_PROVIDERS, not to a path bucket"
      ;; Stub save-buffer because the temp buffer has no file backing.
      ;; The org-entry-put-multivalued-property mutation happens in-buffer
      ;; before save-buffer is called, so the assertion still validates
      ;; the writer's drawer mutation.
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest _) nil)))
        (jf/gptel-test--with-scope-drawer '()
          (let* ((violation (helpers-spec--make-violation-info
                             "run_bash_command" "cloud_auth_denied"
                             :provider "aws"
                             :command "aws s3 ls"))
                 (scope (harden--make-scope violation '("aws") "run_bash_command"
                                            (current-buffer))))
            (harden--with-stub-scope scope
              (jf/gptel-scope--add-to-scope))
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_CLOUD_PROVIDERS")
                    :to-equal '("aws"))
            (expect (org-entry-get-multivalued-property
                     (point-min) "GPTEL_SCOPE_READ")
                    :to-equal nil)
            (let ((parsed (harden--last-callback-plist)))
              (expect (plist-get parsed :success) :to-be t)
              (expect (vectorp (plist-get parsed :patterns_added)) :to-be t)
              (expect (length (plist-get parsed :patterns_added)) :to-equal 1)
              (expect (aref (plist-get parsed :patterns_added) 0) :to-equal "aws")))))))

  (describe "parse_incomplete violation"

    (it "signals user-error and does not invoke the writer"
      (let* ((violation (helpers-spec--make-violation-info
                         "run_bash_command" "parse_incomplete"
                         :command "cat $(garbage|"))
             (scope (harden--make-scope violation '("cat $(garbage|")
                                        "run_bash_command")))
        (spy-on 'jf/gptel-scope--write-pattern-to-drawer)
        (harden--with-stub-scope scope
          (expect (jf/gptel-scope--add-to-scope) :to-throw 'user-error))
        (expect 'jf/gptel-scope--write-pattern-to-drawer
                :not :to-have-been-called)))

    (it "still pumps the expansion queue before signaling"
      (let* ((violation (helpers-spec--make-violation-info
                         "run_bash_command" "parse_incomplete"
                         :command "cat $(garbage|"))
             (scope (harden--make-scope violation '("cat $(garbage|")
                                        "run_bash_command"))
             (queue-pumped nil))
        (cl-letf (((symbol-function 'transient-scope)
                   (lambda (&rest _) scope))
                  ((symbol-function 'transient-quit-one)
                   (lambda (&rest _) nil))
                  ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                   (lambda (&rest _) (setq queue-pumped t))))
          (setq harden--captured-callback-json nil)
          (condition-case _err
              (jf/gptel-scope--add-to-scope)
            (user-error nil))
          (expect queue-pumped :to-be t)))))

  (describe "bash violation with :operation nil and unknown error code"

    (it "signals user-error suggesting allow-once"
      (let* ((violation (list :tool "run_bash_command"
                              :resource "weird"
                              :operation nil
                              :reason nil
                              :validation-type 'bash
                              :metadata nil
                              :error "unrecognised"))
             (scope (harden--make-scope violation '("weird")
                                        "run_bash_command")))
        (spy-on 'jf/gptel-scope--write-pattern-to-drawer)
        (harden--with-stub-scope scope
          (expect (jf/gptel-scope--add-to-scope) :to-throw 'user-error))
        (expect 'jf/gptel-scope--write-pattern-to-drawer
                :not :to-have-been-called)))))

(describe "harden-add-to-scope: Stage 2 — :match-pattern refuse"

  (it "signals user-error and does not invoke the writer"
    (let* ((violation (helpers-spec--make-violation-info
                       "run_bash_command" "not-in-scope"
                       :path "*.txt"
                       :operation :match-pattern))
           (scope (harden--make-scope violation '("*.txt") "run_bash_command")))
      (spy-on 'jf/gptel-scope--write-pattern-to-drawer)
      (harden--with-stub-scope scope
        (expect (jf/gptel-scope--add-to-scope) :to-throw 'user-error))
      (expect 'jf/gptel-scope--write-pattern-to-drawer
              :not :to-have-been-called)))

  (it "still pumps the expansion queue before signaling"
    (let* ((violation (helpers-spec--make-violation-info
                       "run_bash_command" "not-in-scope"
                       :path "*.txt"
                       :operation :match-pattern))
           (scope (harden--make-scope violation '("*.txt") "run_bash_command"))
           (queue-pumped nil))
      (cl-letf (((symbol-function 'transient-scope)
                 (lambda (&rest _) scope))
                ((symbol-function 'transient-quit-one)
                 (lambda (&rest _) nil))
                ((symbol-function 'jf/gptel-scope--process-expansion-queue)
                 (lambda (&rest _) (setq queue-pumped t))))
        (setq harden--captured-callback-json nil)
        (condition-case _err
            (jf/gptel-scope--add-to-scope)
          (user-error nil))
        (expect queue-pumped :to-be t)))))

(describe "register/vocabulary/operation-to-drawer-key strict-error contract"

  (it "writer signals when :match-pattern is reached directly (defect contract)"
    ;; Sanity: the writer's strict-error fallback is the fence that
    ;; cycle-2 introduced.  The hardened action handler must never let
    ;; :match-pattern reach the writer; if it does, this is the user-
    ;; visible defect signal.
    (expect (jf/gptel-scope--map-operation-to-drawer-key :match-pattern)
            :to-throw)))

(describe "harden-add-to-scope: Stage 4 — writer no-op signal threading"

  (describe "bare-command bash violation (writer returns nil)"

    (it "--add-to-scope emits :patterns_added [] rather than phantom-add"
      ;; A bash violation with a bare command name (e.g. \"tree\") routes
      ;; through --add-bash-to-scope's bare-command branch, which returns
      ;; nil without writing.  The outer handler must NOT claim :success
      ;; t :patterns_added [<command>] in that case.
      (let* ((violation (helpers-spec--make-violation-info
                         "run_bash_command" "command-not-allowed"
                         :command "tree"
                         :operation :execute))
             (scope (harden--make-scope violation '("tree") "run_bash_command")))
        ;; Stub the writer-router to simulate the bare-command branch.
        (cl-letf (((symbol-function 'jf/gptel-scope--write-pattern-to-scope)
                   (lambda (&rest _) nil)))
          (harden--with-stub-scope scope
            (jf/gptel-scope--add-to-scope))
          (let ((parsed (harden--last-callback-plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (vectorp (plist-get parsed :patterns_added)) :to-be t)
            (expect (length (plist-get parsed :patterns_added)) :to-equal 0)
            (expect (plist-get parsed :message) :to-match "no-op"))))))

  (describe "dedup short-circuit (writer returns nil because pattern is already in drawer)"

    (it "--add-to-scope emits :patterns_added [] rather than phantom-add"
      (let* ((violation (helpers-spec--make-violation-info
                         "read_file_in_scope" "not-in-scope"
                         :path "/already/scoped.txt"
                         :operation :read))
             (scope (harden--make-scope violation
                                        '("/already/scoped.txt")
                                        "read_file_in_scope")))
        (cl-letf (((symbol-function 'jf/gptel-scope--write-pattern-to-scope)
                   (lambda (&rest _) nil)))
          (harden--with-stub-scope scope
            (jf/gptel-scope--add-to-scope))
          (let ((parsed (harden--last-callback-plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (length (plist-get parsed :patterns_added)) :to-equal 0)
            (expect (plist-get parsed :message) :to-match "no-op")))))

    (it "--add-wildcard-to-scope emits :patterns_added [] rather than phantom-add"
      (let* ((violation (helpers-spec--make-violation-info
                         "read_file_in_scope" "not-in-scope"
                         :path "/already/scoped.txt"
                         :operation :read))
             (scope (harden--make-scope violation
                                        '("/already/scoped.txt")
                                        "read_file_in_scope")))
        (cl-letf (((symbol-function 'jf/gptel-scope--write-pattern-to-scope)
                   (lambda (&rest _) nil)))
          (harden--with-stub-scope scope
            (jf/gptel-scope--add-wildcard-to-scope))
          (let ((parsed (harden--last-callback-plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (length (plist-get parsed :patterns_added)) :to-equal 0)
            (expect (plist-get parsed :message) :to-match "no-op")))))

    (it "--add-custom-to-scope emits :patterns_added [] rather than phantom-add"
      (let* ((violation (helpers-spec--make-violation-info
                         "read_file_in_scope" "not-in-scope"
                         :path "/already/scoped.txt"
                         :operation :read))
             (scope (harden--make-scope violation
                                        '("/already/scoped.txt")
                                        "read_file_in_scope")))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "/already/scoped.txt"))
                  ((symbol-function 'jf/gptel-scope--write-pattern-to-scope)
                   (lambda (&rest _) nil)))
          (harden--with-stub-scope scope
            (jf/gptel-scope--add-custom-to-scope))
          (let ((parsed (harden--last-callback-plist)))
            (expect (plist-get parsed :success) :to-be t)
            (expect (length (plist-get parsed :patterns_added)) :to-equal 0)
            (expect (plist-get parsed :message) :to-match "no-op")))))))

(describe "harden-add-to-scope: Stage 3+4 — happy-path delegation"

  (it "delegates ordinary (:read \"/path\") violations to the writer"
    (let* ((violation (helpers-spec--make-violation-info
                       "read_file_in_scope" "not-in-scope"
                       :path "/workspace/new.txt"
                       :operation :read))
           (scope (harden--make-scope violation '("/workspace/new.txt")
                                      "read_file_in_scope"))
           (writer-called-with nil))
      (cl-letf (((symbol-function 'jf/gptel-scope--write-pattern-to-scope)
                 (lambda (pattern vt tool op)
                   (setq writer-called-with (list pattern vt tool op))
                   "/workspace/new.txt")))
        (harden--with-stub-scope scope
          (jf/gptel-scope--add-to-scope))
        (expect writer-called-with :not :to-be nil)
        (expect (car writer-called-with) :to-equal "/workspace/new.txt")
        (let ((parsed (harden--last-callback-plist)))
          (expect (plist-get parsed :success) :to-be t)
          (expect (length (plist-get parsed :patterns_added)) :to-equal 1))))))

(provide 'harden-add-to-scope-spec)

;;; harden-add-to-scope-spec.el ends here
