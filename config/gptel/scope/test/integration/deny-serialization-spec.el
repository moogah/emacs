;;; deny-serialization-spec.el --- RED: denial payloads must serialize, deny must not hang -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; RED-PHASE regression for the parallel-deny `symbolp' crash / session hang
;; (.tasks/scope-deny-symbolp-crash-parallel-tools.md).
;;
;; PROVEN ROOT CAUSE (supersedes the earlier scope-validation.el:246 theory):
;; `jf/gptel-scope--validate-path-operation' emits `:allowed-patterns' /
;; `:deny-patterns' as plain Lisp LISTS, `format-tool-error' copies them into
;; the denial payload, and Emacs `json-serialize' has NO list->array rule — it
;; treats a non-keyword list as an alist/object and calls `symbol-name' on the
;; first element (a pattern STRING), raising
;;
;;     (wrong-type-argument symbolp "/…/presets/**")
;;
;; It is NOT parallelism-specific: ANY denied filesystem op whose scope has at
;; least one allowed (or deny) pattern triggers it.  Empty / deny-all scope
;; yields `:allowed-patterns nil', which serializes cleanly — which is exactly
;; why the existing empty-scope specs are green and the bug hid.
;;
;; The hang is the async leg: when the denial resolves via the transient suffix
;; `jf/gptel-scope--deny-expansion' (a later turn, outside the scoped-tool
;; macro's `condition-case'), the throw is only `message'-logged, the gptel
;; `process-tool-result' callback NEVER fires, gptel's FSM counter never reaches
;; `ntools', and the session is stuck on the agent overlay forever.
;;
;; These specs FAIL before the fix and PASS after.  They drive the REAL deny
;; suffix (not a mocked direct-callback) against a NON-EMPTY scope, which the
;; pre-existing specs deliberately avoided.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope/scope-shell-tools.el" gptel-dir))
  (require 'gptel))

;;; Infrastructure

(defvar dser--shown nil
  "Transient scopes shown via `transient-setup', newest first.
Captured so a test can drive the REAL deny suffix against each one.")

(defun dser--find-tool (name)
  (cl-block nil
    (dolist (cat gptel--known-tools)
      (dolist (entry (cdr cat))
        (when (string= (car entry) name) (cl-return (cdr entry)))))))

(defun dser--install-transient-capture ()
  "Let the REAL `prompt-expansion' queue run, capturing transients instead of UI.
Mirrors production: first violation SHOWs (active flag set), the rest QUEUE."
  (spy-on 'transient-setup :and-call-fake
          (lambda (_prefix &rest args)
            (push (plist-get (cddr args) :scope) dser--shown)))
  (spy-on 'transient-quit-one)          ; no-op outside a real transient
  (setq jf/gptel-scope--expansion-active nil
        jf/gptel-scope--expansion-queue nil
        dser--shown nil))

(defun dser--deny-current ()
  "Run the REAL `jf/gptel-scope--deny-expansion' against the shown transient.
After denial the production code pumps the queue, which re-enters
`transient-setup' (captured) for the next pending violation."
  (let ((scope (car dser--shown)))
    (cl-letf (((symbol-function 'transient-scope) (lambda (&rest _) scope)))
      (jf/gptel-scope--deny-expansion))))

;;; ============================================================
;;; SUITE 1: the serializer must accept array-shaped (list) fields
;;; ============================================================

(describe "jf/gptel-scope--serialize-tool-result: array-valued fields"

  (it "serializes a plist whose value is a Lisp LIST of strings (the denial-payload shape)"
    ;; This is the exact shape `format-tool-error' produces:
    ;;   (:success nil :allowed-patterns ("/a/**" "/b/**") ...)
    ;; Before the fix, json-serialize throws (wrong-type-argument symbolp ...).
    (expect
     (json-parse-string
      (jf/gptel-scope--serialize-tool-result
       (list :success :false
             :error "not-in-scope"
             :allowed-patterns (list "/a/**" "/b/**")))
      :object-type 'plist)
     :to-be-truthy))

  (it "renders the list field as a JSON ARRAY, not an object"
    (let* ((json (jf/gptel-scope--serialize-tool-result
                  (list :allowed-patterns (list "/a/**" "/b/**"))))
           (parsed (json-parse-string json :object-type 'plist :array-type 'list)))
      (expect (plist-get parsed :allowed-patterns) :to-equal '("/a/**" "/b/**"))))

  (it "never throws — degrades to a serializable payload on a hostile value"
    (expect (stringp (jf/gptel-scope--serialize-tool-result
                      (list :patterns (list "/x/**"))))
            :to-be t)))

;;; ============================================================
;;; SUITE 2: a single denied filesystem call under NON-EMPTY scope
;;; The REAL deny suffix must deliver a structured denial — no hang.
;;; ============================================================

(describe "Real deny suffix under non-empty scope"

  (defvar dser--root nil)
  (defvar dser--result nil)

  (before-each
    (setq dser--root (file-name-as-directory (make-temp-file "dser-root-" t)))
    (setq dser--result nil)
    ;; Read scope = root/**, write = /tmp/** ; init.el read is OUT of scope but
    ;; the scope is NON-EMPTY, so the denial payload carries allowed-patterns.
    (spy-on 'jf/gptel-scope--load-config :and-return-value
            (helpers-spec-make-scope-config
             :read (list (concat dser--root "**"))
             :write '("/tmp/**")))
    (dser--install-transient-capture))

  (after-each
    (when (and dser--root (file-exists-p dser--root))
      (delete-directory dser--root t)))

  (it "delivers a denial to the tool callback (callback fires — FSM can advance)"
    (let* ((outside (make-temp-file "dser-outside-")))
      (unwind-protect
          (progn
            (funcall (gptel-tool-function (dser--find-tool "read_file_in_scope"))
                     (lambda (json) (setq dser--result (json-parse-string json :object-type 'plist)))
                     outside)
            ;; A transient was shown (denied + routed to expansion).
            (expect (length dser--shown) :to-equal 1)
            ;; Drive the REAL deny suffix.  Before the fix this throws inside the
            ;; callback chain and dser--result stays nil (the hang).
            (dser--deny-current)
            (expect dser--result :to-be-truthy)
            (expect (plist-get dser--result :success) :not :to-be t))
        (delete-file outside))))

  (it "the delivered denial carries allowed_patterns as a JSON array"
    (let* ((outside (make-temp-file "dser-outside-")))
      (unwind-protect
          (progn
            (funcall (gptel-tool-function (dser--find-tool "read_file_in_scope"))
                     (lambda (json)
                       (setq dser--result
                             (json-parse-string json :object-type 'plist :array-type 'list)))
                     outside)
            (dser--deny-current)
            ;; JSON object key is "allowed-patterns" -> plist :allowed-patterns
            (expect (listp (plist-get dser--result :allowed-patterns)) :to-be t)
            (expect (plist-get dser--result :allowed-patterns) :to-be-truthy))
        (delete-file outside)))))

;;; ============================================================
;;; SUITE 3: parallel denials must EACH deliver (no stuck FSM)
;;; Mirrors the live trigger: several reads fired together, one in scope,
;;; the rest out, optionally plus a request_scope_expansion.
;;; ============================================================

(describe "Parallel denials under non-empty scope — every callback fires"

  (defvar dser--root2 nil)
  (defvar dser--fired nil)

  (before-each
    (setq dser--root2 (file-name-as-directory (make-temp-file "dser-root2-" t)))
    (setq dser--fired nil)
    (spy-on 'jf/gptel-scope--load-config :and-return-value
            (helpers-spec-make-scope-config
             :read (list (concat dser--root2 "**"))
             :write '("/tmp/**")))
    (dser--install-transient-capture))

  (after-each
    (when (and dser--root2 (file-exists-p dser--root2))
      (delete-directory dser--root2 t)))

  (it "two out-of-scope reads fired together both reach their callbacks"
    (let ((a (make-temp-file "dser-a-"))
          (b (make-temp-file "dser-b-"))
          (read-tool (dser--find-tool "read_file_in_scope")))
      (unwind-protect
          (let ((mk (lambda (id) (lambda (_json) (push id dser--fired)))))
            ;; gptel mapc: fire both immediately.  #1 SHOWs, #2 QUEUEs.
            (funcall (gptel-tool-function read-tool) (funcall mk :a) a)
            (funcall (gptel-tool-function read-tool) (funcall mk :b) b)
            (expect (length dser--shown) :to-equal 1)
            (expect (length jf/gptel-scope--expansion-queue) :to-equal 1)
            ;; Deny the shown one; production pumps the queue -> next transient
            ;; is captured; deny it too.
            (dser--deny-current)
            (dser--deny-current)
            ;; Both tools' callbacks must have fired for gptel's FSM to advance.
            (expect (length dser--fired) :to-equal 2)
            (expect (memq :a dser--fired) :to-be-truthy)
            (expect (memq :b dser--fired) :to-be-truthy))
        (delete-file a)
        (delete-file b))))

  (it "a denied read queued alongside a request_scope_expansion both deliver"
    ;; The full live scenario: one filesystem deny + one request_scope_expansion
    ;; (whose violation-info carries :patterns).  Denying both must not throw
    ;; and must deliver to both callbacks.
    (let ((a (make-temp-file "dser-c-"))
          (read-tool (dser--find-tool "read_file_in_scope"))
          (rse-tool  (dser--find-tool "request_scope_expansion")))
      (unwind-protect
          (let ((mk (lambda (id) (lambda (_json) (push id dser--fired)))))
            (funcall (gptel-tool-function read-tool) (funcall mk :read) a)
            (funcall (gptel-tool-function rse-tool) (funcall mk :rse)
                     "read" (vector (concat dser--root2 "**")) "need it")
            ;; One shown, one queued (order depends on fire order; both pending).
            (expect (+ (length dser--shown) (length jf/gptel-scope--expansion-queue))
                    :to-equal 2)
            (dser--deny-current)
            (dser--deny-current)
            (expect (length dser--fired) :to-equal 2))
        (delete-file a)))))

(provide 'deny-serialization-spec)
;;; deny-serialization-spec.el ends here
