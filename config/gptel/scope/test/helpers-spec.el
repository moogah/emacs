;;; helpers-spec.el --- Shared test infrastructure for scope validation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Shared test infrastructure for scope validation testing.
;; Provides:
;; 1. Custom matchers for validation results
;; 2. Mock utilities for gptel sessions and processes
;; 3. Test fixture creation helpers
;; 4. Common setup/teardown functions
;;
;; Usage:
;;   (require 'helpers-spec)
;;   (describe "My test suite"
;;     (before-each (helpers-spec-setup-session))
;;     (after-each (helpers-spec-teardown-session))
;;     (it "validates command"
;;       (expect (helpers-spec-validate-command "ls")
;;               :to-be-validation-success)))

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load contract validation for mock self-validation
;; config/gptel/scope/test/ -> config/test/contracts/ (3 levels up, then test/contracts)
(let ((contracts-dir (expand-file-name "../../../test/contracts/"
                                       (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path contracts-dir))
(require 'contract-core)
(require 'contract-bash-parser)
(contract--register-buttercup-matcher)

;;; Custom Matchers

(buttercup-define-matcher :to-be-validation-success (result)
  "Match validation result indicating success.
Result should be a plist with :status :success."
  (let ((status (plist-get result :status)))
    (if (eq status :success)
        t
      (cons nil (format "Expected validation success but got status: %S" status)))))

(buttercup-define-matcher :to-be-validation-error (result expected-error-type)
  "Match validation result with specific error type.
Result should be a plist with :status :error and :error matching EXPECTED-ERROR-TYPE."
  (let ((status (plist-get result :status))
        (error-type (plist-get result :error)))
    (if (and (eq status :error)
             (equal error-type expected-error-type))
        t
      (cons nil (format "Expected error %S but got status: %S, error: %S"
                        expected-error-type status error-type)))))

(buttercup-define-matcher :to-have-file-operation (result operation path)
  "Match that semantic result contains file operation.
Checks if RESULT has a file operation with OPERATION type and PATH."
  (let* ((file-ops (plist-get result :file-operations))
         (matching-op (cl-find-if
                       (lambda (op)
                         (and (eq (plist-get op :operation) operation)
                              (string= (plist-get op :file) path)))
                       file-ops)))
    (if matching-op
        t
      (cons nil (format "Expected file operation %S %S but operations were: %S"
                        operation path file-ops)))))

(buttercup-define-matcher :to-have-cloud-auth (result provider)
  "Match that semantic result contains cloud authentication.
Checks if RESULT has cloud auth detection for PROVIDER."
  (let* ((cloud-auth (plist-get result :cloud-auth))
         (detected-provider (plist-get cloud-auth :provider)))
    (if (eq detected-provider provider)
        t
      (cons nil (format "Expected cloud auth for %S but got: %S"
                        provider cloud-auth)))))

(buttercup-define-matcher :to-have-parse-coverage (result expected-ratio)
  "Match that parse coverage ratio meets or exceeds EXPECTED-RATIO.
EXPECTED-RATIO should be a float between 0.0 and 1.0."
  (let* ((coverage (plist-get result :coverage))
         (ratio (plist-get coverage :ratio)))
    (if (and ratio (>= ratio expected-ratio))
        t
      (cons nil (format "Expected coverage >= %.2f but got: %.2f"
                        expected-ratio (or ratio 0.0))))))

;;; Mock Session Management

(defvar helpers-spec--mock-session-dir nil
  "Temporary directory for mock gptel session.")

(defvar helpers-spec--mock-session-buffer nil
  "Buffer used for mock gptel session.")

(defvar helpers-spec--original-session-fn nil
  "Original value of jf/gptel-session--current-session-dir.")

(defun helpers-spec-setup-session ()
  "Set up a mock gptel session for testing.
Creates temporary directory and buffer, mocks session detection."
  ;; Create temp session directory
  (setq helpers-spec--mock-session-dir
        (make-temp-file "gptel-test-session-" t))

  ;; Create mock session buffer
  (setq helpers-spec--mock-session-buffer
        (generate-new-buffer "*gptel-test-session*"))

  ;; Mock session directory detection
  (when (fboundp 'jf/gptel-session--current-session-dir)
    (setq helpers-spec--original-session-fn
          (symbol-function 'jf/gptel-session--current-session-dir))
    (fset 'jf/gptel-session--current-session-dir
          (lambda () helpers-spec--mock-session-dir)))

  ;; Return session info for test use
  (list :dir helpers-spec--mock-session-dir
        :buffer helpers-spec--mock-session-buffer))

(defun helpers-spec-teardown-session ()
  "Tear down mock gptel session.
Removes temporary directory and buffer, restores original functions."
  ;; Restore original session function
  (when helpers-spec--original-session-fn
    (fset 'jf/gptel-session--current-session-dir
          helpers-spec--original-session-fn)
    (setq helpers-spec--original-session-fn nil))

  ;; Clean up mock buffer
  (when (and helpers-spec--mock-session-buffer
             (buffer-live-p helpers-spec--mock-session-buffer))
    (kill-buffer helpers-spec--mock-session-buffer)
    (setq helpers-spec--mock-session-buffer nil))

  ;; Clean up temp directory
  (when helpers-spec--mock-session-dir
    (when (file-exists-p helpers-spec--mock-session-dir)
      (delete-directory helpers-spec--mock-session-dir t))
    (setq helpers-spec--mock-session-dir nil)))

;;; Mock Process Management

(defvar helpers-spec--mock-process-output nil
  "Mock output for process execution.")

(defvar helpers-spec--mock-process-exit-code 0
  "Mock exit code for process execution.")

(defun helpers-spec-mock-process-output (output &optional exit-code)
  "Mock process execution to return OUTPUT with EXIT-CODE.
If EXIT-CODE is nil, defaults to 0 (success)."
  (setq helpers-spec--mock-process-output output)
  (setq helpers-spec--mock-process-exit-code (or exit-code 0)))

(defun helpers-spec-reset-process-mocks ()
  "Reset all process mocking state."
  (setq helpers-spec--mock-process-output nil)
  (setq helpers-spec--mock-process-exit-code 0))

;;; Fixture Creation Helpers

(cl-defun helpers-spec-make-scope-config
    (&key read write execute modify deny read-metadata
          (auth-detection "warn") allowed-providers)
  "Construct a scope-config plist directly for validator tests.

Returns the canonical plist shape produced by
`jf/gptel-scope--load-from-buffer' (see
`register/shape/scope-config-plist'): top-level keys :paths and :cloud
only.  No :security key (deleted in cycle-1; see
`register/invariant/scope-no-security-key-in-plist').

This replaces the historical scope-yml-on-disk + load-schema path:
unit tests construct the plist directly rather than round-tripping
through a YAML file or an org drawer.  For tests that exercise the
loader itself (drawer parsing, file fallback), use
`jf/gptel-test--with-scope-drawer' or write a real `session.org' to a
tmpdir."
  (list :paths (list :read read
                     :read-metadata read-metadata
                     :write write
                     :modify modify
                     :execute execute
                     :deny deny)
        :cloud (list :auth-detection auth-detection
                     :allowed-providers allowed-providers)))

(defun helpers-spec-make-minimal-scope-config ()
  "Construct a minimal valid scope-config plist for testing.
Returns a plist matching `register/shape/scope-config-plist'."
  (helpers-spec-make-scope-config
   :read    '("/workspace" "/workspace/**")
   :write   '("/workspace/**")
   :execute '("/workspace/scripts/**")
   :modify  '("/workspace/config/**")
   :deny    '("/etc/**")
   :auth-detection "warn"))

;;; Assertion Helpers

(defun helpers-spec-assert-validation-success (result)
  "Assert that validation RESULT indicates success.
Throws error if validation failed."
  (unless (eq (plist-get result :status) :success)
    (error "Expected validation success but got: %S" result)))

(defun helpers-spec-assert-validation-error (result expected-error)
  "Assert that validation RESULT has EXPECTED-ERROR.
Throws error if validation succeeded or has different error."
  (unless (eq (plist-get result :status) :error)
    (error "Expected validation error but got success"))
  (unless (equal (plist-get result :error) expected-error)
    (error "Expected error %S but got %S"
           expected-error (plist-get result :error))))

;;; Bash-Parser Mocking Helpers

(defvar helpers-spec--mock-bash-parse-result nil
  "Mock result for jf/bash-parse calls.")

(defvar helpers-spec--mock-bash-semantics-result nil
  "Mock result for jf/bash-extract-semantics calls.")

(defvar helpers-spec--original-bash-parse-fn nil
  "Original value of jf/bash-parse.")

(defvar helpers-spec--original-bash-semantics-fn nil
  "Original value of jf/bash-extract-semantics.")

(defun helpers-spec--mock-parse-result (command commands parse-complete)
  "Mock jf/bash-parse results with contract self-validation.
COMMAND is the input command string.
COMMANDS is the list of all extracted command names (strings).
PARSE-COMPLETE is a boolean flag indicating parse completeness.

Returns a mock plist with :success, :all-commands, :parse-complete.
:all-commands is a list of command plists with :command-name field."
  (let* ((all-commands (mapcar (lambda (cmd-name)
                                 (list :command-name cmd-name))
                               commands))
         (result (list :success t
                       :all-commands all-commands
                       :parse-complete parse-complete
                       :tokens '()
                       :command command)))
    (when-let ((error (contract/bash-parse-result--validate result)))
      (error "Mock parse-result violates contract: %s" error))
    result))

(defun helpers-spec--mock-semantics (file-ops cloud-auth coverage)
  "Mock jf/bash-extract-semantics results with contract self-validation.
FILE-OPS is a list of file operation plists.
CLOUD-AUTH is a cloud auth plist (or nil).  Note: scope consumer expects a flat
  plist here (:provider :aws :command \"cmd\"), not a list-of-plists as bash-parser
  produces.  This is a known consumer-side gap (scope should car the domain value).
COVERAGE is a plist with coverage statistics.

Returns a mock plist with :domains as alist matching bash-parser output."
  (let* ((domains (delq nil
                        (list (cons :filesystem file-ops)
                              (when cloud-auth
                                (cons :authentication cloud-auth)))))
         (result (list :domains domains
                       :coverage coverage
                       :parse-complete t)))
    ;; Validate file-ops individually (contract-enforced)
    (dolist (op file-ops)
      (when-let ((error (contract/bash-file-op--validate op)))
        (error "Mock file-op in semantics violates contract: %s" error)))
    ;; Validate top-level structure (alist domains, required keys)
    (when-let ((error (contract--validate-plist result
                        '((:domains listp)
                          (:coverage listp)
                          (:parse-complete booleanp)))))
      (error "Mock semantics violates contract: %s" error))
    ;; Validate domains is an alist (not plist)
    (when domains
      (when-let ((error (contract/bash-domains--validate domains)))
        (error "Mock semantics domains violates contract: %s" error)))
    result))

(defun helpers-spec--make-file-op (operation path &rest props)
  "Build file operation plist with contract self-validation.
OPERATION is a keyword (:read, :write, :execute, :modify).
PATH is the file path string.
PROPS are additional properties as plist (e.g., :command \"cat\" :source :positional-arg).

Returns file-op plist in bash-parser format: (:file PATH :operation OP :command CMD
 :confidence CONF :source SRC)."
  (let ((result (append (list :file path
                              :operation operation
                              :confidence (or (plist-get props :confidence) :high)
                              :source (or (plist-get props :source) :positional-arg)
                              :command (or (plist-get props :command) "unknown"))
                        props)))
    (when-let ((error (contract/bash-file-op--validate result)))
      (error "Mock file-op violates contract: %s" error))
    result))

(defun helpers-spec-setup-bash-mocks ()
  "Set up bash-parser mocking infrastructure.
Saves original functions and installs mocks."
  ;; Save originals if not already saved
  (when (and (fboundp 'jf/bash-parse)
             (not helpers-spec--original-bash-parse-fn))
    (setq helpers-spec--original-bash-parse-fn
          (symbol-function 'jf/bash-parse)))
  (when (and (fboundp 'jf/bash-extract-semantics)
             (not helpers-spec--original-bash-semantics-fn))
    (setq helpers-spec--original-bash-semantics-fn
          (symbol-function 'jf/bash-extract-semantics)))

  ;; Install mocks
  (when (fboundp 'jf/bash-parse)
    (fset 'jf/bash-parse
          (lambda (_command)
            (or helpers-spec--mock-bash-parse-result
                (list :success t :all-commands '() :parse-complete t)))))
  (when (fboundp 'jf/bash-extract-semantics)
    (fset 'jf/bash-extract-semantics
          (lambda (_parsed)
            (or helpers-spec--mock-bash-semantics-result
                (list :domains '() :coverage '(:ratio 1.0) :parse-complete t))))))

(defun helpers-spec-teardown-bash-mocks ()
  "Tear down bash-parser mocking infrastructure.
Restores original functions."
  ;; Restore originals
  (when helpers-spec--original-bash-parse-fn
    (fset 'jf/bash-parse helpers-spec--original-bash-parse-fn)
    (setq helpers-spec--original-bash-parse-fn nil))
  (when helpers-spec--original-bash-semantics-fn
    (fset 'jf/bash-extract-semantics helpers-spec--original-bash-semantics-fn)
    (setq helpers-spec--original-bash-semantics-fn nil))

  ;; Clear mock state
  (setq helpers-spec--mock-bash-parse-result nil)
  (setq helpers-spec--mock-bash-semantics-result nil))

(defun helpers-spec-mock-bash-parse (command commands parse-complete)
  "Set mock result for next jf/bash-parse call.
COMMAND, COMMANDS, PARSE-COMPLETE are passed to helpers-spec--mock-parse-result."
  (setq helpers-spec--mock-bash-parse-result
        (helpers-spec--mock-parse-result command commands parse-complete)))

(defun helpers-spec-mock-bash-semantics (file-ops cloud-auth coverage)
  "Set mock result for next jf/bash-extract-semantics call.
FILE-OPS, CLOUD-AUTH, COVERAGE are passed to helpers-spec--mock-semantics."
  (setq helpers-spec--mock-bash-semantics-result
        (helpers-spec--mock-semantics file-ops cloud-auth coverage)))

;;; Scope Configuration Builders

(defun helpers-spec--convert-vectors-to-lists (obj)
  "Recursively convert vectors to lists in OBJ.
Also converts YAML boolean keywords to Emacs booleans:
- :true -> t
- :false -> nil"
  (cond
   ;; Convert YAML boolean keywords to Emacs booleans
   ((eq obj :true) t)
   ((eq obj :false) nil)
   ((vectorp obj)
    (mapcar #'helpers-spec--convert-vectors-to-lists (append obj nil)))
   ((and (listp obj) (not (null obj)))
    (if (keywordp (car obj))
        ;; It's a plist, process keys and values
        (let ((result nil))
          (while obj
            (push (car obj) result)  ; key
            (push (helpers-spec--convert-vectors-to-lists (cadr obj)) result)  ; value
            (setq obj (cddr obj)))
          (nreverse result))
      ;; It's a regular list
      (mapcar #'helpers-spec--convert-vectors-to-lists obj)))
   (t obj)))

;; Note: scope-yml fixtures and YAML-based loader helpers were retired
;; alongside `jf/gptel-scope-yaml--load-schema' in cycle-3
;; (delete-yaml-and-security-residue). validation/ tests now construct
;; scope-config plists directly via `helpers-spec-make-scope-config' or
;; — for loader tests — drawer fixtures via
;; `jf/gptel-test--with-scope-drawer'.
;;
;; The legacy helpers below (`helpers-spec-make-scope-yml',
;; `helpers-spec-load-scope-config', `helpers-spec--scope-with-paths',
;; `helpers-spec--scope-with-cloud', `helpers-spec-make-minimal-scope',
;; `helpers-spec-make-scope-with-cloud-deny',
;; `helpers-spec-make-scope-with-allowed-providers') survive only as
;; load-time defuns so that integration/expansion specs that have not
;; yet been migrated still load.  Calling them will signal a runtime
;; error because the underlying YAML loader is gone.

(defun helpers-spec-make-scope-yml (content)
  "Deprecated.  Returns a temp filename containing CONTENT.

The YAML loader these temp files were intended for has been deleted
(cycle-3 delete-yaml-and-security-residue).  Use
`helpers-spec-make-scope-config' for unit tests, or
`jf/gptel-test--with-scope-drawer' for loader tests."
  (let ((scope-file (make-temp-file "scope-" nil ".yml")))
    (with-temp-file scope-file
      (insert content))
    scope-file))

(defun helpers-spec-make-minimal-scope ()
  "Deprecated.  Use `helpers-spec-make-minimal-scope-config' instead."
  (helpers-spec-make-scope-yml
   "paths:\n  read:\n    - \"/workspace\"\n"))

(defun helpers-spec-load-scope-config (_scope-file)
  "Deprecated stub: the YAML loader is gone.

Tests that called `helpers-spec-load-scope-config' should be migrated
to construct a plist directly via `helpers-spec-make-scope-config' or
to use `jf/gptel-test--with-scope-drawer' + `jf/gptel-scope--load-from-buffer'."
  (error "helpers-spec-load-scope-config: YAML loader removed; migrate to helpers-spec-make-scope-config or jf/gptel-test--with-scope-drawer"))

(defun helpers-spec--scope-with-paths (&rest _args)
  "Deprecated stub.  Use `helpers-spec-make-scope-config'."
  (error "helpers-spec--scope-with-paths: YAML helpers removed; use helpers-spec-make-scope-config"))

(defun helpers-spec--scope-with-cloud (&rest _args)
  "Deprecated stub.  Use `helpers-spec-make-scope-config'."
  (error "helpers-spec--scope-with-cloud: YAML helpers removed; use helpers-spec-make-scope-config"))

(defun helpers-spec-make-scope-with-cloud-deny ()
  "Deprecated stub.  Use `helpers-spec-make-scope-config'."
  (error "helpers-spec-make-scope-with-cloud-deny: YAML helpers removed; use helpers-spec-make-scope-config"))

(defun helpers-spec-make-scope-with-allowed-providers (_providers)
  "Deprecated stub.  Use `helpers-spec-make-scope-config'."
  (error "helpers-spec-make-scope-with-allowed-providers: YAML helpers removed; use helpers-spec-make-scope-config"))

;;; Additional Custom Matchers

(buttercup-define-matcher :to-have-warning (result)
  "Match that result contains a warning.
Result should be a plist with :status :warning."
  (let ((status (plist-get result :status)))
    (if (eq status :warning)
        t
      (cons nil (format "Expected warning but got status: %S" status)))))

(buttercup-define-matcher :to-be-denied-command (result)
  "Match command_denied error in result.
Result should have :error :command_denied."
  (let ((error-type (plist-get result :error)))
    (if (eq error-type :command_denied)
        t
      (cons nil (format "Expected :command_denied but got error: %S" error-type)))))

(buttercup-define-matcher :to-be-path-out-of-scope (result)
  "Match path_out_of_scope error in result.
Result should have :error :path_out_of_scope."
  (let ((error-type (plist-get result :error)))
    (if (eq error-type :path_out_of_scope)
        t
      (cons nil (format "Expected :path_out_of_scope but got error: %S" error-type)))))

;;; Violation-Info Fixture Factory

(defun helpers-spec--make-violation-info (tool error-code &rest props)
  "Build a realistic violation-info plist via the real build-violation-info.
TOOL is the tool name string (e.g., \"read_file_in_scope\", \"run_bash_command\").
ERROR-CODE is the machine-readable error string (e.g., \"not-in-scope\",
\"path_out_of_scope\", \"command_denied\").
PROPS are keyword arguments:
  :path       - File path (for path-based errors)
  :command    - Command string (for command errors)
  :provider   - Cloud provider (for cloud auth errors)
  :resource   - Explicit resource override
  :operation  - Operation keyword (:read, :write, etc.)
  :metadata   - File metadata plist
  :message    - Explicit human-readable message override

Constructs a validator-format plist and passes it through the real
jf/gptel-scope--build-violation-info, so the output always matches
what production code produces.  The only logic owned by this factory
is generating a realistic :message when one is not provided."
  (let* ((path (plist-get props :path))
         (command (plist-get props :command))
         (provider (plist-get props :provider))
         (operation (plist-get props :operation))
         (metadata (plist-get props :metadata))
         (explicit-resource (plist-get props :resource))
         (explicit-message (plist-get props :message))
         ;; Generate a human-readable message if not provided
         (message (or explicit-message
                      (helpers-spec--generate-violation-message
                       error-code path command provider operation)))
         ;; Real validators always include :resource as a fallback.
         ;; build-violation-info's default case uses (or :resource :path),
         ;; so we derive :resource from contextual fields when not explicit.
         (resource (or explicit-resource command path provider))
         ;; Derive validation-type from tool name. The only bash-validated
         ;; tool is run_bash_command; everything else flows through the
         ;; filesystem validator and uses 'path.
         (validation-type (if (string= tool "run_bash_command") 'bash 'path))
         ;; Build validator-format input plist. The real validation
         ;; entrypoint tags results with :validation-type; this factory
         ;; reproduces that so build-violation-info reads it directly.
         (validator-plist
          (append
           (list :error error-code
                 :message message
                 :operation operation
                 :metadata metadata
                 :resource resource
                 :validation-type validation-type)
           (when path (list :path path))
           (when command (list :command command))
           (when provider (list :provider provider)))))
    (jf/gptel-scope--build-violation-info validator-plist tool)))

(defun helpers-spec--generate-violation-message (error-code path command provider operation)
  "Generate a realistic human-readable message for ERROR-CODE.
PATH, COMMAND, PROVIDER, OPERATION provide context for the message."
  (pcase error-code
    ("path_out_of_scope"
     (format "Path not in %s scope: %s" (or operation "read") (or path "unknown")))
    ("path_denied"
     (format "Path denied by scope: %s" (or path "unknown")))
    ("not-in-scope"
     (format "Path not in %s scope: %s" (or operation "read") (or path "unknown")))
    ("denied-pattern"
     (format "Path denied by scope: %s" (or path "unknown")))
    ("command_denied"
     (format "Command '%s' is in deny list" (or command "unknown")))
    ("command-not-allowed"
     (format "No bash_tools configuration found. Command denied: %s"
             (or command "unknown")))
    ("cloud_auth_denied"
     (format "Cloud authentication denied: %s (%s provider)"
             (or command "unknown") (or provider "unknown")))
    ("not-in-org-roam-patterns"
     (format "Pattern not in org-roam configuration: %s"
             (or path command "unknown")))
    ("unknown-org-roam-tool"
     (format "Unknown org-roam tool: %s" (or command "unknown")))
    (_ (format "Validation failed: %s" error-code))))

;;; Drawer Fixture Helpers

(defun jf/gptel-test--render-drawer (alist)
  "Render an org `:PROPERTIES:' drawer block from ALIST.
ALIST is a list of (KEY . VALUES) entries where KEY is an org property
keyword like :GPTEL_SCOPE_READ and VALUES is either a list (multi-value:
first emitted as bare key, rest as KEY+) or a string (scalar).  Returns
the drawer text including a trailing newline.

The closed set of acceptable keys is enumerated in
`register/vocabulary/drawer-key-set' (the seven :GPTEL_SCOPE_* keys).
This helper does not enforce the closed set — callers that need that
discipline should validate input before rendering."
  (let ((lines (list ":PROPERTIES:")))
    (dolist (entry alist)
      (let ((key (car entry))
            (val (cdr entry)))
        (cond
         ((stringp val)
          (push (format "%s: %s" key val) lines))
         ((listp val)
          (let ((first t))
            (dolist (v val)
              (push (format "%s%s: %s" key (if first "" "+") v) lines)
              (setq first nil)))))))
    (push ":END:" lines)
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

(defmacro jf/gptel-test--with-scope-drawer (alist &rest body)
  "Run BODY in a temp buffer whose `:PROPERTIES:' drawer is built from ALIST.
The buffer is in `org-mode' with point at `point-min' before BODY runs.
After the drawer, a minimal chat-mode initial-content stub
\(`#+begin_user' / `#+end_user') is inserted so the buffer matches the
shape of a real session.org.  ALIST has the same shape as
`jf/gptel-test--render-drawer'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert (jf/gptel-test--render-drawer ,alist))
     (insert "#+begin_user\n\n#+end_user\n")
     (org-mode)
     (goto-char (point-min))
     ,@body))

;;; Self-tests for the drawer fixture helpers
;;
;; Exercising the helper from inside helpers-spec.el itself documents
;; its expected behaviour and gives the suite a canary if the org-mode
;; multi-value reader's contract drifts under us.

(describe "jf/gptel-test--render-drawer"
  (it "wraps a single multi-value entry in a :PROPERTIES:/:END: block"
    (let ((text (jf/gptel-test--render-drawer
                 '((:GPTEL_SCOPE_READ . ("/a" "/b"))))))
      (expect text :to-equal
              ":PROPERTIES:\n:GPTEL_SCOPE_READ: /a\n:GPTEL_SCOPE_READ+: /b\n:END:\n")))

  (it "emits a scalar value as a single bare key line"
    (let ((text (jf/gptel-test--render-drawer
                 '((:GPTEL_SCOPE_CLOUD_AUTH . "warn")))))
      (expect text :to-equal
              ":PROPERTIES:\n:GPTEL_SCOPE_CLOUD_AUTH: warn\n:END:\n")))

  (it "renders an empty alist as a bare drawer"
    (expect (jf/gptel-test--render-drawer '())
            :to-equal ":PROPERTIES:\n:END:\n")))

(describe "jf/gptel-test--with-scope-drawer"
  (it "round-trips a multi-value list through org-entry-get-multivalued-property"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/a" "/b")))
      (expect (org-entry-get-multivalued-property (point-min) "GPTEL_SCOPE_READ")
              :to-equal '("/a" "/b"))))

  (it "leaves point at point-min before BODY runs"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/a")))
      (expect (point) :to-equal (point-min))))

  (it "puts the buffer in org-mode"
    (jf/gptel-test--with-scope-drawer
        '((:GPTEL_SCOPE_READ . ("/a")))
      (expect major-mode :to-equal 'org-mode))))

;;; Provide

(provide 'helpers-spec)

;;; helpers-spec.el ends here
