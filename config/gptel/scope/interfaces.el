;;; interfaces.el --- Scope system interface contracts -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst scope/interface--error-codes
  '(;; --- Path access denied ---
    ;; Produced when a path matches a deny pattern.
    ;; Producer: jf/gptel-scope--validate-path-operation
    "denied-pattern"

    ;; Produced when a path is not covered by any allow pattern
    ;; for the requested operation.
    ;; Producer: jf/gptel-scope--validate-path-operation
    "not-in-scope"

    ;; --- Parse errors ---
    ;; Produced when bash-parser cannot fully parse the command
    ;; and security.enforce_parse_complete is true.
    ;; Producer: jf/gptel-scope--validate-parse-completeness
    "parse_incomplete"

    ;; --- Cloud authentication ---
    ;; Produced when cloud auth detected and policy is "deny".
    ;; Producer: jf/gptel-scope--validate-cloud-auth
    "cloud_auth_denied"

    ;; Produced when cloud provider not in allowed_providers list.
    ;; Producer: jf/gptel-scope--validate-cloud-auth
    "cloud_provider_denied")
  "Canonical error codes for scope validation.

Every validator MUST produce only codes from this list.
Every consumer (build-violation-info, format-tool-error) MUST handle
every code in this list.

Codes that intentionally live outside this vocabulary:
- \"no_scope_config\"  (macro, before validation runs)
- \"tool_exception\"   (macro, unexpected errors caught around body)
- \"invalid_cloud_auth_mode\" (misconfigured scope.yml, not a user action)
- \"file_not_found\", \"string_not_found\", \"execution-failed\"
  (tool-level errors produced after validation has passed)")

(defconst scope/interface--error-resource-fields
  '(("denied-pattern"       . :resource)
    ("not-in-scope"         . :resource)
    ("parse_incomplete"     . :command)
    ("cloud_auth_denied"    . :provider)
    ("cloud_provider_denied" . :provider))
  "Maps each error code to the plist key holding the resource identifier.

build-violation-info uses this to extract :resource for the expansion UI.
If a validator returns an error code not in this alist, the resource
will be nil and the expansion UI cannot show what was denied.")

(defun scope/interface--validate-validation-result (result)
  "Validate that RESULT is a well-formed validation result plist.
Returns nil if valid, error string if invalid."
  (cl-block validate
    (unless (listp result)
      (cl-return-from validate
        (format "Expected plist, got %s" (type-of result))))

    ;; :allowed is required
    (unless (plist-member result :allowed)
      (cl-return-from validate "Missing required key :allowed"))

    (if (plist-get result :allowed)
        ;; Success case: no other keys required
        nil

      ;; Denial case: check required error fields
      (let ((errors nil))
        (unless (plist-member result :error)
          (push "Denied result missing :error" errors))
        (unless (plist-member result :message)
          (push "Denied result missing :message" errors))

        ;; :error must be a known code
        (when (plist-member result :error)
          (let ((code (plist-get result :error)))
            (unless (member code scope/interface--error-codes)
              (push (format ":error %S not in canonical error codes" code) errors))))

        (when errors
          (mapconcat #'identity (nreverse errors) "; "))))))

(defun scope/interface--validate-violation-info (info)
  "Validate that INFO is a well-formed violation-info plist.
Returns nil if valid, error string if invalid."
  (cl-block validate
    (unless (listp info)
      (cl-return-from validate
        (format "Expected plist, got %s" (type-of info))))

    (let ((errors nil))
      ;; Required fields
      (dolist (key '(:tool :resource :reason :validation-type))
        (unless (plist-member info key)
          (push (format "Missing required key %S" key) errors))
        (when (and (plist-member info key)
                   (null (plist-get info key)))
          (push (format "Required key %S is nil" key) errors)))

      ;; :validation-type must be a known symbol
      (when (plist-member info :validation-type)
        (let ((vtype (plist-get info :validation-type)))
          (unless (memq vtype '(path bash))
            (push (format ":validation-type %S not in (path bash)" vtype)
                  errors))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

(defun scope/interface--compute-allow-once-resource (validation-type tool-name args)
  "Compute the allow-once resource key for VALIDATION-TYPE.
TOOL-NAME and ARGS are the tool invocation parameters.

This is the canonical implementation.  The scope-tool-wrapper macro
and scope-validation's trigger-inline-expansion both delegate to
`jf/gptel-scope--compute-allow-once-resource', which must produce
the same value as this reference implementation for the same inputs."
  (pcase validation-type
    ('path (expand-file-name (car args)))
    ('bash (format "%s:%s" (car args) (expand-file-name (cadr args))))
    (_ nil)))

(defconst scope/interface--glob-test-cases
  '(;; Pattern          Path                          Expected
    ("/workspace/**"    "/workspace/file.txt"         t)
    ("/workspace/**"    "/workspace/sub/file.txt"     t)
    ("/workspace/**"    "/other/file.txt"             nil)
    ("/workspace/*.el"  "/workspace/init.el"          t)
    ("/workspace/*.el"  "/workspace/sub/init.el"      nil)
    ("**/.git/**"       "/workspace/.git/config"      t)
    ("**/.git/**"       "/workspace/sub/.git/HEAD"    t)
    ("/tmp/*"           "/tmp/file"                   t)
    ("/tmp/*"           "/tmp/sub/file"               nil))
  "Test cases for glob pattern matching.
Each entry is (PATTERN PATH EXPECTED-RESULT).

Any function claiming to implement glob matching for scope validation
must pass all of these cases.  Tests import this constant to verify
the canonical implementation honors the contract.")

(provide 'scope-interfaces)
;;; interfaces.el ends here
