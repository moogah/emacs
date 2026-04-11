;;; interfaces.el --- Scope system interface contracts -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst scope/interface--error-codes
  '(;; --- Path access denied ---
    ;; Produced when a path matches a deny pattern.
    ;; Producers: validate-path-tool, validate-operation
    "denied-pattern"

    ;; Produced when a path is not covered by any allow pattern.
    ;; Producers: validate-path-tool, validate-operation
    "not-in-scope"

    ;; --- Bash command denied ---
    ;; Produced when a command appears in bash_tools.deny list.
    ;; Producer: validate-pipeline-commands
    "command_denied"

    ;; Produced when no bash_tools section exists in scope config.
    ;; Producer: validate-bash-tool
    "command-not-allowed"

    ;; --- Parse errors ---
    ;; Produced when bash-parser cannot fully parse the command.
    ;; Producer: validate-parse-completeness
    "parse_incomplete"

    ;; --- Cloud authentication ---
    ;; Produced when cloud auth detected and policy is "deny".
    ;; Producer: validate-cloud-auth
    "cloud_auth_denied"

    ;; Produced when cloud provider not in allowed_providers list.
    ;; Producer: validate-cloud-auth
    "cloud_provider_denied"

    ;; --- Configuration errors ---
    ;; Produced when scope config has invalid structure.
    ;; Producer: validate-bash-tool
    "malformed-config")
  "Canonical error codes for scope validation.

Every validator MUST produce only codes from this list.
Every consumer (build-violation-info, format-tool-error) MUST handle
every code in this list.

Codes not in this list:
- \"unknown-tool\" (from check-tool-permission, not a validation error)
- \"no_scope_config\" (from the macro, before validation runs)
- \"tool_exception\" (from the macro, unexpected errors)
- \"allow-once\" (from check-tool-permission, signals success not error)")

(defconst scope/interface--error-resource-fields
  '(("denied-pattern"       . :resource)
    ("not-in-scope"         . :resource)
    ("command_denied"       . :command)
    ("command-not-allowed"  . :resource)
    ("parse_incomplete"     . :command)
    ("cloud_auth_denied"    . :provider)
    ("cloud_provider_denied" . :provider)
    ("malformed-config"     . :resource))
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
          (unless (memq vtype '(path pattern bash meta))
            (push (format ":validation-type %S not in (path pattern bash meta)" vtype)
                  errors))))

      (when errors
        (mapconcat #'identity (nreverse errors) "; ")))))

(defun scope/interface--compute-allow-once-resource (validation-type tool-name args)
  "Compute the allow-once resource key for VALIDATION-TYPE.
TOOL-NAME and ARGS are the tool invocation parameters.

This is the canonical implementation.  Both trigger-inline-expansion
and check-allow-once should produce the same value for the same inputs."
  (pcase validation-type
    ('path (expand-file-name (car args)))
    ('pattern (format "%s:%s" tool-name (car args)))
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
both the path-tool matcher and the file-ops matcher agree.")

(provide 'scope-interfaces)
;;; interfaces.el ends here
