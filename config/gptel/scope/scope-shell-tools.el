;;; scope-shell-tools.el --- GPTEL Scope Shell and Meta Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Jeff Farr

;;; Commentary:

;; Tool definitions for run_bash_command and request_scope_expansion.
;; Validation logic is in scope-validation.el.

;;; Code:

;; Dependencies


;; [[file:scope-shell-tools.org::*Dependencies][Dependencies:1]]
(require 'cl-lib)
(require 'jf-gptel-scope-tool-wrapper)
(require 'jf-gptel-scope-validation)
;; Dependencies:1 ends here

;; Constants


;; [[file:scope-shell-tools.org::*Constants][Constants:1]]
(defconst jf/gptel-bash--max-output-chars 10000
  "Maximum characters in command output before truncation.")

(defconst jf/gptel-bash--command-timeout 30
  "Timeout in seconds for bash command execution.")
;; Constants:1 ends here

;; Check Absolute Paths


;; [[file:scope-shell-tools.org::*Check Absolute Paths][Check Absolute Paths:1]]
(defun jf/gptel-bash--check-absolute-paths (command)
  "Check if COMMAND contains absolute paths.
Returns warning string if found, nil otherwise."
  (when (string-match "/[[:alnum:]_/-]+" command)
    "Warning: Command contains absolute path arguments. Directory scope may not protect these paths."))
;; Check Absolute Paths:1 ends here

;; Output Sanitization

;; Subprocess output reaches us as bytes.  Depending on the buffer's
;; coding system and the locale, those bytes can land in the buffer as
;; either proper Unicode characters (when decoded as UTF-8) or as raw
;; byte chars in Emacs's internal #x3FFF80..#x3FFFFF range (when the
;; decoder gives up on an invalid UTF-8 sequence or when a no-conversion
;; read is in effect).

;; Raw byte chars are NOT valid JSON values: =json-serialize= rejects
;; them with =wrong-type-argument json-value-p=.  We can't always
;; prevent them at capture time (the locale may say something we don't
;; expect, or the subprocess may emit genuinely invalid UTF-8), so this
;; helper post-processes captured output to guarantee a JSON-serializable
;; string regardless of what the subprocess emitted.

;; The coercion is lossless byte-wise: any raw byte char is re-emitted
;; as its Latin-1 codepoint (every byte 0..255 maps to a valid Unicode
;; code point in Latin-1, so the round-trip cannot fail).  Proper UTF-8
;; characters pass through unchanged.


;; [[file:scope-shell-tools.org::*Output Sanitization][Output Sanitization:1]]
(defun jf/gptel-bash--coerce-to-json-safe (str)
  "Return STR as a JSON-serializable multibyte string.
`json-serialize' rejects two shapes of string with
`wrong-type-argument json-value-p':

- Multibyte strings containing chars in the raw-byte range
  #x3FFF80..#x3FFFFF (bytes that failed UTF-8 decoding).
- Unibyte strings containing any byte >= #x80.

Both cases are coerced to a valid multibyte string by re-emitting
each problem byte as its Latin-1 codepoint (every byte 0..255 maps
to a valid Unicode code point in U+0000..U+00FF, so the round-trip
cannot fail).  ASCII-only strings and clean multibyte UTF-8 pass
through unchanged with no copy."
  (cond
   ((not (stringp str)) str)
   ;; Unibyte string with any high byte: decode as Latin-1 so every
   ;; byte becomes a valid Unicode code point.  ASCII-only unibyte
   ;; passes through unchanged (json-serialize accepts pure ASCII
   ;; unibyte strings).
   ((not (multibyte-string-p str))
    (if (cl-loop for c across str thereis (>= c #x80))
        (decode-coding-string str 'latin-1)
      str))
   ;; Multibyte string with raw-byte chars: encode back to bytes (raw
   ;; byte chars produce their own byte values), then decode as
   ;; Latin-1.  Proper multibyte UTF-8 chars pass through unchanged.
   ((cl-loop for c across str thereis (>= c #x3FFF80))
    (decode-coding-string
     (encode-coding-string str 'utf-8 t)
     'latin-1))
   (t str)))
;; Output Sanitization:1 ends here

;; Execute Command


;; [[file:scope-shell-tools.org::*Execute Command][Execute Command:1]]
(defun jf/gptel-bash--execute-command (command directory)
  "Execute COMMAND in DIRECTORY with timeout and output truncation.

Captured subprocess output is forced through `utf-8-unix' decoding
to keep behaviour stable regardless of the user's locale, and then
run through `jf/gptel-bash--coerce-to-json-safe' so any remaining
raw byte chars (from a subprocess that emits genuinely invalid
UTF-8, e.g. =cat= on a binary fragment) become Latin-1 code points
that `json-serialize' accepts.  Without this, downstream JSON
serialization would fail with =wrong-type-argument json-value-p=
and the wrapper would surface a `tool_exception' to the model."
  (cl-block jf/gptel-bash--execute-command
    (let* ((default-directory (file-truename (expand-file-name directory)))
           (output nil)
           (exit-code nil)
           (truncated nil)
           (warnings nil)
           (error-type nil)
           (max-output-chars jf/gptel-bash--max-output-chars))

      (condition-case err
          (with-timeout (jf/gptel-bash--command-timeout
                         (progn
                           (setq error-type "timeout")
                           (setq exit-code 124)
                           (setq output (format "Command execution timed out after %d seconds."
                                                jf/gptel-bash--command-timeout))
                           (setq warnings
                                 (list (format "Command timed out after %d seconds — consider using more specific filters (head, grep, tail, find with -maxdepth) to reduce execution time."
                                               jf/gptel-bash--command-timeout)))))
            (setq output
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (setq exit-code
                            (call-process shell-file-name nil t nil
                                          shell-command-switch command)))
                    (jf/gptel-bash--coerce-to-json-safe
                     (buffer-string)))))
        (error
         (cl-return-from jf/gptel-bash--execute-command
           (list :output (format "Command execution failed: %s" (error-message-string err))
                 :exit_code 1
                 :error "execution-failed"
                 :truncated nil
                 :warnings nil))))

      ;; Truncate output if too long
      (let ((original-length (length output)))
        (when (> original-length max-output-chars)
          (setq truncated t)
          (setq output
                (concat (substring output 0 max-output-chars)
                        (format "\n\n[Output truncated at %d chars. Total: %d chars. Narrow results with head, grep, tail, or other filters.]"
                                max-output-chars original-length)))))

      ;; Check for warnings
      (let ((path-warning (jf/gptel-bash--check-absolute-paths command)))
        (when path-warning
          (setq warnings (vector path-warning))))

      (list :output output
            :exit_code exit-code
            :truncated truncated
            :warnings warnings
            :error error-type))))
;; Execute Command:1 ends here

;; run_bash_command Tool


;; [[file:scope-shell-tools.org::*run_bash_command Tool][run_bash_command Tool:1]]
(gptel-make-scoped-tool
 "run_bash_command"
 "Execute shell command with semantic scope validation.

Commands are validated semantically — file operations extracted from the command
are checked against scope paths (read/write/execute/modify/deny).

Commands with zero file operations (version checks, help flags) are allowed automatically.

Relative paths in the command are resolved from the session's current working
directory (default-directory), which is set from the session context.

Security features:
- 30-second timeout
- Output truncation at 10,000 chars
- Seven-stage semantic validation pipeline
- Deny list blocks dangerous commands

Examples:
  run_bash_command('ls -la')
  run_bash_command('grep -r TODO . | head -20')
  run_bash_command('git log --oneline -10')"

 (list '(:name "command"
         :type string
         :description "Shell command to execute (pipes and redirects allowed)"))

 ;; No :operation — file operations are extracted from the command by
 ;; the bash semantic pipeline at validation time.

 (let* ((result (jf/gptel-bash--execute-command command default-directory))
        (exit-code (plist-get result :exit_code))
        (output (plist-get result :output))
        (truncated (plist-get result :truncated))
        (warnings (plist-get result :warnings))
        (success (zerop exit-code)))
   (list :success success
         :output output
         :exit_code exit-code
         :truncated truncated
         :warnings warnings)))
;; run_bash_command Tool:1 ends here

;; request_scope_expansion Tool

;; LLM uses this tool to explicitly request user approval for expanding scope.

;; The primary argument is =operation= (a closed enum) rather than a tool
;; name. =:validation-type= is derived directly from =operation= — the same
;; derivation the validation pipeline applies at =scope-validation.el:779-785=
;; — so this tool aligns with every other consumer of =:validation-type=
;; in the codebase rather than recovering it from a name string.

;; Operations outside the closed enum (=read=, =write=, =modify=, =execute=,
;; =bash=) short-circuit the callback with =:success false :error
;; "unknown_operation"= before reaching =jf/gptel-scope-prompt-expansion=.
;; This serves as a safety net for stale system prompts that may still pass
;; a tool-name string.


;; [[file:scope-shell-tools.org::*request_scope_expansion Tool][request_scope_expansion Tool:1]]
(gptel-make-tool
 :name "request_scope_expansion"
 :async t
 :description "Request user approval to expand scope with new patterns.

Displays interactive menu with 3 options:
1. Deny - Reject the expansion request
2. Add to scope - Permanently add patterns to the session's `:PROPERTIES:' drawer in `session.org'
3. Allow once - Temporarily allow for current turn only

Primary argument is `operation', identifying the kind of access being
requested. Valid values: \"read\", \"write\", \"modify\", \"execute\", \"bash\".
Pass the operation matching the tool you intend to invoke next (e.g.
\"read\" when about to call read_file_in_scope, \"bash\" when about to
call run_bash_command). Unknown values return :success false with
:error \"unknown_operation\".

Returns:
- success: true if approved (add-to-scope or allow-once)
- success: false if denied or operation is out of enum
- allowed_once: true if temporary permission granted
- patterns_added: list of patterns if permanently added"
 :args (list '(:name "operation"
               :type string
               :description "Kind of access being requested. One of: \"read\", \"write\", \"modify\", \"execute\", \"bash\". Filesystem operations resolve to path-shaped scope; \"bash\" resolves to bash-shaped scope.")
             '(:name "patterns"
               :type array
               :items (:type string)
               :description "Patterns to add (e.g., [\"/tmp/**\"] for files)")
             '(:name "justification"
               :type string
               :description "Explain why this access is needed."))
 :category "scope"
 :function
 (lambda (callback operation patterns justification)
   (when (vectorp patterns)
     (setq patterns (append patterns nil)))
   ;; Coerce the LLM-supplied operation string into the closed enum the
   ;; downstream pipeline expects. Filesystem operations land as
   ;; *keywords* (`:read', `:write', `:modify', `:execute') because every
   ;; consumer of violation-info `:operation' (--map-operation-to-drawer-key
   ;; at scope-expansion.el:91, --add-path-to-scope at scope-expansion.el:593)
   ;; matches against keywords. Bash is special: `:operation' is left nil
   ;; so Stage 1 of --add-to-scope's dispatcher handles it (the bash
   ;; routing has no single drawer key; pre-emptive bash expansion via
   ;; this tool surfaces as a structured `no-operation' denial today —
   ;; the LLM should request the underlying file operation instead).
   (let ((op-key (cond
                  ((equal operation "read")    :read)
                  ((equal operation "write")   :write)
                  ((equal operation "modify")  :modify)
                  ((equal operation "execute") :execute)
                  ((equal operation "bash")    :bash)
                  (t nil))))
     (cond
      ;; Out-of-enum: short-circuit. This includes stale tool_name
      ;; strings from pre-migration prompts (e.g. "read_file_in_scope").
      ((null op-key)
       (funcall callback
                (json-serialize
                 `(:success :false
                   :error "unknown_operation"
                   :message ,(format "Unknown operation %S. Valid values: read, write, modify, execute, bash."
                                     operation)))))
      (t
       (let* ((bashp (eq op-key :bash))
              (vtype (if bashp 'bash 'path))
              (violation-info
               (list :resource (car patterns)
                     :reason justification
                     :validation-type vtype
                     ;; `:operation' is the keyword for filesystem ops
                     ;; (composable with --map-operation-to-drawer-key);
                     ;; nil for bash so Stage 1 of the action handler
                     ;; intercepts (pre-emptive bash expansion has no
                     ;; canonical drawer-key routing).
                     :operation (unless bashp op-key)
                     :patterns patterns)))
         (jf/gptel-scope-prompt-expansion
          violation-info callback patterns "request_scope_expansion")))))))
;; request_scope_expansion Tool:1 ends here

;; Provide Feature


;; [[file:scope-shell-tools.org::*Provide Feature][Provide Feature:1]]
(provide 'jf-gptel-scope-shell-tools)
;;; scope-shell-tools.el ends here
;; Provide Feature:1 ends here
