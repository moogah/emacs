;;; filesystem-scope-integration-spec.el --- Integration tests: filesystem tools through scope validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; INTEGRATION TESTS: Exercise the real production read_file tool through the
;; full scope validation pipeline, verifying the complete chain from tool
;; invocation through to the gptel callback.
;;
;; What makes these integration tests (vs the existing contract/unit tests):
;;
;; - Uses the REAL read_file tool from scope-filesystem-tools.el (not a
;;   throwaway test tool created inline)
;; - Uses REAL scope config parsed from YAML (not hand-built plists)
;; - Uses REAL path validation (not mocked check-tool-permission)
;; - Only mocks at the boundary: config loading (to inject our YAML) and
;;   expansion UI (to simulate user choices without transient)
;;
;; Tests verify four key properties:
;;   1. The macro runs the appropriate validation function (path validator)
;;   2. Expansion UI responses are handled: deny, allow-once, add-to-scope
;;   3. The tool body executes with correct arguments on success
;;   4. The gptel callback receives properly-formatted JSON results
;;
;; The gptel callback contract:
;;   gptel's process-tool-result expects a single string argument (the tool
;;   result). For async scoped tools, the macro calls:
;;     (funcall callback (json-serialize RESULT-PLIST))
;;   gptel then calls gptel--to-string on the result (no-op for strings)
;;   and stores it in the tool-call plist as :result.

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'json)

;; Load dependencies via path resolution
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       ;; test/integration/ -> test/ -> scope/ -> gptel/ -> config/
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir)))
  ;; Scope helpers (matchers, mock builders)
  (require 'helpers-spec (expand-file-name "helpers-spec.el" scope-test-dir))
  ;; Production scope modules
  (require 'jf-gptel-scope-validation (expand-file-name "scope/scope-validation.el" gptel-dir))
  (require 'jf-gptel-scope-tool-wrapper (expand-file-name "scope/scope-tool-wrapper.el" gptel-dir))
  (require 'jf-gptel-scope-expansion (expand-file-name "scope/scope-expansion.el" gptel-dir))
  (require 'jf-gptel-scope-filesystem-tools (expand-file-name "scope/scope-filesystem-tools.el" gptel-dir))
  ;; gptel itself (for gptel-tool struct accessors and tool registry)
  (require 'gptel))

;;; Test Infrastructure

(defvar fs-integ--callback-result nil
  "Parsed plist from the gptel callback's JSON string argument.")

(defvar fs-integ--callback-raw nil
  "Raw string argument passed to gptel callback (what gptel--to-string sees).")

(defvar fs-integ--temp-dir nil
  "Temporary directory for test files.")

(defvar fs-integ--temp-files nil
  "List of temp files to clean up.")

(defun fs-integ--gptel-callback (result-json)
  "Simulate gptel's process-tool-result callback.
Captures both the raw string and parsed plist for assertions.
RESULT-JSON is the JSON string the scoped tool passes to its callback."
  (setq fs-integ--callback-raw result-json)
  (setq fs-integ--callback-result
        (json-parse-string result-json :object-type 'plist)))

(defun fs-integ--find-tool (name)
  "Find tool NAME in gptel--known-tools registry."
  (cl-block nil
    (dolist (category-entry gptel--known-tools)
      (dolist (tool-entry (cdr category-entry))
        (when (string= (car tool-entry) name)
          (cl-return (cdr tool-entry)))))))

(defun fs-integ--make-scope-yaml (read-paths write-paths deny-paths)
  "Build scope YAML string with given path lists.
READ-PATHS, WRITE-PATHS, DENY-PATHS are lists of glob pattern strings."
  (let ((format-paths (lambda (paths)
                        (if paths
                            (mapconcat (lambda (p) (format "    - \"%s\"" p))
                                       paths "\n")
                          "    []"))))
    (format "paths:
  read:
%s
  write:
%s
  execute:
    []
  modify:
    []
  deny:
%s

bash_tools:
  deny: []

cloud:
  auth_detection: \"warn\"

security:
  enforce_parse_complete: true
  max_coverage_threshold: 0.8
"
            (funcall format-paths read-paths)
            (funcall format-paths write-paths)
            (funcall format-paths deny-paths))))

(defun fs-integ--load-config-from-yaml (yaml-string)
  "Parse YAML-STRING through the real scope config pipeline.
Returns a scope config plist identical to what jf/gptel-scope--load-config returns."
  (jf/gptel-scope--load-schema
   (jf/gptel-scope-yaml--parse-string yaml-string)))

(defun fs-integ--create-temp-file (content &optional suffix)
  "Create a temp file with CONTENT in fs-integ--temp-dir.
Returns the full path. SUFFIX defaults to \".txt\"."
  (let* ((file (make-temp-file
                (expand-file-name "integ-" fs-integ--temp-dir)
                nil (or suffix ".txt"))))
    (with-temp-file file
      (insert content))
    (push file fs-integ--temp-files)
    file))

;;; Test Suites

;; ============================================================
;; SUITE 1: Path allowed — tool executes and callback fires
;; ============================================================

(describe "Filesystem scope integration: path allowed"

  (before-each
    (setq fs-integ--callback-result nil)
    (setq fs-integ--callback-raw nil)
    (setq fs-integ--temp-dir (make-temp-file "fs-integ-" t))
    (setq fs-integ--temp-files nil)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (dolist (f fs-integ--temp-files)
      (when (file-exists-p f) (delete-file f)))
    (when (and fs-integ--temp-dir (file-exists-p fs-integ--temp-dir))
      (delete-directory fs-integ--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "read_file succeeds when path matches read scope"
    (let* ((test-file (fs-integ--create-temp-file "hello integration test"))
           ;; Build config that allows our temp dir
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (expect tool :to-be-truthy)
      (expect (gptel-tool-async tool) :to-be t)

      ;; Mock only config loading — everything else is real
      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      ;; Invoke the REAL read_file tool function
      ;; Async signature: (callback filepath)
      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               test-file)

      ;; Callback was invoked with a JSON string (gptel contract)
      (expect fs-integ--callback-raw :to-be-truthy)
      (expect (stringp fs-integ--callback-raw) :to-be t)

      ;; Result is parseable JSON with success
      (expect (plist-get fs-integ--callback-result :success) :to-be t)
      (expect (plist-get fs-integ--callback-result :content)
              :to-equal "hello integration test")))

  (it "callback result is a valid JSON string consumable by gptel--to-string"
    (let* ((test-file (fs-integ--create-temp-file "gptel contract test"))
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               test-file)

      ;; gptel--to-string returns strings unchanged
      ;; Verify our result would survive that transformation
      (expect (gptel--to-string fs-integ--callback-raw)
              :to-equal fs-integ--callback-raw)

      ;; Verify the JSON round-trips cleanly
      (let ((re-parsed (json-parse-string fs-integ--callback-raw :object-type 'plist)))
        (expect (plist-get re-parsed :success) :to-be t))))

  (it "read_file returns file_not_found for missing file in scope"
    (let* ((missing-path (expand-file-name "nonexistent.txt" fs-integ--temp-dir))
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               missing-path)

      ;; Tool body ran (path was in scope) but file doesn't exist
      (expect (plist-get fs-integ--callback-result :success) :not :to-be t)
      (expect (plist-get fs-integ--callback-result :error)
              :to-equal "file_not_found")))

  (it "uses real path validation — validate-path-tool is called, not mocked"
    (let* ((test-file (fs-integ--create-temp-file "validation check"))
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Spy AND call-through: observe that the real validator runs
      (spy-on 'jf/gptel-scope--validate-path-tool :and-call-through)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               test-file)

      ;; Real validator was called
      (expect 'jf/gptel-scope--validate-path-tool :to-have-been-called)
      ;; And it allowed the path (tool body ran)
      (expect (plist-get fs-integ--callback-result :success) :to-be t))))


;; ============================================================
;; SUITE 2: Path denied — expansion UI responses
;; ============================================================

(describe "Filesystem scope integration: path denied"

  (before-each
    (setq fs-integ--callback-result nil)
    (setq fs-integ--callback-raw nil)
    (setq fs-integ--temp-dir (make-temp-file "fs-integ-" t))
    (setq fs-integ--temp-files nil)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (dolist (f fs-integ--temp-files)
      (when (file-exists-p f) (delete-file f)))
    (when (and fs-integ--temp-dir (file-exists-p fs-integ--temp-dir))
      (delete-directory fs-integ--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "triggers expansion UI when path is outside read scope"
    (let* ((test-file (fs-integ--create-temp-file "secret data"))
           ;; Config allows /workspace/** but NOT our temp dir
           (yaml (fs-integ--make-scope-yaml
                  '("/workspace/**")
                  '("/workspace/**")
                  nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Spy on expansion UI — don't invoke callback (simulates user hasn't responded yet)
      (spy-on 'jf/gptel-scope-prompt-expansion)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               test-file)

      ;; Expansion UI was triggered (not a sync error return)
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)
      ;; gptel callback was NOT called yet (waiting for user)
      (expect fs-integ--callback-result :to-be nil)))

  (it "triggers expansion UI when path is in deny list"
    (let* ((test-file (fs-integ--create-temp-file "denied content"))
           ;; Config: deny our temp dir explicitly
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil
                  (list (concat fs-integ--temp-dir "/**"))))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      (spy-on 'jf/gptel-scope-prompt-expansion)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               test-file)

      ;; Deny takes priority over read allowance — expansion triggers
      (expect 'jf/gptel-scope-prompt-expansion :to-have-been-called)))

  (describe "user denies expansion"

    (it "returns scope error to gptel callback without executing tool body"
      (let* ((test-file (fs-integ--create-temp-file "should not be read"))
             (yaml (fs-integ--make-scope-yaml '("/workspace/**") nil nil))
             (config (fs-integ--load-config-from-yaml yaml))
             (tool (fs-integ--find-tool "read_file")))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        ;; Simulate user choosing "Deny" in transient menu
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize
                            (list :success :false
                                  :user_denied t
                                  :message "User denied scope expansion request.")))))

        (funcall (gptel-tool-function tool)
                 #'fs-integ--gptel-callback
                 test-file)

        ;; Callback was invoked with an error (not success)
        (expect fs-integ--callback-result :to-be-truthy)
        (expect (plist-get fs-integ--callback-result :success) :not :to-be t)
        ;; Error type indicates scope violation
        (expect (plist-get fs-integ--callback-result :error) :to-be-truthy))))

  (describe "user allows once"

    (it "executes tool body and returns result to gptel callback"
      (let* ((test-file (fs-integ--create-temp-file "allow-once content"))
             (yaml (fs-integ--make-scope-yaml '("/workspace/**") nil nil))
             (config (fs-integ--load-config-from-yaml yaml))
             (tool (fs-integ--find-tool "read_file")))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        ;; Simulate "Allow once": add to allow-once list, then signal success
        ;; This mirrors what the real allow-once-action does
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (jf/gptel-scope-add-to-allow-once-list
                   (plist-get violation-info :tool)
                   (or (plist-get violation-info :allow-once-resource)
                       (plist-get violation-info :resource)))
                  (funcall callback
                           (json-serialize '(:success t :allowed_once t)))))

        (funcall (gptel-tool-function tool)
                 #'fs-integ--gptel-callback
                 test-file)

        ;; Tool body executed — file was actually read
        (expect fs-integ--callback-result :to-be-truthy)
        (expect (plist-get fs-integ--callback-result :success) :to-be t)
        (expect (plist-get fs-integ--callback-result :content)
                :to-equal "allow-once content")))

    (it "allow-once permission is consumed (single-use)"
      (let* ((test-file (fs-integ--create-temp-file "single use test"))
             (yaml (fs-integ--make-scope-yaml '("/workspace/**") nil nil))
             (config (fs-integ--load-config-from-yaml yaml))
             (tool (fs-integ--find-tool "read_file"))
             (expansion-count 0))

        (spy-on 'jf/gptel-scope--load-config :and-return-value config)
        ;; Track how many times expansion UI is shown
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (cl-incf expansion-count)
                  (jf/gptel-scope-add-to-allow-once-list
                   (plist-get violation-info :tool)
                   (or (plist-get violation-info :allow-once-resource)
                       (plist-get violation-info :resource)))
                  (funcall callback
                           (json-serialize '(:success t :allowed_once t)))))

        ;; First call: triggers expansion, gets allow-once
        (funcall (gptel-tool-function tool)
                 #'fs-integ--gptel-callback
                 test-file)
        (expect expansion-count :to-equal 1)
        (expect (plist-get fs-integ--callback-result :success) :to-be t)

        ;; Reset callback state
        (setq fs-integ--callback-result nil)

        ;; Second call to same file: should trigger expansion AGAIN
        ;; because the allow-once was consumed on the retry inside the first call
        (funcall (gptel-tool-function tool)
                 #'fs-integ--gptel-callback
                 test-file)
        (expect expansion-count :to-equal 2))))

  (describe "user adds to scope"

    (it "executes tool body after scope is permanently expanded"
      (let* ((test-file (fs-integ--create-temp-file "permanent scope content"))
             ;; Start with a config that doesn't include our temp dir
             (yaml (fs-integ--make-scope-yaml '("/workspace/**") nil nil))
             (config (fs-integ--load-config-from-yaml yaml))
             ;; Build an expanded config that includes our temp dir
             (expanded-yaml (fs-integ--make-scope-yaml
                             (list "/workspace/**"
                                   (concat fs-integ--temp-dir "/**"))
                             nil nil))
             (expanded-config (fs-integ--load-config-from-yaml expanded-yaml))
             (tool (fs-integ--find-tool "read_file"))
             (load-count 0))

        ;; First load returns restrictive config, subsequent loads return expanded
        (spy-on 'jf/gptel-scope--load-config
                :and-call-fake
                (lambda ()
                  (cl-incf load-count)
                  (if (= load-count 1) config expanded-config)))

        ;; Simulate "Add to scope": signal success (scope.yml was updated)
        ;; The macro will reload config on retry and find the path now allowed
        (spy-on 'jf/gptel-scope-prompt-expansion
                :and-call-fake
                (lambda (violation-info callback _patterns _tool-name)
                  (funcall callback
                           (json-serialize
                            (list :success t
                                  :patterns_added
                                  (vector (concat fs-integ--temp-dir "/**")))))))

        (funcall (gptel-tool-function tool)
                 #'fs-integ--gptel-callback
                 test-file)

        ;; Config was loaded twice: initial check + retry after expansion
        (expect load-count :to-equal 2)

        ;; Tool body executed with the real file content
        (expect (plist-get fs-integ--callback-result :success) :to-be t)
        (expect (plist-get fs-integ--callback-result :content)
                :to-equal "permanent scope content")))))


;; ============================================================
;; SUITE 3: gptel tool flow continuity
;; Verify the callback chain is compatible with gptel's expectations
;; ============================================================

(describe "Filesystem scope integration: gptel flow continuity"

  (before-each
    (setq fs-integ--callback-result nil)
    (setq fs-integ--callback-raw nil)
    (setq fs-integ--temp-dir (make-temp-file "fs-integ-" t))
    (setq fs-integ--temp-files nil)
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (after-each
    (dolist (f fs-integ--temp-files)
      (when (file-exists-p f) (delete-file f)))
    (when (and fs-integ--temp-dir (file-exists-p fs-integ--temp-dir))
      (delete-directory fs-integ--temp-dir t))
    (when (boundp 'jf/gptel-scope--allow-once-list)
      (setq jf/gptel-scope--allow-once-list nil)))

  (it "tool struct has :async t (gptel dispatches to callback-first path)"
    (let ((tool (fs-integ--find-tool "read_file")))
      (expect tool :to-be-truthy)
      ;; gptel checks (gptel-tool-async tool-spec) to decide invocation style
      ;; For async: (apply fn process-tool-result arg-values)
      ;; For sync:  (apply fn arg-values), then (funcall process-tool-result result)
      (expect (gptel-tool-async tool) :to-be t)))

  (it "tool is registered in gptel--known-tools under filesystem category"
    (let* ((fs-category (assoc "filesystem" gptel--known-tools))
           (tool-entry (assoc "read_file" (cdr fs-category))))
      (expect tool-entry :to-be-truthy)
      (expect (gptel-tool-name (cdr tool-entry)) :to-equal "read_file")))

  (it "callback receives exactly one string argument (gptel process-tool-result contract)"
    (let* ((test-file (fs-integ--create-temp-file "callback contract"))
           (yaml (fs-integ--make-scope-yaml
                  (list (concat fs-integ--temp-dir "/**"))
                  nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file"))
           (callback-arg-count nil)
           (callback-arg-type nil))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)

      ;; Custom callback that inspects its arguments
      (funcall (gptel-tool-function tool)
               (lambda (&rest args)
                 (setq callback-arg-count (length args))
                 (setq callback-arg-type (type-of (car args))))
               test-file)

      ;; gptel's process-tool-result takes exactly 1 arg
      (expect callback-arg-count :to-equal 1)
      ;; That arg must be a string (JSON)
      (expect callback-arg-type :to-equal 'string)))

  (it "no_scope_config error returns JSON through callback (not a signal)"
    ;; When scope.yml is missing, the tool should still invoke the callback
    ;; (not throw an error), so gptel's flow continues
    (let ((tool (fs-integ--find-tool "read_file")))

      ;; No config available
      (spy-on 'jf/gptel-scope--load-config :and-return-value nil)

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               "/some/path.txt")

      ;; Callback was invoked (flow didn't break)
      (expect fs-integ--callback-result :to-be-truthy)
      (expect (plist-get fs-integ--callback-result :error)
              :to-equal "no_scope_config")))

  (it "tool exception returns JSON through callback (not a signal)"
    ;; Even if something unexpected goes wrong inside the tool, the callback
    ;; should fire so gptel can advance its FSM
    (let* ((yaml (fs-integ--make-scope-yaml
                  '("/**") nil nil))
           (config (fs-integ--load-config-from-yaml yaml))
           (tool (fs-integ--find-tool "read_file")))

      (spy-on 'jf/gptel-scope--load-config :and-return-value config)
      ;; Force an error inside the tool body by making expand-file-name fail
      (spy-on 'expand-file-name :and-call-fake
              (lambda (&rest _) (error "Simulated internal error")))

      (funcall (gptel-tool-function tool)
               #'fs-integ--gptel-callback
               "/some/path.txt")

      ;; Callback was still invoked (error handler in macro caught it)
      (expect fs-integ--callback-result :to-be-truthy)
      (expect (plist-get fs-integ--callback-result :error)
              :to-equal "tool_exception"))))

(provide 'filesystem-scope-integration-spec)

;;; filesystem-scope-integration-spec.el ends here
