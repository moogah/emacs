;;; tool-routing-spec.el --- Tool routing/dispatching tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; CONSOLIDATED TESTS: Tool routing via jf/gptel-scope--infer-validation-type
;;
;; Sources:
;;   - tools/test/unit/config-spec.el (2 routing tests via infer-validation-type)
;;   - New tests for comprehensive routing coverage
;;
;; Tests that jf/gptel-scope--infer-validation-type dispatches tools to the
;; correct validator based on tool category (path, pattern, bash, meta).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load dependencies
(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (scope-test-dir (expand-file-name ".." test-dir))
       (scope-dir (expand-file-name ".." scope-test-dir))
       (gptel-dir (expand-file-name ".." scope-dir))
       (sessions-dir (expand-file-name "sessions" gptel-dir)))
  (require 'gptel-session-constants (expand-file-name "constants.el" sessions-dir))
  (require 'gptel-session-logging (expand-file-name "logging.el" sessions-dir))
  (require 'jf-gptel-scope-core (expand-file-name "scope-core.el" scope-dir))
  (require 'jf-gptel-scope-shell-tools (expand-file-name "scope-shell-tools.el" scope-dir)))

;;; Tool Routing Tests

(describe "jf/gptel-scope--infer-validation-type"

  (describe "path validation routing"
    (it "routes read_file to path validation"
      (expect (jf/gptel-scope--infer-validation-type "read_file") :to-equal 'path))

    (it "routes write_file_in_scope to path validation"
      (expect (jf/gptel-scope--infer-validation-type "write_file_in_scope") :to-equal 'path))

    (it "routes edit_file_in_scope to path validation"
      (expect (jf/gptel-scope--infer-validation-type "edit_file_in_scope") :to-equal 'path))

    (it "routes list_project_files to path validation"
      (expect (jf/gptel-scope--infer-validation-type "list_project_files") :to-equal 'path))

    (it "routes treesitter tools to path validation"
      (expect (jf/gptel-scope--infer-validation-type "get_syntax_tree") :to-equal 'path)
      (expect (jf/gptel-scope--infer-validation-type "list_functions") :to-equal 'path)))

  (describe "bash validation routing"
    (it "routes run_bash_command to bash validation"
      (expect (jf/gptel-scope--infer-validation-type "run_bash_command") :to-equal 'bash)))

  (describe "pattern validation routing"
    (it "routes org-roam tools to pattern validation"
      (expect (jf/gptel-scope--infer-validation-type "create_roam_node_in_scope") :to-equal 'pattern)
      (expect (jf/gptel-scope--infer-validation-type "add_roam_tags_in_scope") :to-equal 'pattern)
      (expect (jf/gptel-scope--infer-validation-type "link_roam_nodes_in_scope") :to-equal 'pattern)))

  (describe "meta validation routing"
    (it "routes PersistentAgent to meta validation"
      (expect (jf/gptel-scope--infer-validation-type "PersistentAgent") :to-equal 'meta))

    (it "routes request_scope_expansion to meta validation"
      (expect (jf/gptel-scope--infer-validation-type "request_scope_expansion") :to-equal 'meta)))

  (describe "unknown tools"
    (it "returns nil for unknown tool names"
      (expect (jf/gptel-scope--infer-validation-type "nonexistent_tool") :to-be nil))

    (it "returns nil for empty string"
      (expect (jf/gptel-scope--infer-validation-type "") :to-be nil))))

(provide 'tool-routing-spec)

;;; tool-routing-spec.el ends here
