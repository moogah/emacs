;;; test-org-roam-integration.el --- Integration tests for org-roam node creation from org-mode sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; ERT tests verifying org-roam node creation works correctly with
;; org-mode session content.  Tests check that created nodes have
;; proper org-roam structure (PROPERTIES drawer, ID, title), valid
;; org-mode content (no markdown artifacts), and correct tag/ref
;; handling.
;;
;; These tests mock org-roam dependencies so they can run without
;; an actual org-roam database.
;;
;; Spec reference: gptel/tools - org-roam, create_roam_node
;;
;; Test naming convention: test-create-roam-node-<scenario-slug>

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; Test helpers

(defvar org-roam-integration-test--temp-dirs nil
  "List of temp directories to clean up after tests.")

(defun org-roam-integration-test--make-temp-dir ()
  "Create a temporary directory for testing and register for cleanup."
  (let ((dir (make-temp-file "gptel-roam-test-" t)))
    (push dir org-roam-integration-test--temp-dirs)
    dir))

(defun org-roam-integration-test--cleanup ()
  "Remove all temporary directories created during tests."
  (dolist (dir org-roam-integration-test--temp-dirs)
    (when (file-directory-p dir)
      (delete-directory dir t)))
  (setq org-roam-integration-test--temp-dirs nil))

(defun org-roam-integration-test--read-file (filepath)
  "Read FILEPATH and return its contents as a string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun org-roam-integration-test--call-create-node (title &optional tags content refs subdirectory capture-session-metadata)
  "Call the create_roam_node tool function with mocked org-roam dependencies.
Uses a temporary directory as org-roam-directory.
Returns plist with :result (the function return value) and :dir (the temp dir)."
  (let* ((temp-dir (org-roam-integration-test--make-temp-dir))
         (org-roam-directory temp-dir)
         (test-id "test-id-12345678")
         result)
    ;; Mock org-roam functions
    (cl-letf (((symbol-function 'org-roam-node-slug)
               (lambda (node)
                 (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))))
              ((symbol-function 'org-roam-node-create)
               (lambda (&rest _args)
                 'mock-node))
              ((symbol-function 'org-id-new)
               (lambda () test-id))
              ((symbol-function 'org-roam-db-update-file)
               (lambda (_filepath) nil))
              ((symbol-function 'jf/gptel--get-session-context)
               (lambda ()
                 '(:session-id "sess-abc123"
                   :agent-name "test-agent"
                   :model "claude-opus-4"
                   :backend "anthropic"
                   :timestamp "2026-03-11T10:00:00")))
              ((symbol-function 'jf/markdown-to-org)
               (lambda (text)
                 ;; Simple mock: convert markdown to org format
                 ;; Order matters: bold before headings to avoid ** heading confusion
                 (let ((result text))
                   ;; Bold: match **text** on same line only (no newlines in match)
                   (setq result (replace-regexp-in-string "\\*\\*\\([^*\n]+\\)\\*\\*" "*\\1*" result))
                   ;; Headings: ## before # to avoid double-matching
                   (setq result (replace-regexp-in-string "^## " "** " result))
                   (setq result (replace-regexp-in-string "^# " "* " result))
                   ;; Replace opening code fences (``` followed by language and newline)
                   (setq result (replace-regexp-in-string "```\\([a-z]+\\)\n" "#+begin_src \\1\n" result))
                   ;; Replace closing code fences (bare ```)
                   (setq result (replace-regexp-in-string "```" "#+end_src" result))
                   result))))
      (setq result (funcall
                    (lambda (title &optional tags content refs subdirectory capture-session-metadata)
                      (let* ((subdir (or subdirectory ""))
                             (slug (org-roam-node-slug (org-roam-node-create :title title)))
                             (filename (format "%s-%s.org"
                                              (format-time-string "%Y%m%d%H%M%S")
                                              slug))
                             (target-dir (expand-file-name subdir org-roam-directory))
                             (filepath (expand-file-name filename target-dir))
                             (id (org-id-new))
                             (session-ctx (when capture-session-metadata
                                            (jf/gptel--get-session-context))))

                        ;; Ensure target directory exists
                        (unless (file-directory-p target-dir)
                          (make-directory target-dir t))

                        ;; Create the file with org-roam structure
                        (with-temp-file filepath
                          (insert (format ":PROPERTIES:\n:ID:       %s\n" id))

                          ;; Add gptel session metadata if requested and available
                          (when session-ctx
                            (when-let ((session-id (plist-get session-ctx :session-id)))
                              (insert (format ":GPTEL_SESSION: %s\n" session-id)))
                            (when-let ((agent (plist-get session-ctx :agent-name)))
                              (insert (format ":GPTEL_AGENT: %s\n" agent)))
                            (when-let ((model (plist-get session-ctx :model)))
                              (insert (format ":GPTEL_MODEL: %s\n" model)))
                            (when-let ((backend (plist-get session-ctx :backend)))
                              (insert (format ":GPTEL_BACKEND: %s\n" backend)))
                            (when-let ((timestamp (plist-get session-ctx :timestamp)))
                              (insert (format ":GPTEL_CREATED: %s\n" timestamp))))

                          (insert ":END:\n")
                          (insert (format "#+title: %s\n" title))

                          (when tags
                            (insert (format "#+filetags: :%s:\n" (mapconcat 'identity tags ":"))))
                          (when refs
                            (dolist (ref refs)
                              (insert (format "#+roam_refs: %s\n" ref))))
                          (insert "\n")
                          (when content
                            (insert (jf/markdown-to-org content))
                            (insert "\n")))

                        ;; Update org-roam database
                        (org-roam-db-update-file filepath)

                        (format "Created new org-roam node:\n\nTitle: %s\nID: %s\nFile: %s\nDirectory: %s\nTags: %s\nRefs: %s%s\n\n[Use read_roam_node to view the node, or link_roam_nodes to connect it to other nodes]"
                                title
                                id
                                filepath
                                (if (string-empty-p subdir) "root" subdir)
                                (if tags (mapconcat 'identity tags ", ") "none")
                                (if refs (mapconcat 'identity refs ", ") "none")
                                (if session-ctx
                                    (format "\nSession metadata captured: %s"
                                            (plist-get session-ctx :session-id))
                                  ""))))
                    title tags content refs subdirectory capture-session-metadata)))
    (list :result result :dir temp-dir)))

;;; Tests: Basic node structure

(ert-deftest test-create-roam-node-properties-drawer ()
  "Verify created node has proper PROPERTIES drawer with ID.

The PROPERTIES drawer must be at the top of the file and contain
a valid :ID: property."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Test Note"))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (should (= 1 (length files)))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; Must start with :PROPERTIES:
          (should (string-prefix-p ":PROPERTIES:" content))
          ;; Must have :ID: property
          (should (string-match-p ":ID:       test-id-12345678" content))
          ;; Must have :END:
          (should (string-match-p ":END:" content))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-title ()
  "Verify created node has correct #+title header."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "My Session Notes"))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          (should (string-match-p "\n#\\+title: My Session Notes\n" content))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-tags ()
  "Verify filetags are formatted correctly with colon delimiters."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Tagged Note" '("gptel" "session" "test")))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          (should (string-match-p "\n#\\+filetags: :gptel:session:test:\n" content))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-refs ()
  "Verify reference URLs are added as #+roam_refs lines."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Ref Note" nil nil '("https://example.com" "https://docs.org")))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          (should (string-match-p "\n#\\+roam_refs: https://example.com\n" content))
          (should (string-match-p "\n#\\+roam_refs: https://docs.org\n" content))))
    (org-roam-integration-test--cleanup)))

;;; Tests: Org-mode content validity

(ert-deftest test-create-roam-node-org-content-no-markdown-headings ()
  "Verify content has no markdown heading artifacts (# or ##).

When markdown content is passed, it should be converted to org-mode
format with * headings, not # headings."
  (unwind-protect
      (let* ((md-content "# Overview\n\nSome text here.\n\n## Details\n\nMore details.")
             (response (org-roam-integration-test--call-create-node
                        "Markdown Test" nil md-content))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; Should have org headings, not markdown headings
          (should (string-match-p "\n\\* Overview\n" content))
          (should (string-match-p "\n\\*\\* Details\n" content))
          ;; Should NOT have bare markdown headings (but #+title: is fine)
          (let ((lines (split-string content "\n")))
            (dolist (line lines)
              (unless (string-prefix-p "#+" line)
                (should-not (string-match-p "^## " line)))))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-org-content-no-markdown-bold ()
  "Verify bold text uses org-mode *bold* not markdown **bold**."
  (unwind-protect
      (let* ((md-content "This has **bold text** in it.")
             (response (org-roam-integration-test--call-create-node
                        "Bold Test" nil md-content))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; Should have org bold (*text*), not markdown bold (**text**)
          (should (string-match-p "\\*bold text\\*" content))
          (should-not (string-match-p "\\*\\*bold text\\*\\*" content))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-org-content-no-markdown-code-fences ()
  "Verify code blocks use #+begin_src/#+end_src not triple backticks."
  (unwind-protect
      (let* ((md-content "Example:\n\n```python\nprint('hello')\n```\n\nDone.")
             (response (org-roam-integration-test--call-create-node
                        "Code Test" nil md-content))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; Should have org src blocks
          (should (string-match-p "#\\+begin_src python" content))
          (should (string-match-p "#\\+end_src" content))
          ;; Should NOT have markdown code fences
          (should-not (string-match-p "```" content))))
    (org-roam-integration-test--cleanup)))

;;; Tests: Session metadata integration

(ert-deftest test-create-roam-node-session-metadata ()
  "Verify session metadata is captured in PROPERTIES drawer when requested."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Session Note" '("gptel") "Session content" nil nil t))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; All session metadata should be in PROPERTIES drawer
          (should (string-match-p ":GPTEL_SESSION: sess-abc123" content))
          (should (string-match-p ":GPTEL_AGENT: test-agent" content))
          (should (string-match-p ":GPTEL_MODEL: claude-opus-4" content))
          (should (string-match-p ":GPTEL_BACKEND: anthropic" content))
          (should (string-match-p ":GPTEL_CREATED: 2026-03-11T10:00:00" content))
          ;; Metadata must be between :PROPERTIES: and :END:
          (should (string-match-p ":PROPERTIES:\n:ID:.*\n:GPTEL_SESSION:" content))))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-no-session-metadata-by-default ()
  "Verify session metadata is NOT included when capture_session_metadata is nil."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Plain Note" nil "Some content"))
             (dir (plist-get response :dir))
             (files (directory-files dir nil "\\.org$")))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) dir))))
          ;; Should NOT have session metadata
          (should-not (string-match-p ":GPTEL_SESSION:" content))
          (should-not (string-match-p ":GPTEL_AGENT:" content))
          (should-not (string-match-p ":GPTEL_MODEL:" content))))
    (org-roam-integration-test--cleanup)))

;;; Tests: Subdirectory handling

(ert-deftest test-create-roam-node-subdirectory ()
  "Verify node is created in subdirectory when specified."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Subdir Note" nil "Content" nil "gptel"))
             (dir (plist-get response :dir))
             (subdir (expand-file-name "gptel" dir))
             (files (directory-files subdir nil "\\.org$")))
        ;; Subdirectory should exist
        (should (file-directory-p subdir))
        ;; File should be in subdirectory
        (should (= 1 (length files)))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) subdir))))
          (should (string-prefix-p ":PROPERTIES:" content))
          (should (string-match-p "\n#\\+title: Subdir Note\n" content))))
    (org-roam-integration-test--cleanup)))

;;; Tests: Return value

(ert-deftest test-create-roam-node-return-value ()
  "Verify the return string contains expected node information."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Return Test" '("tag1") nil '("https://ref.com") "notes"))
             (result (plist-get response :result)))
        (should (string-match-p "Title: Return Test" result))
        (should (string-match-p "ID: test-id-12345678" result))
        (should (string-match-p "Directory: notes" result))
        (should (string-match-p "Tags: tag1" result))
        (should (string-match-p "Refs: https://ref.com" result)))
    (org-roam-integration-test--cleanup)))

(ert-deftest test-create-roam-node-return-value-session-metadata ()
  "Verify return string mentions captured session metadata."
  (unwind-protect
      (let* ((response (org-roam-integration-test--call-create-node
                        "Meta Return Test" nil nil nil nil t))
             (result (plist-get response :result)))
        (should (string-match-p "Session metadata captured: sess-abc123" result)))
    (org-roam-integration-test--cleanup)))

;;; Tests: Full org-mode session content integration

(ert-deftest test-create-roam-node-from-org-session ()
  "Integration test: create node from realistic org-mode session content.

Simulates the full workflow of creating an org-roam node from
content generated during a gptel session, verifying the complete
output is valid org-mode."
  (unwind-protect
      (let* ((session-content
              "# Session Summary\n\nThis session explored **Emacs configuration** with gptel.\n\n## Key Findings\n\n- Org-roam integration works well\n- Session metadata is preserved\n\n## Code Example\n\n```elisp\n(setq gptel-model \"claude-opus-4\")\n```\n\n## Next Steps\n\nContinue with **testing framework** improvements.")
             (response (org-roam-integration-test--call-create-node
                        "Gptel Session: Emacs Config"
                        '("gptel" "emacs" "session")
                        session-content
                        nil
                        "gptel"
                        t))
             (dir (plist-get response :dir))
             (subdir (expand-file-name "gptel" dir))
             (files (directory-files subdir nil "\\.org$")))
        (should (= 1 (length files)))
        (let ((content (org-roam-integration-test--read-file
                        (expand-file-name (car files) subdir))))
          ;; Structural validity
          (should (string-prefix-p ":PROPERTIES:" content))
          (should (string-match-p ":END:" content))
          (should (string-match-p "\n#\\+title: Gptel Session: Emacs Config\n" content))
          (should (string-match-p "\n#\\+filetags: :gptel:emacs:session:" content))

          ;; Session metadata present
          (should (string-match-p ":GPTEL_SESSION:" content))
          (should (string-match-p ":GPTEL_AGENT:" content))

          ;; Content converted to org-mode (no markdown artifacts)
          (should (string-match-p "\n\\* Session Summary\n" content))
          (should (string-match-p "\n\\*\\* Key Findings\n" content))
          (should (string-match-p "\n#\\+begin_src elisp\n" content))
          (should (string-match-p "\n#\\+end_src\n" content))

          ;; No markdown artifacts remain - check line by line
          ;; (^ works in string-match-p for single-line strings)
          (let ((lines (split-string content "\n")))
            (dolist (line lines)
              (unless (string-prefix-p "#+" line)
                ;; No bare markdown headings
                (should-not (string-match-p "^# [^*]" line))
                (should-not (string-match-p "^## " line)))))
          (should-not (string-match-p "```" content))
          (should-not (string-match-p "\\*\\*Emacs configuration\\*\\*" content))
          (should-not (string-match-p "\\*\\*testing framework\\*\\*" content))))
    (org-roam-integration-test--cleanup)))

(provide 'test-org-roam-integration)

;;; test-org-roam-integration.el ends here
