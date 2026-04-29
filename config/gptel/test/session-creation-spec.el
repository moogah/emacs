;;; session-creation-spec.el --- Behavioral tests for session creation (write-side) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests verifying the write-side of session creation via
;; `jf/gptel--create-session-core'.
;;
;; Invariants verified (post-`gptel-scope-in-org-properties'):
;;
;; 1. `session.org' is pre-populated with a `:PROPERTIES:' drawer
;;    containing `GPTEL_PRESET' (and `GPTEL_PARENT_SESSION_ID' for
;;    agent sessions) followed by the chat-mode empty-user-block
;;    template.  The drawer is the authoritative session-level
;;    configuration source (design Decision 4 / Decision 6).
;;
;; 2. No `metadata.yml' sidecar is written during session creation.
;;    The previous `metadata.yml'-backed path has been removed.
;;
;; 3. No `scope.yml' sidecar is written during session creation.
;;    The preset's scope profile is rendered into the `session.org'
;;    drawer via `jf/gptel-scope-profile--render-drawer-text' as
;;    `:GPTEL_SCOPE_*:' multi-value keys (cycle-2 rewire-session-creation;
;;    `register/boundary/scope-profile-applicator' Mode 2a).
;;
;; Uses `with-captured-io' macro from persistence-test-helpers to
;; intercept all filesystem writes.  All custom persistence code runs
;; for real; only Emacs primitives (write-region, make-directory,
;; make-symbolic-link, etc.) are mocked.
;;
;; Drawer assertions read the captured `session.org' content into a
;; temp org buffer and use `org-entry-get-multivalued-property' so the
;; assertion exercises the same parsing path the loader uses
;; (`register/shape/drawer-text-block' / line-shape parity with
;; `org-entry-put-multivalued-property').

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Ensure test directory is on load-path for require
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'persistence-test-helpers)

;; Load production modules
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)
(require 'org)

(defun jf/gptel-test--drawer-multi-value (content key)
  "Read multi-value drawer property KEY from session.org CONTENT.
CONTENT is the raw string written by `with-temp-file' inside
`jf/gptel--create-session-core'.  KEY is a string naming a drawer
property (without leading colon), e.g. \"GPTEL_SCOPE_READ\".

Returns the list of values stored under KEY, using the same
`org-entry-get-multivalued-property' path the production loader uses
to parse `:KEY:' lines emitted by
`jf/gptel-scope-profile--render-drawer-text'."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (org-entry-get-multivalued-property (point-min) key)))

(defun jf/gptel-test--drawer-scalar (content key)
  "Read scalar drawer property KEY from session.org CONTENT.
Like `jf/gptel-test--drawer-multi-value' but for single-valued keys
\(e.g. `GPTEL_PRESET', `GPTEL_SCOPE_CLOUD_AUTH').  Returns the string
value or nil when KEY is absent."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (org-entry-get (point-min) key)))

(describe "Session creation (write-side)"

  (describe "Basic session creation"

    (it "creates branches/main directory structure"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (expect (captured-dir-p captured-dirs "/sessions/test-session-123/branches")
                :to-be-truthy)
        (expect (captured-dir-p captured-dirs "/sessions/test-session-123/branches/main")
                :to-be-truthy)))

    (it "does not write metadata.yml during session creation"
      ;; Post-`session-creation-drawer-prepopulate': the drawer
      ;; embedded in `session.org' replaces the sidecar.  Capture
      ;; every write and assert no path ending in `metadata.yml'
      ;; landed in the hash.
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((paths (hash-table-keys captured-files)))
          (expect (cl-some (lambda (p) (string-suffix-p "/metadata.yml" p)) paths)
                  :to-be nil))))

    (it "writes session.org with pre-populated PROPERTIES drawer by default"
      ;; Per design Decision 4, a fresh `session.org' begins with a
      ;; `:PROPERTIES:' drawer containing `GPTEL_PRESET' so the
      ;; chat-mode restore path applies the preset on first open.
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :to-be-truthy)
          (expect content :to-equal
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_PRESET: executor\n"
                          ":END:\n"
                          "#+begin_user\n"
                          "\n"
                          "#+end_user\n")))))

    (it "writes session.org with custom initial content when provided"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         "# My Session\n\n")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :to-equal "# My Session\n\n"))))

    (it "creates current symlink pointing to branches/main"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor)
        (let ((target (captured-symlink-target
                       captured-symlinks
                       "/sessions/test-session-123/current")))
          (expect target :to-equal "branches/main")))))

  (describe "Return value"

    (it "returns plist with session info"
      (with-captured-io
        (let ((result (jf/gptel--create-session-core
                       "test-session-123" "/sessions/test-session-123" 'executor)))
          (expect (plist-get result :session-id) :to-equal "test-session-123")
          (expect (plist-get result :session-dir) :to-equal "/sessions/test-session-123")
          (expect (plist-get result :branch-name) :to-equal "main")
          (expect (plist-get result :branch-dir) :to-be-truthy)
          (expect (plist-get result :session-file) :to-be-truthy)))))

  (describe "Preset is recorded in session.org drawer"

    (it "encodes preset name into GPTEL_PRESET line"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'researcher)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :to-match ":GPTEL_PRESET: researcher")))))

  (describe "Parent-session-id threading for agent sessions"

    (it "omits GPTEL_PARENT_SESSION_ID when parent-session-id is nil"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         nil nil nil nil)
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "omits GPTEL_PARENT_SESSION_ID when parent-session-id is empty string"
      (with-captured-io
        (jf/gptel--create-session-core
         "test-session-123" "/sessions/test-session-123" 'executor
         nil nil nil "")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/test-session-123/branches/main/session.org")))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "writes GPTEL_PARENT_SESSION_ID when parent-session-id is a non-empty string"
      (with-captured-io
        (jf/gptel--create-session-core
         "agent-abc-20260421120000"
         "/sessions/agent-abc-20260421120000"
         'executor
         nil nil nil
         "parent-xyz-20260101000000")
        (let ((content (captured-file-content
                        captured-files
                        "/sessions/agent-abc-20260421120000/branches/main/session.org")))
          (expect content :to-match ":GPTEL_PARENT_SESSION_ID: parent-xyz-20260101000000")))))

  (describe "Scope profile integration"

    ;; Post-`rewire-session-creation' (cycle-2 commit be6b80c): the
    ;; preset's resolved scope profile is rendered into `session.org's
    ;; `:PROPERTIES:' drawer via Mode 2a
    ;; (`jf/gptel-scope-profile--render-drawer-text'). No `scope.yml'
    ;; sidecar is written. Each test below asserts on drawer content
    ;; via `org-entry-get-multivalued-property' (the same parsing path
    ;; the production loader uses) and verifies absence of `scope.yml'.

    (it "does not write scope.yml during session creation"
      ;; Negative invariant: `register/boundary/scope-profile-applicator'
      ;; Mode 2a is now the sole route from a profile-resolved scope-plist
      ;; to drawer state. The legacy `scope.yml' write path was removed
      ;; in cycle-2; this test catches regressions where the YAML write
      ;; step is accidentally re-introduced.
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("/home/user/project")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset)
          (let ((paths (hash-table-keys captured-files)))
            (expect (cl-some (lambda (p) (string-suffix-p "/scope.yml" p)) paths)
                    :to-be nil)
            ;; Symmetric file-exists-p check via the captured-files mock.
            (expect (file-exists-p
                     "/sessions/test-session-123/branches/main/scope.yml")
                    :to-be nil)))))

    (it "renders preset scope into session.org drawer when available"
      ;; Set up a preset with inline scope defaults
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("/home/user/project")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/session.org")))
            (expect content :to-be-truthy)
            (expect (jf/gptel-test--drawer-multi-value content "GPTEL_SCOPE_READ")
                    :to-equal '("/home/user/project"))
            ;; Preset name remains in the drawer alongside scope keys
            (expect (jf/gptel-test--drawer-scalar content "GPTEL_PRESET")
                    :to-equal "test-preset")))))

    (it "expands ${project_root} into the drawer when project-root provided"
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("${project_root}/src")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset
           nil nil "/home/user/my-project")
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/session.org")))
            (expect content :to-be-truthy)
            (expect (jf/gptel-test--drawer-multi-value content "GPTEL_SCOPE_READ")
                    :to-equal '("/home/user/my-project/src"))
            ;; Unexpanded variable should not appear anywhere in the file.
            (expect content :not :to-match "\\${project_root}")))))

    (it "deep-merges worktree-paths with preset scope into the drawer"
      (let ((jf/gptel-preset--scope-defaults
             `((test-preset . (:paths (:read ("/base/path")))))))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'test-preset
           nil
           '(:paths (:read ("/extra/path")))
           nil)
          (let* ((content (captured-file-content
                           captured-files
                           "/sessions/test-session-123/branches/main/session.org"))
                 (read-paths (jf/gptel-test--drawer-multi-value
                              content "GPTEL_SCOPE_READ")))
            (expect content :to-be-truthy)
            ;; Both paths should be present after deep merge.
            ;; Order is preset-first then worktree (per
            ;; `jf/gptel-scope-profile--merge-lists').
            (expect read-paths :to-equal '("/base/path" "/extra/path"))))))

    (it "renders a minimal drawer with no GPTEL_SCOPE_* keys when preset has no scope configuration"
      ;; Per `jf/gptel-scope-profile--render-drawer-text', an empty
      ;; scope-plist (the resolver returns nil for a preset with no
      ;; scope defaults) yields a drawer carrying only `:GPTEL_PRESET:'.
      ;; This is the renderer-side invariant; it is independent of the
      ;; loader's empty-drawer behaviour, which `disposition-empty-
      ;; drawer-collapse' resolved separately to deny-all defaults.
      (let ((jf/gptel-preset--scope-defaults nil))
        (with-captured-io
          (jf/gptel--create-session-core
           "test-session-123" "/sessions/test-session-123" 'no-scope-preset)
          (let ((content (captured-file-content
                          captured-files
                          "/sessions/test-session-123/branches/main/session.org")))
            (expect content :to-be-truthy)
            ;; Preset is recorded.
            (expect (jf/gptel-test--drawer-scalar content "GPTEL_PRESET")
                    :to-equal "no-scope-preset")
            ;; No GPTEL_SCOPE_* keys are present.
            (dolist (key '("GPTEL_SCOPE_READ"
                           "GPTEL_SCOPE_WRITE"
                           "GPTEL_SCOPE_MODIFY"
                           "GPTEL_SCOPE_EXECUTE"
                           "GPTEL_SCOPE_DENY"
                           "GPTEL_SCOPE_CLOUD_AUTH"
                           "GPTEL_SCOPE_CLOUD_PROVIDERS"))
              (expect (jf/gptel-test--drawer-scalar content key) :to-be nil)
              (expect (jf/gptel-test--drawer-multi-value content key) :to-be nil))
            ;; Belt-and-braces: regex check the raw text.
            (expect content :not :to-match ":GPTEL_SCOPE_")))))))

(provide 'session-creation-spec)
;;; session-creation-spec.el ends here
