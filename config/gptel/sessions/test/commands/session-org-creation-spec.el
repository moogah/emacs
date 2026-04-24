;;; session-org-creation-spec.el --- Session file creation per Decision 4 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the session creation path after the
;; `gptel-chat-state-persistence' rework (design Decision 4 / Decision 6).
;;
;; Invariants verified:
;;
;; 1. `session.org' is pre-populated with a `:PROPERTIES:' drawer
;;    containing `GPTEL_PRESET' (and `GPTEL_PARENT_SESSION_ID' when an
;;    agent session declares a parent) followed by the chat-mode
;;    empty-user-block template.  The drawer shape matches what the
;;    save hook writes on first save, so creation → open → save is a
;;    no-op on disk.
;;
;; 2. The created session file has NO Local Variables block written
;;    during creation.  Chat-mode's block format is self-describing;
;;    persistence is plain `save-buffer' (Decision 18).
;;
;; 3. No `metadata.yml' sidecar is written during session creation.
;;    The drawer embedded in `session.org' is the single authoritative
;;    session-level configuration source (Decision 6).
;;
;; Tests use `with-captured-io' from `persistence-test-helpers' to
;; intercept filesystem writes so all production persistence code
;; runs for real; only Emacs primitives (`write-region',
;; `make-directory', `make-symbolic-link') are mocked.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; The shared `with-captured-io' helper lives with the other gptel
;; behavioral tests under `config/gptel/test/'.  Add that directory
;; to `load-path' so we can `require' it here.
(add-to-list 'load-path
             (expand-file-name
              "../../../test"
              (file-name-directory (or load-file-name buffer-file-name))))
(require 'persistence-test-helpers)

;; Load production modules.
(require 'gptel-session-constants)
(require 'gptel-session-logging)
(require 'gptel-session-filesystem)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

(describe "jf/gptel--create-session-core writes pre-populated session.org"

  (describe "session.org initial content (Decision 4)"

    (it "contains the PROPERTIES drawer with GPTEL_PRESET followed by empty user block"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-decision4-20260421120000"
         "/sessions/sess-decision4-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-decision4-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-be-truthy)
          (expect content :to-equal
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_PRESET: executor\n"
                          ":END:\n"
                          "#+begin_user\n"
                          "\n"
                          "#+end_user\n")))))

    (it "encodes the preset name symbol as its symbol-name in the drawer"
      ;; A differently-named preset lands in the drawer verbatim as a
      ;; string, not as an interned-symbol reader syntax.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-preset-name-20260421120000"
         "/sessions/sess-preset-name-20260421120000"
         'researcher)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-preset-name-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-match "^:GPTEL_PRESET: researcher$"))))

    (it "contains no markdown heading or ### placeholder"
      ;; The pre-chat-mode creation path seeded session.md with a
      ;; markdown `###' placeholder (or `# <session-name>').  Per
      ;; Decision 18, chat-mode files never carry markdown markup.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-no-markdown-20260421120000"
         "/sessions/sess-no-markdown-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-no-markdown-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :not :to-match "^###")
          (expect content :not :to-match "^# "))))

    (it "honours a caller-provided initial-content override verbatim"
      ;; The helper still accepts a caller-supplied initial-content
      ;; string.  When provided, the helper must use it verbatim and
      ;; must not wrap it with a generated drawer.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-custom-content-20260421120000"
         "/sessions/sess-custom-content-20260421120000"
         'executor
         "custom\nseed\n")
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-custom-content-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-equal "custom\nseed\n")))))

  (describe "no Local Variables block during creation (Decision 18)"

    (it "does not write a Local Variables block into session.org"
      ;; Chat-mode's block format is self-describing; there is no
      ;; `gptel--save-state' round-trip and no file-local
      ;; `gptel--bounds' to serialise.  The created session.org MUST
      ;; NOT contain a Local Variables block — org-style
      ;; (`# Local Variables:' / `# End:'), html-comment-style
      ;; (`<!-- Local Variables: -->' / `<!-- End: -->'), or the
      ;; generic `Local Variables:' marker that `hack-local-variables'
      ;; would recognize.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-no-locals-20260421120000"
         "/sessions/sess-no-locals-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-no-locals-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-be-truthy)
          (expect content :not :to-match "Local Variables:")
          (expect content :not :to-match "<!-- Local Variables: -->")
          (expect content :not :to-match "^# Local Variables:")))))

  (describe "no metadata.yml sidecar (Decision 6)"

    (it "does not write any file named metadata.yml under the branch directory"
      ;; The drawer embedded in `session.org' is now authoritative.
      ;; Session creation MUST NOT write the legacy `metadata.yml'
      ;; sidecar; capture the full write set and assert.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-no-metadata-20260421120000"
         "/sessions/sess-no-metadata-20260421120000"
         'executor)
        (let ((paths (hash-table-keys captured-files)))
          (expect (cl-some (lambda (p) (string-suffix-p "/metadata.yml" p)) paths)
                  :to-be nil)))))

  (describe "agent-session drawer includes GPTEL_PARENT_SESSION_ID"

    ;; When `jf/gptel--create-session-core' is invoked with a
    ;; non-empty PARENT-SESSION-ID (the agent-creation path), the
    ;; drawer gains a `:GPTEL_PARENT_SESSION_ID:' line so the
    ;; chat-mode restore path can install
    ;; `jf/gptel--parent-session-id' buffer-locally on first open
    ;; (design Decision 3 / Decision 4).

    (it "omits GPTEL_PARENT_SESSION_ID for standalone / branch sessions (nil parent)"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-no-parent-20260421120000"
         "/sessions/sess-no-parent-20260421120000"
         'executor
         nil nil nil nil)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-no-parent-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "omits GPTEL_PARENT_SESSION_ID when parent-session-id is an empty string"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-empty-parent-20260421120000"
         "/sessions/sess-empty-parent-20260421120000"
         'executor
         nil nil nil "")
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-empty-parent-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :not :to-match "GPTEL_PARENT_SESSION_ID"))))

    (it "writes GPTEL_PARENT_SESSION_ID into the drawer for agent sessions"
      (with-captured-io
        (jf/gptel--create-session-core
         "agent-writer-20260421120000"
         "/sessions/agent-writer-20260421120000"
         'executor
         nil nil nil
         "parent-session-20260101000000")
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/agent-writer-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-be-truthy)
          (expect content :to-match ":GPTEL_PRESET: executor")
          (expect content :to-match
                  ":GPTEL_PARENT_SESSION_ID: parent-session-20260101000000")
          ;; Both drawer lines land before the :END: marker.
          (expect content :to-match
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_PRESET: executor\n"
                          ":GPTEL_PARENT_SESSION_ID: parent-session-20260101000000\n"
                          ":END:\n")))))))

(describe "jf/gptel--initial-session-content builds the drawer-prefixed template"

  ;; Direct coverage for the helper — independent of filesystem
  ;; mocking.  These tests exercise the string-assembly contract that
  ;; `jf/gptel--create-session-core' relies on.

  (it "returns the drawer + empty user block for a preset alone"
    (expect (jf/gptel--initial-session-content 'executor)
            :to-equal
            (concat ":PROPERTIES:\n"
                    ":GPTEL_PRESET: executor\n"
                    ":END:\n"
                    "#+begin_user\n"
                    "\n"
                    "#+end_user\n")))

  (it "inserts GPTEL_PARENT_SESSION_ID between GPTEL_PRESET and :END:"
    (expect (jf/gptel--initial-session-content 'executor "parent-abc")
            :to-equal
            (concat ":PROPERTIES:\n"
                    ":GPTEL_PRESET: executor\n"
                    ":GPTEL_PARENT_SESSION_ID: parent-abc\n"
                    ":END:\n"
                    "#+begin_user\n"
                    "\n"
                    "#+end_user\n")))

  (it "treats an empty string parent-session-id as missing"
    (expect (jf/gptel--initial-session-content 'executor "")
            :to-equal
            (jf/gptel--initial-session-content 'executor)))

  (it "treats nil parent-session-id as missing"
    (expect (jf/gptel--initial-session-content 'executor nil)
            :to-equal
            (jf/gptel--initial-session-content 'executor))))

(provide 'session-org-creation-spec)
;;; session-org-creation-spec.el ends here
