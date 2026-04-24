;;; session-org-creation-spec.el --- Session file creation per Decision 18 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the session creation path after the
;; `gptel-chat-mode' rework (design Decision 9 / Decision 18).
;;
;; Invariants verified:
;;
;; 1. `session.org' contains EXACTLY the chat-mode empty-user-block
;;    template (`#+begin_user\n\n#+end_user\n') — no markdown heading,
;;    no `###' placeholder, no Local Variables block.
;;
;; 2. The created session file has NO Local Variables block written
;;    during creation.  Chat-mode's block format is self-describing;
;;    persistence is plain `save-buffer' (Decision 18).
;;
;; 3. `metadata.yml' still contains the canonical fields: `session_id',
;;    `created', `updated', `preset'.  The metadata path is unchanged
;;    by this rework — only the conversation-file contents change.
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
(require 'gptel-session-metadata)
(require 'gptel-scope-profiles)
(require 'gptel-session-commands)

(describe "jf/gptel--create-session-core writes chat-mode session.org"

  (describe "session.org initial content (Decision 18)"

    (it "contains exactly the chat-mode empty-user-block template"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-decision18-20260421120000"
         "/sessions/sess-decision18-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-decision18-20260421120000"
                                "/branches/main/session.org"))))
          (expect content :to-be-truthy)
          (expect content :to-equal "#+begin_user\n\n#+end_user\n"))))

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
          (expect content :not :to-match "^# ")))))

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

  (describe "metadata.yml population is preserved"

    ;; The metadata.yml schema is explicitly unchanged by Decision 18;
    ;; assert the canonical fields still land in it.

    (it "writes session_id into metadata.yml"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-metadata-20260421120000"
         "/sessions/sess-metadata-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-metadata-20260421120000"
                                "/branches/main/metadata.yml"))))
          (expect content :to-be-truthy)
          (expect content :to-match "session_id: \"sess-metadata-20260421120000\""))))

    (it "writes created timestamp into metadata.yml"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-metadata-20260421120000"
         "/sessions/sess-metadata-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-metadata-20260421120000"
                                "/branches/main/metadata.yml"))))
          (expect content :to-match
                  "created: \"[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"))))

    (it "writes updated timestamp into metadata.yml"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-metadata-20260421120000"
         "/sessions/sess-metadata-20260421120000"
         'executor)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-metadata-20260421120000"
                                "/branches/main/metadata.yml"))))
          (expect content :to-match
                  "updated: \"[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"))))

    (it "writes preset name into metadata.yml"
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-metadata-20260421120000"
         "/sessions/sess-metadata-20260421120000"
         'researcher)
        (let ((content (captured-file-content
                        captured-files
                        (concat "/sessions/sess-metadata-20260421120000"
                                "/branches/main/metadata.yml"))))
          (expect content :to-match "preset: \"researcher\""))))))

(provide 'session-org-creation-spec)
;;; session-org-creation-spec.el ends here
