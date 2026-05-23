;;; session-org-creation-spec.el --- Session file creation per Decision 4 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioral tests for the session creation path after the
;; `gptel-chat-state-persistence' rework (design Decision 4 / Decision 6)
;; and the §Addendum Decision B document-layout restructure.
;;
;; Invariants verified:
;;
;; 1. `session.org' is pre-populated with a file-level `:PROPERTIES:'
;;    config drawer at point-min containing `GPTEL_PRESET' (and
;;    `GPTEL_PARENT_SESSION_ID' when an agent session declares a
;;    parent), followed by a `* System Prompt' heading (folded via
;;    `:VISIBILITY: folded', body seeded from the preset's `:system')
;;    and a `* Chat' heading holding the chat-mode empty-user-block
;;    template.  The drawer shape matches what the save hook writes on
;;    first save, so creation → open → save is a no-op on disk
;;    (`register/shape/session-document-layout').
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
;; The chat parser verifies that the created document's `* System
;; Prompt' body is commentary, not a turn (design.md §Decision 12).
(require 'gptel-chat-parser)

(describe "jf/gptel--create-session-core writes pre-populated session.org"

  (describe "session.org initial content (Decision 4)"

    (it "contains the config drawer, a folded * System Prompt heading, and a * Chat heading with the empty user block"
      ;; Decision 4 / Decision 6 / §Addendum Decision B
      ;; (gptel-drawer-as-source-of-truth): the file-level config
      ;; drawer carries the resolved preset's chat-mode snapshot
      ;; alongside :GPTEL_PRESET:, then a `* System Prompt' heading
      ;; (folded) and a `* Chat' heading hold the document body.
      ;; Structural assertions (rather than literal-text equality) so
      ;; the test is stable as the `executor' preset's tool list /
      ;; model / temperature evolve.
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
          ;; File-level config drawer still at point-min — no heading,
          ;; no blank line precedes ":PROPERTIES:"
          ;; (register/shape/session-document-layout).
          (expect content :to-match "\\`:PROPERTIES:\n")
          (expect content :to-match ":GPTEL_PRESET: executor\n")
          (expect content :to-match "\n:END:\n")
          ;; Exactly one `* System Prompt' heading, carrying a folded
          ;; :VISIBILITY: drawer.
          (expect content :to-match "^\\* System Prompt[ \t]*$")
          (expect content :to-match
                  "^\\* System Prompt\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n")
          ;; Exactly one `* Chat' heading.
          (expect content :to-match "^\\* Chat[ \t]*$")
          ;; The empty user block lives under `* Chat'.
          (expect content :to-match "^\\* Chat\n#\\+begin_user\n\n#\\+end_user\n\\'")
          ;; The config drawer precedes both headings; `* System
          ;; Prompt' precedes `* Chat'.
          (let ((end-pos    (string-match "^:END:$" content))
                (sysp-pos   (string-match "^\\* System Prompt" content))
                (chat-pos   (string-match "^\\* Chat" content)))
            (expect (and end-pos sysp-pos chat-pos) :to-be-truthy)
            (expect (< end-pos sysp-pos) :to-be t)
            (expect (< sysp-pos chat-pos) :to-be t))
          ;; :GPTEL_SYSTEM: must NEVER appear in the rendered drawer
          ;; (Decision 2 / register/invariant/drawer-system-key-write-
          ;; exclusion).
          (expect (string-match-p ":GPTEL_SYSTEM:" content)
                  :to-be nil))))

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

    (it "the renderer-owned regions contain no markdown heading or ### placeholder"
      ;; The pre-chat-mode creation path seeded session.md with a
      ;; markdown `###' placeholder (or `# <session-name>').  Per
      ;; Decision 18, the creation renderer never seeds markdown
      ;; markup.  The `* System Prompt' body is excluded from this
      ;; check: it is verbatim preset `:system' text and may
      ;; legitimately contain markdown lines (`executor.md' does) —
      ;; that is preset content, not renderer markup (§Addendum
      ;; Decision B).  This test asserts the renderer-owned regions:
      ;; the file-level config drawer, the heading shape, and the
      ;; `* Chat' user block.
      (with-captured-io
        (jf/gptel--create-session-core
         "sess-no-markdown-20260421120000"
         "/sessions/sess-no-markdown-20260421120000"
         'executor)
        (let* ((content (captured-file-content
                         captured-files
                         (concat "/sessions/sess-no-markdown-20260421120000"
                                 "/branches/main/session.org")))
               ;; Region 1: everything up to and including the
               ;; `* System Prompt' heading's :END: drawer line.
               (sysp-drawer-end
                (string-match
                 "^\\* System Prompt\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                 content))
               (header-region
                (substring content 0 (match-end 0)))
               ;; Region 2: the `* Chat' heading and everything under
               ;; it (the empty user block).
               (chat-region
                (substring content (string-match "^\\* Chat" content))))
          (expect sysp-drawer-end :to-be-truthy)
          ;; The renderer uses org headings (`*'), never markdown.
          (expect header-region :not :to-match "^###")
          (expect header-region :not :to-match "^# ")
          (expect chat-region :not :to-match "^###")
          (expect chat-region :not :to-match "^# "))))

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
          ;; Both load-bearing keys land between :PROPERTIES: and :END:
          ;; (the snapshot keys may appear between them now — assert
          ;; ordering only, not adjacency).
          (expect content :to-match "\\`:PROPERTIES:\n")
          (expect content :to-match "\n:END:\n")
          (let ((preset-pos (string-match ":GPTEL_PRESET:" content))
                (parent-pos (string-match ":GPTEL_PARENT_SESSION_ID:" content))
                (end-pos (string-match "\n:END:\n" content)))
            (expect preset-pos :to-be-truthy)
            (expect parent-pos :to-be-truthy)
            (expect end-pos :to-be-truthy)
            ;; Both keys precede :END:.
            (expect (< preset-pos end-pos) :to-be t)
            (expect (< parent-pos end-pos) :to-be t)))))))

(describe "jf/gptel--session-headings-block emits the document body shape"

  ;; The single source of truth for the `* System Prompt' / `* Chat'
  ;; heading shape (register/shape/session-document-layout).  These
  ;; tests pin the literal layout the creation renderer and the
  ;; save-path materialiser both depend on.

  (it "emits a folded * System Prompt heading then * Chat with the user block"
    (expect (jf/gptel--session-headings-block
             "You are a helpful assistant."
             "#+begin_user\n\n#+end_user\n")
            :to-equal
            (concat "* System Prompt\n"
                    ":PROPERTIES:\n"
                    ":VISIBILITY: folded\n"
                    ":END:\n"
                    "You are a helpful assistant.\n"
                    "\n"
                    "* Chat\n"
                    "#+begin_user\n"
                    "\n"
                    "#+end_user\n")))

  (it "emits an empty * System Prompt body when the system prompt is nil"
    (expect (jf/gptel--session-headings-block
             nil "#+begin_user\n\n#+end_user\n")
            :to-equal
            (concat "* System Prompt\n"
                    ":PROPERTIES:\n"
                    ":VISIBILITY: folded\n"
                    ":END:\n"
                    "\n"
                    "* Chat\n"
                    "#+begin_user\n"
                    "\n"
                    "#+end_user\n")))

  (it "treats an all-whitespace system prompt as empty"
    (expect (jf/gptel--session-headings-block
             "   \n\t " "#+begin_user\n\n#+end_user\n")
            :to-equal
            (jf/gptel--session-headings-block
             nil "#+begin_user\n\n#+end_user\n")))

  (it "preserves multi-line, special-character system-prompt bodies verbatim"
    ;; The motivating case for §Addendum Finding B: a heading body
    ;; carries multi-line / special-character text with no escaping.
    (let* ((prompt "Line one.\n\nLine two with *bold* and a / slash.\n- bullet")
           (block (jf/gptel--session-headings-block
                   prompt "#+begin_user\n\n#+end_user\n")))
      (expect block :to-match (regexp-quote prompt))
      ;; The body sits between the heading drawer and the `* Chat'
      ;; heading.
      (expect block :to-match
              (concat ":END:\n" (regexp-quote prompt) "\n\n\\* Chat\n")))))

(describe "jf/gptel--initial-session-body wraps the heading block"

  (it "defaults to an empty * System Prompt body when called with no argument"
    (expect (jf/gptel--initial-session-body)
            :to-equal
            (jf/gptel--session-headings-block
             nil "#+begin_user\n\n#+end_user\n")))

  (it "seeds the * System Prompt body from the supplied system-prompt string"
    (expect (jf/gptel--initial-session-body "seeded prompt text")
            :to-equal
            (jf/gptel--session-headings-block
             "seeded prompt text" "#+begin_user\n\n#+end_user\n")))

  (it "produces a buffer the chat parser reads as a single empty user turn"
    ;; The chat parser is heading-indifferent (design.md §Decision
    ;; 12): the `* System Prompt' body and the headings are
    ;; commentary, not turns.  A created session.org therefore parses
    ;; to exactly one user turn — the empty `#+begin_user' block,
    ;; whose verbatim content is the single blank line between the
    ;; delimiters ("\n", per `gptel-chat--scan-user-body').
    (with-temp-buffer
      (insert (jf/gptel--initial-session-body
               "You are a helpful assistant.\nBe concise."))
      (let ((turns (gptel-chat-parse-buffer)))
        (expect (length turns) :to-equal 1)
        (expect (plist-get (car turns) :role) :to-equal 'user)
        (expect (plist-get (car turns) :content) :to-equal "\n"))))

  (it "parses to a single user turn even when the preset has a markdown system prompt"
    ;; The `executor' preset's `:system' body contains markdown
    ;; headings (`# ...').  Seeding it into the `* System Prompt'
    ;; body must not create spurious turns — the parser keys on
    ;; `#+begin_*' markers only.  This pins the §Addendum known
    ;; limitation as accepted: preset system prompts in this repo do
    ;; not contain a column-0 `#+begin_user', so the body never
    ;; mis-parses as a turn.
    (with-temp-buffer
      (insert (jf/gptel--initial-session-body
               (concat "# Role\n\nYou are the executor.\n\n"
                       "## Constraints\n\n- obey the drawer\n")))
      (let ((turns (gptel-chat-parse-buffer)))
        (expect (length turns) :to-equal 1)
        (expect (plist-get (car turns) :role) :to-equal 'user)))))

(provide 'session-org-creation-spec)
;;; session-org-creation-spec.el ends here
