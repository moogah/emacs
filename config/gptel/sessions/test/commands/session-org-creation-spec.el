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
;;    parent), followed directly by an empty `#+begin_user' /
;;    `#+end_user' turn block.  No `* System Prompt' / `* Chat'
;;    headings are emitted — the preset's `:system' lives in the
;;    sibling `system-prompt.<ext>' file (design.md §Decision 1 of
;;    replace-system-prompt-heading-with-sibling-file).  The drawer
;;    shape matches what the save hook writes on first save, so
;;    creation → open → save is a no-op on disk.
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

    (it "contains the config drawer followed directly by an empty user block, with no heading shape"
      ;; replace-system-prompt-heading-with-sibling-file: the
      ;; canonical layout is drawer + bare user block.  No
      ;; `* System Prompt' / `* Chat' headings are emitted.  The
      ;; preset's `:system' is materialised in a sibling file by a
      ;; subsequent task; this spec only verifies the no-heading shape.
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
          ;; File-level config drawer at point-min.
          (expect content :to-match "\\`:PROPERTIES:\n")
          (expect content :to-match ":GPTEL_PRESET: executor\n")
          (expect content :to-match "\n:END:\n")
          ;; No `* System Prompt' / `* Chat' headings anywhere.
          (expect (string-match-p "^\\* System Prompt" content) :to-be nil)
          (expect (string-match-p "^\\* Chat" content) :to-be nil)
          (expect (string-match-p ":VISIBILITY: folded" content) :to-be nil)
          ;; The empty user block follows the drawer directly (no
          ;; intervening blank line — drawer + body concatenation is
          ;; verbatim) and is the last thing in the file.
          (expect content :to-match ":END:\n#\\+begin_user\n\n#\\+end_user\n\\'")
          ;; :GPTEL_SYSTEM: must NEVER appear in the rendered drawer
          ;; (register/invariant/drawer-system-key-write-exclusion).
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

    (it "the renderer never seeds markdown markup into session.org"
      ;; Per Decision 18, the creation renderer never seeds markdown
      ;; markup.  With the heading shape removed, the whole document
      ;; is renderer-owned (no preset `:system' body lives inside the
      ;; file anymore — it goes to the sibling file).  Assert the
      ;; whole file is markdown-free.
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

(describe "jf/gptel--initial-session-body returns the bare user-block template"

  ;; After replace-system-prompt-heading-with-sibling-file, the helper
  ;; takes no arguments and returns only the empty `#+begin_user' /
  ;; `#+end_user' block.  No headings, no preset content.  The
  ;; document parser (heading-indifferent, design.md §Decision 12)
  ;; reads the template as a single empty user turn.

  (it "returns the empty user-block template"
    (expect (jf/gptel--initial-session-body)
            :to-equal "#+begin_user\n\n#+end_user\n"))

  (it "produces a buffer the chat parser reads as a single empty user turn"
    (with-temp-buffer
      (insert (jf/gptel--initial-session-body))
      (let ((turns (gptel-chat-parse-buffer)))
        (expect (length turns) :to-equal 1)
        (expect (plist-get (car turns) :role) :to-equal 'user)
        (expect (plist-get (car turns) :content) :to-equal "\n"))))

  ;; The "parser is heading-indifferent when a markdown system prompt
  ;; is present" contract is no longer exercisable from this helper —
  ;; the system prompt does not live in the document at all.  The
  ;; chat-parser test suite (config/gptel/chat/test/parser/) pins
  ;; heading-indifference independently.
  )

(describe "jf/gptel--create-session-core emits sibling system-prompt file"

  ;; replace-system-prompt-heading-with-sibling-file: when the preset
  ;; declares a non-empty `:system' body, session creation writes
  ;; `system-prompt.<ext>' alongside `session.org' and threads
  ;; `:GPTEL_SYSTEM_PROMPT_FILE:' into the configuration drawer.  When
  ;; the preset has no `:system', the writer no-ops — no sibling file,
  ;; no drawer property.  When the caller supplies INITIAL-CONTENT,
  ;; the override contract is preserved: no sibling file is written
  ;; and no property is threaded.  The integration-spec coverage here
  ;; complements the focused unit tests in
  ;; `sibling-system-prompt-file-spec.el'.

  (describe "preset declares non-empty :system"

    (let ((sibling-preset 'sibling-emission-preset))

      (before-each
        (gptel-make-preset sibling-preset
          :system "You are a careful test agent.\nLine two."))

      (after-each
        (setq gptel--known-presets
              (assq-delete-all sibling-preset gptel--known-presets)))

      (it "writes system-prompt.md alongside session.org with the :system body verbatim"
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-sibling-20260423120000"
           "/sessions/sess-sibling-20260423120000"
           sibling-preset)
          (let* ((branch-prefix
                  "/sessions/sess-sibling-20260423120000/branches/main")
                 (sibling-content
                  (captured-file-content
                   captured-files
                   (concat branch-prefix "/system-prompt.md"))))
            (expect sibling-content :to-be-truthy)
            (expect sibling-content
                    :to-equal
                    "You are a careful test agent.\nLine two."))))

      (it "threads :GPTEL_SYSTEM_PROMPT_FILE: into the drawer inside :PROPERTIES: / :END:"
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-sibling-drawer-20260423120000"
           "/sessions/sess-sibling-drawer-20260423120000"
           sibling-preset)
          (let* ((branch-prefix
                  "/sessions/sess-sibling-drawer-20260423120000/branches/main")
                 (content
                  (captured-file-content
                   captured-files
                   (concat branch-prefix "/session.org"))))
            (expect content :to-be-truthy)
            (expect content
                    :to-match ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n")
            ;; The new property precedes :END: (splice landed inside
            ;; the :PROPERTIES: block, not after :END:).
            (let ((file-pos (string-match ":GPTEL_SYSTEM_PROMPT_FILE:" content))
                  (end-pos  (string-match "\n:END:\n" content)))
              (expect file-pos :to-be-truthy)
              (expect end-pos :to-be-truthy)
              (expect (< file-pos end-pos) :to-be t))
            ;; Drawer + body adjacency is preserved (no separator
            ;; injected between :END: and #+begin_user).
            (expect content
                    :to-match
                    ":END:\n#\\+begin_user\n\n#\\+end_user\n\\'"))))))

  (describe "preset declares no :system"

    (let ((empty-preset 'sibling-empty-preset))

      (before-each
        (gptel-make-preset empty-preset :temperature 0.5))

      (after-each
        (setq gptel--known-presets
              (assq-delete-all empty-preset gptel--known-presets)))

      (it "writes no sibling file and omits :GPTEL_SYSTEM_PROMPT_FILE: from the drawer"
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-no-sibling-20260423120000"
           "/sessions/sess-no-sibling-20260423120000"
           empty-preset)
          (let* ((branch-prefix
                  "/sessions/sess-no-sibling-20260423120000/branches/main")
                 (content
                  (captured-file-content
                   captured-files
                   (concat branch-prefix "/session.org")))
                 (paths (hash-table-keys captured-files)))
            (expect content :to-be-truthy)
            (expect (string-match-p ":GPTEL_SYSTEM_PROMPT_FILE:" content)
                    :to-be nil)
            (expect (cl-some (lambda (p)
                               (string-match-p "/system-prompt\\." p))
                             paths)
                    :to-be nil))))))

  (describe "caller-supplied INITIAL-CONTENT override"

    (let ((override-preset 'sibling-override-preset))

      (before-each
        (gptel-make-preset override-preset
          :system "Body that must NOT leak into a sibling file."))

      (after-each
        (setq gptel--known-presets
              (assq-delete-all override-preset gptel--known-presets)))

      (it "skips sibling-file emission entirely when INITIAL-CONTENT is non-nil"
        ;; The override contract gives the caller full ownership of
        ;; the on-disk shape.  Emitting a sibling file alongside a
        ;; caller-composed document would surprise the contract.
        (with-captured-io
          (jf/gptel--create-session-core
           "sess-override-20260423120000"
           "/sessions/sess-override-20260423120000"
           override-preset
           "caller-supplied\nbody\n")
          (let* ((branch-prefix
                  "/sessions/sess-override-20260423120000/branches/main")
                 (content
                  (captured-file-content
                   captured-files
                   (concat branch-prefix "/session.org")))
                 (paths (hash-table-keys captured-files)))
            (expect content :to-equal "caller-supplied\nbody\n")
            (expect (cl-some (lambda (p)
                               (string-match-p "/system-prompt\\." p))
                             paths)
                    :to-be nil)))))))

(describe "jf/gptel--append-drawer-property"

  ;; Pure string helper: splices `:KEY: VALUE\n' inside an existing
  ;; `:PROPERTIES:' / `:END:' block, preserving the drawer's
  ;; load-bearing tail-adjacency to the body
  ;; (`register/invariant/scope-drawer-no-duplication' + the no-
  ;; separator drawer + body concatenation in
  ;; `jf/gptel--create-session-core').

  (it "inserts the property line immediately before the closing :END: marker"
    (let* ((drawer ":PROPERTIES:\n:GPTEL_PRESET: executor\n:END:\n")
           (augmented (jf/gptel--append-drawer-property
                       drawer "GPTEL_SYSTEM_PROMPT_FILE" "system-prompt.md")))
      (expect augmented
              :to-equal
              (concat ":PROPERTIES:\n"
                      ":GPTEL_PRESET: executor\n"
                      ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                      ":END:\n"))))

  (it "preserves :END: as the final line so drawer + body concatenation stays adjacent"
    (let ((augmented (jf/gptel--append-drawer-property
                      ":PROPERTIES:\n:GPTEL_PRESET: x\n:END:\n"
                      "KEY" "value")))
      (expect (string-suffix-p ":END:\n" augmented) :to-be t)))

  (it "signals user-error when drawer-text does not end with :END:"
    (expect (jf/gptel--append-drawer-property
             ":PROPERTIES:\n:GPTEL_PRESET: x\n"
             "KEY" "value")
            :to-throw 'user-error)))

(provide 'session-org-creation-spec)
;;; session-org-creation-spec.el ends here
