;;; migration-spec.el --- Read-time migration of in-block * lines -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--migrate-headings', the read-time
;; migration that applies the heading-collision escape to chat-block
;; bodies on `gptel-chat-mode' activation
;; (`openspec/changes/gptel-chat-heading-scoping/design.md' §Decision 5,
;; `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-migration-on-read.md').
;;
;; Coverage targets the five scenarios in the task body:
;;
;;   1. Fixture buffer with unescaped `* Heading' in an assistant body
;;      → migration applies, buffer is marked modified, content is
;;      normalised (the original repro shape from
;;      ~/.gptel/sessions/heading-test-20260430145834/...).
;;   2. Buffer whose chat blocks have no column-0 `*' lines → no
;;      migration, buffer stays clean (the
;;      `register/invariant/chat-migration-on-read-clean-buffer-stays-clean'
;;      invariant).
;;   3. Buffer with `* Heading' OUTSIDE any chat block AND no column-0
;;      `*' lines inside chat blocks → no migration, the outer heading
;;      is preserved as a real heading.
;;   4. Buffer with `* Heading' outside chat blocks AND `* In-block'
;;      inside an assistant block → only the in-block line is
;;      escaped; the outer heading is untouched.
;;   5. Already-escaped headings inside a chat block → no further
;;      migration (idempotent).

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'seq)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

(require 'gptel-chat-mode)
(require 'gptel-chat-parser)

(defconst gptel-chat-test--mode-fixtures-dir
  (expand-file-name
   "fixtures/"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory holding fixture files for migration specs.")

(defun gptel-chat-test--load-fixture (name)
  "Return the contents of fixture file NAME under the mode/fixtures dir."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name gptel-chat-test--mode-fixtures-dir))
    (buffer-string)))

(defmacro gptel-chat-test--with-mode-buffer (content &rest body)
  "Run BODY in a fresh buffer pre-populated with CONTENT, then activated.

The buffer is created with `with-temp-buffer', CONTENT is inserted
verbatim, point is moved to point-min, and `gptel-chat-mode' is
activated *before* BODY runs.  This drives the real read-time
migration code path: the migration runs as the last step of
`define-derived-mode' body."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (gptel-chat-mode)
     ,@body))

(describe "gptel-chat--migrate-headings: read-time heading escape"

  (describe "Scenario 1: fixture buffer with unescaped `* Heading'"
    ;; Original repro shape — see
    ;; ~/.gptel/sessions/heading-test-20260430145834/branches/main/session.org
    (it "rewrites the in-block heading and marks the buffer modified"
      (let ((content (gptel-chat-test--load-fixture
                      "unescaped-heading-in-assistant.org")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; The unescaped column-0 `* Test Heading' is replaced with
          ;; ` * Test Heading' (one leading space at default
          ;; gptel-chat-content-indentation = 1).
          (expect (string-match-p "^\\* Test Heading$" (buffer-string))
                  :to-be nil)
          (expect (string-match-p "^ \\* Test Heading$" (buffer-string))
                  :to-be-truthy)
          ;; The block delimiters themselves remain at column 0.
          (expect (string-match-p "^#\\+begin_assistant$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^#\\+end_assistant$" (buffer-string))
                  :to-be-truthy))))

    (it "produces a parseable buffer (the AST is no longer corrupted)"
      ;; The whole point of the migration: after it runs,
      ;; gptel-chat-parse-buffer must be able to walk the buffer
      ;; without choking on a heading-absorbed assistant body.
      (let ((content (gptel-chat-test--load-fixture
                      "unescaped-heading-in-assistant.org")))
        (gptel-chat-test--with-mode-buffer content
          (let ((turns (gptel-chat-parse-buffer)))
            (expect (length turns) :to-equal 2)
            (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
            (expect (plist-get (nth 1 turns) :role) :to-equal 'assistant))))))

  (describe "Scenario 2: clean buffer stays clean"

    (it "does not mark the buffer modified when no migration is needed"
      ;; Two clean turns; the `* heading' is in a comment-style line
      ;; that is NOT column-0 (note the leading space).
      (gptel-chat-test--with-mode-buffer
          (concat "#+begin_user\n"
                  "Hello.\n"
                  "#+end_user\n"
                  "\n"
                  "#+begin_assistant\n"
                  "Hi back.\n"
                  " * already-escaped\n"
                  "#+end_assistant\n")
        (expect (buffer-modified-p) :to-be nil)))

    (it "leaves an empty buffer untouched"
      (gptel-chat-test--with-mode-buffer ""
        (expect (buffer-modified-p) :to-be nil)
        (expect (buffer-string) :to-equal "")))

    (it "leaves a buffer with no chat blocks untouched"
      (gptel-chat-test--with-mode-buffer
          "Just some plain prose, no chat blocks here.\n"
        (expect (buffer-modified-p) :to-be nil))))

  (describe "Scenario 3: outer heading + clean blocks"

    (it "preserves headings outside chat blocks and leaves buffer clean"
      ;; A real `* Heading' between a metadata block and a clean user
      ;; block.  Migration must NOT touch the outer heading and the
      ;; buffer must NOT be marked modified.
      (let ((content (concat "#+title: Mixed buffer\n"
                             "\n"
                             "* Real Heading\n"
                             "Some prose under the heading.\n"
                             "\n"
                             "#+begin_user\n"
                             "Hello.\n"
                             "#+end_user\n"
                             "\n"
                             "#+begin_assistant\n"
                             "Hi.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be nil)
          (expect (string-match-p "^\\* Real Heading$" (buffer-string))
                  :to-be-truthy)))))

  (describe "Scenario 4: outer heading kept; in-block heading escaped"

    (it "escapes only the in-block line; the outer heading is untouched"
      (let ((content (concat "* Outer Heading\n"
                             "Outer prose.\n"
                             "\n"
                             "#+begin_assistant\n"
                             "Reply intro.\n"
                             "* In-block\n"
                             "More body.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; Outer heading is preserved verbatim.
          (expect (string-match-p "^\\* Outer Heading$" (buffer-string))
                  :to-be-truthy)
          ;; In-block heading is escaped to ` * In-block'.
          (expect (string-match-p "^\\* In-block$" (buffer-string))
                  :to-be nil)
          (expect (string-match-p "^ \\* In-block$" (buffer-string))
                  :to-be-truthy)))))

  (describe "Scenario 5: idempotence on already-escaped content"

    (it "does not re-indent already-escaped headings"
      (let ((content (concat "#+begin_assistant\n"
                             "Reply.\n"
                             " * already-escaped\n"
                             " ** also-escaped\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be nil)
          ;; The escaped lines remain at exactly one leading space —
          ;; the migration must not stack additional spaces on top.
          (expect (string-match-p "^ \\* already-escaped$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^  \\* already-escaped$" (buffer-string))
                  :to-be nil)
          (expect (string-match-p "^ \\*\\* also-escaped$" (buffer-string))
                  :to-be-truthy)))))

  (describe "additional shape coverage"

    (it "escapes column-0 `* line' inside a user block too"
      (let ((content (concat "#+begin_user\n"
                             "Question:\n"
                             "* part one\n"
                             "tail\n"
                             "#+end_user\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          (expect (string-match-p "^ \\* part one$" (buffer-string))
                  :to-be-truthy))))

    (it "escapes column-0 `*' lines inside a nested tool block body"
      (let ((content (concat "#+begin_assistant\n"
                             "Intro.\n"
                             "#+begin_tool (search :q \"foo\")\n"
                             "* tool result heading\n"
                             "more text\n"
                             "#+end_tool\n"
                             "Outro.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          (expect (string-match-p "^ \\* tool result heading$"
                                  (buffer-string))
                  :to-be-truthy)
          ;; Tool delimiter lines themselves stay at column 0.
          (expect (string-match-p "^#\\+begin_tool " (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^#\\+end_tool$" (buffer-string))
                  :to-be-truthy))))

    (it "honours a non-default `gptel-chat-content-indentation' value"
      (let ((gptel-chat-content-indentation 2)
            (content (concat "#+begin_assistant\n"
                             "* heading\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          (expect (string-match-p "^  \\* heading$" (buffer-string))
                  :to-be-truthy))))

    (it "does not signal on a malformed buffer (unclosed block)"
      ;; Mid-stream / partially-typed structure: the parser raises
      ;; `user-error' from inside the migration; we catch it and skip
      ;; the rewrite so mode activation still succeeds.
      (let ((content "#+begin_user\nincomplete\n"))
        (expect (gptel-chat-test--with-mode-buffer content
                  (buffer-string))
                :to-equal content)))

    (it "treats `gptel-chat-content-indentation' = 0 as a no-op"
      ;; Pathological config: indentation 0 cannot break the heading
      ;; regex, so the migration simply does nothing.  The buffer is
      ;; left untouched; modified state restored.
      (let ((gptel-chat-content-indentation 0)
            (content (concat "#+begin_assistant\n"
                             "* heading\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be nil)
          (expect (string-match-p "^\\* heading$" (buffer-string))
                  :to-be-truthy))))))

(provide 'migration-spec)

;;; migration-spec.el ends here
