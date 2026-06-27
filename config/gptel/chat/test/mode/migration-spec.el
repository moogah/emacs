;;; migration-spec.el --- Read-time chat-block body indentation migration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--migrate-buffer' /
;; `gptel-chat--migrate-body', the read-time migration that
;; normalises every chat-block body to the indented form on
;; `gptel-chat-mode' activation.  See
;; `openspec/changes/gptel-chat-heading-scoping/design.md' Decision 7
;; and the `Migration of pre-indentation session files' requirement
;; of `specs/gptel/chat-mode.md'.
;;
;; A chat-block body is an indented region: every body line is
;; indented by `gptel-chat--body-indent' spaces (default 2).
;; Migration converges pre-indentation (column-0 content), escape-era
;; (`,#+end_*' commas and 1-space `*' heading escapes), and current
;; (already-indented) on-disk formats onto that form.  Coverage:
;;
;;   1. A pre-indentation session is normalised on activation and the
;;      buffer marked modified.
;;   2. An already-indented session opens UNMODIFIED.
;;   3. Migration is idempotent.
;;   4. Content outside chat blocks (column-0 headings between turns)
;;      is untouched.
;;   5. Nested `#+begin_tool' / `#+end_tool' delimiter lines stay at
;;      column 0.
;;   6. A legacy `,#+end_*' body line has the comma stripped.
;;   7. A legacy 1-space-escaped `*' heading line is de-escaped and
;;      indented to the body width.

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
verbatim, point is moved to point-min, the modified flag is cleared,
and `gptel-chat-mode' is activated *before* BODY runs.  This drives
the real read-time migration code path: the migration runs as the
last step of the `define-derived-mode' body."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (gptel-chat-mode)
     ,@body))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "gptel-chat--migrate-buffer: read-time body indentation"

  (describe "Scenario 1: a pre-indentation session is normalised"
    ;; Fixture has a `#+begin_assistant' body with a column-0
    ;; `* Test Heading' — a heading-shaped body line that would be
    ;; read as document structure if left at column 0.

    (it "indents the column-0 body content and marks the buffer modified"
      (let ((content (gptel-chat-test--load-fixture
                      "unescaped-heading-in-assistant.org")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; The column-0 `* Test Heading' is shifted to the body
          ;; indent (default 2); no column-0 heading survives.
          (expect (string-match-p "^\\* Test Heading$" (buffer-string))
                  :to-be nil)
          (expect (string-match-p "^  \\* Test Heading$" (buffer-string))
                  :to-be-truthy)
          ;; Surrounding body prose is indented to the same width.
          (expect (string-match-p "^  Some prose under the heading\\.$"
                                  (buffer-string))
                  :to-be-truthy)
          ;; The block delimiters themselves stay at column 0.
          (expect (string-match-p "^#\\+begin_assistant$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^#\\+end_assistant$" (buffer-string))
                  :to-be-truthy))))

    (it "produces a parseable buffer (the AST is no longer corrupted)"
      ;; After migration `gptel-chat-parse-buffer' must walk the
      ;; buffer without the heading absorbing the assistant body.
      (let ((content (gptel-chat-test--load-fixture
                      "unescaped-heading-in-assistant.org")))
        (gptel-chat-test--with-mode-buffer content
          (let ((turns (gptel-chat-parse-buffer)))
            (expect (length turns) :to-equal 2)
            (expect (plist-get (nth 0 turns) :role) :to-equal 'user)
            (expect (plist-get (nth 1 turns) :role) :to-equal 'assistant))))))

  (describe "Scenario 2: an already-indented session opens unmodified"

    (it "does not mark the buffer modified when bodies are already indented"
      (gptel-chat-test--with-mode-buffer
          (concat "#+begin_user\n"
                  "  Hello.\n"
                  "#+end_user\n"
                  "\n"
                  "#+begin_assistant\n"
                  "  Hi back.\n"
                  "  * a heading in prose\n"
                  "#+end_assistant\n")
        (expect (buffer-modified-p) :to-be nil)))

    (it "leaves an empty buffer untouched"
      (gptel-chat-test--with-mode-buffer ""
        (expect (buffer-modified-p) :to-be nil)
        (expect (buffer-string) :to-equal "")))

    (it "leaves a buffer with no chat blocks untouched"
      (gptel-chat-test--with-mode-buffer
          "Just some plain prose, no chat blocks here.\n"
        (expect (buffer-modified-p) :to-be nil)))

    (it "leaves the already-indented body text byte-for-byte unchanged"
      (let ((content (concat "#+begin_user\n"
                             "  Already indented.\n"
                             "#+end_user\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-string) :to-equal content)
          (expect (buffer-modified-p) :to-be nil)))))

  (describe "Scenario 3: idempotence"

    (it "a second activation on the migrated buffer makes no further change"
      (let ((content (concat "#+begin_assistant\n"
                             "* heading\n"
                             "body line\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          ;; First activation migrated the body.
          (expect (buffer-modified-p) :to-be-truthy)
          (let ((migrated (buffer-string)))
            ;; Re-running migration on the already-migrated buffer is
            ;; a no-op: the text does not grow another indent level.
            (set-buffer-modified-p nil)
            (expect (gptel-chat--migrate-buffer) :to-be nil)
            (expect (buffer-string) :to-equal migrated)
            (expect (buffer-modified-p) :to-be nil))))))

  (describe "Scenario 4: content outside chat blocks is untouched"

    (it "leaves a column-0 heading between turns as a real document heading"
      (let ((content (concat "#+title: Mixed buffer\n"
                             "\n"
                             "* Section A\n"
                             "Free-form prose under the heading.\n"
                             "\n"
                             "#+begin_user\n"
                             "Question.\n"
                             "#+end_user\n"
                             "\n"
                             "* Section B\n"
                             "\n"
                             "#+begin_assistant\n"
                             "Answer.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          ;; The chat-block bodies were at column 0, so migration runs.
          (expect (buffer-modified-p) :to-be-truthy)
          ;; Both out-of-block headings stay column-0 document headings.
          (expect (string-match-p "^\\* Section A$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^\\* Section B$" (buffer-string))
                  :to-be-truthy)
          ;; The free-form prose under Section A is not indented.
          (expect (string-match-p "^Free-form prose under the heading\\.$"
                                  (buffer-string))
                  :to-be-truthy)
          ;; Only the in-block body content is indented.
          (expect (string-match-p "^  Question\\.$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^  Answer\\.$" (buffer-string))
                  :to-be-truthy))))

    (it "leaves a clean buffer with an outer heading unmodified"
      ;; Bodies already indented, an outer heading present — nothing
      ;; needs migrating, so the buffer must open clean.
      (let ((content (concat "* Real Heading\n"
                             "Some prose under the heading.\n"
                             "\n"
                             "#+begin_user\n"
                             "  Hello.\n"
                             "#+end_user\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be nil)
          (expect (string-match-p "^\\* Real Heading$" (buffer-string))
                  :to-be-truthy)))))

  (describe "Scenario 5: nested tool delimiter lines stay at column 0"

    (it "indents tool result content but leaves the tool delimiters flush-left"
      (let ((content (concat "#+begin_assistant\n"
                             "Intro.\n"
                             "#+begin_tool (search :q \"foo\")\n"
                             "tool result line\n"
                             "* result heading\n"
                             "#+end_tool\n"
                             "Outro.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; Tool delimiter lines remain at column 0.
          (expect (string-match-p "^#\\+begin_tool " (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^#\\+end_tool$" (buffer-string))
                  :to-be-truthy)
          ;; The tool result content is indented to the body width.
          (expect (string-match-p "^  tool result line$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^  \\* result heading$" (buffer-string))
                  :to-be-truthy)
          ;; The surrounding assistant prose is indented too.
          (expect (string-match-p "^  Intro\\.$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^  Outro\\.$" (buffer-string))
                  :to-be-truthy)))))

  (describe "Scenario 6: legacy `,#+end_*' comma escape is stripped"

    (it "strips the leading comma from a `,#+end_assistant' body line"
      ;; An escape-era session: a model-emitted `#+end_assistant' line
      ;; inside the body was protected with a leading comma.  Migration
      ;; strips the comma and indents the line as ordinary content.
      (let ((content (concat "#+begin_assistant\n"
                             "Here is a literal closer:\n"
                             ",#+end_assistant\n"
                             "and more prose.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; The comma is gone and the line is indented body content.
          (expect (string-match-p "^,#\\+end_assistant$" (buffer-string))
                  :to-be nil)
          (expect (string-match-p "^  #\\+end_assistant$" (buffer-string))
                  :to-be-truthy)
          ;; The real (terminating) `#+end_assistant' stays at column 0.
          ;; Exactly one column-0 `#+end_assistant' remains.
          (save-excursion
            (goto-char (point-min))
            (expect
             (cl-loop while (re-search-forward "^#\\+end_assistant$" nil t)
                      count t)
             :to-equal 1))))))

  (describe "Scenario 7: legacy 1-space `*' heading escape is de-escaped"

    (it "strips the 1-space prefix so the heading lands at the body width"
      ;; An escape-era session: a column-0 `* heading' inside a body
      ;; was protected by the per-`*' escape with a single leading
      ;; space.  Migration must strip that space and indent the line
      ;; to the body width like ordinary content — not carry the
      ;; stray space through, which would leave the heading ragged at
      ;; column 3 while sibling prose sits at column 2.
      (let ((content (concat "#+begin_assistant\n"
                             "Intro prose.\n"
                             " * Escaped heading\n"
                             "Outro prose.\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          ;; The escaped heading lands at the body indent (2), flush
          ;; with the surrounding prose — not ragged at column 3.
          (expect (string-match-p "^  \\* Escaped heading$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^   \\* Escaped heading$" (buffer-string))
                  :to-be nil)
          ;; Surrounding prose is indented to the same width.
          (expect (string-match-p "^  Intro prose\\.$" (buffer-string))
                  :to-be-truthy)
          (expect (string-match-p "^  Outro prose\\.$" (buffer-string))
                  :to-be-truthy))))

    (it "de-escapes a multi-star heading shape too"
      (let ((content (concat "#+begin_assistant\n"
                             " *** Deep escaped heading\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          (expect (string-match-p "^  \\*\\*\\* Deep escaped heading$"
                                  (buffer-string))
                  :to-be-truthy)))))

  (describe "robustness"

    (it "does not signal on a malformed buffer (unclosed block)"
      ;; Mid-stream / partially-typed structure: the parser raises
      ;; `user-error' from inside the migration; it is caught and the
      ;; rewrite skipped so mode activation still succeeds.
      (let ((content "#+begin_user\nincomplete\n"))
        (expect (gptel-chat-test--with-mode-buffer content
                  (buffer-string))
                :to-equal content)))

    (it "honours a non-default `gptel-chat-content-indentation' value"
      (let ((gptel-chat-content-indentation 4)
            (content (concat "#+begin_assistant\n"
                             "* heading\n"
                             "#+end_assistant\n")))
        (gptel-chat-test--with-mode-buffer content
          (expect (buffer-modified-p) :to-be-truthy)
          (expect (string-match-p "^    \\* heading$" (buffer-string))
                  :to-be-truthy))))))

(provide 'gptel-chat-migration-spec)

;;; migration-spec.el ends here
