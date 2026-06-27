;;; body-indent-round-trip-spec.el --- Body indent / dedent inverse-pair tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs pinning the chat-block body-indentation round-trip
;; (change `gptel-chat-heading-scoping', design.md §Decisions 1 and 3).
;;
;; A chat-block body is an indented region: every body line is offset
;; by `gptel-chat-content-indentation' spaces on the write path so
;; org's column-0-anchored structural scanners never read body text as
;; document structure.  On the send path the indentation must come
;; back off before the content reaches the model.
;;
;; Two functions form the inverse pair:
;;
;; 1. Write side — `gptel-chat--sanitize-chunk' (stream.el) is a
;;    per-line indenter: a non-blank line is returned with
;;    `gptel-chat--body-indent' leading spaces prepended; a blank line
;;    is returned unchanged.
;;
;; 2. Read side — `gptel-chat--dedent' (parser.el) is a pure
;;    string -> string function: it measures the common minimum
;;    leading-space indentation across the non-blank lines and removes
;;    that many leading spaces from every line (the
;;    `org-do-remove-indentation' model).  Blank lines do not count
;;    toward the minimum; nested (deeper) indentation is preserved
;;    relative to the body.  nil / empty / no-common-indent input is
;;    returned unchanged.
;;
;; The asymmetry is deliberate: write indents by exactly N, read
;; strips whatever common indent it measures.  That is what makes the
;; round-trip robust against a later change of
;; `gptel-chat-content-indentation'.
;;
;; This file covers `gptel-chat--dedent' directly, and the
;; indent -> dedent round-trip: content indented by
;; `gptel-chat--sanitize-chunk' then dedented by `gptel-chat--dedent'
;; recovers the original content modulo a uniform shift.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

;; Modules under test.
;; `gptel-chat-mode' owns the `gptel-chat-content-indentation' defcustom
;; and the `gptel-chat--body-indent' accessor consulted by
;; `gptel-chat--sanitize-chunk'; load it so the write-side specs see the
;; configured default rather than the sanitizer's standalone fallback.
(require 'gptel-chat-mode)
(require 'gptel-chat-parser)
(require 'gptel-chat-stream)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defun gptel-chat-test--indent-body (body)
  "Indent BODY line-by-line via `gptel-chat--sanitize-chunk'.
Mirrors the streaming inserter's per-line write path: split at
`\\n', indent each line, re-join with `\\n'.  Returns the indented
string."
  (mapconcat #'gptel-chat--sanitize-chunk
             (split-string body "\n")
             "\n"))

;;; ---------------------------------------------------------------
;;; gptel-chat--dedent — common-minimum measure-and-strip
;;; ---------------------------------------------------------------

(describe "gptel-chat--dedent: common-minimum measure-and-strip"

  (describe "removes the common minimum leading indentation"

    (it "strips a uniform two-space indent from every line"
      (expect (gptel-chat--dedent "  line one\n  line two\n  line three")
              :to-equal "line one\nline two\nline three"))

    (it "strips a uniform four-space indent"
      (expect (gptel-chat--dedent "    a\n    b")
              :to-equal "a\nb"))

    (it "strips only the common minimum when indentation is ragged"
      ;; The minimum across non-blank lines is 2; the deeper line keeps
      ;; its extra indentation.
      (expect (gptel-chat--dedent "  shallow\n      deeper")
              :to-equal "shallow\n    deeper"))

    (it "strips a single-space indent"
      (expect (gptel-chat--dedent " a\n b\n c")
              :to-equal "a\nb\nc")))

  (describe "blank lines do not count toward the measured minimum"

    (it "ignores a fully empty line when measuring"
      ;; The empty middle line contributes 0 leading spaces but must
      ;; not drag the minimum to 0.
      (expect (gptel-chat--dedent "  first\n\n  second")
              :to-equal "first\n\nsecond"))

    (it "ignores a whitespace-only line when measuring"
      ;; A line of only spaces is blank; it must not anchor the
      ;; minimum even if it carries fewer spaces than the content.
      (expect (gptel-chat--dedent "    content one\n \n    content two")
              :to-equal "content one\n\ncontent two"))

    (it "leaves a blank line blank rather than producing negative indent"
      (let ((result (gptel-chat--dedent "  a\n\n  b")))
        (expect result :to-equal "a\n\nb")
        ;; The middle element is the empty string, not whitespace.
        (expect (nth 1 (split-string result "\n")) :to-equal ""))))

  (describe "preserves the relative indentation of nested content"

    (it "keeps a nested list's deeper indentation after dedent"
      ;; Body indent 2; the nested list items carry 2 extra spaces.
      ;; Dedent removes the common 2 and the nesting survives.
      (expect (gptel-chat--dedent
               (concat "  Outer paragraph.\n"
                       "  - item one\n"
                       "    - nested item\n"
                       "  - item two"))
              :to-equal
              (concat "Outer paragraph.\n"
                      "- item one\n"
                      "  - nested item\n"
                      "- item two")))

    (it "keeps an indented code block's interior indentation"
      (expect (gptel-chat--dedent
               (concat "  intro\n"
                       "  #+begin_src emacs-lisp\n"
                       "    (defun f () 1)\n"
                       "  #+end_src"))
              :to-equal
              (concat "intro\n"
                      "#+begin_src emacs-lisp\n"
                      "  (defun f () 1)\n"
                      "#+end_src"))))

  (describe "robust to a changed indentation width"

    (it "strips a width-4 region just as it strips a width-2 region"
      ;; The amount removed is measured, not assumed: a body authored
      ;; at width 2 and a body authored at width 4 both dedent clean.
      (expect (gptel-chat--dedent "  width-two body")
              :to-equal "width-two body")
      (expect (gptel-chat--dedent "    width-four body")
              :to-equal "width-four body"))

    (it "dedents correctly regardless of the prevailing defcustom value"
      ;; Even with the defcustom set to a value that disagrees with the
      ;; region's actual indent, the measure-and-strip recovers cleanly.
      (let ((gptel-chat-content-indentation 2))
        (expect (gptel-chat--dedent "      six-space line")
                :to-equal "six-space line"))
      (let ((gptel-chat-content-indentation 8))
        (expect (gptel-chat--dedent "  two-space line")
                :to-equal "two-space line"))))

  (describe "passthrough cases"

    (it "returns the empty string for nil input"
      ;; `gptel-chat--dedent' coerces a nil argument to the empty
      ;; string (its `(or text "")' nil arm) rather than passing nil
      ;; through — callers downstream (`gptel-chat--turn-to-messages')
      ;; always feed it a string anyway.
      (expect (gptel-chat--dedent nil) :to-equal ""))

    (it "returns the empty string unchanged"
      (expect (gptel-chat--dedent "") :to-equal ""))

    (it "returns content with no common leading indent unchanged"
      ;; The first line starts at column 0, so the common minimum is 0
      ;; and nothing is stripped.
      (expect (gptel-chat--dedent "column-0 line\n  indented line")
              :to-equal "column-0 line\n  indented line"))

    (it "is a no-op on a single column-0 line"
      (expect (gptel-chat--dedent "* My Heading")
              :to-equal "* My Heading"))

    (it "is a no-op on whitespace-only input"
      ;; No non-blank line, so no minimum is measured; the string is
      ;; returned unchanged.
      (expect (gptel-chat--dedent "   \n\t\n  ")
              :to-equal "   \n\t\n  "))

    (it "is idempotent — dedent of an already-dedented string is a no-op"
      (let* ((once  (gptel-chat--dedent "  a\n  b\n  c"))
             (twice (gptel-chat--dedent once)))
        (expect once  :to-equal "a\nb\nc")
        (expect twice :to-equal once))))

  (describe "structural tokens are recovered at column 0"

    (it "recovers an indented `* heading' body line to column 0"
      (expect (gptel-chat--dedent "  * My Heading")
              :to-equal "* My Heading"))

    (it "recovers an indented `#+end_assistant' body line to column 0"
      (expect (gptel-chat--dedent "  #+end_assistant")
              :to-equal "#+end_assistant"))

    (it "recovers a mixed body with heading and delimiter shapes"
      (expect (gptel-chat--dedent
               (concat "  prose line\n"
                       "  * Heading\n"
                       "  #+end_assistant\n"
                       "  tail"))
              :to-equal
              (concat "prose line\n"
                      "* Heading\n"
                      "#+end_assistant\n"
                      "tail")))))


;;; ---------------------------------------------------------------
;;; indent -> dedent round-trip (write side then read side)
;;; ---------------------------------------------------------------

(describe "sanitize-chunk / dedent round-trip"

  (describe "a body indented by sanitize-chunk dedents back to the original"

    (dolist (width '(1 2 4))
      (it (format "round-trips a mixed body at indent width %d" width)
        (let* ((gptel-chat-content-indentation width)
               (body (concat "intro line\n"
                             "* Heading\n"
                             "more prose\n"
                             "#+end_assistant\n"
                             "tail"))
               (indented (gptel-chat-test--indent-body body))
               (recovered (gptel-chat--dedent indented)))
          ;; Every non-blank line picked up exactly WIDTH spaces.
          (expect indented
                  :to-equal
                  (let ((pad (make-string width ?\s)))
                    (concat pad "intro line\n"
                            pad "* Heading\n"
                            pad "more prose\n"
                            pad "#+end_assistant\n"
                            pad "tail")))
          ;; And the dedent recovers the original column-0 content.
          (expect recovered :to-equal body))))

    (it "round-trips a body that contains blank lines"
      ;; Blank lines are left untouched by the indenter and ignored by
      ;; the dedent measure — they survive the cycle as empty strings.
      (let* ((gptel-chat-content-indentation 2)
             (body (concat "first paragraph\n"
                           "\n"
                           "second paragraph\n"
                           "\n"
                           "* A heading"))
             (indented (gptel-chat-test--indent-body body))
             (recovered (gptel-chat--dedent indented)))
        ;; The blank lines stayed empty (no trailing whitespace).
        (expect (nth 1 (split-string indented "\n")) :to-equal "")
        (expect (nth 3 (split-string indented "\n")) :to-equal "")
        (expect recovered :to-equal body)))

    (it "round-trips a body with nested deeper indentation"
      ;; The indenter shifts every non-blank line by the body width as a
      ;; unit; relative depth is preserved, and the dedent removes only
      ;; the common minimum, so the nesting survives the full cycle.
      (let* ((gptel-chat-content-indentation 2)
             (body (concat "Outer.\n"
                           "- item one\n"
                           "  - nested item\n"
                           "- item two"))
             (indented (gptel-chat-test--indent-body body))
             (recovered (gptel-chat--dedent indented)))
        (expect recovered :to-equal body))))

  (describe "round-trip recovery modulo a uniform shift"

    (it "recovers original content even when the buffer is re-opened at a different width"
      ;; design.md Decision 3: write at width 2, dedent under a
      ;; defcustom of 4.  The dedent measures the actual indentation
      ;; (2), not the current defcustom (4), so the original content is
      ;; recovered exactly.
      (let* ((body (concat "answer line\n"
                           "* Heading\n"
                           "#+end_assistant"))
             (indented (let ((gptel-chat-content-indentation 2))
                         (gptel-chat-test--indent-body body)))
             (recovered (let ((gptel-chat-content-indentation 4))
                          (gptel-chat--dedent indented))))
        (expect recovered :to-equal body)))

    (it "a content body whose every line is uniformly indented loses exactly its outer level"
      ;; The measure-and-strip is symmetric by intent, not by count: a
      ;; body that is itself uniformly indented (no column-0 anchor
      ;; line) has its outermost common level removed — exactly
      ;; org-src-block behaviour.  The recovery is therefore "modulo a
      ;; uniform shift": dedent of an indented body equals the original
      ;; only when the original had a column-0 line.
      (let* ((already-indented "  a\n  b\n  c")
             (indented (let ((gptel-chat-content-indentation 2))
                         (gptel-chat-test--indent-body already-indented)))
             (recovered (gptel-chat--dedent indented)))
        ;; Indenting adds 2 more (total 4); dedent measures 4 and strips
        ;; all of it, recovering the column-0 form, not the 2-space form.
        (expect indented   :to-equal "    a\n    b\n    c")
        (expect recovered  :to-equal "a\nb\nc")))))


;;; ---------------------------------------------------------------
;;; Full pipeline: parse -> message -> re-indent -> parse
;;; ---------------------------------------------------------------

(describe "parse -> message -> write-back -> parse round-trip"

  ;; The full cycle:
  ;;   (1) Buffer contains an assistant body whose lines are indented
  ;;       (the on-disk form produced by the streaming indenter).
  ;;   (2) Parse the buffer -> turn list.
  ;;   (3) Convert turns -> messages; the parser's `gptel-chat--dedent'
  ;;       strips the body indentation.
  ;;   (4) Re-indent the message text as if it were being re-streamed.
  ;;   (5) Wrap the re-indented body in `#+begin_assistant' /
  ;;       `#+end_assistant' and parse again; the resulting message
  ;;       text equals the message text from step (3).

  (it "round-trips an indented assistant body through the full pipeline"
    (let* ((gptel-chat-content-indentation 2)
           (assistant-body
            (concat "  Here is a heading-shaped line:\n"
                    "  * My Heading\n"
                    "  and a delimiter-shaped line:\n"
                    "  #+end_assistant\n"
                    "  and more text.\n"))
           (buffer-text
            (concat "#+begin_assistant\n"
                    assistant-body
                    "#+end_assistant\n"))
           ;; Step 2-3: parse + message-construct the starting buffer.
           (msgs-original
            (gptel-chat-test--with-buffer buffer-text
              (gptel-chat-turns-to-messages
               (gptel-chat-parse-buffer))))
           (message-text (cdr (car msgs-original)))
           ;; Step 4: re-indent the message text line-by-line.
           (reindented (gptel-chat-test--indent-body message-text))
           ;; Step 5: embed the re-indented body back in an assistant
           ;; block and reparse.
           (buffer-text-2
            (concat "#+begin_assistant\n"
                    reindented
                    (unless (string-suffix-p "\n" reindented) "\n")
                    "#+end_assistant\n"))
           (msgs-roundtripped
            (gptel-chat-test--with-buffer buffer-text-2
              (gptel-chat-turns-to-messages
               (gptel-chat-parse-buffer)))))
      ;; The dedented message text has column-0 structural tokens.
      (expect message-text :to-match "^\\* My Heading$")
      (expect message-text :to-match "^#\\+end_assistant$")
      ;; After re-indenting, the leading spaces are back.
      (expect reindented :to-match "^  \\* My Heading$")
      (expect reindented :to-match "^  #\\+end_assistant$")
      ;; And round-tripping yields the same message content.
      (expect msgs-roundtripped :to-equal msgs-original)))

  (it "round-trips an indented user body through the full pipeline"
    (let* ((gptel-chat-content-indentation 2)
           (user-body
            (concat "  Quoting a delimiter literally:\n"
                    "  #+end_user\n"
                    "  to illustrate.\n"))
           (buffer-text (concat "#+begin_user\n"
                                user-body
                                "#+end_user\n"))
           (msgs (gptel-chat-test--with-buffer buffer-text
                   (gptel-chat-turns-to-messages
                    (gptel-chat-parse-buffer))))
           (text (cdr (car msgs))))
      (expect (car (car msgs)) :to-equal 'prompt)
      ;; Body indentation stripped: the delimiter literal is column-0.
      (expect text :to-match "^#\\+end_user$")
      ;; Re-indent + reparse recovers the same message.
      (let* ((reindented (gptel-chat-test--indent-body text))
             (buffer-text-2
              (concat "#+begin_user\n"
                      reindented
                      (unless (string-suffix-p "\n" reindented) "\n")
                      "#+end_user\n"))
             (msgs-2 (gptel-chat-test--with-buffer buffer-text-2
                       (gptel-chat-turns-to-messages
                        (gptel-chat-parse-buffer)))))
        (expect msgs-2 :to-equal msgs)))))

(provide 'body-indent-round-trip-spec)

;;; body-indent-round-trip-spec.el ends here
