;;; user-typed-escape-spec.el --- post-self-insert-hook heading escape -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--escape-typed-heading', the
;; `post-self-insert-hook' function that inserts a leading
;; indent-prefix in front of a column-0 `*' typed inside a
;; chat-block body (see
;; `openspec/changes/gptel-chat-heading-scoping/design.md' §Decision 2
;; and `tasks/open/add-user-typed-heading-escape.md').
;;
;; These specs exercise the function via `self-insert-command' so
;; the real `post-self-insert-hook' runs and `last-command-event' is
;; bound by Emacs's input loop simulator — i.e., the integration
;; surface a user actually hits.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

(require 'gptel-chat-mode)
(require 'gptel-chat-parser)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defun gptel-chat-test--type-char (char)
  "Simulate typing CHAR via `self-insert-command'.

Binds `last-command-event' the way Emacs's input loop would and
runs `post-self-insert-hook' as a side effect of
`self-insert-command'."
  (let ((last-command-event char))
    (self-insert-command 1)))

(defmacro gptel-chat-test--in-chat-buffer (content &rest body)
  "Activate `gptel-chat-mode' in a temp buffer with CONTENT, run BODY.

CONTENT is inserted before mode activation so post-mode-activation
hooks see a populated buffer; point is left at point-min after
mode activation, ready for caller-driven positioning."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (goto-char (point-min))
     ,@body))

(defun gptel-chat-test--goto-line-col (line col)
  "Move point to 1-based LINE and 0-based COL in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column col))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "gptel-chat--escape-typed-heading"

  (describe "trigger conditions"

    (it "escapes a column-0 `*' typed inside a `#+begin_user' body"
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\n\n#+end_user\n"
        ;; Position on the empty body line (line 2).
        (gptel-chat-test--goto-line-col 2 0)
        (gptel-chat-test--type-char ?*)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#+begin_user\n *\n#+end_user\n")
        ;; Cursor-after semantics: point should be past the typed `*'.
        (expect (current-column) :to-equal 2)))

    (it "does not escape `*' typed at column 5 inside a chat block"
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\nhello\n#+end_user\n"
        ;; Append `*' at end of "hello" (column 5 of line 2).
        (gptel-chat-test--goto-line-col 2 5)
        (gptel-chat-test--type-char ?*)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#+begin_user\nhello*\n#+end_user\n")))

    (it "does not escape `*' typed at column 0 outside any chat block"
      (gptel-chat-test--in-chat-buffer
          "prelude line\n"
        ;; Brand new line at point-min; type `*' at column 0.
        (gptel-chat-test--goto-line-col 1 0)
        (gptel-chat-test--type-char ?*)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "*prelude line\n")))

    (it "does not escape on a `#+begin_user' delimiter line itself"
      ;; Typing `*' on a `#+begin_user' line is contrived (the line
      ;; starts with `#'), but the predicate must still exclude it.
      ;; We simulate by deleting the leading `#' first to expose the
      ;; line, then typing `*' there — the predicate sees the line
      ;; start as the (now broken) opener line, but practically the
      ;; predicate's delimiter check guards us against this whole
      ;; class.  Easier: position on a real opener line at a column
      ;; where typing `*' could plausibly land it at column 1, and
      ;; verify no escape applies.
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\nbody\n#+end_user\n"
        ;; Place point at column 0 of the opener line.  We can't
        ;; insert `*' there at column 0 *and* still have it be the
        ;; opener line afterward (the buffer becomes
        ;; "*#+begin_user..."), but the test is: when the function
        ;; runs after a column-0 `*' insertion on what is now a
        ;; non-delimiter line, does the predicate guard hold?
        ;;
        ;; The realistic delimiter-line check happens when `*' is
        ;; typed at column 1 on an opener line: that's column 1, not
        ;; column 0, so the function's column-1 guard rejects
        ;; immediately and the predicate is never consulted.
        (gptel-chat-test--goto-line-col 1 1)
        (gptel-chat-test--type-char ?*)
        ;; Result: `*' is inserted at column 1 of "#+begin_user".
        ;; Function bails on the column-1 check (point is at column
        ;; 2 after insert), so no escape is applied.
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#*+begin_user\nbody\n#+end_user\n")))

    (it "escapes a column-0 `*' typed inside a nested `#+begin_tool' body"
      (gptel-chat-test--in-chat-buffer
          (concat "#+begin_assistant\n"
                  "intro\n"
                  "#+begin_tool (foo)\n"
                  "\n"             ; line 4 — empty tool body line
                  "#+end_tool\n"
                  "outro\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 4 0)
        (gptel-chat-test--type-char ?*)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                (concat "#+begin_assistant\n"
                        "intro\n"
                        "#+begin_tool (foo)\n"
                        " *\n"
                        "#+end_tool\n"
                        "outro\n"
                        "#+end_assistant\n"))
        (expect (current-column) :to-equal 2)))

    (it "does not escape when a non-`*' character is typed at column 0"
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\n\n#+end_user\n"
        (gptel-chat-test--goto-line-col 2 0)
        (gptel-chat-test--type-char ?a)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#+begin_user\na\n#+end_user\n")))

    (it "escapes the first `*', appends the second without re-escaping"
      ;; Sequential `*'+`*' at column 0: first becomes ` *', the
      ;; second is now typed at column 2 (not column 0), so no
      ;; further escape applies.  Result: ` **'.
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\n\n#+end_user\n"
        (gptel-chat-test--goto-line-col 2 0)
        (gptel-chat-test--type-char ?*)
        (gptel-chat-test--type-char ?*)
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#+begin_user\n **\n#+end_user\n")
        (expect (current-column) :to-equal 3))))

  (describe "configurable indent width"

    (it "uses `gptel-chat-content-indentation' = 2 when set to 2"
      (let ((gptel-chat-content-indentation 2))
        (gptel-chat-test--in-chat-buffer
            "#+begin_user\n\n#+end_user\n"
          (gptel-chat-test--goto-line-col 2 0)
          (gptel-chat-test--type-char ?*)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal
                  "#+begin_user\n  *\n#+end_user\n")
          (expect (current-column) :to-equal 3))))))

(provide 'user-typed-escape-spec)

;;; user-typed-escape-spec.el ends here
