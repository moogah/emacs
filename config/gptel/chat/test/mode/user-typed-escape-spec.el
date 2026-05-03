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

    (it "does not escape on a `#+begin_user' delimiter line itself (column-1 guard short-circuits)"
      ;; This spec exercises the *column-1 guard's* short-circuit arm.
      ;; Typing `*' on a `#+begin_user' line is contrived (the line
      ;; starts with `#'); we position point at column 1 of the opener
      ;; line and type `*'.  After insertion point is at column 2, so
      ;; the function's `(= (current-column) 1)' guard rejects
      ;; immediately and the predicate is never consulted.
      ;;
      ;; The *predicate's* delimiter-line rejection arm is exercised
      ;; by the sibling spec below (which calls
      ;; `gptel-chat--escape-typed-heading' directly to bypass the
      ;; column-1 guard).  Together they cover both arms of the
      ;; delimiter-line invariant
      ;; (`register/invariant/chat-block-delimiter-lines-stay-at-column-0').
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\nbody\n#+end_user\n"
        (gptel-chat-test--goto-line-col 1 1)
        (gptel-chat-test--type-char ?*)
        ;; Result: `*' is inserted at column 1 of "#+begin_user".
        ;; Function bails on the column-1 check (point is at column
        ;; 2 after insert), so no escape is applied.
        (expect (buffer-substring-no-properties (point-min) (point-max))
                :to-equal
                "#*+begin_user\nbody\n#+end_user\n")))

    (it "does not escape when called with point on a delimiter line (predicate enforcement)"
      ;; Direct-call spec: bypass `self-insert-command' and invoke
      ;; `gptel-chat--escape-typed-heading' with point at column 1 of a
      ;; `#+begin_user' line.  The column-1 guard now succeeds (point
      ;; is at column 1) and `last-command-event' is bound to `?*', so
      ;; the third condition — the predicate call inside the `(when
      ;; ...)' — is the load-bearing check.  Without that
      ;; predicate-call condition, typed-escape would proceed past the
      ;; col=1 guard and insert a prefix at BOL of `#+begin_user',
      ;; mutating the delimiter line and violating
      ;; `register/invariant/chat-block-delimiter-lines-stay-at-column-0'.
      ;; Note: the predicate's internal delimiter-line short-circuit
      ;; arm (`parser.el' point-in-block-body-p) is covered separately
      ;; by `point-in-block-spec.el' (the nested-tool scenario where
      ;; removing the short-circuit would let the predicate return t).
      ;; This spec catches a maintainer dropping the predicate call
      ;; from `gptel-chat--escape-typed-heading', not the predicate's
      ;; internal arm.
      (gptel-chat-test--in-chat-buffer
          "#+begin_user\nhi\n#+end_user\n"
        (gptel-chat-test--goto-line-col 1 1) ; column 1 of "#+begin_user"
        (let ((last-command-event ?*)
              (before (buffer-substring-no-properties
                       (point-min) (point-max))))
          (gptel-chat--escape-typed-heading)
          (expect (buffer-substring-no-properties (point-min) (point-max))
                  :to-equal before))))

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
