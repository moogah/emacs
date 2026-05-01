;;; point-in-block-spec.el --- Predicate: is point inside a chat block body? -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--point-in-block-body-p', the
;; sub-millisecond predicate that gates the chat-heading-collision
;; escape boundary (design.md Decisions 2 and 3).
;;
;; Coverage targets the contract from
;; `openspec/changes/gptel-chat-heading-scoping/tasks/open/add-point-in-block-body-predicate.md':
;;
;;   - POS strictly inside a `#+begin_user' / `#+begin_assistant'
;;     body returns non-nil.
;;   - POS inside a `#+begin_tool' nested inside `#+begin_assistant'
;;     also returns non-nil (a `*'-heading inside a tool body would
;;     corrupt the surrounding assistant block too).
;;   - POS on either the opener or the closer delimiter line returns
;;     nil (per the chat-block-delimiter-lines-stay-at-column-0
;;     invariant).
;;   - POS in pre-block metadata, between blocks, or in a
;;     blocks-free buffer returns nil.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load shared fixtures (sibling of this file's parent directory).
(let ((helpers (expand-file-name
                "../test-helpers.el"
                (file-name-directory (or load-file-name buffer-file-name)))))
  (load helpers nil t))

(require 'gptel-chat-parser)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defun gptel-chat-test--goto-line-col (line col)
  "Move point to 1-based LINE and 0-based COL in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column col))

(defmacro gptel-chat-test--with-content (content &rest body)
  "Insert CONTENT in a temp buffer, leave point at point-min, run BODY."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; ---------------------------------------------------------------
;;; Inside / outside basic cases
;;; ---------------------------------------------------------------

(describe "gptel-chat--point-in-block-body-p"

  (describe "inside an outer block body"

    (it "returns non-nil for point strictly inside a user body"
      (gptel-chat-test--with-content
          "#+begin_user\nhello world\n#+end_user\n"
        ;; line 2, column 0 — first char of "hello world".
        (gptel-chat-test--goto-line-col 2 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy)))

    (it "returns non-nil for point strictly inside an assistant body"
      (gptel-chat-test--with-content
          "#+begin_assistant\nthe answer is 42\n#+end_assistant\n"
        (gptel-chat-test--goto-line-col 2 7)
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy)))

    (it "returns non-nil at column 0 of a body line that is not a delimiter"
      ;; A `* heading' line lives at column 0.  The predicate must
      ;; still return non-nil so the user-typed escape can decide to
      ;; indent it.
      (gptel-chat-test--with-content
          "#+begin_assistant\n* heading\n#+end_assistant\n"
        (gptel-chat-test--goto-line-col 2 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy)))

    (it "returns non-nil at a column past EOL of a body line"
      ;; Point at the newline at end of body line (still inside body).
      (gptel-chat-test--with-content
          "#+begin_user\nshort\n#+end_user\n"
        (gptel-chat-test--goto-line-col 2 5)  ; right after "short"
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy))))

  (describe "on a delimiter line"

    (it "returns nil for point on the `#+begin_user' opener line"
      (gptel-chat-test--with-content
          "#+begin_user\nhi\n#+end_user\n"
        (gptel-chat-test--goto-line-col 1 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point at end of the opener line"
      (gptel-chat-test--with-content
          "#+begin_user\nhi\n#+end_user\n"
        (gptel-chat-test--goto-line-col 1 12)  ; after `r' in `_user'
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point on the `#+end_user' closer line"
      (gptel-chat-test--with-content
          "#+begin_user\nhi\n#+end_user\n"
        (gptel-chat-test--goto-line-col 3 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point on `#+begin_assistant'"
      (gptel-chat-test--with-content
          "#+begin_assistant\nfoo\n#+end_assistant\n"
        (gptel-chat-test--goto-line-col 1 5)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point on `#+end_assistant'"
      (gptel-chat-test--with-content
          "#+begin_assistant\nfoo\n#+end_assistant\n"
        (gptel-chat-test--goto-line-col 3 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point on a `#+begin_tool' line nested in assistant"
      (gptel-chat-test--with-content
          (concat "#+begin_assistant\n"
                  "before\n"
                  "#+begin_tool (foo)\n"
                  "result\n"
                  "#+end_tool\n"
                  "after\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 3 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point on a `#+end_tool' line nested in assistant"
      (gptel-chat-test--with-content
          (concat "#+begin_assistant\n"
                  "before\n"
                  "#+begin_tool (foo)\n"
                  "result\n"
                  "#+end_tool\n"
                  "after\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 5 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil))))

  (describe "nested tool block inside assistant"

    (it "returns non-nil for point inside a tool body"
      (gptel-chat-test--with-content
          (concat "#+begin_assistant\n"
                  "intro text\n"
                  "#+begin_tool (search :q \"foo\")\n"
                  "result-line-1\n"
                  "result-line-2\n"
                  "#+end_tool\n"
                  "outro\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 4 0)  ; "result-line-1"
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy)))

    (it "returns non-nil for point in assistant prose between two tool blocks"
      ;; Walk-backward from POS hits `#+end_tool' first; the stack
      ;; algorithm pops past the closed tool block and finds
      ;; `#+begin_assistant' as the actual enclosing opener.
      (gptel-chat-test--with-content
          (concat "#+begin_assistant\n"
                  "first text\n"
                  "#+begin_tool (a)\n"
                  "ra\n"
                  "#+end_tool\n"
                  "middle prose\n"   ; line 6 — POS here
                  "#+begin_tool (b)\n"
                  "rb\n"
                  "#+end_tool\n"
                  "tail\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 6 4)
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy)))

    (it "returns non-nil for assistant prose AFTER a closed tool block"
      (gptel-chat-test--with-content
          (concat "#+begin_assistant\n"
                  "#+begin_tool (a)\n"
                  "ra\n"
                  "#+end_tool\n"
                  "tail prose\n"   ; line 5
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 5 0)
        (expect (gptel-chat--point-in-block-body-p) :to-be-truthy))))

  (describe "outside any block"

    (it "returns nil at point-min of an empty buffer"
      (gptel-chat-test--with-content ""
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for a buffer of only metadata / drawer / heading"
      (gptel-chat-test--with-content
          (concat ":PROPERTIES:\n"
                  ":ID:       abc-123\n"
                  ":END:\n"
                  "#+title: My Chat\n"
                  "* Heading\n"
                  "Some prose.\n")
        (gptel-chat-test--goto-line-col 6 3)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point in pre-block metadata before the first block"
      (gptel-chat-test--with-content
          (concat ":PROPERTIES:\n"
                  ":ID:       abc-123\n"   ; line 2 — POS here
                  ":END:\n"
                  "#+begin_user\n"
                  "hi\n"
                  "#+end_user\n")
        (gptel-chat-test--goto-line-col 2 4)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point between two blocks"
      (gptel-chat-test--with-content
          (concat "#+begin_user\n"
                  "first\n"
                  "#+end_user\n"
                  "between\n"          ; line 4 — POS here
                  "#+begin_assistant\n"
                  "second\n"
                  "#+end_assistant\n")
        (gptel-chat-test--goto-line-col 4 3)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil)))

    (it "returns nil for point after the final closer"
      (gptel-chat-test--with-content
          (concat "#+begin_user\n"
                  "hi\n"
                  "#+end_user\n"
                  "trailing prose\n")
        (gptel-chat-test--goto-line-col 4 5)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil))))

  (describe "malformed structure"

    (it "returns nil when the enclosing block is unclosed"
      ;; Mid-stream / mid-edit: the closer hasn't been written yet.
      ;; The escape boundary's safe failure mode is to leave user
      ;; input untouched, so the predicate returns nil.
      (gptel-chat-test--with-content
          (concat "#+begin_user\n"
                  "hi\n"
                  "no-closer\n")
        (gptel-chat-test--goto-line-col 3 4)
        (expect (gptel-chat--point-in-block-body-p) :to-be nil))))

  (describe "POS and BUFFER arguments"

    (it "respects an explicit POS argument"
      (gptel-chat-test--with-content
          "#+begin_user\nhello\n#+end_user\n"
        ;; Point sits at point-min (delimiter line) but we ask about
        ;; an explicit position inside the body.
        (let* ((target (save-excursion
                         (goto-char (point-min))
                         (forward-line 1)
                         (point))))
          (expect (gptel-chat--point-in-block-body-p target)
                  :to-be-truthy)
          ;; And the implicit-point call still returns nil (point at
          ;; the delimiter line).
          (expect (gptel-chat--point-in-block-body-p) :to-be nil))))

    (it "respects an explicit BUFFER argument"
      (let ((buf (generate-new-buffer " *gptel-chat-test*")))
        (unwind-protect
            (with-current-buffer buf
              (insert "#+begin_user\nin-buf\n#+end_user\n")
              (goto-char (point-min))
              (forward-line 1)
              ;; Switch context to a different buffer, then ask
              ;; about BUF explicitly.
              (with-temp-buffer
                (expect (gptel-chat--point-in-block-body-p
                         (with-current-buffer buf (point))
                         buf)
                        :to-be-truthy)))
          (kill-buffer buf))))))

(provide 'point-in-block-spec)

;;; point-in-block-spec.el ends here
