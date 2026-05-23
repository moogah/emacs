;;; indent-line-spec.el --- chat-mode indent-line-function -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat--indent-line', the buffer-local
;; `indent-line-function' of `gptel-chat-mode'.  See
;; `openspec/changes/gptel-chat-heading-scoping/design.md' Decision 6
;; and the `Chat-block body indentation' requirement of
;; `specs/gptel/chat-mode.md'.
;;
;; A chat-block body is an indented region.  The indent function
;; keeps it that way for ordinary typing: a body line indented below
;; the body width (`gptel-chat--body-indent', default 2) is brought
;; up to it.  It never *reduces* indentation, and it leaves delimiter
;; lines and content outside any block entirely alone.  Coverage:
;;
;;   1. A body line indented below the body width is brought up to it.
;;   2. A delimiter line is left alone (no-op).
;;   3. Content outside any chat block is left alone (no-op).
;;   4. A body line already at or beyond the body width is not reduced.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add the source dir to `load-path' so `require' below resolves.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; `gptel-chat--indent-line' calls `gptel-chat--point-in-block-body-p'
;; at runtime, so the parser module must be loaded first.
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defmacro gptel-chat-indent-test--with-chat-buffer (content &rest body)
  "Activate `gptel-chat-mode' in a temp buffer, insert CONTENT, run BODY.

CONTENT is inserted *before* mode activation so the migration pass
does not run against caller-controlled fixtures (the specs below
supply already-indented or deliberately-malformed content and assert
on the exact result of a direct `gptel-chat--indent-line' call).
Point is left at `point-min' after activation."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (goto-char (point-min))
     ,@body))

(defun gptel-chat-indent-test--goto-line (line)
  "Move point to the start of 1-based LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "gptel-chat--indent-line"

  ;; -------------------------------------------------------------------
  ;; A body line indented below the body width is brought up to it.
  ;; -------------------------------------------------------------------
  (it "brings an under-indented body line up to the body width"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "* heading\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 2)   ; the column-0 body line
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  * heading\n"
                      "#+end_user\n"))))

  (it "brings a partially-indented body line up to the full body width"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                " under\n"          ; one space — below the width of 2
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 2)
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  under\n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; A delimiter line is left alone — the predicate returns nil for it,
  ;; so the indent function is a no-op.
  ;; -------------------------------------------------------------------
  (it "leaves a `#+begin_user' delimiter line alone"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  body\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 1)   ; the `#+begin_user' line
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  body\n"
                      "#+end_user\n"))))

  (it "leaves a `#+end_user' delimiter line alone"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  body\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 3)   ; the `#+end_user' line
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  body\n"
                      "#+end_user\n"))))

  (it "leaves a nested `#+begin_tool' delimiter line alone"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_assistant\n"
                "  intro\n"
                "#+begin_tool (foo)\n"
                "  result\n"
                "#+end_tool\n"
                "#+end_assistant\n")
      (gptel-chat-indent-test--goto-line 3)   ; the `#+begin_tool' line
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_assistant\n"
                      "  intro\n"
                      "#+begin_tool (foo)\n"
                      "  result\n"
                      "#+end_tool\n"
                      "#+end_assistant\n"))))

  ;; -------------------------------------------------------------------
  ;; Content outside any chat block is left alone — the predicate
  ;; returns nil, so a column-0 heading there stays a real heading.
  ;; -------------------------------------------------------------------
  (it "leaves a column-0 heading outside any chat block alone"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "* A top-level heading\n"
                "\n"
                "#+begin_user\n"
                "  body\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 1)   ; the out-of-block heading
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "* A top-level heading\n"
                      "\n"
                      "#+begin_user\n"
                      "  body\n"
                      "#+end_user\n"))))

  (it "leaves column-0 prose between two turn blocks alone"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  q\n"
                "#+end_user\n"
                "between-turn prose\n"
                "#+begin_assistant\n"
                "  a\n"
                "#+end_assistant\n")
      (gptel-chat-indent-test--goto-line 4)   ; the between-turn line
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  q\n"
                      "#+end_user\n"
                      "between-turn prose\n"
                      "#+begin_assistant\n"
                      "  a\n"
                      "#+end_assistant\n"))))

  ;; -------------------------------------------------------------------
  ;; A body line already at or beyond the body width is not reduced —
  ;; the function never decreases indentation.
  ;; -------------------------------------------------------------------
  (it "does not reduce a body line already at the body width"
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  exactly two\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 2)
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  exactly two\n"
                      "#+end_user\n"))))

  (it "does not reduce a body line indented deeper than the body width"
    ;; A user-indented nested list / indented code block is left
    ;; exactly as deep as the user put it.
    (gptel-chat-indent-test--with-chat-buffer
        (concat "#+begin_user\n"
                "        deeply indented\n"
                "#+end_user\n")
      (gptel-chat-indent-test--goto-line 2)
      (gptel-chat--indent-line)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "        deeply indented\n"
                      "#+end_user\n")))))

(provide 'gptel-chat-indent-line-spec)

;;; indent-line-spec.el ends here
