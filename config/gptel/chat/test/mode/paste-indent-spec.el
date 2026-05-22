;;; paste-indent-spec.el --- deferred paste/yank body indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for the chat-mode paste/yank body-indentation path
;; (`openspec/changes/gptel-chat-heading-scoping/design.md' Decision 6,
;; the `Chat-block body indentation' requirement of
;; `specs/gptel/chat-mode.md').
;;
;; The path is deliberately split in two:
;;
;;   - `gptel-chat--indent-inserted-region' runs from
;;     `after-change-functions'.  It does NOT mutate the buffer — it
;;     only RECORDS the inserted region and schedules an idle timer.
;;     Inserting from within an after-change dispatch corrupts the
;;     change bookkeeping of `org-indent-mode' / `org-element', so the
;;     mutation must happen outside the dispatch.
;;   - `gptel-chat--indent-pending-regions' runs from that timer (and
;;     is callable directly — these specs drive it directly rather
;;     than waiting for idle time).  It shifts each recorded region via
;;     `gptel-chat--indent-region' so its least-indented line reaches
;;     the body indent (`gptel-chat--body-indent', default 2).
;;
;; The shift is applied to the region as a unit, so relative
;; indentation of nested content is preserved; it is
;; `max(0, body-indent - region-minimum-indent)', so an
;; already-indented paste is a no-op.  Coverage:
;;
;;   1. The after-change handler records without mutating; draining
;;      the deferred indenter performs the shift.
;;   2. A multi-line paste into a chat-block body is shifted to the
;;      body indent.
;;   3. Relative indentation of nested content is preserved.
;;   4. An already-indented paste is idempotent (shift 0).
;;   5. A paste straddling a `#+end_*' delimiter shifts only the
;;      in-body portion.
;;   6. A pure deletion (END = BEG) records nothing.
;;   7. A paste outside any chat block is never shifted.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add the source dir to `load-path' so `require' below resolves.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

;; The handler calls `gptel-chat--point-in-block-body-p' at runtime, so
;; the parser module must be loaded before it fires.
(require 'gptel-chat-parser)
(require 'gptel-chat-mode)

;;; ---------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------

(defmacro gptel-chat-paste-test--with-chat-buffer (content &rest body)
  "Activate `gptel-chat-mode' in a temp buffer, insert CONTENT, run BODY.

CONTENT is inserted *before* mode activation so the mode hook does
not see it as an after-change event — which keeps the fixture
deterministic regardless of the on-read migration pass.  Point is
left at `point-min' after activation; tests reposition it
explicitly."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (gptel-chat-mode)
     (goto-char (point-min))
     ,@body))

(defun gptel-chat-paste-test--goto-marker (marker)
  "Move point to the first occurrence of MARKER and delete the marker.
Returns the position where the marker was."
  (goto-char (point-min))
  (search-forward marker)
  (let ((pos (match-beginning 0)))
    (delete-region pos (match-end 0))
    (goto-char pos)
    pos))

(defun gptel-chat-paste-test--paste (text)
  "Insert TEXT at point, then drain the deferred body-indenter.
`gptel-chat--indent-inserted-region' (the `after-change' hook) only
RECORDS the inserted region; the shift runs later from an idle timer.
Tests drive that timer's callback, `gptel-chat--indent-pending-regions',
directly instead of waiting for idle time."
  (insert text)
  (gptel-chat--indent-pending-regions))

;;; ---------------------------------------------------------------
;;; Specs
;;; ---------------------------------------------------------------

(describe "gptel-chat paste/yank body indentation"

  ;; -------------------------------------------------------------------
  ;; The after-change handler is non-mutating: it records the inserted
  ;; region and the shift is deferred.  Inserting from within an
  ;; after-change dispatch corrupts org-indent / org-element change
  ;; bookkeeping, so the mutation must run outside the dispatch.
  ;; -------------------------------------------------------------------
  (it "records the region without mutating until the indenter is drained"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (insert "* H1\n")
      ;; The after-change handler only records — the buffer is not
      ;; shifted yet.
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              "#+begin_user\n* H1\n#+end_user\n")
      ;; Draining the deferred indenter performs the shift.
      (gptel-chat--indent-pending-regions)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              "#+begin_user\n  * H1\n#+end_user\n")))

  ;; -------------------------------------------------------------------
  ;; A multi-line paste into a chat-block body is shifted as a unit so
  ;; its least-indented line lands at the body indent (default 2).
  ;; -------------------------------------------------------------------
  (it "shifts a multi-line paste to the body indent"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; Paste three column-0 lines, two of them heading-shaped.
      (gptel-chat-paste-test--paste "* H1\n- list\n** H2\n")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  * H1\n"
                      "  - list\n"
                      "  ** H2\n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; The region shifts as a unit: the deepest line keeps its extra
  ;; indentation relative to the shallowest line.
  ;; -------------------------------------------------------------------
  (it "preserves the relative indentation of nested content"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; The least-indented line is column 0; the nested list is two
      ;; spaces deeper.  After the shift the whole region moves +2, so
      ;; the list keeps its +2 offset relative to the heading.
      (gptel-chat-paste-test--paste "* H1\n  - nested\n    - deeper\n")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  * H1\n"
                      "    - nested\n"
                      "      - deeper\n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; A paste whose minimum indentation already meets the body indent is
  ;; left unchanged — shift is `max(0, indent - min)' = 0.
  ;; -------------------------------------------------------------------
  (it "is idempotent on already-indented content (shift is zero)"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; Every line is already indented at or beyond the body width.
      (gptel-chat-paste-test--paste "  * H1\n  - list\n      deep\n")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  * H1\n"
                      "  - list\n"
                      "      deep\n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; A paste that straddles `#+end_*' shifts only the lines whose BOL
  ;; falls strictly inside the surviving (first) block body; the
  ;; delimiter line and content past it stay at column 0.
  ;; -------------------------------------------------------------------
  (it "shifts only the in-body portion of a paste crossing a delimiter"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "<HERE>"
                "#+end_user\n")
      (gptel-chat-paste-test--goto-marker "<HERE>")
      ;; The pasted text carries its own `#+end_user' line.  After
      ;; insertion there are two `#+end_user' lines; only lines whose
      ;; BOL is strictly inside the first block body are shifted.
      (gptel-chat-paste-test--paste
       "* before-closer\n#+end_user\n* after-closer\n")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  * before-closer\n"
                      "#+end_user\n"
                      "* after-closer\n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; A pure deletion fires `after-change-functions' with END = BEG; the
  ;; recorder's `(> end beg)' gate short-circuits, so nothing is
  ;; recorded and a drain is a no-op.
  ;; -------------------------------------------------------------------
  (it "records nothing on a pure deletion (END = BEG)"
    (gptel-chat-paste-test--with-chat-buffer
        (concat "#+begin_user\n"
                "  body line\n"
                "  trailing\n"
                "#+end_user\n")
      (goto-char (point-min))
      (search-forward "trailing")
      ;; Delete the word `trailing' — a pure deletion (END = BEG).
      (delete-region (match-beginning 0) (match-end 0))
      (gptel-chat--indent-pending-regions)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "#+begin_user\n"
                      "  body line\n"
                      "  \n"
                      "#+end_user\n"))))

  ;; -------------------------------------------------------------------
  ;; A paste entirely outside any chat block is never shifted — the
  ;; per-line predicate returns nil for every candidate line.
  ;; -------------------------------------------------------------------
  (it "does not shift a paste made outside any chat block"
    (gptel-chat-paste-test--with-chat-buffer
        "<HERE>"
      (gptel-chat-paste-test--goto-marker "<HERE>")
      (gptel-chat-paste-test--paste "* H1\n- list\n** H2\n")
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal
              (concat "* H1\n"
                      "- list\n"
                      "** H2\n")))))

(provide 'gptel-chat-paste-indent-spec)

;;; paste-indent-spec.el ends here
