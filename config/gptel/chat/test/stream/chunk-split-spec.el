;;; chunk-split-spec.el --- Buttercup tests for split-across-chunks body indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Headline scenario from spec §"Response streaming and sanitization":
;;
;;   WHEN one chunk ends with `\n* He' and the next begins with
;;        `ading\nmore'
;;   THEN the per-line holdback completes the line as `* Heading' and
;;        indents it by the body width before insertion
;;
;; This is the reason the streaming closure carries a one-line
;; holdback instead of indenting each raw chunk independently.
;; design.md §Decision 6: the per-line indenter must see a *complete*
;; line, so a line split across chunk boundaries has to be recomposed
;; first.  Without the holdback, the indenter would prefix each
;; partial fragment instead of the whole line.
;;
;; Also covers variants that exercise the same holdback mechanism:
;; splits at other byte offsets, and a line split across three chunks
;; (to confirm the holdback is not just a one-chunk lookback).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test.  `gptel-chat-mode' owns the
;; `gptel-chat--body-indent' accessor consulted by the indenter.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-mode)
(require 'gptel-chat-stream)


;;; Fixtures -----------------------------------------------------------------

(defvar gptel-chat-chunk-split-test--buffer nil)
(defvar gptel-chat-chunk-split-test--marker nil)

(defun gptel-chat-chunk-split-test--setup ()
  (setq gptel-chat-chunk-split-test--buffer
        (generate-new-buffer " *gptel-chat-chunk-split-test*"))
  (with-current-buffer gptel-chat-chunk-split-test--buffer
    (setq gptel-chat-chunk-split-test--marker
          (copy-marker (point-min) t))))

(defun gptel-chat-chunk-split-test--cleanup ()
  (when (buffer-live-p gptel-chat-chunk-split-test--buffer)
    (kill-buffer gptel-chat-chunk-split-test--buffer))
  (setq gptel-chat-chunk-split-test--buffer nil
        gptel-chat-chunk-split-test--marker nil))

(defun gptel-chat-chunk-split-test--buffer-string ()
  (with-current-buffer gptel-chat-chunk-split-test--buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;;; Specs --------------------------------------------------------------------

(describe "Split-across-chunks body indentation"

  (before-each (gptel-chat-chunk-split-test--setup))
  (after-each  (gptel-chat-chunk-split-test--cleanup))

  (describe "headline scenario from spec §Response streaming and sanitization"

    (it "indents a heading split as `* He' + `ading\\nmore'"
      ;; The first chunk holds `* He' back (no embedded newline); the
      ;; second chunk supplies `ading\n' which completes the line.
      ;; The completed `* Heading' is indented by the body width
      ;; before insertion.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "* He")                      ; chunk 1 — all holdback
        ;; Mid-sequence: nothing is committed yet because no newline
        ;; has arrived.
        (expect (gptel-chat-chunk-split-test--buffer-string)
                :to-equal "")
        (funcall cb "ading\nmore")               ; chunk 2 — completes the line
        (funcall cb t)                            ; flush trailing "more"
        (expect (gptel-chat-chunk-split-test--buffer-string)
                :to-equal "  * Heading\n  more")))

    (it "indents an end-delimiter split as `#+end_ass' + `istant\\nmore'"
      ;; A streamed `#+end_assistant' line is body content; recomposing
      ;; it across a chunk boundary and indenting it keeps the
      ;; containing block well-formed.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "#+end_ass")
        (funcall cb "istant\nmore")
        (funcall cb t)
        (expect (gptel-chat-chunk-split-test--buffer-string)
                :to-equal "  #+end_assistant\n  more"))))

  (describe "variants exercising the same holdback mechanism"

    (it "indents a line split after a single char"
      ;; Chunk 1 ends with a single `#', chunk 2 supplies the rest.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "prose\n#")
        (funcall cb "+end_user\nmore\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "  prose\n  #+end_user\n  more\n"))

    (it "indents a line split across three chunks"
      ;; `#+end_tool' split as `#+end' + `_to' + `ol\n'.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "#+end")
        (funcall cb "_to")
        (funcall cb "ol\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "  #+end_tool\n"))

    (it "indents a heading split across chunks"
      ;; Recomposed across a boundary, the completed `*** Deep' line is
      ;; indented like any other body line.
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "*** De")
        (funcall cb "ep\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "  *** Deep\n")))

  (describe "holdback preserves in-order inserts around a split line"

    (it "content before and after the split appears in correct order"
      (let* ((handle (gptel-chat--make-stream-inserter
                      gptel-chat-chunk-split-test--marker))
             (cb (gptel-chat-stream-insert handle)))
        (funcall cb "line1\nlin")               ; emit line1; holdback = "lin"
        (funcall cb "e2\n* Head")               ; emit line2;  holdback = "* Head"
        (funcall cb "ing\nafter\n")             ; emit recomposed heading, then after
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal
              "  line1\n  line2\n  * Heading\n  after\n"))))

(provide 'chunk-split-spec)

;;; chunk-split-spec.el ends here
