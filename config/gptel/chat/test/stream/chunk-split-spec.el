;;; chunk-split-spec.el --- Buttercup tests for split-across-chunks collision escape -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Headline scenario from spec §"Response streaming and sanitization":
;;
;;   WHEN one chunk ends with `#+end_ass' and the next begins with
;;        `istant\nmore'
;;   THEN the completed line is recognized as a collision and
;;        escaped before final insertion
;;
;; This is the reason the streaming closure carries a one-line
;; holdback instead of sanitizing each raw chunk independently.
;; Design.md §Decision 3b: without a holdback, the two chunks each
;; look like harmless prose to the line-level sanitizer; only after
;; concatenation does the composite line `#+end_assistant' become
;; recognizable.
;;
;; Also covers variants that exercise the same holdback mechanism:
;; collisions split at other byte offsets, and a collision split
;; across three chunks (to confirm the holdback is not just a
;; one-chunk lookback).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

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

(describe "Split-across-chunks collision escaping"

  (before-each (gptel-chat-chunk-split-test--setup))
  (after-each  (gptel-chat-chunk-split-test--cleanup))

  (describe "headline scenario from spec §Response streaming and sanitization"

    (it "escapes `#+end_assistant' split as `#+end_ass' + `istant\\nmore'"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "#+end_ass")                ; chunk 1 — all holdback
        ;; Mid-sequence: nothing is committed yet because no newline
        ;; has arrived.
        (expect (gptel-chat-chunk-split-test--buffer-string)
                :to-equal "")
        (funcall cb "istant\nmore")             ; chunk 2 — completes the line
        (funcall cb t)                           ; flush trailing "more"
        (expect (gptel-chat-chunk-split-test--buffer-string)
                :to-equal ",#+end_assistant\nmore"))))

  (describe "variants exercising the same holdback mechanism"

    (it "escapes a collision split after a single char"
      ;; Chunk 1 ends with a single `#', chunk 2 supplies the rest.
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "prose\n#")
        (funcall cb "+end_user\nmore\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "prose\n,#+end_user\nmore\n"))

    (it "escapes a collision split across three chunks"
      ;; `#+end_tool' split as `#+end` + `_to` + `ol\n'.
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "#+end")
        (funcall cb "_to")
        (funcall cb "ol\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal ",#+end_tool\n"))

    (it "escapes a case-variant collision split across chunks"
      ;; Mixed case recomposed across a boundary — still escaped
      ;; because the sanitizer uses case-fold-search.
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "#+End_Ass")
        (funcall cb "istant\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal ",#+End_Assistant\n"))

    (it "does NOT escape a recomposed #+end_src (not one of the three delimiters)"
      ;; Negative control: split `#+end_src' across chunks; the
      ;; recomposed line must pass through untouched.
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "#+end_")
        (funcall cb "src\n")
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "#+end_src\n")))

  (describe "holdback preserves in-order inserts around a split collision"

    (it "content before and after the split appears in correct order"
      (let ((cb (gptel-chat--make-stream-closure
                 gptel-chat-chunk-split-test--marker)))
        (funcall cb "line1\nlin")               ; emit line1; holdback = "lin"
        (funcall cb "e2\n#+end_")                ; emit line2;  holdback = "#+end_"
        (funcall cb "assistant\nafter\n")        ; emit escaped collision; then after
        (funcall cb t))
      (expect (gptel-chat-chunk-split-test--buffer-string)
              :to-equal "line1\nline2\n,#+end_assistant\nafter\n"))))

(provide 'chunk-split-spec)

;;; chunk-split-spec.el ends here
