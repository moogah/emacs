;;; context-truncation-spec.el --- Buttercup tests for context truncation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `jf/gptel--copy-truncated-context'.
;;
;; Coverage (from
;; openspec/changes/gptel-chat-mode/specs/gptel/sessions-branching.md,
;; MODIFIED Requirement: Context truncation):
;;   - Copying content up to branch point writes bytes 1..(POS-1).
;;   - Empty branch from first-turn exclude is valid chat-mode.
;;   - Branch preserves org commentary verbatim.
;;   - No gptel--bounds filtering is performed (chat-mode has no such
;;     text properties — the block structure is the authoritative
;;     turn layout).

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-session-branching)

;;; Small helpers.

(defvar jf-branching-test--tempdirs nil
  "List of temporary directories created during tests for cleanup.")

(defun jf-branching-test--make-tempdir ()
  "Create a fresh temporary directory and register it for cleanup."
  (let ((dir (make-temp-file "gptel-branching-truncation-" t)))
    (push dir jf-branching-test--tempdirs)
    dir))

(defun jf-branching-test--file-contents (path)
  "Return the full contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun jf-branching-test--write-file (path content)
  "Write CONTENT to PATH, overwriting any existing file."
  (with-temp-file path (insert content)))

;;; Sample content.

(defconst jf-branching-test--sample
  (concat "#+begin_user\n"         ; 1
          "Q1\n"
          "#+end_user\n"            ; end_user of turn 1
          "\n"
          "#+begin_assistant\n"
          "A1\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Q2\n"
          "#+end_user\n"))

(defconst jf-branching-test--with-prose
  (concat "* Notes\n"
          "\n"
          "Free prose above any turn.\n"
          "\n"
          "#+begin_user\n"
          "First.\n"
          "#+end_user\n"
          "\n"
          "* More notes\n"
          "\n"
          "Commentary between turns.\n"
          "\n"
          "#+begin_assistant\n"
          "Answer.\n"
          "#+end_assistant\n"))

;;; Tests.

(describe "jf/gptel--copy-truncated-context"

  (after-each
    (dolist (dir jf-branching-test--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-branching-test--tempdirs nil))

  (it "writes bytes 1..(POS-1) of the source verbatim"
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir)))
      (jf-branching-test--write-file src jf-branching-test--sample)
      ;; Truncate at a small, known position (10 = after "#+begin_us").
      (jf/gptel--copy-truncated-context src dst 10)
      (expect (jf-branching-test--file-contents dst)
              :to-equal (substring jf-branching-test--sample 0 9))))

  (it "truncates exactly at the branch-point position for a realistic branch point"
    ;; Branch point after first #+end_user: everything through the
    ;; newline that closes the `#+end_user' line.
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir))
           ;; "#+begin_user\nQ1\n#+end_user\n" is 27 chars; position
           ;; immediately after is 28 (1-based).
           (pos (1+ (length "#+begin_user\nQ1\n#+end_user\n"))))
      (jf-branching-test--write-file src jf-branching-test--sample)
      (jf/gptel--copy-truncated-context src dst pos)
      (expect (jf-branching-test--file-contents dst)
              :to-equal "#+begin_user\nQ1\n#+end_user\n")))

  (it "produces an empty file when branch-position is 1"
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir)))
      (jf-branching-test--write-file src jf-branching-test--sample)
      (jf/gptel--copy-truncated-context src dst 1)
      (expect (jf-branching-test--file-contents dst) :to-equal "")))

  (it "copies the full file when branch-position is past the end"
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir))
           ;; Position well past EOF is clamped.
           (pos 1000000))
      (jf-branching-test--write-file src jf-branching-test--sample)
      (jf/gptel--copy-truncated-context src dst pos)
      (expect (jf-branching-test--file-contents dst)
              :to-equal jf-branching-test--sample)))

  (it "preserves org commentary (headings and prose) verbatim"
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir))
           ;; Truncate after the first `#+end_user\n' line so the
           ;; destination retains the leading heading + prose block.
           (marker "#+end_user\n")
           (cut (+ (string-match (regexp-quote marker)
                                 jf-branching-test--with-prose)
                   (length marker)))
           (pos (1+ cut)))
      (jf-branching-test--write-file src jf-branching-test--with-prose)
      (jf/gptel--copy-truncated-context src dst pos)
      (let ((written (jf-branching-test--file-contents dst)))
        ;; Heading and prose are retained verbatim.
        (expect (string-match-p (regexp-quote "* Notes\n") written)
                :to-be-truthy)
        (expect (string-match-p
                 (regexp-quote "Free prose above any turn.\n") written)
                :to-be-truthy)
        ;; The assistant block (after the cut) is excluded.
        (expect (string-match-p (regexp-quote "#+begin_assistant") written)
                :not :to-be-truthy))))

  (it "does not filter, rewrite, or normalize the content (no bounds processing)"
    ;; Write source content that would have been matched by the
    ;; previous text-property / bounds-filtering path if it still ran.
    ;; The new behaviour is a pure byte-copy — every byte before the
    ;; truncation point is preserved exactly.
    (let* ((dir (jf-branching-test--make-tempdir))
           (src (expand-file-name "source.org" dir))
           (dst (expand-file-name "dest.org" dir))
           (content (concat "Comment with <!-- gptel--bounds: ((response (1 10))) -->\n"
                            "#+begin_user\n"
                            "Hello.\n"
                            "#+end_user\n"))
           ;; Truncate at end of source.
           (pos (1+ (length content))))
      (jf-branching-test--write-file src content)
      (jf/gptel--copy-truncated-context src dst pos)
      ;; Byte-for-byte identical — including the bogus comment that
      ;; the old code would have tried to read and rewrite.
      (expect (jf-branching-test--file-contents dst) :to-equal content))))

(provide 'context-truncation-spec)
;;; context-truncation-spec.el ends here
