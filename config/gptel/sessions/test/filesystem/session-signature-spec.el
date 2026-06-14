;;; session-signature-spec.el --- Buttercup tests for gptel session signature -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Verify the content-based gptel session-recognition primitives:
;;
;; - `jf/gptel--session-signature-p' (buffer predicate): returns non-nil
;;   ONLY for a buffer whose first non-blank content is a genuine
;;   `:PROPERTIES:' drawer carrying at least one `:GPTEL_[A-Z0-9_]+:'
;;   key line before `:END:'. It is anchored to a real drawer at
;;   point-min, NOT a bare substring search, and never signals on a
;;   non-org / plain-text buffer.
;;
;; - `jf/gptel--read-session-drawer-head' (on-disk head-read): runs the
;;   same drawer scan over a file's head in a temp buffer and returns an
;;   alist of the `:GPTEL_*:' keys found, keyed by the bare key string
;;   (e.g. "GPTEL_SESSION_ID").
;;
;; These two functions are the shared recognition primitive for
;; content-addressed activation (magic-mode-alist) and offline discovery
;; (init-registry). The anchoring is load-bearing: the predicate is
;; wired into `magic-mode-alist', so a false match would hijack an
;; ordinary user file into `gptel-chat-mode' at open time.

;;; Code:

(require 'buttercup)
(require 'gptel-session-filesystem)

(defvar jf-gptel-signature-test--tempfiles nil
  "List of temporary files created during tests for cleanup.")

(defun jf-gptel-signature-test--make-tempfile (contents)
  "Write CONTENTS to a fresh temp file, register it, and return its path."
  (let ((path (make-temp-file "gptel-session-signature-test-" nil ".org")))
    (push path jf-gptel-signature-test--tempfiles)
    (with-temp-file path
      (insert contents))
    path))

(defun jf-gptel-signature-test--in-buffer (contents)
  "Insert CONTENTS into the current (temp) buffer and move point to start."
  (insert contents)
  (goto-char (point-min)))

(describe "jf/gptel--session-signature-p (buffer predicate)"

  (it "recognizes a point-min drawer carrying :GPTEL_PRESET:"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n\n* Conversation\n")
      (expect (jf/gptel--session-signature-p) :to-be-truthy)))

  (it "recognizes a drawer carrying only :GPTEL_SESSION_ID: (any GPTEL_ key qualifies)"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       ":PROPERTIES:\n:GPTEL_SESSION_ID: demo-20260101000000\n:END:\n")
      (expect (jf/gptel--session-signature-p) :to-be-truthy)))

  (it "recognizes a drawer after leading blank lines"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       "\n\n   \n:PROPERTIES:\n:GPTEL_BRANCH: main\n:END:\n")
      (expect (jf/gptel--session-signature-p) :to-be-truthy)))

  (it "recognizes a legacy session that only carries :GPTEL_PRESET: (pre-change)"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       ":PROPERTIES:\n:GPTEL_MODEL: gpt-4\n:GPTEL_PRESET: default\n:END:\n\nbody\n")
      (expect (jf/gptel--session-signature-p) :to-be-truthy)))

  (it "rejects an org buffer that merely mentions :GPTEL_PRESET: inside a paragraph"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       "* Notes\n\nThe drawer key :GPTEL_PRESET: lives in a paragraph here.\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "rejects an org buffer that mentions :GPTEL_PRESET: inside a src block"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       "#+begin_src org\n:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n#+end_src\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "rejects a drawer whose first content is not :PROPERTIES:"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       "* Heading\n:PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "rejects a properties drawer that carries no GPTEL_ key"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       ":PROPERTIES:\n:ID: abc-123\n:CATEGORY: notes\n:END:\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "does not match a GPTEL_ key that appears AFTER the drawer :END:"
    (with-temp-buffer
      (jf-gptel-signature-test--in-buffer
       ":PROPERTIES:\n:ID: abc\n:END:\n:GPTEL_PRESET: coding\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "returns nil (no error) for a plain-text / non-org buffer"
    (with-temp-buffer
      (fundamental-mode)
      (jf-gptel-signature-test--in-buffer
       "just some plain text without any drawer at all\n")
      (expect (jf/gptel--session-signature-p) :to-be nil)))

  (it "returns nil (no error) for an empty buffer"
    (with-temp-buffer
      (expect (jf/gptel--session-signature-p) :to-be nil))))

(describe "jf/gptel--read-session-drawer-head (on-disk head-read)"

  (after-each
    (dolist (path jf-gptel-signature-test--tempfiles)
      (when (and path (file-exists-p path))
        (delete-file path)))
    (setq jf-gptel-signature-test--tempfiles nil))

  (it "returns the GPTEL_SESSION_ID / GPTEL_BRANCH / GPTEL_PARENT_SESSION_ID keys"
    (let* ((path (jf-gptel-signature-test--make-tempfile
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_SESSION_ID: demo-20260101000000\n"
                          ":GPTEL_BRANCH: main\n"
                          ":GPTEL_PARENT_SESSION_ID: parent-20251231000000\n"
                          ":END:\n\n* Conversation\n")))
           (result (jf/gptel--read-session-drawer-head path)))
      (expect (cdr (assoc "GPTEL_SESSION_ID" result))
              :to-equal "demo-20260101000000")
      (expect (cdr (assoc "GPTEL_BRANCH" result))
              :to-equal "main")
      (expect (cdr (assoc "GPTEL_PARENT_SESSION_ID" result))
              :to-equal "parent-20251231000000")))

  (it "returns GPTEL_PRESET when present"
    (let* ((path (jf-gptel-signature-test--make-tempfile
                  ":PROPERTIES:\n:GPTEL_PRESET: coding\n:END:\n"))
           (result (jf/gptel--read-session-drawer-head path)))
      (expect (cdr (assoc "GPTEL_PRESET" result)) :to-equal "coding")))

  (it "omits identity keys that are absent (no parent on a top-level session)"
    (let* ((path (jf-gptel-signature-test--make-tempfile
                  (concat ":PROPERTIES:\n"
                          ":GPTEL_SESSION_ID: demo-20260101000000\n"
                          ":GPTEL_BRANCH: main\n"
                          ":END:\n")))
           (result (jf/gptel--read-session-drawer-head path)))
      (expect (cdr (assoc "GPTEL_SESSION_ID" result))
              :to-equal "demo-20260101000000")
      (expect (assoc "GPTEL_PARENT_SESSION_ID" result) :to-be nil)))

  (it "returns nil for a file with no point-min drawer"
    (let* ((path (jf-gptel-signature-test--make-tempfile
                  "* Notes\n\nThe key :GPTEL_PRESET: only in prose.\n"))
           (result (jf/gptel--read-session-drawer-head path)))
      (expect result :to-be nil)))

  (it "returns nil for a nonexistent file (no error)"
    (expect (jf/gptel--read-session-drawer-head
             "/nonexistent/path/does/not/exist.org")
            :to-be nil)))

(provide 'session-signature-spec)
;;; session-signature-spec.el ends here
