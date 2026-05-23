;;; stream-paste-interaction-spec.el --- streaming insert vs paste recorder -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup spec for the composition of two chat-mode write paths in
;; one live buffer: the streaming insert path
;; (`gptel-chat-stream-callback') and the paste / yank path (the
;; mode's `after-change-functions' recorder
;; `gptel-chat--indent-inserted-region').
;;
;; Both are active in a `gptel-chat-mode' buffer.  Streamed lines are
;; already indented by `gptel-chat--sanitize-chunk', so the streaming
;; callback binds `gptel-chat--indenting' to keep the paste recorder
;; from recording streamed inserts as pasted regions to be re-shifted.
;;
;; The streaming suite (`streaming-spec.el') and the paste suite
;; (`paste-indent-spec.el') each test their own producer in
;; isolation; the register invariant `all-write-paths-indent-body' is
;; conjunctive across producers, and this spec is the one that
;; co-activates both producers in a single buffer — the regression
;; guard for the `gptel-chat--indenting' streaming guard.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Add the source dir to `load-path' so `require' below resolves.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-parser)
(require 'gptel-chat-mode)
(require 'gptel-chat-stream)

(describe "streaming insert vs. paste recorder, co-active in one buffer"

  (it "binds `gptel-chat--indenting' so streamed inserts are not recorded"
    ;; In a live `gptel-chat-mode' buffer the after-change paste
    ;; recorder is installed; `gptel-chat-stream-callback' must keep it
    ;; from recording the streamed inserts, which are already indented
    ;; by `gptel-chat--sanitize-chunk'.
    (with-temp-buffer
      ;; Content is inserted BEFORE mode activation so the recorder
      ;; does not see it.  The assistant block is left open for the
      ;; streaming callback to stream into and then close.
      (insert "#+begin_user\nhi\n#+end_user\n#+begin_assistant\n")
      (gptel-chat-mode)
      (let* ((marker (copy-marker (point-max) t))
             (cb (gptel-chat-stream-callback marker)))
        ;; Stream a multi-line chunk — including a heading-shaped line
        ;; — then complete the turn.
        (funcall cb "First line.\n* A heading line\nLast line.\n" nil)
        (funcall cb t nil)
        ;; The streaming guard kept the recorder from recording any
        ;; streamed insert: nothing is queued for a deferred shift...
        (expect gptel-chat--pending-indent-regions :to-be nil)
        ;; ...and no one-shot `post-command-hook' was installed.
        (expect (memq #'gptel-chat--indent-pending-regions
                      post-command-hook)
                :to-be nil)
        ;; The streamed heading line carries exactly one body indent
        ;; (default 2): the per-line indenter ran once and the paste
        ;; recorder did not pile a second shift on top.
        (expect (string-match-p "^  \\* A heading line$" (buffer-string))
                :to-be-truthy)
        (expect (string-match-p "^    \\* A heading line$" (buffer-string))
                :to-be nil)))))

(provide 'gptel-chat-stream-paste-interaction-spec)

;;; stream-paste-interaction-spec.el ends here
