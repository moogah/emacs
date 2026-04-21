;;; regenerate-spec.el --- Buttercup tests for gptel-chat-regenerate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Buttercup specs for `gptel-chat-regenerate'.
;;
;; Coverage (from openspec/changes/gptel-chat-mode/specs/gptel-chat-mode/spec.md
;; §"Regenerate last response"):
;;   - Regenerate after a completed response deletes the trailing assistant
;;     block and re-issues the request (via `gptel-chat-send', spied on).
;;   - Regenerate when only a user block exists emits the contracted
;;     "No response to regenerate" message and leaves the buffer unchanged.
;;   - Regenerate on an empty buffer emits the same message.
;;
;; `gptel-chat-send' is owned by a sibling task (`send-command') and may
;; not be defined when this suite runs.  The tests use `spy-on' with
;; `:and-return-value', which both installs a stub and records calls —
;; so the spec validates the contract without importing `send.el'.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

;; Load the module under test from the co-located source directory.
;; `file-name-directory' of this spec is .../config/gptel/chat/test/nav/;
;; two levels up is .../config/gptel/chat/, which holds `nav.el'.
(let* ((spec-dir (file-name-directory (or load-file-name buffer-file-name)))
       (chat-dir (expand-file-name "../../" spec-dir)))
  (add-to-list 'load-path chat-dir))

(require 'gptel-chat-parser)
(require 'gptel-chat-nav)

;; `gptel-chat-send' is owned by a later task.  Provide a batch-safe
;; default binding so `spy-on' has a symbol to replace.
(unless (fboundp 'gptel-chat-send)
  (defun gptel-chat-send ()
    "Stub for batch-context testing; replaced by `spy-on' in each spec."
    nil))


;;; Fixtures -----------------------------------------------------------------

(defconst gptel-chat-regenerate-test--completed
  (concat "#+begin_user\n"
          "What's the capital of France?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Paris.\n"
          "#+end_assistant\n")
  "Buffer ending in a completed assistant response.")

(defconst gptel-chat-regenerate-test--only-user
  (concat "#+begin_user\n"
          "What's the capital of France?\n"
          "#+end_user\n")
  "Buffer whose last (and only) turn is an unanswered user block.")

(defmacro gptel-chat-regenerate-test--with-buffer (content &rest body)
  "Insert CONTENT in a fresh temp buffer and evaluate BODY there.
Point starts at `point-min'."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))


;;; Tests --------------------------------------------------------------------

(describe "gptel-chat-regenerate"

  (describe "when the last turn is a completed assistant block"
    (it "deletes the trailing assistant block and calls gptel-chat-send"
      (spy-on 'gptel-chat-send :and-return-value nil)
      (gptel-chat-regenerate-test--with-buffer
          gptel-chat-regenerate-test--completed
        (gptel-chat-regenerate)
        ;; The assistant block (and its closing delimiter) must be gone.
        (expect (buffer-string) :not :to-match "#\\+begin_assistant")
        (expect (buffer-string) :not :to-match "#\\+end_assistant")
        (expect (buffer-string) :not :to-match "Paris\\.")
        ;; The preceding user block must still be intact.
        (expect (buffer-string) :to-match "#\\+begin_user")
        (expect (buffer-string) :to-match "capital of France")
        ;; And send must have been re-issued.
        (expect 'gptel-chat-send :to-have-been-called))))

  (describe "when the last turn is an unanswered user block"
    (it "emits \"No response to regenerate\" and leaves the buffer unchanged"
      (spy-on 'gptel-chat-send :and-return-value nil)
      (gptel-chat-regenerate-test--with-buffer
          gptel-chat-regenerate-test--only-user
        (let* ((before (buffer-string))
               (msg nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq msg (apply #'format fmt args)))))
            (gptel-chat-regenerate))
          (expect (buffer-string) :to-equal before)
          (expect msg :to-equal "No response to regenerate")
          (expect 'gptel-chat-send :not :to-have-been-called)))))

  (describe "when the buffer has no turns"
    (it "emits \"No response to regenerate\" and leaves the buffer unchanged"
      (spy-on 'gptel-chat-send :and-return-value nil)
      (gptel-chat-regenerate-test--with-buffer
          "* Heading\n\nJust prose, no turn blocks here.\n"
        (let* ((before (buffer-string))
               (msg nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq msg (apply #'format fmt args)))))
            (gptel-chat-regenerate))
          (expect (buffer-string) :to-equal before)
          (expect msg :to-equal "No response to regenerate")
          (expect 'gptel-chat-send :not :to-have-been-called))))))


(provide 'gptel-chat-regenerate-spec)

;;; regenerate-spec.el ends here
