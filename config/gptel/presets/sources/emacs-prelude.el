;;; emacs-prelude.el --- Static chat-prelude fragment -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: gptel, llm, presets

;;; Commentary:

;; The canonical static leading fragment for the chat context: the
;; runtime-framing prelude prepended to every chat-mode system prompt.  This
;; file is tangled from emacs-prelude.org -- edit the .org, never this .el.
;; On load it renders its static fragment plist once for the `claude' backend
;; (a load-time pre-render, never per-send) and populates the composer's chat
;; lead seam `jf/gptel-fragment-chat-prelude-text'.  The rendered text is also
;; mirrored to the committed sibling `emacs-prelude.txt' artifact for diffing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(let ((presets-dir
       (file-name-directory
        (directory-file-name
         (file-name-directory (or load-file-name buffer-file-name))))))
  (require 'jf-gptel-fragments
           (expand-file-name "fragments.el" presets-dir)))

(defconst jf/gptel-fragment-emacs-prelude--fragment
  '(:kind static
    :sections
    (("Runtime" .
      "You are operating inside GNU Emacs, through the gptel chat interface. Keep these in mind:\n- Your responses render as Org-mode text in an Emacs buffer — prefer Org markup over Markdown.\n- You have tools that read and write files and inspect the user's editor, within the session's file-access scope.\n- The user works in Emacs; assume familiarity with it.")))
  "Static runtime-framing prelude fragment for the chat context.
A fragment plist (:kind static :sections ((NAME . BODY) ...)) — the shape
`jf/gptel-fragment--parse-source' produces — rendered once at load time by
`jf/gptel-fragment-render' and wired into the composer's chat lead seam.")

(defconst jf/gptel-fragment-emacs-prelude--text
  (jf/gptel-fragment-render
   jf/gptel-fragment-emacs-prelude--fragment
   'claude)
  "Pre-rendered Claude text of the static chat prelude fragment.")

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (artifact (expand-file-name "emacs-prelude.txt" this-dir)))
  (ignore-errors
    (unless (and (file-readable-p artifact)
                 (string= (with-temp-buffer
                            (insert-file-contents artifact)
                            (buffer-string))
                          jf/gptel-fragment-emacs-prelude--text))
      (write-region jf/gptel-fragment-emacs-prelude--text nil artifact nil 'silent))))

(setq jf/gptel-fragment-chat-prelude-text jf/gptel-fragment-emacs-prelude--text)

(provide 'jf-gptel-fragment-emacs-prelude)
;;; emacs-prelude.el ends here
