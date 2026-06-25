;;; agent-preamble.el --- Static agent-preamble fragment -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: gptel, llm, presets

;;; Commentary:

;; The canonical static leading fragment for the agent context: the baseline
;; harness preamble prepended to every persistent agent's system prompt.  This
;; file is tangled from agent-preamble.org -- edit the .org, never this .el.
;; On load it renders its static fragment plist once for the `claude' backend
;; (a load-time pre-render, never per-send), populates the composer's agent
;; lead seam `jf/gptel-fragment-agent-preamble-text', and exposes the rendered
;; text via `jf/gptel-fragment-agent-preamble--text' for the persistent-agent
;; system-prompt writer.  The rendered text is mirrored to the committed
;; sibling `agent-preamble.txt' artifact for diffing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(let ((presets-dir
       (file-name-directory
        (directory-file-name
         (file-name-directory (or load-file-name buffer-file-name))))))
  (require 'jf-gptel-fragments
           (expand-file-name "fragments.el" presets-dir)))

(defconst jf/gptel-fragment-agent-preamble--fragment
  '(:kind static
    :sections
    (("Role" .
      "You are an autonomous sub-agent launched by a parent agent to carry out one task.")
     ("Operating Rules" .
      "- Do the task YOURSELF, directly, using the tools available to you. Do NOT delegate it to another agent — never call the PersistentAgent tool to perform your work. You are the agent that does the work.\n- You run headless and cannot ask the user follow-up questions. When something is ambiguous, make a reasonable assumption, state it, and proceed.\n- Stay within the task you were given and the file scope you were granted. If an operation is refused as out of scope and the task genuinely needs it, request a scope expansion rather than abandoning the task.\n- When the task is complete, STOP and write a single final message that fully answers it (results, file paths, and anything the parent needs). That final message is the ONLY thing returned to the parent, so make it self-contained — do not rely on intermediate tool output being visible upstream.")))
  "Static agent harness preamble fragment for the agent context.
A fragment plist (:kind static :sections ((NAME . BODY) ...)) — the shape
`jf/gptel-fragment--parse-source' produces — rendered once at load time by
`jf/gptel-fragment-render' and wired into the composer's agent lead seam.")

(defconst jf/gptel-fragment-agent-preamble--text
  (jf/gptel-fragment-render
   jf/gptel-fragment-agent-preamble--fragment
   'claude)
  "Pre-rendered Claude text of the static agent preamble fragment.
Consumed verbatim by the composer's agent lead seam and by
`jf/gptel-persistent-agent--write-system-prompt' (the agent-creation-time
sibling-file writer).")

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (artifact (expand-file-name "agent-preamble.txt" this-dir)))
  (ignore-errors
    (unless (and (file-readable-p artifact)
                 (string= (with-temp-buffer
                            (insert-file-contents artifact)
                            (buffer-string))
                          jf/gptel-fragment-agent-preamble--text))
      (write-region jf/gptel-fragment-agent-preamble--text nil artifact nil 'silent))))

(setq jf/gptel-fragment-agent-preamble-text jf/gptel-fragment-agent-preamble--text)

(provide 'jf-gptel-fragment-agent-preamble)
;;; agent-preamble.el ends here
