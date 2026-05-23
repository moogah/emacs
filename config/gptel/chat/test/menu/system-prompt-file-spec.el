;;; system-prompt-file-spec.el --- Sibling system-prompt restore -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Focused unit tests for `gptel-chat--system-prompt-file-path' (the
;; drawer-property resolver) and `gptel-chat--apply-system-prompt-file'
;; (the activation-time installer that reads the sibling
;; `system-prompt.<ext>' file and installs its contents as buffer-local
;; `gptel--system-message').  Per design.md §Decision 1 of
;; `replace-system-prompt-heading-with-sibling-file', the sibling file
;; is the canonical source of truth for the chat-mode system prompt;
;; the installer is a per-session cache populated at activation time
;; and refreshed before every chat request (next task
;; `add-pre-send-refresh').

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel)
(require 'org)

(defvar jf-sysprompt-file-test--tmp-dir nil
  "Temp directory holding the test session.org and sibling file.")

(defun jf-sysprompt-file-test--write-session (drawer)
  "Write session.org under the temp dir with DRAWER as `:PROPERTIES:' body.
Returns the absolute session.org path."
  (let* ((session-dir jf-sysprompt-file-test--tmp-dir)
         (session-file (expand-file-name "session.org" session-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n"
              drawer
              ":END:\n"
              "#+begin_user\n\n#+end_user\n"))
    session-file))

(defun jf-sysprompt-file-test--write-sibling (basename body)
  "Write BODY to <tmp>/BASENAME and return its absolute path."
  (let ((path (expand-file-name basename jf-sysprompt-file-test--tmp-dir)))
    (write-region body nil path nil 'silent)
    path))

(describe "gptel-chat--system-prompt-file-path"
  :var (session-file)

  (before-each
    (setq jf-sysprompt-file-test--tmp-dir
          (make-temp-file "jf-sysprompt-resolve-" t)))

  (after-each
    (when (and jf-sysprompt-file-test--tmp-dir
               (file-directory-p jf-sysprompt-file-test--tmp-dir))
      (delete-directory jf-sysprompt-file-test--tmp-dir t)))

  (it "returns nil when GPTEL_SYSTEM_PROMPT_FILE is unset"
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (expect (gptel-chat--system-prompt-file-path) :to-be nil))
        (kill-buffer buf))))

  (it "resolves a basename relative to session.org directory"
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            ;; macOS resolves /var/folders to /private/var/folders for
            ;; symlinked tmp dirs.  find-file-noselect canonicalises the
            ;; buffer's filename, so compare via `file-truename'.
            (expect (file-truename (gptel-chat--system-prompt-file-path))
                    :to-equal
                    (file-truename
                     (expand-file-name "system-prompt.md"
                                       jf-sysprompt-file-test--tmp-dir))))
        (kill-buffer buf))))

  (it "honors an absolute path verbatim"
    (let* ((abs-path "/tmp/some-absolute-prompt.md")
           (drawer (concat ":GPTEL_PRESET: coding\n"
                           ":GPTEL_SYSTEM_PROMPT_FILE: " abs-path "\n")))
      (setq session-file (jf-sysprompt-file-test--write-session drawer))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (expect (gptel-chat--system-prompt-file-path)
                      :to-equal abs-path))
          (kill-buffer buf))))))

(describe "gptel-chat--apply-system-prompt-file"
  :var (session-file)

  (before-each
    (setq jf-sysprompt-file-test--tmp-dir
          (make-temp-file "jf-sysprompt-apply-" t)))

  (after-each
    (when (and jf-sysprompt-file-test--tmp-dir
               (file-directory-p jf-sysprompt-file-test--tmp-dir))
      (delete-directory jf-sysprompt-file-test--tmp-dir t)))

  (it "installs file body as buffer-local gptel--system-message"
    (jf-sysprompt-file-test--write-sibling "system-prompt.md"
                                           "You are a careful assistant.")
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat--apply-system-prompt-file)
            (expect (local-variable-p 'gptel--system-message) :to-be t)
            (expect gptel--system-message
                    :to-equal "You are a careful assistant."))
        (kill-buffer buf))))

  (it "no-ops when GPTEL_SYSTEM_PROMPT_FILE is unset (preset value stands)"
    (setq session-file
          (jf-sysprompt-file-test--write-session ":GPTEL_PRESET: coding\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local gptel--system-message "preset-installed")
            (gptel-chat--apply-system-prompt-file)
            (expect gptel--system-message :to-equal "preset-installed"))
        (kill-buffer buf))))

  (it "no-ops when the file is missing / unreadable"
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: ghost.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local gptel--system-message "preset-installed")
            (gptel-chat--apply-system-prompt-file)
            (expect gptel--system-message :to-equal "preset-installed"))
        (kill-buffer buf))))

  (it "no-ops when the file is empty (preset value stands)"
    (jf-sysprompt-file-test--write-sibling "system-prompt.md" "")
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local gptel--system-message "preset-installed")
            (gptel-chat--apply-system-prompt-file)
            (expect gptel--system-message :to-equal "preset-installed"))
        (kill-buffer buf))))

  (it "no-ops when the file is whitespace-only (preset value stands)"
    (jf-sysprompt-file-test--write-sibling "system-prompt.md" "   \n\t  \n")
    (setq session-file
          (jf-sysprompt-file-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (setq-local gptel--system-message "preset-installed")
            (gptel-chat--apply-system-prompt-file)
            (expect gptel--system-message :to-equal "preset-installed"))
        (kill-buffer buf))))

  (it "preserves verbatim whitespace and special characters from the file"
    (let ((body (concat "# Markdown heading\n\n"
                        "```bash\n"
                        "echo \"quoted\"\n"
                        "```\n\n"
                        "<tag>xml-like content</tag>\n"
                        "  leading-spaces preserved\n"
                        "trailing-spaces preserved   \n")))
      (jf-sysprompt-file-test--write-sibling "system-prompt.md" body)
      (setq session-file
            (jf-sysprompt-file-test--write-session
             ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat--apply-system-prompt-file)
              (expect gptel--system-message :to-equal body))
          (kill-buffer buf))))))

(provide 'system-prompt-file-spec)

;;; system-prompt-file-spec.el ends here
