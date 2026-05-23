;;; pre-send-refresh-spec.el --- Pre-send sibling system-prompt refresh -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural tests for `gptel-chat--refresh-system-prompt-from-file'
;; and its `:before' advice on `gptel-request'.  The refresh re-reads
;; the sibling `system-prompt.<ext>' file into buffer-local
;; `gptel--system-message' before every chat-mode request so a
;; mid-session edit on disk is picked up on the next send (design.md
;; §Decision 4 of replace-system-prompt-heading-with-sibling-file —
;; the buffer-local value is a per-request cache, not durable state).
;;
;; The activation-time installer
;; (`gptel-chat--apply-system-prompt-file', covered by
;; `system-prompt-file-spec.el') populates the cache when the buffer
;; opens; this refresh keeps it aligned with the file across the
;; session's lifetime.
;;
;; Test scenarios:
;;
;; 1. Refresh re-reads the sibling file and updates buffer-local
;;    `gptel--system-message'.
;; 2. No-op when the drawer carries no `:GPTEL_SYSTEM_PROMPT_FILE:'
;;    (no cache update).
;; 3. Logs a warning and preserves cache when the property is set but
;;    the file becomes unreadable mid-session.
;; 4. The `:before' advice fires for `gptel-request' calls originating
;;    from a chat-mode buffer.
;; 5. The advice no-ops for `gptel-request' calls originating from a
;;    non-chat-mode buffer (predicate-only filter, zero cost).
;;
;; macOS quirk: `/var/folders/...' temp paths canonicalise to
;; `/private/var/folders/...' under `file-truename'.  `find-file-noselect'
;; canonicalises buffer paths, so any expected-path comparison must
;; pass both sides through `file-truename'.  The drawer-relative
;; resolution path goes through the production resolver, which already
;; uses `expand-file-name' against `buffer-file-name', so comparisons
;; here only need the truename normalisation on equality assertions.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-chat-mode)
(require 'gptel-chat-menu)
(require 'gptel)
(require 'org)

(defvar jf-presend-test--tmp-dir nil
  "Temp directory holding the test session.org and sibling file.")

(defun jf-presend-test--write-session (drawer)
  "Write session.org with DRAWER as `:PROPERTIES:' body; return absolute path."
  (let* ((session-dir jf-presend-test--tmp-dir)
         (session-file (expand-file-name "session.org" session-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n"
              drawer
              ":END:\n"
              "#+begin_user\n\n#+end_user\n"))
    session-file))

(defun jf-presend-test--write-sibling (basename body)
  "Write BODY to <tmp>/BASENAME; return its absolute path."
  (let ((path (expand-file-name basename jf-presend-test--tmp-dir)))
    (write-region body nil path nil 'silent)
    path))

(describe "gptel-chat--refresh-system-prompt-from-file"

  :var (session-file)

  (before-each
    (setq jf-presend-test--tmp-dir
          (make-temp-file "jf-presend-refresh-" t)))

  (after-each
    (when (and jf-presend-test--tmp-dir
               (file-directory-p jf-presend-test--tmp-dir))
      (delete-directory jf-presend-test--tmp-dir t)))

  (it "re-reads sibling file before dispatch and updates buffer-local cache"
    (jf-presend-test--write-sibling "system-prompt.md" "Old prompt body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            ;; Force chat-mode so the function body's
            ;; `derived-mode-p' guard passes; the activation-time
            ;; installer would have done this in a real session, but
            ;; `find-file-noselect' on a temp file does not trigger
            ;; the session auto-init pipeline.
            (gptel-chat-mode)
            ;; Seed the cache as if the activation-time installer ran
            ;; and read the file body.
            (setq-local gptel--system-message "Old prompt body.\n")
            ;; A user edits the sibling file on disk; no buffer revert.
            (jf-presend-test--write-sibling
             "system-prompt.md" "New prompt body.\n")
            ;; Invoke the refresh directly (the advice path is
            ;; exercised by the `pre-send wiring' describe below).
            (gptel-chat--refresh-system-prompt-from-file)
            (expect gptel--system-message :to-equal "New prompt body.\n"))
        (kill-buffer buf))))

  (it "no-ops when GPTEL_SYSTEM_PROMPT_FILE is unset"
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            ;; Stamp a known buffer-local value to confirm the
            ;; refresh does not touch it.
            (setq-local gptel--system-message "sentinel-untouched")
            (gptel-chat--refresh-system-prompt-from-file)
            (expect gptel--system-message :to-equal "sentinel-untouched"))
        (kill-buffer buf))))

  (it "logs a warning and preserves cache when sibling file becomes unreadable"
    (jf-presend-test--write-sibling "system-prompt.md" "Live prompt body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel--system-message "Live prompt body.\n")
            ;; Delete the sibling file to simulate an unreadable state.
            (let* ((sibling (gptel-chat--system-prompt-file-path)))
              (when (and sibling (file-exists-p sibling))
                (delete-file sibling))
              (spy-on 'jf/gptel--log)
              (gptel-chat--refresh-system-prompt-from-file)
              ;; Cache preserved.
              (expect gptel--system-message :to-equal "Live prompt body.\n")
              ;; Warning logged with the resolver's path.
              (expect 'jf/gptel--log :to-have-been-called)
              (let ((args (spy-calls-args-for 'jf/gptel--log 0)))
                (expect (nth 0 args) :to-equal 'warn)
                (expect (nth 1 args)
                        :to-equal "system-prompt sibling file unreadable: %s")
                (expect (file-truename (nth 2 args))
                        :to-equal (file-truename sibling)))))
        (kill-buffer buf))))

  (it "leaves cache untouched when sibling file is present but blank"
    (jf-presend-test--write-sibling "system-prompt.md" "Live prompt body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel--system-message "Live prompt body.\n")
            (jf-presend-test--write-sibling "system-prompt.md" "\n   \n")
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Blank body should not replace the live cache.
            (expect gptel--system-message :to-equal "Live prompt body.\n"))
        (kill-buffer buf)))))

(describe "pre-send wiring: :before advice on gptel-request"

  :var (session-file)

  (before-each
    (setq jf-presend-test--tmp-dir
          (make-temp-file "jf-presend-wiring-" t)))

  (after-each
    (when (and jf-presend-test--tmp-dir
               (file-directory-p jf-presend-test--tmp-dir))
      (delete-directory jf-presend-test--tmp-dir t)))

  (it "refresh fires when gptel-request is invoked from a chat-mode buffer"
    (jf-presend-test--write-sibling "system-prompt.md" "Body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (spy-on 'gptel-chat--refresh-system-prompt-from-file
                    :and-call-through)
            ;; Stub gptel-request to avoid network / FSM machinery; the
            ;; advice should still fire because it is :before-advice.
            (cl-letf (((symbol-function 'gptel-request)
                       (lambda (&rest _ignored) nil)))
              ;; Re-install the advice on the stub so the :before
              ;; chain executes when we invoke gptel-request.
              (advice-add 'gptel-request :before
                          #'gptel-chat--refresh-system-prompt-from-file)
              (unwind-protect
                  (progn
                    (gptel-request "hello")
                    (expect 'gptel-chat--refresh-system-prompt-from-file
                            :to-have-been-called))
                (advice-remove 'gptel-request
                               #'gptel-chat--refresh-system-prompt-from-file))))
        (kill-buffer buf))))

  (it "refresh does not update the cache for gptel-request from a non-chat-mode buffer"
    (jf-presend-test--write-sibling "system-prompt.md" "Body.\n")
    ;; Plain buffer, no chat-mode.  The advice will fire but the
    ;; function body's `derived-mode-p' guard makes it a no-op.
    (with-temp-buffer
      (text-mode)
      (set (make-local-variable 'gptel--system-message) "sentinel-plain-buf")
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _ignored) nil)))
        (advice-add 'gptel-request :before
                    #'gptel-chat--refresh-system-prompt-from-file)
        (unwind-protect
            (progn
              (gptel-request "hello")
              ;; Cache preserved because the predicate failed.
              (expect gptel--system-message :to-equal "sentinel-plain-buf"))
          (advice-remove 'gptel-request
                         #'gptel-chat--refresh-system-prompt-from-file))))))

(provide 'pre-send-refresh-spec)
;;; pre-send-refresh-spec.el ends here
