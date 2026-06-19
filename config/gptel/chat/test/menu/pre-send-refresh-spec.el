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

  ;; NOTE: `gptel-chat--refresh-system-prompt-from-file' was widened from
  ;; a sibling-file cache refresh into the per-send COMPOSER (task
  ;; pre-send-compose-and-wire): it now recomposes `gptel--system-message'
  ;; WHOLESALE as `role + "\n\n" + environment-block', with the env block
  ;; as the unconditional tail.  These specs therefore assert the ROLE
  ;; portion (sibling re-read / cache preservation / blank-body fallback)
  ;; via the role base and the role prefix of the composed value, rather
  ;; than `gptel--system-message' equalling the bare role.  The composition
  ;; mechanics (tail order, idempotency, scope reflection) are covered in
  ;; `environment-preamble-spec.el's "pre-send composition" block.

  (it "re-reads sibling file before dispatch and updates the role base"
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
            ;; content-addressed session activation.
            (gptel-chat-mode)
            ;; Seed the cache as if the activation-time installer ran
            ;; and read the file body.
            (setq-local gptel--system-message "Old prompt body.\n")
            (setq-local gptel-chat--system-prompt-base "Old prompt body.\n")
            ;; A user edits the sibling file on disk; no buffer revert.
            (jf-presend-test--write-sibling
             "system-prompt.md" "New prompt body.\n")
            ;; Invoke the composer directly (the advice path is
            ;; exercised by the `pre-send wiring' describe below).
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Role base tracks the re-read sibling body (role-only).
            (expect gptel-chat--system-prompt-base :to-equal "New prompt body.\n")
            ;; The composed message leads with the fresh role.
            (expect gptel--system-message :to-match "\\`New prompt body\\.")
            (expect gptel--system-message :not :to-match "Old prompt body"))
        (kill-buffer buf))))

  (it "composes from the role base when GPTEL_SYSTEM_PROMPT_FILE is unset"
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            ;; With no sibling property the composer uses the role base.
            (setq-local gptel-chat--system-prompt-base "sentinel-role")
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Role base unchanged (no sibling to re-read), composed value
            ;; leads with it and appends the env block.
            (expect gptel-chat--system-prompt-base :to-equal "sentinel-role")
            (expect gptel--system-message :to-match "\\`sentinel-role")
            (expect gptel--system-message :to-match "# Environment"))
        (kill-buffer buf))))

  (it "logs a warning and preserves the role base when sibling file becomes unreadable"
    (jf-presend-test--write-sibling "system-prompt.md" "Live prompt body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel--system-message "Live prompt body.\n")
            (setq-local gptel-chat--system-prompt-base "Live prompt body.\n")
            ;; Delete the sibling file to simulate an unreadable state.
            (let* ((sibling (gptel-chat--system-prompt-file-path)))
              (when (and sibling (file-exists-p sibling))
                (delete-file sibling))
              (spy-on 'jf/gptel--log)
              (gptel-chat--refresh-system-prompt-from-file)
              ;; Role base preserved (fallback), composed value leads with it.
              (expect gptel-chat--system-prompt-base :to-equal "Live prompt body.\n")
              (expect gptel--system-message :to-match "\\`Live prompt body\\.")
              ;; Warning logged with the resolver's path.
              (expect 'jf/gptel--log :to-have-been-called)
              (let ((args (spy-calls-args-for 'jf/gptel--log 0)))
                (expect (nth 0 args) :to-equal 'warn)
                (expect (nth 1 args)
                        :to-equal "system-prompt sibling file unreadable: %s")
                (expect (file-truename (nth 2 args))
                        :to-equal (file-truename sibling)))))
        (kill-buffer buf))))

  (it "leaves the role base untouched when sibling file is present but blank"
    (jf-presend-test--write-sibling "system-prompt.md" "Live prompt body.\n")
    (setq session-file
          (jf-presend-test--write-session
           ":GPTEL_PRESET: coding\n:GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel--system-message "Live prompt body.\n")
            (setq-local gptel-chat--system-prompt-base "Live prompt body.\n")
            (jf-presend-test--write-sibling "system-prompt.md" "\n   \n")
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Blank body should not replace the live role base.
            (expect gptel-chat--system-prompt-base :to-equal "Live prompt body.\n")
            (expect gptel--system-message :to-match "\\`Live prompt body\\."))
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
                         #'gptel-chat--refresh-system-prompt-from-file)))))

  (it "is registered as :before advice on gptel-request at module load"
    ;; Regression guard: the wiring tests above use a `cl-letf'-stubbed
    ;; `gptel-request' and `advice-add' the refresh themselves, so they
    ;; would still pass if the production module-load `advice-add' at
    ;; the bottom of `config/gptel/chat/menu.org's System-Prompt-sibling-
    ;; file section were removed or mistyped.  This spec asserts the
    ;; production advice install is actually present on the real
    ;; `gptel-request' symbol, independent of any test scaffolding.
    (let ((found nil))
      (advice-mapc
       (lambda (fn _props)
         (when (eq fn 'gptel-chat--refresh-system-prompt-from-file)
           (setq found t)))
       'gptel-request)
      (expect found :to-be t))))

(provide 'pre-send-refresh-spec)
;;; pre-send-refresh-spec.el ends here
