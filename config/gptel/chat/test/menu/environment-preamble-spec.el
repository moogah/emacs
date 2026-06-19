;;; environment-preamble-spec.el --- Dynamic environment preamble specs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural / unit tests for the dynamic environment preamble that
;; gptel-chat-mode appends to `gptel--system-message' on every send.
;;
;; This file is shared across three tasks of the
;; `gptel-dynamic-environment-preamble' change:
;;
;;   * `gptel-chat--build-environment-block' (this task,
;;     env-block-builder) — builds the "# Environment" markdown block
;;     from `default-directory' + the point-min drawer's raw
;;     `GPTEL_SCOPE_*' keys, rendered as verbatim globs (design.md D3),
;;     degrading gracefully when no scope keys exist (design.md D5).
;;   * role-base composition (a sibling task) — appends its
;;     describe-block below.
;;   * pre-send composition (a sibling task) — appends its
;;     describe-block below.
;;
;; The builder describe-block below is the ONLY block this task owns.
;; Other tasks append their describe-blocks to this same file.
;;
;; Test approach: real buffer state, no mocking.  Each scenario inserts
;; a `:PROPERTIES:' drawer, enables `org-mode' (so
;; `org-entry-get-multivalued-property' reads the scope keys), and sets
;; `default-directory' buffer-locally.  Scope is asserted VERBATIM —
;; the block must echo the authored globs, not a summary.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-chat-menu)
(require 'gptel-session-filesystem)
(require 'org)

(defun jf-envblk-test--with-drawer-buffer (drawer dir fn)
  "Call FN in a temp `org-mode' buffer holding DRAWER, with DIR as `default-directory'.
DRAWER is the body between `:PROPERTIES:' / `:END:' (each key on its
own line, with a trailing newline); it may be the empty string for a
no-scope buffer.  DIR is bound buffer-locally as `default-directory'."
  (with-temp-buffer
    (insert ":PROPERTIES:\n"
            drawer
            ":END:\n"
            "#+begin_user\n\n#+end_user\n")
    (org-mode)
    (setq-local default-directory dir)
    (goto-char (point-min))
    (funcall fn)))

(describe "gptel-chat--build-environment-block"

  (it "reports the work root from default-directory"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/my-work-root/"
     (lambda ()
       (let ((block (gptel-chat--build-environment-block)))
         (expect block :to-match
                 "- Working directory: /tmp/my-work-root/")))))

  (it "renders read/write/deny as verbatim globs from the drawer keys"
    (jf-envblk-test--with-drawer-buffer
     (concat ":GPTEL_SCOPE_READ: src/** docs/**\n"
             ":GPTEL_SCOPE_WRITE: build/**\n"
             ":GPTEL_SCOPE_DENY: secrets/**\n")
     "/tmp/work/"
     (lambda ()
       (let ((block (gptel-chat--build-environment-block)))
         ;; Verbatim globs, comma-joined, not summarised.
         (expect block :to-match "- Readable: src/\\*\\*, docs/\\*\\*")
         (expect block :to-match "- Writable: build/\\*\\*")
         (expect block :to-match "- Denied:  *secrets/\\*\\*")))))

  (it "includes the live-note sentence"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/work/"
     (lambda ()
       (expect (gptel-chat--build-environment-block)
               :to-match "current as of this message"))))

  (it "renders the Environment heading"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/work/"
     (lambda ()
       (expect (gptel-chat--build-environment-block)
               :to-match "# Environment"))))

  (it "switches to scoped form when only GPTEL_SCOPE_DENY is present"
    ;; The presence of ANY one of the three rendered scope keys is the
    ;; scoped-form gate, not all three.
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_DENY: /etc/**\n"
     "/tmp/work/"
     (lambda ()
       (let ((block (gptel-chat--build-environment-block)))
         (expect block :to-match "- Denied:  */etc/\\*\\*")
         (expect block :not :to-match "no scope restrictions")))))

  (describe "with no GPTEL_SCOPE_* keys (degradation, design.md D5)"

    (it "reports default-directory and a no-scope-restrictions line"
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_PRESET: coding\n"
       "/tmp/unscoped/"
       (lambda ()
         (let ((block (gptel-chat--build-environment-block)))
           (expect block :to-match "- Working directory: /tmp/unscoped/")
           (expect block :to-match
                   "no scope restrictions (this buffer is not a scoped session)")))))

    (it "does not render the Readable/Writable/Denied bullets"
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_PRESET: coding\n"
       "/tmp/unscoped/"
       (lambda ()
         (let ((block (gptel-chat--build-environment-block)))
           (expect block :not :to-match "- Readable:")
           (expect block :not :to-match "- Writable:")
           (expect block :not :to-match "- Denied:")))))

    (it "does not error and returns a non-empty string"
      (jf-envblk-test--with-drawer-buffer
       ""
       "/tmp/bare/"
       (lambda ()
         (let ((block (gptel-chat--build-environment-block)))
           (expect (stringp block) :to-be t)
           (expect (length block) :to-be-greater-than 0))))))

  (describe "input neutrality (register/boundary/environment-block-input-neutrality)"

    (it "builds from a plain temp buffer with no workspaces context (no workspaces symbol referenced)"
      ;; The builder must touch no workspaces-package symbol — its
      ;; inputs are INTRINSIC (default-directory + drawer keys) only.
      ;; A temp buffer is not associated with any workspace, so a
      ;; correct builder produces a valid block here regardless of
      ;; whether the workspaces package happens to be loaded in the
      ;; test image.  The complementary static guarantee — that the
      ;; source references no `workspace' symbol — is enforced by the
      ;; task's grep gate over menu.org.
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_SCOPE_READ: src/**\n"
       "/tmp/work/"
       (lambda ()
         (let ((block (gptel-chat--build-environment-block)))
           (expect (stringp block) :to-be t)
           (expect block :to-match "- Working directory: /tmp/work/")))))))

;; ---------------------------------------------------------------------------
;; role base (task: stable-role-base)
;;
;; `gptel-chat--system-prompt-base' is a buffer-local that holds the
;; current ROLE content (the system-prompt body) WITHOUT any environment
;; block, set at every site that installs role content into
;; `gptel--system-message'.  The pre-send composer (sibling task) reads
;; this base — never the composed `gptel--system-message' — for the
;; no-sibling-file case, so the environment block never accumulates
;; across sends (register/invariant/composed-system-message-write-only).
;; ---------------------------------------------------------------------------

(require 'gptel-chat-mode)
(require 'gptel)

(defvar jf-rolebase-test--tmp-dir nil
  "Temp directory holding the test session.org and sibling file.")

(defun jf-rolebase-test--write-session (drawer)
  "Write session.org with DRAWER as `:PROPERTIES:' body; return absolute path."
  (let ((session-file (expand-file-name "session.org" jf-rolebase-test--tmp-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n"
              drawer
              ":END:\n"
              "#+begin_user\n\n#+end_user\n"))
    session-file))

(defun jf-rolebase-test--write-sibling (basename body)
  "Write BODY to <tmp>/BASENAME; return its absolute path."
  (let ((path (expand-file-name basename jf-rolebase-test--tmp-dir)))
    (write-region body nil path nil 'silent)
    path))

(describe "role base"

  :var (session-file)

  (before-each
    (setq jf-rolebase-test--tmp-dir
          (make-temp-file "jf-rolebase-" t)))

  (after-each
    (when (and jf-rolebase-test--tmp-dir
               (file-directory-p jf-rolebase-test--tmp-dir))
      (delete-directory jf-rolebase-test--tmp-dir t)))

  (describe "set from the legacy :GPTEL_SYSTEM: drawer overlay"

    (it "captures the installed role body into the base"
      (setq session-file
            (jf-rolebase-test--write-session
             ":GPTEL_SYSTEM: You are a careful assistant.\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              (gptel-chat--apply-drawer-overrides)
              (expect (local-variable-p 'gptel-chat--system-prompt-base)
                      :to-be t)
              ;; Base equals the installed role body, identical to
              ;; `gptel--system-message'.
              (expect gptel-chat--system-prompt-base
                      :to-equal "You are a careful assistant.")
              (expect gptel-chat--system-prompt-base
                      :to-equal gptel--system-message))
          (kill-buffer buf))))

    (it "holds role-only content (no environment block)"
      (setq session-file
            (jf-rolebase-test--write-session
             ":GPTEL_SYSTEM: You are a careful assistant.\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              (gptel-chat--apply-drawer-overrides)
              ;; Guard against a regression where the base is set from
              ;; the composed `role + env-block' value.
              (expect gptel-chat--system-prompt-base
                      :not :to-match "# Environment")
              (expect gptel-chat--system-prompt-base
                      :not :to-match "current as of this message"))
          (kill-buffer buf)))))

  (describe "set from the sibling system-prompt file installer"

    (it "captures the sibling-file body into the base"
      (jf-rolebase-test--write-sibling "system-prompt.md"
                                       "You are a meticulous reviewer.")
      (setq session-file
            (jf-rolebase-test--write-session
             ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              (gptel-chat--apply-system-prompt-file)
              (expect (local-variable-p 'gptel-chat--system-prompt-base)
                      :to-be t)
              (expect gptel-chat--system-prompt-base
                      :to-equal "You are a meticulous reviewer.")
              (expect gptel-chat--system-prompt-base
                      :to-equal gptel--system-message))
          (kill-buffer buf))))

    (it "holds role-only content (no environment block)"
      (jf-rolebase-test--write-sibling "system-prompt.md"
                                       "You are a meticulous reviewer.")
      (setq session-file
            (jf-rolebase-test--write-session
             ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              (gptel-chat--apply-system-prompt-file)
              (expect gptel-chat--system-prompt-base
                      :not :to-match "# Environment")
              (expect gptel-chat--system-prompt-base
                      :not :to-match "current as of this message"))
          (kill-buffer buf)))))

  (describe "set from the pre-send sibling refresh"

    (it "keeps the base aligned with the re-read sibling body, role-only"
      (jf-rolebase-test--write-sibling "system-prompt.md" "Old body.\n")
      (setq session-file
            (jf-rolebase-test--write-session
             ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"))
      (let ((buf (find-file-noselect session-file)))
        (unwind-protect
            (with-current-buffer buf
              (gptel-chat-mode)
              ;; A user edits the sibling on disk; refresh re-reads it.
              (jf-rolebase-test--write-sibling "system-prompt.md" "New body.\n")
              (gptel-chat--refresh-system-prompt-from-file)
              (expect gptel-chat--system-prompt-base :to-equal "New body.\n")
              (expect gptel-chat--system-prompt-base
                      :not :to-match "# Environment"))
          (kill-buffer buf))))))

;;; environment-preamble-spec.el ends here
