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

;;; environment-preamble-spec.el ends here
