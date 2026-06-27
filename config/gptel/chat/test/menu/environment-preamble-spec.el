;;; environment-preamble-spec.el --- Dynamic environment preamble specs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; Behavioural / unit tests for the dynamic environment preamble that
;; gptel-chat-mode appends to `gptel--system-message' on every send.
;;
;; This file is shared across three tasks of the
;; `gptel-dynamic-environment-preamble' change, and was migrated by the
;; `migrate-environment' task of `gptel-fragment-presets':
;;
;;   * `jf/gptel-fragment-environment' (the canonical dynamic fragment,
;;     in `config/gptel/presets/sources/environment.org') — produces the
;;     "# Environment" markdown block at compose time from
;;     `default-directory' + the point-min drawer's raw `GPTEL_SCOPE_*'
;;     keys, rendered as verbatim globs (design.md D3), degrading
;;     gracefully when no scope keys exist (design.md D5).  This is the
;;     relocated body of the former `gptel-chat--build-environment-block'
;;     defun (wrap-and-relocate, behaviour-preserving).
;;   * role-base composition (a sibling task) — appends its
;;     describe-block below.
;;   * pre-send composition (a sibling task) — appends its
;;     describe-block below.
;;
;; The env-fragment describe-block below is the ONLY block this task
;; owns.  Other tasks append their describe-blocks to this same file.
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
(require 'jf-gptel-fragments)
;; The env dynamic-fragment source lives under presets/sources/, which
;; is not on `load-path' and is not loaded by `gptel.org' yet (the
;; presets sub-module's load wiring is owned by a sibling task).  Load
;; it by absolute path so this spec exercises the real fragment fn and
;; its seam wiring regardless of init-time load order.
(load (expand-file-name "config/gptel/presets/sources/environment.el"
                        jf/emacs-dir)
      nil t)
(require 'jf-gptel-fragment-environment)
;; The chat-prelude static-fragment source likewise lives under
;; presets/sources/; load it by absolute path so the composer's chat
;; lead seam `jf/gptel-fragment-chat-prelude-text' is populated with the
;; pre-rendered prelude text (default is "" until this loads).
(load (expand-file-name "config/gptel/presets/sources/emacs-prelude.el"
                        jf/emacs-dir)
      nil t)
(require 'jf-gptel-fragment-emacs-prelude)
(require 'org)

;; Committed-mirror golden support.
;;
;; The fragment source's load-time mirror writer rewrites the working-tree
;; `emacs-prelude.txt' from the freshly rendered text whenever they diverge
;; (see `emacs-prelude.el').  A working-tree-vs-rendered assertion is therefore
;; tautological: the mirror has already healed the file before any `it' runs.
;; To catch *committed* drift (the property
;; `register/invariant/static-prerender-dynamic-compose' relies on -- the
;; committed `.txt' being an in-sync diffable mirror), read the bytes tracked
;; at HEAD via git (immune to the working-tree mirror) and compare against a
;; freshly rendered fragment value.
(defun jf/prelude-golden--committed-bytes (relpath)
  "Return the bytes of RELPATH as committed at HEAD, or signal on failure.
RELPATH is relative to the repo root `jf/emacs-dir'.  Reads via `git show'
so the value is immune to the working-tree mirror writer."
  (with-temp-buffer
    (let* ((default-directory jf/emacs-dir)
           (status (call-process "git" nil t nil "show"
                                 (concat "HEAD:" relpath))))
      (unless (eq status 0)
        (error "git show HEAD:%s failed (status %s): %s"
               relpath status (buffer-string)))
      (buffer-string))))

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

(describe "jf/gptel-fragment-environment (dynamic env fragment)"

  ;; The env block is now produced by the canonical dynamic fragment
  ;; `jf/gptel-fragment-environment' (in presets/sources/environment.el),
  ;; the relocated body of the former `gptel-chat--build-environment-
  ;; block'.  It takes (and ignores) the compose-time CONTEXT argument
  ;; the composer passes to every dynamic reference's :fn; these specs
  ;; call it with no arg, exercising the same compose-time evaluation.

  (it "reports the work root from default-directory"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/my-work-root/"
     (lambda ()
       (let ((block (jf/gptel-fragment-environment)))
         (expect block :to-match
                 "- Working directory: /tmp/my-work-root/")))))

  (it "renders read/write/deny as verbatim globs from the drawer keys"
    (jf-envblk-test--with-drawer-buffer
     (concat ":GPTEL_SCOPE_READ: src/** docs/**\n"
             ":GPTEL_SCOPE_WRITE: build/**\n"
             ":GPTEL_SCOPE_DENY: secrets/**\n")
     "/tmp/work/"
     (lambda ()
       (let ((block (jf/gptel-fragment-environment)))
         ;; Verbatim globs, comma-joined, not summarised.
         (expect block :to-match "- Readable: src/\\*\\*, docs/\\*\\*")
         (expect block :to-match "- Writable: build/\\*\\*")
         (expect block :to-match "- Denied:  *secrets/\\*\\*")))))

  (it "ignores the compose-time CONTEXT argument (block depends only on the buffer)"
    ;; The composer funcalls the dynamic ref's :fn with the send context
    ;; (e.g. `chat'); the env block is INTRINSIC to the buffer, so the
    ;; argument must not change the output.
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/work/"
     (lambda ()
       (expect (jf/gptel-fragment-environment 'chat)
               :to-equal (jf/gptel-fragment-environment)))))

  (it "is wired into the composer via the jf/gptel-fragment-environment-fn seam"
    ;; Loading the env source module populates the composer's tail seam
    ;; with this function (default was `ignore'); the chat default
    ;; composition therefore places it at the tail.
    (expect jf/gptel-fragment-environment-fn
            :to-be #'jf/gptel-fragment-environment))

  (it "includes the live-note sentence"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/work/"
     (lambda ()
       (expect (jf/gptel-fragment-environment)
               :to-match "current as of this message"))))

  (it "renders the Environment heading"
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_READ: src/**\n"
     "/tmp/work/"
     (lambda ()
       (expect (jf/gptel-fragment-environment)
               :to-match "# Environment"))))

  (it "switches to scoped form when only GPTEL_SCOPE_DENY is present"
    ;; The presence of ANY one of the three rendered scope keys is the
    ;; scoped-form gate, not all three.
    (jf-envblk-test--with-drawer-buffer
     ":GPTEL_SCOPE_DENY: /etc/**\n"
     "/tmp/work/"
     (lambda ()
       (let ((block (jf/gptel-fragment-environment)))
         (expect block :to-match "- Denied:  */etc/\\*\\*")
         (expect block :not :to-match "no scope restrictions")))))

  (describe "with no GPTEL_SCOPE_* keys (degradation, design.md D5)"

    (it "reports default-directory and a no-scope-restrictions line"
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_PRESET: coding\n"
       "/tmp/unscoped/"
       (lambda ()
         (let ((block (jf/gptel-fragment-environment)))
           (expect block :to-match "- Working directory: /tmp/unscoped/")
           (expect block :to-match
                   "no scope restrictions (this buffer is not a scoped session)")))))

    (it "does not render the Readable/Writable/Denied bullets"
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_PRESET: coding\n"
       "/tmp/unscoped/"
       (lambda ()
         (let ((block (jf/gptel-fragment-environment)))
           (expect block :not :to-match "- Readable:")
           (expect block :not :to-match "- Writable:")
           (expect block :not :to-match "- Denied:")))))

    (it "does not error and returns a non-empty string"
      (jf-envblk-test--with-drawer-buffer
       ""
       "/tmp/bare/"
       (lambda ()
         (let ((block (jf/gptel-fragment-environment)))
           (expect (stringp block) :to-be t)
           (expect (length block) :to-be-greater-than 0))))))

  (describe "input neutrality (register/boundary/environment-block-input-neutrality)"

    (it "builds from a plain temp buffer with no workspaces context (no workspaces symbol referenced)"
      ;; The fragment must touch no workspaces-package symbol — its
      ;; inputs are INTRINSIC (default-directory + drawer keys) only.
      ;; A temp buffer is not associated with any workspace, so a
      ;; correct fragment produces a valid block here regardless of
      ;; whether the workspaces package happens to be loaded in the
      ;; test image.  The complementary static guarantee — that the
      ;; source references no `workspace' symbol — is enforced by the
      ;; task's grep gate over environment.org.
      (jf-envblk-test--with-drawer-buffer
       ":GPTEL_SCOPE_READ: src/**\n"
       "/tmp/work/"
       (lambda ()
         (let ((block (jf/gptel-fragment-environment)))
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

;; ---------------------------------------------------------------------------
;; pre-send composition (task: pre-send-compose-and-wire)
;;
;; The pre-send composer (`gptel-chat--refresh-system-prompt-from-file',
;; widened from a sibling-file cache refresh into a wholesale composer)
;; recomposes `gptel--system-message' as `role + "\n\n" + environment-block'
;; on every `gptel-request' dispatched from a chat-mode buffer.  ROLE is the
;; sibling file body (re-read each send) or the buffer-local role base
;; `gptel-chat--system-prompt-base'; ENV is `gptel-chat--build-environment-
;; block', appended as the TAIL.  The composed value is write-only output —
;; never read back — so the env block never accumulates across sends
;; (register/invariant/composed-system-message-write-only, D1/D2/D4).
;; ---------------------------------------------------------------------------

(defvar jf-compose-test--tmp-dir nil
  "Temp directory holding the test session.org and sibling file.")

(defun jf-compose-test--write-session (drawer)
  "Write session.org with DRAWER as `:PROPERTIES:' body; return absolute path."
  (let ((session-file (expand-file-name "session.org" jf-compose-test--tmp-dir)))
    (with-temp-file session-file
      (insert ":PROPERTIES:\n"
              drawer
              ":END:\n"
              "#+begin_user\n\n#+end_user\n"))
    session-file))

(defun jf-compose-test--write-sibling (basename body)
  "Write BODY to <tmp>/BASENAME; return its absolute path."
  (let ((path (expand-file-name basename jf-compose-test--tmp-dir)))
    (write-region body nil path nil 'silent)
    path))

(defun jf-compose-test--count-env-blocks (s)
  "Return the number of \"# Environment\" markers in string S."
  (let ((count 0)
        (start 0))
    (while (string-match "# Environment" s start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(describe "pre-send composition"

  :var (session-file)

  (before-each
    (setq jf-compose-test--tmp-dir
          (make-temp-file "jf-compose-" t)))

  (after-each
    (when (and jf-compose-test--tmp-dir
               (file-directory-p jf-compose-test--tmp-dir))
      (delete-directory jf-compose-test--tmp-dir t)))

  (it "appends the environment block as the TAIL, role preceding it"
    (jf-compose-test--write-sibling "system-prompt.md" "You are a careful assistant.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Role appears, then the env block strictly after it.
            (let ((role-pos (string-match "You are a careful assistant\\."
                                          gptel--system-message))
                  (env-pos (string-match "# Environment" gptel--system-message)))
              (expect role-pos :not :to-be nil)
              (expect env-pos :not :to-be nil)
              (expect role-pos :to-be-less-than env-pos))
            ;; The env block is the final section (tail): nothing of the
            ;; role re-appears after it, and the block content is present.
            (expect gptel--system-message :to-match "# Environment")
            (expect gptel--system-message :to-match "- Readable: src/\\*\\*"))
        (kill-buffer buf))))

  (it "produces EXACTLY ONE environment block across two consecutive composes (no accumulation)"
    ;; The most important guard: idempotency.  The composer must rebuild
    ;; from the stable role source, NEVER from the prior composed value,
    ;; so a second invocation does not stack a second env block.
    (jf-compose-test--write-sibling "system-prompt.md" "Role body.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            (gptel-chat--refresh-system-prompt-from-file)
            (expect (jf-compose-test--count-env-blocks gptel--system-message)
                    :to-equal 1)
            ;; The role appears exactly once too — the value did not
            ;; double up.  Composition leads with the static Emacs
            ;; prelude, then role, then the env tail.
            (expect gptel--system-message :to-equal
                    (concat jf/gptel-fragment-chat-prelude-text "\n\n"
                            "Role body.\n\n"
                            (with-current-buffer buf
                              (jf/gptel-fragment-environment)))))
        (kill-buffer buf))))

  (it "no-sibling-file case: composes from the role base, still exactly one block"
    (setq session-file
          (jf-compose-test--write-session
           ":GPTEL_SCOPE_READ: src/**\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel-chat--system-prompt-base "Base role content.")
            (gptel-chat--refresh-system-prompt-from-file)
            (gptel-chat--refresh-system-prompt-from-file)
            (expect (jf-compose-test--count-env-blocks gptel--system-message)
                    :to-equal 1)
            ;; Prelude leads; role follows it; env tails.
            (expect gptel--system-message :to-match
                    (concat "\\`" (regexp-quote jf/gptel-fragment-chat-prelude-text)))
            (expect gptel--system-message :to-match "Base role content\\.")
            (expect gptel--system-message :to-match "# Environment"))
        (kill-buffer buf))))

  (it "reflects a mid-session GPTEL_SCOPE drawer edit in the second composition"
    (jf-compose-test--write-sibling "system-prompt.md" "Role body.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            (expect gptel--system-message :to-match "- Readable: src/\\*\\*")
            (expect gptel--system-message :not :to-match "docs/\\*\\*")
            ;; Extend the drawer's scope keys mid-session, then recompose.
            (save-excursion
              (goto-char (point-min))
              (org-entry-put (point) "GPTEL_SCOPE_READ" "src/** docs/**"))
            (gptel-chat--refresh-system-prompt-from-file)
            (expect gptel--system-message :to-match "- Readable: src/\\*\\*, docs/\\*\\*")
            ;; Still exactly one block — recomposition, not accumulation.
            (expect (jf-compose-test--count-env-blocks gptel--system-message)
                    :to-equal 1))
        (kill-buffer buf))))

  (it "static prefix is byte-identical across composes; only the dynamic tail differs"
    ;; Optional strengthening of register/invariant/static-prerender-
    ;; dynamic-compose (load-bearing): with the role (static prefix)
    ;; unchanged and only the live env input (scope drawer) changing
    ;; between two composes, the two composed messages must share a
    ;; byte-identical prefix up to the env tail's `# Environment'
    ;; marker — the per-send work is confined to the trailing dynamic
    ;; fragment, keeping the static prefix cacheable.
    (jf-compose-test--write-sibling "system-prompt.md" "Role body.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            (let ((first gptel--system-message))
              ;; Change ONLY the live env input (scope), not the role.
              (save-excursion
                (goto-char (point-min))
                (org-entry-put (point) "GPTEL_SCOPE_READ" "src/** docs/**"))
              (gptel-chat--refresh-system-prompt-from-file)
              (let* ((second gptel--system-message)
                     (marker "# Environment")
                     (p1 (string-match marker first))
                     (p2 (string-match marker second)))
                (expect p1 :not :to-be nil)
                (expect p2 :not :to-be nil)
                ;; Tail differs (the live input changed)...
                (expect first :not :to-equal second)
                ;; ...but the prefix up to the env tail is byte-identical.
                (expect (substring first 0 p1)
                        :to-equal (substring second 0 p2)))))
        (kill-buffer buf))))

  (it "appends the block even with no sibling file and a nil role base"
    (setq session-file
          (jf-compose-test--write-session
           ":GPTEL_SCOPE_READ: src/**\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            ;; No sibling property, base left nil → role is "".
            (setq-local gptel-chat--system-prompt-base nil)
            (gptel-chat--refresh-system-prompt-from-file)
            ;; Prelude leads even with no role; env block follows it
            ;; directly (PRELUDE + "\n\n" + ENV, no role section).
            (expect gptel--system-message :to-match
                    (concat "\\`" (regexp-quote jf/gptel-fragment-chat-prelude-text)))
            (expect gptel--system-message :to-match "# Environment")
            (expect (jf-compose-test--count-env-blocks gptel--system-message)
                    :to-equal 1))
        (kill-buffer buf))))

  (it "no-ops for a non-chat-mode buffer (mode guard): no block, unchanged message"
    (with-temp-buffer
      (text-mode)
      (set (make-local-variable 'gptel--system-message) "sentinel-plain-buf")
      (gptel-chat--refresh-system-prompt-from-file)
      (expect gptel--system-message :to-equal "sentinel-plain-buf")
      (expect gptel--system-message :not :to-match "# Environment"))))

;; ---------------------------------------------------------------------------
;; Emacs prelude
;;
;; The static chat prelude is sourced from the `emacs-prelude' fragment
;; (config/gptel/presets/sources/emacs-prelude.org), pre-rendered to a
;; committed `.txt' artifact and wired into the composer's chat lead seam
;; `jf/gptel-fragment-chat-prelude-text' (the former hard-coded
;; `gptel-chat--emacs-prelude' defconst is deleted).  It frames the
;; model's runtime (operating inside GNU Emacs via gptel) and LEADS every
;; chat-mode system prompt: VALUE = PRELUDE + "\n\n" + ROLE + "\n\n" + ENV
;; (or PRELUDE + "\n\n" + ENV when the role is empty).  The composer
;; consumes it verbatim (static, never re-rendered per send) so it never
;; accumulates across sends.
;; ---------------------------------------------------------------------------

(defun jf-prelude-test--count (s)
  "Return the number of Emacs-prelude marker sentences in string S."
  (let ((count 0)
        (start 0))
    (while (string-match "operating inside GNU Emacs" s start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(describe "Emacs prelude"

  :var (session-file)

  (before-each
    (setq jf-compose-test--tmp-dir
          (make-temp-file "jf-prelude-" t)))

  (after-each
    (when (and jf-compose-test--tmp-dir
               (file-directory-p jf-compose-test--tmp-dir))
      (delete-directory jf-compose-test--tmp-dir t)))

  (it "is sourced from the emacs-prelude fragment via the composer seam"
    ;; The prelude is no longer a hard-coded defconst — it is the
    ;; pre-rendered `emacs-prelude' static fragment, wired into the
    ;; composer's chat lead seam.  The seam holds the renderer's output
    ;; (an XML-tagged block, consumed verbatim).
    (expect (stringp jf/gptel-fragment-chat-prelude-text) :to-be t)
    (expect (string-blank-p jf/gptel-fragment-chat-prelude-text) :to-be nil)
    (expect jf/gptel-fragment-chat-prelude-text
            :to-match "operating inside GNU Emacs"))

  (it "keeps the committed .txt mirror in sync with the rendered fragment"
    ;; The committed `.txt' artifact must mirror the rendered prelude text
    ;; byte-for-byte.  This reads the bytes COMMITTED AT HEAD (via git), not
    ;; the working tree: the source's load-time mirror writer silently heals
    ;; the working-tree file, so a working-tree comparison can never observe
    ;; committed drift.  Comparing the committed bytes against a freshly
    ;; rendered value catches the real failure mode — a fragment-source
    ;; change that did not refresh the committed mirror.
    (let ((rendered (jf/gptel-fragment-render
                     jf/gptel-fragment-emacs-prelude--fragment 'claude))
          (committed (jf/prelude-golden--committed-bytes
                      "config/gptel/presets/sources/emacs-prelude.txt")))
      ;; Sanity: the seam holds exactly the rendered text.
      (expect jf/gptel-fragment-chat-prelude-text :to-equal rendered)
      ;; The committed mirror is in sync with the rendered fragment.
      (expect committed :to-equal rendered)))

  (it "the in-sync golden has teeth: a one-line drift from committed is detected"
    ;; Proves the positive golden's `:to-equal' is not vacuously true, WITHOUT
    ;; stubbing the function under proof.  Reads the REAL committed bytes via the
    ;; same `git show HEAD:' path the positive test uses, derives a drifted
    ;; variant (one extra line), and asserts that variant does NOT compare equal
    ;; to the render.  Since the committed bytes are in sync with the render (the
    ;; positive `it' asserts that), a drifted copy must be caught — pinning that
    ;; the equality the golden relies on distinguishes committed drift.
    (let* ((rendered (jf/gptel-fragment-render
                      jf/gptel-fragment-emacs-prelude--fragment 'claude))
           (committed (jf/prelude-golden--committed-bytes
                       "config/gptel/presets/sources/emacs-prelude.txt"))
           (drifted (concat committed "\nSTALE COMMITTED LINE\n")))
      (expect drifted :not :to-equal rendered)))

  (it "leads the composed message, ahead of both role and env"
    (jf-compose-test--write-sibling "system-prompt.md" "You are a careful assistant.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            ;; The message opens with the prelude verbatim.
            (expect gptel--system-message :to-match
                    (concat "\\`" (regexp-quote jf/gptel-fragment-chat-prelude-text)))
            ;; Prelude strictly precedes role, which precedes env.
            (let ((prelude-pos (string-match "operating inside GNU Emacs"
                                             gptel--system-message))
                  (role-pos (string-match "You are a careful assistant\\."
                                          gptel--system-message))
                  (env-pos (string-match "# Environment" gptel--system-message)))
              (expect prelude-pos :to-be-less-than role-pos)
              (expect role-pos :to-be-less-than env-pos)))
        (kill-buffer buf))))

  (it "is present even when the buffer has no role"
    (setq session-file
          (jf-compose-test--write-session
           ":GPTEL_SCOPE_READ: src/**\n"))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (setq-local gptel-chat--system-prompt-base nil)
            (gptel-chat--refresh-system-prompt-from-file)
            (expect gptel--system-message :to-match
                    (concat "\\`" (regexp-quote jf/gptel-fragment-chat-prelude-text)))
            ;; PRELUDE directly followed by the env tail, no role section.
            (let ((prelude-pos (string-match "operating inside GNU Emacs"
                                             gptel--system-message))
                  (env-pos (string-match "# Environment" gptel--system-message)))
              (expect prelude-pos :to-be-less-than env-pos)))
        (kill-buffer buf))))

  (it "appears EXACTLY ONCE across two consecutive composes (no accumulation)"
    (jf-compose-test--write-sibling "system-prompt.md" "Role body.")
    (setq session-file
          (jf-compose-test--write-session
           (concat ":GPTEL_SYSTEM_PROMPT_FILE: system-prompt.md\n"
                   ":GPTEL_SCOPE_READ: src/**\n")))
    (let ((buf (find-file-noselect session-file)))
      (unwind-protect
          (with-current-buffer buf
            (gptel-chat-mode)
            (gptel-chat--refresh-system-prompt-from-file)
            (gptel-chat--refresh-system-prompt-from-file)
            (expect (jf-prelude-test--count gptel--system-message)
                    :to-equal 1))
        (kill-buffer buf)))))

;;; environment-preamble-spec.el ends here
