;;; branching-integration-spec.el --- Integration tests for session branching -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Farr

;; Author: Jeff Farr
;; Keywords: tests

;;; Commentary:

;; End-to-end behavioural tests for `jf/gptel--create-branch-session',
;; mounted on a real temporary session directory. Exercises:
;;   - Parser-driven branch-point selection composed with truncation
;;     and branch-directory creation (registry, metadata, symlink).
;;   - Empty branch from first-turn EXCLUDE still produces a valid
;;     `session.org' in a real branch directory.
;;   - Org commentary before the first user turn is preserved verbatim
;;     in the new branch.
;;   - Registry-update ordering: `jf/gptel--create-branch-session' does
;;     NOT register the new branch; registration is a lazy side effect
;;     of opening the branch's `session.org' — the content-addressed
;;     `gptel-chat-mode-hook' binder `jf/gptel--bind-session-buffer'.
;;
;; Coverage (from
;; openspec/changes/gptel-chat-mode/specs/gptel/sessions-branching.md):
;;   Include → branch point is after #+end_user of the selected turn.
;;   Exclude → branch point is before #+begin_user of the selected turn.
;;   Empty branch from first-turn exclude is valid chat-mode.
;;   Branch preserves org commentary verbatim.
;;   session.org (not session.md) is the authoritative branch file name.

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'gptel-chat-parser)
(require 'gptel-session-constants)
(require 'gptel-session-filesystem)
(require 'gptel-session-registry)
(require 'gptel-session-commands)
(require 'gptel-session-branching)

(defvar jf-branching-integration--tempdirs nil
  "List of temporary directories created during tests for cleanup.")

(defun jf-branching-integration--make-tempdir ()
  (let ((dir (make-temp-file "gptel-branching-integration-" t)))
    (push dir jf-branching-integration--tempdirs)
    dir))

(defun jf-branching-integration--file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;;; Sample content the branch point will be computed from.

;; Parent `session.org' always starts with a `:PROPERTIES:' drawer
;; carrying `GPTEL_PRESET' (Decision 7 of gptel-chat-state-persistence:
;; the drawer at point-min propagates the preset to new branches via
;; plain session.org truncation — no separate metadata.yml copy).

(defconst jf-branching-integration--parent-drawer
  (concat ":PROPERTIES:\n"
          ":GPTEL_PRESET: executor\n"
          ":END:\n"))

(defconst jf-branching-integration--parent-session
  (concat jf-branching-integration--parent-drawer
          "#+begin_user\n"
          "First question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "First answer.\n"
          "#+end_assistant\n"
          "\n"
          "#+begin_user\n"
          "Second question?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Second answer.\n"
          "#+end_assistant\n"))

(defconst jf-branching-integration--parent-with-prose
  (concat jf-branching-integration--parent-drawer
          "* Intro\n"
          "\n"
          "Context notes the user keeps above every turn.\n"
          "\n"
          "#+begin_user\n"
          "Hello?\n"
          "#+end_user\n"
          "\n"
          "#+begin_assistant\n"
          "Hi!\n"
          "#+end_assistant\n"))

(defun jf-branching-integration--bootstrap-parent (session-root parent-content)
  "Create a session directory with a `main' branch under SESSION-ROOT.
Writes PARENT-CONTENT to `main/session.org' and returns a plist:
  (:session-dir SESSION-DIR :branch-dir BRANCH-DIR :context-file PATH)."
  (let* ((jf/gptel-sessions-directory session-root)
         (session-dir (jf/gptel--create-session-directory
                       "integration-20260421000000"))
         (branch-dir (jf/gptel--create-branch-directory session-dir "main"))
         (ctx (jf/gptel--context-file-path branch-dir)))
    (with-temp-file ctx (insert parent-content))
    (list :session-dir session-dir
          :branch-dir branch-dir
          :context-file ctx)))

(defun jf-branching-integration--parse-file (path)
  "Parse PATH with `gptel-chat-parse-buffer' and return the turn list."
  (with-temp-buffer
    (insert-file-contents path)
    (gptel-chat-parse-buffer (current-buffer))))

(describe "Session branching integration (chat-mode turn list)"

  (after-each
    (dolist (dir jf-branching-integration--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-branching-integration--tempdirs nil))

  (describe "INCLUDE branch point: new session.org ends after the selected #+end_user"

    (it "writes a session.org that contains exactly the selected turn and no more"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       ;; First user turn, INCLUDE.
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-include" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; Output is parseable chat-mode: exactly one user turn.
          (expect (length turns) :to-equal 1)
          (expect (plist-get (car turns) :role) :to-equal 'user)
          (expect (string-trim (plist-get (car turns) :content))
                  :to-equal "First question?")
          ;; No assistant block carried through from the parent.
          (expect (string-match-p (regexp-quote "#+begin_assistant") written)
                  :not :to-be-truthy)
          (expect (string-match-p (regexp-quote "Second question?") written)
                  :not :to-be-truthy)
          ;; The selected `#+end_user' line is preserved verbatim.
          (expect (string-match-p (regexp-quote "#+end_user") written)
                  :to-be-truthy)))))

  (describe "EXCLUDE branch point: new session.org stops before the selected #+begin_user"

    (it "writes a session.org whose final content is strictly before the selected turn"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       ;; Second user turn, EXCLUDE.
                       (second (nth 1 turns)))
                  (jf/gptel--branching-turn-branch-point second nil)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-exclude" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; Parseable chat-mode with the first user turn and its
          ;; assistant reply — but NOT the second user turn.
          (expect (mapcar (lambda (tn) (plist-get tn :role)) turns)
                  :to-equal '(user assistant))
          (expect (string-match-p (regexp-quote "Second question?") written)
                  :not :to-be-truthy)
          (expect (string-match-p (regexp-quote "First question?") written)
                  :to-be-truthy)))))

  (describe "First-turn EXCLUDE produces a valid empty chat-mode session"

    (it "writes a session.org with zero user turns and zero assistant turns"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first nil)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "empty-head" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx))
               (turns (jf-branching-integration--parse-file new-ctx)))
          ;; No turns at all — a valid empty chat-mode session.
          (expect turns :to-equal nil)
          (expect (string-match-p "#\\+begin_user" written)
                  :not :to-be-truthy)
          (expect (string-match-p "#\\+begin_assistant" written)
                  :not :to-be-truthy)))))

  (describe "Branch preserves org commentary verbatim"

    (it "retains headings and prose above the selected turn"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-with-prose))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  ;; INCLUDE the lone user turn.
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "exp-commentary" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx)))
          (expect (string-match-p (regexp-quote "* Intro\n") written)
                  :to-be-truthy)
          (expect (string-match-p
                   (regexp-quote "Context notes the user keeps above every turn.\n")
                   written)
                  :to-be-truthy)
          (expect (string-match-p (regexp-quote "#+begin_user\nHello?\n#+end_user\n")
                                  written)
                  :to-be-truthy)
          ;; The assistant reply is truncated.
          (expect (string-match-p (regexp-quote "#+begin_assistant") written)
                  :not :to-be-truthy)))))

  (describe "Branch directory conventions"

    (it "creates branches/<timestamp>-<name>/session.org (not session.md)"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents (plist-get bootstrap :context-file))
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "convention" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir)))
          (expect (file-exists-p new-ctx) :to-be-truthy)
          (expect (file-name-nondirectory new-ctx) :to-equal "session.org")
          ;; Legacy markdown filename must NOT appear in the new branch.
          (expect (file-exists-p (expand-file-name "session.md" new-branch-dir))
                  :not :to-be-truthy)
          ;; Branch-metadata.yml records parent and branch-point position.
          (let ((branch-metadata-path
                 (expand-file-name "branch-metadata.yml" new-branch-dir)))
            (expect (file-exists-p branch-metadata-path) :to-be-truthy))
          ;; Legacy session-level metadata.yml is NOT copied anymore —
          ;; the preset rides along inside the parent's `:PROPERTIES:'
          ;; drawer at the top of session.org (Decision 7).
          (expect (file-exists-p
                   (expand-file-name "metadata.yml" new-branch-dir))
                  :not :to-be-truthy)
          ;; Current symlink resolves to the new branch.
          (let ((current-link (expand-file-name "current" session-dir)))
            (expect (file-symlink-p current-link) :to-be-truthy)
            (expect (file-truename (expand-file-name "session.org" current-link))
                    :to-equal (file-truename new-ctx)))))))

  (describe "Preset drawer inheritance (Decision 7)"

    ;; The parent's `:PROPERTIES:' drawer at the top of session.org is
    ;; copied verbatim by the truncated-context copy, so the new branch
    ;; inherits the parent's preset (and scope keys) without a separate
    ;; metadata.yml copy step.  After the copy,
    ;; `jf/gptel--create-branch-session' rewrites the branch's identity
    ;; keys (`:GPTEL_SESSION_ID:' shared, `:GPTEL_BRANCH:' set to the
    ;; new branch name — register/invariant/branch-drawer-shares-id-
    ;; not-branch), so the drawer is NOT byte-identical to the parent's
    ;; — the non-identity keys (preset) are what carry through verbatim.

    (it "starts the new branch's session.org with the parent's drawer keys plus its own identity"
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (parent-ctx (plist-get bootstrap :context-file))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents parent-ctx)
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  ;; INCLUDE the first user turn.
                  (jf/gptel--branching-turn-branch-point first t)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "drawer-inherit" branch-pos))
               (new-branch-name (file-name-nondirectory new-branch-dir))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx)))
          ;; The new session.org still opens with the `:PROPERTIES:'
          ;; header (drawer is at point-min of parent, preserved by the
          ;; truncated copy).
          (expect (string-prefix-p ":PROPERTIES:\n" written) :to-be t)
          ;; The GPTEL_PRESET value carries through verbatim from the
          ;; parent's drawer (non-identity keys are inherited).
          (expect (string-match-p
                   (regexp-quote ":GPTEL_PRESET: executor")
                   written)
                  :to-be-truthy)
          ;; The branch gained its OWN identity keys via the rewrite.
          (expect (string-match-p
                   (concat ":GPTEL_BRANCH: " (regexp-quote new-branch-name))
                   written)
                  :to-be-truthy))))

    (it "preserves the drawer even when the branch body is empty (first-turn EXCLUDE)"
      ;; When the user selects the first user turn with EXCLUDE, the
      ;; truncated branch contains everything before the first
      ;; `#+begin_user' — which is exactly the parent's drawer. The
      ;; preset inheritance therefore holds for empty branches too.
      (let* ((root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (parent-ctx (plist-get bootstrap :context-file))
             (branch-pos
              (with-temp-buffer
                (insert-file-contents parent-ctx)
                (let* ((turns (jf/gptel--branching-user-turns))
                       (first (nth 0 turns)))
                  (jf/gptel--branching-turn-branch-point first nil)))))
        (let* ((new-branch-dir
                (jf/gptel--create-branch-session
                 session-dir "main" "drawer-empty-body" branch-pos))
               (new-ctx (jf/gptel--context-file-path new-branch-dir))
               (written (jf-branching-integration--file-contents new-ctx)))
          (expect (string-match-p
                   (regexp-quote ":GPTEL_PRESET: executor")
                   written)
                  :to-be-truthy)
          (expect (string-match-p (regexp-quote ":END:") written)
                  :to-be-truthy)))))

  (describe "dirty-buffer handling"

    ;; Regression coverage for the dirty-buffer policy in
    ;; `jf/gptel-branch-session': when the parent session buffer has
    ;; unsaved edits, branch-point positions are computed from the
    ;; LIVE buffer but the branch's `session.org' is truncated from
    ;; the ON-DISK file. Policy A (save before branching) closes the
    ;; gap by flushing the live buffer to disk before position
    ;; selection, so position-source and content-source are the same
    ;; file. Without the save, the new branch silently misaligns
    ;; (missing the unsaved bytes entirely).
    ;;
    ;; Test shape: bootstrap a real session directory, attach a
    ;; buffer to `session.org' with the buffer-local session vars
    ;; that `jf/gptel--bind-session-buffer' would set, append
    ;; a new user turn in-memory only (no save), and invoke
    ;; `jf/gptel-branch-session' with the unsaved user turn
    ;; selected INCLUDE. After branch creation:
    ;;   (a) The parent file on disk now contains the in-memory
    ;;       edit (save-buffer fired).
    ;;   (b) The new branch's `session.org' includes the unsaved
    ;;       turn — evidence that position-source and content-source
    ;;       agree.

    (it "flushes unsaved parent edits before deriving branch-point bytes"
      (let* ((make-backup-files nil)
             (auto-save-default nil)
             (root (jf-branching-integration--make-tempdir))
             (jf/gptel-sessions-directory root)
             (bootstrap (jf-branching-integration--bootstrap-parent
                         root jf-branching-integration--parent-session))
             (session-dir (plist-get bootstrap :session-dir))
             (parent-ctx  (plist-get bootstrap :context-file))
             ;; The unsaved addition: a new user turn appended to the
             ;; live buffer but not written to disk before the call.
             (unsaved-turn (concat "\n"
                                   "#+begin_user\n"
                                   "Unsaved third question?\n"
                                   "#+end_user\n"))
             (parent-buf (generate-new-buffer " *dirty-parent*"))
             (new-branch-dir nil))
        (unwind-protect
            (with-current-buffer parent-buf
              ;; Bind the buffer to the parent session.org file
              ;; without going through `find-file-noselect' (which
              ;; would run `set-auto-mode' and pull in org-mode +
              ;; content-addressed `gptel-chat-mode' activation). We
              ;; only need a file-visiting buffer that `save-buffer'
              ;; will write to the expected path.
              (setq-local buffer-file-name parent-ctx)
              (insert-file-contents parent-ctx)
              (set-buffer-modified-p nil)

              ;; Wire up buffer-local session vars the way
              ;; `jf/gptel--bind-session-buffer' would. We bypass the
              ;; real content-addressed activation (and its org-mode /
              ;; yasnippet side-effects) by setting the vars directly.
              (setq-local jf/gptel--session-dir session-dir)
              (setq-local jf/gptel--branch-name "main")
              (setq-local jf/gptel--session-id
                          (jf/gptel--session-id-from-directory session-dir))

              ;; Apply the unsaved edit: append a new user turn
              ;; in-memory only. The buffer is now dirty.
              (goto-char (point-max))
              (insert unsaved-turn)
              (expect (buffer-modified-p) :to-be-truthy)

              ;; Pre-assert: the on-disk parent file does NOT yet
              ;; contain the unsaved bytes. If this ever passes
              ;; trivially, the regression no longer distinguishes
              ;; the dirty-buffer path from the clean-buffer path.
              (expect (string-match-p
                       (regexp-quote "Unsaved third question?")
                       (jf-branching-integration--file-contents parent-ctx))
                      :not :to-be-truthy)

              (cl-letf*
                  ;; Allow the function body past the chat-mode
                  ;; check without actually activating
                  ;; `gptel-chat-mode' (which pulls in org-mode and
                  ;; a preset-hook chain unrelated to this test).
                  (((symbol-function 'derived-mode-p)
                    (lambda (&rest modes) (memq 'gptel-chat-mode modes)))
                   ;; Deterministic branch-point selection: the
                   ;; unsaved third user turn, INCLUDE. With the
                   ;; buffer's unsaved edit the parser sees three
                   ;; user turns (First/Second/Unsaved-third), so
                   ;; the label for turn 3 is the unsaved prompt.
                   ;; The format matches
                   ;; `jf/gptel--branching-select-branch-point''s
                   ;; "<index>. <label>" formatter.
                   ((symbol-function 'completing-read)
                    (lambda (_prompt _collection &rest _) "3. Unsaved third question?"))
                   ((symbol-function 'y-or-n-p)
                    (lambda (_prompt) t))
                   ;; Do not open the new branch buffer — we read it
                   ;; from disk below. `find-file' would otherwise
                   ;; drag in `gptel-chat-mode' activation.
                   ((symbol-function 'find-file)
                    (lambda (_path) nil)))
                (setq new-branch-dir
                      (progn
                        (jf/gptel-branch-session "dirty-buffer-regression")
                        ;; Re-compute: `jf/gptel-branch-session' does
                        ;; not return the new branch dir, so find
                        ;; it via the current symlink.
                        (file-truename
                         (expand-file-name "current" session-dir)))))

              ;; Post-assertion (a): save fired. Parent file on
              ;; disk now contains the previously-unsaved turn.
              (expect (string-match-p
                       (regexp-quote "Unsaved third question?")
                       (jf-branching-integration--file-contents parent-ctx))
                      :to-be-truthy)

              ;; Post-assertion (b): the new branch's session.org
              ;; was truncated at a position computed against the
              ;; live buffer AND read from a disk file that now
              ;; includes the unsaved turn. Selecting the unsaved
              ;; third user turn INCLUDE means the branch contains
              ;; all three user turns (with the two original
              ;; assistant replies interleaved) and nothing after
              ;; the unsaved turn's `#+end_user'.
              (let* ((new-ctx (jf/gptel--context-file-path new-branch-dir))
                     (written (jf-branching-integration--file-contents new-ctx)))
                ;; Parseable as a chat-mode session.
                (let ((turns (jf-branching-integration--parse-file new-ctx)))
                  ;; user, assistant, user, assistant, user — the
                  ;; trailing user turn is the unsaved one, now
                  ;; persisted via save-buffer.
                  (expect (mapcar (lambda (tn) (plist-get tn :role)) turns)
                          :to-equal '(user assistant user assistant user))
                  (expect (string-trim
                           (plist-get (nth 4 turns) :content))
                          :to-equal "Unsaved third question?"))
                ;; Bytes: all three original turns preserved, the
                ;; unsaved turn present, and no content written
                ;; beyond its `#+end_user' line.
                (expect (string-match-p (regexp-quote "First question?")
                                        written)
                        :to-be-truthy)
                (expect (string-match-p (regexp-quote "Second question?")
                                        written)
                        :to-be-truthy)
                (expect (string-match-p
                         (regexp-quote "Unsaved third question?") written)
                        :to-be-truthy)))
          (when (buffer-live-p parent-buf)
            (with-current-buffer parent-buf
              (set-buffer-modified-p nil))
            (kill-buffer parent-buf)))))))

(describe "registry-update ordering"

  ;; Finding #4: `jf/gptel--create-branch-session' does NOT register
  ;; the new branch in `jf/gptel--session-registry'. Registration is
  ;; a side effect of opening the new branch's `session.org' — under
  ;; the content-addressed model, the `gptel-chat-mode-hook' binder
  ;; (`jf/gptel--bind-session-buffer') registers a signature-bearing
  ;; buffer.  These specs pin that asymmetric behaviour.

  (after-each
    (dolist (dir jf-branching-integration--tempdirs)
      (when (and dir (file-directory-p dir))
        (delete-directory dir t)))
    (setq jf-branching-integration--tempdirs nil))

  (it "does NOT register the new branch during `jf/gptel--create-branch-session'"
    (let* ((root (jf-branching-integration--make-tempdir))
           (jf/gptel-sessions-directory root)
           (bootstrap (jf-branching-integration--bootstrap-parent
                       root jf-branching-integration--parent-session))
           (session-dir (plist-get bootstrap :session-dir))
           (session-id (jf/gptel--session-id-from-directory session-dir))
           (branch-pos
            (with-temp-buffer
              (insert-file-contents (plist-get bootstrap :context-file))
              (let* ((turns (jf/gptel--branching-user-turns))
                     (first (nth 0 turns)))
                (jf/gptel--branching-turn-branch-point first t))))
           (baseline-count (hash-table-count jf/gptel--session-registry))
           (new-branch-dir
            (jf/gptel--create-branch-session
             session-dir "main" "no-register" branch-pos))
           (new-branch-name (file-name-nondirectory new-branch-dir)))
      ;; Branch creation is purely a filesystem operation — the
      ;; registry is untouched.
      (expect (hash-table-count jf/gptel--session-registry)
              :to-equal baseline-count)
      (expect (jf/gptel-session-find session-id new-branch-name)
              :to-be nil)))

  (it "populates the registry lazily on first open via the content-addressed binder"
    (let* ((root (jf-branching-integration--make-tempdir))
           (jf/gptel-sessions-directory root)
           (bootstrap (jf-branching-integration--bootstrap-parent
                       root jf-branching-integration--parent-session))
           (session-dir (plist-get bootstrap :session-dir))
           (session-id (jf/gptel--session-id-from-directory session-dir))
           (branch-pos
            (with-temp-buffer
              (insert-file-contents (plist-get bootstrap :context-file))
              (let* ((turns (jf/gptel--branching-user-turns))
                     (first (nth 0 turns)))
                (jf/gptel--branching-turn-branch-point first t))))
           (new-branch-dir
            (jf/gptel--create-branch-session
             session-dir "main" "lazy-register" branch-pos))
           (new-branch-name (file-name-nondirectory new-branch-dir))
           (new-ctx (jf/gptel--context-file-path new-branch-dir))
           (registry-key (jf/gptel--registry-key session-id new-branch-name))
           (buf (generate-new-buffer "session.org")))
      ;; Pre-condition: create-branch-session did not register.
      (expect (gethash registry-key jf/gptel--session-registry) :to-be nil)
      (unwind-protect
          ;; Simulate opening the branch: a real `gptel-chat-mode'
          ;; activation would fire `gptel-chat-mode-hook' and run the
          ;; content-addressed binder.  Here we set `buffer-file-name',
          ;; load the on-disk session.org content (so the buffer carries
          ;; its `:GPTEL_*:' drawer signature), and invoke the binder
          ;; directly — this spec cares only about registry side effects.
          (with-current-buffer buf
            (setq buffer-file-name new-ctx)
            (insert-file-contents new-ctx)
            (jf/gptel--bind-session-buffer)
            ;; Registry entry now exists for the new branch.
            (let ((entry (gethash registry-key jf/gptel--session-registry)))
              (expect entry :to-be-truthy)
              (expect (plist-get entry :session-id) :to-equal session-id)
              (expect (plist-get entry :branch-name) :to-equal new-branch-name)
              ;; The binder stores branch-dir with a trailing slash
              ;; (from `file-name-directory'), while
              ;; `jf/gptel--create-branch-session' returns the path
              ;; without one. Compare as directories so the test
              ;; pins identity, not representation.
              (expect (file-name-as-directory (plist-get entry :branch-dir))
                      :to-equal (file-name-as-directory new-branch-dir))
              (expect (plist-get entry :buffer) :to-equal buf)))
        ;; Cleanup: unregister and kill buffer.
        (remhash registry-key jf/gptel--session-registry)
        (kill-buffer buf)))))

(provide 'branching-integration-spec)
;;; branching-integration-spec.el ends here
